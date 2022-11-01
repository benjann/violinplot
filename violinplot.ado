*! version 1.0.5  01nov2022  Ben Jann

program violinplot
    version 15
    
    // syntax
    syntax [anything] [if] [in] [pw iw fw] [, ///
        VERTical HORizontal ///
        overlay ///
        asover ///
        over(str) ///
        gap(real 0.5) ///
        atover ///
        swap ///
        nostack ///
        by(str) ///
        DScale(str) ///
        tight ///
        range(numlist max=2 missingok) ///
        PDFopts(str) ///
        qdef(passthru) ///
        cw ///
        LABels(str asis) ///
        OLABels(str asis) ///
        BYLABels(str asis) ///
        noLABel ///
        key(str) ///
        NOLINE     LINE     LINE2(str)     LColor(str) ///
        NOFILL     FILL     FILL2(str)     FColor(str) ///
        NOWHISKers WHISKers WHISKers2(str) WColor(str) ///
        NOBOX      BOX      BOX2(str)      BColor(str) ///
        NOMEDian   MEDian   MEDian2(str)   MEDColor(str) ///
        NOMEAN     MEAN     MEAN2(str)     MEANColor(str) ///
        COLor(str) ///
        PSTYles(numlist int >0) ///
        BYOPTs(str) ///
        addplot(str asis) * ]
    // - orientation
    if "`vertical'"!="" & "`horizontal'"!="" {
        di as err "vertical and horizontal not both allowed"
        exit 198
    }
    if "`overlay'"!="" {
        if "`horizontal'"=="" local vertical vertical
    }
    // - by() and over()
    if `"`by'"'!=""   _parse_overby by `by'
    if `"`over'"'!="" {
        if "`asover'"!="" {
            di as err "asover and over() not both allowed"
            exit 198
        }
        _parse_overby over `over'
        if "`atover'"!="" {
            if "`swap'"!="" {
                di as err "atover and swap not both allowed"
                exit 198
            }
            if "`over_missing'"!="" {
                di as err "missing suboption in over() not allowed with atover"
                exit 198
            }
            if "`over_total'"!="" {
                di as err "total suboption in over() not allowed with atover"
                exit 198
            }
        }
    }
    else {
        if "`atover'"!="" {
            di as err "atover requires over()"
            exit 198
        }
        if "`asover'"!="" & "`swap'"!="" {
            di as err "asover and swap not both allowed"
            exit 198
        }
    }
    // - fill option; default is nofill (unless noline is specified)
    if `"`fill2'"'!="" local fill fill
    if "`fill'"!="" {
        if "`nofill'"!="" {
            di as err "fill and nofill not both allowed"
            exit 198
        }
    }
    _parse_fill, `fill2'
    // - line option; default is line
    if "`noline'"==""       local line line
    else if `"`line2'"'!="" local line line
    if "`line'"!="" {
        if "`noline'"!="" {
            di as err "line and noline not both allowed"
            exit 198
        }
    }
    else {
        if "`nofill'"!="" {
            di as err "noline and nofill not both allowed"
            exit 198
        }
        if `"`fillselect'"'!="" {
            di as err "noline and fill(select()) not both allowed"
            exit 198
        }
        local fill fill // set fill on if noline has been specified
    }
    // - whiskers option: default is whiskers unless overlay has been specified
    if `"`whiskers2'"'!="" local whiskers whiskers
    if "`whiskers'"!="" {
        if "`nowhiskers'"!="" {
            di as err "whiskers and nowhiskers not both allowed"
            exit 198
        }
    }
    if "`nowhiskers'"=="" {
        if "`overlay'"=="" local whiskers whiskers
    }
    // - box option: default is box unless overlay has been specified
    if `"`box2'"'!="" local box box
    if "`box'"!="" {
        if "`nobox'"!="" {
            di as err "box and nobox not both allowed"
            exit 198
        }
    }
    if "`nobox'"=="" {
        if "`overlay'"=="" local box box
    }
    // - median option: default is median
    if `"`median2'"'!="" local median median
    if "`median'"!="" {
        if "`nomedian'"!="" {
            di as err "median and nomedian not both allowed"
            exit 198
        }
    }
    if "`nomedian'"=="" local median median
    // - mean option: default is nomean
    if `"`mean2'"'!="" local mean mean
    if "`mean'"!="" {
        if "`nomean'"!="" {
            di as err "mean and nomean not both allowed"
            exit 198
        }
    }
    // - list of plot types / determine key
    local plotlist `fill' `line' `whiskers' `box' `median' `mean'
    _parse_key, `key'
    if "`key'"!="" {
        if !`: list key in plotlist' {
            di as err "key(): `key' not available"
            exit 198
        }
    }
    else {
        if "`line'"!="" local key "line"
        else            local key "fill"
    }
    // - further option
    _parse_pdfopts, `pdfopts' // extracts -exact- option
    _parse_range `range' // returns lb ub
    _parse_dscale `dscale' // returns dscale and dstype
    local dscale = `dscale' * 0.5
    
    // parse varlist
    gettoken varlist : anything, match(haspar)
    if "`haspar'"!="" {
        _parse_vlists `anything' // returns varlist, k_var, varlist_#, k_var_#
    }
    else {
        _parse_vlist `anything' // returns varlist, k_var
        local k_grp 1
        local k_var_1 `k_var'
        local varlist_1 `varlist'
    }
    if `k_grp'>1 {
        if "`asover'"=="" & "`by'"!="" {
            di as err "by() not allowed with multiple variable groups" /*
                */ " (unless asover is specified)"
            exit 198
        }
    }
    if "`atover'"!="" {
        if `k_var'>1 & "`overlay'"=="" {
            di as err "atover not allowed with multiple variables" /*
                */ " unless overlay is specified"
            exit 198
        }
        else if `k_var'==1 & "`overlay'"!="" {
            di as err "overlay not allowed if atover is specified with" /*
                */ " single variable"
            exit 198
        }
    }
    
    // preserve current estimates
    tempname ecurrent
    _estimates hold `ecurrent', restore nullok
    
    // sample and weights
    if "`cw'"!="" marksample touse
    else          marksample touse, novarlist
    if "`over'"!="" & "`over_missing'"=="" markout `touse' `by', strok
    if "`by'"!=""   & "`by_missing'"==""   markout `touse' `by', strok
    if "`weight'"!="" local wgt `"[`weight'`exp']"'
    else              local wgt
    
    // collect levels of over and by
    if "`over'"!="" {
        qui levelsof `over' if `touse', `over_missing'
        local k0_over    = r(r)
        local k_over     = `k0_over' + ("`over_total'"!="")
        local overlevels `"`r(levels)'"'
        local overstr    = substr("`: type `over''",1,3)=="str"
        if "`atover'"!="" & `overstr' {
            di as err "atover not allowed with string variable"
            exit 198
        }
    }
    else {
        local k0_over    0
        local k_over     1
        local overlevels ""
        local overstr    0
    }
    if "`by'"!="" {
        qui levelsof `by' if `touse', `by_missing'
        local k0_by = r(r)
        local k_by  = `k0_by' + ("`by_total'"!="")
        local bylevels `"`r(levels)'"'
        local bystr = substr("`: type `by''",1,3)=="str"
    }
    else {
        local k0_by     0
        local k_by      1
        local bylevels  ""
        local bystr     0
    }
    
    // assign results layers
    // - subgraphs
    if "`by'"!=""                      local BY by
    else if "`asover'"=="" & `k_grp'>1 local BY grp
    else                               local BY
    if "`BY'"=="" local By by
    else          local By `BY'
    // - over
    if "`asover'"!="" local OVER grp
    else              local OVER over
    // - clusters and plot series
    if      `k_var'==1 & `k_`OVER''>1  local swap "swap"
    else if `k_var'>1  & `k_`OVER''==1 local swap 
    if "`swap'"!="" {
        local CLUS var
        local PLOT `OVER'
    }
    else {
        local CLUS `OVER'
        local PLOT var
    }
    local stack = "`stack'"=="" // 0 if nostack is specified
    if "`overlay'"!="" | `k_`CLUS''==1 local stack 0
    
    // generate colors
    if `"`color'"'!="" {
        _parse comma lhs rhs : color
        if `"`rhs'"'=="" local rhs ","
        colorpalette `lhs' `rhs' nograph n(`k_`PLOT'')
        if r(n)!=`k_`PLOT'' {
            // wrong number of colors, e.g. because select() was applied
            colorpalette `r(p)', nograph n(`k_`PLOT'') class(`r(pclass)')
        }
        local color `"`r(p)'"'
    }
    foreach opt in l f w b med mean  {
        if `"``opt'color'"'=="" {
            if "`opt'"=="med" {
                if "`box'"=="" | `"`median2'"'!="" {
                    local `opt'color `"`color'"'
                }
            }
            else {
                local `opt'color `"`color'"'
            }
        }
        else {
            _parse comma lhs rhs : `opt'color
            if `"`rhs'"'=="" local rhs ","
            colorpalette `lhs' `rhs' nograph n(`k_`PLOT'')
            if r(n)!=`k_`PLOT'' {
                // wrong number of colors, e.g. because select() was applied
                colorpalette `r(p)', nograph n(`k_`PLOT'') class(`r(pclass)')
            }
            local `opt'color `"`r(p)'"'
        }
    }
    
    // collect labels
    // - variable labels
    local varlbls
    local ii 0
    forv g = 1/`k_grp' {
        local varlbls_`g'
        local space
        forv i = 1/`k_var_`g'' {
            gettoken lab labels : labels
            if `"`lab'"'=="" {
                local v: word `i' of `varlist_`g''
                if "`label'"=="" local lab: var lab `v'
                if `"`lab'"'=="" local lab "`v'"
            }
            local varlbls_`g' `"`varlbls_`g''`space'`"`lab'"'"'
            if `i'>`ii' {
                local varlbls `"`varlbls'`space'`"`lab'"'"'
                local ++ii
            }
            local space " "
        }
    }
    // - variable groups (use label/name of first variable in group)
    local grplbls
    local space
    if "`BY'"=="grp" local glabels `"`bylabels'"'
    else             local glabels `"`olabels'"'
    forv g = 1/`k_grp' {
        gettoken lab glabels : glabels
        if `"`lab'"'=="" {
            local v: word 1 of `varlist_`g''
            if "`label'"=="" local lab: var lab `v'
            if `"`lab'"'=="" local lab "`v'"
        }
        local grplbls `"`grplbls'`space'`"`lab'"'"'
        local space " "
    }
    // - over categories and by categories
    local overlabels: copy local olabels
    foreach tmp in by over {
        local `tmp'lbls
        local space
        foreach lev of local `tmp'levels {
            gettoken lab `tmp'labels : `tmp'labels
            if `"`lab'"'=="" {
                if ``tmp'str' local lab `"`lev'"'
                else if `"`lev'"'=="." { // sysmis
                    local lab `"``tmp'_missing2'"'
                    if `"`lab'"'=="" {
                        if "`label'"!="" local lab "."
                        else             local lab "(missing)"
                    }
                }
                else {
                    if "`label'"!="" local lab `"`lev'"'
                    else             local lab: lab (``tmp'') `lev'
                }
            }
            local `tmp'lbls `"``tmp'lbls'`space'`"`lab'"'"'
            local space " "
        }
        if `k_`tmp''>`k0_`tmp'' {
            gettoken lab `tmp'labels : `tmp'labels
            if `"`lab'"'=="" local lab `"``tmp'_total2'"'
            if `"`lab'"'=="" local lab "(total)"
            local `tmp'lbls `"``tmp'lbls'`space'`"`lab'"'"'
        }
    }
    
    // prepare tempvars
    tempvar byid overid grpid varid pos avg med p25 p75 wlo wup dup dlo at
    qui gen byte `byid'    = . // id of by subpop
    qui gen byte `overid'  = . // id of over subpop
    qui gen byte `grpid'   = . // id of variable group
    qui gen byte `varid'   = . // id of variable (within variable group)
    foreach v in pos avg med p25 p75 wlo wup dup dlo at {
        qui gen double ``v'' = .
    }
    
    // compute results
    local preserve 0
    local offset 0
    local j 0
    local o 0
    local bylvls: copy local bylevels
    forv j = 1/`k_by' {
        gettoken bylev bylvls : bylvls
        local olvls: copy local overlevels
        forv o = 1/`k_over' {
            gettoken olev olvls : olvls
            _makeiff `touse' /*
                */ `j' `k0_by'   "`by'"   `bystr'    `"`bylev'"' /*
                */ `o' `k0_over' "`over'" `overstr'  `"`olev'"'
            forv g = 1/`k_grp' {
                forv i = 1/`k_var_`g'' {
                    local xvar: word `i' of `varlist_`g''
                    su `xvar' `iff', meanonly
                    if (r(N)==0) continue // no observation
                    local x_is_cons = r(min)==r(max)
                    // density estimate
                    if !`x_is_cons' {
                        Fit_PDF "`xvar'" `"`wgt'"' `"`iff'"' "`tight'" /*
                            */ "`lb'" "`ub'" "`r(min)'" "`r(max)'" /*
                            */ "`exact'" `"`pdfopts'"'
                        local n = colsof(e(b))
                    }
                    else local n 1 // skip density estimate if x is constant
                    local a = `offset' + 1
                    local b = `offset' + `n' + 1 // insert empty row (missings)
                    if `b' > _N {
                        if `preserve'==0 {
                            preserve
                            local preserve 1
                        }
                        qui set obs `b'
                    }
                    if !`x_is_cons' {
                        mata: st_store((`a',`b'-1), "`dup'",  st_matrix("e(b)")')
                        mata: st_store((`a',`b'-1), "`at'", st_matrix("e(at)")')
                    }
                    // stats
                    Fit_stats "`xvar'" `"`wgt'"' `"`iff'"' `a' /*
                        */ `avg' `med' `p25' `p75' `wlo' `wup' `"`qdef'"'
                    // ids
                    qui replace `varid'   = `i' in `a'/`b'
                    qui replace `grpid'   = `g' in `a'/`b'
                    qui replace `overid'  = `o' in `a'/`b'
                    qui replace `byid'    = `j' in `a'/`b'
                    local offset `b'
                }
            }
        }
    }
    
    // check whether there have been any valid observations at all
    if `offset'==0 error 2000
    
    // remove extra observations (unless addplot() has been specified); this
    // is just to gain a bit of speed in very large datasets
    if `"`addplot'"'=="" {
        if `offset' < _N {
            if `preserve'==0 {
                preserve
                local preserve 1
            }
            qui keep in 1/`offset'
        }
    }
    
    // determine plot positions on categorical axis
    if "`atover'"!="" {
        if `k_`CLUS''==1 {
            local i 0
            foreach ii of local overlevels {
                local ++i
                qui replace `pos' = `ii' if ``PLOT'id'==`i'
            }
            local plotpos `"`overlevels'"'
        }
        else {
            local i 0
            foreach ii of local overlevels {
                local ++i
                qui replace `pos' = `ii' if ``CLUS'id'==`i'
            }
            local cluspos `"`overlevels'"'
        }
    }
    else if "`overlay'"!="" {
        qui replace `pos' = ``CLUS'id'
        numlist "1/`k_`CLUS''"
        local cluspos `r(numlist)'
    }
    else {
        if `k_`CLUS''==1 {
            qui replace `pos' = ``PLOT'id'
            numlist "1/`k_`PLOT''"
            local plotpos `r(numlist)'
        }
        else {
            local cluspos
            local plotpos
            local ii 0
            forv c = 1/`k_`CLUS'' {
                if "`CLUS'"=="grp" local ktmp `k_var_`c''
                else               local ktmp `k_`PLOT''
                local ii0 = `ii' + 1
                forv i = 1/`ktmp' {
                    local ii = `ii' + 1
                    qui replace `pos' = `ii' if ``CLUS'id'==`c' & ``PLOT'id'==`i'
                    local plotpos `plotpos' `ii'
                }
                local cluspos `cluspos' `= (`ii0' + `ii') / 2'
                local ii = `ii' + `gap'
            }
        }
    }
    
    // shift and rescale of PDFs
    if "`dstype'"=="individual" {
        forv j = 1/`k_`By'' {
            forv c = 1/`k_`CLUS'' {
                forv i = 1/`k_`PLOT'' {
                    _dscale `dup' `dlo' `pos' if ``By'id'==`j' & /*
                        */ ``CLUS'id'==`c' & ``PLOT'id'==`i', dscale(`dscale')
                }
            }
        }
    }
    else if "`dstype'"=="plot" {
        forv j = 1/`k_`By'' {
            forv i = 1/`k_`PLOT'' {
                _dscale `dup' `dlo' `pos' if ``By'id'==`j' & /*
                    */ ``PLOT'id'==`i', dscale(`dscale')
            }
        }
    }
    else if "`dstype'"=="group" {
        forv j = 1/`k_`By'' {
            forv c = 1/`k_`CLUS'' {
                _dscale `dup' `dlo' `pos' if ``By'id'==`j' & /*
                    */ ``CLUS'id'==`c', dscale(`dscale')
            }
        }
    }
    else if "`dstype'"=="subgraph" {
        forv j = 1/`k_`By'' {
            _dscale `dup' `dlo' `pos' if ``By'id'==`j', dscale(`dscale')
        }
    }
    else {
        _dscale `dup' `dlo' `pos', dscale(`dscale')
    }
    
    // prepare pstyles
    if "`pstyles'"=="" {
        if "`overlay'"!=""    local pstyles `k_`PLOT''
        else if `stack'       local pstyles 1
        else if `k_`CLUS''==1 local pstyles 1
        else                  local pstyles `k_`PLOT''
        if `pstyles'!=1 {
            numlist "1/`=min(`pstyles', 15)'"
            local pstyles `r(numlist)'
        }
    }
    foreach p of local pstyles {    // collect p#()
        _parse_popts `p' , `options' // must type space before ","
    }
    
    // settings related to orientation; also handles addition of second axis in
    // case of stacked labels
    if "`vertical'"!="" local rvert horizontal
    else                local rhor  horizontal
    if `stack' {
       if "`vertical'"!="" local rvert `rvert' xaxis(1 2)
       else                local rvert `rvert' yaxis(1 2)
    }
    
    // plot counter (for legend)
    local keys 0
    local ikey 0
    
    // fill plots
    local finten fintensity(50)
    if `"`fill2'"'=="" local finten fintensity(50)
    else               local finten
    local pfill
    if "`fillselect'"!="" {
        local ii 0
        forv j = 1/`k_`By'' { // subgraph
            if "`CLUS'"=="var" {
                if "`BY'"=="grp" local ktmp `k_var_`j''
                else             local ktmp `k_var'
            }
            else                 local ktmp `k_`CLUS''
            forv c = 1/`ktmp' { // cluster
                if "`PLOT'"=="var" {
                    if        "`BY'"=="grp" local ktmp2 `k_var_`j''
                    else if "`CLUS'"=="grp" local ktmp2 `k_var_`c''
                    else                    local ktmp2 `k_var'
                }
                else                        local ktmp2 `k_`PLOT''
                local psty `pstyles'
                forv i = 1/`ktmp2' { // plot
                    local ++ii
                    if "`psty'"=="" local psty `pstyles'
                    gettoken p psty : psty
                    if !`:list ii in fillselect' continue
                    local ++keys
                    getcolr `i' `fcolor'
                    local pfill `pfill' /*
                        */ (rarea `dlo' `dup' `at' if ``By'id'==`j' /*
                        */ & ``CLUS'id'==`c' & ``PLOT'id'==`i', /*
                        */ `rvert' pstyle(p`p') `popts_`p'' /*
                        */ lc(%0) `finten' `colr' `fill2')
                }
            }
        }
    }
    else if "`fill'"!="" {
        local psty `pstyles'
        forv i = 1/`k_`PLOT'' {
            if "`psty'"=="" local psty `pstyles'
            gettoken p psty : psty
            getcolr `i' `fcolor'
            local pfill `pfill' /*
                */ (rarea `dlo' `dup' `at' if ``PLOT'id'==`i', cmissing(n) /*
                */ `rvert' pstyle(p`p') `popts_`p'' lc(%0) `finten' `colr' `fill2')
        }
        local keys = `keys' + `k_`PLOT''
    }
    
    // line plots
    if "`key'"=="line" local ikey `keys'
    local pline
    if "`line'"!="" {
        local psty `pstyles'
        forv i = 1/`k_`PLOT'' {
            if "`psty'"=="" local psty `pstyles'
            gettoken p psty : psty
            getcolr `i' `lcolor'
            local pline `pline' /*
                */ (rline `dlo' `dup' `at' if ``PLOT'id'==`i', cmissing(n) /*
                */ `rvert' pstyle(p`p') `popts_`p'' `colr' `line2')
        }
        local keys = `keys' + `k_`PLOT''
    }
    
    // whisker plots
    if "`key'"=="whiskers" local ikey `keys'
    local pwhisk
    if "`whiskers'"!="" {
        local psty `pstyles'
        forv i = 1/`k_`PLOT'' {
            if "`psty'"=="" local psty `pstyles'
            gettoken p psty : psty
            getcolr `i' `wcolor'
            local pwhisk `pwhisk' /*
                */ (rspike `wlo' `p25' `pos' if ``PLOT'id'==`i', `rhor' /*
                */ pstyle(p`p') `popts_`p'' `colr' `whiskers2') /*
                */ (rspike `p75' `wup' `pos' if ``PLOT'id'==`i', `rhor' /*
                */ pstyle(p`p') `popts_`p'' `colr' `whiskers2')
        }
        local keys = `keys' + 2*`k_`PLOT''
    }
    
    // box plots
    if "`key'"=="box" local ikey `keys'
    local pbox
    if "`box'"!="" {
        if `"`box2'"'=="" local boxwd lw(vthick)
        else              local boxwd
        local psty `pstyles'
        forv i = 1/`k_`PLOT'' {
            if "`psty'"=="" local psty `pstyles'
            gettoken p psty : psty
            getcolr `i' `bcolor'
            local pbox `pbox' /*
                */ (rspike `p25' `p75' `pos' if ``PLOT'id'==`i', `rhor' /*
                */ pstyle(p`p') `popts_`p'' `boxwd' `colr' `box2')
        }
        local keys = `keys' + `k_`PLOT''
    }
    
    // median plots
    if "`key'"=="median" local ikey `keys'
    local pmed
    if "`median'"!="" {
        if "`vertical'"!=""  local vlist `med' `pos'
        else                 local vlist `pos' `med'
        if "`box'"!=""       local msym msymbol(O) msize(vsmall)
        else                 local msym
        if `"`median2'"'!="" local msym
        local psty `pstyles'
        forv i = 1/`k_`PLOT'' {
            if "`psty'"=="" local psty `pstyles'
            gettoken p psty : psty
            getcolr `i' `medcolor'
            if `"`colr'"'=="" {
                if "`box'"!="" & `"`median2'"'=="" local colr mcolor(white)
            }
            local pbox `pbox' /*
                */ (scatter `vlist' if ``PLOT'id'==`i', pstyle(p`p') /*
                */ `popts_`p'' `msym' `colr' `median2')
        }
        local keys = `keys' + `k_`PLOT''
    }
    
    // mean plots
    if "`key'"=="mean" local ikey `keys'
    local pmean
    if "`mean'"!="" {
        local msopts msymbol(pipe) msize(huge)
        if "`vertical'"!="" {
            local vlist `avg' `pos'
            local msopts `msopts' msangle(90)
        }
        else local vlist `pos' `avg'
        if `"`mean2'"'!="" local msopts
        local psty `pstyles'
        forv i = 1/`k_`PLOT'' {
            if "`psty'"=="" local psty `pstyles'
            gettoken p psty : psty
            getcolr `i' `meancolor'
            local pmean `pmean' /*
                */ (scatter `vlist' if ``PLOT'id'==`i', pstyle(p`p') /*
                */ `popts_`p'' `msopts' `colr' `mean2')
        }
        local keys = `keys' + `k_`PLOT''
    }
    
    // compile tick labels
    if "`atover'"!="" {
        if "`vertical'"!="" local ylabels xlabels(`overlevels')
        else                local ylabels ylabels(`overlevels')
        local noitick
    }
    else if `stack' {
        local ylabels
        local ylabels2
        forv c = 1 / `k_`CLUS'' {
            gettoken ii cluspos : cluspos
            gettoken lab `CLUS'lbls : `CLUS'lbls
            local ylabels2 `ylabels2' `ii' `"`lab'"'
            if "`CLUS'"=="grp" local tmp _`c'
            else               local tmp
            local tmplbls: copy local `PLOT'lbls`tmp'
            forv i = 1 / `k_`PLOT'`tmp'' {
                gettoken ii plotpos : plotpos
                gettoken lab tmplbls : tmplbls
                local ylabels `ylabels' `ii' `"`lab'"'
            }
        }
        local tstyle notick labgap(tiny) nogrid
        if "`vertical'"!="" {
            local ylabels xlabels(`ylabels', `tstyle') /*
                */ xlabels(`ylabels2', axis(2) `tstyle') xscale(axis(2) alt noline)
            local noitick noixtick
        }
        else {
            local ylabels ylabels(`ylabels', `tstyle' angle(0)) /*
                */ yscale(reverse) ylabels(`ylabels2', axis(2) `tstyle' angle(0))/*
                */ yscale(axis(2) alt noline)
            local noitick noiytick
        }
    }
    else {
        if "`overlay'"!="" | `k_`CLUS''>1 {
            local ylabels
            if `k_`CLUS''==1 local ylabels none
            else {
                forv c = 1 / `k_`CLUS'' {
                    gettoken ii cluspos : cluspos
                    gettoken lab `CLUS'lbls : `CLUS'lbls
                    local ylabels `ylabels' `ii' `"`lab'"'
                }
            }
        }
        else {
            local ylabels
            if `k_`PLOT''==1 local ylabels none
            else {
                forv i = 1 / `k_`PLOT'' {
                    gettoken ii plotpos : plotpos
                    gettoken lab `PLOT'lbls : `PLOT'lbls
                    local ylabels `ylabels' `ii' `"`lab'"'
                }
            }
        }
        local tstyle notick labgap(tiny) nogrid
        if "`vertical'"!="" {
            local ylabels xlabels(`ylabels', `tstyle')
            local noitick noixtick
        }
        else {
            local ylabels ylabels(`ylabels', `tstyle' angle(0)) yscale(reverse)
            local noitick noiytick
        }
    }
    
    // compile legend option
    if `k_`PLOT''>1 & (`k_`CLUS''>1 | "`overlay'"!="") & !`stack' {
        local legend
        forv i = 1 / `k_`PLOT'' {
            gettoken lab `PLOT'lbls : `PLOT'lbls
            local ++ikey
            local legend `legend' `ikey' `"`lab'"'
        }
        local legend legend(order(`legend'))
        local bylegend
    }
    else {
        local legend legend(off)
        local bylegend `legend'
    }
    
    // compile by option
    if "`BY'"=="" local byopt
    else {
        forv j = 1/`k_`BY'' {
            gettoken lab `BY'lbls : `BY'lbls
            lab def ``BY'id' `j' `"`lab'"', add
        }
        lab val ``BY'id' ``BY'id'
        local byopt by(``BY'id', note("") `noitick' `bylegend' `by_options' `byopts')
    }
    
    // axis titles
    if `:list sizeof varlist'==1 local xti `"`varlbls'"'
    else                         local xti `""""'
    if "`vertical'"!="" local xti ytitle(`xti') xtitle("")
    else                local xti xtitle(`xti') ytitle("")
    if "`atover'"!="" {
        if "`label'"=="" local yti: var lab `over'
        if `"`yti'"'=="" local yti "`over'"
    }
    else local yti `""""'
    if "`vertical'"!="" local yti xtitle(`yti')
    else                local yti ytitle(`yti')
    
    // graph
    if `"`addplot'"'!="" {
        local addplot `"|| `addplot' ||"'
    }
    twoway `pfill' `pline' `pwhisk' `pbox' `pmed' `pmean' `addplot' /*
        */ , `xti' `yti' `legend' `ylabels' `byopt' `options'
end

program _parse_overby
    gettoken type 0 : 0
    if "`type'"=="by" local opts *
    capt n syntax varname [, ///
        MISSing MISSing2(passthru) TOTal TOTal2(passthru) `opts' ]
    if _rc {
        di as err "error in option `type'()"
        exit _rc
    }
    c_local `type'         `varlist'
    c_local `type'_options `options'
    if `"`missing2'"'!="" {
        local 0 `", `missing2'"'
        syntax [, missing2(str) ]
        local missing missing
    }
    if `"`total2'"'!="" {
        local 0 `", `total2'"'
        syntax [, total2(str) ]
        local total total
    }
    c_local `type'_missing  `missing'
    c_local `type'_missing2 `"`missing2'"'
    c_local `type'_total    `total'
    c_local `type'_total2   `"`total2'"'
end

program _parse_fill
    syntax [, Select(numlist int >0) *]
    c_local fillselect `select'
    c_local fill2 `options'
end

program _parse_key
    syntax [, line fill WHISKers box MEDian mean ]
    local key `line' `fill' `whiskers' `box' `median' `mean'
    if `:list sizeof key'>1 {
        di as err "key(): too many elements specified"
        exit 198
    }
    c_local key `key'
end

program _parse_pdfopts
    syntax [, exact * ]
    c_local exact `exact'
    c_local pdfopts `options'
end

program _parse_range
    args lb ub
    if "`lb'"!="" {
        if `lb'>=. local lb
    }
    if "`ub'"!="" {
        if `ub'>=. local ub
    }
    if "`lb'"!="" & "`ub'"!="" {
        if `lb'>`ub' {
            di as err "range() invalid -- elements out of order"
            exit 198
        }
    }
    c_local lb `lb'
    c_local ub `ub'
end

program _parse_dscale
    syntax [anything] [, Individual Group Plot Subgraph ]
    local dstype `individual' `group' `plot' `subgraph'
    if `:list sizeof dstype'>1 {
        di as err "dscale(): only one scaling option allowed"
        exit 198
    }
    if `"`anything'"'!="" {
        capt numlist `"`anything'"', range(>0)
        if _rc {
            di as err "dscale(): invalid number"
            exit 198
        }
    }
    else local anything 1
    c_local dscale `anything'
    c_local dstype `dstype'
end

program _parse_vlists
    local i 0
    local kmax 0
    while (`"`0'"'!="") {
        gettoken varlist 0 : 0, match(haspar)
        _parse_vlist `varlist'
        local allvars `allvars' `varlist'
        local ++i
        local kmax = max(`kmax', `k_var')
        c_local k_var_`i' `k_var'
        c_local varlist_`i' `varlist'
    }
    c_local k_grp `i'
    c_local k_var `kmax'
    c_local varlist: list uniq allvars
end

program _parse_vlist
    syntax varlist(numeric)
    c_local k_var: list sizeof varlist
    c_local varlist `varlist'
end

program getcolr
    gettoken i colors : 0
    local color: word `i' of `colors'
    if `"`color'"'!=""  local color color(`"`color'"')
    c_local colr `"`color'"'
end

program _makeiff
    args touse j j0 by bystr bylev o o0 over ostr olev
    local iff "if `touse'"
    if "`by'"!="" {
        if `j'<=`j0' { // (i.e. if not total)
            if `bystr' local iff `"`iff' & `by'==`"`bylev'"'"'
            else       local iff `"`iff' & `by'==`bylev'"'
        }
    }
    if "`over'"!="" {
        if `o'<=`o0' { // (i.e. if not total)
            if `ostr' local iff `"`iff' & `over'==`"`olev'"'"'
            else      local iff `"`iff' & `over'==`olev'"'
        }
    }
    c_local iff `"`iff'"'
end

program Fit_PDF
    args xvar wgt iff tight lb ub min max exact pdfopts
    if "`tight'"!="" {
        local ll `min'
        local ul `max'
    }
    else {
        qui dstat pdf `xvar' `iff' `wgt', nose `exact' `pdfopts'
        local ll = el(e(at),1,1)
        local ul = el(e(at),1,colsof(e(b)))
    }
    local wd0 = `ul' - `ll'
    if "`lb'"!="" {
        if `lb'>`ll' local ll `lb'
    }
    if "`ub'"!="" {
        if `ub'<`ul' local ul `ub'
    }
    local wd = `ul' - `ll'
    if "`tight'"!="" | `wd'!=`wd0' {
        if `wd'/`wd0' < 0.5 local exact exact
        qui dstat pdf `xvar' `iff' `wgt', nose range(`ll' `ul') /*
            */ `exact' `pdfopts'
    }
end

program Fit_stats
    args xvar wgt iff i avg med p25 p75 wlo wup qdef
    qui dstat (mean med p25 p75) `xvar' `iff' `wgt', nose `qdef'
    tempname S
    mat `S' = e(b)
    mat `S' = `S', `S'[1,3]-1.5*(`S'[1,4]-`S'[1,3]), /*
                */ `S'[1,4]+1.5*(`S'[1,4]-`S'[1,3])
    qui replace `avg' = `S'[1,1] in `i'
    qui replace `med' = `S'[1,2] in `i'
    qui replace `p25' = `S'[1,3] in `i'
    qui replace `p75' = `S'[1,4] in `i'
    local ifrng inrange(`xvar', `S'[1,5], `S'[1,6])
    if `"`iff'"'!="" local ifrng `"`iff' & `ifrng'"'
    else             local ifrng `"if `ifrng'"'
    su `xvar' `ifrng', meanonly
    qui replace `wlo' = r(min) in `i'
    qui replace `wup' = r(max) in `i'
end

program _parse_popts
    gettoken p 0 : 0
    syntax [, p`p'(str) * ]
    c_local popts_`p' `p`p''
    c_local options   `options'
end

program _dscale
    syntax varlist [if], dscale(str)
    gettoken dup dlo : varlist
    gettoken dlo pos : dlo
    gettoken pos     : pos
    su `dup' `if', meanonly
    qui replace `dup' = `dup'*`dscale'/r(max) + `pos' `if'
    qui replace `dlo' = 2*`pos' - `dup' `if'
end
