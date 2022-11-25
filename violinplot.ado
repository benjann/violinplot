*! version 1.0.8  25nov2022  Ben Jann

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
        split(str) ///
        by(str) ///
        DScale(str) ///
        ABSolute ///
        tight ltight rtight ///
        range(numlist max=2 missingok) ///
        n(numlist int >=1 max=1) ///
        PDFopts(str) ///
        qdef(passthru) ///
        cw ///
        LABels(str asis) ///
        OLABels(str asis) ///
        SLABels(str asis) ///
        BYLABels(str asis) ///
        noLABel ///
        key(str) ///
        order(str) ///
        NOLINE     LINE     LINE2(str)     LColor(str asis) ///
        NOFILL     FILL     FILL2(str)     FColor(str asis) ///
        NOWHISKers WHISKers WHISKers2(str) WColor(str asis) ///
        NOBOX      BOX      BOX2(str)      BColor(str asis) ///
        NOMEDian   MEDian   MEDian2(str)   MEDColor(str asis) ///
        NOMEAN     MEAN     MEAN2(str)     MEANColor(str asis) ///
        COLor(str asis) ///
        PSTYles(numlist int >0) ///
        BYOPTs(str) ///
        addplot(str asis) * ]
    // - split()
    _parse_split `split'
    if "`split'"!="" {
        if "`stack'"!="" {
            di as err "split() and unstack not both allowed"
            exit 198
        }
        if "`overlay'"!="" {
            di as err "split() and overlay not both allowed"
            exit 198
        }
    }
    // - orientation
    if "`vertical'"!="" & "`horizontal'"!="" {
        di as err "vertical and horizontal not both allowed"
        exit 198
    }
    if "`overlay'`split'"!="" {
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
            if "`over_sort'"!="" {
                di as err "sort suboption in over() not allowed with atover"
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
    if "`nofill'"=="" & "`split'"!=""  local fill fill
    else if `"`fill2'"'!=""            local fill fill
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
        if `"`fill_select'"'!="" {
            di as err "noline and fill(select()) not both allowed"
            exit 198
        }
        local fill fill // set fill on if noline has been specified
    }
    // - whiskers option: default is whiskers unless overlay has been specified
    if "`nowhiskers'`overlay'`split'"=="" local whiskers whiskers
    if `"`whiskers2'"'!=""                local whiskers whiskers
    if "`whiskers'"!="" {
        if "`nowhiskers'"!="" {
            di as err "whiskers and nowhiskers not both allowed"
            exit 198
        }
    }
    // - box option: default is box unless overlay has been specified
    if "`nobox'`overlay'"=="" local box box
    if `"`box2'"'!=""         local box box
    if "`box'"!="" {
        if "`nobox'"!="" {
            di as err "box and nobox not both allowed"
            exit 198
        }
        _parse_box, `box2' // returns box_type, box2
        if "`box_type'"=="" {
            if "`split'"!="" local box_type lines
            else             local box_type bar
        }
    }
    // - median option: default is median
    if "`nomedian'"==""  local median median
    if `"`median2'"'!="" local median median
    if "`median'"!="" {
        if "`nomedian'"!="" {
            di as err "median and nomedian not both allowed"
            exit 198
        }
        _parse_med, `median2' // returns median_type, median2
        if "`median_type'"=="" {
            if "`split'"!="" local median_type line
            else             local median_type marker
        }
    }
    // - mean option: default is nomean
    if `"`mean2'"'!="" local mean mean
    if "`mean'"!="" {
        if "`nomean'"!="" {
            di as err "mean and nomean not both allowed"
            exit 198
        }
        _parse_mean, `mean2' // returns mean_type, mean2
        if "`mean_type'"=="" local mean_type marker
    }
    // - list of summary statistics
    if `"`mean_stat'"'==""   local stats mean
    else                     local stats `"`mean_stat'"'
    if `"`median_stat'"'=="" local stats `"`stats' median"'
    else                     local stats `"`stats' `median_stat'"'
    if `"`box_stat'"'==""    local stats `"`stats' p25 p75"'
    else                     local stats `"`stats' `box_stat'"'
    // - list (and order) of plot types / determine legend key
    local plotlist `fill' `whiskers' `box'
    if "`median_type'"=="line" local plotlist `plotlist' `median'
    if "`mean_type'"=="line"   local plotlist `plotlist' `mean'
    local plotlist `plotlist' `line'
    if "`median_type'"!="line" local plotlist `plotlist' `median'
    if "`mean_type'"!="line"   local plotlist `plotlist' `mean'
    if `"`order'"'!="" {
        _parse_order `"`order'"' "`plotlist'" // returns plotlist
    }
    _parse_key, `key'
    if "`key'"!="" {
        if !`: list key in plotlist' {
            di as err "key(): `key' not available"
            exit 198
        }
        if "`key'"=="fill" & "`fill_select'"!="" {
            di as err "key(fill) not allowed with fill(select())"
            exit 198
        }
    }
    else {
        if "`line'"!="" local key "line"
        else            local key "fill"
    }
    // - further option
    if "`n'"!="" local nopt n(`n')
    _parse_pdfopts, `pdfopts' // extracts -exact- option
    _parse_range `range' // returns lb ub
    _parse_dscale `dscale' // returns dscale and dstype
    local dscale = `dscale' * 0.5
    if "`tight'"=="" local tight `ltight' `rtight'
    local hasaddplot = `"`addplot'"'!=""
    
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
    if "`over_sort'"!="" {
        if "`over_sort2'"=="" local over_sort2 1
        if `over_sort2'>`k_var_1' { // support for first variable group only
            di as err "over(, sort()): variable #`over_sort2' not found"
            exit 499
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
    if "`split'"!=""                       markout `touse' `split', strok
    if "`weight'"!="" local wgt `"[`weight'`exp']"'
    else              local wgt
    
    // collect levels of split
    if "`split'"!="" {
        qui levelsof `split' if `touse'
        local k_split = r(r)
        if `k_split'!=2 {
            di as err "split() must identify exactly two groups"
            exit 499
        }
        local splitlevels `"`r(levels)'"'
        local splitstr = substr("`: type `split''",1,3)=="str"
    }
    else {
        local k_split     1
        local splitlevels ""
        local splitstr    0
    }
    
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
    // - clusters and plots within clusters
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
    // - physical plots
    if "`split'"!="" local PID split
    else             local PID `PLOT'
    
    // prepare pstyles
    if "`pstyles'"=="" {
        if "`split'"!=""        local pstyles 2
        else if "`overlay'"!="" local pstyles `k_`PLOT''
        else if `stack'         local pstyles 1
        else if `k_`CLUS''==1   local pstyles 1
        else                    local pstyles `k_`PLOT''
        if `pstyles'!=1 {
            numlist "1/`=min(`pstyles', 15)'"
            local pstyles `r(numlist)'
        }
    }
    
    // collect p#[el]() options
    forv p = 1/`k_`PID'' {
        _parse_popts "" `p' , `options' // must type space before ","
        foreach el of local plotlist {
            _parse_popts `el' `p' , `options' // must type space before ","
        }
    }
    
    // generate colors
    if `"`color'"'!="" {
        _parse comma lhs rhs : color
        if `"`rhs'"'=="" local rhs ","
        colorpalette `lhs' `rhs' nograph n(`k_`PID'')
        if r(n)!=`k_`PID'' {
            // wrong number of colors, e.g. because select() was applied
            colorpalette `r(p)', nograph n(`k_`PID'') class(`r(pclass)')
        }
        local color `"`r(p)'"'
    }
    foreach opt in l f w b med mean  {
        if `"``opt'color'"'=="" {
            if "`opt'"=="med" continue // will be handled later
            local `opt'color `"`color'"'
            continue
        }
        _parse comma lhs rhs : `opt'color
        if `"`rhs'"'=="" local rhs ","
        colorpalette `lhs' `rhs' nograph n(`k_`PID'')
        if r(n)!=`k_`PID'' {
            // wrong number of colors, e.g. because select() was applied
            colorpalette `r(p)', nograph n(`k_`PID'') class(`r(pclass)')
        }
        local `opt'color `"`r(p)'"'
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
    // - split labels
    local splitlbls
    local space
    foreach lev of local splitlevels {
        gettoken lab slabels : slabels
        if `"`lab'"'=="" {
            if `splitstr' local lab `"`lev'"'
            else {
                if "`label'"!="" local lab `"`lev'"'
                else             local lab: lab (`split') `lev'
            }
        }
        local splitlbls `"`splitlbls'`space'`"`lab'"'"'
        local space " "
    }
    
    // prepare tempvars
    local ids  byid overid splitid grpid varid
    local vres pos avg med blo bup wlo wup dup dlo at
    tempvar tag `ids' `vres'
    qui gen byte `tag' = . // tag start of new result
    foreach v of local ids {
        qui gen byte ``v'' = .
    }
    foreach v of local vres {
        qui gen double ``v'' = .
    }
    
    // fill in byid for existing observations so that addplot() will
    // will work with by()
    if `hasaddplot' & "`by'"!="" {
        local j 0
        foreach lev of local bylevels {
            local ++j
            if `bystr' local lev `"`"`lev'"'"'
            qui replace `byid' = `j' if `by'==`lev'
        }
    }
    
    // compute results
    local preserve preserve
    if `hasaddplot' local offset0 = _N // (append results)
    else            local offset0 0
    local offset `offset0'
    local j 0
    local o 0
    local bylvls: copy local bylevels
    forv j = 1/`k_by' {
        gettoken bylev bylvls : bylvls
        local olvls: copy local overlevels
        forv o = 1/`k_over' {
            gettoken olev olvls : olvls
            local slvls: copy local splitlevels
            forv s = 1/`k_split' {
                gettoken slev slvls : slvls
                _makeiff `touse' /*
                    */ `j' `k0_by'   "`by'"    `bystr'    `"`bylev'"' /*
                    */ `o' `k0_over' "`over'"  `overstr'  `"`olev'"' /*
                    */ `s'           "`split'" `splitstr' `"`slev'"'
                forv g = 1/`k_grp' {
                    forv i = 1/`k_var_`g'' {
                        local xvar: word `i' of `varlist_`g''
                        su `xvar' `iff', meanonly
                        if (r(N)==0) continue // no observation
                        local x_is_cons = r(max)==r(min)
                        // density estimate
                        if !`x_is_cons' {
                            Fit_PDF "`xvar'" `"`wgt'"' `"`iff'"' "`tight'"/*
                                */ "`lb'" "`ub'" "`exact'" "`nopt'" `"`pdfopts'"'
                            local n = colsof(e(b))
                        }
                        else local n 1 // skip density estimate if x is constant
                        local a = `offset' + 1
                        local b = `offset' + `n' + 1 // insert empty row at end
                        if `b' > _N {
                            `preserve'
                            local preserve
                            qui set obs `b'
                        }
                        if !`x_is_cons' {
                            if "`absolute'"!="" {
                                mata: st_store((`a',`b'-1), "`dup'", ///
                                    st_numscalar("e(W)") * st_matrix("e(b)")')
                            }
                            else {
                                mata: st_store((`a',`b'-1), "`dup'", ///
                                    st_matrix("e(b)")')
                            }
                            mata: st_store((`a',`b'-1), "`at'", ///
                                st_matrix("e(at)")')
                        }
                        // stats
                        Fit_stats `"`stats'"' "`xvar'" `"`wgt'"' `"`iff'"' `a'/*
                            */ `avg' `med' `blo' `bup' `wlo' `wup' `"`qdef'"'
                        // ids
                        qui replace `tag'     = 1   in `a'
                        qui replace `varid'   = `i' in `a'/`b'
                        qui replace `grpid'   = `g' in `a'/`b'
                        qui replace `splitid' = `s' in `a'/`b'
                        qui replace `overid'  = `o' in `a'/`b'
                        qui replace `byid'    = `j' in `a'/`b'
                        local offset `b'
                    }
                }
            }
        }
    }
    
    // check whether there have been any valid observations at all
    if `offset'==`offset0' error 2000
    
    // sort results
    if "`over_sort'"!="" {
        tempname sorttag
        qui gen byte `sorttag' = `tag'<. & `varid'==`over_sort2'/*
            */ & `grpid'==1 & `byid'==1 in `=`offset0'+1'/`offset'
        mata: _over_sort((`offset0'+1, `offset'))
    }
    
    // determine plot positions on categorical axis
    local in "in `=`offset0'+1'/`offset'"
    if "`atover'"!="" {
        if `k_`CLUS''==1 {
            local i 0
            foreach ii of local overlevels {
                local ++i
                qui replace `pos' = `ii' if ``PLOT'id'==`i' `in'
            }
            local plotpos `"`overlevels'"'
        }
        else {
            local i 0
            foreach ii of local overlevels {
                local ++i
                qui replace `pos' = `ii' if ``CLUS'id'==`i' `in'
            }
            local cluspos `"`overlevels'"'
        }
    }
    else if "`overlay'"!="" {
        qui replace `pos' = ``CLUS'id' `in'
        numlist "1/`k_`CLUS''"
        local cluspos `r(numlist)'
    }
    else {
        if `k_`CLUS''==1 {
            qui replace `pos' = ``PLOT'id' `in'
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
                    qui replace `pos' = `ii' if ``CLUS'id'==`c' &/*
                        */ ``PLOT'id'==`i' `in'
                    local plotpos `plotpos' `ii'
                }
                local cluspos `cluspos' `= (`ii0' + `ii') / 2'
                local ii = `ii' + `gap'
            }
        }
    }
    
    // shift and rescale of PDFs
    local in "in `=`offset0'+1'/`offset'"
    if "`dstype'"=="individual" {
        forv j = 1/`k_`By'' {
            forv c = 1/`k_`CLUS'' {
                forv i = 1/`k_`PLOT'' {
                    _dscale `dup' `dlo' `pos' if ``By'id'==`j' & /*
                        */ ``CLUS'id'==`c' & ``PLOT'id'==`i' `in'/*
                        */, dscale(`dscale')
                }
            }
        }
    }
    else if "`dstype'"=="plot" {
        forv j = 1/`k_`By'' {
            forv i = 1/`k_`PLOT'' {
                _dscale `dup' `dlo' `pos' if ``By'id'==`j' & /*
                    */ ``PLOT'id'==`i' `in', dscale(`dscale')
            }
        }
    }
    else if "`dstype'"=="group" {
        forv j = 1/`k_`By'' {
            forv c = 1/`k_`CLUS'' {
                _dscale `dup' `dlo' `pos' if ``By'id'==`j' & /*
                    */ ``CLUS'id'==`c' `in', dscale(`dscale')
            }
        }
    }
    else if "`dstype'"=="subgraph" {
        forv j = 1/`k_`By'' {
            _dscale `dup' `dlo' `pos' if ``By'id'==`j' `in', dscale(`dscale')
        }
    }
    else {
        _dscale `dup' `dlo' `pos' `in', dscale(`dscale')
    }
    
    // add offset if split
    local in "in `=`offset0'+1'/`offset'"
    if "`split'"!="" {
        if "`split_offset'"=="" {
            su `pos' `in', meanonly
            local split_offset = r(max)/100
        }
        tempname POS
        qui generate `POS' = `pos' + `split_offset'*cond(`splitid'==1,-1,1) `in'
    }
    else local POS `pos'
    
    // compute points on density curve used by some plot types
    local pdftmp
    if      "`box_type'"=="fill"  local pdftmp box_dlo box_dup box_at
    else if "`box_type'"=="lines" local pdftmp box_dlo box_dup box_at
    if "`median_type'"=="line"    local pdftmp `pdftmp' med_dlo med_dup
    if "`mean_type'"=="line"      local pdftmp `pdftmp' avg_dlo avg_dup
    if "`pdftmp'"!="" {
        tempvar `pdftmp'
        mata: _ipolate_PDF(`offset0'+1, `offset')
    }
    
    // modify data for plotting
    if !`hasaddplot' {
        // remove extra observations unless addplot() has been specified; this
        // is just to gain a bit of speed in very large datasets
        if `offset' < _N {
            `preserve'
            local preserve
            qui keep in 1/`offset'
        }
    }
    else if "`by'"!="" {
        // copy data if -total- has been specified, so that addplot() will have
        // access to the data in the -total- subgraph
        if "`by_total'"!="" {
            `preserve'
            local preserve
            tempvar dupl
            qui expand 2 if `touse'==1, generate(`dupl')
            fre `dupl'
            qui replace `byid' = `k_by' if `dupl'==1
        }
    }
    else if "`BY'"!="" { // BY is equal to grp in this case
        // make k_grp copies of data, so that addplot() will have access to all
        // data in each subgraph
        `preserve'
        local preserve
        tempname sortindex
        qui gen double `sortindex' = _n
        qui expand `k_`BY'' if `touse'==1
        sort `sortindex'
        qui by `sortindex': replace ``BY'id' = _n if `touse'==1
        drop `sortindex'
    }
    
    // settings related to orientation and choice of axes
    if "`vertical'"!="" local rvert horizontal
    else                local rhor  horizontal
    if `stack' {
       if "`vertical'"!="" local axes xaxis(1 2)
       else                local axes yaxis(1 2)
    }
    
    // fill plots
    local pfill
    if "`fill'"!="" {
        if "`PID'"=="split" {
            local plt1 rarea `dlo' `pos' `at'
            local plt2 rarea `dup' `pos' `at'
        }
        else {
            local plt rarea `dlo' `dup' `at'
        }
        if `"`fill2'"'=="" local finten fintensity(50)
        else               local finten
        if "`fill_select'"!="" {
            if "`PID'"=="split" {
                local plt \`plt\`s''
            }
            local fkeys 0
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
                        forv s = 1/`k_split' {
                            local ++ii
                            if "`psty'"=="" local psty `pstyles'
                            gettoken p psty : psty
                            if !`:list ii in fill_select' continue
                            local ++fkeys
                            getcolr `i' `fcolor'
                            local popts `axes' `rvert' pstyle(p`p') `p_`i''/*
                                */ lc(%0) `finten' `colr' `fill2' `p_`i'f'
                            local pfill `pfill'/*
                                */ (`plt' if ``By'id'==`j' & ``CLUS'id'==`c'/*
                                */ & ``PLOT'id'==`i' & `splitid'==`s', `popts')
                        }
                    }
                }
            }
        }
        else {
            if "`PID'"=="split" {
                local plt \`plt\`i''
            }
            local fkeys `k_`PID''
            local psty `pstyles'
            forv i = 1/`fkeys' {
                if "`psty'"=="" local psty `pstyles'
                gettoken p psty : psty
                getcolr `i' `fcolor'
                local popts `axes' `rvert' pstyle(p`p') `p_`i'' lc(%0) `finten'/*
                    */ `colr' `fill2' `p_`i'f'
                local pfill `pfill'/*
                    */ (`plt' if ``PID'id'==`i', cmissing(n) `popts')
            }
        }
    }
    
    // line plots
    local pline
    if "`line'"!="" {
        if "`PID'"=="split" {
            if "`vertical'"!="" {
                local plt1 line `at' `dlo'
                local plt2 line `at' `dup'
            }
            else {
                local plt1 line `dlo' `at'
                local plt2 line `dup' `at'
            }
            local plt \`plt\`i''
            local popts0
        }
        else {
            local plt rline `dlo' `dup' `at'
            local popts0 `rvert'
        }
        local lkeys `k_`PID''
        local psty `pstyles'
        forv i = 1/`lkeys' {
            if "`psty'"=="" local psty `pstyles'
            gettoken p psty : psty
            getcolr `i' `lcolor'
            local popts `axes' `popts0' pstyle(p`p') `p_`i'' `colr' `line2'/*
                */ `p_`i'l'
            local pline `pline'/*
                */ (`plt' if ``PID'id'==`i', cmissing(n) `popts')
        }
    }
    
    // whisker plots
    local pwhisk
    if "`whiskers'"!="" {
        local psty `pstyles'
        forv i = 1/`k_`PID'' {
            if "`psty'"=="" local psty `pstyles'
            gettoken p psty : psty
            getcolr `i' `wcolor'
            local popts `axes' `rhor' pstyle(p`p') `p_`i'' `colr' `whiskers2'/*
                */ `p_`i'w'
            local pwhisk `pwhisk'/*
                */ (rspike `wlo' `blo' `POS' if ``PID'id'==`i', `popts') /*
                */ (rspike `bup' `wup' `POS' if ``PID'id'==`i', `popts')
        }
        local wkeys = 2*`k_`PID''
    }
    
    // box plots
    local pbox
    if "`box'"!="" {
        local bkeys `k_`PID''
        local psty `pstyles'
        if "`box_type'"=="fill" {
            if "`PID'"=="split" {
                local plt1 rarea `box_dlo' `pos' `box_at'
                local plt2 rarea `box_dup' `pos' `box_at'
                local plt \`plt\`i''
            }
            else {
                local plt rarea `box_dlo' `box_dup' `box_at'
            }
            if `"`box2'"'=="" local finten fintensity(50)
            else              local finten
            forv i = 1/`bkeys' {
                if "`psty'"=="" local psty `pstyles'
                gettoken p psty : psty
                getcolr `i' `bcolor'
                local popts `axes' `rvert' pstyle(p`p') `p_`i'' lc(%0)/*
                    */ `finten' `colr' `box2' `p_`i'b'
                local pbox `pbox'/*
                    */ (`plt' if ``PID'id'==`i', cmissing(n) `popts')
            }
        }
        else if "`box_type'"=="lines" {
            if `"`box2'"'=="" local lpopt lpattern(-)
            else              local lpopt
            if "`PID'"=="split" {
                local plt1 rspike `box_dlo' `pos' `box_at'
                local plt2 rspike `box_dup' `pos' `box_at'
                local plt \`plt\`i''
            }
            else {
                local plt rspike `box_dlo' `box_dup' `box_at'
            }
            forv i = 1/`bkeys' {
                if "`psty'"=="" local psty `pstyles'
                gettoken p psty : psty
                getcolr `i' `bcolor'
                local popts `axes' `rvert' pstyle(p`p') `p_`i'' `lpopt' `colr'/*
                    */ `box2' `p_`i'b'
                local pbox `pbox'/*
                    */ (`plt' if ``PID'id'==`i', `popts')
            }
        }
        else {
            if `"`box2'"'=="" local boxwd lw(vthick)
            else              local boxwd
            forv i = 1/`bkeys' {
                if "`psty'"=="" local psty `pstyles'
                gettoken p psty : psty
                getcolr `i' `bcolor'
                local popts `axes' `rhor' pstyle(p`p') `p_`i'' `boxwd' `colr'/*
                    */ `box2' `p_`i'b'
                local pbox `pbox'/*
                    */ (rspike `blo' `bup' `POS' if ``PID'id'==`i', `popts')
            }
        }
    }
    
    // median plots
    local pmed
    if "`median'"!="" {
        local mkeys `k_`PID''
        local psty `pstyles'
        if "`median_type'"=="line" {
            if "`PID'"=="split" {
                local plt1 rspike `med_dlo' `pos' `med'
                local plt2 rspike `med_dup' `pos' `med'
                local plt \`plt\`i''
            }
            else {
                local plt rspike `med_dlo' `med_dup' `med'
            }
            if `"`medcolor'"'=="" local medcolor `"`color'"'
            forv i = 1/`mkeys' {
                if "`psty'"=="" local psty `pstyles'
                gettoken p psty : psty
                getcolr `i' `medcolor'
                local popts `axes' `rvert' pstyle(p`p') `p_`i'' `colr'/*
                    */ `median2' `p_`i'med'
                local pmed `pmed'/*
                    */ (`plt' if ``PID'id'==`i', `popts')
            }
        }
        else {
            if "`vertical'"!=""  local vlist `med' `POS'
            else                 local vlist `POS' `med'
            if `"`median2'"'!="" | "`box_type'"!="bar" {
                local msym
                if `"`medcolor'"'=="" local medcolor `"`color'"'
            }
            else {
                local msym msymbol(O) msize(vsmall)
                if `"`medcolor'"'=="" {
                    mata: st_local("medcolor", invtokens(J(1,`mkeys',"white")))
                }
            }
            forv i = 1/`mkeys' {
                if "`psty'"=="" local psty `pstyles'
                gettoken p psty : psty
                getcolr `i' `medcolor'
                local popts `axes' pstyle(p`p') `p_`i'' `msym' `colr'/*
                    */ `median2' `p_`i'med'
                local pmed `pmed'/*
                    */ (scatter `vlist' if ``PID'id'==`i', `popts')
            }
        }
    }
    
    // mean plots
    local pmean
    if "`mean'"!="" {
        local akeys `k_`PID''
        local psty `pstyles'
        if "`mean_type'"=="line" {
            if "`PID'"=="split" {
                local plt1 rspike `avg_dlo' `pos' `avg'
                local plt2 rspike `avg_dup' `pos' `avg'
                local plt \`plt\`i''
            }
            else {
                local plt rspike `avg_dlo' `avg_dup' `avg'
            }
            
            forv i = 1/`akeys' {
                if "`psty'"=="" local psty `pstyles'
                gettoken p psty : psty
                getcolr `i' `meancolor'
                local popts `axes' `rvert' pstyle(p`p') `p_`i'' `colr'/*
                    */ `mean2' `p_`i'mean'
                local pmean `pmean'/*
                    */ (`plt' if ``PID'id'==`i', `popts')
            }
        }
        else {
            if "`PID'"!="split" local msopts msymbol(pipe) msize(huge)
            else                local msopts msymbol(x)
            if "`vertical'"!="" {
                local vlist `avg' `POS'
                if "`PID'"!="split" local msopts `msopts' msangle(90)
            }
            else local vlist `POS' `avg'
            if `"`mean2'"'!="" local msopts
            forv i = 1/`akeys' {
                if "`psty'"=="" local psty `pstyles'
                gettoken p psty : psty
                getcolr `i' `meancolor'
                local popts `axes' pstyle(p`p') `p_`i'' `msopts' `colr'/*
                    */ `mean2' `p_`i'mean'
                local pmean `pmean'/*
                    */ (scatter `vlist' if ``PID'id'==`i', `popts')
            }
        }
    }
    
    // put together plot command in right order and determine legend keys
    local plots
    local nkeys 0
    local ikey 0
    foreach el of local plotlist {
        if "`el'"=="fill" {
            local plots `plots' `pfill'
            if "`key'"=="`el'" local ikey  `nkeys'
            else               local nkeys = `nkeys' + `fkeys'
        }
        else if "`el'"=="line" {
            local plots `plots' `pline'
            if "`key'"=="`el'" local ikey `nkeys'
            else               local nkeys = `nkeys' + `lkeys'
        }
        else if "`el'"=="whiskers" {
            local plots `plots' `pwhisk'
            if "`key'"=="`el'" local ikey `nkeys'
            else               local nkeys = `nkeys' + `wkeys'
        }
        else if "`el'"=="box" {
            local plots `plots' `pbox'
            if "`key'"=="`el'" local ikey `nkeys'
            else               local nkeys = `nkeys' + `bkeys'
        }
        else if "`el'"=="median" {
            local plots `plots' `pmed'
            if "`key'"=="`el'" local ikey `nkeys'
            else               local nkeys = `nkeys' + `mkeys'
        }
        else if "`el'"=="mean" {
            local plots `plots' `pmean'
            if "`key'"=="`el'" local ikey `nkeys'
            else               local nkeys = `nkeys' + `akeys'
        }
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
            local ylabels xlabels(`ylabels', `tstyle')/*
                */ xlabels(`ylabels2', axis(2) labsize(medium)/*
                */ `tstyle') xscale(axis(2) alt noline)
            local noitick noixtick
        }
        else {
            local ylabels ylabels(`ylabels', `tstyle' angle(0))/*
                */ yscale(reverse) ylabels(`ylabels2', axis(2) labsize(medium)/*
                */ `tstyle' angle(0)) yscale(axis(2) alt noline)
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
    local legend
    if "`split'"!="" {
        forv i = 1 / `k_split' {
            gettoken lab splitlbls : splitlbls
            local ++ikey
            local legend `legend' `ikey' `"`lab'"'
            if "`key'"=="whiskers" local ++ikey
        }
    }
    else if `k_`PLOT''>1 & (`k_`CLUS''>1 | "`overlay'"!="") & !`stack' {
        forv i = 1 / `k_`PLOT'' {
            gettoken lab `PLOT'lbls : `PLOT'lbls
            local ++ikey
            local legend `legend' `ikey' `"`lab'"'
            if "`key'"=="whiskers" local ++ikey
        }
    }
    if `"`legend'"'!="" {
        local legend legend(order(`legend') all)
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
        local byopt by(``BY'id', note("") `noitick' `bylegend' `by_options'/*
            */ `byopts')
    }
    
    // axis titles
    if `:list sizeof varlist'==1 local xti `"`varlbls'"'
    else                         local xti `""""'
    if "`vertical'"!="" {
        if `stack' local xti ytitle(`xti') xtitle("") xtitle("", axis(2))
        else       local xti ytitle(`xti') xtitle("")
    }
    else {
        if `stack' local xti xtitle(`xti') ytitle("") ytitle("", axis(2))
        else       local xti xtitle(`xti') ytitle("")
    }
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
    twoway `plots' `addplot', `xti' `yti' `legend' `ylabels' `byopt' `options'
end

program _parse_overby
    gettoken type 0 : 0
    if   "`type'"=="by" local opts *
    else                local opts SORT SORT2(numlist int >0 max=1) DEScending/*
                                    */ TLast
    capt n syntax varname [, ///
        MISSing MISSing2(passthru) TOTal TOTal2(passthru) `opts' ]
    if _rc {
        di as err "error in option `type'()"
        exit _rc
    }
    if "`sort2'"!=""      local sort sort
    if "`descending'"!="" local sort sort
    c_local `type'            `varlist'
    c_local `type'_sort       `sort'
    c_local `type'_sort2      `sort2'
    c_local `type'_descending `descending'
    c_local `type'_options    `options'
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
    if "`tlast'"!="" local total total
    c_local `type'_missing  `missing'
    c_local `type'_missing2 `"`missing2'"'
    c_local `type'_total    `total'
    c_local `type'_total2   `"`total2'"'
    c_local `type'_tlast    `tlast'
end

program _parse_split
    capt n syntax [varname(default=none)] [, OFFset(numlist max=1) ]
    if _rc {
        di as err "error in split()"
        exit _rc
    }
    c_local split        `varlist'
    c_local split_offset `offset'
end

program _parse_fill
    syntax [, Select(numlist int >0) *]
    c_local fill_select `select'
    c_local fill2 `options'
end

program _parse_box
    syntax [, stat(str) type(str) * ]
    capt n _parse_box_type, `type'
    if _rc {
        di as err "error in option box(type())"
        exit _rc
    }
    if `"`stat'"'!="" {
        _parse_count_stats `stat'
        if `nstats'>2 {
            di as err "box(stat()): too many statistics specified"
            exit 198
        }
        else if `nstats'<2 {
            di as err "box(stat()): two statistics required"
            exit 198
        }
    }
    c_local box_stat `"`stat'"'
    c_local box_type `type'
    c_local box2 `options'
end

program _parse_box_type
    syntax [, Bar Fill Lines ]
    local type `bar' `fill' `lines'
    if `:list sizeof type'>1 {
        di as err "too many types specified"
        exit 198
    }
    c_local type `type'
end

program _parse_med
    syntax [, stat(str) type(str) * ]
    capt n _parse_med_type, `type'
    if _rc {
        di as err "error in option median(type())"
        exit _rc
    }
    _parse_count_stats `stat'
    if `nstats'>1 {
        di as err "median(stat()): only one statistic allowed"
        exit 198
    }
    c_local median_stat `"`stat'"'
    c_local median_type `type'
    c_local median2     `options'
end

program _parse_mean
    syntax [, stat(str) type(str) * ]
    capt n _parse_med_type, `type'
    if _rc {
        di as err "error in option mean(type())"
        exit _rc
    }
    _parse_count_stats `stat'
    if `nstats'>1 {
        di as err "mean(stat()): only one statistic allowed"
        exit 198
    }
    c_local mean_stat `"`stat'"'
    c_local mean_type `type'
    c_local mean2     `options'
end

program _parse_med_type
    syntax [, Marker Line ]
    local type `marker' `line'
    if `:list sizeof type'>1 {
        di as err "too many types specified"
        exit 198
    }
    c_local type `type'
end

program _parse_count_stats
    local j 0
    while (`"`0'"'!="") {
        gettoken s 0 : 0, bind
        local ++j
    }
    c_local nstats `j'
end

program _parse_order
    args order plotlist
    capt n _parse_elist, `order' // return error if invalid order()
    if _rc {
        di as err "error in option order()"
        exit _rc
    }
    local newlist
    foreach el of local order {
        _parse_elist, `el'
        if !`: list elist in plotlist' {
            di as err "order(): `elist' not available"
            exit 198
        }
        local newlist `newlist' `elist'
        local plotlist: list plotlist - elist
    }
    c_local plotlist `newlist' `plotlist'
end

program _parse_elist
    syntax [, Line Fill Whiskers Box MEDian mean ]
    c_local elist `line' `fill' `whiskers' `box' `median' `mean'
end

program _parse_key
    capt n _parse_elist `0'
    if _rc {
        di as err "error in option key()"
        exit _rc
    }
    if `:list sizeof elist'>1 {
        di as err "key(): too many elements specified"
        exit 198
    }
    c_local key `elist'
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

program _parse_popts
    gettoken el 0 : 0
    if "`el'"!="" {
        if      "`el'"=="median"   local el "med"
        else if "`el'"!="mean"     local el = substr("`el'",1,1)
    }
    gettoken p 0 : 0
    syntax [, p`p'`el'(str) * ]
    c_local p_`p'`el' `p`p'`el''
    c_local options   `options'
end

program getcolr
    gettoken i colors : 0
    local color: word `i' of `colors'
    if `"`color'"'!="" local color color(`"`color'"')
    c_local colr `"`color'"'
end

program _makeiff
    args touse j j0 by bystr bylev o o0 over ostr olev s split sstr slev
    local iff "if `touse'==1"
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
    if "`split'"!="" {
        if `sstr' local iff `"`iff' & `split'==`"`slev'"'"'
        else      local iff `"`iff' & `split'==`slev'"'
    }
    c_local iff `"`iff'"'
end

program Fit_PDF
    tempname ll ul wd0 wd
    args xvar wgt iff tight lb ub exact n pdfopts
    qui dstat pdf `xvar' `iff' `wgt', nose `tight' `exact' `n' `pdfopts'
    local n = colsof(e(b))
    scalar `ll' = el(e(at),1,1)
    scalar `ul' = el(e(at),1,`n')
    scalar `wd0' = `ul' - `ll'
    if "`lb'"!="" {
        if `lb'>`ll' scalar `ll' = `lb'
    }
    if "`ub'"!="" {
        if `ub'<`ul' scalar `ul' = `ub'
    }
    scalar `wd' = `ul' - `ll'
    if `wd'!=`wd0' {
        if `wd'/`wd0' < 0.5 local exact exact
        // use matrix to avoid precision issues
        tempname at
        mata: st_matrix("`at'", /*
            */ rangen(st_numscalar("`ll'"), st_numscalar("`ul'"),`n')')
        qui dstat pdf `xvar' `iff' `wgt', nose at(`at') `exact' `pdfopts'
    }
end

program Fit_stats
    args stats xvar wgt iff i avg med blo bup wlo wup qdef
    qui dstat (`stats') `xvar' `iff' `wgt', nose `qdef'
    tempname S
    mat `S' = e(b)
    if colsof(`S')!=4 {
        di as err "unexpected error; wrong number of summary statistics computed"
        exit 499
    }
    mat `S' = `S', `S'[1,3]-1.5*(`S'[1,4]-`S'[1,3]), /*
                */ `S'[1,4]+1.5*(`S'[1,4]-`S'[1,3])
    qui replace `avg' = `S'[1,1] in `i'
    qui replace `med' = `S'[1,2] in `i'
    qui replace `blo' = `S'[1,3] in `i'
    qui replace `bup' = `S'[1,4] in `i'
    local ifrng inrange(`xvar', `S'[1,5], `S'[1,6])
    if `"`iff'"'!="" local ifrng `"`iff' & `ifrng'"'
    else             local ifrng `"if `ifrng'"'
    su `xvar' `ifrng', meanonly
    qui replace `wlo' = r(min) in `i'
    qui replace `wup' = r(max) in `i'
end

program _dscale
    syntax varlist [if] [in], dscale(str)
    gettoken dup dlo : varlist
    gettoken dlo pos : dlo
    gettoken pos     : pos
    su `dup' `if', meanonly
    qui replace `dup' = `dup'*`dscale'/r(max) + `pos' `if' `in'
    qui replace `dlo' = 2*`pos' - `dup' `if' `in'
end

version 15
mata:
mata set matastrict on

void _over_sort(real rowvector ab)
{
    real scalar      dir, r
    real colvector   med, p, id
    string colvector lbls
    
    // establish sort order
    dir = st_local("over_descending")=="" ? 1 : -1
    med = st_data(ab, st_local("med"), st_local("sorttag"))
    if (st_local("over_tlast")!="") {
        r = rows(med)
        p = order(med[|1\r-1|], dir) \ r
    }
    else p = order(med, dir)
    // recode data
    id    = J(rows(p), 1, .)
    id[p] = st_data(ab, st_local("overid"), st_local("sorttag"))
    st_store(ab, st_local("overid"), id[st_data(ab, (st_local("overid")))])
    // reorder labels
    lbls = tokens(st_local("overlbls"))'[p]
    lbls = "`" :+ `"""' :+ lbls :+ `"""' :+ "'"
    st_local("overlbls", invtokens(lbls'))
}

void _ipolate_PDF(real scalar a, real scalar b)
{
    real scalar      j, fill
    string rowvector tmp
    real rowvector   box, med, avg, d
    real matrix      idx
    
    // add variables
    tmp = tokens(st_local("pdftmp"))
    for (j=length(tmp); j; j--) (void) st_addvar("double", st_local(tmp[j]))
    // collect ids of variables
    d = st_varindex((st_local("dup"), st_local("at"), st_local("pos")))
    if (st_local("box_type")=="lines" | st_local("box_type")=="fill") {
        box = st_varindex((st_local("blo"), st_local("bup"),
            st_local("box_dlo"), st_local("box_dup"), st_local("box_at")))
        fill = (st_local("box_type")=="fill") 
    }
    if (st_local("median_type")=="line") {
        med = st_varindex((st_local("med"),
            st_local("med_dlo"), st_local("med_dup")))
    }
    if (st_local("mean_type")=="line") {
        avg = st_varindex((st_local("avg"),
            st_local("avg_dlo"), st_local("avg_dup")))
    }
    // get index of results
    idx = _ipolate_PDF_index(a, b)
    // compute
    for (j=rows(idx);j;j--) {
        if (idx[j,2]<=idx[j,1]) continue // must have at least two obs
        if (length(box)) _ipolate_PDF_box(idx[j,1], idx[j,2], d, box, fill)
        if (length(med)) _ipolate_PDF_line(idx[j,1], idx[j,2], d, med)
        if (length(avg)) _ipolate_PDF_line(idx[j,1], idx[j,2], d, avg)
    }
}

real matrix _ipolate_PDF_index(real scalar a, real scalar b)
{
    real scalar    r
    real colvector idx
    
    // start of result
    idx = (a-1) :+ selectindex(st_data((a,b), st_local("tag")):<.)
    // end of result (assuming one empty row between results)
    r = rows(idx)
    if (r>1) return((idx, (idx[|2 \ r|]:-2 \ b-1)))
    return((idx, b-1)) 
}

void _ipolate_PDF_box(real scalar a, real scalar b, real rowvector did,
    real rowvector bid, real scalar fill)
{   // did = [d, at, pos]; bid = [atlo, atup, dlo, dup, dat]
    real scalar    r
    real colvector at0, d0, at, d, p
    
    at0 = st_data((a,b), did[2])
    d0  = st_data((a,b), did[1])
    at  = sort(_st_data(a, bid[1]) \ _st_data(a, bid[2]), 1)
    d   = mm_ipolate(at0, d0, at)
    if (hasmissing(d)) return // at is outside of range of at0
    if (!fill) {
        _st_store(a,   bid[4], d[1])
        _st_store(a,   bid[3], 2*_st_data(a, did[3]) - d[1])
        _st_store(a,   bid[5], at[1])
        _st_store(a+1, bid[4], d[2])
        _st_store(a+1, bid[3], 2*_st_data(a, did[3]) - d[2])
        _st_store(a+1, bid[5], at[2])
        return
    }
    p   = selectindex(at0:>at[1] :& at0:<at[2])
    at  = at[1] \ at0[p] \ at[2]
    d   = d[1]  \ d0[p]  \ d[2]
    r   = a + rows(d) - 1
    if (r>b) return // not enough space to store result
    st_store((a,r), bid[4], d)
    st_store((a,r), bid[3], 2*_st_data(a, did[3]) :- d)
    st_store((a,r), bid[5], at)
}

void _ipolate_PDF_line(real scalar a, real scalar b, real rowvector did,
    real rowvector lid)
{   // did = [d, at, pos]; lid = [at, dlo, dup]
    real scalar d

    d = mm_ipolate(st_data((a,b), did[2]), st_data((a,b), did[1]),
        _st_data(a, lid[1]))
    _st_store(a, lid[3], d)
    _st_store(a, lid[2], 2*_st_data(a, did[3]) - d)
}


end

