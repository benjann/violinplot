*! version 1.2.6  13may2025  Ben Jann

program violinplot, rclass
    version 15
    
    // syntax
    syntax [anything] [if] [in] [pw iw fw] [, TABle ///
        VERTical HORizontal ///
        Left Right ///
        OVERLay ///
        ASOver Over(str) ATOver swap noSTack ///
        SPlit(str) ///
        by(str) ///
        DScale(str) ABSolute tight ltight rtight ///
        RAnge(str) n(numlist int >=1 max=1) PDFopts(str) ///
        qdef(passthru) ///
        cw ///
        LABels(str asis) OLABels(str asis) SLABels(str asis) ///
        BYLABels(str asis) noLABel ///
        key(str) order(str) bind ///
        OFFset(numlist) DOFFset(numlist) ///
        NODensity ///
        NOLine     Line     Line2(str)     LColors(str asis) ///
        NOFill     Fill     Fill2(str)     FColors(str asis) ///
        NOWhiskers Whiskers Whiskers2(str) WColors(str asis) ///
        NOBox      Box      Box2(str)      BColors(str asis) ///
        NOMEDian   MEDian   MEDian2(str)   MEDColors(str asis) ///
        NOMEAN     MEAN     MEAN2(str)     MEANColors(str asis) ///
        NORAG      RAG      RAG2(str)      RAGColors(str asis) ///
        OUTsides OUTsides2(str) ///
        Colors(str asis) ///
        gap(real 0.5) pad(numlist max=2 >=0) ///
        PSTYles(numlist int >0) ///
        BYOPTs(str) ///
        addplot(str asis) * ]
    // outsides() -> rag()
    if `"`outsides2'"'!="" local outsides outsides
    if "`outsides'"!="" {
        if `"`rag'`rag2'"'!="" {
            di as err "only on of rag() and outsides() allowed"
            exit 198
        }
        local rag2 outsides msymbol(o) `outsides2'
        local outsides
        local outsides2
    }
    // - split()
    _parse_split `split'
    if "`split'"!="" {
        if "`stack'"!="" {
            di as err "split() and nostack not both allowed"
            exit 198
        }
        if "`overlay'"!="" {
            di as err "split() and overlay not both allowed"
            exit 198
        }
    }
    // - orientation
    if "`split'"!="" | ("`left'"!="" & "`right'"!="") {
        local left
        local right
    }
    if "`vertical'"!="" & "`horizontal'"!="" {
        di as err "vertical and horizontal not both allowed"
        exit 198
    }
    if "`horizontal'"=="" {
        if "`split'"!=""                              local vertical vertical
        else if "`overlay'"!="" & "`left'`right'"=="" local vertical vertical
        else if "`nodensity'"!=""                     local vertical vertical
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
    // - nodensity
    if "`nodensity'"!="" {
        local noline noline
        local line
        local line2
        local nofill nofill
        local fill
        local fill2
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
    _parse_fill, `fill2' // fill_select, fill_line ... fill2
    // - line option; default is line
    if "`noline'"==""       local line line
    else if `"`line2'"'!="" local line line
    if "`line'"!="" {
        if "`noline'"!="" {
            di as err "line and noline not both allowed"
            exit 198
        }
    }
    else if "`nofill'"=="" local fill fill // set fill on if noline
    // - whiskers option: default is whiskers unless overlay has been specified
    if "`nowhiskers'"=="" {
        if "`nodensity'"!=""           local whiskers whiskers
        else if "`overlay'`split'"=="" local whiskers whiskers
    }
    if `"`whiskers2'"'!="" local whiskers whiskers
    if "`whiskers'"!="" {
        if "`nowhiskers'"!="" {
            di as err "whiskers and nowhiskers not both allowed"
            exit 198
        }
        _parse_whisk, `whiskers2' // whisk_stat, whisk_cap ... whiskers2
    }
    // - box option: default is box unless overlay has been specified
    if "`nobox'"=="" {
        if "`nodensity'"!=""    local box box
        else if "`overlay'"=="" local box box
    }
    if `"`box2'"'!="" local box box
    if "`box'"!="" {
        if "`nobox'"!="" {
            di as err "box and nobox not both allowed"
            exit 198
        }
        _parse_box, `box2' // box_type, box_limits, box_stat ... box2
        if "`box_type'"=="" {
            if "`nodensity'"!=""  local box_type bar
            else if "`split'"!="" local box_type lines
            else                  local box_type spike
        }
    }
    if "`box_BARW'"!="" {
        if "`split'"!="" local box_BARW = `box_BARW'/2
        local box_barw barwidth(`box_BARW')
    }
    else {
        if "`nodensity'"!="" local box_BARW 0.7
        else                 local box_BARW = 1/6
        if "`split'"!=""     local box_BARW = `box_BARW'/2
    }
    if "`whiskers'"!="" {
        if "`box_type'"=="bar" & "`whisk_nocap'"=="" local whisk_cap cap
        if "`whisk_capw'"=="" {
            if "`box_type'"=="bar" local whisk_capw = `box_BARW' * .6
            else if "`split'"!=""  local whisk_capw 0.05
            else                   local whisk_capw 0.1
        }
        else if "`split'"!=""      local whisk_capw = `whisk_capw'/2
    }
    // - median() and mean(): default is median and nomean
    if "`nomedian'"=="" local median median
    foreach el in median mean {
        if `"``el'2'"'!="" local `el' `el'
        if "``el''"!="" {
            if "`no`el''"!="" {
                di as err "`el' and no`el' not both allowed"
                exit 198
            }
            _parse_med `el', ``el'2' // `el'_type, `el'_stat ... `el'2
            if "``el'_type'"=="" {
                if "`el'"=="median" {
                    if "`box_type'"=="bar" local `el'_type bline
                    else if "`split'"!=""  local `el'_type line
                    else                   local `el'_type marker
                }
                else local `el'_type marker
            }
            if "``el'_type'"=="bline" {
                if "``el'_BARW'"==""  local `el'_BARW = `box_BARW'
                else if "`split'"!="" local `el'_BARW = ``el'_BARW'/2
            }
            else if "``el'_BARW'"!="" local `el'_barw barwidth(``el'_BARW')
        }
    }
    // - rag option: default is norag
    if `"`rag2'"'!="" local rag rag
    if "`rag'"!="" {
        if "`norag'"!="" {
            di as err "rag and norag not both allowed"
            exit 198
        }
        _parse_rag, `rag2' // rag_offset, rag_spread ... rag2
        if "`left'"!="" | "`split'"!="" {
            if "`rag_right'"=="" local rag_left left
        }
        else if "`right'"!="" {
            if "`rag_left'"=="" local rag_right right
        }
        if "`rag_left'"!="" & "`rag_right'"!="" {
            local rag_left
            local rag_right
        }
    }
    // - list of summary statistics
    if `"`mean_stat'"'==""   local stats mean
    else                     local stats `"`mean_stat'"'
    if `"`median_stat'"'=="" local stats `"`stats' median"'
    else                     local stats `"`stats' `median_stat'"'
    if `"`box_stat'"'==""    local stats `"`stats' p25 p75"'
    else                     local stats `"`stats' `box_stat'"'
    // - list of plot types
    local plotlist `fill'
    if "`box_type'"=="fill" local plotlist `plotlist' `box'
    local plotlist `plotlist' `rag' `whiskers'
    if "`box_type'"!="fill" local plotlist `plotlist' `box'
    if "`median_type'"=="line" local plotlist `plotlist' `median'
    if "`mean_type'"=="line"   local plotlist `plotlist' `mean'
    local plotlist `plotlist' `line'
    if "`median_type'"!="line" local plotlist `plotlist' `median'
    if "`mean_type'"!="line"   local plotlist `plotlist' `mean'
    // - activate (suppressed) line if plotlist is empty
    if "`plotlist'"=="" {
        local line line
        local plotlist line
        local lineomit 1
    }
    else local lineomit 0
    // - reorder plot types
    if `"`order'"'!="" {
        _parse_order `"`order'"' "`plotlist'" // returns plotlist
    }
    // - determine legend key
    _parse_key, `key'
    if "`key'"!="" {
        if !`: list key in plotlist' {
            di as err "key(): `key' not available"
            exit 198
        }
    }
    else {
        if "`line'"!=""      local key "line"
        else if "`fill'"!="" local key "fill"
        else if "`box'"!=""  local key "box"
        else gettoken key : plotlist
    }
    // - further option
    if "`n'"!="" local nopt n(`n')
    _parse_pdfopts, `pdfopts' // extracts -exact- option
    _parse_range `range' // returns pdf_l pdf_u
    _parse_dscale `dscale' // returns dscale and dstype
    local dscale = `dscale' * 0.5
    if "`tight'"=="" local tight `ltight' `rtight'
    if "`pad'"=="" {
        if `dscale'>=. local pad 0
        else           local pad 0.5
    }
    local hasaddplot = `"`addplot'"'!=""
    
    // determine whether density estimation is needed
    if "`line'`fill'"=="" {
        local domit 1
        if      "`box_type'"=="fill"    local domit 0
        else if "`box_type'"=="lines"   local domit 0
        else if "`median_type'"=="line" local domit 0
        else if "`mean_type'"=="line"   local domit 0
        else if "`rag_spread'"!="" {
            if `rag_spread2'>=.         local domit 0
        }
        else if "`rag_stack'"!="" {
            if `rag_stack'!=1           local domit 0
        }
    }
    else local domit 0
    if `domit' {
        local dstype
        local doffset
    }
    
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
    if `"`colors'"'!="" {
        _parse comma lhs rhs : colors
        if `"`rhs'"'=="" local rhs ","
        colorpalette `lhs' `rhs' nograph n(`k_`PID'')
        if r(n)!=`k_`PID'' {
            // wrong number of colors, e.g. because select() was applied
            colorpalette `r(p)', nograph n(`k_`PID'') class(`r(pclass)')
        }
        local colors `"`r(p)'"'
    }
    foreach opt in l f w b med mean rag {
        if `"``opt'colors'"'=="" {
            local `opt'colors `"`colors'"'
            continue
        }
        _parse comma lhs rhs : `opt'colors
        if `"`rhs'"'=="" local rhs ","
        colorpalette `lhs' `rhs' nograph n(`k_`PID'')
        if r(n)!=`k_`PID'' {
            // wrong number of colors, e.g. because select() was applied
            colorpalette `r(p)', nograph n(`k_`PID'') class(`r(pclass)')
        }
        local `opt'colors `"`r(p)'"'
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
    local ids  tag byid overid splitid grpid varid
    local vres pos avg med blo bup wlo wup dup dlo at
    if "`rag'"!="" {
        local ids `ids' rtag
        local vres `vres' xrag
        if "`weight'"=="" & "`rag_unique'"=="" local rag_noweight noweight
        if "`rag_noweight'"=="" local vres `vres' wrag wvar
    }
    tempvar `ids' `vres'
    foreach v of local ids {
        qui gen byte ``v'' = .
    }
    foreach v of local vres {
        qui gen double ``v'' = .
    }
    tempvar TOUSE
    if "`by'`over'`split'"!="" {
        qui gen byte `TOUSE' = .
    }
    if "`wvar'"!="" {
        if "`weight'"!="" qui replace `wvar' `exp'
        else              qui replace `wvar' = 1
        local ragwgt [aw=`wrag']
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
    tempname TMP N NOBS S STATS BWIDTH
    local preserve preserve
    if `hasaddplot' local r0 = _N // (append results)
    else            local r0 0
    local r1 `r0'
    local eq 0
    local eqs
    local bylvls: copy local bylevels
    forv j = 1/`k_by' {
        gettoken bylev bylvls : bylvls
        local olvls: copy local overlevels
        forv o = 1/`k_over' {
            gettoken olev olvls : olvls
            local slvls: copy local splitlevels
            forv s = 1/`k_split' {
                local ++eq
                gettoken slev slvls : slvls
                _makeiff `touse' `TOUSE' /* returns iff, tousej, eqlbl
                    */ `j' `k0_by'   "`by'"    `bystr'    `"`bylev'"' /*
                    */ `o' `k0_over' "`over'"  `overstr'  `"`olev'"' /*
                    */ `s'           "`split'" `splitstr' `"`slev'"'
                local eq`eq' `"`eqlbl'"'
                local EQ`eq' `"`eqLBL'"'
                forv g = 1/`k_grp' {
                    forv i = 1/`k_var_`g'' {
                        local eqs `eqs' `eq'
                        local xvar: word `i' of `varlist_`g''
                        su `xvar' `iff', meanonly
                        matrix `N' = (r(N), 0)
                        matrix rown `N' = `xvar'
                        if (`N'[1,1]==0) { // no observations
                            matrix `NOBS' = nullmat(`NOBS') \ `N'
                            matrix `TMP' = .
                            mat rown `TMP' = `xvar'
                            matrix `BWIDTH' = nullmat(`BWIDTH') \ `TMP'
                            matrix `TMP' = J(1,6,.)
                            mat rown `TMP' = `xvar'
                            matrix `STATS' = nullmat(`STATS')  \ `TMP'
                            continue
                        }
                        if `domit' local x_is_cons 1
                        else       local x_is_cons = r(max)==r(min)
                        // summary stats
                        Fit_stats `"`stats'"' "`xvar'" `"`wgt'"' `"`iff'"'/*
                            */ `"`qdef'"' "`box_limits'" `"`whisk_stat'"'/*
                            */ `"`pdf_l'"' `"`pdf_u'"'
                        matrix `S' = r(stats)
                        matrix `STATS' = nullmat(`STATS') \ `S'[1,1..6]
                        matrix `N'[1,2] = r(sum_w)
                        matrix `NOBS'  = nullmat(`NOBS') \ `N'
                        // density estimate
                        if !`x_is_cons' {
                            Fit_PDF "`xvar'" `"`wgt'"' `"`iff'"' "`tight'"/*
                                */ "`S'" "`exact'" "`nopt'" `"`pdfopts'"'
                            local n: colsof e(b)
                            matrix `BWIDTH' = nullmat(`BWIDTH') \ e(bwidth)'
                        }
                        else { // skip density estimate if x is constant
                            local n 1
                            matrix `TMP' = .
                            mat rown `TMP' = `xvar'
                            matrix `BWIDTH' = nullmat(`BWIDTH') \ `TMP'
                        }
                        local a = `r1' + 1
                        local b = `r1' + `n' + 1 // insert empty row at end
                        if `b' > _N {
                            `preserve'
                            local preserve
                            _addobs `b' `touse' `tousej'
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
                        // tag start and end of density estimate
                        qui replace `tag' = 1 in `a' // first row
                        qui replace `tag' = 2 in `b' // last row (missing)
                        // store summary stats in data
                        qui replace `avg' = `S'[1,1] in `a'
                        qui replace `med' = `S'[1,2] in `a'
                        qui replace `blo' = `S'[1,3] in `a'
                        qui replace `bup' = `S'[1,4] in `a'
                        qui replace `wlo' = `S'[1,5] in `a'
                        qui replace `wup' = `S'[1,6] in `a'
                        // rag: copy data
                        if "`rag'"!="" {
                            qui replace `rtag' = 1 in `a' // first row
                            mata: _copyrag(`a', `b', "`rag_outsides'", /*
                                */ "`rag_unique'"!="",  "`wvar'", "`S'")
                        }
                        // ids
                        qui replace `varid'   = `i' in `a'/`b'
                        qui replace `grpid'   = `g' in `a'/`b'
                        qui replace `splitid' = `s' in `a'/`b'
                        qui replace `overid'  = `o' in `a'/`b'
                        qui replace `byid'    = `j' in `a'/`b'
                        local r1 `b'
                    }
                }
            }
        }
    }
    local neq `eq'
    if `"`whisk_stat'"'!="" local coln `stats' `whisk_stat'
    else                    local coln `stats' whisk_l whisk_u
    if !`domit' {
        mat `STATS' = `BWIDTH', `STATS'
        local coln bwidth `coln'
    }
    if !inlist("`weight'","","fweight") {
        mat `STATS' = `NOBS', `STATS'
        local coln N sum_w `coln'
    }
    else {
        mat `STATS' = `NOBS'[1...,1], `STATS'
        local coln N `coln'
    }
    mat coln `STATS' = `coln'
    if "`by'`over'`split'"!="" {
        mat roweq `STATS'  = `eqs'
    }
    
    // check whether there have been any valid observations at all
    if `r1'==`r0' error 2000
    
    // sort results
    if "`over_sort'"!="" {
        tempname sorttag
        qui gen byte `sorttag' = `tag'==1 & `varid'==`over_sort2'/*
            */ & `grpid'==1 & `byid'==1 in `=`r0'+1'/`r1'
        mata: _over_sort((`r0'+1, `r1'))
    }
    
    // determine plot positions on categorical axis
    local in "in `=`r0'+1'/`r1'"
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
        if "`vertical'"!="" {
            qui replace `pos' = ``CLUS'id'-1 `in'
            numlist "0/`=`k_`CLUS''-1'"
        }
        else {
            qui replace `pos' = -(``CLUS'id'-1) `in'
            numlist "0(-1)`=-(`k_`CLUS''-1)'"
        }
        local cluspos `r(numlist)'
    }
    else {
        if `k_`CLUS''==1 {
            if "`vertical'"!="" {
                qui replace `pos' = ``PLOT'id'-1 `in'
                numlist "0/`=`k_`PLOT''-1'"
            }
            else {
                qui replace `pos' = -(``PLOT'id'-1) `in'
                numlist "0(-1)`=-(`k_`PLOT''-1)'"
            }
            local plotpos `r(numlist)'
        }
        else {
            local cluspos
            local plotpos
            if "`vertical'"!="" local step  1
            else                local step -1
            local ii 0
            forv c = 1/`k_`CLUS'' {
                if "`CLUS'"=="grp" local ktmp `k_var_`c''
                else               local ktmp `k_`PLOT''
                local ii0 = `ii'
                forv i = 1/`ktmp' {
                    qui replace `pos' = `ii' if ``CLUS'id'==`c' &/*
                        */ ``PLOT'id'==`i' `in'
                    local plotpos `plotpos' `ii'
                    local ii = `ii' + `step'
                }
                local cluspos `cluspos' `= (`ii0' + `ii' - `step') / 2'
                local ii = `ii' + `step'*`gap'
            }
        }
    }
    // determine (minimum) range of categorical axis
    if "`pad'"!="0" {
        su `pos', meanonly
        gettoken pad0 pad1 : pad
        gettoken pad1 : pad1
        if "`pad1'"=="" local pad1 `pad0'
        local csrange "`=r(min) - `pad0'' `=r(max) + `pad1''"
    }
    
    // shift and rescale PDFs
    local in "in `=`r0'+1'/`r1'"
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
            _dscale `dup' `dlo' `pos' if ``By'id'==`j' `in',/*
                */ dscale(`dscale')
        }
    }
    else if !`domit' {
        _dscale `dup' `dlo' `pos' `in', dscale(`dscale')
    }
    
    // add offsets
    local in "in `=`r0'+1'/`r1'"
    // - determine default offset
    if "`split'"!="" {
        if "`split_offset'"!="" local offset `split_offset' // old syntax
        if "`offset'"=="" {
            if "`box_type'"=="bar" local offset = `box_BARW'/2
            else { // add offset equal to 1% of range of axis
                _get_split_offset `cluspos' `plotpos' // returns offset
            }
        }
    }
    if "`rag'"!="" & "`rag_offset'"=="" local rag_offset `offset'
    // - elements of box plot
    if !inlist("`offset'", "", "0") {
        if `: list sizeof offset'>1 {
            tempname POS
            qui gen double `POS' = .
            local tmp `offset'
            forv p = 1/`k_`PID'' {
                if "`tmp'"=="" local tmp `offset'
                gettoken num tmp : tmp
                if "`vertical'"=="" local num = -`num'
                qui replace `POS' = `pos' + `num'*cond(`splitid'==1,-1,1)/*
                    */ if ``PID'id'==`p' `in'
            }
        }
        else {
            if "`vertical'"=="" local offset = -`offset'
            tempname POS
            qui gen double `POS' = `pos' + `offset'*cond(`splitid'==1,-1,1) `in'
        }
    }
    else local POS `pos'
    // - rag
    if "`rag'"!="" {
        if !inlist("`rag_offset'", "", "0") {
            if `: list sizeof rag_offset'>1 {
                tempname RPOS
                qui gen double `RPOS' = .
                local tmp `rag_offset'
                forv p = 1/`k_`PID'' {
                    if "`tmp'"=="" local tmp `rag_offset'
                    gettoken num tmp : tmp
                    if "`vertical'"=="" local num = -`num'
                    qui replace `RPOS' = `pos' + `num'*cond(`splitid'==1,-1,1)/*
                        */ if ``PID'id'==`p' `in'
                }
            }
            else {
                if "`vertical'"=="" local rag_offset = -`rag_offset'
                tempname RPOS
                qui gen double `RPOS' = `pos' + `rag_offset'*cond(`splitid'==1,-1,1) `in'
            }
        }
        else if "`rag_spread'`rag_stack'"!="" {
            tempname RPOS
            qui gen double `RPOS' = `pos' `in'
        }
        else local RPOS `pos'
        if "`rag_spread'`rag_stack'"!="" {
            local rag_dir = "`rag_left'`rag_right'"!=""
            if `rag_dir' {
                if      "`rag_left'"!="" & "`vertical'"!=""  local rag_dir -1
                else if "`rag_right'"!="" & "`vertical'"=="" local rag_dir -1
            }
            if "`rag_spread'"!="" {
                mata: _rag_spread(`r0'+1, `r1', `rag_spread', `rag_spread2',/*
                    */ `rag_dir')
            }
            else {
                mata: _rag_stack(`r0'+1, `r1', `rag_stack', `rag_stack2',/*
                    */ `rag_dir')
            }
        }
    }
    
    // - density
    if !inlist("`doffset'", "", "0") {
        if `: list sizeof doffset'>1 {
            tempname Pos
            qui gen double `Pos' = .
            local tmp `doffset'
            forv p = 1/`k_`PID'' {
                if "`tmp'"=="" local tmp `doffset'
                gettoken num tmp : tmp
                if "`vertical'"=="" local num = -`num'
                qui replace `Pos' = `pos' + `num'*cond(`splitid'==1,-1,1) /*
                    */ if ``PID'id'==`p' `in'
                qui replace `dup' = `dup' + `num'*cond(`splitid'==1,-1,1)/*
                    */ if ``PID'id'==`p' `in'
                qui replace `dlo' = `dlo' + `num'*cond(`splitid'==1,-1,1)/*
                    */ if ``PID'id'==`p' `in'
            }
        }
        else {
            if "`vertical'"=="" local doffset = -`doffset'
            tempname Pos
            qui gen double `Pos' = `pos' + `doffset'*cond(`splitid'==1,-1,1) `in'
            qui replace `dup'    = `dup' + `doffset'*cond(`splitid'==1,-1,1) `in'
            qui replace `dlo'    = `dlo' + `doffset'*cond(`splitid'==1,-1,1) `in'
        }
    }
    else local Pos `pos'
    
    // compute points on density curve used by some plot types
    local pdftmp
    if      "`box_type'"=="fill"  local pdftmp box_dlo box_dup box_at
    else if "`box_type'"=="lines" local pdftmp box_dlo box_dup box_at
    if "`median_type'"=="line"    local pdftmp `pdftmp' med_dlo med_dup
    if "`mean_type'"=="line"      local pdftmp `pdftmp' avg_dlo avg_dup
    if "`pdftmp'"!="" {
        tempvar `pdftmp'
        mata: _ipolate_PDF(`r0'+1, `r1')
    }
    
    // modify data for plotting
    if !`hasaddplot' {
        // remove extra observations unless addplot() has been specified; this
        // is just to gain a bit of speed in very large datasets
        if `r1' < _N {
            `preserve'
            local preserve
            qui keep in 1/`r1'
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
    if "`vertical'"=="" {
        foreach tmp in "" box_ med_ avg_ {
            mata: _lswap("`tmp'dlo", "`tmp'dup")
        }
    }

    // fill plots
    if "`fill'"!="" {
        if "`PID'"=="split" {
            local plt1 rarea `dlo' `Pos' `at'
            local plt2 rarea `Pos' `dup' `at'
        }
        else if "`left'"!=""  local plt rarea `dlo' `Pos' `at'
        else if "`right'"!="" local plt rarea `Pos' `dup' `at'
        else                  local plt rarea `dlo' `dup' `at'
        local OPTS fintensity(100) `fill_fc' `fill_lc' 
        if `"`fill_fc'"'=="" {
            if "`overlay'"!="" local OPTS `OPTS' fcolor(%50)
            else               local OPTS `OPTS' fcolor(*.5)
        }
        if `"`fill_line'`fill_lc'"'=="" local OPTS `OPTS' lcolor(%0)
        if "`fill_select'"!="" {
            if "`PID'"=="split" {
                local plt \`plt\`s''
            }
            local K_fill 0
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
                            local ++K_fill
                            _getcolr `i' `fcolors'
                            local popts `axes' `rvert' pstyle(p`p') `p_`i''/*
                                */ `colr' `OPTS' `fill2' `p_`i'f'
                            if !`:list ii in fill_select' local iff 0
                            else {
                                local iff ``By'id'==`j' & ``CLUS'id'==`c'/*
                                    */ & ``PLOT'id'==`i' & `splitid'==`s'
                            }
                            local pfill_`i' `pfill_`i''/*
                                */ (`plt' if `iff', `popts')
                        }
                    }
                }
            }
        }
        else {
            if "`PID'"=="split" {
                local plt \`plt\`i''
            }
            local K_fill `k_`PID''
            local psty `pstyles'
            forv i = 1/`K_fill' {
                if "`psty'"=="" local psty `pstyles'
                gettoken p psty : psty
                _getcolr `i' `fcolors'
                local popts `axes' `rvert' pstyle(p`p') `p_`i'' `colr' `OPTS'/*
                    */ `fill2' `p_`i'f'
                local pfill_`i' (`plt' if ``PID'id'==`i', cmissing(n) `popts')
            }
        }
    }
    
    // line plots
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
        else if "`left'"!="" {
            if "`vertical'"!="" local plt line `at' `dlo'
            else                local plt line `dlo' `at'
            local popts0
        }
        else if "`right'"!="" {
            if "`vertical'"!="" local plt line `at' `dup'
            else                local plt line `dup' `at'
            local popts0
        }
        else {
            local plt rline `dlo' `dup' `at'
            local popts0 `rvert'
        }
        local K_line `k_`PID''
        local psty `pstyles'
        if `lineomit' local iff 0
        else          local iff "``PID'id'==\`i'"
        forv i = 1/`K_line' {
            if "`psty'"=="" local psty `pstyles'
            gettoken p psty : psty
            _getcolr `i' `lcolors'
            local popts `axes' `popts0' pstyle(p`p') `p_`i'' `colr' `line2'/*
                */ `p_`i'l'
            local pline_`i' (`plt' if `iff', cmissing(n) `popts')
        }
    }
    
    // whisker plots
    if "`whiskers'"!="" {
        local plt1 rspike `wlo' `blo' `POS'
        local plt2 rspike `bup' `wup' `POS'
        if "`whisk_cap'"!="" {
            tempname WPOS0 WPOS1
            qui gen double `WPOS0' = `POS' - (`whisk_capw'/2)
            qui gen double `WPOS1' = `POS' + (`whisk_capw'/2)
            local plt3 rspike `WPOS0' `WPOS1' `wlo'
            local plt4 rspike `WPOS0' `WPOS1' `wup'
        }
        local psty `pstyles'
        forv i = 1/`k_`PID'' {
            if "`psty'"=="" local psty `pstyles'
            gettoken p psty : psty
            _getcolr `i' `wcolors'
            local popts `axes' pstyle(p`p') `p_`i'' `colr' `whiskers2'/*
                */ `p_`i'w'
            local pwhiskers_`i'/*
                */ (`plt1' if ``PID'id'==`i', `rhor' `popts') /*
                */ (`plt2' if ``PID'id'==`i', `rhor' `popts')
            if "`whisk_cap'"!="" {
                local pwhiskers_`i' `pwhiskers_`i''/*
                    */ (`plt3' if ``PID'id'==`i', `rvert' `popts') /*
                    */ (`plt4' if ``PID'id'==`i', `rvert' `popts')
            }
        }
        local K_whiskers = 2 * `k_`PID''
        if "`whisk_cap'"!="" local K_whiskers = 2 * `K_whiskers'
    }
    
    // box plots
    if "`box'"!="" {
        local K_box `k_`PID''
        local psty `pstyles'
        local OPTS `box_fc' `box_lp' `box_lw' `box_lc' `box_barw'
        if "`box_type'"=="fill" {
            local OPTS fintensity(100) `OPTS'
            if "`PID'"=="split" {
                local plt1 rarea `box_dlo' `Pos' `box_at'
                local plt2 rarea `Pos' `box_dup' `box_at'
                local plt \`plt\`i''
            }
            else if "`left'"!=""  local plt rarea `box_dlo' `Pos' `box_at'
            else if "`right'"!="" local plt rarea `Pos' `box_dup' `box_at'
            else                  local plt rarea `box_dlo' `box_dup' `box_at'
            if `"`box_fc'"'=="" {
                if "`overlay'"!="" local OPTS `OPTS' fcolor(%50)
                else               local OPTS `OPTS' fcolor(*.5)
            }
            if `"`box_lc'"'=="" local OPTS `OPTS' lcolor(%0)
            forv i = 1/`K_box' {
                if "`psty'"=="" local psty `pstyles'
                gettoken p psty : psty
                _getcolr `i' `bcolors'
                local popts `axes' `rvert' pstyle(p`p') `p_`i'' `colr' `OPTS'/*
                    */ `box2' `p_`i'b'
                local pbox_`i' (`plt' if ``PID'id'==`i', cmissing(n) `popts')
            }
        }
        else if "`box_type'"=="lines" {
            if `"`box_lp'"'=="" local OPTS `OPTS' lpattern(-)
            if "`PID'"=="split" {
                local plt1 rspike `box_dlo' `Pos' `box_at'
                local plt2 rspike `box_dup' `Pos' `box_at'
                local plt \`plt\`i''
            }
            else if "`left'"!=""  local plt rspike `box_dlo' `Pos' `box_at'
            else if "`right'"!="" local plt rspike `Pos' `box_dup' `box_at'
            else                  local plt rspike `box_dlo' `box_dup' `box_at'
            forv i = 1/`K_box' {
                if "`psty'"=="" local psty `pstyles'
                gettoken p psty : psty
                _getcolr `i' `bcolors'
                local popts `axes' `rvert' pstyle(p`p') `p_`i'' `colr' `OPTS'/*
                    */ `box2' `p_`i'b'
                local pbox_`i' (`plt' if ``PID'id'==`i', `popts')
            }
        }
        else {
            if "`box_type'"=="bar" {
                local OPTS fintensity(100) `OPTS'
                local plt rbar `blo' `bup' `POS'
                if `"`box_fc'"'=="" {
                    if "`overlay'"!="" local OPTS `OPTS' fcolor(%50)
                    else               local OPTS `OPTS' fcolor(*.5)
                }
                if `"`box_barw'"'=="" local OPTS `OPTS' barwidth(`box_BARW')
            }
            else {
                local plt rspike `blo' `bup' `POS'
                if `"`box_lw'"'=="" local OPTS `OPTS' lwidth(vthick)
            }
            forv i = 1/`K_box' {
                if "`psty'"=="" local psty `pstyles'
                gettoken p psty : psty
                _getcolr `i' `bcolors'
                local popts `axes' `rhor' pstyle(p`p') `p_`i'' `colr' `OPTS'/*
                    */ `box2' `p_`i'b'
                local pbox_`i' (`plt' if ``PID'id'==`i', `popts')
            }
        }
    }
    
    // median/mean plots
    foreach el in median mean {
        if "`el'"=="mean" {
            local EL avg
            local eL mean
        }
        else {
            local EL med
            local eL med
        }
        if "``el''"=="" continue
        local K_`el' `k_`PID''
        local psty `pstyles'
        local OPTS ``el'_msymb' ``el'_msize' ``el'_msang' ``el'_mc' ``el'_barw'
        if "``el'_type'"=="line" {
            if "`PID'"=="split" {
                local plt1 rspike ``EL'_dlo' `Pos' ``EL''
                local plt2 rspike ``EL'_dup' `Pos' ``EL''
                local plt \`plt\`i''
            }
            else if "`left'"!=""   local plt rspike ``EL'_dlo' `Pos' ``EL''
            else if "`right'"!=""  local plt rspike `Pos' ``EL'_dup' ``EL''
            else                   local plt rspike ``EL'_dlo' ``EL'_dup' ``EL''
            forv i = 1/`K_`el'' {
                if "`psty'"=="" local psty `pstyles'
                gettoken p psty : psty
                _getcolr `i' ``eL'colors'
                local popts `axes' `rvert' pstyle(p`p') `p_`i'' `colr' `OPTS'/*
                    */ ``el'2' `p_`i'`eL''
                local p`el'_`i' (`plt' if ``PID'id'==`i', `popts')
            }
        }
        else if "``el'_type'"=="bline" {
            tempname `EL'POS0 `EL'POS1
            qui gen double ``EL'POS0' = `POS' - (``el'_BARW'/2)
            qui gen double ``EL'POS1' = `POS' + (``el'_BARW'/2)
            local plt rspike ``EL'POS0' ``EL'POS1' ``EL''
            forv i = 1/`K_`el'' {
                if "`psty'"=="" local psty `pstyles'
                gettoken p psty : psty
                _getcolr `i' ``eL'colors'
                local popts `axes' `rvert' pstyle(p`p') `p_`i'' `colr' `OPTS'/*
                    */ ``el'2' `p_`i'`eL''
                local p`el'_`i' (`plt' if ``PID'id'==`i', `popts')
            }
        }
        else {
            if "`el'"=="mean" {
                if `"`mean_msymb'"'=="" {
                    if "`box_type'"=="spike" {
                        local OPTS `OPTS' msymbol(pipe)
                        if `"`mean_msize'"'=="" local OPTS `OPTS' msize(huge)
                        if "`vertical'"!="" & `"`mean_msang'"'==""/*
                            */ local OPTS `OPTS' msangle(90)
                    }
                    else local OPTS `OPTS' msymbol(X)
                }
            }
            else {
                if "`box_type'"=="spike" {
                    if `"`median_msymb'"'=="" local OPTS `OPTS' msymbol(O)
                    if `"`median_msize'"'=="" local OPTS `OPTS' msize(vsmall)
                    if `"`median_mc'"'==""    local OPTS `OPTS' mcolor(white)
                }
            }
            if "`vertical'"!="" local vlist ``EL'' `POS'
            else                local vlist `POS' ``EL''
            forv i = 1/`K_`el'' {
                if "`psty'"=="" local psty `pstyles'
                gettoken p psty : psty
                _getcolr `i' ``eL'colors'
                local popts `axes' pstyle(p`p') `p_`i'' `colr' `OPTS'/*
                    */ ``el'2' `p_`i'`eL''
                local p`el'_`i' (scatter `vlist' if ``PID'id'==`i', `popts')
            }
        }
    }
    
    // rag plots
    if "`rag'"!="" {
        local K_rag `k_`PID''
        local psty `pstyles'
        local OPTS `rag_msymb' `rag_msang'
        if `"`rag_msymb'"'=="" {
            local OPTS `OPTS' msymbol(pipe)
            if "`vertical'"!="" & `"`rag_msang'"'==""/*
                */ local OPTS `OPTS' msangle(90)
        }
        if "`vertical'"!="" local vlist `xrag' `RPOS'
        else                local vlist `RPOS' `xrag'
        forv i = 1/`K_rag' {
            if "`psty'"=="" local psty `pstyles'
            gettoken p psty : psty
            _getcolr `i' `ragcolors'
            local popts `axes' pstyle(p`p') `p_`i'' `OPTS' `colr' `rag2'/*
                */ `p_`i'rag'
            local prag_`i' (scatter `vlist' `ragwgt' if ``PID'id'==`i', `popts')
        }
    }
    
    // put together plot command in right order and determine legend keys
    local plots
    local ikeys
    local nkeys 0
    if "`bind'"!="" { // plot by plot
        forv i = 1/`k_`PID'' {
            foreach el of local plotlist {
                local plots `plots' `p`el'_`i''
                local ++nkeys
                if "`key'"=="`el'" local ikeys `ikeys' `nkeys'
                if "`el'"=="whiskers" {
                    local ++nkeys
                    if "`whisk_cap'"!="" local nkeys = `nkeys' + 2
                }
            }
        }
    }
    else { // element by element
        foreach el of local plotlist {
            forv i = 1/`k_`PID'' {
                local plots `plots' `p`el'_`i''
            }
            if "`key'"=="`el'" {
                forv i = 1/`k_`PID'' {
                    local ++nkeys
                    local ikeys `ikeys' `nkeys'
                    if "`el'"=="whiskers" {
                        local ++nkeys
                        if "`whisk_cap'"!="" local nkeys = `nkeys' + 2
                    }
                }
            }
            else local nkeys = `nkeys' + `K_`el''
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
                */ ylabels(`ylabels2', axis(2) labsize(medium)/*
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
        if `"`ylabels'"'=="none" & `dscale'>=. {
            local ylabels
            local noitick
            if "`absolute'"!="" local yti "Frequency"
            else                local yti "Density"
        }
        else {
            local tstyle notick labgap(tiny) nogrid
            if "`vertical'"!="" {
                local ylabels xlabels(`ylabels', `tstyle')
                local noitick noixtick
            }
            else {
                local ylabels ylabels(`ylabels', `tstyle' angle(0))
                local noitick noiytick
            }
        }
    }
    if "`pad'"!="0" {
        if "`vertical'"!="" local ylabels `ylabels' xscale(range(`csrange'))
        else                local ylabels `ylabels' yscale(range(`csrange'))
    }
    
    // compile legend option
    local legend
    if ("`split'"!="") |/*
        */ (`k_`PLOT''>1 & (`k_`CLUS''>1 | "`overlay'"!="") & !`stack') {
        foreach i of local ikeys {
            gettoken lab `PID'lbls : `PID'lbls
            local legend `legend' `i' `"`lab'"'
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
    else if `"`yti'"'=="" local yti `""""'
    if "`vertical'"!="" local yti xtitle(`yti')
    else                local yti ytitle(`yti')
    
    // graph
    if `"`addplot'"'!="" {
        local addplot `"|| `addplot' ||"'
    }
    twoway `plots' `addplot', `xti' `yti' `legend' `ylabels' `byopt' `options'
    
    // display
    if "`table'"!="" {
        if "`by'`over'`split'"!="" {
            di ""
            forv eq = 1/`neq' {
                di as txt "{ralign 13:`eq'}" as txt `": `eq`eq''"'
            }
        }
        matlist `STATS', border(rows)
    }
    
    // returns
    return scalar neq = `neq'
    forv eq = `neq'(-1)1 {
        return local eq`eq' `"`EQ`eq''"'
    }
    return local legend `"`legend'"'
    return matrix table = `STATS'
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
    c_local split_offset `offset'    // split(, offset()) is old syntax
end

program _parse_fill
    syntax [, Select(numlist int >0) Line FColor(passthru) LColor(passthru) * ]
    c_local fill_select `select'
    c_local fill_line   `line'
    c_local fill_fc     `fcolor'
    c_local fill_lc     `lcolor'
    c_local fill2       `options'
end

program _parse_whisk
    syntax [, Statistics(str) NOCAP cap CAP2(numlist max=1) * ]
    if `"`statistics'"'!="" {
        _parse_stats `statistics' // stats, nstats
        if `nstats'>2 {
            di as err "whiskers(statistics()): too many statistics specified"
            exit 198
        }
        else if `nstats'<2 {
            di as err "whiskers(statistics()): two statistics required"
            exit 198
        }
    }
    if "`cap2'"!="" local cap cap
    if "`cap'"!="" & "`nocap'"!="" {
        di as err "whiskers(): cap and nocap not both allowed"
        exit 198
    }
    c_local whisk_stat `"`stats'"'
    c_local whisk_nocap `nocap'
    c_local whisk_cap   `cap'
    c_local whisk_capw  `cap2'
    c_local whiskers2   `options'
end

program _parse_box
    syntax [, Statistics(str) Type(str) LIMits(numlist max=2 missingok)/*
        */ FColor(passthru) LPattern(passthru) LWidth(passthru)/*
        */ LColor(passthru) BARWidth(numlist max=1) * ]
    capt n _parse_box_type, `type'
    if _rc {
        di as err "error in option box(type())"
        exit _rc
    }
    if `"`statistics'"'!="" {
        _parse_stats `statistics' // stats, nstats
        if `nstats'>2 {
            di as err "box(statistics()): too many statistics specified"
            exit 198
        }
        else if `nstats'<2 {
            di as err "box(statistics()): two statistics required"
            exit 198
        }
    }
    c_local box_limits "`limits'"
    c_local box_stat   `"`stats'"'
    c_local box_type   `type'
    c_local box_fc     `fcolor'
    c_local box_lp     `lpattern'
    c_local box_lw     `lwidth'
    c_local box_lc     `lcolor'
    c_local box_BARW   `barwidth'
    c_local box2       `options'
end

program _parse_box_type
    syntax [, SPike Bar Fill Lines ]
    local type `spike' `bar' `fill' `lines'
    if `:list sizeof type'>1 {
        di as err "too many types specified"
        exit 198
    }
    c_local type `type'
end

program _parse_med
    gettoken med 0 : 0, parse(,)
    syntax [, Statistic(str) Type(str) Msymbol(passthru) MSIze(passthru)/*
        */ MSAngle(passthru) MColor(passthru) BARWidth(numlist max=1) * ]
    capt n _parse_med_type, `type'
    if _rc {
        di as err "error in option `med'(type())"
        exit _rc
    }
    _parse_stats `statistic' // stats, nstat
    if `nstats'>1 {
        di as err "`med'(statistic()): only one statistic allowed"
        exit 198
    }
    c_local `med'_stat  `"`stats'"'
    c_local `med'_type  `type'
    c_local `med'_msymb `msymbol'
    c_local `med'_msize `msize'
    c_local `med'_msang `msangle'
    c_local `med'_mc    `mcolor'
    c_local `med'_BARW  `barwidth'
    c_local `med'2      `options'
end

program _parse_med_type
    syntax [, Marker Bline Line  ]
    local type `marker' `bline' `line'
    if `:list sizeof type'>1 {
        di as err "too many types specified"
        exit 198
    }
    c_local type `type'
end

program _parse_stats
    local j 0
    local stats
    local space
    while (`"`0'"'!="") {
        gettoken s 0 : 0, bind
        gettoken next : 0, match(par)
        if "`par'"=="(" {
            gettoken next 0 : 0, match(par)
            local s `"`s'(`next')"'
        }
        local ++j
        local s: subinstr local s " " "", all
        local stats `"`stats'`space'`s'"'
        local space " "
    }
    c_local nstats `j'
    c_local stats `"`stats'"'
end

program _parse_rag
    syntax [, OFFset(numlist) Unique noWeight/*
        */ SPread SPread2(numlist max=2 missingokay)/*
        */ STack STack2(str)/*
        */ Left Right/*
        */ BOUTsides OUTsides/*
        */ Msymbol(passthru) MSAngle(passthru) * ]
    // spread
    if "`spread'`spread2'"!="" {
        if `"`stack'`stack2'"'!="" {
            di as err "rag(): spread() and stack() not both allowed"
            exit 198
        }
        gettoken spread spread2 : spread2
        gettoken spread2        : spread2
        if "`spread'"!="" {
            if `spread'>=. local spread 1
            capt n numlist "`spread'", range(>=.001 <=100) 
            if _rc==1 exit _rc
            if _rc {
                di as err "rag(): error in spread()"
                exit _rc
            }
            local spread `r(numlist)'
        }
        else local spread 1
        local spread = 100/`spread'
        if "`spread2'"=="" local spread2 .
    }
    else if `"`stack'`stack2'"'!="" {
        if "`unique'"!="" {
            di as err "rag(): stack() and unique not both allowed"
            exit 198
        }
        local stack = substr(`"`stack2'"',1,1)=="*"
        if `stack' local stack2 = strtrim(substr(`"`stack2'"',2,.))
        if `"`stack2'"'!="" {
            capt n numlist "`stack2'", max(1) missingok
            if _rc==1 exit _rc
            if _rc {
                di as err "rag(): error in stack()"
                exit _rc
            }
            local stack2 `r(numlist)'
            if `stack2'>=. local stack 0
            else           local ++stack
        }
        else local stack2 .
        /* stack = 1: fixed value, stack = 2: factor; stack = 0: missing */
    }
    // spread
    if      "`boutsides'"!="" local outsides b
    else if "`outsides'"!=""  local outsides w
    c_local rag_offset   `offset'
    c_local rag_unique   `unique'
    c_local rag_noweight `weight'
    c_local rag_spread   `spread'
    c_local rag_spread2  `spread2'
    c_local rag_stack    `stack'
    c_local rag_stack2   `stack2'
    c_local rag_left     `left'
    c_local rag_right    `right'
    c_local rag_outsides `outsides'
    c_local rag_msymb    `msymbol'
    c_local rag_msang    `msangle'
    c_local rag2         `options'
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
    syntax [, Line Fill Whiskers Box MEDian mean rag ]
    c_local elist `line' `fill' `whiskers' `box' `median' `mean' `rag'
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
    if `"`0'"'=="box" local 0 box_l box_u
    if `"`0'"'==substr("whiskers",1,max(4,strlen(`"`0'"')))/*
        */ local 0 whisk_l whisk_u
    _parse_stats `0' // stats, nstat
    if `nstats'>2 {
        di as err "rage() invalid -- too many elements"
        exit 198
    }
    gettoken lb stats : stats
    gettoken ub       : stats
    if `"`lb'"'=="." local lb
    if `"`ub'"'=="." local ub
    c_local pdf_l `"`lb'"'
    c_local pdf_u `"`ub'"'
end

program _parse_dscale
    syntax [anything] [, Individual Group Plot Subgraph ]
    local dstype `individual' `group' `plot' `subgraph'
    if `:list sizeof dstype'>1 {
        di as err "dscale(): only one scaling option allowed"
        exit 198
    }
    if `"`anything'"'!="" {
        capt numlist `"`anything'"', range(>0) missingokay
        if _rc {
            di as err "dscale(): invalid number"
            exit 198
        }
    }
    else local anything 1
    if `anything'>=. local dstype // no rescaling
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
        if      "`el'"=="median" local el "med"
        else if "`el'"=="mean"   local el "mean"
        else if "`el'"=="rag"    local el "rag"
        else                     local el = substr("`el'", 1, 1)
    }
    gettoken p 0 : 0
    syntax [, p`p'`el'(str) * ]
    c_local p_`p'`el' `p`p'`el''
    c_local options `options'
end

program _getcolr
    gettoken i colors : 0
    local color: word `i' of `colors'
    if `"`color'"'!="" local color color(`"`color'"')
    c_local colr `"`color'"'
end

program _makeiff
    args touse TOUSE j j0 by bystr bylev o o0 over ostr olev s split sstr slev
    local iff
    local lbl
    local LBL
    local totl 0
    if "`by'"!="" {
        if `j'<=`j0' { // (i.e. if not total)
            if `bystr' local iff `"`by'==`"`bylev'"'"'
            else       local iff `"`by'==`bylev'"'
            local lbl `"`by' = {bf:`bylev'}"'
            local LBL `"`by' = `bylev'"'
        }
        else local totl 1
    }
    if "`over'"!="" {
        if `o'<=`o0' { // (i.e. if not total)
            if `"`iff'"'!="" local iff `"`iff' & "'
            if `ostr' local iff `"`iff'`over'==`"`olev'"'"'
            else      local iff `"`iff'`over'==`olev'"'
            if `"`lbl'"'!="" {
                local lbl `"`lbl', "'
                local LBL `"`LBL', "'
            }
            local lbl `"`lbl'`over' = {bf:`olev'}"'
            local LBL `"`LBL'`over' = `olev'"'
        }
        else local totl 1
    }
    if "`split'"!="" {
        if `"`iff'"'!="" local iff `"`iff' & "'
        if `sstr' local iff `"`iff'`split'==`"`slev'"'"'
        else      local iff `"`iff'`split'==`slev'"'
        if `"`lbl'"'!="" {
            local lbl `"`lbl', "'
            local LBL `"`LBL', "'
        }
        local lbl `"`lbl'`split' = {bf:`slev'}"'
        local LBL `"`LBL'`split' = `slev'"'
    }
    if `"`iff'"'!="" {
        qui replace `TOUSE' = (`touse' & `iff')
        c_local tousej `TOUSE'
        c_local iff `"if `TOUSE'"'
    }
    else {
        c_local tousej `touse'
        c_local iff "if `touse'"
    }
    if `totl' {
        if `"`lbl'"'=="" {
            local lbl "Total"
            local LBL "Total"
        }
    }
    c_local eqlbl `"`lbl'"'
    c_local eqLBL `"`LBL'"'
end

program _addobs
    args b touse tousej
    local a = _N + 1
    qui set obs `b'
    qui replace `touse' = 0 in `a'/`b'
    if "`tousej'"!="`touse'" {
        qui replace `tousej' = 0 in `a'/`b'
    }
end

program Fit_PDF
    tempname ll ul wd0 lb ub wd
    args xvar wgt iff tight S exact n pdfopts
    qui dstat pdf `xvar' `iff' `wgt', nose noheader notable/*
        */ `tight' `exact' `n' `pdfopts'
    local n: colsof e(b)
    scalar `ll' = el(e(at),1,1)
    scalar `ul' = el(e(at),1,`n')
    scalar `wd0' = `ul' - `ll'
    scalar `lb' = `S'[1,7]
    scalar `ub' = `S'[1,8]
    if `lb'<. {
        if `lb'>`ub' { // swap
            scalar `lb' = `S'[1,8]
            scalar `ub' = `S'[1,7]
        }
        if `lb'>`ll' scalar `ll' = `lb'
    }
    if `ub'<`ul'     scalar `ul' = `ub'
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

program Fit_stats, rclass
    args stats xvar wgt iff qdef box_limits whisk pdf_l pdf_u
    // compile list of target statistics
    local nstats 4
    local haswhisk 0
    if `"`whisk'"'!="" {
        local stats `"`stats' `whisk'"'
        local nstats = `nstats' + 2
        local haswhisk 1
    }
    foreach l in l u {
        local haspdf_`l' 0
        if `"`pdf_`l''"'=="" continue
        if inlist(`"`pdf_`l''"',"box_l","box_u","whisk_l","whisk_u") continue
        capt confirm number `pdf_`l''
        if !_rc continue
        local stats `"`stats' `pdf_`l''"'
        local ++nstats
        local haspdf_`l' 1
    }
    if `"`:list dups stats'"'!="" local noclean noclean
    else                          local noclean
    // compute statistics
    qui dstat (`stats') `xvar' `iff' `wgt', nose noheader notable/*
        */ `noclean' `qdef'
    // copy results
    tempname b S
    matrix `b' = e(b)
    if colsof(`b')!=`nstats' {
        di as err "unexpected error; wrong number of summary statistics computed"
        exit 499
    }
    matrix `S' = J(1,8,.)
    local nstats 4
    matrix `S'[1,1] = `b'[1,1..4]
    if `haswhisk' {
        local nstats 6
        matrix `S'[1,5] = `b'[1,5..6]
    }
    local j 6
    foreach l in l u {
        local ++j
        if !`haspdf_`l'' continue
        local ++nstats
        matrix `S'[1,`j'] = `b'[1,`nstats']
    }
    // apply limits() to box
    if "`box_limits'"!="" { 
        local j 2
        foreach l of local box_limits {
            local ++j
            if `l'>=. continue
            mat `S'[1,`j'] = `l'
        }
    }
    // whiskers
    if !`haswhisk' {
        matrix `S'[1,5] = `S'[1,3]-1.5*(`S'[1,4]-`S'[1,3])
        matrix `S'[1,6] = `S'[1,4]+1.5*(`S'[1,4]-`S'[1,3])
        local ifrng inrange(`xvar', `S'[1,5], `S'[1,6])
        if `"`iff'"'!="" local ifrng `"`iff' & `ifrng'"'
        else             local ifrng `"if `ifrng'"'
        su `xvar' `ifrng', meanonly
        mat `S'[1,5] = r(min)
        mat `S'[1,6] = r(max)
    }
    // pdf range
    local j 0
    foreach l in l u {
        local ++j
        if `haspdf_`l''      continue
        if `"`pdf_`l''"'=="" continue
        if      `"`pdf_`l''"'=="box_l"   matrix `S'[1,6+`j'] = `S'[1,3]
        else if `"`pdf_`l''"'=="box_u"   matrix `S'[1,6+`j'] = `S'[1,4]
        else if `"`pdf_`l''"'=="whisk_l" matrix `S'[1,6+`j'] = `S'[1,5]
        else if `"`pdf_`l''"'=="whisk_u" matrix `S'[1,6+`j'] = `S'[1,6]
        else                             matrix `S'[1,6+`j'] = `pdf_`l''
    }
    // returns
    return scalar N = e(N)
    return scalar sum_w = e(W)
    matrix rown `S' = `xvar'
    return matrix stats = `S'
end

program _dscale
    syntax varlist [if] [in], dscale(str)
    gettoken dup dlo : varlist
    gettoken dlo pos : dlo
    gettoken pos     : pos
    if `dscale'<. {
        su `dup' `if', meanonly
        qui replace `dup' = `dup' * (`dscale' / r(max)) `if' `in'
    }
    qui replace `dup' = `dup'   + `pos' `if' `in'
    qui replace `dlo' = 2*`pos' - `dup' `if' `in'
end

program _get_split_offset
    local 0: list sort 0
    local n: list sizeof 0
    local min: word 1 of `0'
    local max: word `n' of `0'
    c_local offset = (`max' - `min' + 1)/100
end

version 15
mata:
mata set matastrict on

void _copyrag(real scalar a, real scalar b, string scalar out,
    real scalar uniq, string scalar wvar, string scalar S)
{
    real scalar    n, wmean, wrag
    real rowvector lim
    real colvector x, w, p

    // get data
    x = st_data(., st_local("xvar"), st_local("tousej"))
    x = x[p = selectindex(x:<.)]
    if (wvar!="") {
        w = abs(st_data(., wvar, st_local("tousej"))[p])
        wmean = mean(w)
    }
    if (uniq) {
        if (wvar!="") _copyrag_unique(x, w)
        else          x = mm_unique(x)
    }
    if (out!="") {
        if (out=="b") lim = st_matrix(S)[(3,4)]
        else          lim = st_matrix(S)[(5,6)]
        p = selectindex(x:<lim[1] :| x:>lim[2])
        if (!length(p)) p = J(0,1,.)
        x = x[p]
        if (wvar!="") w = w[p]
    }
    n = rows(x)
    if (!n) { // nothing to store
        _st_store(a+1, st_varindex(st_local("rtag")), 2) // tag last row
        return
    }
    // reorder data in case of weights (so that large dots will be at the back)
    if (wvar!="") {
        p = order((w,(1::n)), (-1,2)) // keep original order within ties
        _collate(w, p)
        _collate(x, p)
    }
    // store data
    if ((a+n+1)>b) {
        // update range and add observations if necessary
        b = a + n+1
        st_local("b", strofreal(b))
        if (b>st_nobs()) {
            stata(st_local("preserve"))
            stata("local preserve")
            stata("_addobs "+st_local("b")+" "+st_local("touse")+" "+
                st_local("tousej"))
        }
    }
    st_store((a, a+n-1), st_local("xrag"), x)
    _st_store(a+n+1, st_varindex(st_local("rtag")), 2) // tag last row
    if (wvar!="") {
        wrag = st_varindex(st_local("wrag"))
        st_store((a, a+n-1), wrag, w)
        /* including mean(w) and mean(w)*100 among the weights ensures that the
           average weight corresponds to the default (unweighted) marker size
           (unless the maximum weight is larger than 100 times the average);
           inclusion of mean(w) is needed because options unique and
           [b]outsides may lead to a situation in which all remaining weights
           are larger than mean(w) */
        _st_store(a+n, wrag, wmean)
        _st_store(a+n+1, wrag, wmean*100) 
    }
}

void _copyrag_unique(real colvector x, real colvector w)
{
    real scalar    i, a, b
    real colvector p
    
    p = order(x,1)
    _collate(x, p)
    _collate(w, p)
    p = selectindex(_mm_unique_tag(x))
    a = rows(x) + 1
    for (i = rows(p);i;i--) {
        b = a - 1
        a = p[i]
        w[a] = sum(w[|a \ b|])
    }
    x = x[p]
    w = w[p]
}

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

void _rag_stack(real scalar a0, real scalar b0, real scalar typ,
    real scalar val, real scalar dir)
{   // typ=1: val contains fixed step size
    // typ=2: val contains factor; scale automatic step size by val
    // typ=0: val is missing; use automatic step size
    real scalar    j, d, n, sgn
    real scalar    xrag, rpos, spid
    real colvector D, X
    real matrix    idx
    
    xrag = st_varindex(st_local("xrag"))
    rpos = st_varindex(st_local("RPOS"))
    spid = st_varindex(st_local("splitid"))
    if (typ!=1) {
        d = max(abs(st_data((a0,b0), st_local("dup")) - 
                    st_data((a0,b0), st_local("dlo")))) / 2
        n = 5
    }
    idx = _get_index(a0, b0, "rtag", 2)
    D   = J(b0-a0+1,1,.)
    for (j=rows(idx);j;j--) {
        if (idx[j,1]>idx[j,2]) continue // no data
        if (dir) sgn = (-1)^(_st_data(idx[j,1], spid)!=1) * dir
        else     sgn = 0
        st_view(X=., idx[j,], xrag)
        D[|idx[j,]':+(1-a0)|] = _rag_stack_offset(X, sgn, n)
    }
    if (typ!=1) {
        d = d / (n - 1) * .8
        if (d>=.) d = 0
        printf("{txt}(stacked rag: step size set to %g", d)
        if (typ) {
            d = d * val
            printf(" * %g", val)
        }
        printf(")\n")
    }
    else d = val
    st_store((a0,b0), rpos, st_data((a0,b0), rpos) + D * d)
}

real _rag_stack_offset(real colvector X, real scalar sgn, real scalar nmax)
{   // updates nmax
    real scalar    j, n, a, b
    real colvector L, d, D, p

    // sort order
    p = order((X,(1::rows(X))), (1,2))
    // identify groups (X is sorted)
    L = 0 \ selectindex(_mm_unique_tag(X[p], 1))
    // generate offsets
    j = rows(L)
    D = J(L[j],1,.)
    a = L[j--] + 1
    while (j) {
        b = a - 1
        a = L[j--] + 1
        n = b - a + 1
        nmax = max((nmax, n))
        if (sgn) {
            // asymmetric stack
            D[|a\b|] = (0::n-1) * sgn
        }
        else {
            // symmetric stack: generate index that forms groups of two
            // (and a group of 1 with index 0 if n is uneven) and make one of
            // each group negative
            d = floor(((0::n-1):+mod(n,2))/2) :+ !mod(n,2)
            D[|a\b|] = d :* (-1):^mod(1::n,2)[order((d,uniform(n,1)),(1,2))]
        }
    }
    // return offsets
    return(D[invorder(p)])
}

void _rag_spread(real scalar a0, real scalar b0, real scalar s1, real scalar s2,
    real scalar dir)
{
    real scalar    j, n, sgn
    real scalar    xrag, spid, wrag, dlo, dup, at
    real colvector D, d, X, a
    real matrix    idx, pdf
    
    xrag = st_varindex(st_local("xrag"))
    spid = st_varindex(st_local("splitid"))
    if (st_local("wrag")!="") wrag = st_varindex(st_local("wrag"))
    idx = _get_index(a0, b0, "rtag", 2)
    if (s2>=.) {
        pdf = _get_index(a0, b0, "tag", 1)
        dlo  = st_varindex(st_local("dlo"))
        dup  = st_varindex(st_local("dup"))
        at   = st_varindex(st_local("at"))
    }
    st_view(D=., ., st_varindex(st_local("RPOS")))
    for (j=rows(idx);j;j--) {
        n = idx[j,2] - idx[j,1] + 1
        if (!n) continue
        if (dir) sgn = (-1)^(_st_data(idx[j,1], spid)!=1) * dir
        if (wrag<. | s2>=.) st_view(X=., idx[j,], xrag)
        if (wrag<.) a = rowmin((J(n,1,1e5),rowmax((J(n,1,1),
            s1 :* (st_data(idx[j,], wrag) / _st_data(idx[j,2]+1, wrag))))))
        else a = J(n, 1, s1)
        if (s2<.) d = s2
        else d = editmissing(mm_ipolate(st_data(pdf[j,], at),
            st_data(pdf[j,], dup)-st_data(pdf[j,], dlo), X), 0)
        if (dir) d = d :* (sgn * abs(rbeta(1,1,a,a):-.5))
        else     d = d :* (          rbeta(1,1,a,a):-.5)
        D[|idx[j,]'|] = D[|idx[j,]'|] + d
    }
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
    d = st_varindex((st_local("dup"), st_local("at"), st_local("Pos")))
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
    idx = _get_index(a, b, "tag", 1)
    // compute
    for (j=rows(idx);j;j--) {
        if (idx[j,2]<=idx[j,1]) continue // must have at least two obs
        if (length(box)) _ipolate_PDF_box(idx[j,1], idx[j,2], d, box, fill)
        if (length(med)) _ipolate_PDF_line(idx[j,1], idx[j,2], d, med)
        if (length(avg)) _ipolate_PDF_line(idx[j,1], idx[j,2], d, avg)
    }
}

void _ipolate_PDF_box(real scalar a, real scalar b, real rowvector did,
    real rowvector bid, real scalar fill)
{   // did = [d, at, Pos]; bid = [atlo, atup, dlo, dup, dat]
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
{   // did = [d, at, Pos]; lid = [at, dlo, dup]
    real scalar d

    d = mm_ipolate(st_data((a,b), did[2]), st_data((a,b), did[1]),
        _st_data(a, lid[1]))
    _st_store(a, lid[3], d)
    _st_store(a, lid[2], 2*_st_data(a, did[3]) - d)
}

real matrix _get_index(real scalar a, real scalar b, string scalar tag,
    real scalar offset) // returns indices of start and end of each result
{
    real scalar t

    t = st_varindex(st_local(tag))
    return((a-1) :+ (selectindex(st_data((a,b), t):==1),
                     selectindex(st_data((a,b), t):==2) :- offset))
}

void _lswap(string scalar a, string scalar b)
{
    string scalar a0
    
    a0 = st_local(a)
    st_local(a, st_local(b))
    st_local(b, a0)
}

end

