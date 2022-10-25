*! version 1.0.4  25oct2022  Ben Jann

program violinplot
    version 15
    
    // syntax
    syntax [anything] [if] [in] [pw iw fw] [, ///
        VERTical HORizontal ///
        overlay ///
        DScale(str) ///
        PDFopts(str) tight ///
        qdef(passthru) ///
        range(numlist max=2 missingok) ///
        LABels(str asis) ///
        BYLABels(str asis) ///
        key(str) ///
        NOLINE     LINE     LINE2(str)     LColor(str) ///
        NOFILL     FILL     FILL2(str)     FColor(str) ///
        NOWHISKers WHISKers WHISKers2(str) WColor(str) ///
        NOBOX      BOX      BOX2(str)      BColor(str) ///
        NOMEDian   MEDian   MEDian2(str)   MEDColor(str) ///
        NOMEAN     MEAN     MEAN2(str)     MEANColor(str) ///
        COLor(str) ///
        PSTYles(numlist int >0) ///
        BYOPTs(str) * ]
    // orientation
    if "`vertical'"!="" & "`horizontal'"!="" {
        di as err "vertical and horizontal not both allowed"
        exit 198
    }
    if "`overlay'"!="" {
        if "`horizontal'"=="" local vertical vertical
    }
    // fill option; default is nofill (unless noline is specified)
    if `"`fill2'"'!="" local fill fill
    if "`fill'"!="" {
        if "`nofill'"!="" {
            di as err "fill and nofill not both allowed"
            exit 198
        }
    }
    _parse_fill, `fill2'
    // line option; default is line
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
    // whiskers option: default is whiskers unless overlay has been specified
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
    // box option: default is box unless overlay has been specified
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
    // median option: default is median
    if `"`median2'"'!="" local median median
    if "`median'"!="" {
        if "`nomedian'"!="" {
            di as err "median and nomedian not both allowed"
            exit 198
        }
    }
    if "`nomedian'"=="" local median median
    // mean option: default is nomean
    if `"`mean2'"'!="" local mean mean
    if "`mean'"!="" {
        if "`nomean'"!="" {
            di as err "mean and nomean not both allowed"
            exit 198
        }
    }
    // list of plot types / determine key
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
    // further option
    _parse_range `range' // returns lb ub
    _parse_dscale `dscale' // returns dscale and dstype
    local dscale = `dscale' * 0.5
    
    // parse varlist
    gettoken varlist : anything, match(haspar)
    if "`haspar'"!="" {
        _parse_vlist_by `anything'
    }
    else {
        _parse_vlist `anything'
        local g 1
        local k_1 `k'
        local varlist_1 `varlist'
    }
    
    // generate colors
    if `"`color'"'!="" {
        _parse comma lhs rhs : color
        if `"`rhs'"'=="" local rhs ","
        colorpalette `lhs' `rhs' nograph n(`k')
        if r(n)!=`k' {
            // wrong number of colors, e.g. because select() was applied
            colorpalette `r(p)', nograph n(`k') class(`r(pclass)')
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
            colorpalette `lhs' `rhs' nograph n(`k')
            if r(n)!=`k' {
                // wrong number of colors, e.g. because select() was applied
                colorpalette `r(p)', nograph n(`k') class(`r(pclass)')
            }
            local `opt'color `"`r(p)'"'
        }
    }
    
    // preserve current estimates
    tempname ecurrent
    _estimates hold `ecurrent', restore nullok
    
    // sample and weights
    marksample touse, novarlist
    preserve
    qui keep if `touse'
    if "`weight'"!="" local wgt `"[`weight'`exp']"'
    else              local wgt
    
    // prepare variables for bygroup, id, mean, med, box, whiskers
    tempvar by id avg med p25 p75 wlo wup
    qui gen byte `by' = .
    qui gen byte `id' = .
    foreach v in avg med p25 p75 wlo wup {
        qui gen double ``v'' = .
    }
    
    // prepare tempvars for density estimates
    local d
    local at
    forv i = 1/`k' {
        tempvar tmp
        qui gen double `tmp' = .
        local d `d' `tmp'
        tempvar tmp
        local q `q' `tmp'
        qui gen double `tmp' = .
        tempvar tmp
        qui gen double `tmp' = .
        local at `at' `tmp'
    }
    
    // compute results
    tempname S
    local offset 0
    forv j = 1/`g' {
        local n `k_`j''
        local N = `offset' + `n'
        if `N' > _N qui set obs `N'
        forv i = 1/`k_`j'' {
            local xvar: word `i' of `varlist_`j''
            if "`xvar'"=="" continue, break
            // density estimate
            local dvar:    word `i' of `d'
            local atvar:   word `i' of `at'
            Fit_PDF "`xvar'" `"`wgt'"' "`tight'" "`lb'" "`ub'" `"`pdfopts'"'
            local n = max(`n', colsof(e(b)))
            local N = `offset' + `n'
            if `N' > _N qui set obs `N'
            mata: storepdf(`offset')
            // stats
            qui dstat (mean med p25 p75) `xvar' `wgt', nose `qdef'
            mat `S' = e(b)
            mat `S' = `S', `S'[1,3]-1.5*(`S'[1,4]-`S'[1,3]), /*
                        */ `S'[1,4]+1.5*(`S'[1,4]-`S'[1,3])
            local ii = `offset' + `i'
            qui replace `id'  = `i'      in `ii'
            qui replace `avg' = `S'[1,1] in `ii'
            qui replace `med' = `S'[1,2] in `ii'
            qui replace `p25' = `S'[1,3] in `ii'
            qui replace `p75' = `S'[1,4] in `ii'
            su `xvar' if inrange(`xvar', `S'[1,5], `S'[1,6]), meanonly
            qui replace `wlo' = r(min) in `ii'
            qui replace `wup' = r(max) in `ii'
        }
        local N0 = `offset' + 1
        qui replace `by' = `j' in `N0'/`N'
        local offset = `N' + 1 // insert empty row (missings)
    }
    
    // obtain max of pdf, rescale, mirror, and shift
    if "`dstype'"=="individual" {
        forv j = 1/`g' {
            local i 0
            foreach v of local d {
                local ++i
                if "`overlay'"!="" local pos `j'
                else               local pos `i'
                su `v' if `by'==`j', meanonly
                qui replace `v' = `v' * `dscale' / r(max) + `pos' if `by'==`j'
                local tmp: word `i' of `q'
                qui replace `tmp' = 2*`pos' - `v' if `by'==`j'
            }
        }
    }
    else if "`dstype'"=="group" {
        forv j = 1/`g' {
            local dmax 0
            foreach v of local d {
                su `v' if `by'==`j', meanonly
                local dmax = max(`dmax', r(max))
            }
            local i 0
            foreach v of local d {
                local ++i
                if "`overlay'"!="" local pos `j'
                else               local pos `i'
                qui replace `v' = `v' * `dscale' / `dmax' + `pos' if `by'==`j'
                local tmp: word `i' of `q'
                qui replace `tmp' = 2*`pos' - `v' if `by'==`j'
            }
        }
    }
    else {
        local dmax 0
        foreach v of local d {
            su `v', meanonly
            local dmax = max(`dmax', r(max))
        }
        forv j = 1/`g' {
            local i 0
            foreach v of local d {
                local ++i
                if "`overlay'"!="" local pos `j'
                else               local pos `i'
                qui replace `v' = `v' * `dscale' / `dmax' + `pos' if `by'==`j'
                local tmp: word `i' of `q'
                qui replace `tmp' = 2*`pos' - `v' if `by'==`j'
            }
        }
    }
    
    // settings related to orientation
    if "`vertical'"!="" local rvert horizontal
    else                local rhor  horizontal
    
    // prepare pstyles
    if "`pstyles'"=="" {
        if "`overlay'"=="" local pstyles 1
        else {
            numlist "1/`=min(`k',15)'"
            local pstyles `r(numlist)'
        }
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
        forv j = 1/`g' {
            local psty `pstyles'
            forv i = 1/`k_`j'' {
                local ++ii
                if "`psty'"=="" local psty `pstyles'
                gettoken p psty : psty
                if !`:list ii in fillselect' continue
                local ++keys
                local qvar: word `i' of `q'
                local dvar: word `i' of `d'
                local atvar: word `i' of `at'
                getcolr `i' `fcolor'
                local pfill `pfill' /*
                    */ (rarea `qvar' `dvar' `atvar' if `by'==`j'`pdrop', /*
                    */ `rvert' pstyle(p`p') lc(%0) `finten' `colr' `fill2')
            }
        }
    }
    else if "`fill'"!="" {
        local psty `pstyles'
        forv i = 1/`k' {
            if "`psty'"=="" local psty `pstyles'
            gettoken p psty : psty
            local qvar: word `i' of `q'
            local dvar: word `i' of `d'
            local atvar: word `i' of `at'
            getcolr `i' `fcolor'
            local pfill `pfill' /*
                */ (rarea `qvar' `dvar' `atvar', cmissing(n) `rvert' /*
                */ pstyle(p`p') lc(%0) `finten' `colr' `fill2')
        }
        local keys = `keys' + `k'
    }
    
    // line plots
    if "`key'"=="line" local ikey `keys'
    local pline
    if "`line'"!="" {
        local psty `pstyles'
        forv i = 1/`k' {
            if "`psty'"=="" local psty `pstyles'
            gettoken p psty : psty
            local qvar: word `i' of `q'
            local dvar: word `i' of `d'
            local atvar: word `i' of `at'
            getcolr `i' `lcolor'
            local pline `pline' /*
                */ (rline `qvar' `dvar' `atvar', cmissing(n) `rvert' /*
                */ pstyle(p`p') `colr' `line2')
        }
        local keys = `keys' + `k'
    }
    
    // plot positions for whiskers etc
    if "`overlay'"!="" local pos `by'
    else               local pos `id'
    
    // whisker plots
    if "`key'"=="whiskers" local ikey `keys'
    local pwhisk
    if "`whiskers'"!="" {
        local psty `pstyles'
        forv i = 1/`k' {
            if "`psty'"=="" local psty `pstyles'
            gettoken p psty : psty
            getcolr `i' `wcolor'
            local pwhisk `pwhisk' /*
                */ (rspike `wlo' `p25' `pos' if `id'==`i', `rhor' /*
                */ pstyle(p`p') `colr' `whiskers2') /*
                */ (rspike `p75' `wup' `pos' if `id'==`i', `rhor' /*
                */ pstyle(p`p') `colr' `whiskers2')
        }
        local keys = `keys' + 2*`k'
    }
    
    // box plots
    if "`key'"=="box" local ikey `keys'
    local pbox
    if "`box'"!="" {
        if `"`box2'"'=="" local boxwd lw(vthick)
        else              local boxwd
        local psty `pstyles'
        forv i = 1/`k' {
            if "`psty'"=="" local psty `pstyles'
            gettoken p psty : psty
            getcolr `i' `bcolor'
            local pbox `pbox' /*
                */ (rspike `p25' `p75' `pos' if `id'==`i', `rhor' /*
                */ pstyle(p`p') `boxwd' `colr' `box2')
        }
        local keys = `keys' + `k'
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
        forv i = 1/`k' {
            if "`psty'"=="" local psty `pstyles'
            gettoken p psty : psty
            getcolr `i' `medcolor'
            if `"`colr'"'=="" {
                if "`box'"!="" & `"`median2'"'=="" local colr mcolor(white)
            }
            local pbox `pbox' /*
                */ (scatter `vlist' if `id'==`i', pstyle(p`p') /*
                */ `msym' `colr' `median2')
        }
        local keys = `keys' + `k'
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
        forv i = 1/`k' {
            if "`psty'"=="" local psty `pstyles'
            gettoken p psty : psty
            getcolr `i' `meancolor'
            local pmean `pmean' /*
                */ (scatter `vlist' if `id'==`i', pstyle(p`p') /*
                */ `msopts' `colr' `mean2')
        }
        local keys = `keys' + `k'
    }
    
    // collect labels
    local bylbls
    forv j = 1/`g' {
        gettoken bylab bylabels : bylabels
        if `"`bylab'"'=="" {
            local v: word 1 of `varlist_`j''
            local bylab: var lab `v'
            if `"`bylab'"'=="" local bylab "`v'"
        }
        local bylbls `bylbls' `j' `"`bylab'"'
    }
    if "`overlay'"=="" local ikey 0 // no legend
    local ylabels
    forv i = 1/`k' {
        gettoken ylab labels : labels
        if `"`ylab'"'=="" {
            local v: word `i' of `varlist'
            local ylab: var lab `v'
            if `"`ylab'"'=="" local ylab "`v'"
        }
        local ++ikey
        local ylabels `ylabels' `ikey' `"`ylab'"'
    }
    
    // assign labels
    if "`overlay'"!="" {
        // legend
        if `k'>1 local legend legend(order(`ylabels'))
        else     local legend legend(off)
        // by option
        local byopt
        // axis labels
        if `g'>1 local ylabels `"`bylbls'"'
        else     local ylabels none
        if "`vertical'"!="" local ylabels xlabels(`ylabels')
        else local ylabels ylabels(`ylabels', angle(horizontal)) yscale(reverse)
    }
    else {
        // legend
        local legend legend(off)
        // by option
        if `g'>1 {
            lab def `by' `bylbls', modify
            lab val `by' `by'
            local byopt by(`by', legend(off) note("") `byopts')
        }
        else local byopt
        // axis labels
        if `k'==1 local ylabels none
        if "`vertical'"!="" local ylabels xlabels(`ylabels')
        else local ylabels ylabels(`ylabels', angle(horizontal)) yscale(reverse)
    }
    
    // graph
    twoway `pfill' `pline' `pwhisk' `pbox' `pmed' `pmean' /*
        */ , `legend' `ylabels' `byopt' `options'
end

program _parse_fill
    syntax [, SELect(numlist int >0) *]
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
    syntax [anything] [, Individual Group ]
    if "`individual'"!="" & "`group'"!="" {
        di as err "dscale(): only one of individual and group allowed"
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
    c_local dstype `individual' `group'
end

program _parse_vlist_by
    local i 0
    local kmax 0
    while (`"`0'"'!="") {
        gettoken varlist 0 : 0, match(haspar)
        _parse_vlist `varlist'
        local allvars `allvars' `varlist'
        local ++i
        local k: list sizeof varlist
        local kmax = max(`kmax', `k')
        c_local k_`i' `k'
        c_local varlist_`i' `varlist'
    }
    c_local g `i'
    c_local k `kmax'
    c_local varlist `allvars'
end

program _parse_vlist
    syntax varlist(numeric)
    c_local k: list sizeof varlist
    c_local varlist `varlist'
end

program getcolr
    gettoken i colors : 0
    local color: word `i' of `colors'
    if `"`color'"'!=""  local color color(`"`color'"')
    c_local colr `"`color'"'
end

program Fit_PDF
    args xvar wgt tight lb ub pdfopts
    if "`tight'"!="" {
        su `xvar', meanonly
        local ll = r(min)
        local ul = r(max)
        if "`lb'"!="" {
            if `lb'>`ll' local ll `lb'
        }
        if "`ub'"!="" {
            if `ub'<`ul' local ul `ub'
        }
        qui dstat pdf `xvar' `wgt', nose range(`ll' `ul') `pdfopts'
    }
    else {
        qui dstat pdf `xvar' `wgt', nose `pdfopts'
        if "`lb'`ub'"!="" { // apply range() if necessary
            local ll
            local ul
            local a = el(e(at),1,1)
            local b = el(e(at),1,colsof(e(b)))
            if "`lb'"!="" {
                if `lb'>`a' local ll `lb'
            }
            if "`ub'"!="" {
                if `ub'<`b' local ul `ub'
            }
            if "`ll'`ul'"!="" {
                if      "`ll'"=="" local ll `a'
                else if "`ul'"=="" local ul `b'
                qui dstat pdf `xvar' `wgt', nose range(`ll' `ul') `pdfopts'
            }
        }
    }
end

version 15
mata:
mata set matastrict on

void storepdf(real scalar offset)
{
    real scalar    n, a, b
    real colvector d, at
    
    d  = st_matrix("e(b)")'
    at = st_matrix("e(at)")'
    n = rows(d)
    a = offset + 1
    b = offset + n
    st_store((a,b), st_local("dvar"), d)
    st_store((a,b), st_local("atvar"), at)
}

end


