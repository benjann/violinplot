*! version 1.0.0  20oct2022  Ben Jann

/*
requires: dstat, moremata, palettes, colrspace

syntax:
    single subgraph:
        violinplot varlist [if] [in] [weights] [, options]

    multiple subgraphs:
        violinplot (varlist) (varlist) ... [if] [in] [weights] [, options]

options:
    pdfopts(opts)       options passed though to -dstat pdf-
    range(a b)          limit evaluation range of pdf; should also specify pdf(exact)
    dscale([#][, opts]) # sets a custom scaling of the pdf; default is #=1
                        opts can either be subgraph (determine scaling by subgraph)
                        or individual (determine scaling)
    noline              omit pdf lines
    line(opts)          options affecting rendering of the pdf lines
    lcolor(spec)        set color(s); spec is -palette [, options]- compatible with
                        colorpalette; colors will be expanded/interpolated/recycled
                        to as many colors as are needed; specify single color to use
                        same color for all; colors will be recycled between
                        subgraphs
    fill[(opts)]        add shading to pdf; opts affect the rendering; can also
                        type fill(select(numlist)) to apply shading only to selected
                        variables
    fcolor(spec)        like lcolor(), but for fill
    nowhiskers          omit whiskers
    whiskers(opts)      options affecting rendering of the whiskers
    wcolor(spec)        like lcolor(), but for whiskers
    nobox               omit box
    box(opts)           options affecting rendering of box
    bcolor(spec)        like lcolor(), but for box
    nomedian            omit median
    median(opts)        options affecting rendering of median
    medcolor(spec)      like lcolor(), but for median
    mean[(opts)]        add mean; opts affect the rendering
    meancolor(spec)     like lcolor(), but for mean
    color(spec)         like lcolor(), but affecting all elements (unless element-
                        specific option is specified)
    vertical            draw vertical violins
    labels(strlist)     provide custom labels; default is to use variable labels/names
    bylabels(strlist)   provide titles for subgraphs
    byopts(opts)        by options
    twoway_options      general twoway options

there is no over() option or anything; use command -separate- if you want to
display results by subpopulations; example:

    . sysuse nlsw88
    . separate wage, by(race) veryshortlabel 
    . violinplot wage?

here's another example that illustrates the use of colors from -colorpalette-:

    . sysuse nlsw88
    . separate wage, by(industry) veryshortlabel
    . violinplot wage?*, color(sb muted) nobox nowhiskers median(msymbol(o))
*/

program violinplot
    version 15
    
    // syntax
    syntax [anything] [if] [in] [pw iw fw] [, ///
        PDFopts(str) range(numlist max=2 missingok) DScale(str) ///
        noline     LINE2(str)     LColor(str) ///
        fill       FILL2(str)     FColor(str) ///
        noWHISKers WHISKers2(str) WColor(str) ///
        nobox      BOX2(str)      BColor(str) ///
        noMEDian   MEDian2(str)   MEDColor(str) ///
        mean       MEAN2(str)     MEANColor(str) ///
        COLor(str) ///
        VERTical ///
        LABels(str asis) ///
        BYLABels(str asis) BYOPTs(str) * ]
    if `"`fill2'"'!="" local fill fill
    _parse_fill, `fill2'
    if "`line'" !="" {
        if `"`fillselect'"'!="" {
            di as err "{bf:fill(select())} not allowed if {bf:noline} is specified"
            exit 198
        }
    }
    if `"`mean2'"'!="" local mean eman
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
        local color `"`r(p)'"'
    }
    foreach opt in l f w b med mean  {
        if `"``opt'color'"'=="" {
            if !("`opt'"=="med" & "`box'"=="") {
                local `opt'color `"`color'"'
            }
        }
        else {
            _parse comma lhs rhs : `opt'color
            if `"`rhs'"'=="" local rhs ","
            colorpalette `lhs' `rhs' nograph n(`k')
            local `opt'color `"`r(p)'"'
        }
    }
    
    // preserve current estimates
    tempname ecurrent
    _estimates hold `ecurrent', restore nullok
    
    // sample
    marksample touse, novarlist
    preserve
    qui keep if `touse'
    
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
            qui dstat pdf `xvar', nose `pdfopts'
            if "`lb'`ub'"!="" {
                // apply range if necessary
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
                    qui dstat pdf `xvar', nose range(`ll' `ul') `pdfopts'
                }
            }
            local n = max(`n', colsof(e(b)))
            local N = `offset' + `n'
            if `N' > _N qui set obs `N'
            mata: storepdf(`offset')
            // stats
            qui dstat (mean med p25 p75 min max) `xvar', nose
            mat `S' = e(b)
            local ii = `offset' + `i'
            qui replace `id'  = `i'      in `ii'
            qui replace `avg' = `S'[1,1] in `ii'
            qui replace `med' = `S'[1,2] in `ii'
            qui replace `p25' = `S'[1,3] in `ii'
            qui replace `p75' = `S'[1,4] in `ii'
            qui replace `wlo' = max(`S'[1,3]-1.5*(`S'[1,4]-`S'[1,3]), `S'[1,5]) in `ii'
            qui replace `wup' = min(`S'[1,4]+1.5*(`S'[1,4]-`S'[1,3]), `S'[1,6]) in `ii'
        }
        local N0 = `offset' + 1
        qui replace `by' = `j' in `N0'/`N'
        local offset = `N'
    }
    
    // obtain max of pdf, rescale, mirror, and shift
    if "`dstype'"=="individual" {
        forv j = 1/`g' {
            local i 0
            foreach v of local d {
                local ++i
                su `v' if `by'==`j', meanonly
                qui replace `v' = `v' * `dscale' / r(max) + `i' if `by'==`j'
                local tmp: word `i' of `q'
                qui replace `tmp' = 2*`i' - `v' if `by'==`j'
            }
        }
    }
    else if "`dstype'"=="subgraph" {
        forv j = 1/`g' {
            local dmax 0
            foreach v of local d {
                su `v' if `by'==`j', meanonly
                local dmax = max(`dmax', r(max))
            }
            local i 0
            foreach v of local d {
                local ++i
                qui replace `v' = `v' * `dscale' / `dmax' + `i' if `by'==`j'
                local tmp: word `i' of `q'
                qui replace `tmp' = 2*`i' - `v' if `by'==`j'
            }
        }
    }
    else {
        local dmax 0
        foreach v of local d {
            su `v', meanonly
            local dmax = max(`dmax', r(max))
        }
        local i 0
        foreach v of local d {
            local ++i
            qui replace `v' = `v' * `dscale' / `dmax' + `i'
            local tmp: word `i' of `q'
            qui replace `tmp' = 2*`i' - `v'
        }
    }
    
    // define bylabels
    forv j = 1/`g' {
        gettoken bylab bylabels : bylabels
        if `"`bylab'"'=="" {
            gettoken bylab : varlist_`j'
        }
        lab def `by' `j' `"`bylab'"', modify
    }
    lab val `by' `by'
    
    // define ylabels handle vertical
    local ylabels
    forv i = 1/`k' {
        gettoken ylab labels : labels
        if `"`ylab'"'=="" {
            local v: word `i' of `varlist'
            local ylab: var lab `v'
            if `"`ylab'"'=="" {
                local ylab "`v'"
            }
        }
        local ylabels `ylabels' `i' `"`ylab'"'
    }
    if "`vertical'"!="" {
        local ylabels xlabels(`ylabels')
        local rvert horizontal
    }
    else {
        local rhor horizontal
        local ylabels ylabels(`ylabels', angle(horizontal)) yscale(reverse)
    }
    
    // fill plots
    local pfill
    if "`fillselect'"!="" {
        local ii 0
        forv j = 1/`g' {
            forv i = 1/`k_`j'' {
                local ++ii
                local qvar: word `i' of `q'
                local dvar: word `i' of `d'
                local atvar: word `i' of `at'
                getcolr `i' `fcolor'
                if "`fillselect'"!="" {
                    if !`:list ii in fillselect' continue
                }
                local pfill `pfill' /*
                    */ (rarea `qvar' `dvar' `atvar' if `by'==`j', `rvert' /*
                    */ pstyle(p1) lc(%0) fintensity(50) `colr' `fill2')
            }
        }
    }
    else if "`fill'"!="" {
        forv i = 1/`k' {
            local qvar: word `i' of `q'
            local dvar: word `i' of `d'
            local atvar: word `i' of `at'
            getcolr `i' `fcolor'
            local pfill `pfill' /*
                */ (rarea `qvar' `dvar' `atvar', `rvert' /*
                */ pstyle(p1) lc(%0) fintensity(50) `colr' `fill2')
        }
    }
    
    // line plots
    local pline
    if "`line'"=="" {
        forv i = 1/`k' {
            local qvar: word `i' of `q'
            local dvar: word `i' of `d'
            local atvar: word `i' of `at'
            getcolr `i' `lcolor'
            local pline `pline' /*
                */ (rline `qvar' `dvar' `atvar', `rvert' /*
                */ pstyle(p1) `colr' `line2')
        }
    }
    
    // whisker plots
    local pwhisk
    if "`whiskers'"=="" {
        forv i = 1/`k' {
            getcolr `i' `wcolor'
            local pwhisk `pwhisk' /*
                */ (rspike `wlo' `wup' `id' if `id'==`i', `rhor' /*
                */ pstyle(p1) `colr' `whiskers2')
        }
    }
    
    // box plots
    local pbox
    if "`box'"=="" {
        forv i = 1/`k' {
            getcolr `i' `bcolor'
            local pbox `pbox' /*
                */ (rspike `p25' `p75' `id' if `id'==`i', `rhor' /*
                */ pstyle(p1) lw(vvthick) `colr' `box2')
        }
    }
    
    // median plots
    local pmed
    if "`median'"=="" {
        if "`vertical'"!="" local vlist `med' `id' 
        else                local vlist `id' `med' 
        forv i = 1/`k' {
            getcolr `i' `medcolor'
            if `"`colr'"'=="" {
                if "`box'"=="" local colr color(white)
            }
            local pbox `pbox' /*
                */ (scatter `vlist' if `id'==`i', pstyle(p1) /*
                */ `colr' ms(O) mlcolor(%0) `median2')
        }
    }
    
    // mean plots
    local pmean
    if "`mean'"!="" {
        local msopts msymbol(pipe) msize(huge)
        if "`vertical'"!="" {
            local vlist `avg' `id'
            local msopts `msopts' msangle(90)
        }
        else local vlist `id' `avg' 
        forv i = 1/`k' {
            getcolr `i' `meancolor'
            local pmean `pmean' /*
                */ (scatter `vlist' if `id'==`i', pstyle(p1) /*
                */ `msopts' `colr' `mean2')
        }
    }
    
    // graph
    if `g'>1 local byopt by(`by', legend(off) note("") `byopts')
    else     local byopts
    twoway `pfill' `pline' `pwhisk' `pbox' `pmed' `pmean' /*
        */ , legend(off) `ylabels' `byopt' `options'
end

program _parse_fill
    syntax [, SELect(numlist int >0) *]
    c_local fillselect `select'
    c_local fill2 `options'
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
    syntax [anything] [, Individual Subgraph ]
    if "`individual'"!="" & "`subgraph'"!="" {
        di as err "dscale(): only one of individual and subgraph allowed"
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
    c_local dstype `individual' `subgraph'
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


