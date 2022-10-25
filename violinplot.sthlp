{smcl}
{* 25oct2022}{...}
{hi:help violinplot}{...}
{right:{browse "https://github.com/benjann/violinplot/"}}
{hline}

{title:Title}

{pstd}{hi:violinplot} {hline 2} Draw violin plots


{title:Syntax}

{pstd}
    Single subgraph / single overlay:

{p 8 15 2}
    {cmd:violinplot} [{cmd:(}]{varlist}[{cmd:)}]
    {ifin} {weight}
    [{cmd:,}
    {help violinplot##opt:{it:options}}
    ]

{pstd}
    Multiple subgraphs / multiple overlays:

{p 8 15 2}
    {cmd:violinplot} {cmd:(}{varlist}{cmd:)} {cmd:(}{varlist}{cmd:)} [{it:...}]
    {ifin} {weight}
    [{cmd:,}
    {help violinplot##opt:{it:options}}
    ]


{synoptset 22 tabbed}{...}
{marker opt}{synopthdr:options}
{synoptline}
{syntab :Main}
{synopt :{opt vert:ical}}vertical plot; default if {cmd:overlay} is specified
    {p_end}
{synopt :{opt hor:izontal}}horizontal plot; default if {cmd:overlay} is omitted
    {p_end}
{synopt :{opt overlay}}create overlays rather than subgraphs
    {p_end}
{synopt :{cmdab:ds:cale(}{it:{help violinplot##dscale:spec}}{cmd:)}}change the
    scaling of the plotted PDFs
    {p_end}

{syntab :Estimation}
{synopt :{cmd:range(}{it:{help violinplot##range:spec}}{cmd:)}}restrict maximum density evaluation range
    {p_end}
{synopt :{cmd:tight}}use tight density evaluation grid
    {p_end}
{synopt :{cmdab:pdf:opts(}{it:{help dstat##densopts:options}}{cmd:)}}density estimation
    options passed through to {helpb dstat}
    {p_end}
{synopt :{cmd:qdef(}{it:{help dstat##quant:#}}{cmd:)}}quantile definition to be used by {helpb dstat}
    {p_end}

{syntab :Labels}
{synopt :{opt lab:els(strlist)}}provide custom labels for ticks or legend keys
    {p_end}
{synopt :{opt bylab:els(strlist)}}provide custom labels for subgraphs or ticks
    {p_end}
{synopt :{cmd:key(}{it:{help violinplot##key:element}}{cmd:)}}plot element to be used in legend keys
    {p_end}

{syntab :Elements}
{synopt :[{cmd:no}]{cmd:line}[{cmd:(}{it:{help violinplot##line:options}}{cmd:)}]}whether
    PDF lines are printed; rendering of PDF lines
    {p_end}
{synopt :{cmd:fill}[{cmd:(}{it:{help violinplot##fill:options}}{cmd:)}]}add
    shading; rendering of shading
    {p_end}
{synopt :[{ul:{cmd:no}}]{cmdab:whisk:ers}[{cmd:(}{it:{help violinplot##whiskers:opts}}{cmd:)}]}whether
    whiskers are printed; rendering of whiskers
    {p_end}
{synopt :[{cmd:no}]{cmd:box}[{cmd:(}{it:{help violinplot##box:options}}{cmd:)}]}whether
    box is printed; rendering of box
    {p_end}
{synopt :[{ul:{cmd:no}}]{cmdab:med:ian}[{cmd:(}{it:{help violinplot##median:opts}}{cmd:)}]}whether
    median is printed; rendering of median
    {p_end}
{synopt :{cmd:mean}[{cmd:(}{it:{help violinplot##mean:options}}{cmd:)}]}add marker for mean;
    rendering of mean
    {p_end}

{syntab :Colors}
{synopt :{cmdab:col:or(}{it:{help violinplot##color:spec}}{cmd:)}}assign colors
    to variables, affecting all elements
    {p_end}
{synopt :{opt lc:olor(spec)}}like {helpb violinplot##color:color()}, but only affecting PDF lines
    {p_end}
{synopt :{opt fc:olor(spec)}}like {helpb violinplot##color:color()}, but only affecting shading
    {p_end}
{synopt :{opt wc:olor(spec)}}like {helpb violinplot##color:color()}, but only affecting whiskers
    {p_end}
{synopt :{opt bc:olor(spec)}}like {helpb violinplot##color:color()}, but only affecting box
    {p_end}
{synopt :{opt medc:olor(spec)}}like {helpb violinplot##color:color()}, but only affecting median
    {p_end}
{synopt :{opt meanc:olor(spec)}}like {helpb violinplot##color:color()}, but only affecting mean
    {p_end}

{syntab :Other}
{synopt :{cmdab:psty:les(}{it:{help violinplot##pstyles:numlist}})}assign plot styles to variables
    {p_end}
{synopt :{cmdab:byopt:s(}{it:{help by_option:byopts}}{cmd:)}}options passed through
    to {helpb by_option:by()}
    {p_end}
{synopt :{it:{help twoway_options}}}general options passed through to {helpb graph twoway}
    {p_end}
{synoptline}
{pstd}
    {cmd:fweight}s, {cmd:pweight}s, and {cmd:iweight}s are allowed; see help {help weight}.


{title:Description}

{pstd}
    {cmd:violinplot} draws so called violin plots that illustrate the distributions
    of the specified variables. A violin plot is an overlay of a (reflected) density
    estimate and a box plot (Hintze and Nelson 1998).

{pstd}
    Alternative implementations of violin plots are provided by command
    {helpb vioplot} (Winter and Nichols 2008) and command {helpb violin}
    (Steichen 1998, 2001). Furthermore, command {helpb joy_plot} (Rios-Avila
    2022) provides functionality for violin plots.


{title:Dependencies}

{pstd}
    {cmd:violinplot} requires {helpb dstat}, {helpb moremata}, {helpb palettes}, and {helpb colrspace}. To install
    these packages, type:

        . {stata ssc install dstat}
        . {stata ssc install moremata}
        . {stata ssc install palettes}
        . {stata ssc install colrspace}


{title:Options}

{dlgtab:Main}

{phang}
    {opt vertical} draws vertical violins. This is the default if {cmd:overlay}
    is specified.

{phang}
    {opt horizontal} draws horizontal violins. This is the default if {cmd:overlay} is
    omitted.

{phang}
    {opt overlay} overlays the results from the individual variables in a group
    and does not create subgraphs if there are multiple groups. The default is to print the
    results from the individual variables next to each other and to create a
    separate subgraph for each group.

{marker dscale}{...}
{phang}
    {opt dscale(spec)} determines how the density curves are scaled. {it:spec} is

            [{it:#}][{cmd:,} {it:option}]

{pmore}
    where {it:#}>0 is a custom factor by which the default scaling will
    be multiplied and {it:option} is either {cmdab:i:ndividual} to determine
    the scaling for each variable individually or {cmdab:g:roup} to
    determine the scaling by group. If {it:option} is omitted, the scaling is
    determined jointly across all variables.

{dlgtab:Estimation}

{marker range}{...}
{phang}
    {cmd:range(}{it:from} [{it:to}]{cmd:)} limits the maximum range across which
    the PDF will be evaluated. This may be useful if there are outliers in the
    data. {it:from} can be specified as {cmd:.} (missing) to set no lower
    limit; {it:to} can be omitted or specified as {cmd:.} (missing) to set no
    upper limit.

{pmore}
    Depending on situation, it may be necessary to specify
    {cmd:pdfopts(exact)} if {cmd:range()} is applied. By default,
    {helpb dstat} uses a linear approximation grid for density estimation, and
    only few points of the grid may lie within the specified range. Option
    {cmd:pdfopts(exact)} will cause {helpb dstat} to use exact estimation.

{phang}
    {cmd:tight} limits the density evaluation to the range between the observed
    minimum to the observed maximum of the data. By default, the range across
    which the density is evaluated is slightly larger than the observed range
    of the data. Specify {cmd:tight} to remove this extra padding. In any case,
    any the evaluation grid will not go beyond the limits specified by
    {cmd:range()}.

{phang}
    {opt pdfopts(options)} provides density estimation options to be passed through to
    {helpb dstat}; see {it:{help dstat##densopts:density_options}}
    in help {helpb dstat}. The same options will be applied to all variables.

{phang}
    {opt qdef(#)} sets the quantile definition to be used when computing the median
    and the quartiles for the box and whiskers; see option {helpb dstat##quant:qdef()} in
    help {helpb dstat}.

{dlgtab:Labels}

{phang}
    {opt lab:els(strlist)} provides a space separated list of custom labels
    for the ticks (if {cmd:overlay} is omitted) or the legend keys (if {cmd:overlay}
    is specified). Enclose labels that contain spaces in double quotes.

{phang}
    {opt bylabels(strlist)} provides a space separated list of custom titles
    for the subgraphs (if {cmd:overlay} is omitted) or labels for the ticks
    (if {cmd:overlay} is specified). Enclose labels that contain spaces in
    double quotes.

{marker key}{...}
{phang}
    {opt key(element)} selects the plot element to be used in legend
    keys. This is only relevant in cases in which a legend is
    displayed. Depending on context, {it:element} is one of {cmdab:line},
    {cmd:fill}, {cmdab:whisk:ers}, {cmd:box}, {cmdab:med:ian}, or {cmd:mean} (elements
    that are not plotted will not be available). The default is
    {cmd:key(line)} or, if option {cmd:noline} has been specified,
    {cmd:key(fill)}.

{dlgtab:Elements}

{marker line}{...}
{phang}
    [{cmd:no}]{cmd:line}[{cmd:(}{it:options}{cmd:)}] determines
    whether the PDF lines are printed or not. The default is to print the lines; type
    {cmd:noline} to omit the lines. Specify {it:options} to affect the rendering of
    the lines; see help {it:{help connect_options}}. {cmd:noline} implies {cmd:fill}.

{marker fill}{...}
{phang}
    {cmd:fill}[{cmd:(}{it:options}{cmd:)}] adds shading to the PDF. {it:options}
    are

            {opth sel:ect(numlist)} {it:{help area_options}}

{pmore}
    where {cmd:select()} restricts the set of variables to be affected
    and {it:{help area_options}} determine the rendering of the shading. By
    default, shading is added to all variables if option {cmd:fill} is
    specified. Type, for example, {cmd:select(2 5)} to add shading only to
    the 2nd and 5th variable (the counting is across all variables, not within
    group). Option {cmd:select()} is not allowed if {cmd:noline} is
    specified. By default, option {cmd:fintensity(50)} is applied
    to the shading; this is skipped if any {it:{help area_options}} are
    specified. Furthermore, option {cmd:lcolor(%0)} is applied, which will not
    be skipped; specify, e.g., {cmd:fill(lcolor(%100)}} to override this
    setting.

{marker whiskers}{...}
{phang}
    [{cmd:no}]{cmdab:whiskers}[{cmd:(}{it:options}{cmd:)}] determines
    whether the whiskers are printed or not. The default is to print the whiskers
    unless option {cmd:overlay} is specified. Specify {it:options} to affect the
    rendering of the whiskers; see help {it:{help line_options}}.

{marker box}{...}
{phang}
    [{cmd:no}]{cmd:box}[{cmd:(}{it:options}{cmd:)}] determines
    whether the IQR box is printed or not. The default is to print the box
    unless option {cmd:overlay} is specified. Specify {it:options} to affect the
    rendering of the whiskers; see help {it:{help line_options}}. Note that
    {helpb twoway_rspike:rspike} is used to generate the box; type
    {cmd:box(recast(rbar))} to change the plot type to
    {helpb twoway_rbar:rbar}. By default, {cmd:lwidth(vthick)} is applied; this
    is skipped if any {it:options} are specified.

{marker median}{...}
{phang}
    [{cmd:no}]{cmdab:med:ian}[{cmd:(}{it:options}{cmd:)}] determines whether
    a marker for the median is printed or not. The default is to print the
    marker. Specify {it:options} to affect the rendering; see help
    {it:{help marker_options}}. By default, {cmd:msymbol(O)} and
    {cmd:msize(vsmall)} are applied unless {cmd:nobox} is specified, and
    {cmd:mcolor(white)} is applied unless {cmd:nobox} or {cmd:medcolor()} is
    specified. These defaults are skipped if any {it:options} are specified.

{marker mean}{...}
{phang}
    {cmd:mean}[{cmd:(}{it:options}{cmd:)}] prints a marker for the mean. Specify
    {it:options} to affect the rendering; see help
    {it:{help marker_options}}. By default, {cmd:msymbol(pipe)},
    {cmd:msize(huge)}, and, depending on context, {cmd:msangle(90)} are
    applied; this is skipped if any {it:options} are specified.

{dlgtab:Colors}

{marker color}{...}
{phang}
    {opt color(spec)} assigns colors to the variables, affecting all elements
    of the violin plot (except the median depending on context). The colors will
    be recycled across groups. {it:spec} is

            {it:{help colorpalette##palette:palette}} [{cmd:,} {it:{help colorpalette##opts:palette_options}}]

{pmore}
    where {it:palette} is any palette or color specification compatible with
    {helpb colorpalette} and {it:palette_options} are options passed through to
    {helpb colorpalette}. The correct number of colors will be retrieved automatically;
    option {cmd:n()} is not allowed. Specify a single color to use the same color
    for all variables.

{phang}
    {opt lcolor(spec)} is like {helpb violinplot##color:color()}, but only affects the PDF lines.

{phang}
    {opt fcolor(spec)} is like {helpb violinplot##color:color()}, but only affects the shading.

{phang}
    {opt wcolor(spec)} is like {helpb violinplot##color:color()}, but only affects the  whiskers.

{phang}
    {opt bcolor(spec)} is like {helpb violinplot##color:color()}, but only affects the IQR box.

{phang}
    {opt medcolor(spec)} is like {helpb violinplot##color:color()}, but only affects the median.

{phang}
    {opt meancolor(spec)} is like {helpb violinplot##color:color()}, but only affects the mean.

{dlgtab:Other}

{marker pstyles}{...}
{phang}
    {opth pstyles(numlist)} determines the plot styles applied to the
    variables. If {cmd:pstyles()} is omitted, the default is to use style {cmd:p1}
    for all variables or, if {cmd:overlay} is specified, to use consecutive
    styles across the variables in each group (i.e. {cmd:p1} for the first variable,
    {cmd:p2} for the second variables, etc.). For example, specify
    {cmd:pstyles(5 3 7)} to use {cmd:p5} for the first variable, {cmd:p3} for the second
    variable, and {cmd:p7} for the third variable. The styles will be recycled
    of {it:{help numlist}} contains fewer elements than there are variables per group.

{phang}
    {opt byopts(byopts)} are options passed through to the {cmd:by()} option,
    which is internally used to generate subgraphs; see help {it:{help by_option}}.

{phang}
    {it:twoway_options} are general options passed through to {helpb graph twoway}; see
    help {it:{help twoway_options}}.


{title:Examples}

{pstd}
    Basic examples:

        . {stata sysuse nlsw88}
        . {stata violinplot wage ttl_exp tenure}
        . {stata violinplot wage ttl_exp tenure, nobox}
        . {stata violinplot wage ttl_exp tenure, mean}
{p 8 12 2}
        . {stata violinplot wage ttl_exp tenure, nobox mean(msymbol(X) msize(large) mcolor(maroon))}
        {p_end}
{p 8 12 2}
        . {stata violinplot wage ttl_exp tenure, box(recast(rbar) fintensity(50) barwidth(.15)) median(msymbol(d))}
        {p_end}
        . {stata violinplot wage ttl_exp tenure, vertical}
        . {stata violinplot wage ttl_exp tenure, pstyles(1/3)}
        . {stata violinplot wage ttl_exp tenure, fill}
        . {stata violinplot wage ttl_exp tenure, noline}
        . {stata violinplot wage ttl_exp tenure, fill(select(3))}

{pstd}
    Use option {cmd:pdfopts()} to specify {help dstat##densopts:density estimation options}. For
    example, wages, work experience, and tenure cannot be negative, so let's apply boundary
    correction at zero:

        . {stata violinplot wage ttl_exp tenure, pdf(ll(0))}

{pstd}
    {cmd:violinplot} does not feature and {cmd:over()} option or similar. Use
    command {helpb separate} if you want to display results by subpopulations:

        . {stata separate wage, by(union) veryshortlabel}
        . {stata violinplot wage?}
        . {stata drop wage?}

{pstd}
    Use parentheses to create subgraphs:

        . {stata separate ttl_exp, by(race) veryshortlabel}
        . {stata separate tenure, by(race) veryshortlabel}
{p 8 12 2}
        . {stata violinplot (ttl_exp?) (tenure?), bylabels(ttl_exp tenure) pdf(ll(0))}
        {p_end}
        . {stata drop ttl_exp? tenure?}

{pstd}
    Overlays (whiskers and boxes omitted by default):

        . {stata violinplot wage ttl_exp tenure, pdf(ll(0)) overlay}
{p 8 12 2}
        . {stata violinplot wage ttl_exp tenure, pdf(ll(0)) overlay fill(color(%50)) noline}

{pstd}
    Use parentheses to create multiple overlays:

        . {stata separate ttl_exp, by(race) veryshortlabel}
        . {stata separate tenure, by(race) veryshortlabel}
{p 8 12 2}
        . {stata violinplot (ttl_exp?) (tenure?), pdf(ll(0)) overlay bylabels(ttl_exp tenure)}
        {p_end}
        . {stata drop ttl_exp? tenure?}

{pstd}
    Options {cmd:color()} (all elements), {cmd:lcolor()} (PDF lines), {cmd:bcolor()}
    (boxes), etc. can be used to obtain colors from {helpb colorpalette}:

        . {stata separate wage, by(industry) veryshortlabel}
{p 8 12 2}
        . {stata violinplot wage?*, color(sb muted) nobox nowhiskers median(msymbol(o))}
        {p_end}
{p 8 12 2}
        . {stata violinplot wage?*, color(sb muted) bcolor(sb muted, opacity(50)) nowhiskers}
        {p_end}
{p 8 12 2}
        . {stata violinplot wage?*, color(plasma) fill nobox nowhiskers nomedian}
        {p_end}
        . {stata drop wage?*}

        . {stata separate wage, by(south)}
        . {stata separate ttl_exp, by(south)}
        . {stata separate tenure, by(south)}
        . {stata local legend legend(on order(1 "Not south" 2 "South"))}
{p 8 12 2}
        . {stata local ylabel ylabel(1.5 "wage" 3.5 "ttl_exp" 5.5 "tenure", nogrid)}
        {p_end}
{p 8 12 2}
        . {stata violinplot wage? ttl_exp? tenure?, pdf(ll(0)) color(Set1, select(1 2)) `legend' `ylabel'}
        {p_end}
        . {stata drop wage? ttl_exp? tenure?}


{title:References}

{phang}
    Hintze, J.L., R.D. Nelson
    (1998). {browse "https://doi.org/10.1080/00031305.1998.10480559":Violin Plots: A Box Plot-Density Trace Synergism.} The
    American Statistician 52:181-184.
    {p_end}
{phang}
    Rios-Avila, F. (2022). JOY_PLOT: Stata module to produce joy plots. Available
    from {browse "https://ideas.repec.org/c/boc/bocode/s459066.html"}.
    {p_end}
{phang}
    Steichen, T.J. (1998). gr33: Violin plots. Stata Technical Bulletin 46:13-18.
    {p_end}
{phang}
    Steichen, T.J. (2001). gr33.1: Violin plots for Stata 6 and 7. Stata Technical Bulletin 61:10.
    {p_end}
{phang}
    Winter, N., A. Nichols (2008). VIOPLOT: Stata module to produce violin plots
    with current graphics. Available from
    {browse "https://ideas.repec.org/c/boc/bocode/s456902.html"}.
    {p_end}


{title:Author}

{pstd}
    Ben Jann, University of Bern, ben.jann@unibe.ch

{pstd}
    Thanks for citing this software as follows:

{pmore}
    Jann, B. (2022). violinplot: Stata module to draw violin plots. Available from
    {browse "https://github.com/benjann/violinplot/"}.


{title:Also see}

{psee}
    Online:  help for {helpb graph twoway}, {helpb graph box}

