{smcl}
{* 10feb2024}{...}
{hi:help violinplot}{...}
{right:{browse "https://github.com/benjann/violinplot/"}}
{hline}

{title:Title}

{pstd}{hi:violinplot} {hline 2} Draw violin plots


{title:Syntax}

{pstd}
    Syntax 1: Single group of outcome variables

{p 8 15 2}
    {cmd:violinplot} [{cmd:(}]{varlist}[{cmd:)}]
    {ifin} {weight}
    [{cmd:,}
    {help violinplot##opt:{it:options}}
    ]

{pstd}
    Syntax 2: Multiple groups of outcome variables

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
{synopt :{opt vert:ical}}vertical violins
    {p_end}
{synopt :{opt hor:izontal}}horizontal violins
    {p_end}
{synopt :{opt l:eft}}print left half of violin
    {p_end}
{synopt :{opt r:ight}}print right half of violin
    {p_end}
{synopt :{opt overl:ay}}create overlays rather than separate violins
    {p_end}
{synopt :{opt aso:ver}}treat variable groups as over categories
    {p_end}
{synopt :{cmdab:o:ver(}{it:{help violinplot##over:spec}}{cmd:)}}display results by
    categories of over-variable
    {p_end}
{synopt :{opt ato:ver}}use values of over categories as plot positions
    {p_end}
{synopt :{opt swap}}swap variables and over categories
    {p_end}
{synopt :{opt nost:ack}}do not use stacked axis labels, use a legend
    {p_end}
{synopt :{opt gap(#)}}size of extra gap between groups of results; default is 0.5
    {p_end}
{synopt :{cmdab:sp:lit(}{it:{help violinplot##split:varname}}{cmd:)}}create half
    violins
    {p_end}
{synopt :{cmd:by(}{it:{help violinplot##by:spec}}{cmd:)}}create subgraphs by
    categories of by-variable
    {p_end}

{syntab :Estimation}
{synopt :{cmdab:ds:cale(}{it:{help violinplot##dscale:spec}}{cmd:)}}change the
    scaling of the plotted PDFs
    {p_end}
{synopt :{cmdab:abs:olute}}use absolute scaling
    {p_end}
{synopt :[{cmd:l}|{cmd:r}]{cmd:tight}}use tight density evaluation grid
    {p_end}
{synopt :{cmdab:ra:nge(}{it:{help violinplot##range:spec}}{cmd:)}}restrict maximum
    density evaluation range
    {p_end}
{synopt :{opt n(#)}}size of evaluation grid; default is {cmd:n(99)}
    {p_end}
{synopt :{cmdab:pdf:opts(}{it:{help dstat##densopts:options}}{cmd:)}}density
    estimation options passed through to {helpb dstat}
    {p_end}
{synopt :{cmd:qdef(}{it:{help dstat##quant:#}}{cmd:)}}quantile definition to be
    used by {helpb dstat}
    {p_end}
{synopt :{opt cw}}omit observations with missing value on any outcome variable
    {p_end}

{syntab :Labels}
{synopt :{opt lab:els(strlist)}}provide custom labels for outcome variables
    {p_end}
{synopt :{opt olab:els(strlist)}}provide custom labels for over categories
    {p_end}
{synopt :{opt slab:els(strlist)}}provide custom labels for splits
    {p_end}
{synopt :{opt bylab:els(strlist)}}provide custom labels for subgraphs
    {p_end}
{synopt :{opt nolab:el}}use names and values, not labels
    {p_end}
{synopt :{cmd:key(}{it:{help violinplot##key:element}}{cmd:)}}plot element to be
    used in legend keys
    {p_end}

{syntab :Elements}
{synopt :[{ul:{cmd:no}}]{cmdab:l:ine}[{cmd:(}{it:{help violinplot##line:options}}{cmd:)}]}whether
    PDF lines are printed; rendering of PDF lines
    {p_end}
{synopt :[{ul:{cmd:no}}]{cmdab:f:ill}[{cmd:(}{it:{help violinplot##fill:options}}{cmd:)}]}whether
    shading is added; rendering of shading
    {p_end}
{synopt :[{ul:{cmd:no}}]{cmdab:w:hiskers}[{cmd:(}{it:{help violinplot##whiskers:opts}}{cmd:)}]}whether
    whiskers are printed; rendering of whiskers
    {p_end}
{synopt :[{ul:{cmd:no}}]{cmdab:b:ox}[{cmd:(}{it:{help violinplot##box:options}}{cmd:)}]}whether
    box is printed; rendering of box
    {p_end}
{synopt :[{ul:{cmd:no}}]{cmdab:med:ian}[{cmd:(}{it:{help violinplot##median:opts}}{cmd:)}]}whether
    median is printed; rendering of median
    {p_end}
{synopt :{cmd:mean}[{cmd:(}{it:{help violinplot##mean:options}}{cmd:)}]}add marker for mean;
    rendering of mean
    {p_end}
{synopt :{cmd:rag}[{cmd:(}{it:{help violinplot##rag:options}}{cmd:)}]}add markers for data points;
    rendering of the rag
    {p_end}
{synopt :{cmdab:ord:er(}{it:{help violinplot##order:list}}{cmd:)}}change order in which
    elements are printed
    {p_end}
{synopt :{opth off:set(numlist)}}shift position of box plot (and rag)
    {p_end}
{synopt :{opth doff:set(numlist)}}shift position of PDF
    {p_end}

{syntab :Colors}
{synopt :{cmdab:c:olors(}{it:{help violinplot##colors:spec}}{cmd:)}}assign colors
    to results, affecting all elements
    {p_end}
{synopt :{opt lc:olors(spec)}}like {helpb violinplot##colors:colors()}, but only affecting PDF lines
    {p_end}
{synopt :{opt fc:olors(spec)}}like {helpb violinplot##colors:colors()}, but only affecting shading
    {p_end}
{synopt :{opt wc:olors(spec)}}like {helpb violinplot##colors:colors()}, but only affecting whiskers
    {p_end}
{synopt :{opt bc:olors(spec)}}like {helpb violinplot##colors:colors()}, but only affecting box
    {p_end}
{synopt :{opt medc:olors(spec)}}like {helpb violinplot##colors:colors()}, but only affecting median
    {p_end}
{synopt :{opt meanc:olors(spec)}}like {helpb violinplot##colors:colors()}, but only affecting mean
    {p_end}
{synopt :{opt ragc:olors(spec)}}like {helpb violinplot##colors:colors()}, but only affecting rag
    {p_end}

{syntab :Other}
{synopt :{cmdab:psty:les(}{it:{help violinplot##pstyles:numlist}}{cmd:)}}assign plot styles to results
    {p_end}
{synopt :{cmd:p}{it:#}{cmd:(}{it:{help violinplot##pnum:options}}{cmd:)}}options passed through to #th result (within group)
    {p_end}
{synopt :{cmd:p}{it:#}{it:el}{cmd:(}{it:{help violinplot##pnumel:options}}{cmd:)}}options passed through to element of #th result (within group)
    {p_end}
{synopt :{cmdab:byopt:s(}{it:{help violinplot##byopts:byopts}}{cmd:)}}options passed through
    to {helpb by_option:by()}
    {p_end}
{synopt :{cmdab:addplot(}{it:{help addplot_option:plot}}{cmd:)}}add other plots to the graph
    {p_end}
{synopt :{it:{help twoway_options}}}general options passed through to {helpb graph twoway}
    {p_end}
{synoptline}
{pstd}
    {cmd:fweight}s, {cmd:pweight}s, and {cmd:iweight}s are allowed; see help {help weight}.


{title:Description}

{pstd}
    {cmd:violinplot} draws so called violin plots that illustrate the
    distributions of the specified outcome variables. A violin plot is an
    overlay of a (reflected) density estimate and a box plot (Hintze and Nelson
    1998).

{pstd}
    Technical note: By default (unless option {cmd:atover} is specified),
    {cmd:violinplot} deactivates ticks and grid lines on the categorical axis
    and sets the gap between the labels and the axis to {cmd:tiny} (mimicking
    the behavior of {helpb graph box}). To print the ticks and gridlines and
    to restore the default gap as used by {helpb graph twoway}, specify
    {cmd:ylabel(, tick grid labgap(half_tiny))} (if in horizontal mode;
    in vertical mode use the {cmd:xlabel()} option). If your graph contains
    subgraphs, you additionally need to specify {cmd:byopts(iytick)} or
    {cmd:byopts(ixtick)}.

{pstd}
    Alternative implementations of violin plots are provided by command
    {helpb vioplot} (Winter and Nichols 2008) and command {helpb violin}
    (Steichen 1998, 2001). Furthermore, command {helpb joy_plot} (Rios-Avila
    2022) provides functionality for violin plots.


{title:Dependencies}

{pstd}
    {cmd:violinplot} requires {helpb dstat}, {helpb moremata}, {helpb palettes},
    and {helpb colrspace}. To install these packages, type:

        . {stata ssc install dstat}
        . {stata ssc install moremata}
        . {stata ssc install palettes}
        . {stata ssc install colrspace}


{title:Options}

{dlgtab:Main}

{phang}
    {opt vertical} draws vertical violins. This is the default if {cmd:overlay}
    (but not {cmd:left} or {cmd:right}) or {cmd:split()} is specified.

{phang}
    {opt horizontal} draws horizontal violins. This is the default unless
    {cmd:overlay} (but not {cmd:left} or {cmd:right}) or {cmd:split()} is specified.

{phang}
    {opt left} prints the left half of each violin. The default is to print both
    halves.

{phang}
    {opt right} prints the right half of each violin. The default is to print both
    halves.

{phang}
    {opt overlay} overlays the results from several variables or subpopulations. The
    default is to print each result individually. Overlay implies {cmd:vertical},
    {cmd:nowhiskers}, and {cmd:nobox}. {cmd:overlay} is not allowed if
    {cmd:split()} is specified.

{phang}
    {opt asover} treats variable groups (Syntax 2) as over categories. The default
    is to create a separate subgraph for each variable group. {cmd:asover} and
    {cmd:over()} are not both allowed.

{marker over}{...}
{phang}
    {cmd:over(}{varname}[{cmd:,} {it:options}]{cmd:)} displays subpopulation
    results by levels of {it:varname}. {it:varname} may be numeric or string. Options
    are as follows.

{phang2}
    {cmdab:tot:al}[{cmd:(}{it:str}{cmd:)}] includes total results across all
    subpopulations. Optional {it:str} specifies a custom label; default is "{cmd:(total)}".

{phang2}
    {cmdab:miss:ing}[{cmd:(}{it:str}{cmd:)}] includes results from subpopulations for which
    {it:varname} is missing; each type of missing value ({cmd:.}, {cmd:.a}, {cmd:.b}, etc.)
    will be treated as a separate category. The default is to exclude missing
    values. Optional {it:str} specifies a custom label for system missing ({cmd:.});
    default is "{cmd:(missing)}".

{phang2}
    {cmd:sort}[{cmd:(}{it:#}{cmd:)}] puts the results in order of their
    medians (or in order of the statistic selected by
    {cmd:median(stat())}). If there are multiple outcome variables, specify
    {it:#} to select the relevant variable; the ordering will then be based on
    the results from the {it:#}th outcome variable (within the first variable
    group). Default is to use the first outcome variable.

{phang2}
    {cmdab:des:cending} uses descending sort order. The default is to use
    ascending sort order. {cmd:descending} implies {cmd:sort}.

{phang2}
    {cmdab:tl:ast} always places the total results last, irrespective of sort
    order. {cmd:tlast} implies {cmd:total}.

{phang}
    {opt atover} uses the values of the over variable to determine the plot
    positions of the results rather than placing them along a categorical
    axis. {cmd:atover} requires {cmd:over()} and is only allowed in selected
    situations. For example, {opt atover} cannot be combined with option {cmd:swap}
    or with over suboptions {cmd:total}, {cmd:missing}, or {cmd:sort}, and the over variable
    must be numeric. Furthermore, {opt atover} is allowed with multiple outcome variables
    only of {cmd:overlay} is specified; likewise, {opt atover} is allowed with a
    single outcome variable only if {cmd:overlay} is omitted.

{pmore}
    The maximum space consumed by an individual results is set to one unit by default, which
    may not be appropriate if {cmd:atover} is specified (depending scaling of
    the over variable). Use option {cmd:dscale()} to change the default;
    for example, type {cmd:dscale(2)} to consume at most two units; type {cmd:dscale(0.5)}
    to consume at most half a unit.

{phang}
    {opt swap} changes the arrangement of results. The default is to group
    results from multiple outcome variables by over category. Specify
    {cmd:swap} to group results from over categories by outcome variable.

{phang}
    {opt nostack} causes a legend to be created for the different results within
    groups of results. The default is to label each result individually on the
    categorical axis, using two levels of labels. This is only relevant
    if there are multiple outcome variables and multiple over
    categories. {cmd:nostack} also assigns different plot styles to the
    results. {cmd:nostack} is not allowed if {cmd:split()} is specified.

{phang}
    {opt gap(#)} specifies the size of the extra gap inserted between clusters of
    results that are grouped together within the same subgraph. This is only relevant
    if there are multiple outcome variables and multiple over categories (and if {cmd:overlay} is
    not specified). The default extra gap is 0.5 (i.e. half the size of the space
    each result consumes).

{marker split}{...}
{phang}
    {opth split(varname)} splits the violins
    by the two groups identified by the levels of {it:varname}. {cmd:split()}
    implies {cmd:vertical}, {cmd:fill}, and {cmd:nowhiskers}, sets the default type of
    {cmd:box()} to {cmd:lines}, and sets the default type of {cmd:median()}
    to {cmd:line}. Options {cmd:nostack} and {cmd:overlay} are not allowed if
    {cmd:split()} is specified.

{marker by}{...}
{phang}
    {cmd:by(}{varname}[{cmd:,} {it:options}]{cmd:)} displays separate subgraphs
    for subpopulation results by levels of {it:varname}. {it:varname} may be
    numeric or string. Option {cmd:by()} is only allowed in Syntax 1 (single group
    of outcome variables; or in Syntax 2 if option {cmd:asover} is specified). Options are
    as follows.

{phang2}
    {cmdab:tot:al}[{cmd:(}{it:str}{cmd:)}] includes total results across all
    subpopulations. Optional {it:str} specifies a custom label; default is "{cmd:(total)}".

{phang2}
    {cmdab:miss:ing}[{cmd:(}{it:str}{cmd:)}] includes results from subpopulations for which
    {it:varname} is missing; each type of missing value ({cmd:.}, {cmd:.a}, {cmd:.b}, etc.)
    will be treated as a separate category. The default is to exclude missing
    values. Optional {it:str} specifies a custom label for system missing ({cmd:.});
    default is "{cmd:(missing)}".

{phang2}
    {it:byopts} are options passed through to graph's {helpb by()} option, which
    is internally used to generate the subgraphs; see help {it:{help by_option}}.

{dlgtab:Estimation}

{marker dscale}{...}
{phang}
    {opt dscale(spec)} determines how the density curves are scaled. {it:spec} is

            [{it:#}][{cmd:,} {it:option}]

{pmore}
    where {it:#}>0 is a custom factor by which the default scaling will
    be multiplied and {it:option} is {cmdab:i:ndividual} to determine
    the scaling for each violin individually, {cmdab:g:roup} to determine the
    scaling by group of results (i.e. by clusters of results that are grouped
    together within the graph), {cmdab:p:lot} to determine the
    scaling by series across clusters (i.e. 1st violin in each group, 2nd violin
    in each group, etc.), or {cmdab:s:ubgraph} to determine the
    scaling by subgraph. The default is to determine the scaling
    jointly across all results.

{pmore}
    You can also specify {cmd:dscale(.)} to omit rescaling. In this case,
    the original scale of each PDF will be retained and, if the graph
    only includes a single violin (per subgraph) or a single overlay of multiple
    violins (per subgraph), axis labels for the scale will be added.

{phang}
    {opt absolute} uses absolute scaling such that the area under the PDF
    is proportional to the size of the relevant group.

{phang}
    {cmd:tight} limits the density evaluation grid to the range between the observed
    minimum to the observed maximum of the relevant data. By default, the range across
    which the density is evaluated is slightly larger than the observed range
    of the data. Specify {cmd:tight} to remove this extra padding. Specify
    {cmd:ltight} to remove the extra padding only on the left; specify
    {cmd:rtight} to remove the extra padding only on the right. In any case,
    the evaluation grid will not go beyond the limits specified by
    {cmd:range()}.

{marker range}{...}
{phang}
    {cmd:range(}{it:from} [{it:to}]{cmd:)} limits the maximum range across which
    the PDF will be evaluated. This may be useful if there are outliers in the
    data and you want to clip the display to the main part of the data. {it:from}
    can be specified as {cmd:.} (missing) to set no lower limit; {it:to} can be
    omitted or specified as {cmd:.} (missing) to set no upper limit.

{pmore}
    By default, fast density estimation based on an approximation grid is used
    by {cmd:violinplot} to obtain the PDF. However, if {cmd:range()} results
    in an evaluation range that is less than half of the original (unrestricted)
    evaluation range, {cmd:violinplot} switches to exact estimation of the affected PDF
    to prevent approximation error from becoming visible. Specify option {cmd:pdfopts(exact)}
    if you want to apply exact estimation in any case.

{phang}
    {cmd:n(#)} sets the size of the density evaluation grid (number of evaluation
    points). The default is {cmd:n(99)}.

{phang}
    {opt pdfopts(options)} provides density estimation options to be passed through to
    {helpb dstat}; see {it:{help dstat##densopts:density_options}}
    in help {helpb dstat}. The same options will be applied used for all results.

{phang}
    {opt qdef(#)} sets the quantile definition to be used when computing the median
    and the quartiles for the box and whiskers; see option {helpb dstat##quant:qdef()} in
    help {helpb dstat}.

{phang}
    {opt cw} specifies casewise deletion. If {cmd:cw} is specified, observations
    for which any of the outcome variables are missing are ignored. The default
    is to determine the estimation sample for each variable individually.

{dlgtab:Labels}

{phang}
    {opt labels(strlist)} provides a space separated list of custom labels
    for the outcome variables. Enclose labels that
    contain spaces in double quotes.

{phang}
    {opt olabels(strlist)} provides a space separated list of custom labels
    for the over categories. If {cmd:asover} is specified (Syntax 2),
    {cmd:olabels()} provides labels for the outcome variable groups. Enclose
    labels that contain spaces in double quotes.

{phang}
    {opt slabels(strlist)} provides a space separated list of custom labels
    for the groups by which the violins are split. Enclose labels that contain spaces in
    double quotes.

{phang}
    {opt bylabels(strlist)} provides a space separated list of custom titles
    for the subgraphs. Enclose labels that contain spaces in
    double quotes.

{phang}
    {opt nolabel} uses names and values rather variable labels and value labels.

{marker key}{...}
{phang}
    {opt key(element)} selects the plot element to be used in legend
    keys. This is only relevant in cases in which a legend is
    displayed. Depending on context, {it:element} is one of {cmdab:l:ine},
    {cmdab:f:ill}, {cmdab:w:hiskers}, {cmdab:b:ox}, {cmdab:med:ian}, {cmd:mean},
    or {cmd:rag} (elements that are not plotted will not be available). The default is
    {cmd:key(line)} or, if option {cmd:noline} has been specified, the element
    that is plotted first.

{dlgtab:Elements}

{marker line}{...}
{phang}
    [{cmd:no}]{cmd:line}[{cmd:(}{it:options}{cmd:)}] determines
    whether the PDF lines are printed or not. The default is to print the lines; type
    {cmd:noline} to omit the lines. Specify {it:options} to affect the rendering of
    the lines; see help {it:{help connect_options}}. {cmd:noline} implies
    {cmd:fill}. {cmd:noline} cannot be combined with {cmd:fill(select())}.

{marker fill}{...}
{phang}
    [{cmd:no}]{cmd:fill}[{cmd:(}{it:options}{cmd:)}] determines
    whether shading is added to the PDF. The default is to omit shading unless
    {cmd:split()} is specified. {it:options} are as follows.

{phang2}
    {opth s:elect(numlist)} restricts the set of results to be affected. By
    default, shading is added to all results if option {cmd:fill} is
    specified. Type, for example, {cmd:select(2 5)} to add shading only to
    the 2nd and 5th result (the counting is across all displayed results,
    not within groups or subgraphs). Option {cmd:select()} is not allowed if
    {cmd:noline} is specified.

{phang2}
    {it:area_options} are graph options affecting the rendering
    of the shading; see help {it:{help area_options}}. By default, option {cmd:fintensity(50)} is applied
    to the shading; this is skipped if any {it:area_options} are
    specified. Furthermore, option {cmd:lcolor(%0)} is applied, which will not
    be skipped; specify, e.g., {cmd:fill(lcolor(%100)} to override this
    setting.

{marker whiskers}{...}
{phang}
    [{cmd:no}]{cmdab:whiskers}[{cmd:(}{it:options}{cmd:)}] determines
    whether the whiskers are printed or not. The default is to print the whiskers
    unless {cmd:overlay} or {cmd:split()} is specified. Specify {it:options} to affect the
    rendering of the whiskers; see help {it:{help line_options}}.

{marker box}{...}
{phang}
    [{cmd:no}]{cmd:box}[{cmd:(}{it:options}{cmd:)}] determines
    whether the IQR box is printed or not. The default is to print the box
    unless option {cmd:overlay} is specified. {it:options}
    are as follows.

{phang2}
    {opt t:ype(type)} sets the type of plot to by used, where {it:type} can
    be {cmdab:b:ar}, {cmdab:f:ill}, or {cmdab:l:ines}. The default is
    {cmd:type(bar)} unless {cmd:split()} is specified, which prints the box as a
    bar. Specify {cmd:type(fill)} to display the box by adding shading to the
    density estimate between the lower and upper bounds. Type {cmd:type(lines)}
    to display the box as two lines across the density estimate, one for each
    bound; this is the default if {cmd:split()} is specified.

{pmore2}
    Note that twoway plottype {helpb twoway_rspike:rspike}, not
    {helpb twoway_rbar:rbar}, is used to generate the
    box in case of {cmd:type(bar)}; type, for example,
    {cmd:box(recast(rbar) barwidth(0.1))} to change the
    plottype to {helpb twoway_rbar:rbar} and use a bar width of 0.1.

{pmore2}
    Furthermore, note that {cmd:type(fill)} and {cmd:type(lines)} will not
    display anything if option {helpb violinplot##range:range()} has been
    applied and the selected range is such that the lower or upper
    bound lies outside the density evaluation range.

{phang2}
    {opt s:tatistics(lo up)} selects custom statistics to be used for the
    lower and upper bounds of the box. Any statistics supported by
    {helpb dstat##stats:dstat} are allowed. Default is {cmd:stat(p25 p75)}
    (lower and upper quartile). For example, type {cmd:stat(p10 p90)} to print
    a box spanning the range between the 10% quantile and the 90%
    quantile. The selected statistics will also affect the computation of the
    whiskers (whiskers are defined, in part, as a function of the lower and
    upper bounds of the box).

{phang2}
    {it:line_options} or {it:area_options} are graph options affecting the rendering
    of the box; see help {it:{help line_options}} for {cmd:type(bar)} or {cmd:type(lines)},
    and help {it:{help area_options}} for {cmd:type(fill)}. In case of {cmd:type(bar)},
    the default is to apply {cmd:lwidth(vthick)}; this is skipped if any
    {it:line_options} are specified. In case of {cmd:type(fill)}, the default is to apply
    {cmd:fintensity(50)} and {cmd:lcolor(%0)} to the shading; the former, but not the latter,
    will be skipped if any {it:area_options} are specified (specify, e.g.,
    {cmd:lcolor(%100)} to override the latter). In case of {cmd:type(lines)}, the
    default is to apply {cmd:plattern(shortdash)}; this is skipped if any
    {it:line_options} are specified.

{marker median}{...}
{phang}
    [{cmd:no}]{cmdab:med:ian}[{cmd:(}{it:options}{cmd:)}] determines whether
    a marker for the median is printed or not. The default is to print the
    marker. {it:options} are as follows.

{phang2}
    {opt t:ype(type)} sets the type of plot to by used, where {it:type} can
    be {cmdab:m:arker} or {cmdab:l:ine}. The default is {cmd:type(marker)}
    unless {cmd:split()} is specified, which prints the median as a marker. Specify
    {cmd:type(line)} to display the median as a line across the density
    estimate. This is the default if {cmd:split()} is specified. Note that
    {cmd:type(line)} will not display anything if option
    {helpb violinplot##range:range()} has been applied and the selected range
    is such that median lies outside the density evaluation range.

{phang2}
    {opt s:tatistic(statistic)} selects a custom statistic to be used instead of the
    median. Any statistic supported by {helpb dstat##stats:dstat} is allowed. Default
    is {cmd:stat(median)}. For example, type {cmd:stat(hl)} to use the
    Hodges-Lehmann location measure.

{phang2}
    {it:marker_options} or {it:line_options} are graph options affecting the
    rendering of the median; see help {it:{help marker_options}} for {cmd:type(marker)}
    and help {it:{help line_options}} for {cmd:type(line)}. In case of {cmd:type(marker)},
    the default is to apply options {cmd:msymbol(O)} and
    {cmd:msize(vsmall)} if a box of type {cmd:bar} is displayed, and to apply
    {cmd:mcolor(white)} if a box of type {cmd:bar} is displayed and {cmd:medcolors()} is
    not specified; these defaults are skipped if any {it:marker_options} are
    specified.

{marker mean}{...}
{phang}
    {cmd:mean}[{cmd:(}{it:options}{cmd:)}] prints a marker for the
    mean. {it:options} are as follows.

{phang2}
    {opt t:ype(type)} sets the type of plot to by used, where {it:type} can
    be {cmdab:m:arker} or {cmdab:l:ine}. The default is
    {cmd:type(marker)}, which prints the mean as a marker. Specify {cmd:type(line)}
    to display the mean as a line across the density estimate. Note that {cmd:type(line)}
    will not display anything if option {helpb violinplot##range:range()} has been
    applied and the selected range is such that mean lies outside the density
    evaluation range.

{phang2}
    {opt s:tatistic(statistic)} selects a custom statistic to be used instead of the
    mean. Any statistic supported by {helpb dstat##stats:dstat} is allowed. Default
    is {cmd:stat(mean)} (arithmetic mean). For example, type {cmd:stat(trim(5))} to use
    the 5% trimmed mean.

{phang2}
    {it:marker_options} or {it:line_options} are graph options affecting the
    rendering of the mean; see help {it:{help marker_options}} for {cmd:type(marker)}
    and help {it:{help line_options}} for {cmd:type(line)}. In case of {cmd:type(marker)},
    if {cmd:split()} is omitted, the default is to apply options {cmd:msymbol(pipe)},
    {cmd:msize(huge)}, and, depending on context, {cmd:msangle(90)}; if
    {cmd:split()} is specified, the default is to apply {cmd:msymbol(x)}. These
    defaults are skipped if any {it:marker_options} are specified.

{marker rag}{...}
{phang}
    {cmd:rag}[{cmd:(}{it:options}{cmd:)}] prints a rag of markers, one marker for each
    data point. {it:options} are as follows.

{phang2}
    {opt out:sides} only includes outside values, that is, data points that are
    smaller (larger) than the lower (upper) limit of the lower (upper)
    whisker. The default is to include all data points.

{phang2}
    {opt bout:sides} includes values that are smaller (larger) than the lower
    (upper) limit of the box. {cmd:boutsides} implies {cmd:outsides}.

{phang2}
    {opt u:nique} omits markers for repeated data points. Use this option to
    reduce graph size in larger datasets.

{phang2}
    {opth off:set(numlist)} shifts the position of the rag(s) by the specified
    value(s). The default is to shift the rag(s) as set by the global
    {helpb violinplot##offset:offset()} option. Use suboption {opt offset()}
    to override this default (i.e., if you want to use different offsets for
    box plots and rags).

{phang2}
    {opt sp:read}[{cmd:(}{it:#1} [{it:#2}]{cmd:)}] spreads out the markers by
    adding beta distributed random errors to their positions. At each level of
    the data, the error distribution will be scaled in proportion
    to the density estimate at that level (such that the rag mimics the shape of the
    violin). Argument {it:#1} in [0.001, 100] sets the degree of
    spreading; {it:#1}=100 (maximum spreading) is equivalent to using
    a uniform error distribution; the default is {it:#1}=1 (moderate spreading).

{pmore2}
    Optional argument {it:#2} sets the width of the spread to a fixed value
    rather than making the spread proportional to the density estimate. Specify
    {it:#2} if you want to generate a uniform rag rather than a rag that mimics
    the shape of the violin. Unless {cmd:dscale(.)} is specified, argument
    {it:#2} is in units of the space allocated to each violin
    (or half-violin). For example, type {cmd:rag(spread(100 0.2))} to generate
    a uniform rag that has a width of 20 percent (argument {it:#1} is set to
    100 in this example so that the markers are spread out uniformly across
    the width). If {cmd:dscale(.)} is specified, argument {it:#2} is in
    absolute units (or half-units).

{phang2}
    {opt st:ack}[{cmd:(}{it:#}{cmd:)}] prints stacks of markers at the unique
    values of the data. Use this option to create raindrop plots. Argument
    {it:#} sets the step size between markers within stacks. An appropriate value for the step
    size is determined from the data if {it:#} is omitted or set to missing. Only
    one of {cmd:stack()} and {cmd:spread()} is allowed.

{phang2}
    {opt l:eft} spreads the rag in direction of the left half of the violin. The
    default is to spread in both directions. Suboption {cmd:left} is implied if
    global option {cmd:left} or global option {cmd:split()} is specified. Specify
    suboption {cmd:right}, or both {cmd:left} and {cmd:right}, to override this behavior.

{phang2}
    {opt r:ight} spreads the rag in direction of the right half of the violin. The
    default is to spread in both directions. Suboption {cmd:right} is implied if
    global option {cmd:right} is specified. Specify suboption
    {cmd:left}, or both {cmd:left} and {cmd:right}, to override this behavior.

{phang2}
    {it:marker_options} are graph options affecting the rendering of the
    markers; see help {it:{help marker_options}}. By default, options
    {cmd:msymbol(pipe)} and, depending on context, {cmd:msangle(90)} are
    applied; you can override the default by specifying these options
    explicitly.

{marker order}{...}
{phang}
    {opt order(list)} changes the order in which the elements are placed on the
    plot (determining whether an element is in the foreground or in the
    background). {it:list} is a space separated list of elements, where an element
    is one of {cmdab:l:ine}, {cmdab:f:ill}, {cmdab:w:hiskers}, {cmdab:b:ox}, {cmdab:med:ian},
    {cmd:mean}, and {cmd:rag} (each element can only be listed once; only elements that are
    included in the plot are allowed). Elements not included in the list will be
    added last (in their default order). The default order depends on context.

{marker offset}{...}
{phang}
    {opth offset(numlist)} shifts the position(s) of the elements of the box plot(s)
    (whiskers, box if type is {cmd:bar}, median if type is {cmd:marker}, mean
    if type is {cmd:marker}) by the specified value(s). The default is to place each box plot
    at the center of the corresponding violin. A positive value shifts the box plot to the left side of
    the violin; a negative value shifts the box plot to the right. A violin
    typically consumes space of up to one unit, so that values between -0.5 and 0.5
    usually make sense. Specify multiple values
    to use different values by physical plots; values will be recycled if {it:numlist} has
    less elements than there are physical plots. A physical plot is a series of results
    drawn by a single plot command; see {helpb violinplot##pstyles:pstyles()} for details
    on physical plots.

{pmore}
    Note on usage of {cmd:offset()} together with {cmd:split()}: If {cmd:split()}
    is specified, a negative value shifts the box plot into the
    relevant half violin (and a positive value shifts the box plot into the
    other half), and a default offset of about 1% of the range of
    the categorical axis is applied (so that the box plots of the two halves
    are not printed on top of each other). You can override this default by providing
    custom values(s) in {cmd:offset()}.

{phang}
    {opth doffset(numlist)} shifts the position(s) of the PDF(s) (line and fill) by
    the specified value(s). The default is to center each violin at the corresponding
    tick of the categorical axis. A positive value shifts the violin up/to the
    left; a negative value shifts the violin down/to the right. Specify multiple values
    to use different values by physical plots; values will be recycled if {it:numlist} has
    less elements than there are physical plots. A physical plot is a series of results
    drawn by a single plot command; see {helpb violinplot##pstyles:pstyles()} for details
    on physical plots.

{dlgtab:Colors}

{marker colors}{...}
{phang}
    {opt colors(spec)} assigns colors to the results, affecting all elements
    of the violin plot (except the median depending on context). The colors will
    be recycled across groups of results and across subgraphs. {it:spec} is

            {it:{help colorpalette##palette:palette}} [{cmd:,} {it:{help colorpalette##opts:palette_options}}]

{pmore}
    where {it:palette} is any palette or color specification compatible with
    {helpb colorpalette} and {it:palette_options} are options passed through to
    {helpb colorpalette}. The correct number of colors will be retrieved automatically;
    option {cmd:n()} is not allowed. Specify a single color to use the same color
    for all variables.

{phang}
    {opt lcolors(spec)} is like {helpb violinplot##colors:colors()}, but only affects the PDF lines.

{phang}
    {opt fcolors(spec)} is like {helpb violinplot##colors:colors()}, but only affects the shading.

{phang}
    {opt wcolors(spec)} is like {helpb violinplot##colors:colors()}, but only affects the  whiskers.

{phang}
    {opt bcolors(spec)} is like {helpb violinplot##colors:colors()}, but only affects the IQR box.

{phang}
    {opt medcolors(spec)} is like {helpb violinplot##colors:colors()}, but only affects the median.

{phang}
    {opt meancolors(spec)} is like {helpb violinplot##colors:colors()}, but only affects the mean.

{phang}
    {opt ragcolors(spec)} is like {helpb violinplot##colors:colors()}, but only affects the rag.

{dlgtab:Other}

{marker pstyles}{...}
{phang}
    {opth pstyles(numlist)} determines the plot styles applied to the
    results. If {cmd:pstyles()} is omitted, the default is to use style {cmd:p1}
    for all results or, if {cmd:overlay} or {cmd:nostack} is specified, to use
    consecutive styles across groups of results (i.e. {cmd:p1} for the first result
    within each group, {cmd:p2} for the second result within each group,
    etc.). For example, specify {cmd:pstyles(5 3 7)} to use {cmd:p5} for the
    first result, {cmd:p3} for the second result, and {cmd:p7} for the third
    result. If {cmd:split()} is specified, only two styles will be used.

{pmore}
    Technically, the styles specified in {cmd:pstyles()} are applied one after
    another to the physical plots in the graph, recycling styles
    if {it:{help numlist}} contains fewer elements than there are physical
    plots. A physical plot is a series of results drawn by a single plot
    command. If there is only a single group of results, each result is a
    separate physical plot. If there are multiple groups of results, physical plots
    are organized by positions within group (i.e., all first results are a single
    physical plot, all 2nd results are a single physical plot, and so on). If
    option {cmd:split()} is specified, all left halves
    are a single physical plot and all right halves are a single physical plot.

{marker pnum}{...}
{phang}
    {cmd:p}{it:#}{cmd:(}{it:options}{cmd:)} are graph options to be passed through
    to the #th physical plot.

{marker pnumel}{...}
{phang}
    {cmd:p}{it:#}{it:el}{cmd:(}{it:options}{cmd:)} are graph options to be passed
    through to element {it:el} of the #th physical plot. Depending on context,
    {it:el} is one of {cmd:l} (PDF lines),
    {cmd:f} (shading of PDF), {cmd:w} (whiskers), {cmd:b} (box), {cmd:med} (median),
    {cmd:mean}, and {cmd:rag} (only elements that are included in the plots are
    allowed). For example, type {cmd:p2med(mcolor(red))} to print the median
    marker of each 2nd result per group in red.

{marker byopts}{...}
{phang}
    {opt byopts(byopts)} are options passed through to the {cmd:by()} option,
    which is internally used to generate subgraphs; see help {it:{help by_option}}.

{phang}
    {opt addplot(plot)} allows adding more {helpb graph twoway} plots to the graph;
    see help {it:{help addplot_option}}.

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
{p 8 12 2}
        . {stata violinplot wage ttl_exp tenure, range(. 31) plotregion(margin(r=0))}
        {p_end}
        . {stata violinplot wage ttl_exp tenure, fill}
        . {stata violinplot wage ttl_exp tenure, noline}
        . {stata violinplot wage ttl_exp tenure, fill(select(3))}

{pstd}
    Use option {cmd:pdfopts()} to specify {help dstat##densopts:density estimation options}. For
    example, wages, work experience, and tenure cannot be negative, so let's apply boundary
    correction at zero:

        . {stata violinplot wage ttl_exp tenure, pdf(ll(0))}

{pstd}
    Use option {cmd:over()} to display results by subpopulations:

        . {stata violinplot wage, pdf(ll(0)) over(union)}
        . {stata violinplot wage ttl_exp tenure, pdf(ll(0)) over(union)}
        . {stata violinplot wage ttl_exp tenure, pdf(ll(0)) over(union) swap}
{p 8 12 2}
        . {stata violinplot wage ttl_exp tenure, pdf(ll(0)) over(union) swap nostack}
        {p_end}
{p 8 12 2}
        . {stata violinplot wage, pdf(ll(0)) over(industry, sort tlast) nobox nowhiskers}
        {p_end}

{pstd}
    Use option {cmd:by()} to create subgraphs by subpopulations:

        . {stata violinplot wage ttl_exp tenure, pdf(ll(0)) by(union)}

{pstd}
    Combination of {cmd:over()} and {cmd:by()}:

        . {stata violinplot wage ttl_exp tenure, pdf(ll(0)) over(race) by(union)}
{p 8 12 2}
        . {stata violinplot wage ttl_exp tenure, pdf(ll(0)) over(race) by(union) swap}
        {p_end}

{pstd}
    Use parentheses to define groups of variables that will be placed in separate
    subgraphs:

        . {stata violinplot (wage) (ttl_exp), over(race)}

{pstd}
    Use option {cmd:overlay} to create overlays (whiskers and boxes will be omitted by default):

{p 8 12 2}
        . {stata violinplot wage ttl_exp tenure, pdf(ll(0)) over(union) overlay swap}
        {p_end}
{p 8 12 2}
        . {stata violinplot wage ttl_exp tenure, pdf(ll(0)) over(union) overlay swap noline fill(color(%30)) key(median)}
        {p_end}

{pstd}
    Use option {cmd:split()} to create half violins (whiskers will be omitted,
    lines will be used to display box and median, and fill will be added by default):

{p 8 12 2}
        . {stata violinplot wage ttl_exp tenure, pdf(ll(0)) split(union) key(fill)}
        {p_end}
{p 8 12 2}
        . {stata violinplot wage tenure, pdf(ll(0)) over(south) swap gap(0) split(union) absolute}
        {p_end}

{pstd}
    Use options {cmd:colors()} (all elements), {cmd:lcolors()} (PDF lines), {cmd:bcolors()}
    (boxes), etc., to obtain colors from {helpb colorpalette}:

{p 8 12 2}
        . {stata violinplot wage ttl_exp tenure, pdf(ll(0)) over(union) swap nostack colors(Set1)}
        {p_end}

{p 8 12 2}
        . {stata violinplot wage, over(industry) colors(sb muted) nobox nowhiskers median(msymbol(o))}
        {p_end}
{p 8 12 2}
        . {stata violinplot wage, over(industry) colors(sb muted) box(type(fill)) median(msymbol(o) msize(vsmall) color(white)) nowhiskers}
        {p_end}
{p 8 12 2}
        . {stata violinplot wage, over(industry, descending) colors(plasma) fill nobox nowhiskers nomedian}
        {p_end}

{pstd}
    Print data points using option {cmd:rag()}:

{p 8 12 2}
        . {stata violinplot wage, left rag(offset(-.005) mlcolor(%20)) dscale(.)}
        {p_end}
{p 8 12 2}
        . {stata violinplot wage, left rag(offset(-.005) spread right) dscale(.) ylabel(0(.05).15)}
        {p_end}

        . {stata sysuse auto, clear}
{p 8 12 2}
        . {stata violinplot price, over(foreign) vertical fill rag(spread(5) msymbol(o) mcolor(maroon%50) mlcolor(%0))}
        {p_end}


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
    Jann, B. (2022). VIOLINPLOT: Stata module to draw violin plots. Available from
    {browse "https://ideas.repec.org/c/boc/bocode/s459132.html"}.


{title:Also see}

{psee}
    Online:  help for {helpb graph twoway}, {helpb graph box}

