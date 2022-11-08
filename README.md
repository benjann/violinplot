# violinplot
Stata module to draw violin plots

`violinplot` draws so called violin plots that illustrate the distributions
of the specified variables. A violin plot is an overlay of a (reflected) density
estimate and a box plot.

`violinplot` Stata 15 or newer as well as the following packages from SSC:
`dstat`, `moremata`, `palettes`, `colrspace`

---

Installation from GitHub:

    . net from https://raw.githubusercontent.com/benjann/violinplot/main/
    . net install violinplot, replace

Installation from SSC:

    . ssc install violinplot, replace

Install dependencies from SSC:

    . ssc install dstat, replace
    . ssc install moremata, replace
    . ssc install palettes, replace
    . ssc install colrspace, replace

---

Some examples (use of the `grytsle` package is made; type `ssc install grstyle` to install the package):

    clear all
    sysuse nlsw88
    grstyle init
    grstyle set imesh
    grstyle set color sb muted
    violinplot wage ttl_exp tenure, pdf(ll(0))

![example 1](/images/1.png)

    violinplot wage ttl_exp tenure, pdf(ll(0)) fill

![example 2](/images/2.png)

    violinplot wage ttl_exp tenure, pdf(ll(0)) fill vertical

![example 3](/images/3.png)

    violinplot (ttl_exp) (tenure), pdf(ll(0)) fill over(race)

![example 4](/images/4.png)

    violinplot wage, pdf(ll(0)) noline fill(fcolor(%50) lcolor(%100)) over(union) overlay

![example 5](/images/5.png)

    violinplot wage, pdf(ll(0)) over(industry) color(plasma) fill nomedian nobox nowhiskers

![example 6](/images/6.png)

    violinplot wage, pdf(ll(0)) over(industry, sort) color(plasma) box(type(fill)) nowhiskers

![example 7](/images/7.png)

---

Main changes:

    07nov2022 (version 1.0.6)
    - suboptions sort(), descending, and tlast added in over()
    - suboptions type() and stat() added in box(), median(), and mean()
    - option order() added
    - p#el() options added; behavior of p#() improved
    - option n() added
    - addplot() did not work well if by() has been specified; this is fixed
    - now using labsize(medium) for main axis labels if there are two levels
      of axis labels

    01nov2022 (version 1.0.5)
    - option by() added
    - options over(), olabels(), swap, gap(), atover, asover, nostack added
    - change in suboptions of dscale()
    - option nolabel added
    - option addplot() added
    - option cw added
    - support for p#() options added
    - now returning error if there are no valid observations
    - density estimation now skipped if x is constant
    - now using exact density estimation if range() restricts the evaluation
      range to less than half the default evaluation range
    - ticks and grid lines on categorical axis now deactivated (unless atover is
      specified)

    25oct2022 (version 1.0.4)
    - weights were not taken into account; this is fixed
    - option key() added
    - option qdef() added
    - option tight added
    
    24oct2022 (version 1.0.3)
    - the definition used to compute the whiskers was not correct; this is fixed

    22oct2022 (version 1.0.2)
    - changed default rendering of box and median (such that box is less thick)

    22oct2022 (version 1.0.1)
    - documentation added
    - overlay option added
    - option pstyles() added
    - option horizontal added
    - dscale(,subgraph) renamed to dscale(,group)
    - various additional refinements and bug fixes

    20oct2022 (version 1.0.0)
    - released on GitHub
