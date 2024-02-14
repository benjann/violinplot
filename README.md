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

    violinplot wage ttl_exp tenure, pdf(ll(0)) split(union) key(fill)

![example 6](/images/6.png)

    violinplot wage, pdf(ll(0)) over(industry) colors(plasma) fill nomedian nobox nowhiskers

![example 7](/images/7.png)

    violinplot wage, pdf(ll(0)) over(industry, sort) colors(plasma) box(type(fill)) nowhiskers

![example 8](/images/8.png)

    violinplot wage, left dscale(.) rag(offset(-.005) mlcolor(%20))

![example 9](/images/9.png)

    violinplot wage, over(union) overlay left dscale(.) nomedian ///
        rag(offset(-.005 -0.01) mlcolor(%40))

![example 10](/images/10.png)

    violinplot wage, over(union) vertical fill absolute ///
        rag(spread(10) msymbol(o) mcolor(%20) mlc(%0) pstyle(p2))

![example 10](/images/11.png)

    sysuse auto, clear
    violinplot mpg turn, left rag(stack right msymbol(o) offset(-.05))

![example 10](/images/12.png)

---

Main changes:

    14feb2024 (1.1.7)
    - revised the computation of weighted markers in the rag; the sizes are now
      determined in a way such that average weight typically corresponds to the
      default (unweighted) marker size
    - reorganized some code and fixed some minor issues

    13feb2024 (1.1.6)
    - markers in rag() will now be scaled by the weights if weights have been
      specified; likewise, if suboption -unique- is specified, the markers of the
      rag will be scaled by the number of collapsed observations (or sum of
      weights); across results, the scaling of markers will be determined in a
      way such that it is consistent with the scaling applied to the density
      estimates; specify rag(noweight) to omit the scaling of markers

    12feb2024 (1.1.5)
    - rag() now has suboption stack() that can be used to produce raindrop plots

    10feb2024 (1.1.3)
    - second argument in rag(spread()) now allowed to use a fixed value for the
      width of the spread (i.e. do not scale the distribution in proportion to the
      density estimate)
    - noline and nofill are now both allowed; noline and fill(select()) are now both
      allowed
    - key(fill) now also allowed with fill(select()); default key now rearmost
      element if noline is specified

    22mar2023 (1.1.2)
    - violinplot did not work in Stata 15; this is fixed

    02dec2022 (1.1.1)
    - suboption unique added to rag()

    29nov2022 (1.1.0)
    - numlist now allowed in offset(), doffset(), and rag(offset()); numbers
      will be recycled across physical plots

    28nov2022 (1.0.9)
    - options rag(), ragcolors(), and p#rag() added
    - options left and right added
    - options offset() and doffset() added; split(, offset())) no longer documented
      (but still supported for backward compatibility)
    - option dscale(.) now retains the original scaling of the PDFs (and replaces
      the categprical axis by a continuous axis if there is only one violin or only
      one overlay of violins)
    - shorter abbreviations are now allowed for various options
    - internal change in how positions of the violins are determines; counting now
      starts at zero; now using negative counting in case of -horizontal- (rather
      then using a revered axis)

    25nov2022 (1.0.8)
    - option slabels() did not work as expected; this is fixed
    - options color(), lcolor(), fcolor() etc. did not allow color specifications
      enclosed in double quotes; this is fixed

    20nov2022 (version 1.0.7)
    - options split() and slabels() added
    - option absolute added
    - can now also specify ltight or rtight; [l|r]tight is now passed through to
      dstat; this requires the newest updates of dstat and moremata

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
