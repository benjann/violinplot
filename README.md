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

    separate ttl_exp, by(race) veryshortlabel
    separate tenure, by(race) veryshortlabel
    violinplot (ttl_exp?) (tenure?), pdf(ll(0)) fill bylabels(ttl_exp tenure) 
    drop ttl_exp? tenure?

![example 4](/images/4.png)

    separate wage, by(union) veryshortlabel
    violinplot wage?, pdf(ll(0)) noline fill(fcolor(%50) lcolor(%100)) overlay 
    drop wage?

![example 5](/images/5.png)

    separate wage, by(industry) veryshortlabel
    violinplot wage?*, pdf(ll(0)) color(plasma) fill nomedian nobox nowhiskers
    drop wage?

![example 6](/images/6.png)

---

Main changes:

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
