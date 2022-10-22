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

Install dependencies from SSC:

    . ssc install dstat, replace
    . ssc install moremata, replace
    . ssc install palettes, replace
    . ssc install colrspace, replace

---

Main changes:

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
