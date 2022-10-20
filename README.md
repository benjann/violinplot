# violinplot
Stata module to draw violin plots

This is a very preliminary version. No documentation is available yet. Minimal
explanations can be found in the header of the code.

Requires Stata 15 or newer as well as the following packages from SSC:
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

    20oct2022 (version 1.0.0)
    - released on GitHub