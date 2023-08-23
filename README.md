
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rnwb

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of `rnwb` is to provide R users access to [“Neurodata Without
Borders” (NWB)](https://www.nwb.org/). This access is viable through the
NWB official Python package
[pynwb](https://github.com/NeurodataWithoutBorders/pynwb) and
Python-to-R tunnel provided by [RAVE (Reproducible framework for
Analyzing and Visualizing intracranial EEG)](https://rave.wiki)

## Installation

The package is not on CRAN (yet). You can install the development
version of `rnwb` like so:

``` r
## First, install R package
# install.packages("remotes")
remotes::install_github("dipterix/rnwb")

## Next, set up environment (one-time code)
rnwb::install_nwb()
```

## Example

This is a basic example of reading a `NWB` file:

``` r
file <- "<path to .nwb>"
# Open a NWB file container
container <- rnwb::NWBHDF5IO$new(file, mode = "r")

# Use `with` to properly close the files at the end
container$with({
  nwb_data <- container$read()
  electrodes <- nwb_data$electrodes
  
  columns_to_extract <- c("location", "filtering", "channID", "hemisph", "label")
  
  # Use `[]` to load data into memory
  # Use `convert = TRUE` to convert table to an R object
  electrode_table <- electrodes[ , columns_to_extract, convert = TRUE]
})

# Pretty-print
data.table::data.table(electrode_table)
#>                 location filtering channID hemisph       label
#>   1:            amygdala  300-3000     257       L  MI_D1_C257
#>   2:            amygdala  300-3000     258       L  MI_D1_C258
#>   3:            amygdala  300-3000     259       L  MI_D1_C259
#>   4:            amygdala  300-3000     260       L  MI_D1_C260
#>   5:            amygdala  300-3000     261       L  MI_D1_C261
#>  ---                                                          
#> 166: posterior cingulate  0.1-1000     150       R MA_D13_C150
#> 167: posterior cingulate  0.1-1000     151       R MA_D13_C151
#> 168: posterior cingulate  0.1-1000     152       R MA_D13_C152
#> 169: posterior cingulate  0.1-1000     153       R MA_D13_C153
#> 170: posterior cingulate  0.1-1000     154       R MA_D13_C154
```

You can also access the low-level Python code via R:

``` r
nwb <- rnwb::load_nwb()
print(nwb)
#> 
#> ── Original python documentation ───────────────────────────────────────────────
#> Help on package pynwb:
#> 
#> NAME
#>     pynwb
#> 
#> DESCRIPTION
#>     This package will contain functions, classes, and objects
#>     for reading and writing data in NWB format
#> 
#> PACKAGE CONTENTS
#> ... (Max lines reached, Limit: 10 lines)
#> 
#> 
#> Module(pynwb)
#> 
#> ── Footnotes ───────────────────────────────────────────────────────────────────
#> ℹ Please use the following command to see the full documentation: `rnwb::py_help(nwb)`
#> ℹ Above documentation is for Python. Please use `$` instead of `.` for modules and functions in R (e.g. nwb$NWBHDF5IO instead of nwb.NWBHDF5IO)
#> ────────────────────────────────────────────────────────────────────────────────

# Get elements/functions in R via `$`
nwb$NWBFile
#> 
#> ── Original python documentation ───────────────────────────────────────────────
#> Help on class NWBFile in module pynwb.file:
#> 
#> class NWBFile(pynwb.core.MultiContainerInterface, hdmf.container.ExternalResourcesManager)
#>  |  NWBFile(*args, **kwargs)
#>  |  
#>  |  A representation of an NWB file.
#>  |  
#>  |  Method resolution order:
#>  |      NWBFile
#>  |      pynwb.core.MultiContainerInterface
#> ... (Max lines reached, Limit: 10 lines)
#> 
#> 
#> <class 'pynwb.file.NWBFile'>
#> 
#> ── Footnotes ───────────────────────────────────────────────────────────────────
#> ℹ Please use the following command to see the full documentation: `rnwb::py_help(nwb$NWBFile)`
#> ℹ Above documentation is for Python. Please use `$` instead of `.` for modules and functions in R (e.g. nwb$NWBHDF5IO instead of nwb.NWBHDF5IO)
#> ────────────────────────────────────────────────────────────────────────────────
```
