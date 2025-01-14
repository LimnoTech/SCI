
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SCI

<!-- badges: start -->
<!-- badges: end -->

The goal of SCI is to calculate scores for hydrology, water quality,
human health, and aquatic biology metrics to assess streams within the
District of Columbia.

## Installation

You can install the development version of SCI from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("LimnoTech/SCI")
```

## Using the Package

Produce the SCI scores in just a few simple steps:

1.  Complete the [pre-processing
    steps](https://limnotech.github.io/SCI/articles/pre-processing.html),
    as needed.

2.  In RStudio, open the
    [workflow.R](https://github.com/LimnoTech/SCI/blob/master/workflow.R)
    script.

3.  Update the “Input Variables” section of the script and confirm the
    latest data files are saved in the
    [data](https://github.com/LimnoTech/SCI/tree/master/data) folder.

4.  Update
    [lookup_tables.xlsx](https://github.com/LimnoTech/SCI/blob/master/data/lookup_tables.xlsx)
    and run code from the “Lookup Tables” section of the script, as
    needed. Note: this step is not typically necessary.

5.  Click the Source button to run the workflow.R script.

6.  View the results in
    [all_scores.csv](https://github.com/LimnoTech/SCI/blob/master/all_scores.csv).
