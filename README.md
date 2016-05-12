
<!-- README.md is generated from README.Rmd. Please edit that file -->
wundr
=====

**wundr** provides API interfaces to Personal Weather Station (PWS) data maintained by [Weather Underground](https://www.wunderground.com). Tables of PWS locations and metadata for user specified geographic areas are constructed. Robust retrieval of current and historical weather condition data for selected PWS. Computational tools for spatial analysis, including kriging (Gaussian processes) for interpolation, as well as prediction of missing data and forecasts. Visualization of micro-climate data methods including contextual static maps and interactive web maps.

This package was developed as a final project for Prof. Balasubramanian Narasimhan's [Paradigms for Computing with Data](http://statweb.stanford.edu/~naras/stat290/Stat290_Website/Stat_290.html) at Stanford University.

Installation
------------

Install the development version from github

``` r
# install.packages("devtools")
install_github("MatthewSchumwinger/wundr", build_vignettes = TRUE)
```

Overview
--------

Key features of **wundr** include:

1.  Creation of S4-Class Table describing a given region's PWS.
2.  Subsettable tables based on \#1 above.
3.  Robust web retrieval of data and storage in memory.
4.  Visualization of micro-climate data through density maps and time-series.
5.  Computational tools for spatial analysis, including kriging (Gaussian processes) for interpolation, as well as prediction of missing data and forecasts.
6.  Web interface for interactive visuals using leaflet.js.

To see examples of these features in use, please view the '**overview**' vignette.

``` r
vignette(package = "wundr")
```
