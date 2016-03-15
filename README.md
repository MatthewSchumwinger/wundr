# wundr
Final project R package for Stats 290

## Task List
 who | by when | what
- [x] team | 3-14 | compile tar of "safety" package
- [x] team | 3-14 | Your package must pass R CMD check --as-cran without errors. It should not produce any warnings or notes either!
- [x] team | 3-14 | Your package must have test scripts in the tests directory to demonstrate that the main computations in the package do work. We will discuss testing in later classes via testhat.
- [ ] team | 3-14 | 4-page summary.Rmd vignette
- [x] team | 3-14 | Your code must adhere to R coding style. See Wickham's Advanced R book and Google Style for some suggestions. I use camelCase for naming, except for well-known conventions like is.x. Transplanting loops from other languages is sure to earn demerits.
- [x] S    | 3-11 | part 5 - Computational Facility based on Interpolations, etc.
- [ ] J    | .... | Getter for S4 global.R object with respect to Temperature, Humidity, etc.
- [ ] J    | .... | global.R file for package (global variable for Conditions)
- [ ] M    | 3-14 | feature for user to graphically select point for search radius
- [ ] M    | 3-10 | part 6 - improve web map pop-ups
- [ ] M    | 3-10 | create custom ggplot/ggmap mapping theme(s)
- [ ] M    | .... | part 4 - in lieu of Torque.js, function to animate rasters by time


### Mission
The "Advanced Weather Station Data Project" enhances the suggested assignment by significantly expanding the use case of the final product by adding multidimensional microclimate overlays and producing interactive visuals.  Upon seamless integration of the S4 class and subclass retrieval features, the team envisions the addition of relevant social media and the pivoting of the extant PWS ("Personal Weather Stations") into sensors for broadcast purposes.  The broad spectrum of use cases is outlined in the proposal.Rmd.

#### proposed features

 1. Creation of S4-Class Table describing a given region's PWS – this involves extensive workarounds to circumvent the (intentional)   limitations of the API
 2. Subsettable tables based on #1 above
 3. Robust web retrieval of data and storage in memory as well as in (SQL) databases
 4. Visualization of microclimate data through (animated) density maps and time-series
 5. Computational tools for spatial analysis, including Kriging (Gaussian processes) for interpolation, as well as prediction of missing data and forecasts
 6. Web interface with interactive Visuals (LeafletR)
 7. Possible integration with other data providers and services
