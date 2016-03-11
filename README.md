# 290project
Final project R package for Stats 290

## Task List
 who | by when | what

- [ ] team | 3-14 | final exportable package passing all checks
- [x] team | 3-10 | consolidate all code into draft package
- [ ] team | .... | package documentation
- [ ] team | .... | package vignettes
- [ ] S    | 3-11 | part 5 - Computational Facility based on Interpolations, etc.
- [ ] S    | 3-10 | part 1 - optimized api functions
- [x] J    | 3-09 | Properly functional PWS.Conditions S4
- [x] J    | 3-10 | part 2 - Properly functional PWS.Subset S4
- [ ] J    | .... | Properly functional PWS History S4
- [ ] J    | .... | Getter / Setter S4 for Temperature, Humidity, etc.
- [x] J    | 3-10 | (something related to the %over% function) included as validator in PWS.Subset.S4
- [ ] J    | .... | global.R file for package (global variable for Conditions)
- [ ] M    | .... | feature for user to graphically select point for search radius
- [x] M    | 3-9 | wire plotting/mapping and cdb functions to new S4object
- [ ] M    | 3-10 | part 6 - improve web map pop-ups
- [ ] M    | 3-10 | create custom ggplot/ggmap mapping theme(s)
- [x] M    | 3-8 | part 7 - extension to map PWS to CartoDB thru api
- [x] M    | 3-9 | part 7 - extension to export data to CartoDB thru api
- [ ] M    | .... | part 4 - extension to Torque.js
- [ ] M    | .... | part 4 - in lieu of Torque.js, function to animate rasters by time
- [ ] J    | TBD | switch out RJSONIO for jsonlite in gGeocode 

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
