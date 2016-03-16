# wundr
Final project R package for Stats 290

### Mission
The "Advanced Weather Station Data Project" enhances the suggested assignment by significantly expanding the use case of the final product by adding multidimensional microclimate overlays and producing interactive visuals.  Upon seamless integration of the S4 class and subclass retrieval features, the team envisions the addition of relevant social media and the pivoting of the extant PWS ("Personal Weather Stations") into sensors for broadcast purposes.  The broad spectrum of use cases is outlined in the proposal.Rmd.

####  features

 1. Creation of S4-Class Table describing a given region's PWS – this involves extensive workarounds to circumvent the (intentional)   limitations of the API
 2. Subsettable tables based on #1 above
 3. Robust web retrieval of data and storage in memory as well as in (SQL) databases
 4. Visualization of microclimate data through (animated) density maps and time-series
 5. Computational tools for spatial analysis, including Kriging (Gaussian processes) for interpolation, as well as prediction of missing data and forecasts
 6. Web interface with interactive Visuals (LeafletR)
 7. Integration with other data providers and services
