## ---- echo = FALSE-------------------------------------------------------
library(wundr)

## ---- fig.show='hold', warning = FALSE, message = FALSE, eval = TRUE-----
str(PWS.Hist.Chicago, max.level = 2)

## ---- fig.show='hold', warning = FALSE, message = FALSE, eval = TRUE-----
knitr::kable( head( PWS.Loc.Chicago@spatialPtDF@data[,1:7] ) )

## ---- fig.show='hold', warning = FALSE, message = FALSE, eval = FALSE----
#  my_subset  <-  draw_subset(PWS.Conds.Chicago)

## ---- fig.show='hold', warning = FALSE, message = FALSE, include = TRUE----
simple_density(PWS.Conds.Chicago, title = "Chicago PWS points over \nPWS density contour")
simple_pnts(PWS.Conds.Chicago, add = TRUE)

## ---- fig.show='hold', warning = FALSE, message = FALSE, include = TRUE----
basemap <- set_basemap(PWS.Conds.Chicago, zoom = 12)
gg_points(PWS.Conds.Chicago, basemap, title = "Downtown Chicago PWS")

## ---- eval=FALSE---------------------------------------------------------
#  hist.zoo <- history_zoo(Rio_history,"IRIODEJA53",c("hum","tempm"))
#  plot(hist.zoo,col='red', main = "Humidity and Temperatur")

## ---- eval=FALSE---------------------------------------------------------
#  hist.ts <- history_ts(Rio_history,"IRIODEJA53","hum")
#  hist.forecast <- history_forecast(hist.ts)
#  plot(hist.forecast, main = 'Forecast', xlab='Time (days)', ylab='Humidity (%)')

## ---- fig.show='hold', echo=FALSE----------------------------------------
hist.zoo <- history_zoo(Rio_history,"IRIODEJA53",c("hum","tempm"))
plot(hist.zoo,col='red', main = "Humidity and Temperatur")
hist.ts <- history_ts(Rio_history,"IRIODEJA53","hum")
hist.forecast <- history_forecast(hist.ts)
plot(hist.forecast, main = 'Forecast', xlab='Time (days)', ylab='Humidity (%)')

## ------------------------------------------------------------------------
data.geo <- create_geo_cond(Rio_conditions,"temp_c")

## ----eval=FALSE----------------------------------------------------------
#  model<-GP_fit(data.geo)
#  ggplot2::ggplot(data = model, ggplot2::aes(x=lon, y=lat)) +
#    ggplot2::geom_tile(ggplot2::aes(fill = value),colour = "white") +
#    ggplot2::scale_fill_gradient(low = "yellow", high = "red") +
#    ggplot2::geom_point(data=Rio_metadata$PWSmetadata,col='black')

## ----echo=FALSE, results='hide' , warning=FALSE, comment=FALSE-----------
model<-GP_fit(data.geo)
ggplot2::ggplot(data = model, ggplot2::aes(x=lon, y=lat)) +
  ggplot2::geom_tile(ggplot2::aes(fill = value),colour = "white") +
  ggplot2::scale_fill_gradient(low = "yellow", high = "red") +
  ggplot2::geom_point(data=Rio_metadata$PWSmetadata,col='black')

## ---- fig.show='hold'----------------------------------------------------
webmap_pnts(PWS.Conds.Chicago)

## ------------------------------------------------------------------------
webmap_raster(PWS.Conds.Chicago)

## ---- eval = FALSE-------------------------------------------------------
#  matt.cdb.key <- "f09ad502b34fa4096a62ea306b4650337d41009c"
#  matt.cdb.account <- "biglakedata"
#  pizza <- PWS.Conds.Chicago
#  r2cdb(matt.cdb.key, matt.cdb.account, pizza)

## ---- eval = TRUE--------------------------------------------------------
# matt_cdb_table <- get_cdb_table("condTest", matt.cdb.account)
head(matt_cdb_table$rows[ , c("cartodb_id", "station_id", "temperature_string", 
                              "dewpoint_string")],2)# pulled from presaved data file

