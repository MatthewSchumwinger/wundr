## ---- echo = FALSE-------------------------------------------------------
library(wundr)

## ---- fig.show='hold', warning = FALSE, message = FALSE, eval = FALSE----
#  PWS.Loc.Chicago <- PWS.Locations(-87.6298, 41.87811 radius=5, user.key=jam.key.2)
#  PWS.Loc.Chicago <- PWS.Locations("Chicago, IL", radius=5, user.key=jam.key.2)
#  plot(PWS.Loc.Chicago)   ##yields plot similar to one below

## ---- fig.show='hold', warning = FALSE, message = FALSE, eval = FALSE----
#  PWS.Loc.Sub.Chicago <- PWS.Query.Subset(PWS.Loc.Chicago, -87.62, 41.88, 2)
#  subRegion.Pnts(PWS.Loc.Chicago, PWS.Loc.Sub.Chicago)   ##yields the geospatial plot here below

## ---- fig.show='hold', warning = FALSE, message = FALSE, eval = FALSE----
#  PWS.Conds.Chicago <- PWS.Conditions(PWS.Loc.Chicago, user.key=jam.key.2)
#  PWS.Hist.Chicago <- PWS.History(PWS.Loc.Chicago, "20150306", "20150310", user.key)

## ---- fig.show='hold', warning = FALSE, message = FALSE, include = TRUE----
simple_density(PWS.Conds.Chicago, "Downtown Chicago PWS")

## ---- fig.show='hold', warning = FALSE, message = FALSE, eval = FALSE----
#  my_subset  <-  draw_subset(PWS.Conds.Chicago) ## this is interactive and must run from console

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

