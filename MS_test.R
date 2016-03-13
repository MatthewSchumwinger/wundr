# Matt's testing scripts for wundr
matt.wu.key <- "fd9858dfc94a85ea"
matt.cdb.key <- "f09ad502b34fa4096a62ea306b4650337d41009c"
matt.cdb.account <- "biglakedata"

# get weather data
# dm_cond <- PWS.Conditions("Des Moines, IA", radius=50, user.key=matt.wu.key)
# devtools::use_data(dm_cond, overwrite = T)
data("dm_cond")

# simple plots
simple_pnts(dm_cond, "Hello World!")
simple_density(dm_cond, "Hello World!")

# static map
basemapDM <- set_basemap(dm_cond, zoom = 12)
gg_points(dm_cond, basemapDM)

# interactive web map
webmap_pnts(dm_cond)


# low-level plots for Stefan computation
