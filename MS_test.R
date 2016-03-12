# Matt's testing scripts for wundr
matt.wu.key <- "fd9858dfc94a85ea"
matt.cdb.key <- "f09ad502b34fa4096a62ea306b4650337d41009c"
matt.cdb.account <- "biglakedata"

# get weather data
desMoines_coord <- c(-93.609106, 41.600545)
dm_PWS <- PWS_meta_query(desMoines_coord[1], desMoines_coord[2], 10, matt.wu.key)
dm_cond <- PWS_conditions(dm_PWS, matt.wu.key)

# plots for Stefan computation
