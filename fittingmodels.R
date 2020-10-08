
### FITTING MODELS 

# PREP ----
library(GpGp)
library(fields)
library(ncdf4)

satellite <- function(ws, sat, lat, lon, n, time = NULL) {
  ## returns just the ws, lat, and lon for only satellite one
  
  len = length(ws)
  
  ind = (1:len)[sat != n]
  
  ws = ws[-ind]
  lat = lat[-ind]
  lon = lon[-ind]
  time = time[-ind]
  
  return(list("ws" = ws, "lat" = lat, "lon" = lon, "time" = time))
  
  
}
removeNA <- function(ws,lon,lat) {
  ## removes NA values in ws and the corresponding observation from lon/lat.
  ## returns ws, lon, and lat without any NA's. also returns naind, the indices of removed NA's.
  
  len = length(ws)
  sumna = apply(!is.na(ws), 1, sum)
  naind = (1:len)[sumna==0]
  
  ws = ws[-(naind)]
  lon = lon[-(naind)]
  lat = lat[-(naind)]
  
  return(list("ws" = ws, "lon" = lon, "lat" = lat, "naind" = naind))
}

directory = '/Users/annalaufey/Desktop/data' # CHANGE
setwd(directory)

# READING IN JUNE 15.
file = 'cyg.ddmi.s20200615-000000-e20200615-235959.l2.wind-mss.a21.d21.nc' 
j15 = nc_open(file)
names(j15[['var']])
ws15 = ncvar_get(j15, 'wind_speed')
lat15 = ncvar_get(j15, 'lat')
lon15 = ncvar_get(j15, 'lon') - 180  # need to subtract 180 to make longitude between -180 and 180, as opposed to 0<lon<360.
sat15 = ncvar_get(j15, 'spacecraft_num')
time15 = ncvar_get(j15, 'sample_time')/3600

# JUNE 15, SATELLITE ONE ----

sat1_15 = satellite(ws15, sat15, lat15, lon15, 1, time15) # only use observations labelled Satellite One
removed1_15 = removeNA(sat1_15$ws,sat1_15$lon,sat1_15$lat) # remove the NA's to prep for model fitting

naind1_15 = removed1_15$naind # remember the indices of the NA values

# The locations we do have.
loc1_15 = cbind(removed1_15$lon, removed1_15$lat)
loctime1_15 = cbind(removed1_15$lon, removed1_15$lat, sat1_15$time[-naind1_15])

# Design Matrix.
dm1_15 = rep(1,length(naind1_15))
dm1_15 = as.matrix(dm1_15)

# The missing locations,
missloc1_15 = cbind(sat1_15$lon[naind1_15], sat1_15$lat[naind1_15])
missloctime1_15 = cbind(sat1_15$lon[naind1_15], sat1_15$lat[naind1_15], sat1_15$time[naind1_15])

# Locations put together for graphing long vs lat individually.
long1_15 = c(sat1_15$lon[naind1_15], removed1_15$lon)
lati1_15 = c(sat1_15$lat[naind1_15], removed1_15$lat)

# JUNE 15, SATELLITE ONE: MATERN_ISOTROPIC ----

#fm1_matiso = fit_model(removed1_15$ws, loc1_15) 
#pred1_matiso = predictions(fit = fm1_matiso, locs_pred = missloc1_15, X_pred = dm1_15) 
#save(fm1_matiso, pred1_matiso, file = 'sat1_matiso.RData')

load('sat1_matiso.RData')

# Combined predicted wind speed and what we had before.
matiso1_15 = c(pred1_matiso, removed1_15$ws)

quilt.plot(x = removed1_15$lon, y = removed1_15$lat, z = removed1_15$ws, nx = 1000, ny = 1000, xlab = "Longitude", ylab = "Latitude", main = "Global Wind Speeds for June 15, 2020, Satellite One")
quilt.plot(x = sat1_15$lon[naind1_15], y = sat1_15$lat[naind1_15], z = pred1_matiso, nx = 1000, ny = 1000, xlab = "Longitude", ylab = "Latitude", main = "matern_isotropic predictions for June 15, 2020, Satellite One")
quilt.plot(x = long1_15, y = lati1_15, z = matiso1_15, nx = 1000, ny = 1000, xlab = "Longitude", ylab = "Latitude", main = "Satellite One for June 15, 2020 (w/ matern_isotropic)")


# JUNE 15, SATELLITE ONE: EXPONENTIAL_SPHERETIME ----


#fm1_expsphtime = fit_model(removed1_15$ws, loctime1_15, covfun_name = "exponential_spheretime")
#pred1_expsphtime = predictions(fit = fm1_expsphtime, locs_pred = missloctime1_15, X_pred = dm1_15) 
#save(fm1_expsphtime, pred1_expsphtime, file = 'sat1_expsphtime.RData')
load('sat1_expsphtime.RData')

# Combined predicted wind speed and what we had before.
expsphtime1_15 = c(pred1_expsphtime, removed1_15$ws)
comtime1_15 = c(sat1_15$time[naind1_15], sat1_15$time[-naind1_15])/3600

quilt.plot(x = removed1_15$lon, y = removed1_15$lat, z = removed1_15$ws, nx = 1000, ny = 1000, xlab = "Longitude", ylab = "Latitude", main = "Global Wind Speeds for June 15, 2020, Satellite One")
quilt.plot(x = sat1_15$lon[naind1_15], y = sat1_15$lat[naind1_15], z = pred1_expsphtime, nx = 1000, ny = 1000, xlab = "Longitude", ylab = "Latitude", main = "exponential_spheretime predictions for June 15, 2020, Satellite One")
quilt.plot(x = long1_15, y = lati1_15, z = expsphtime1_15, nx = 1000, ny = 1000, xlab = "Longitude", ylab = "Latitude", main = "Satellite One for June 15, 2020 (w/ exponential_spheretime)")

# PLOT BY HOUR
par(mfrow=c(2,2))
inds <- comtime1_15 < 6
quilt.plot(long1_15[inds],lati1_15t[inds], expsphtime1_15[inds], nx = 1000, ny = 1000, xlab="Longitude",ylab="Latitude",legend.lab = "windspeed (m/s)")

inds <- ((comtime1_15  < 12) && (comtime1_15  > 6))
quilt.plot(long1_15[inds],lati1_15t[inds], expsphtime1_15[inds], nx = 1000, ny = 1000, xlab="Longitude",ylab="Latitude",legend.lab = "windspeed (m/s)")

inds <- ((comtime1_15  < 18) && (comtime1_15  > 12))
quilt.plot(long1_15[inds],lati1_15t[inds], expsphtime1_15[inds], nx = 1000, ny = 1000, xlab="Longitude",ylab="Latitude",legend.lab = "windspeed (m/s)")

inds <- ((comtime1_15  < 24) && (comtime1_15  > 18))
quilt.plot(long1_15[inds],lati1_15t[inds], expsphtime1_15[inds], nx = 1000, ny = 1000, xlab="Longitude",ylab="Latitude",legend.lab = "windspeed (m/s)")



# SATELLITE TWO ----

