
# Functions to prep the CYGNSS data to be used.

  ## returns just the ws, lat, and lon for only satellite n
satellite <- function(ws, sat, lat, lon, n, time = NULL) {
  ## returns just the ws, lat, and lon for only satellite n
  
  len = length(ws)
  
  ind = (1:len)[sat != n]
  
  ws = ws[-ind]
  lat = lat[-ind]
  lon = lon[-ind]
  time = time[-ind]
  
  return(list("ws" = ws, "lat" = lat, "lon" = lon, "time" = time))
  
  
}

  ## removes NA values in ws and the corresponding observation from lon/lat.
removeNA <- function(ws,lon,lat) {
  ## removes NA values in ws and the corresponding observation from lon/lat.
  ## returns ws, lon, and lat, combined, without any NA's. also returns naind, the indices of removed NA's.
  
  len = length(ws)
  sumna = !is.na(ws)
  naind = (1:len)[sumna==0]
  
  ws = ws[-(naind)]
  lon = lon[-(naind)]
  lat = lat[-(naind)]
  
  y = cbind(ws,lon,lat)
  
  return(list("data" = y, "naind" = naind))
}