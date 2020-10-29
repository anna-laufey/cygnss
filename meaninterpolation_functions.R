
# Basic mean interpolation functions.

interpolateMeans <- function(y, missings, d = 10) {
  # y - object (? x 4 [ws, lon, lat])
  # missings - object (? x 2 [lon, lat])
  # d - radius of lon/lat to average over
  
  ws = y[,1]
  lon = y[,2]
  lat = y[,3]
  
  missingLon = missings[,1]
  missingLat = missings[,2]
  
  n = length(missingLon)
  len = length(lon)
  
  wind = rep(NA, n)
  
  for (i in 1:n) {
    
    # Longitude
    upperLon = missingLon[i] + d
    lowerLon = missingLon[i] - d
    
    lowLon = (1:len)[lon < upperLon]
    upLon = (1:len)[lon > lowerLon]
    indLon = intersect(lowLon, upLon)
    
    # Latitude
    upperLat = missingLat[i] + d
    lowerLat = missingLat[i] - d
    
    lowLat = (1:len)[lat < upperLat]
    upLat = (1:len)[lat > lowerLat]
    indLat = intersect(lowLat, upLat)
    
    # Combined
    ind = intersect(indLon,indLat)
    
    wind[i] = mean(ws[ind])
    
  }
  
  return(wind)
  
}

interpolateMeans_time <- function(y, missings, d = 10, t = 4) {
  ## NOT DONE !!!
  
  # y - object (? x 4 [ws, lon, lat, time])
  # missings - object (? x 3 [lon, lat, time])
  # d - radius of lon/lat to average over
  # t - half life for weight of time (in hours)
  
  ws = y[,1]
  lon = y[,2]
  lat = y[,3]
  time = y[,4]
  
  missingLon = missings[,1]
  missingLat = missings[,2]
  missingTime = missings[,3]
  
  n = length(missingLon)
  len = length(lon)
  
  wind = rep(NA, n)
  
  for (i in 1:n) {
    
    # Longitude
    upperLon = missingLon[i] + d
    lowerLon = missingLon[i] - d
    
    lowLon = (1:len)[lon < upperLon]
    upLon = (1:len)[lon > lowerLon]
    indLon = intersect(lowLon, upLon)
    
    # Latitude
    upperLat = missingLat[i] + d
    lowerLat = missingLat[i] - d
    
    lowLat = (1:len)[lat < upperLat]
    upLat = (1:len)[lat > lowerLat]
    indLat = intersect(lowLat, upLat)
    
    # Combined
    ind = intersect(indLon,indLat)
    
    tt = time[ind]
    
    
    wind[i] = weighted.mean(ws[ind], weights)
    
  }
  
  return(wind)
  
}