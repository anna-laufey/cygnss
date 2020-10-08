
library(maps)
## TO RUN NOW

# DONE: GRAPH MEAN INTERPOLATION ----

load('interpmean_Sat1.RData')

allmean1_15 = c(w, removed1_15$ws)

quilt.plot(x = long1_15, y = lati1_15, z = allmean1_15, nx = 1000, ny = 1000, xlab = "Longitude", ylab = "Latitude", main = "Satellite One for June 15, 2020 (w/ mean method)")




# DONE: GRAPH SPHERETIME PREDICTION BY 6 HOURS ----

comtime1_15 = c(sat1_15$time[naind1_15], sat1_15$time[-naind1_15])
expsphtime1_15 = c(pred1_expsphtime, removed1_15$ws)


par(mfrow=c(2,2))
inds <- comtime1_15 < 6
quilt.plot(long1_15[inds],lati1_15[inds], expsphtime1_15[inds], nx = 1000, ny = 1000, xlab="Longitude",ylab="Latitude", main = "exponential_spheretime, June 15th \n hours 1-6")

inds <- (comtime1_15 < 12) & (comtime1_15 > 6)
quilt.plot(long1_15[inds],lati1_15[inds], expsphtime1_15[inds], nx = 1000, ny = 1000, xlab="Longitude",ylab="Latitude", main = "exponential_spheretime, June 15th \n hours 6-12")

inds <- (comtime1_15 > 12) & (comtime1_15  < 18)
quilt.plot(long1_15[inds],lati1_15[inds], expsphtime1_15[inds], nx = 1000, ny = 1000, xlab="Longitude",ylab="Latitude", main = "exponential_spheretime, June 15th \n hours 12-18")

inds <- (comtime1_15 > 18) & (comtime1_15  < 24)
quilt.plot(long1_15[inds],lati1_15[inds], expsphtime1_15[inds], nx = 1000, ny = 1000, xlab="Longitude",ylab="Latitude", main = "exponential_spheretime, June 15th \n hours 18-24")



# NOT WORK: APPLY & GRAPH SPHERETIME PREDICTION TO ALL LOCATIONS ----

mediantime <- median(sat1_15$time) #10.85385
latgrid <- seq( min(sat1_15$lat), max(sat1_15$lat), length.out = 60 )
longrid <- seq( -180, 180, length.out = 121)[1:120] # so no locations repeated
locs_pred <- as.matrix( expand.grid(longrid,latgrid) )
n_pred <- nrow(locs_pred)
locstime_pred <- cbind( locs_pred, rep(mediantime, n_pred) )
X_pred <- as.matrix( rep(1,n_pred) )

pred <- predictions(fit = fm1_expsphtime, 
                    locs_pred = locstime_pred, X_pred = X_pred,
                    st_scale = c(0.1,24))

quilt.plot(x = locs_pred[,1], y = locs_pred[,2], z = pred, nx = 1000, 
           ny = 1000, xlab = "Longitude", ylab = "Latitude", 
           main = "Satellite One for June 15, 2020 (w/exponential_sphere)")

sim <- cond_sim(fit = fm1_expsphtime, 
                locs_pred = locstime_pred, X_pred = X_pred,
                st_scale = c(0.1,24))

zlims <- range(c(pred))
pred_array <- array( pred, c(length(longrid),length(latgrid)) )
fields::image.plot(longrid,latgrid,pred_array,zlim=zlims)


# DONE: GRAPH SPHERE PREDICTION TO MISSING LOCATIONS ----
#fm1_expsphere = fit_model(removed1_15$ws, loc1_15, covfun_name = "exponential_sphere")
#pred1_expsphere = predictions(fit = fm1_expsphere, locs_pred = missloc1_15, X_pred = dm1_15) 
#save(fm1_expsphere, pred1_expsphere, file = 'sat1_expsphere.RData')

load('sat1_expsphere.RData')

expsphere1_15 = c(pred1_expsphere, removed1_15$ws)
quilt.plot(x = long1_15, y = lati1_15, z = expsphere1_15, nx = 1000, ny = 1000, xlab = "Longitude", ylab = "Latitude", main = "Satellite One for June 15, 2020 (w/exponential_sphere)")


# NOT WORK: APPLY & GRAPH SPHERE PREDICTION TO ALL LOCATIONS ----

latgrid <- seq( min(sat1_15$lat), max(sat1_15$lat), length.out = 60 )
longrid <- seq( -180, 180, length.out = 121)[1:120] # so no locations repeated
locs_pred <- as.matrix( expand.grid(longrid,latgrid) )
n_pred <- nrow(locs_pred)
X_pred <- as.matrix( rep(1,n_pred) )

pred <- predictions(fit = fm1_expsphere, 
                    locs_pred = locs_pred, X_pred = X_pred,
                    st_scale = c(0.1,24))

quilt.plot(x = locs_pred[,1], y = locs_pred[,2], z = pred, nx = 1000, ny = 1000, xlab = "Longitude", ylab = "Latitude", main = "Satellite One for June 15, 2020 (w/exponential_sphere)")

sim <- cond_sim(fit = fm1_expsphtime, 
                locs_pred = locstime_pred, X_pred = X_pred,
                st_scale = c(0.1,24), nsims = 2)

zlims <- range(c(pred,sim))
pred_array <- array( pred, c(length(longrid),length(latgrid)) )
fields::image.plot(longrid,latgrid,pred_array,zlim=zlims)

# READY: K-FOLD CROSS VALIDATION ----
# k = 10, covfun = ‘exponential_sphere’, satellite 1, June 15th
mse_s1_15 = kcv('exponential_sphere', FALSE, sat1_15)
save(mse_s1_15, file = 'mse_s1_15.RData')

# k = 10, covfun = ‘exponential_sphere’, satellite 2, June 15th
sat2_15 = satellite(ws15, sat15, lat15, lon15, 2, time15) # only use observations labelled Satellite One

mse_s2_15 = kcv('exponential_sphere', FALSE, sat2_15)
save(mse_s2_15, file = 'mse_s2_15.RData')

# k = 10, covfun = ‘exponential_spheretime’, satellite 1, June 15th
#mse = kcv('exponential_spheretime', TRUE, sat1_15)
#mse_st1_15 = e
#save(mse_st1_15, file = 'mse_st1_15.RData')

# k = 10, covfun = ‘exponential_spheretime’, satellite 2, June 15th
mse_st2_15 = kcv('exponential_spheretime', TRUE, sat2_15)
save(mse_st2_15, file = 'mse_st2_15.RData')

# k = 10, mean interpolation, satellite 1, June 15th 

kcvMEANINT <- function(sat, k = 10) {
  
  library(GpGp)
  
  # k-fold cross validation function.
  
  # covfun : string of the name of the covariance function to input into GpGp function.
  # temporal : boolean, TRUE if covfun is spatial-temporal, FALSE otherwise
  # sat : resulting list from satellite() function
  # k : the number of test sets to split into
  
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
  interpolateMeans <- function(lon, lat, ws, missingLon, missingLat, d = 10) {
    
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
  
  
  removed = removeNA(sat$ws,sat$lon,sat$lat)
  
  ws = removed$ws
  lat = removed$lat
  lon = removed$lon
  
  n = length(ws)
  kn = floor(n/k)
  
  set.seed(4990)
  ind = sample(1:n, (k*kn), replace=FALSE)
  
  #Design Matrix.
  dm = rep(1,kn)
  dm = as.matrix(dm)
  
  
  errors = rep(NA, k)
  
  for (i in 0:(k-1)) {
    
    cat(i+1,'th training.\n')
    
    train = ind[-(((i*kn)+1):(i*kn)+kn)]
    test = ind[((i*kn)+1):((i*kn)+kn)]
      
    pred = interpolateMeans(lon[train], lat[train], ws[train], lon[test], lat[test])
    
    mse = mean((ws[test] - pred)^2)
    
    cat(i+1, 'th training MSE:', mse, '\n')
    
    errors[i+1] = mse
    
  }
  
  return(mean(errors))
  
  
}
mse_meanint1_15 = kcvMEANINT(sat1_15)
save(mse_meanint1_15, file = 'mse_meanint1_15.RData')

  