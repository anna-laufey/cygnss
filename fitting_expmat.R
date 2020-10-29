
# Fitting exponential_spheretime and matern_spheretime

# LOADING REQUIRED TOOLS ----
load('data/initialData.RData')
source('sampling_function.R')


# GET SUBSAMPLES OF 20,000 ----
mJ20k_r = CYGNSSsampling(midJune, 20000, type='random')
quilt.plot(x = mJ20k_r[,2], y = mJ20k_r[,3], z = mJ20k_r[,1], nx = 1000, ny = 1000, xlab = "Longitude", ylab = "Latitude", main = "Global Wind Speeds, June 15-19 (ALL SATELLITES) \n Random Subsample (n=20,000)")
mJ20k_e = CYGNSSsampling(midJune, 20000, type='every-few')
quilt.plot(x = mJ20k_e[,2], y = mJ20k_e[,3], z = mJ20k_e[,1], nx = 1000, ny = 1000, xlab = "Longitude", ylab = "Latitude", main = "Global Wind Speeds, June 15-19 (ALL SATELLITES) \n Every-Few Subsample (n=20,000)")


mediantime <- median(mJ20k_r[,4]) #12.16014
latgrid <- seq( min(mJ20k_r[,3]), max(mJ20k_r[,3], length.out = 60 ))
longrid <- seq( 0, 360, length.out = 121)[1:120] # so no locations repeated
locs_pred <- as.matrix( expand.grid(longrid,latgrid) )
n_pred <- nrow(locs_pred)
locstime_pred <- cbind( locs_pred, rep(mediantime, n_pred) )
X_pred <- as.matrix( rep(1,n_pred) )

# EXPONENTIAL_SPHERETIME: NOTHING SPECIFIED (RANDOM) ----

mJ20k_r_expsphtime = fit_model(mJ20k_r[,1], mJ20k_r[,2:4], covfun_name = "exponential_spheretime")

pred_mJ20k_r_expsphtime <- predictions(fit = mJ20k_r_expsphtime, 
                      locs_pred = locstime_pred, X_pred = X_pred,
                      st_scale = c(0.1,24))

zlims <- range(c(pred_mJ20k_r_expsphtime))
pred_array <- array( pred_mJ20k_r_expsphtime, c(length(longrid),length(latgrid)) )
fields::image.plot(longrid,latgrid,pred_array,zlim=zlims,main = "Prediction for noon from June 15-19, 2020 \n(w/exponential_sphere)")
map("world2",add=TRUE)

# EXPONENTIAL_SPHERETIME: NOTHING SPECIFIED (EVERY-FEW) ----

mJ20k_e_expsphtime = fit_model(mJ20k_e[,1], mJ20k_e[,2:4], covfun_name = "exponential_spheretime")

pred_mJ20k_e_expsphtime <- predictions(fit = mJ20k_e_expsphtime, 
                                       locs_pred = locstime_pred, X_pred = X_pred,
                                       st_scale = c(0.1,24))

zlims <- range(c(pred_mJ20k_e_expsphtime))
pred_array <- array( pred_mJ20k_e_expsphtime, c(length(longrid),length(latgrid)) )
fields::image.plot(longrid,latgrid,pred_array,zlim=zlims,main = "Prediction for noon from June 15-19, 2020 \n(w/exponential_spheretime & 'every-few')")
map("world2",add=TRUE)

# EXPONENTIAL_SPHERETIME: ST_SCALE(c(0.1,24)) ----

mJ20k_r_expsphtime_st24 = fit_model(mJ20k_r[,1], mJ20k_r[,2:4], covfun_name = "exponential_spheretime", st_scale=c(0.1,24))

pred_mJ20k_r_expsphtime_st24 <- predictions(fit = mJ20k_r_expsphtime_st24, 
                                            locs_pred = locstime_pred, X_pred = X_pred,
                                            st_scale = c(0.1,24))

zlims <- range(c(pred_mJ20k_r_expsphtime_st24))
pred_array <- array( pred_mJ20k_r_expsphtime_st24, c(length(longrid),length(latgrid)) )
fields::image.plot(longrid,latgrid,pred_array,zlim=zlims,main = "Prediction for noon from June 15-19, 2020 \n(w/exponential_sphere, st_scale = c(0.1,24))")
map("world2",add=TRUE)

# EXPONENTIAL_SPHERETIME: ST_SCALE(c(0.1,100)) ----

mJ20k_r_expsphtime_st100 = fit_model(mJ20k_r[,1], mJ20k_r[,2:4], covfun_name = "exponential_spheretime", st_scale=c(0.1,100))

pred_mJ20k_r_expsphtime_st100 <- predictions(fit = mJ20k_r_expsphtime_st100, 
                                       locs_pred = locstime_pred, X_pred = X_pred,
                                       st_scale = c(0.1,24))

zlims <- range(c(pred_mJ20k_r_expsphtime_st100))
pred_array <- array( pred_mJ20k_r_expsphtime_st100, c(length(longrid),length(latgrid)) )
fields::image.plot(longrid,latgrid,pred_array,zlim=zlims,main = "Prediction for noon from June 15-19, 2020 \n(w/exponential_sphere, st_scale = c(0.1,100))")
map("world2",add=TRUE)

# EXPONENTIAL_SPHERETIME: ST_SCALE(c(0.1,200)) (EVERY-FEW) ----

mJ20k_e_expsphtime = fit_model(mJ20k_e[,1], mJ20k_e[,2:4], covfun_name = "exponential_spheretime", st_scale=c(0.1,200))

pred_mJ20k_e_expsphtime <- predictions(fit = mJ20k_e_expsphtime, 
                                       locs_pred = locstime_pred, X_pred = X_pred,
                                       st_scale = c(0.1,200))

zlims <- range(c(pred_mJ20k_e_expsphtime))
pred_array <- array( pred_mJ20k_e_expsphtime, c(length(longrid),length(latgrid)) )
fields::image.plot(longrid,latgrid,pred_array,zlim=zlims,main = "Prediction for noon from June 15-19, 2020 \n(w/exponential_spheretime & 'every-few', st_scale = c(0.1,200))")
map("world2",add=TRUE)

# EXPONENTIAL_SPHERETIME: ST_SCALE(c(0.1,1000)) ----

mJ20k_r_expsphtime_st1000 = fit_model(mJ20k_r[,1], mJ20k_r[,2:4], covfun_name = "exponential_spheretime", st_scale=c(0.1,1000))

pred_mJ20k_r_expsphtime_st1000 <- predictions(fit = mJ20k_r_expsphtime_st1000, 
                                            locs_pred = locstime_pred, X_pred = X_pred,
                                            st_scale = c(0.1,1000))

zlims <- range(c(pred_mJ20k_r_expsphtime_st1000))
pred_array <- array( pred_mJ20k_r_expsphtime_st1000, c(length(longrid),length(latgrid)) )
fields::image.plot(longrid,latgrid,pred_array,zlim=zlims,main = "Prediction for noon from June 15-19, 2020 \n(w/exponential_sphere, st_scale = c(0.1,1000))")
map("world2",add=TRUE)

# EXPONENTIAL_SPHERETIME: CHECKING ST_SCALE VALUES (RANDOM) ----

vals = c(1,12,24,60,100,120,150,200,360)
n = length(vals)
tempscale = rep(NA, n)

for (i in 1:n) {
  temp = fit_model(mJ20k_r[,1], mJ20k_r[,2:4], covfun_name = "exponential_spheretime", st_scale=c(0.1,vals[i]))
  tempscale[i] = temp$covparms[3]
}

plot(vals, tempscale, ylab = "temporal scale", ylim = c(1.3,1.4), type='l')


# EXPONENTIAL_SPHERETIME: CHECKING ST_SCALE VALUES (EVERY-FEW) ----

vals = c(1,12,24,60,100,120,150,200,360)
n = length(vals)
tempscale = rep(NA, n)

for (i in 1:n) {
  temp = fit_model(mJ20k_e[,1], mJ20k_e[,2:4], covfun_name = "exponential_spheretime", st_scale=c(0.1,vals[i]))
  tempscale[i] = temp$covparms[3]
}

plot(vals, tempscale, ylab = "temporal scale", type='l')


# MATERN_SPHERETIME: NOTHING SPECIFIED  ----
mJ20k_r_matsphtime = fit_model(mJ20k_r[,1], mJ20k_r[,2:4], covfun_name = "matern_spheretime")

pred_mJ20k_r_matsphtime <- predictions(fit = mJ20k_r_matsphtime, 
                                       locs_pred = locstime_pred, X_pred = X_pred)

zlims <- range(c(pred_mJ20k_r_matsphtime))
pred_array <- array( pred_mJ20k_r_matsphtime, c(length(longrid),length(latgrid)) )
fields::image.plot(longrid,latgrid,pred_array,zlim=zlims,main = "Prediction for noon from June 15-19, 2020 \n(w/matern_spheretime)")
map("world2",add=TRUE)

# MATERN_SPHERETIME ----

vals = c(1,12,24,60,100,120,150,200,360)
n = length(vals)
tempscale = rep(NA, n)
smooth = rep(NA, n)

for (i in 1:n) {
  temp = fit_model(mJ20k_r[,1], mJ20k_r[,2:4], covfun_name = "matern_spheretime", st_scale=c(0.1,vals[i]))
  tempscale[i] = temp$covparms[3]
  smooth[i] = temp$covparms[5]
  
  predtemp <- predictions(fit = temp, locs_pred = locstime_pred, X_pred = X_pred)
  
  zlims <- range(c(predtemp))
  pred_array <- array( predtemp, c(length(longrid),length(latgrid)) )
  str = paste("Prediction for noon from June 15-19, 2020 \n(w/matern_spheretime, st_scale = c(0.1,",vals[i],")", sep="")
  png(file = paste('mat',i,'.png',sep=""), width = 1200, height = 600)
  fields::image.plot(longrid,latgrid,pred_array,zlim=zlims,main = str)
  map("world2",add=TRUE)
  dev.off()
  
}

png(file = 'matern_temporalscale.png', width = 1200, height = 600)
plot(vals, tempscale, ylab = "temporal scale", type='l')
dev.off()
png(file = 'matern_smooth.png', width = 1200, height = 600)
plot(vals, smooth, ylab = "smoothness", type='l')
dev.off()



