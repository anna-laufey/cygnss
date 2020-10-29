
# Exploring the CYGNSS data.

# NECESSARY TOOLS ----
library(animation)
library(maps)
load('data/initialData.RData')
source('sampling_function.R')

# ALL DATA ----

# Initial graph.
png(filename = '../newplots/midJune.png', width = 1200, height = 600, bg = 'white')
quilt.plot(x = midJune[,2], y = midJune[,3], z = midJune[,1], nx = 1000, ny = 1000, xlab = "Longitude", ylab = "Latitude", main = "Global Wind Speeds, June 15-19 (ALL SATELLITES)")
dev.off()

# ANIMATION OF ALL DAYS ----

# BY DAY
saveGIF({
  
  quilt.plot(june15[,2],june15[,3], june15[,1], nx = 1000, ny = 1000, xlab="Longitude",ylab="Latitude",legend.lab = "windspeed (m/s)", main="June 15, 2020")
  map("world2",add=TRUE)
  quilt.plot(june16[,2],june16[,3], june16[,1], nx = 1000, ny = 1000, xlab="Longitude",ylab="Latitude",legend.lab = "windspeed (m/s)", main="June 16, 2020")
  map("world2",add=TRUE)
  quilt.plot(june17[,2],june17[,3], june17[,1], nx = 1000, ny = 1000, xlab="Longitude",ylab="Latitude",legend.lab = "windspeed (m/s)", main="June 17, 2020")
  map("world2",add=TRUE)
  quilt.plot(june18[,2],june18[,3], june18[,1], nx = 1000, ny = 1000, xlab="Longitude",ylab="Latitude",legend.lab = "windspeed (m/s)", main="June 18, 2020")
  map("world2",add=TRUE)
  quilt.plot(june19[,2],june19[,3], june19[,1], nx = 1000, ny = 1000, xlab="Longitude",ylab="Latitude",legend.lab = "windspeed (m/s)", main="June 19, 2020")
  map("world2",add=TRUE)
  
})

# BY HOUR, OVER FIVE DAYS
inds = list(june15[,4] < 6, ((june15[,4]  < 12) & (june15[,4]  > 6)), ((june15[,4]  < 18) & (june15[,4]  > 12)), ((june15[,4]  < 24) & (june15[,4]  > 18)),
            june16[,4] < 6, ((june16[,4]  < 12) & (june16[,4]  > 6)), ((june16[,4]  < 18) & (june16[,4]  > 12)), ((june16[,4]  < 24) & (june16[,4]  > 18)),
            june17[,4] < 6, ((june17[,4]  < 12) & (june17[,4]  > 6)), ((june17[,4]  < 18) & (june17[,4]  > 12)), ((june17[,4]  < 24) & (june17[,4]  > 18)),
            june18[,4] < 6, ((june18[,4]  < 12) & (june18[,4]  > 6)), ((june18[,4]  < 18) & (june18[,4]  > 12)), ((june18[,4]  < 24) & (june18[,4]  > 18)),
            june19[,4] < 6, ((june19[,4]  < 12) & (june19[,4]  > 6)), ((june19[,4]  < 18) & (june19[,4]  > 12)), ((june19[,4]  < 24) & (june19[,4]  > 18)))

saveGIF({
   #June 15
  for (i in 1:4) {
      #png(filename = paste('../newplots/june15_',i,sep=''), width = 1200, height = 600, bg = 'white')
      quilt.plot(june15[inds[[i]],2],june15[inds[[i]],3], june15[inds[[i]],1], nx = 1000, ny = 1000, xlab="Longitude",ylab="Latitude",legend.lab = "windspeed (m/s)", main="June 15")
      map("world2",add=TRUE)
    #dev.off()
      }
  
  #June 16
  for (i in 5:8) {
    #png(filename = paste('../newplots/june16_',i,sep=''), width = 1200, height = 600, bg = 'white')
    quilt.plot(june16[inds[[i]],2],june16[inds[[i]],3], june16[inds[[i]],1], nx = 1000, ny = 1000, xlab="Longitude",ylab="Latitude",legend.lab = "windspeed (m/s)", main="June 16")
    map("world2",add=TRUE)
    #dev.off()
  }
  #June 17
  for (i in 9:12) {
    #png(filename = paste('../newplots/june17_',i,sep=''), width = 1200, height = 600, bg = 'white')
    quilt.plot(june17[inds[[i]],2],june17[inds[[i]],3], june17[inds[[i]],1], nx = 1000, ny = 1000, xlab="Longitude",ylab="Latitude",legend.lab = "windspeed (m/s)", main="June 17")
    map("world2",add=TRUE)
    #dev.off()
  }
  #June 18
  for (i in 13:16) {
    #png(filename = paste('../newplots/june18_',i,sep=''), width = 1200, height = 600, bg = 'white')
    quilt.plot(june18[inds[[i]],2],june18[inds[[i]],3], june18[inds[[i]],1], nx = 1000, ny = 1000, xlab="Longitude",ylab="Latitude",legend.lab = "windspeed (m/s)", main="June 18")
    map("world2",add=TRUE)
    #dev.off()
  }
  #June 19
  for (i in 17:20) {
    #png(filename = paste('../newplots/june19_',i,sep=''), width = 1200, height = 600, bg = 'white')
    quilt.plot(june19[inds[[i]],2],june19[inds[[i]],3], june19[inds[[i]],1], nx = 1000, ny = 1000, xlab="Longitude",ylab="Latitude",legend.lab = "windspeed (m/s)", main="June 19")
    map("world2",add=TRUE)
    #dev.off()
  }
})


# SUBSAMPLES ----

r = CYGNSSsampling(midJune, 500000, type='random')
png(filename = '../newplots/randomAll.png', width = 1200, height = 600, bg = 'white')
quilt.plot(x = r[,2], y = r[,3], z = r[,1], nx = 1000, ny = 1000, xlab = "Longitude", ylab = "Latitude", main = "Global Wind Speeds, June 15-19 (ALL SATELLITES) \n Random Subsample (n=500,000)")
dev.off()

e = CYGNSSsampling(midJune, 500000, type='every-few')
png(filename = '../newplots/everyfewAll.png', width = 1200, height = 600, bg = 'white')
quilt.plot(x = e[,2], y = e[,3], z = e[,1], nx = 1000, ny = 1000, xlab = "Longitude", ylab = "Latitude", main = "Global Wind Speeds, June 15-19 (ALL SATELLITES) \n Every-Few Subsample (n=500,000)")
dev.off()

a = CYGNSSsampling(midJune, 500000, type='averaging')
png(filename = '../newplots/averagingAll.png', width = 1200, height = 600, bg = 'white')
quilt.plot(x = a[,2], y = a[,3], z = a[,1], nx = 1000, ny = 1000, xlab = "Longitude", ylab = "Latitude", main = "Global Wind Speeds, June 15-19 (ALL SATELLITES) \n Averaging Subsample (n=500,000)")
dev.off()
