
# This file results in what is saved as "initialData.RData" in the data folder, and 
# individualDays.RData.

# Anna Halldorsdottir
# Fall 2020


#directory = 'CYGNSS/data'
#setwd(directory)

# NECESSARY LIBRARIES ----
library(GpGp)
library(fields)
library(ncdf4)
source('dataprep_functions.R')

# READ IN DATA FOR JUNE 15-19 ----

# JUNE 15.
file = 'data/cyg.ddmi.s20200615-000000-e20200615-235959.l2.wind-mss.a21.d21.nc' 
j15 = nc_open(file)
#names(j15[['var']])
ws15 = ncvar_get(j15, 'wind_speed')
lat15 = ncvar_get(j15, 'lat')
lon15 = ncvar_get(j15, 'lon')
sat15 = ncvar_get(j15, 'spacecraft_num')
time15 = ncvar_get(j15, 'sample_time')/3600
nc_close(j15)

# JUNE 16
file = 'data/cyg.ddmi.s20200616-000000-e20200616-235959.l2.wind-mss.a21.d21.nc' 
j16 = nc_open(file)
#names(j16[['var']])
ws16 = ncvar_get(j16, 'wind_speed')
lat16 = ncvar_get(j16, 'lat')
lon16 = ncvar_get(j16, 'lon')
sat16 = ncvar_get(j16, 'spacecraft_num')
time16 = ncvar_get(j16, 'sample_time')/3600
nc_close(j16)

# JUNE 17
file = 'data/cyg.ddmi.s20200617-000000-e20200617-235959.l2.wind-mss.a21.d21.nc' 
j17 = nc_open(file)
#names(j17[['var']])
ws17 = ncvar_get(j17, 'wind_speed')
lat17 = ncvar_get(j17, 'lat')
lon17 = ncvar_get(j17, 'lon')
sat17 = ncvar_get(j17, 'spacecraft_num')
time17 = ncvar_get(j17, 'sample_time')/3600
nc_close(j17)

# JUNE 18
file = 'data/cyg.ddmi.s20200618-000000-e20200618-235959.l2.wind-mss.a21.d21.nc' 
j18 = nc_open(file)
#names(j18[['var']])
ws18 = ncvar_get(j18, 'wind_speed')
lat18 = ncvar_get(j18, 'lat')
lon18 = ncvar_get(j18, 'lon')
sat18 = ncvar_get(j18, 'spacecraft_num')
time18 = ncvar_get(j18, 'sample_time')/3600
nc_close(j18)

# JUNE 19
file = 'data/cyg.ddmi.s20200619-000000-e20200619-235959.l2.wind-mss.a21.d21.nc' 
j19 = nc_open(file)
#names(j19[['var']])
ws19 = ncvar_get(j19, 'wind_speed')
lat19 = ncvar_get(j19, 'lat')
lon19 = ncvar_get(j19, 'lon')
sat19 = ncvar_get(j19, 'spacecraft_num')
time19 = ncvar_get(j19, 'sample_time')/3600
nc_close(j19)

remove(file)
remove(j15,j16,j17,j18,j19)

# COMBINE DAYS ----

ws = c(ws15,ws16,ws17,ws18,ws19)
lon = c(lon15,lon16,lon17,lon18,lon19)
lat = c(lat15,lat16,lat17,lat18,lat19)
time = c(time15,time16,time17,time18,time19)

# REMOVE NA'S ----

removed = removeNA(ws,lon,lat)

# EXTRA: DEALING WITH INDIVIDUAL DAYS ----

removed15 = removeNA(ws15,lon15,lat15)
removed16 = removeNA(ws16,lon16,lat16)
removed17 = removeNA(ws17,lon17,lat17)
removed18 = removeNA(ws18,lon18,lat18)
removed19 = removeNA(ws19,lon19,lat19)

june15 = cbind(removed15$data, time15[-(removed15$naind)])
june16 = cbind(removed16$data, time16[-(removed16$naind)])
june17 = cbind(removed17$data, time17[-(removed17$naind)])
june18 = cbind(removed18$data, time18[-(removed18$naind)])
june19 = cbind(removed19$data, time19[-(removed19$naind)])

save(june15,june16,june17,june18,june19, file = 'data/individualDays.RData')

# COMBINE INTO ONE MATRIX ----

midJune = cbind(removed$data, time[-(removed$naind)])

# SAVE ----

save(midJune, file = 'data/initialData.RData')
