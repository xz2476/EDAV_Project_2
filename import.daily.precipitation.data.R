library(chron)
library(ncdf4)
##-------------------------------------------##
#      Import precipition data
##-------------------------------------------##  
setwd("~/desktop/github/EDAV_Project_2/pre")
readfile <- function(name){
  fid_pre <- nc_open(name)
  #print(fid_pre)
  #Dimensions:
  time_pre <- ncvar_get(fid_pre,"time",verbose = FALSE)
  lon_pre <- ncvar_get(fid_pre,"lon", verbose = FALSE)-180
  lat_pre <- ncvar_get(fid_pre, "lat", verbose = FALSE)
  
  #Variable:
  precip <- ncvar_get(fid_pre,"precip")
  dim(precip)
  
  # Time and Location:
  ltime_pre <- chron(c(0:364), origin = c(1, 1, 1985))
  ltime_pre <- as.Date(ltime_pre)
  lonlat_pre <- expand.grid(lon_pre,lat_pre)
  
  # make matrix:
  precip.period <- as.vector(precip)
  precip.period[is.na(precip)] <- 0
  precip.mat <- matrix(precip.period, nrow = dim(lon_pre)*dim(lat_pre), ncol = length(ltime_pre))
  precip.mat <- data.frame(cbind(lonlat_pre, precip.mat))
  colnames(precip.mat) <- c("Longitude","Latitude",as.character(ltime_pre))
  precip.mat <- precip.mat[precip.mat$Longitude >= (75) & precip.mat$Longitude <= (92.5) & precip.mat$Latitude >= 37.5 & precip.mat$Latitude <= 47.5,]
  
  return(precip.mat)
}  


precip.mat <- readfile("precip.V1.0.1985.nc")
for (i in 1986:2016){
  name <- paste("precip.V1.0",i,"nc", sep = ".") 
  precip.add <- readfile(name)
  precip.mat <- cbind(precip.mat,precip.add[,-c(1,2)])
}

csvfile <- "precip(1985-2016).csv"
write.table(precip.mat, csvfile, row.names = FALSE, sep = ",")
