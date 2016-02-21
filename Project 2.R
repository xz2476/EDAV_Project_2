setwd("~/desktop/github/EDAV_Project_2")
MasterTable <- read.csv("MasterTable.csv",nrows = 4319)
Analyses<- read.csv("Analyses.csv",nrows = 30)
View(MasterTable)
View(Analyses)

#Data Cleanning:
summary(MasterTable)
#Replace missing value with "NA":


library(chron)
library(ncdf4)
fid <- nc_open("NOAA_Daily_phi_500mb.nc")
#Dimensions:
time <- ncvar_get(fid,"T",verbose = FALSE)
lon <- ncvar_get(fid,"X")
pressure <- ncvar_get(fid, "P",verbose = FALSE)
lat <- ncvar_get(fid, "Y", verbose = FALSE)
tunits <- ncatt_get(fid,"T","units")

#Variable:
dname <- "phi"
phi <- ncvar_get(fid,dname)
dim(phi)


# split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth = as.integer(unlist(tdstr)[2])
tday = as.integer(unlist(tdstr)[3])
tyear = as.integer(unlist(tdstr)[1])
ltime <- chron(time, origin = c(tmonth, tday, tyear))

#matrix for one time slice (eg:01/10/48):
m <- which(ltime == "01/10/48")
lonlat <- expand.grid(lon,lat)
phi.slice <- phi[, , m]
phi.vec <- as.vector(phi.slice)
phi.t1 <- data.frame(cbind(lonlat,phi.vec))
colnames(phi.t1) <- c("Longitude","Latitude","Presure")
head(phi.t1)

#matrix for period of time (eg: 01/01/50 ti 01/01/70)
a <- which(ltime == "01/01/50")
b <- which(ltime == "01/01/70")
phi.period <- as.vector(phi[, , a:b])
phi.mat <- matrix(phi.period, nrow = dim(lon)*dim(lat), ncol = (b-a+1))
phi.mat <- data.frame(cbind(lonlat, phi.mat))
colnames(phi.mat) <- c("Longitude","Latitude",as.character(ltime[a:b]))

csvfile <- "phi_location(1950-1970).csv"
write.table(na.omit(phi.mat),csvfile, row.names = FALSE, sep = ",")


#plot:
library(ggmap)
box <- make_bbox(lon= phi.mat$Longitude,lat = phi.mat$Latitude)
map <- get_map(location = box, source = "stamen")
ggmap(map)
qmplot(Longitude,Latitude,data = phi.mat)
