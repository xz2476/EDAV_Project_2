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

#matrix for period of time (eg: 01/12/1984 to 01/01/2016)
a <- which(ltime == "11/30/84")
b <- which(ltime == "02/05/16")
phi.period <- as.vector(phi[, , a:b])
phi.mat <- matrix(phi.period, nrow = dim(lon)*dim(lat), ncol = (b-a+1))
phi.mat <- data.frame(cbind(lonlat, phi.mat))
colnames(phi.mat) <- c("Longitude","Latitude",as.character(ltime[a:b]))
ltime <- ltime[a:b]

#Subset the date to the area we are interested in:
MasterTable$Centroid.X <- as.numeric(as.character(MasterTable$Centroid.X))
MasterTable$Centroid.Y <- as.numeric(as.character(MasterTable$Centroid.Y))
MasterTable <- MasterTable[MasterTable$Centroid.X >= (-92.5) & MasterTable$Centroid.X <= (-75) & MasterTable$Centroid.Y >= 37.5 & MasterTable$Centroid.Y <= 47.5, ]
MasterTable <- MasterTable[!is.na(MasterTable$Centroid.X),]
####################################################################

library(ggmap)
library(ggplot2)

#flood for purticular day:
#take subset of a particular day:

#formate the date in "mastertable" to match the date format in "phi.mat":
MasterTable$Began <- as.character(MasterTable$Began)
MasterTable$Began <- as.Date(MasterTable$Began,"%d-%B-%y")

MasterTable$Ended <- as.character(MasterTable$Ended)
MasterTable$Ended <- as.Date(MasterTable$Ended,"%d-%B-%y")

ltime <- as.Date(ltime)
#create subset of "mastertable" contains inforamtion of certain day:
box <- make_bbox(lon = c(-92.5,-75),lat = c(37.5,47.5))
map <- get_map(location = box)

#plot:
for(i in 1:length(ltime)){
  if(any(MasterTable$Began <= ltime[i] & MasterTable$Ended >= ltime[i])==TRUE)
  { 
    df <- MasterTable[MasterTable$Began <= ltime[i] & MasterTable$Ended >= ltime[i] ,]   
    date <-as.character(ltime[i]) 
    x <- ggmap(map) +geom_point(data = df, aes(x = Centroid.X, y = Centroid.Y),color = "red",size = 4)+ ggtitle(date)
    ggsave(x, device = "png",filename = date,path = "~/desktop")
    }
  else{
  }
}
