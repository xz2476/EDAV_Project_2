##-------------------------------------------##
#      Data Cleaning
##-------------------------------------------## 
#setwd("~/desktop/github/EDAV_Project_2")
MasterTable <- read.csv("MasterTable.csv",nrows = 4319)
MasterTable$Centroid.X <- as.numeric(as.character(MasterTable$Centroid.X))
MasterTable$Centroid.Y <- as.numeric(as.character(MasterTable$Centroid.Y))
MasterTable <- MasterTable[!is.na(MasterTable$Centroid.X),]

MasterTable$Country <- as.character(MasterTable$Country)
MasterTable$Country <- gsub("\xff","",MasterTable$Country)
MasterTable$Severity.. <- as.numeric(as.character(MasterTable$Severity..))

##-------------------------------------------##
#      Grid_points
##-------------------------------------------## 
new_master = MasterTable
num_record = nrow(new_master)

# compute the influence grid point of each flood
flood_grid = matrix(rep(F,num_record*40),ncol = 40)
rownames(flood_grid) = new_master$Register..
colnames(flood_grid) = 1:40
X_grid = seq(-92.5,-75,2.5)
Y_grid = seq(47.5,37.5,-2.5)

# points distance (km)
two_point_distance = function(x1,y1,x2,y2){
  x1 = 2*pi*x1/360
  x2 = 2*pi*x2/360
  y1 = 2*pi*y1/360
  y2 = 2*pi*y2/360
  dlon = x2 - x1 
  dlat = y2 - y1 
  a = (sin(dlat/2))^2 + cos(y1) * cos(y2) * (sin(dlon/2))^2 
  c = 2 * atan2( sqrt(a), sqrt(1-a) ) 
  d = 6372 * c 
  return (d)
}

# loop through all floods
for (i in 1:num_record){
  X_flood = as.numeric(as.character(new_master$Centroid.X[i]))
  Y_flood = as.numeric(as.character(new_master$Centroid.Y[i]))
  affect = as.numeric(as.character(new_master$Affected.sq.km[i]))
  r = (affect/pi)^0.5
  for (j in 1:40){
    temp = j%%8
    if (temp == 0){
      X_gridpoint = X_grid[8]
    }
    else{
      X_gridpoint = X_grid[j%%8]
    }
    Y_gridpoint = Y_grid[ceiling(j/8)]
    if (two_point_distance(X_gridpoint,Y_gridpoint,X_flood,Y_flood)<= r){
      flood_grid[i,j] = T
    }
  }
}

# date
Began = as.Date(new_master$Began,"%d-%b-%y")
End = as.Date(new_master$Ended,"%d-%b-%y")

# 
days = rep(seq(min(Began),max(End),1),40)
num_days = length(days)/40
points = matrix(matrix(rep(1:40,num_days),ncol = 40, byrow = T),ncol = 1)
result = cbind(days,points)
result = cbind(result,rep(F,num_days))
for (i in 1:num_record){
  for (j in 1:40){
    if (flood_grid[i,j] == T){
      B = as.numeric(as.Date(as.character(new_master$Began[i]),"%d-%b-%y"))
      E = as.numeric(as.Date(as.character(new_master$Ended[i]),"%d-%b-%y"))
      temp = (result[,1]>=B)&(result[,1]<=E)&(result[,2]==j)
      result[temp,3] = T
    }
  }
}

csvfile <- "grid_point_result.csv"
write.table(result, csvfile, row.names = FALSE, sep = ",")
##-------------------------------------------##
#      Import pressure data
##-------------------------------------------## 
#setwd("~/desktop/github/EDAV_Project_2/pre")
readfile <- function(name,i){
  fid_pre <- nc_open(name)
  print(fid_pre)
  #Dimensions:
  time_pre <- ncvar_get(fid_pre,"time",verbose = FALSE)
  lon_pre <- ncvar_get(fid_pre,"lon", verbose = FALSE)-360
  lat_pre <- ncvar_get(fid_pre, "lat", verbose = FALSE)
  
  #Variable:
  precip <- ncvar_get(fid_pre,"precip")
  dim(precip)
  
  # Time and Location:
  ltime_pre <- chron(c(0:(length(time_pre)-1)), origin = c(1, 1, i))
  ltime_pre <- as.Date(ltime_pre)
  lonlat_pre <- expand.grid(lon_pre,lat_pre)
  
  # make matrix:
  precip.period <- as.vector(precip)
  precip.period[is.na(precip)] <- 0
  precip.mat <- matrix(precip.period, nrow = dim(lon_pre)*dim(lat_pre), ncol = length(ltime_pre))
  precip.mat <- data.frame(cbind(lonlat_pre, precip.mat))
  colnames(precip.mat) <- c("Longitude","Latitude",as.character(ltime_pre))
  precip.mat <- precip.mat[precip.mat$Longitude >= (-92) & precip.mat$Longitude <= (-75) & precip.mat$Latitude >= 37.5 & precip.mat$Latitude <= 47.5,]
  
  return(precip.mat)
}  


precip.mat <- readfile("precip.V1.0.1985.nc",1985)
for (i in 1986:2016){
  name <- paste("precip.V1.0",i,"nc", sep = ".") 
  precip.add <- readfile(name,i)
  precip.mat <- cbind(precip.mat,precip.add[,-c(1,2)])
}


csvfile <- "precip(1985-2016).csv"
write.table(precip.mat, csvfile, row.names = FALSE, sep = ",")


##-------------------------------------------##
#      Import precipitation data
##-------------------------------------------## 
library(chron)
library(ncdf4)
##-------------------------------------------##
#      Import precipition data
##-------------------------------------------##  
#setwd("~/desktop/github/EDAV_Project_2/pre")
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

##-------------------------------------------##
#      Map_plot
##-------------------------------------------## 
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


library(ggplot2)
flood_data = MasterTable
flood_data$Magnitude..M... = as.numeric(as.character(flood_data$Magnitude..M...))
flood_data$Duration.in.Days = as.numeric(as.character(flood_data$Duration.in.Days))
flood_data$Severity..=  as.numeric(as.character(flood_data$Severity..))
flood_data$Affected.sq.km =  as.numeric(as.character(flood_data$Affected.sq.km))
colnames(flood_data)[19] = "Magnitude"
colnames(flood_data)[12] = "Duration"
colnames(flood_data)[17] = "Severity"

attach(flood_data)
# four standers
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld
mp <- mp+ geom_point(aes(Centroid.X, Centroid.Y,size = Magnitude),shape=21,colour = "black",fill="cornsilk",alpha = 0.5)
mp <- mp + scale_size_area(breaks= c(2,3,4,5,6,7),trans = 'exp',max_size = 15)

mp2 <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp2 <- ggplot() +   mapWorld
mp2 <- mp2+ geom_point(aes(Centroid.X, Centroid.Y,size = Duration),shape=21,colour = "black",fill="cornsilk",alpha = 0.5)
mp2 <- mp2 + scale_size_area(max_size=15)

mp3 <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp3 <- ggplot() +   mapWorld
mp3 <- mp3+ geom_point(aes(Centroid.X, Centroid.Y,size = Severity),shape=21,colour = "black",fill="cornsilk",alpha = 0.5)
mp3 <- mp3 + scale_size_area(breaks= c(1,1.5,2),trans = 'exp',max_size = 8)

mp4 <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp4 <- ggplot() +   mapWorld
mp4 <- mp4+ geom_point(aes(Centroid.X, Centroid.Y,size = Affected.sq.km),shape=21,colour = "black",fill="cornsilk",alpha = 0.5)
mp4 <- mp4 + scale_size_area(max_size=15)

multiplot(mp, mp2, mp3, mp4, cols=1)


# four seasons (1995-2015 (21 years))
# Spring: March - May
# Summer: June - Aug
# Autume: Sep - Nov
# Winter: Dec - Feb
Began = as.Date(Began,"%d-%b-%y")
Month = as.numeric(format(Began, "%m"))
detach(flood_data)
Spring = flood_data[(Month %in% c(3,4,5))[1:3393],19:21]
Summer = flood_data[(Month %in% c(6,7,8))[1:3393],19:21]
Autume = flood_data[(Month %in% c(9,10,11))[1:3393],19:21]
Winter = flood_data[(Month %in% c(12,1,2))[1:3393],19:21]

mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld
mp <- mp+ geom_point(aes(Spring$Centroid.X, Spring$Centroid.Y,size = Spring$Magnitude),shape=21,colour = "black",fill="cornsilk",alpha = 0.5)
mp <- mp + scale_size_area(breaks= c(2,3,4,5,6,7),trans = 'exp',max_size = 15)

mp2 <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp2 <- ggplot() +   mapWorld
mp2 <- mp2+ geom_point(aes(Summer$Centroid.X, Summer$Centroid.Y,size = Summer$Magnitude),shape=21,colour = "black",fill="cornsilk",alpha = 0.5)
mp2 <- mp2 + scale_size_area(breaks= c(2,3,4,5,6,7),trans = 'exp',max_size = 15)

mp3 <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp3 <- ggplot() +   mapWorld
mp3 <- mp3+ geom_point(aes(Autume$Centroid.X, Autume$Centroid.Y,size = Autume$Magnitude),shape=21,colour = "black",fill="cornsilk",alpha = 0.5)
mp3 <- mp3 + scale_size_area(breaks= c(2,3,4,5,6,7),trans = 'exp',max_size = 15)

mp4 <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp4 <- ggplot() +   mapWorld
mp4 <- mp4+ geom_point(aes(Winter$Centroid.X, Winter$Centroid.Y,size = Winter$Magnitude),shape=21,colour = "black",fill="cornsilk",alpha = 0.5)
mp4 <- mp4 + scale_size_area(breaks= c(2,3,4,5,6,7),trans = 'exp',max_size = 15)
multiplot(mp, mp2, mp3, mp4, cols=1)

##-------------------------------------------##
#      Plot Floods in the US
##-------------------------------------------## 
library(ggmap)
library(mapproj)
library(rworldmap)
library(ggplot2)
state <- map_data("state")
state <- state[state$lat >= 35,]
USA <- MasterTable[MasterTable$Centroid.X >= (-125) & MasterTable$Centroid.X <= (-65.0) & MasterTable$Centroid.Y >= 35 & MasterTable$Country == "USA", ]
ggplot(state, aes(long,lat,label = name))+ borders("state")+scale_size_area()+coord_quickmap()+
  geom_point(data = USA, aes(Centroid.X, Centroid.Y),size = 2,colour = "red",alpha = 1)+
  geom_segment(aes(x = -75, y = 37.5, xend = -92.5, yend = 37.5),color = "blue")+
  geom_segment(aes(x = -75, y = 47.5, xend = -92.5, yend = 47.5),color = "blue")+
  geom_segment(aes(x = -75, y = 37.5, xend = -75, yend = 47.5),color = "blue")+
  geom_segment(aes(x = -92.5, y = 37.5, xend = -92.5, yend = 47.5),color = "blue")

##-------------------------------------------##
#      Transform_data
##-------------------------------------------## 
library(xlsx)
MasterTable <- read.xlsx(
  "data/GlobalFloodsRecord.xls", sheetName='MasterTable', 
  endRow=4320, colIndex=c(10, 12, 18:21),
  colClasses=c("character", rep("numeric", 7))
)
MasterTable <- MasterTable[rowSums(is.na(MasterTable)) == 0, ]
names(MasterTable) <- c(
  "began", "duration", "area", "magnitude", "x", "y"
)
MasterTable$area <- round(MasterTable$area, 0)
MasterTable$magnitude <- round(MasterTable$magnitude, 0)
MasterTable$x <- round(MasterTable$x, 1)
MasterTable$y <- round(MasterTable$y, 1)
write.csv(MasterTable, "data/floods.csv", row.names=FALSE)

 