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


#library(ggmap)
#library(mapproj)
#map <- get_map(zoom = 3)
#ggmap(map)
#library(rworldmap)
library(ggplot2)

flood_data = read.csv("GlobalFloodsRecord.csv")
#newmap <- getMap(resolution = "hi")
#plot(newmap)
#points(as.numeric(as.character(flood_data$Centroid.X)), as.numeric(as.character(flood_data$Centroid.Y)), col = "blue",cex = .6)
flood_data = flood_data[1:3419,]
flood_data$Centroid.X = as.numeric(as.character(flood_data$Centroid.X))
flood_data$Centroid.Y = as.numeric(as.character(flood_data$Centroid.Y))
flood_data$Magnitude..M... = as.numeric(as.character(flood_data$Magnitude..M...))
flood_data$Duration.in.Days = as.numeric(as.character(flood_data$Duration.in.Days))
flood_data$Severity..=  as.numeric(as.character(flood_data$Severity..))
flood_data$Affected.sq.km =  as.numeric(as.character(flood_data$Affected.sq.km))
colnames(flood_data)[19] = "Magnitude"
colnames(flood_data)[12] = "Duration"
colnames(flood_data)[17] = "Severity"


#mp <- NULL
#mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
#mp <- ggplot() + mapWorld
##Now Layer the cities on top
#mp <- mp + geom_point(aes(flood_data$Centroid.X, flood_data$Centroid.Y) ,color="blue", size=1) 
#mp
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


# reasons






