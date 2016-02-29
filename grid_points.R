master_table = read.csv("MasterTable.csv")
X_na = is.na(as.numeric(as.character(master_table$Centroid.X)))
Y_na = is.na(as.numeric(as.character(master_table$Centroid.Y)))
new_master = master_table[(!X_na)&(!Y_na),]
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