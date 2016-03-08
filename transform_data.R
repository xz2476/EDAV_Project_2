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
