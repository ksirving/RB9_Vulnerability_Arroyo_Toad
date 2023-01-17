#DOING PCA FOR CURRENT MODEL

library(vegan)
setwd("D:/MaxEnt/2013SeptModels/Current/Misc and Orig Data/Prep and MaxEnt")
source(file = "HighstatLib.R")

pres<-read.csv("200mCells_FullData_PresAbs_Complete_ThinnedCols.csv")

head(pres)
colnames(pres)

pres.AllandTC<-pres[,c(5,6,7,8,12:51,66:71,88:93,96:100)] #Note- need to use 96-100 to include clay; previously used 97 accidentally
 colnames(pres.AllandTC)

presCurr<-pres.AllandTC
colnames(presCurr)<- c("MRVBF","CatchArea","Elev","VRM03","VRM18","Ppt01","Ppt02","Ppt03","Ppt04","Ppt05","Ppt06","Ppt07","Ppt08","Ppt09","Ppt10","Ppt11","Ppt12","Ppt13","TMx01","TMx02","TMx03","TMx04","TMx05","TMx06","TMx07","TMx08","TMx09","TMx10","TMx11","TMx12","TMx13","TMn01","TMn02","TMn03","TMn04","TMn05","TMn06","TMn07","TMn08","TMn09","TMn10","TMn11","TMn12","TMn13","Brt03.Med","Brt03.Var","Grn03.Med","Grn03.Var","Wet03.Med","Wet03.Var","Brt09.Med","Brt09.Var","Grn09.Med","Grn09.Var","Wet09.Med","Wet09.Var","Clay","Silt","Sand","WaterSt","Slope")

colnames(presCurr)
presCurr.pca<-rda(presCurr, scale=TRUE)
bstick(presCurr.pca)
screeplot(presCurr.pca, bstick=TRUE)
summary(presCurr.pca, display="species")

screeplot(presCurr.pca, npcs=15, bstick=TRUE)
t_loadings.curr<-scores(presCurr.pca,choices=c(1:10), display="species")
t_loadings.curr
t_loadings.curr2<-scores(presCurr.pca,choices=c(1:10), display="species", scaling=0)
t_loadings.curr3<-scores(presCurr.pca,choices=c(1:10), display="species", scaling=3)
head(t_loadings.curr3)
head(t_loadings.curr)
head(t_loadings.curr2)

#output<-cbind(pres[,c(1:4)],t_loadings)

#Write out Unscaled Species Scores = TRUE VARIABLE LOADINGS
write.csv(t_loadings.curr2, file="D:/MaxEnt/2013SeptModels/Current/Misc and Orig Data/Prep and MaxEnt/1981_CurrPCA_Scale0.csv", quote=TRUE, row.names=TRUE)


pdf("D:/MaxEnt/2013SeptModels/Current/Misc and Orig Data/Prep and MaxEnt/1981_CurrPCA_Biplots.pdf", width=8, height=8)
for (i in 2:10){
plot(presCurr.pca, display="species", choices=c(1,i))
}
dev.off()


t_loadings<-as.data.frame(scores(presCurr.pca,choices=c(1:10), display="sites"))
head(t_loadings)

output2<-cbind(pres[,c(101,1:4)],t_loadings)
head(output2)

dim(output2)


write.table(output2, file="PresAbs_allpts_CORRECT.csv", quote=TRUE, row.names=FALSE,sep=",")




library(rgdal)
library(raster)


bg.pres <- readOGR(dsn = "D:/MaxEnt/2013SeptModels/Current/Misc and Orig Data/Prep and MaxEnt/bgPres_Convert_MFD.shp", layer = "bgPres_Convert_MFD")

#bg.pres<-read.table("PresAbs_allpts_CORRECT.csv", head=TRUE, sep=",")

#head(bg.pres)

#bg.pres.spdf<-SpatialPointsDataFrame(bg.pres[,4:5],data.frame(bg.pres[,4:15]))
#proj4string(bg.pres.spdf)="+proj=utm +zone=11 +datum=WGS84"
#plot(bg.pres.spdf)

#writeOGR(bg.pres.spdf, "bgPresTest.shp", "bgPresTest", driver="ESRI Shapefile")


head(bg.pres@data)
plot(bg.pres)
#Load in Raster that you want to use as a template
NHD200<-raster("D:/MaxEnt/2013SeptModels/Historic/Saga/NHD24k_SPLIT No0_Dissolve All [ID].sdat")

plot(NHD200, add=T)

#Can also create raster to use as template...

## Rasterize the shapefile
#Create list of column names you want to rasterize
fields<-c("PC1", "PC2", "PC3", "PC4", "PC4", "PC5", "PC6", "PC7", "PC8","PC9","PC10")

#Create a "rasterBrick" of the template raster
x<-brick(NHD200)

# Parallel processing doesnt seem to do anything for this, but I left the code commented out in case you want it
#library(doParallel)
#cl <- makeCluster(8) # number of cores to use
#registerDoParallel(cl)
 for (i in fields){
 x[[i]]<-rasterize(bg.pres, NHD200, field=i)
 }
 #endCluster()

y<-stack(x[[2]],x[[3]],x[[4]],x[[5]],x[[6]],x[[7]],x[[8]],x[[9]],x[[10]],x[[11]])
projection(y)<-"+proj=utm +zone=11 +datum=WGS84"
plot(y)
writeRaster(y, "D:/MaxEnt/2013SeptModels/Current/Misc and Orig Data/Prep and MaxEnt/CurrentGridFeb14.grd", format="raster", crs="+proj=utm +zone=11 +datum=WGS84", overwrite=TRUE)

