InstallPkgs <- function(pkgs) {
    pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
    if (length(pkgs_miss) > 0) {
        install.packages(pkgs_miss, dep=TRUE, repos="http://cran.us.r-project.org")
      }  
      pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
    if (length(pkgs_miss) > 0) {
        install.packages(pkgs_miss, dep=TRUE, repos="http://cran.us.r-project.org")
      }
    attached <- search()
      attached_pkgs <- attached[grepl("package", attached)]
         need_to_attach <- pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))]
    if (length(need_to_attach) > 0) {
      for (i in 1:length(need_to_attach)) require(need_to_attach[i], character.only = TRUE)
      }
    }
InstallPkgs(c("sp","rgdal","raster","randomForest","kernlab","rgl","ks"))
#Normally need to install these packages,"rgl","ks"
library(sp)
library(rgdal)
library(raster)
library(randomForest)
library(kernlab)
library(rgl)
library(ks)
library(sm)


################# PLEASE CHANGE YOUR DIRECTORY PATH HERE #################
#wsdir="D:/MaxEnt/2013SeptModels/Current/randomForests"

	

##########################################################################
##########################################################################
##########################################################################
# CHUNK 2
# Random Forest Binary Classification Model 
##########################################################################
##########################################################################
##########################################################################
wsdir="D:/AT DistributionModeling/Current/randomForests"
#wsdir="E:/MLT DATA/AT DistributionModeling/Current/randomForests"
path=paste(wsdir, "PARTITIONING", sep="/") 
setwd(path)
source(paste(paste(path, "DATA3", sep="/"), "Functions.R", sep="/"))

# Training data
inshape="200mCells_PresAbs2005 PCA PresAbs Pts_MLT" ## presence absence

# NUMBER OF BOOTSTRAP REPLICATES
b=10001



#########################
#  CREATE TRANING DATA  #
#########################
# READ AND CHECK RASTERS, CREATE RASTER STACK
#Uses SP and RASTER Packages - do wildcard for images
#
rlist=list.files(paste(path, "DATA3", sep="/"), pattern="grd", full.names=TRUE)  ## env vars
  #rlist <- rlist[-c(1,9,10,12,14,16,18)]
              
  r.info <- as.data.frame(array(0, dim=c( 0, 11)))
    names(r.info) <- c("RASTER", "MIN", "MAX", "ROWS", "COLS", "NCELLS", "CELLSIZE", "
                        XMIN", "XMAX", "YMIN", "YMAX")
  for (i in rlist ) { 
    r <- raster(i) 
      e <- extent(r)
      rsum <- data.frame(RASTER=i, MIN=minValue(r), MAX=maxValue(r), 
                         ROWS=nrow(r), COLS=ncol(r), NCELLS=ncell(r), 
	  					 CELLSIZE=res(r)[1], XMIN=e@xmin, XMAX=e@xmax, 
	  					 YMIN=e@ymin, YMAX=e@ymax)
      r.info <- rbind(rsum, r.info)						   
        }
# write.csv(r.info, "RasterSummaries.csv", row.names=FALSE)

# CREATE RASTER STACK
xvars <- stack(rlist)      
#plot(xvars)
# READ SPECIES OBSERVATION DATA - Converts .SHP to R format
#For S4 objects, use @ instead of $ whiel it is still in slot
orig.sdata<-sdata <- readOGR(dsn=paste(path, "DATA3", sep="/"), layer=inshape) ## p/a
  str(sdata@data)
proj4string(sdata)<-CRS("+proj=utm +zone=11 +datum=WGS84")
proj4string(orig.sdata)<-CRS("+proj=utm +zone=11 +datum=WGS84")

#sdata <- sdata[1]
str(sdata)
proj4string(sdata)<-CRS("+proj=utm +zone=11 +datum=WGS84")

####Standardize Environ Vars
# xvarsTrans <- xvars
# for(i in 1:dim(xvars)[3]){
  # xvarsTrans[[i]] <- (xvars[[i]]-cellStats(xvars[[i]],mean))/cellStats(xvars[[i]],sd)
# }
# mask <- xvarsTrans[[1]]>-1000

##################################################
## KDE Bias Surface
##################################################
# develop KDE sampling bias surface
orig.sdata2<-subset(orig.sdata, PresAbs200==1)
mask <- xvars[[1]]>-1000
bias <- cellFromXY(mask, orig.sdata[,-1])
cells <- unique(sort(bias))
kernelXY <- xyFromCell(mask, cells)
samps <- as.numeric(table(bias))

# code to make KDE raster
KDEsur <- sm.density(kernelXY, weights=samps, display="none", ngrid=812, 
                     ylim=c(3600402,3730202), xlim=c(423638,563638), nbins=0)
KDErast=SpatialPoints(expand.grid(x=KDEsur$eval.points[,1], y=KDEsur$eval.points[,2]))
KDErast = SpatialPixelsDataFrame(KDErast, data.frame(kde = array(KDEsur$estimate, 
                                                    length(KDEsur$estimate))))
KDErast <- raster(KDErast)
KDErast <- resample(KDErast, mask)
KDErast <- KDErast*mask
KDEpts <- rasterToPoints(KDErast, spatial=F) #(Potential) PseudoAbsences are created here 

#Now to integrate Pseudoabsences into Presence Data
a=KDEpts[sample(seq(1:nrow(KDEpts)), size=702, replace=T, prob=KDEpts[,"layer"]),1:2]

PA.abs<-data.frame(PresAbs200=rep(0,nrow(a)))
head(PA.abs)
a.sp<-SpatialPoints(a, proj4string=CRS("+proj=utm +zone=11 +datum=WGS84"))
a.spdf<-SpatialPointsDataFrame(a.sp, PA.abs)
sdata<-rbind(sdata,a.spdf)
plot(sdata)
# FETCH RASTER VALUES FOR X VARABLES AND JOIN TO SPECIES OBSERVATION DATA (Assigns raster values to points)
sdata@data <- data.frame(sdata@data, extract(xvars, sdata))
  str(sdata@data)  

  # cl <- MultiColinear(sdata@data[,3:ncol(sdata@data)], p=0.05)
 # xdata <- sdata@data[,3:ncol(sdata@data)]  
  # for(l in cl) {
    # cl.test <- xdata[,-which(names(xdata)==l)]
	  # print(paste("REMOVE VARIABLE", l, sep=": "))
    # MultiColinear(cl.test, p=0.05)	
  # }

# # REMOVE MULTI-COLINEAR VARIABLES
# for(l in cl) { sdata@data <- sdata@data[,-which(names(sdata@data)==l)] }

###########################################
#   RANDOM FORESTS CLASSIFICATION MODEL   #
###########################################
# CREATE X,Y DATA
# head(ydata)
# head(sdata@data)
ydata <- as.factor(sdata@data[,"PresAbs200"])
xdata <- sdata@data[,2:ncol(sdata@data)]

# PERCENT OF PRESENCE OBSERVATIONS
( dim(sdata[sdata$PresAbs200 == 1, ])[1] / dim(sdata)[1] ) * 100

# RUN RANDOM FORESTS MODEL SELECTION FUNCTION
#Also provides variable importance and such
#It is important to look at highest class error - Look at Global OOB errors
#TEST Object is super important; Row number corresponds to paramerers, in output
( rf.model <- rf.modelSel(x=xdata, y=ydata, imp.scale="mir", ntree=b, nodesize=5) ) 

# CREATE NEW XDATA BASED ON SELECTED MODEL AND RUN FINAL RF MODEL (Model 4)
#RF runs differently when you use symbolis languate (using ~ as in an Lin. Model... use the indexing approacy [y=rf.data[,1]...]
sel.vars <- rf.model$PARAMETERS[[1]]#Set to use All Vars
  rf.data <- data.frame(y=ydata, xdata[,sel.vars])	
( rf.final <- randomForest(y=rf.data[,1], x=rf.data[,2:ncol(rf.data)], ntree=b, nodesize=5,
                           importance=TRUE, norm.votes=TRUE, proximity=TRUE) )
    
# PLOT BOOTSTRAP ERROR CONVERGENCE
plot(rf.final, main="Bootstrap Error Convergence")

# PLOT VARIABLE IMPORTANCE
  p <- as.matrix(rf.final$importance[,3])    
    ord <- rev(order(p[,1], decreasing=TRUE)[1:dim(p)[1]])  
  dotchart(p[ord,1], main="Scaled Variable Importance", pch=19)
  
# PLOT PROTOTYPES (MULTIVARIATE CLASS CENTERS)					  
rf.p <- classCenter(xdata, ydata, rf.final$prox)
v1=4; v2=1
plot(xdata[,v1], xdata[,v2], pch=21, xlab=names(xdata)[v1], ylab=names(xdata)[v2],
     bg=c("black", "grey94")[as.numeric(factor(ydata))], main="Data with Prototypes")
        points(rf.p[,v1], rf.p[,v2], pch=21, cex=2.5, bg=c("blue", "red"))					  

######################################  
# PREDICT MODEL PROBABILITIES RASTER #
######################################
#Index refers to the right column of probabilities - in this model the second column, which is probs of "1"
predict(xvars, rf.final, filename="Cur_10x_Final_Feb2014/Model1/SppProbs.img", type="prob",  index=2,   
        na.rm=TRUE, overwrite=TRUE, progress="window")   
pred<-raster("Cur_10x_Final_Feb2014/Model1/SppProbs.img")		 
##Plot Predictions with Pres Abs
pdf(paste(path, "Cur_10x_Final_Feb2014/Model1/Map.pdf", sep="/"), width=8, height=8)
plot(pred)
plot(subset(orig.sdata,PresAbs200==1), col="blue", add=T)
plot(subset(orig.sdata,PresAbs200==0), col="red", add=T)
plot(pred, add=T)
dev.off()
######################################
# PARTIAL PLOTS WITH LOESS SMOOTHING #
######################################
sp=0.6
pdf(paste(path,"Cur_10x_Final_Feb2014/Model1/PartialPlots_6.pdf", sep="/"), width=8, height=8)
  p <- as.matrix(rf.final$importance[,3])    
    ord <- rev(order(p[,1], decreasing=TRUE)[1:dim(p)[1]])  
  dotchart(p[ord,1], main="Scaled Variable Importance", pch=19)
plot(rf.final, main="Bootstrap Error Convergence")
  for (i in 1:length(names(rf.data[,2:ncol(rf.data)])) ) {
    p <- partialPlot(rf.final, rf.data[,2:ncol(rf.data)], 
       names(rf.data[,2:ncol(rf.data)])[i], which.class="1", plot=FALSE)   
    p$y <- (p$y - min(p$y)) / (max(p$y) - min(p$y)) 
 plot( y=lowess(y=p$y, x=p$x, f=sp)$y, x=p$x, type="l", ylab="p",  
         xlab=names(rf.data[,2:ncol(rf.data)])[i],
         main=paste("PARTIAL PLOT", names(rf.data[,2:ncol(rf.data)])[i], sep=" - ")) 
  }
dev.off() 
##################################################
# PROXIMITY MULTIDIMENSIONAL SCALING (MDS) PLOTS #
##################################################

######### FUNCTION TO CREATE COLOR VECTOR #########
#AWESOME!!! SAVE THIS FOREVER!!! - Orders color ramp with data#
cRamp <- function(x,d=c("blue", "red")){
  crange <- function(x)(as.numeric(x)-min(as.numeric(x)))/diff(range(as.numeric(x)))
    cols <- colorRamp(d)(crange(x))
  apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
}  
###################################################

# CREATE CMD SCALED DISTANCES OF RF PROXMITIES
rf.cmd <- cmdscale(1 - rf.final$proximity, eig=TRUE, k=4)  
  pa.col <- cRamp(ydata)  
    rf.cmd <- data.frame(rf.cmd$points)

# 2D PLOT OF FIRST TWO MDS DIMENSIONS	
plot(rf.cmd[,1:2], ylab="DIM 1", xlab="DIM 2", pch=16, col=pa.col,
      main= paste("Pres/Abs", "PROXIMITY MATRIX MDS d=2", sep=" - "))
  legend("bottomright", pch=c(16,16), col=c("blue", "red"), 
         legend=c("Absent","Present")) 

# 3D PLOT OF FIRST THREE MDS DIMENSIONS - Interactive using Windows RGL Driver (Library RGL)	   
plot3d(rf.cmd[,1],rf.cmd[,2],rf.cmd[,3], col=pa.col,
    pch=18, size=1.25, type="s", xlab="MDS dim 1", ylab="MDS dim 2", 
      zlab="MDS dim 3")	

# SAVE R IMAGE 
save.image( paste(path, "Cur_10x_Final_Feb2014/Model1/RFClassModel.RData", sep="/") ) 