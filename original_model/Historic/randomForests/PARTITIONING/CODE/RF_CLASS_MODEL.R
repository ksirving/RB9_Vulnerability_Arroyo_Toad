##########################################################################
# RANDOM FOREST MODEL AND RASTER PREDICTION
##########################################################################
# CONTACT:
#     Jeffrey S. Evans
#     Senior Landscape Ecologist  
#     The Nature Conservancy
#     Central Science/DbyD
#     Laramie, WY 82070 
#     jeffrey_evans@tnc.org
#     (970) 672-6766
##########################################################################
require(randomForest)
require(raster)
require(rgdal)
require(raster)
path="D:/IALE2013/PARTITIONING"
source(paste(paste(path, "DATA", sep="/"), "Functions.R", sep="/"))

# Training data
inshape="PresAbs"

# NUMBER OF BOOTSTRAP REPLICATES
b=1001 

##########################################################################
# CREATE TRANING DATA
##########################################################################
# READ AND CHECK RASTERS, CREATE RASTER STACK
rlist=list.files(paste(path, "DATA", sep="/"), pattern="img$", full.names=TRUE) 
  rlist <- rlist[-c(1,9,10,12,14,16,18)]
              
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
  
# READ SPECIES OBSERVATION DATA
sdata <- readOGR(dsn=paste(path, "DATA", sep="/"), layer=inshape)
  str(sdata@data)
  
# FETCH RASTER VALUES FOR X VARABLES AND JOIN TO SPECIES OBSERVATION DATA
sdata@data <- data.frame(sdata@data, extract(xvars, sdata))
  str(sdata@data)  

# ITTERATIVE TEST FOR MULTI-COLINEARITY
cl <- MultiColinear(sdata@data[,3:ncol(sdata@data)], p=0.05)
 xdata <- sdata@data[,3:ncol(sdata@data)]  
  for(l in cl) {
    cl.test <- xdata[,-which(names(xdata)==l)]
	  print(paste("REMOVE VARIABLE", l, sep=": "))
    MultiColinear(cl.test, p=0.05)	
  }

# REMOVE MULTI-COLINEAR VARIABLES
for(l in cl) { sdata@data <- sdata@data[,-which(names(sdata@data)==l)] }

##########################################################################
# RANDOM FORESTS CLASSIFICATION MODEL
##########################################################################
# CREATE X,Y DATA
ydata <- as.factor(sdata@data[,"Present"])
xdata <- sdata@data[,3:ncol(sdata@data)]

# PERCENT OF PRESENCE OBSERVATIONS
( dim(sdata[sdata$Present == 1, ])[1] / dim(sdata)[1] ) * 100

# RUN RANDOM FORESTS MODEL SELECTION FUNCTION
( rf.model <- rf.modelSel(x=xdata, y=ydata, imp.scale="mir", ntree=b) ) 

# CREATE NEW XDATA BASED ON SELECTED MODEL AND RUN FINAL RF MODEL
sel.vars <- rf.model$PARAMETERS[[4]]
  rf.data <- data.frame(y=ydata, xdata[,sel.vars])	
( rf.final <- randomForest(y=rf.data[,1], x=rf.data[,2:ncol(rf.data)], ntree=b, 
                           importance=TRUE, norm.votes=TRUE, proximity=TRUE) )

					    
# PLOT BOOTSTRAP ERROR CONVERGENCE
plot(rf.final, main="Bootstrap Error Convergence")

# PLOT VARIABLE IMPORTANCE
  p <- as.matrix(rf.final$importance[,3])    
    ord <- rev(order(p[,1], decreasing=TRUE)[1:dim(p)[1]])  
  dotchart(p[ord,1], main="Scaled Variable Importance", pch=19)						     

###################################################  
# PREDICT MODEL PROBABILITIES RASTER
###################################################
predict()
  
###################################################
# PARTIAL PLOTS WITH LOESS SMOOTHING
###################################################
sp=0.80
pdf(paste(path,"PartialPlots.pdf", sep="/"), width=8, height=8)
  for (i in 1:length(names(rf.data[,2:ncol(rf.data)])) ) {
    p <- partialPlot(rf.final, rf.data[,2:ncol(rf.data)], 
       names(rf.data[,2:ncol(rf.data)])[i], which.class="1", plot=FALSE)   
    p$y <- (p$y - min(p$y)) / (max(p$y) - min(p$y)) 
 plot( y=lowess(y=p$y, x=p$x, f=sp)$y, x=p$x, type="l", ylab="p",  
         xlab=names(rf.data[,2:ncol(rf.data)])[i],
         main=paste("PARTIAL PLOT", names(rf.data[,2:ncol(rf.data)])[i], sep=" - ")) 
  }
dev.off() 

###################################################	   
# BIVARIATE KERNEL DENSITY ESTIMATE 
###################################################
require(ks)
ks.data <- data.frame(p=rf.prob[,2], map=xdata[,"map30"])
   H.scv <- Hscv(x=ks.data)
   fhat <- kde(x=ks.data, H=H.scv)
plot(fhat, display="persp", border=NA, col="grey96", shade=0.75)

###################################################
# SAVE R IMAGE 
save.image( paste(path, "RFModel.RData", sep="/") ) 
###################################################

# PLOT PROTOTYPES (MULTIVARIATE CLASS CENTERS)					  
rf.p <- classCenter(xdata, ydata, rf.final$prox)
v1=8; v2=9
plot(xdata[,v1], xdata[,v2], pch=21, xlab=names(xdata)[v1], ylab=names(xdata)[v2],
     bg=c("red", "blue", "green")[as.numeric(factor(ydata))], main="Data with Prototypes")
    points(rf.p[,v1], rf.p[,v2], pch=21, cex=2.5, bg=c("red", "blue", "green"))					  

###################################################
# MULTIDIMENSIONAL SCALING (MDS) PLOTS
###################################################

# FUNCTION TO CREATE COLOR VECTOR
cRamp <- function(x,d=c("blue", "red")){
  crange <- function(x)(as.numeric(x)-min(as.numeric(x)))/diff(range(as.numeric(x)))
    cols <- colorRamp(d)(crange(x))
  apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
}  

# CREATE CMD SCALED DISTANCES OF RF PROXMITIES
rf.cmd <- cmdscale(1 - rf.final$proximity, eig=TRUE, k=4)  
  # pa.col = ifelse(ydata == "1", "red", ifelse(ydata == "0", "black", NA))
  pa.col <- cRamp(ydata)  
    rf.cmd <- data.frame(rf.cmd$points)

# 2D PLOT OF FIRST TWO MDS DIMENSIONS	
plot(rf.cmd[,1:2], ylab="DIM 1", xlab="DIM 2", pch=16, col=pa.col,
      main= paste("Pres/Abs", "PROXIMITY MATRIX MDS d=2", sep=" - "))
  legend("bottomleft",pch=c(16,16), col=c("black", "red"), 
         legend=c("Absent","Present")) 

# 3D PLOT OF FIRST THREE MDS DIMENSIONS	
require(rgl)	   
  plot3d(rf.cmd[,1],rf.cmd[,2],rf.cmd[,3], col=pa.col,
         pch=18, size=1.25, type="s", xlab="MDS dim 1", ylab="MDS dim 2", 
	       zlab="MDS dim 3")	
		   
