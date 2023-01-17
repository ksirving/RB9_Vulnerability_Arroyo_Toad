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
path="D:/IALE2013_WORKSHOP/PARTITIONING/RFLAB"
setwd(path)
source(paste(paste(path, "CODE", sep="/"), "functions.R", sep="/"))
inshape="Abundance"

# NUMBER OF BOOTSTRAP REPLICATES
b=501 
  
# BUILD MODEL DATA  
rlist=list.files(paste(path, "DATA", sep="/"), pattern="img$", full.names=TRUE) 
  rlist <- rlist[-c(9,10,12,14,16)]
    rlist <- rlist[-c(1,13)]  
      xvars <- stack(rlist) 
sdata <- readOGR(dsn=paste(path, "DATA", sep="/"), layer=inshape)
  sdata@data$ABUN <- sdata@data$ABUN + 1  
sdata@data <- data.frame(sdata@data, extract(xvars, sdata))

# TEST FOR MULTI-COLINEARITY
cl <- MultiColinear(sdata@data[,4:ncol(sdata@data)], p=0.05)
 xdata <- sdata@data[,4:ncol(sdata@data)]  
  for(l in cl) {
    cl.test <- xdata[,-which(names(xdata)==l)]
	  print(paste("REMOVE VARIABLE", l, sep=": "))
    MultiColinear(cl.test, p=0.05)	
  }
for(l in cl) { sdata@data <- sdata@data[,-which(names(sdata@data)==l)] }

# CREATE X,Y DATA
ydata <- sdata@data[,"ABUN"]
xdata <- sdata@data[,4:ncol(sdata@data)]

# RUN RANDOM FORESTS MODEL SELECTION FUNCTION
( rf.model <- rf.modelSel(x=xdata, y=ydata, imp.scale="mir", ntree=b) ) 

# CREATE NEW XDATA BASED ON SELECTED MODEL AND RUN FINAL RF MODEL
sel.vars <- rf.model$SELVARS
  rf.data <- data.frame(y=ydata, xdata[,sel.vars])	
( rf.final <- randomForest(y=rf.data[,1], x=rf.data[,2:ncol(rf.data)], ntree=b, 
                           importance=TRUE) )

# Predict abundance regression model
predict(xvars, rf.final, file="AbunRegression.img", na.rm=TRUE, overwrite=TRUE,
       progress="window") 

# Premutate model (not run, takes time)
#( rf.test <- rf.Permutation(y=rf.data[,1], x=rf.data[,2:ncol(rf.data)], nperm=999,
#                            ntree=501) )
							
	   