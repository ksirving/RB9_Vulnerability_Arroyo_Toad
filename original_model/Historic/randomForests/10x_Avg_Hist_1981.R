library(raster)

###########################################
## Deal with Historic/Potential models#####
###########################################
h1<-raster("D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final/Model1/SppProbs.img")
h2<-raster("D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final/Model2/SppProbs.img")
h3<-raster("D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final/Model3/SppProbs.img")
h4<-raster("D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final/Model4/SppProbs.img")
h5<-raster("D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final/Model5/SppProbs.img")
h6<-raster("D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final/Model6/SppProbs.img")
h7<-raster("D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final/Model7/SppProbs.img")
h8<-raster("D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final/Model8/SppProbs.img")
h9<-raster("D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final/Model9/SppProbs.img")
h10<-raster("D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final/Model10/SppProbs.img")

sh<-stack(c(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10))
plot(sh)

hAvg<-(h1+h2+h3+h4+h5+h6+h7+h8+h9+h10)/10
plot(hAvg)
writeRaster(hAvg, "D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final/hAvg.img") #Writes into Erdas Imagine Images (.img)

###########################################
## Deal with Current models################
###########################################
c1<-raster("D:/AT DistributionModeling/Current/randomForests/PARTITIONING/Cur_10x_Final/Model1/SppProbs.img")
c2<-raster("D:/AT DistributionModeling/Current/randomForests/PARTITIONING/Cur_10x_Final/Model2/SppProbs.img")
c3<-raster("D:/AT DistributionModeling/Current/randomForests/PARTITIONING/Cur_10x_Final/Model3/SppProbs.img")
c4<-raster("D:/AT DistributionModeling/Current/randomForests/PARTITIONING/Cur_10x_Final/Model4/SppProbs.img")
c5<-raster("D:/AT DistributionModeling/Current/randomForests/PARTITIONING/Cur_10x_Final/Model5/SppProbs.img")
c6<-raster("D:/AT DistributionModeling/Current/randomForests/PARTITIONING/Cur_10x_Final/Model6/SppProbs.img")
c7<-raster("D:/AT DistributionModeling/Current/randomForests/PARTITIONING/Cur_10x_Final/Model7/SppProbs.img")
c8<-raster("D:/AT DistributionModeling/Current/randomForests/PARTITIONING/Cur_10x_Final/Model8/SppProbs.img")
c9<-raster("D:/AT DistributionModeling/Current/randomForests/PARTITIONING/Cur_10x_Final/Model9/SppProbs.img")
c10<-raster("D:/AT DistributionModeling/Current/randomForests/PARTITIONING/Cur_10x_Final/Model10/SppProbs.img")
sc<-stack(c(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10))
plot(sc)
cAvg<-(c1+c2+c3+c4+c5+c6+c7+c8+c9+c10)/10
plot(cAvg)
writeRaster(cAvg, "D:/AT DistributionModeling/Current/randomForests/PARTITIONING/Cur_10x_Final/cAvg.img", format="HFA")




###########################################
## Deal with Hist 1981 models################
###########################################


hb1<-raster("D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final_1981/Model1/SppProbs.img")
hb2<-raster("D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final_1981/Model2/SppProbs.img")
hb3<-raster("D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final_1981/Model3/SppProbs.img")
hb4<-raster("D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final_1981/Model4/SppProbs.img")
hb5<-raster("D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final_1981/Model5/SppProbs.img")
hb6<-raster("D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final_1981/Model6/SppProbs.img")
hb7<-raster("D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final_1981/Model7/SppProbs.img")
hb8<-raster("D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final_1981/Model8/SppProbs.img")
hb9<-raster("D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final_1981/Model9/SppProbs.img")
hb10<-raster("D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final_1981/Model10/SppProbs.img")

shb<-stack(c(hb1,hb2,hb3,hb4,hb5,hb6,hb7,hb8,hb9,hb10))
plot(shb)

hbAvg<-(hb1+hb2+hb3+hb4+hb5+hb6+hb7+hb8+hb9+hb10)/10
plot(hbAvg)
writeRaster(hbAvg, "D:/AT DistributionModeling/Historic/randomForests/PARTITIONING/Hist_10x_Final_1981/hbAvg.img") #Writes into Erdas Imagine Images (.img)