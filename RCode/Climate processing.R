

install.packages('spocc')
install.packages('sp')
install.packages('dismo')
install.packages('rJava')
install.packages('raster')
install.packages('ENMeval')
install.packages('zoo')
install.packages('readxl')
install.packages('adehabitatHR')
install.packages('plyr')
install.packages('rgdal')
install.packages('dplyr')
install.packages('rgeos')
install.packages('velox')

library(spocc)
library(spThin)
library(dismo)
library(rgeos)
library(ENMeval)
library(dplyr)
library(rJava)
library(stringr)
library(ggplot2)
library(caTools)
library(ncdf4)
library(raster)
library(rgdal)
library(plyr)
library(sp)
library(readxl)
library(reshape2)
library(ggmap)
library(velox)
library(zoo)
library(rgdal)
library(adehabitatHR)
library(doParallel)

### Obtain and Process Environmental Data
### add section about downloading climate data

# Climate Stuff -----------------------------------------------------------

setwd('C:/Users/amealor/Desktop/watershedData/RCode')

# "sheds" is a shapefile of planning level watersheds in california. 
# Climate data will be summarized for each watershed over a 30 year period in a table.
# Data from this table will then be added back into the shapefile for each climate variable used.

sheds <- readOGR(dsn = ".", layer = "GoogleWatershedNames")
sheds <- spTransform(sheds, crs(tmn))

#inititate dataframes that will store climate summaries for each climate variable
setwd('C:/Users/amealor/Desktop/watershedData/cbm_hst')
ppt <- data.frame(matrix(nrow=length(sheds)))
ppt$ID<-sheds$waternum

tmn <- data.frame(matrix(nrow=length(sheds)))
tmn$ID<-sheds$waternum

tmx <- data.frame(matrix(nrow=length(sheds)))
tmx$ID<-sheds$waternum

cwd <- data.frame(matrix(nrow=length(sheds)))
cwd$ID<-sheds$waternum

#Years over which the "historic" climate range will be drawn from
years <- c(1980:2009)

##using the velox package for extra fast raster processing
##this is yearly data
setwd('C:/Users/amealor/Desktop/watershedData/cbm_hst')
for(i in 1:30){
  start_time <- Sys.time()
  ff<- paste("ppt_",years[i],".nc",sep="")
  ## each layer in the stack corresponds to a monthly raster
  temp.stack<-stack(ff)
  ##important to make sure stack has same projection as shapefile
  projection(temp.stack)<-pr
  ## velox is over 10x faster than rasters
  vx <- velox(temp.stack)
  ##extract monthly values
  ex.mat <- vx$extract(sheds,fun = function(x) mean(x, na.rm = TRUE))
  ## add monthly PPT to get yearly PPT
  yearly <- rowSums(ex.mat)
  ## each column corresponds to a year
  ppt <- cbind(ppt,yearly)
  end_time <- Sys.time()
  print(end_time - start_time)
  
}
setwd('C:/Users/amealor/Desktop/watershedData')
##save table of yearly summaries for future processing
write.csv(ppt, file ="yearlyShedppt3.csv", row.names=F)

##repeat the process for TMN, TMX, and CWD min/max
setwd('C:/Users/amealor/Desktop/watershedData/cbm_hst')
for(i in 1:30){
  start_time <- Sys.time()
  ff<- paste("tmn_",years[i],".nc",sep="")
  temp.stack<-stack(ff)
  projection(temp.stack)<-pr
  vx <- velox(temp.stack)
  ex.mat <- vx$extract(sheds,fun = function(x) mean(x, na.rm = TRUE))
  #instead of summing, find abs min
  yearly <- apply(ex.mat,1,min)
  tmn <- cbind(tmn,yearly)
  end_time <- Sys.time()
  print(end_time - start_time)
  
}
setwd('C:/Users/amealor/Desktop/watershedData')
write.csv(tmn, file ="yearlyShedtmn2.csv", row.names=F)

setwd('C:/Users/amealor/Desktop/watershedData/cbm_hst')
for(i in 1:30){
  start_time <- Sys.time()
  ff<- paste("tmx_",years[i],".nc",sep="")
  temp.stack<-stack(ff)
  projection(temp.stack)<-pr
  vx <- velox(temp.stack)
  ex.mat <- vx$extract(sheds,fun = function(x) mean(x, na.rm = TRUE))
  # get yearly max
  yearly <- apply(ex.mat,1,max)
  tmx <- cbind(tmx,yearly)
  end_time <- Sys.time()
  print(end_time - start_time)
  
}
setwd('C:/Users/amealor/Desktop/watershedData')
write.csv(tmx, file ="yearlyShedtmx2.csv", row.names=F)

setwd('C:/Users/amealor/Desktop/watershedData/cbm_hst')
for(i in 1:30){
  start_time <- Sys.time()
  ff<- paste("cwd_",years[i],".nc",sep="")
  temp.stack<-stack(ff)
  projection(temp.stack)<-pr
  vx <- velox(temp.stack)
  ex.mat <- vx$extract(sheds,fun = function(x) mean(x, na.rm = TRUE))
  #sum as with PPT
  yearly <- rowSums(ex.mat)
  cwd <- cbind(cwd,yearly)
  end_time <- Sys.time()
  print(end_time - start_time)
  
}
setwd('C:/Users/amealor/Desktop/watershedData')
write.csv(cwd, file ="yearlyShedcwd2.csv", row.names=F)
## now that wee have yearly summaries for each variable, we need to create 30-year "norms" for each watershed

##maxPPT
ppt <- read.csv("yearlyShedppt3.csv")
#this is currently finding the max of the five year rolling average of yearly PPT over a 30 year period
tabs <- apply(ppt,1,function(x) max(rollmean(as.numeric(x[c(3:32)]),5))/10 )
tabs<- as.data.frame(tabs)
tabs$ID<- ppt$ID
##add the summarized data back onto the shapefile
sheds@data$maxPPT<- tabs$tabs

##min PPT
tabs <- apply(ppt,1,function(x) min(rollmean(as.numeric(x[c(3:32)]),5))/10 )
tabs<- as.data.frame(tabs)
tabs$ID<- ppt$ID
##add the summarized data back onto the shapefile
sheds@data$minPPT<- tabs$tabs

##max cwd
##tabulate yearly data for each watershed
cwd <- read.csv("yearlyShedcwd2.csv")
tabs <- apply(cwd,1,function(x) min(rollmean(as.numeric(x[c(3:32)]),5))/10 )
tabs<- as.data.frame(tabs)
tabs$ID<- cwd$ID

##add the summarized data back onto the shapefile
sheds@data$maxCWD<- tabs$tabs
##min cwd
cwd <- read.csv("yearlyShedcwd2.csv")
tabs <- apply(cwd,1,function(x) min(rollmean(as.numeric(x[c(3:32)]),5))/10 )
tabs<- as.data.frame(tabs)
tabs$ID<- cwd$ID

##add the summarized data back onto the shapefile
sheds@data$minCWD<- tabs$tabs

##maxPPT
ppt <- read.csv("yearlyShedppt3.csv")
tabs <- apply(ppt,1,function(x) max(rollmean(as.numeric(x[c(3:32)]),5))/10 )
tabs<- as.data.frame(tabs)
tabs$ID<- ppt$ID
##add the summarized data back onto the shapefile
sheds@data$maxPPT<- tabs$tabs

##min PPT
tabs <- apply(ppt,1,function(x) min(rollmean(as.numeric(x[c(3:32)]),5))/10 )
tabs<- as.data.frame(tabs)
tabs$ID<- ppt$ID
##add the summarized data back onto the shapefile
sheds@data$minPPT<- tabs$tabs

##max Temp
tmx <- read.csv("yearlyShedtmx2.csv")
tabs <- apply(tmx,1,function(x) max(rollmean(as.numeric(x[c(3:32)]),5)) )
tabs<- as.data.frame(tabs)
tabs$ID<- tmx$ID
##add the summarized data back onto the shapefile
sheds@data$tmx<- tabs$tabs

##min Temp
##Note that tmn uses a different table than tmx 
tmn <- read.csv("yearlyShedtmn2.csv")
tabs <- apply(tmx,1,function(x) min(rollmean(as.numeric(x[c(3:32)]),5)) )
tabs<- as.data.frame(tabs)
tabs$ID<- tmn$ID
##add the summarized data back onto the shapefile
sheds@data$tmn<- tabs$tabs

##########################
###project every raster onto the same blank template, using a previously created raster 

tmn <- raster('shedTMNabs.tif')
ca_blank<-projectExtent(tmn,crs(tmn))

## create raster each climate variable using the shapefile to produce maps that are summarized by both watershed and the 30 year min/max
setwd('C:/Users/amealor/Desktop/watershedData')
ras <- rasterize(sheds,ca_blank,field= "minCWD")
writeRaster(ras,'shedMinCWD5.tif', overwrite=TRUE, format = "GTiff")

ras <- rasterize(sheds,ca_blank,field= "maxCWD")
writeRaster(ras,'shedMaxCWD5.tif', overwrite=TRUE, format = "GTiff")

ras <- rasterize(sheds,ca_blank,field= "tmn")
writeRaster(ras,'shedTMN5.tif', overwrite=TRUE, format = "GTiff")

ras <- rasterize(sheds,ca_blank,field= "tmx")
writeRaster(ras,'shedTMX5.tif', overwrite=TRUE, format = "GTiff")

ras <- rasterize(sheds,ca_blank,field= "minPPT")
writeRaster(ras,'shedMinPPT5.tif', overwrite=TRUE, format = "GTiff")

ras <- rasterize(sheds,ca_blank,field= "maxPPT")
writeRaster(ras,'shedMaxPPT5.tif', overwrite=TRUE, format = "GTiff")

##Make Soil Maps
#muagatt is a table of soil traits summarazed by map units
muagatt<- read.csv(file="muaggatt2.csv")
#convert values to integers while preserving some precision, in order to speed calculation times
muagatt$aws0100wtaX10<-as.integer(round(muagatt$aws0100wta*100))
muagatt$shrinkswellX10<-as.integer(round(muagatt$shrinkswell*100))

setwd('C:/Users/amealor/Desktop/watershedData')

#soilTif is a very fine scale raster of soil map units (each cell value is the map unit number)
soilTif <- raster("SoilMap2.tif")

#subs switches the raster value with the soil trait value using muagatt as a lookup table
soilExp <- subs(soilTif,muagatt,"mukey","shrinkswellX10",subsWithNA=T)
#Then we need to project the raster so that it matches the climate rasters
soilExp2<- projectRaster(soilExp,ca_blank)
writeRaster(soilExp2, 'soilExp3.tif', overwrite=TRUE,datatype='INT2S')

soilAWS <- subs(soilTif,muagatt,"mukey","aws0100wtaX10",subsWithNA=T)
soilAWS2<- projectRaster(soilAWS,ca_blank)
writeRaster(soilAWS2, 'soilAWS3.tif', overwrite=TRUE,datatype='INT2S')

####################### Get species info
# get just one species
df<-read.csv('calflora complete2.csv')
df$Taxon <- as.factor(df$Taxon)


# get just species in tool
spn <- read.csv("calSpeciesName2.csv",col.names = "names")
df2 <- df[df$Taxon %in% spn$names,]
df3 <- transform(df2, id=match(Taxon, unique(Taxon)))

###remove duplicate obs (same point, same species)
occs.dups2 <- duplicated(df3[c('Longitude', 'Latitude','Taxon')])
occs2 <- df3[!occs.dups2,]
occs2$Latitude <- as.numeric(occs2$Latitude)
occs2$Longitude <- as.numeric(occs2$Longitude)
# give all records a unique ID
occs2$occID <- row.names(occs2)

Maxcwd<- raster('shedMaxCWDabs.tif')
Mincwd <- raster('shedMinCWDabs.tif')
# CWD <- raster('shednormCWD5.tif')
# PPT <- raster('shednormPPT4.tif')
Minppt <- raster('shedMinPPTabs.tif')
MaxPPT<- raster('shedMaxPPTabs.tif')
tmn <- raster('shedTMNabs.tif')
tmx <- raster('shedTMXabs.tif')
exp <- raster('EXP60.tif')
aws<- raster('soilAWS10060.tif')

## set missing values- very important, as obs points with missing values for any feature won't be used
# R will assign NA to any cell with a value below those specified below
# default NA is typically lowest possible number given the datatype, so -1000 is suffucient 
NAvalue(tmn) <- -1000
NAvalue(tmx) <- -1000
NAvalue(Maxcwd) <- -1000
NAvalue(Mincwd) <- -1000
NAvalue(Minppt) <- -1000
NAvalue(MaxPPT) <- -1000
NAvalue(exp)<- -1
NAvalue(aws)<- -1

## all features must have some extent, projection for stack function to work
envs<- stack(Maxcwd,Mincwd,Minppt,MaxPPT,tmn,tmx,exp,aws) ##this works!
# good idea to plot envs in order to check for correct NA attribution, projection
plot(envs)
colnames(spn) <- "names"

##this is where function can start

# remove tool names that aren't real species
spn <- as.data.frame(spn[!(spn$names %in% c('Grasses',"Salix spp.",'Juncus spp.')),])
colnames(spn) <- "names"

##package names must be called out explicitly for par processing, which is several times faster
foreach(q=169:1,.packages = c("dismo","rgeos","ENMeval","rJava","stringr", "raster","rgdal","plyr","sp","adehabitatHR"
))%dopar%{
  ## init java first, if this fails maxent won't work
  .jinit()
  #q loops through species list
  spec <- as.character(spn$names[[q]]) 
  occs<- occs2[occs2$Taxon==spec,]
  ## this checks both for missing species (included in tool, but no observation), and species with not enough obs for maxent to work
  ## 10 is an arbitrary number- 30 is probably better, but almost all species have over 50 obs so it isn't very important
  if(nrow(occs)>10){
    
    occs.xy <- occs[c('Longitude', 'Latitude')]
    sp::coordinates(occs.xy) <- ~ Longitude + Latitude
    
    #covert to same projection as environmental variables
    proj4string(occs.xy) <- CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83
+no_defs +towgs84=0,0,0")
    occs.xy<- spTransform(occs.xy,envs[[1]]@crs)
    
    #background occurences should be taken from all the other species not currently being modeled, as they should have similiar biases
    #process the same as before
    bgoccs <- occs2[occs2$Taxon!=spec,]
    bgoccs <- bgoccs[sample(nrow(bgoccs),15000),]
    bgoccs.xy <- bgoccs[c('Longitude', 'Latitude')]
    sp::coordinates(bgoccs.xy) <- ~ Longitude + Latitude
    proj4string(bgoccs.xy) <- CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83
+no_defs +towgs84=0,0,0")
    bgoccs.xy<- spTransform(bgoccs.xy,envs[[1]]@crs)
    
    # extract environmental values at occ grid cells
    locs.vals <- raster::extract(envs[[1]], occs.xy)
    # remove occs without environmental values to save some time later
    occs <- occs[!is.na(locs.vals), ]  
    locs.vals<- as.data.frame(locs.vals)
    
    # run garbage collection in order to reduce ram
    gc()
    J("java.lang.Runtime")$getRuntime()$gc()
    
    # this is maxent- 'LQP' denotes that the combination of linear, quadratic, and product, functions will be used. 
    #"L,Q,P" would run those three functions separately, producing three models for each species and effectively tripling comp time
    # "H" for Hinge is another commonly used function, but is best used on its own
    #RM values can also be a list, and will run separately (multiply comp time)
    # Block method splits points into four areas for cross validation
    # clamp is not needed for these runs
    e4 <- ENMeval::ENMevaluate(occs.xy, envs, bg.coords = bgoccs.xy, RMvalues = 1, fc = c('LQP'),
                               method = 'block', clamp = FALSE, algorithm = "maxent.jar")
    
    #get parameter values
    lamb<-paste(e4@models[[1]]@lambdas,sep=";",collapse = ";")
    name<-paste(c(spec,"Par",".csv"),sep="",collapse="")
    write.csv(lamb,name)
    
    #e4@results ( AUC, etc)
    name<-paste(c(spec,"Score",".csv"),sep="",collapse="")
    write.csv(e4@results,name)
    
    #importance
    name<-paste(c(spec,"Import",".csv"),sep="",collapse="")
    
    write.csv(var.importance(e4@models[[1]]),name)
  }
}
#Make sure to stop the cluster to free up ram after running
parallel::stopCluster(cl)


###combine data into one dataset
setwd('F:/Watershed Data/Raw Results/')
setwd('F:/Watershed Data/')
path = 'F:/Watershed Data/Raw Results/'
file.names <- list.files(path, pattern ="Score.csv")
p3 <- read.csv(file.names[72],stringsAsFactors=FALSE)

cNames<-cbind(t(colnames(p3)),"names")
cNames
out.file<- data.frame(matrix(ncol = 18, nrow = 0))
colnames(out.file)<- cNames

for(i in 1:length(file.names)){
  file <- read.csv(file.names[i])
  n<-gsub('.{9}$', '', file.names[i])
  #file[[4]]<- as.numeric(file[[4]])
  cval<- cbind(file,n)
  #cval
  #cval<-t(file[4])
  out.file <- rbind(out.file, cval)
  row.names(out.file[i,])<-n
}

setwd('F:/Watershed Data/')
write.csv(out.file,"scoreLQ3.csv")
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


setwd('F:/Watershed Data/Raw Results/')
setwd('F:/Watershed Data/')
path = 'F:/Watershed Data/Raw Results/'
file.names <- list.files(path, pattern ="Import.csv")
p3 <- read.csv(file.names[72],stringsAsFactors=FALSE)
p3$variable
cNames<-rbind(p3[2],"names")
out.file<- data.frame(matrix(ncol = 9, nrow = 0))
colnames(out.file)<- t(cNames)
p3[4]


cNames <-p3[,2][1]

for(i in 1:length(file.names)){
  file <- read.csv(file.names[i])
  n<-gsub('.{10}$', '', file.names[i])
  file[[4]]<- as.numeric(file[[4]])
  cval<-t(rbind(file[4],n))
  #cval<-t(file[4])
  out.file <- rbind(out.file, cval)
  row.names(out.file[i,])<-n
}

write.csv(out.file,"importAll.csv")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

file.names <- list.files(path, pattern ="Par.csv")
for(i in 2:17){
  print(t(p3[i])[c(2:4)])
}
df1<- data.frame(matrix(ncol = 53, nrow = 0))
cnames<- c("AWS60","AWSmin","AWSmax",
           "EXP60","EXPmin","EXPmax",
           "TMN602","TMNmin2","TMNmax2",
           "TMX602","TMXmin2","TMXmax2",
           "maxCWD60","maxCWDmin","maxCWDmax",
           "maxPPT60","maxPPTmin","maxPPTmax",
           "minCWD60","minCWDmin","minCWDmax",
           "minPPT60","minPPTmin","minPPTmax",
           "AWS60^2","AWS60^2min","AWS60^2max",
           "EXP60^2","EXP60^2min","EXP60^2max",
           "TMN602^2","TMN602^2min","TMN602^2max",
           "TMX602^2","TMX602^2min","TMX602^2max",
           "maxCWD60^2","maxCWD60^2min","maxCWD60^2max",
           "maxPPT60^2","maxPPT60^2min","maxPPT60^2max",
           "minCWD60^2","minCWD60^2min","minCWD60^2max",
           "minPPT60^2","minPPT60^2min","minPPT^2max",
           "linearPredictorNormalizer","densityNormalizer","numBackgroundPoints","entropy","Name")

colnames(df)<-cnames
p3 <- read.csv(file.names[32],stringsAsFactors=FALSE)
cnames
for(k in 1:length(file.names)){
  df<- data.frame(matrix(ncol = 53, nrow = 1))
  p<-read.csv(file.names[k],stringsAsFactors=FALSE)
  n<-gsub('.{7}$', '', file.names[k])
  df[1,53]<-n
  for(i in 2:length(p)){
    #print(t(p3[i])[c(2:4)])
    var <- p[1,i]
    if(var %in% cnames[1:48]){
      j<-match(var, cnames)
      
      df[1,j] <- p[2,i]
      df[1,j+1] <- p[3,i]
      df[1,j+2] <- p[4,i]
    }
    if(var %in% cnames[49:52]){
      j<-match(var, cnames)
      
      df[1,j] <- p[2,i]
    }
  }
  df1<- rbind(df1,df)
}

colnames(df1)<-cnames

write.csv(df1,"totalPar602.csv")

df1$Name
p3[1,3]
test<-as.character(as.vector(p3[1:1,]))

test<-t(p3[2])[c(2:4)][[1]]
test

p3[4]


cNames <-p3[,2][1]

for(i in 1:length(file.names)){
  file <- read.csv(file.names[i])
  n<-gsub('.{10}$', '', file.names[i])
  file[[4]]<- as.numeric(file[[4]])
  cval<-t(rbind(file[4],n))
  #cval<-t(file[4])
  out.file <- rbind(out.file, cval)
  row.names(out.file[i,])<-n
}

write.csv(out.file,"importAll.csv")







colnames(out.file)<- t(p3[2])
scoreM<- colMeans(out.file)
scoreSTD<-apply(out.file, 2, sd)
sum(scoreM)
barplot(colMeans(out.file))
out.file[] <- lapply(out.file, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
summary(out.file)
barplot(summary(out.file))

pred <- ENMeval::maxnet.predictRaster(e4@models[[1]], envsBgMsk, type = 'cloglog') 


e3 <- ENMeval::ENMevaluate(occs.xy, envsBgMsk, bg.coords = bg.xy, RMvalues = rms, fc = c('L', 'LQ', 'LQP'), 
                           method = 'user', occs.grp, bg.grp, clamp = FALSE, algorithm = "maxent.jar")
e4@models[[3]]@html
plot(e4@predictions[[1]],type="cloglog")
# unpack the results data frame, the list of models, and the RasterStack of raw predictions
evalTbl <- e4@results
evalMods <- e4@models
names(evalMods) <- e3@results$settings
evalPreds <- e3@predictions
aic.opt <- e2@models[[which(e2@results$delta.AICc==0)]]
e4@models
evalMods
class(aic.opt)
e4@results
write.csv(evalTbl,"evalTblQuercusLobata.csv")
write.csv(evalMods,"evalTbl.csv")

plot(envsBgMsk$EXP60,ylim=c(-100000,100000),xlim=c(-300000,-100000))+points(occs.xy)
response(e4@models[[1]])
# view response curves for environmental variables with non-zero coefficients
plot(evalMods[["LQ_1"]], vars = c('bio01', 'bio02', 'bio08', 'bio09', 'bio13', 'bio14', 'bio15', 'bio17', 'bio18', 'bio03', 'bio05', 'bio06', 'bio07', 'bio12', 'bio16'), type = "cloglog")



# view ENMeval results
ENMeval::eval.plot(evalTbl, value = "avg.test.or10pct")

# Select your model from the models list
mod <- evalMods[[1]]
plot(envsBgMsk$maxCWD60,ylim=c(-100000,100000),xlim=c(-300000,-100000))
envsBgCrop <- raster::crop(envs, bgExt)
envsBgCrop2 <- raster::crop(envs, extent(sheds) )
plot(envsBgCrop2)
###################
# generate cloglog prediction
pred2 <- ENMeval::maxnet.predictRaster(e4@models[[1]], envsBgCrop2, type = 'cloglog', clamp = FALSE) 
plot(pred2)
plot(hist)
name<-paste(c(spec,"hist",".tif"),sep="",collapse="")
writeRaster(pred,name, overwrite=TRUE, format = "GTiff")


#########
#cloglog future
Maxcwd<- raster('shedMaxCWDCNRM.tif')/10
Mincwd <- raster('shedMinCWDCNRM.tif')/10
Minppt <- raster('shedMinPPTCNRM.tif')
MaxPPT<- raster('shedMaxPPTCNRM.tif')
tmn <- raster('shedTMN4CNRM.tif')
tmx <- raster('shedTMXCNRM.tif')
exp <- raster('EXP60.tif')
aws<- raster('AWS60.tif')

exp <- raster::crop(exp, extent(tmn))
aws <- raster::crop(aws,  extent(tmn))
Minppt2<-raster::crop(Minppt2,  extent(tmn))
tmn2<-raster::crop(tmn2,  extent(tmn))
extent(Maxcwd)
extent(Mincwd)
plot(Minppt, zlim=c(0,175))
plot(Minppt2, zlim=c(0,175))

plot(tmn,zlim=c(0,10))
plot(tmn2,zlim=c(0,10))

NAvalue(tmn) <- -1000
NAvalue(tmx) <- -1000
NAvalue(Maxcwd) <- -1000
NAvalue(Mincwd) <- -1000
NAvalue(Minppt) <- -1000
NAvalue(MaxPPT) <- -1000
NAvalue(exp)<- -1
NAvalue(aws)<- -1

envs2<- stack(Maxcwd,Mincwd,Minppt,MaxPPT,tmn,tmx,exp,aws) ##this works!
names(envs2)<- names(envs)
pred3 <- ENMeval::maxnet.predictRaster(e4@models[[1]], envs2, type = 'cloglog', clamp = TRUE) 
plot(pred3)
plot(pred)
name<-paste(c(spec,"CNRM",".tif"),sep="",collapse="")
writeRaster(pred,name, overwrite=TRUE, format = "GTiff")

hist <- raster("Frangula californicahist.tif")
plot(hist)























### Project Niche Model

# You selected to project your model. First define a polygon with the
# coordinates you chose, then crop and mask your predictor rasters.
# Finally, predict suitability values for these new raster cells based on
# the model you selected.
# 

projCoords <- data.frame(x = c(-123.8311, -122.4699, -119.484, -116.9372, -117.7276, -119.8792, -121.8551, -122.9968, -123.8311), y = c(39.8018, 36.3788, 33.314, 33.4974, 36.0953, 38.1276, 40.3063, 40.4068, 39.8018))
projPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(projCoords)), ID=1)))
```

### Project Niche Model to New Time

Now download the future climate variables chosen with *Wallace*, crop
and mask them by projPoly, and use the maxnet.predictRaster() function
to predict the values for the new time based on the model selected.

```{r}
envsFuture <- raster::getData("CMIP5", var = "bio", res = 2.5, rcp = 60, model = "MC", year = 50)

predsProj <- raster::crop(envsFuture, projPoly)
predsProj <- raster::mask(predsProj, projPoly)

# rename future climate variable names
names(predsProj) <- paste0('bio', sprintf("%02d", 1:19))
# select climate variables
predsProj <- raster::subset(predsProj, names(envs))
```

```{r}
# predict model
proj <- ENMeval::maxnet.predictRaster(mod, predsProj, type = 'cloglog', clamp = FALSE)
```

```{r}
# plot the model prediction
plot(proj)












library(stringr)
library(ggplot2)
library(caTools)
library(ncdf4)
library(raster)
library(rgdal)
library(plyr)
library(sp)
library(readxl)
library(reshape2)
library(ggmap)

setwd('C:/Users/amealor/Desktop/watershedData/cbm_hst')
###############
#extent around spow points in CA Teale albers matching rasters
#ext<-extent(c(-262430.084732,-209559.752951,-31799.780822,34050.692296))

############### download climate data

error.list<-data.frame(cbind("file","error"))##3780 rows
names(error.list)<-c("file","error")
years<-as.data.frame(1980:2009) #years available on usgs server
variable<-c('tmn_',"tmx_","ppt_","cwd_")

for(mm in 1:4){ ###Based on which variable you want !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  
  #base url
  mods<-paste("http://cida.usgs.gov/thredds/fileServer/CA-BCM-2014/HST/Monthly/CA_BCM_HST_Monthly",variable[mm],sep="_")
  
  #get nc file web url
  url_grid <-paste(mods,yr,".nc",sep="")
  ff<-paste(variable[mm],yr,".nc",sep="")
  
  
  w<-"error"; class(w)<-"try-error"
  w<-1
  options(timeout=600)
  while(inherits(w,"try-error") | w != 0){
    w<-try(download.file(url_grid, ff, method = "auto", quiet = TRUE, mode = "wb"),silent=TRUE)
    
  }
  
  
}



##########
########## work with species location info
# reference projection
cr<-raster("V:/Project/climate_change/models/climate_ready/crsa270.tif")
pr<-projection(cr)
pr <- crs(cr)
cr@crs@projargs
pr
#all species observations

#df <- read_excel("C:\\Users\\amealor\\Desktop\\watershedData\\calflora completexl.xlsx")
df <- read_excel("C:\\Users\\amealor\\Desktop\\watershedData\\calflora complete2.xlsx")
df$Taxon <- as.factor(df$Taxon)

# get just species in tool
#spn <- read_excel("C:\\Users\\amealor\\Desktop\\watershedData\\toolSpeciesNames.xlsx",col_names = F)
spn <- read_excel("C:\\Users\\amealor\\Desktop\\watershedData\\calSpeciesName2.xlsx",col_names = F)
spn$names <- as.factor(spn$X__1)

df2 <- df[df$Taxon %in% spn$names,]
df3 <- transform(df2, id=match(Taxon, unique(Taxon)))
df4 <- data.frame(df3$Latitude, df3$Longitude, df3$id)
names(df4) <- c("lat", "long", "id")
df5 <-data.frame(df3$Latitude, df3$Longitude, df3$id,df3$Taxon)
names(df5) <- c("lat", "long", "id","Taxon")

# df4 is list of observations of just species of interest, with just lat/long and id number

setwd('C:/Users/amealor/Desktop/watershedData')

coordinates(df4)<- ~long+lat
proj4string(df4) <- CRS("+proj=longlat +datum=WGS84")  ## for example
df5

res <- spTransform(df5, CRS(pr))

#projection(df4)=pr
shapefile(df5,'calToolSpeciesNames.shp',overwrite=T)
## load observation list into a spatial vector object
shape3 <- readOGR(dsn= getwd(),layer= "calToolSpecies3")
shape2 <- readOGR(dsn= getwd(),layer= "calToolSpecies2")
shape <-  readOGR(dsn= getwd(),layer= "calToolSpecies")
coordinates(shape)[,c(1,2)]
str(shape)
extent(shape3)

setwd('C:/Users/amealor/Desktop/watershedData/cbm_hst')
years <- c(1980:2009)

length(shape)
###CWD
cwd <- data.frame(matrix(nrow=length(shape3)))

#ff <- "cwd_1980.nc")
for(i in 1:30){
  start_time <- Sys.time()
  ff<- paste("cwd_",years[i],".nc",sep="")
  temp.stack<-stack(ff)
  projection(temp.stack)<-pr
  ##stack creates a raster stack
  value2 <- extract(temp.stack,coordinates(shape3)[,c(1,2)])
  yearly <- rowSums(value2)
  cwd <- cbind(cwd,yearly)
  end_time <- Sys.time()
  print(end_time - start_time)
  
}

setwd('C:/Users/amealor/Desktop/watershedData')
write.csv(cwd, file ="yearlycwd2.csv", row.names=F)

###ppt
setwd('C:/Users/amealor/Desktop/watershedData/cbm_hst')
ppt <- data.frame(matrix(nrow=length(shape3)))

#ff <- "cwd_1980.nc")
for(i in 1:30){
  start_time <- Sys.time()
  ff<- paste("ppt_",years[i],".nc",sep="")
  temp.stack<-stack(ff)
  projection(temp.stack)<-pr
  
  value2 <- extract(temp.stack,coordinates(shape3)[,c(1,2)])
  yearly <- rowSums(value2)
  ppt <- cbind(ppt,yearly)
  end_time <- Sys.time()
  print(end_time - start_time)
  
}
setwd('C:/Users/amealor/Desktop/watershedData')
write.csv(ppt, file ="yearlyppt2.csv", row.names=F)

###tmn
setwd('C:/Users/amealor/Desktop/watershedData/cbm_hst')
tmn <- data.frame(matrix(nrow=length(shape3)))

#ff <- "cwd_1980.nc")
for(i in 1:30){
  start_time <- Sys.time()
  ff<- paste("tmn_",years[i],".nc",sep="")
  temp.stack<-stack(ff)
  projection(temp.stack)<-pr
  
  value2 <- extract(temp.stack,coordinates(shape3)[,c(1,2)])
  yearly <- apply(value2,1,min)
  tmn <- cbind(tmn,yearly)
  end_time <- Sys.time()
  print(end_time - start_time)
  
}
setwd('C:/Users/amealor/Desktop/watershedData')
write.csv(tmn, file ="yearlytmn2.csv", row.names=F)
###tmx
setwd('C:/Users/amealor/Desktop/watershedData/cbm_hst')
tmx <- data.frame(matrix(nrow=length(shape3)))

#ff <- "cwd_1980.nc")
for(i in 1:30){
  start_time <- Sys.time()
  ff<- paste("tmx_",years[i],".nc",sep="")
  temp.stack<-stack(ff)
  projection(temp.stack)<-pr
  
  value2 <- extract(temp.stack,coordinates(shape3)[,c(1,2)])
  yearly <- apply(value2,1,max)
  tmx <- cbind(tmx,yearly)
  end_time <- Sys.time()
  print(end_time - start_time)
  
}
setwd('C:/Users/amealor/Desktop/watershedData')
write.csv(tmx, file ="yearlytmx2.csv", row.names=F)
warnings()
library(zoo) #for rolling averages
setwd('C:/Users/amealor/Desktop/watershedData')
cwd[,1]<- coordinates(shape)[,c(3)]
colnames(cwd)<- c("id",years)

cwd <- read.csv(file ="yearlycwd2.csv")
ppt <- read.csv(file ="yearlyppt2.csv")
tmx <- read.csv(file ="yearlytmx2.csv")
tmn <- read.csv(file ="yearlytmn2.csv")


cwd$species <- df3$Taxon
ppt$species <- df3$Taxon
tmx$species <- df3$Taxon
tmn$species <- df3$Taxon

tmn2 <- tmn[,2:32]
tmn2 <- melt(tmn2, id="species")
tabs <- ddply(.data = tmn2, .variables= .(species), .fun = summarise,      
              
              Tmin2 = quantile(value,.05, na.rm = T),
              Tmin1 = quantile(value,.25, na.rm = T),
              observations= length(species)/30
              
)

tmx2 <- tmx[,2:32]
tmx2 <- melt(tmx2, id="species")
tabs2 <- ddply(.data = tmx2, .variables= .(species), .fun = summarise,      
               
               Tmax1 = quantile(value,.75, na.rm = T),
               Tmax2 = quantile(value,.95, na.rm = T),
               observations= length(species)/30
               
)



cwd2 <- cwd[,2:32]
cwd2 <- melt(cwd2, id="species")
tabs3 <- ddply(.data = cwd2, .variables= .(species), .fun = summarise,      
               
               CWDmin2 = quantile(value,.05, na.rm = T),
               CWDmin1 = quantile(value,.25, na.rm = T),
               CWDmax1 = quantile(value,.75, na.rm = T),
               CWDmax2 = quantile(value,.95, na.rm = T),
               observations= length(species)/30
               
)


ppt2 <- ppt[,2:32]
ppt2 <- melt(ppt2, id="species")
tabs4 <- ddply(.data = ppt2, .variables= .(species), .fun = summarise,      
               PPTmin2 = quantile(value,.05, na.rm = T)/10,
               PPTmin1 = quantile(value,.25, na.rm = T)/10,
               PPTmax1 = quantile(value,.75, na.rm = T)/10,
               PPTmax2 = quantile(value,.95, na.rm = T)/10,
               observations= length(species)/30
               
)

tabs5 <- join_all(list(tabs,tabs2,tabs3,tabs4), type = 'full')
str(tabs)

write.csv(tabs5, file ="plant_climate_8_22_18.csv", row.names=F)
write.csv(cwd, file ="cwddata.csv", row.names=F)
write.csv(tmn, file ="tmndata.csv", row.names=F)
write.csv(tmx, file ="tmxdata.csv", row.names=F)
write.csv(ppt, file ="pptdata.csv", row.names=F)



###making sure data looks okay

###ppt
setwd('C:/Users/amealor/Desktop/watershedData/cbm_hst')
ppt <- data.frame(matrix(nrow=length(shape3)))

#ff <- "cwd_1980.nc")
i <- 5

ff<- paste("ppt_",years[i],".nc",sep="")
temp.stack<-stack(ff)
projection(temp.stack)<-pr

value2 <- extract(temp.stack,coordinates(shape3)[,c(1,2)])
yearly <- rowSums(value2)
ppt <- cbind(ppt,yearly)
end_time <- Sys.time()
print(end_time - start_time)




temp.stack[,,1]
as.data.frame(temp.stack$X1984.01.01)
pptPoints<- as(temp.stack$X1984.01.01,"SpatialPixelsDataFrame")
test_df <- as.data.frame(pptPoints)
colnames(test_df) <- c("value", "x", "y")

coordinates(temp.stack$X1984.01.01)
shape3
#sf_df <- fortify(shape3)
map <- ggplot()+geom_tile(data=test_df, aes(x=x, y=y, fill=value), alpha=0.8)+geom_point(aes(x=coords.x1,y=coords.x2),data=as.data.frame(coordinates(shape3)))
map

