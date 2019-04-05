####################################################
#########   Ex No 1 Handling The Vector Data    ######
#####################################################


library(rgdal)

#Read the shapefile
data1<-readOGR(dsn="DATA/EX_1",layer="Tourist_Kerala")

#### Write the shapefile
writeOGR(data1,"OUTPUT","TouristDensity2",driver="ESRI Shapefile")

#Getting the summary of the shapefile
summary(data1)

#Getting the attribute information of the shapefile
attribute<-data1@data
attribute
###OR###
a<-data.frame(data1)
a

#Getting the initial attribute details of the shapefile
head(attribute)


#Getting the details of first two rows of attribute table
head(attribute,2)


#Getting the final attribute details of the shapefile
tail(attribute)


length(attribute)


names(attribute)


#Getting the second column details of the shapefile
attribute[2]


#Getting the sixth attribute details of the shapefile
attribute[10, ]


#####################################################################
######### Ex No 2 Displaying the shapefile data in R #################
######################################################################

# loading the required Packages

library(rgdal)

#Read the shapefile
data2<-readOGR(dsn="DATA/EX_1_4",layer="cropland")

summary(data2)

data.frame(data2)

#Displaying the shapefile
plot(data2)

#Displaying the by giving single color
plot(data2,col="Green")

#Giving Title
plot(data2,col="Green",main="Crop Land Map")

##PLOTING THE SHAPEFILE AFTER CLASSIFYING BASED ON ITS ATTRIBUTES##
data1_col<-c(palette())[data2$Type]

data1_col

plot(data2,col=data1_col)

##PROVIDING TITLE TO THE PLOT##
plot(data2,
     col=data1_col,
     main="Crop Land Map")




#########################################################
###############EX No 3  QUERYING ( ATTRIBUTE QUERY)  ##########
#######################################################

library(rgdal)
land<-readOGR(dsn="DATA/EX_1_5",layer="Cropland")
plot(land)
summary(land)
land_attribute<-data.frame(land)
land_attribute


####QUERYING THE ATTRIBUTES####
paddy<-land[land$Type=="Paddy",]
paddy
summary(paddy)
#########plotting the result##########
plot(paddy,col='red')


####################MULTIPLE QUERYING##############

MQ<-land[land$Type=="Paddy"&land$area>5000,]
MQ
plot(MQ,col='cyan')
plot(land,col='yellow')
plot(MQ,col='Cyan',add=TRUE)


##############################################################
#######   EX No 4 HANDLING THE PROJECTION OF THE SHAPEFILE #######
###############################################################

library(rgdal)
land<-readOGR(dsn="DATA/EX_2",layer="ward")
###CHECKING THE PROJECTION####
summary(land)
##Removing The Existing Projection####
proj4string(land)<-NA_character_
summary(land)
#####Providing a new projection#####
proj4string(land)<-CRS("+init=epsg:4326")
summary(land)
#####Changing the projection#####
landprojected<-spTransform(land,CRS("+init=epsg:32643"))
summary(landprojected)



##########################################################
###########  Ex No 5 Handling the csv files################
###########################################################

library(rgdal)
######READING CSV FILE
data<-read.csv(file="Data/EX_3/Hospitals.csv",header=TRUE,sep=',')
data
##READING CSV FILE AND MAKING AS A SHAPEFILE##
data<-read.csv(file="Data/EX_3/Hospitals.csv",header=TRUE,sep=',')
data
coordinates(data)<-cbind("Longitude","Latitude")
proj4string(data)<-CRS("+init=epsg:4326")
plot(data,col="red")
writeOGR(data,"Output","HospitalsLoc",driver="ESRI Shapefile")


###Merging an external csv file with the shapefile ###
D1<-readOGR(dsn="Data/EX_3",layer="district")
D2<-read.csv(file="Data/EX_3/Cropdetails.csv",header=TRUE,sep=',')
D1@data
D2
Merge<-merge(D1,D2,by.x='Name',by.y='District')
Merge@data
plot(Merge)


######################################################
###  EX No 6  CALCULATING THE AREA OF THE SHAPEFILE###
######################################################
library(rgdal)
library(rgeos)
D1<-readOGR(dsn="RTRIAL",layer="Kerala")
plot(D1)
D1@data
summary(D1)
D1projected<-spTransform(D1,CRS("+init=epsg:32643"))
summary(D1projected)
D1projected$Area1<-gArea(D1projected,byid=TRUE)
D1projected@data



######################################################
#########    Ex No 7 BUFFER ANALYSIS   ###############
######################################################

library(rgdal)
library(rgeos)
roads<-readOGR(dsn="Data/EX_5",layer="Roads1")
plot(roads)
summary(roads)
roadsreproj<-spTransform(roads,CRS("+init=epsg:4326"))
roadsbuffer<-gBuffer(roadsreproj,width=100,byid=TRUE)
plot(roadsbuffer,col="cyan")
plot(roadsreproj,add=TRUE)
writeOGR(roadsbuffer,"bufferoutput","roadsbuffered2",driver="ESRI Shapefile")



##########################################################################
#########   EX No 8 Select By Location(Spatial Query)   ##################
###########################################################################


###Load the required libraries
library(rgdal)
library(rgeos)
Waterbody<-readOGR(dsn="Data/EX_6",layer="watertank")
Cadastry<-readOGR(dsn="Data/EX_6",layer="surveyplots")
plot(Cadastry,col='Brown')
plot(Waterbody,col='Blue',add=TRUE)
summary(Cadastry)
Cadastryreproj<-spTransform(Cadastry,CRS("+init=epsg:32643"))
summary(Waterbody)
Waterbodyreproj<-spTransform(Waterbody,CRS("+init=epsg:32643"))
summary(Waterbodyreproj)
waterbodybuffer<-gBuffer(Waterbodyreproj,width=100)
cadastryintersects=gIntersects(waterbodybuffer,Cadastryreproj,byid=TRUE)
summary(cadastryintersects)
cadastryintersects
cadastryselection <- Cadastryreproj[as.vector(cadastryintersects), ]
summary(cadastryselection)
cadastryselection@data
plot(cadastryselection,col="cyan")
plot(Cadastry)
plot(cadastryselection,col='green',add=TRUE)
plot(Waterbody,col="red",lwd=2,add=TRUE)




###################################################
############   EX No : 9 Spatial Join    #########
###################################################

library(rgdal)
Trv<-readOGR(dsn="DATA/EX_7",layer="Trivandrum")
plot(Trv)
Assets<-readOGR(dsn="DATA/EX_7",layer="Assets")
data.frame(Assets)
summary(Assets)
plot(Assets,add=TRUE)
Corp<-readOGR(dsn="DATA/EX_7",layer="Corp")
plot(Corp,add=TRUE,col="yellow")
spatialjoin<-aggregate(Assets,by=Corp,FUN=length)
names(spatialjoin)
class(spatialjoin)
spplot(spatialjoin)
data.frame(spatialjoin)



###########################################################
#############  EX No :10 Geoprocessing Analysis   #########
############################################################

library(rgdal)
library(rgeos)
polygon1<-readOGR(dsn="Data/EX_8",layer='Field')
polygon1
polygon1@data
polygon2<-readOGR(dsn="Data/EX_8",layer='Soil')
polygon2@data
plot(polygon1,col='RED')
plot(polygon2,add=TRUE)
union=gUnion(polygon1,polygon2,byid=TRUE)
summary(union)
plot(union)
intersection1=gIntersection(polygon2,polygon1,byid=TRUE)
intersection1
plot(intersection1,col='Yellow')
plot(polygon1,col='RED')
plot(polygon2,add=TRUE)
plot(intersection1,col='Yellow',add=TRUE)
SYMDIFF<-gSymdifference(polygon1,polygon2,byid=FALSE)
SYMDIFF
plot(SYMDIFF,col='Green')
DIFF1<-gDifference(polygon1,polygon2,byid=FALSE)
DIFF1
plot(DIFF1,col='PINK')
DIFF2<-gDifference(polygon2,polygon1,byid=FALSE)
DIFF2
plot(DIFF2,col='BROWN')



###############################################################################
############Geoprocessing Analysis using Raster Library########################

library(rgdal)
library(raster)

place<-readOGR(dsn="Data/EX_8",layer="Trivandrum")
plot(place)
P1<-readOGR(dsn="Data/EX_8",layer="Corp")
plot(P1,add=TRUE)
SYMDIFF1<-symdif(place,P1)
plot(SYMDIFF1,col="Blue")
SYMDIFF1@data
u=union(place,P1)
plot(u)
i=intersect(place,P1)
plot(i)
d1=erase(place,P1)
plot(d1,col='red')
d2=erase(P1,place)
d2
plot(d2)



####################################################################
##############    EX No 11: WEBMAP OVERLAY   ########################
####################################################################

library(rgdal)

library(ggplot2)

library(ggmap)


#Read the shapefiles
place<-readOGR(dsn="DATA/EX_11",layer="Trivandrum")

plot(place)

P1<-readOGR(dsn="DATA/EX_11",layer="Corp")

plot(P1,add=TRUE)


BB<-bbox(place)

BBmap2<-get_map(location=BB,maptype="terrain",source="stamen",zoom=10)

ggmap(BBmap2)+geom_polygon(data=P1,aes(x=long,y=lat),fill="yellow",color="red",alpha=0.2)


############################################################################
####################        FOR LINES     ####################################
############################################################################


roads<-readOGR(dsn="DATA/EX_11",layer="Roads1")

plot(roads)

plot(place)
plot(roads,add=TRUE)


BBmap4<-get_map(location=BB,maptype="terrain",source="stamen",zoom=10)

ggmap(BBmap4)+geom_path(data=roads,aes(x=long,y=lat,group=group),color="red")



##########################################################################
###################      FOR POINTS   ####################################
###########################################################################


##############################################
#####Displaying from the csv data##############

hosp<-read.csv(file="DATA/EX_11/Hospitals.csv",header=TRUE,sep=',')

hosp

summary(hosp)

wsbbox<-bbox(place)

BBmap1<-get_map(location=wsbbox,source="stamen",maptype="terrain",zoom=10)

ggmap(BBmap1)+geom_point(data=hosp,aes(x=Longitude,y=Latitude),color='red',size=1)

######################################################################################################


############################################################
########## Displaying from the point shapefile laye #########

#Read the shapefile

Assets<-readOGR(dsn="DATA/EX_11",layer="Assets")

Adata<-data.frame(Assets)

Adata

summary(Assets)

##Convert it to dataframe
df<-as.data.frame(Assets)

df

ggmap(BBmap1)+geom_point(data=df,aes(x=coords.x1,y=coords.x2),color='blue',size=1)





##############################################################################
#############  Ex No : 12 Handling Raster Data in R    ########################
##############################################################################


library(raster)

#Reading raster data
dem<-raster("DATA/EX_10/DEM.tif")

#Gives the summary
dem

#To get the coordinates of the raster data
crs(dem)

#To remove existing crs
crs(dem)<-NA_character_
crs(dem)

#To add new crs
crs(dem)<-"+init=epsg:4326"
crs(dem)

#To transform the projection
demreproj<-projectRaster(dem,crs=crs("+init=epsg:32643"))
demreproj

crs(demreproj)

#Plot the data
plot(dem)


########Clip the raster data based on the available shapefile###########
library(rgdal)
place<-readOGR(dsn="DATA/EX_10",layer="Trivandrum")
plot(place)
cropped<-mask(dem,place)
plot(cropped)

#################Extract the pixel from the raster data based on the given shapefile
assets<-readOGR(dsn="DATA/EX_10",layer="HospitalsLoc")
plot(assets,col="red",pch=19,cex=0.5,add=TRUE)
extractpixel<-extract(dem,assets)
extractpixel
summary(extractpixel)
head(extractpixel)



###############################################################
########  Ex No : 13  Handling DEM and its Analysis#############
###############################################################

library(raster)
library(rgl)
library(rasterVis)
library(rgdal)

####Read the DEM data########
dem<-raster("DEM.tif")

dem

#####Plot the data###########
plot(dem)

#####For getting the  coordinate reference system
crs(dem)

######Plot the data three dimensionally########
plot3D(dem)

##########Find the Slope############
slope<-terrain(dem,opt="slope")
plot(slope)

#########Find the Aspect###########
aspect<-terrain(dem,opt="aspect")
plot(aspect)

######Hillshade Analysis#########
hillshade<-hillShade(slope,aspect)
plot(hillshade,legend = FALSE, main = "Hillshade")
plot(dem,col=rainbow(25,alpha=0.35),add=TRUE)

#######Saving the result as seperate raster data######
writeRaster(slope,"Sloperesult",format="GTiff")

########Finding the contour lines from the given raster data######
contourlines<-rasterToContour(dem,maxpixels = 100000)
plot(contourlines)

