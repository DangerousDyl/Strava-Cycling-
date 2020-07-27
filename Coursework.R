install.packages(c("raster", "rgdal", "ape", "scales", "deldir", "gstat"))
install.packages("tidyverse")
install.packages("psych")
install.packages(c("sf", "remotes", "stplanr", "tmap", "osmdata", "kableExtra", "shinyjs"))
install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")

library(raster) # rasters
library(rgdal)  # spatial data
library(ape)    # Clustering (Moran's I)
library(scales) # transparencies
library(deldir) # triangulation
library(gstat)  # geostatistics
library(tidyverse)
library(psych)
library(sf)
library(remotes)
library(stplanr)
library(tmap)
library(osmdata)
library(kableExtra)
library(shinyjs)
library(spDataLarge)

f     <- "D:/Transport Project/Tyne Wear 2015 7zOC8218A62/Analysis"

# Names of files
##Edges 
edges <-"tyneandwear.gpkg"
edges1 <- read.csv("tynewear_20150101_20151231_ride_edge_alignment_hourly.csv")
edges2 <- readOGR(dsn=path.expand("D:/Transport Project/Tyne Wear 2015 7zOC8218A62/Analysis"), layer="tyneandwear")
plot(edges2)
names(edges1)[1] <- "ID"
edge <- merge(edges1, edges2, by='ID')



########################Data Analysis 

## Historgrams 

ggplot(edge) +
  geom_histogram(mapping = aes(x = edge$hour), binwidth = 0.5) ## Heavy weigting towards peak hours. Non-existent at night 

ggplot(edge) +
  geom_histogram(mapping = aes(x = edge$day), binwidth = 0.5) ## Summer heavy Distribution 

ggplot(edge) +
  geom_histogram(mapping = aes(x = edge$minute), binwidth = 0.5) ## All zero 

ggplot(edge) +
  geom_histogram(mapping = aes(x = edge$commute_count), binwidth = 0.5) ## Mostly zeroe - Everything less than 25 

ggplot(edge) +
  geom_histogram(mapping = aes(x = edge$total_athlete_count), binwidth = 0.5) ## Mostly zeroe - Everything less than 25 

###
library(psych)
describe(edge)






##Yearly Data 
install.packages('dplyr')

edgeyear <- edge[c(1,6:31)]
edgeyear1 <- edgeyear[c(1:10)]
aggregate(cbind(1, 2)


library(dplyr)

edgeyear2 <- edgeyear1 %>%
  group_by(ID) %>%
    summarise_each(funs(sum))

describe(edgeyear2)

describeBy(edgeyear2, group = edgeyear2$commute_count)

##ScatterPlot 

plot(commute_count ~ ID, data = edgeyear2)

## Barplot 

ggplot(data=edgeyear,aes(x=reorder(edgeyear$ID,c),y=edge$commute_count)) + 
  geom_bar(stat ='identity',aes(fill=edge$commute_count))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="total_activity_count by street")+
  labs(title = 'Ranking of streets by Activity coutn',
       y='total_activity_count',x='Street')+ 
  geom_hline(yintercept = mean(edge$commute_count),size = 1, color = 'blue')

#Handling vector
head(edge)       #attribute table
head(edge$ID) #attribute values
head(pts$solar)      #attribute values shorthand
names(pts)           #names of attribute table columns
edge$ID@bbox             #bounding box (extent) as matrix
extent(edge)          #extent as 'Extent class'
coordnames(edge)      #names of coordinate columns
proj4string(pts)     #projection as string
crs(pts)             #projection as CRS object

summary(edge)
spts$solarmonthly<-pts$solar/12
summary(pts$solarmonthly)



##Nodes 
nodes <- "tyneandwear_nodes.gpkg"
nodes1 <- read.csv("tynewear_20150101_20151231_ride_node_alignment_hourly.csv")
nodes2 <- readOGR(dsn=path.expand("D:/Transport Project/Tyne Wear 2015 7zOC8218A62/Analysis"), layer="tyneandwear_nodes")
plot(nodes2)


cyclelanes <- ""

#OD 
OD <- "tyneandwear_edges_od_polygons_polygons.gpkg"
OD1 <- read.csv("tynewear_20150101_20151231_ride_od_rollup_total.csv")
OD2 <- readOGR(dsn=path.expand("D:/Transport Project/Tyne Wear 2015 7zOC8218A62/Analysis"), layer="tyneandwear_edges_od_polygons_polygons")
plot(OD2)



#Join Tables: 

tmap_mode("plot")
tmap_style("gray")
tm_basemap("OpenStreetMap.Mapnik") +
  tm_shape(OD2) + 
  tm_layout(title = "Bristol and its TTWA zones", title.size = 0.8) +
  tm_fill(col = "red", alpha = 0.5) + 
  tm_borders(col = "black", lwd = 1) 



############################################################
###   Load datasets
############################################################

#Load points
# !!! IF USING OWN DATA !!! make sure to rename correct columns to "alt" and "solar"
pts   <- readOGR(file.path(f,pname))
names(pts) <- c("id","solar" ,"alt", "aspect") 
coordnames(pts) <-c("x", "y")                     #rename coordinates from x1, x2 to x, y

#Load country outline
outl  <- readOGR(file.path(f,cname))

#Load rasters
s  <- raster(file.path(f,sname)) #solar potential
a  <- raster(file.path(f,aname)) #altitude

# clip raster to extent of points
sc <- crop(s, pts)               #solar potential cropped
ac <- crop(a, pts)               #altitude cropped


############################################################
###   Explore datasets
############################################################
