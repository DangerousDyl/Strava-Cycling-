
#Bristol Bicycle Tutorial: 
plot(bristol_region, main="Bristol")
plot(st_geometry(bristol_ttwa), main="Bristol and its TTWA zones") #just the contour
plot(st_geometry(bristol_region), add=TRUE) #Adding both regions 

bristol_zones1 <- bristol_zones

tmap_mode("view")
tmap_style("gray")
tm_basemap("OpenStreetMap.Mapnik") +
  tm_shape(bristol_zones1) + 
  tm_layout(title = "Bristol and its TTWA zones", title.size = 0.8) +
  tm_fill(col = "red", alpha = 0.5) + 
  tm_borders(col = "black", lwd = 1) + 
  tm_shape(bristol_ttwa) + 
  tm_scale_bar() + 
  tm_borders(col = "black", lwd = 2)


?tm_basemap

#explore the different Bristol Zones 
names(bristol_zones)
bristol_zones$geo_code[1:5]
bristol_zones$name [1:5]
bristol_zones$geometry
nrow(bristol_zones)
nrow(bristol_od)
View(bristol_od)

#Aggregate the OD dataset
zones_attr = bristol_od %>% 
  group_by(o) %>% 
  summarize_if(is.numeric, sum) %>%
  dplyr:: rename(geo_code = o)

summary(zones_attr)

#Check that the IDs match those in the zones dataset 
summary(zones_attr$geo_code %in% bristol_zones$geo_code)

#Now we join zones_attrr with bristol_zones in a new dataset called zones_joines: 
#Join datasets 
zones_joined = left_join(bristol_zones, zones_attr, by = "geo_code")
sum(zones_joined$all)
names(zones_joined)
View(zones_joined)
plot(zones_joined)


##Aggregating OD datasets to the zone of origin, they can alsob be aggregated by destination zones. Zones_od contains a new column 
#that reports the number of trip destinations by mode

zones_od = bristol_od %>% 
  group_by(d) %>% 
  summarize_if(is.numeric, sum) %>%
  dplyr::select(geo_code = d, all_dest = all) %>% 
  inner_join(zones_joined, ., by = "geo_code")

View(zones_od)
summary(zones_od)

#With the following code lines it is possible to create a map with the number of trips per origin and destination for the different Bristol Zones

tmap_mode("plot")

#tmap mode set to plotting 


tm_shape(zones_od) + tm_fill(c("all", "all_dest"),
                             palette = viridis::plasma(4),
                             breaks = c(0, 2000, 4000, 10000, 50000),
                             title = "Number of Trips" ) +
  tm_scale_bar() + 
  tm_compass(position = c("LEFT", "TOP")) + 
  tm_borders(col = "black", lwd = 1.0) + 
  tm_facets(free.scales = FALSE, ncol = 2) +
  tm_layout(panel.labels = c("Zone of Origin", "Zone of Destination"))



plot(bristol_zones)
View(zones_attr)

##Trips join-up the different levle3s of transport systems: reperesent desire lines connecting different zones centroids (nodes) while 
##observing geospatila limitations (e.g. you cannot go through a house when driving a car), and they can be allocated onto the route networks
#as routes. Trips unlike zones, which represent trip OD, connect the centroid of the OD zone. 
#On an ideal case, trips represent the quickest 'beeline' or 'crow flies' route, considering obstacles, between two nodes.

View(bristol_od)

od_top5 = bristol_od %>% 
  arrange(desc(all)) %>% 
  top_n(5, wt = all)

#Table 

knitr::kable(od_top5) %>% 
  kable_styling(bootstrap_options = c("stripped", "hover"))


## Two main types of od pair: interzonal and intrazonal. Interzonal represents travel between zones in which the destination is different from
#from the origin while intrazonal represents travel within the same zone. 


#Defining active modes (i.e. walking and cycling)
od_intra = filter(bristol_od, o == d)
od_inter = filter(bristol_od, o != d)

#Convert od_inter into a polygon 
desire_lines = od2line(od_inter, zones_od)
plot(desire_lines)


#From a policy perspective, the raw data presented in Table 1 is of limited use to detect where policy measures are needed. As well, 
#it does not give the proportion of trips made by walking and cycling. To know that, please write the following code lines
#to calculate the % of each desired line that is made by walking and cycling (i.e. active models)


View(bristol_od)
View(desire_lines)
#Defining active modes (i.e. walking and cycling)
bristol_od$Active = (bristol_od$bicycle = bristol_od$foot) /
  bristol_od$all * 100 

desire_lines$Active = (desire_lines$bicycle = desire_lines$foot) /
  desire_lines$all * 100 

#Plotting top five desire line start and end points. 
tmap_mode("plot")

plot(desire_lines_top5)


desire_lines_top5 = od2line(od_top5, zones_od)
tm_basemap("OpenStreetMap.Mapnik") + 
  tm_shape(desire_lines) + 
  tm_lines(palette = "plasma", breaks = c(0, 5, 10, 20, 40, 100),
           lwd = "all",
           scale = 9, 
           title.lwd = "Number of trips",
           alpha = 0.6,
           col = "Active",
           title.col = "Active",
           title = "Active travel (%)") +
  tm_shape(desire_lines_top5) +
  tm_lines(lwd = 5, col = "black", alpha = 0.7) +
  tm_scale_bar()



st_is_valid(desire_lines_top5)

##Routes 
#Routes are desired lines that are no longer straight. Desire lines contain only two vertices but routes can contain hundreds of vertices. 
#Routes are generated from od pairs using routing services 

#First the benefits of cycling trips are greatest when they replace car trips - in this case 5km Euclidean distance. 
#Second the focus will be for od pairs that have high (300+) number of car trips. 

#Routing is done by the stplanr function line2route() which converts straight lines in Spaital or sf objects and returns 'bendy lines that 
#represent closer routs on the transport network. 

#Car routes
desire_lines$distance = as.numeric(st_length(desire_lines))
desire_carshort = dplyr:: filter(desire_lines, car_driver > 300 & distance < 5000) 
desire_carshort$geom_car = st_geometry(route_carshort)

#Routes along which many (300+) short (<5km Euclidean distance) car journeys are made (red) overlaying desire lines (i.e. Euclidean distance) 
#representing the same trips (blue)
plot(st_geometry(desire_carshort), col = "blue", main="Car Route")
plot(desire_carshort$geom_car, col = "red", add = TRUE)


# Putting the routes inside the TTWA 
plot(st_geometry(bristol_ttwa), main="Car Route with TTWA zone")
plot(st_geometry(desire_carshort), col = "blue", add = TRUE)
plot(desire_carshort$geom_car, col = "red", add = TRUE)

#With Open Maps
#Euclidean Distance 
tmap_mode("view")

tm_basemap("OpenStreetMap.Mapnik") + 
  tm_shape(desire_carshort$geometry) + 
  tm_lines(col = "blue", lwd = 2) + 
  tm_layout(title = "Euclidean and real route car route", title.size = 0.8) +
  tm_shape(desire_carshort$geom_car) +
  tm_lines(col = "red", lwd = 2) + 
  tm_shape(bristol_ttwa) +
  tm_borders(col = "purple", lwd = 3)


#Nodes 
#1) Create an origin, destination and rail station matrix
#2) Identify nearest stations for the origens and destinations
#3) Convert a single desire line to multiline 

#Top three train desire lines 
desire_rail = top_n(desire_lines, n = 3, wt = train)
desire_rail = line_via(desire_rail, bristol_stations)
zone_cents_rail = st_centroid(zones_od)

View(desire_rail)

#In the majority of cases, the destination leg is very short, but the origin legs may be sufficiently far to justify investment in cycling 
#infrastructure to encourage people to cycle to the staitions. 


#Plot desire_rail 
plot(desire_rail$geometry, expandBB = c(.1, .1, .1, .1))
plot(desire_rail$leg_orig, add= TRUE, col = "red", lwd = 3)
plot(desire_rail$leg_via, add= TRUE, col = "gray", lwd = 2)
plot(bristol_stations, add= TRUE, col = "blue", lwd = 5)
plot(zone_cents_rail, add = TRUE, col = "black")


tmap_mode("view")
tm_basemap("OpenStreetMap.Mapnik") + 
  tm_shape(desire_rail$geometry) + 
  tm_lines(col = "black", lwd = 2) + 
  tm_layout(title = "Bristol Rail Routs", title.size = 0.8) +
  tm_shape(desire_rail$leg_orig) +
  tm_lines(col = "red", lwd = 3) + 
  tm_shape(desire_rail$leg_via) +
  tm_lines(col = "grey", lwd = 2) +
  tm_shape(bristol_stations) +
  tm_dots(size = .1, alpha = 0.5)
tm_borders(col = "blue", lwd = 5)


#Route Networks
summary(bristol_ways)

#As mentioned, route networks can usefully be represented as mathematical grpahs, with nodes on the network connected by edges.
#However doing it manually may drop geographic attributes of the route networks. 

#In the next code chunk, we are going to filter roads by the maximum speeds of 30 and 70 mph and plot them together to observe the route network.

ways_30 = bristol_ways %>% filter(maxspeed == "30 mph")
ways_70 = bristol_ways %>% filter(maxspeed == "70 mph")


#First plot the section of the network with the above speeds
tmap_mode("view")

tm_basemap("Stamen.TonerLite") +
  tm_shape(bristol_ttwa) +
  tm_borders(col = "purple", lwd = 3) + 
  tm_shape(ways_30) +
  tm_lines(col = "red", lwd = 1) +
  tm_shape(ways_70) + 
  tm_lines(col = "blue", lwd = 1)

##We use stplanr::SpatialLineNetwork() to create an object that is both route networks (i.e. lines) and graphs (i.e. class igraph) that
##allow us to represent the road netwrok as mathematical graphs. 

#Using only segments where the speed is 70 mph 

ways_sln = stplanr::SpatialLinesNetwork(ways_70) #ways_sln is an sfNetwork object. You can acccess individual individual components by the @operator
#The spatial component of the network (named sl)
#The graph component (g)
#The weightfield, the edge variable used for the shortest path calculation (by defult segment distance)

slotNames(ways_sln)
weightfield(ways_sln)
class(ways_sln@g)

#Before plotting, compute 'edge betweeness', the number of shrotest paths passing through each edge
e = igraph::edge_betweenness(ways_sln@g)
#Plot the geometry with the line length defined by the edge betweenness 
plot(ways_sln@sl$geometry, lwd = e / 500)
#Illustration of a small route network with segment thickness proportional to its betweeneness, generated using the igraph package. 


#####Prioritizing new infrastructure: 
#The car routes are our target commute trips, for which new bike infrastructures may change people's mode choice. 
route_cycleway = rbind(route_carshort)
route_cycleway$all = desire_carshort$all
View(bristol_ways)
View(bristol_ttwa)
View(route_cycleway)

tmap_mode("plot")
tm_shape(bristol_ttwa) + 
  tm_borders(col = "purple", lwd = 3) + 
  tm_shape(bristol_ways) + 
  tm_lines(col = "highway", title.col = "Routes Type", lwd = 3, palette = c("lightgreen", "grey", "pink")) +
  tm_scale_bar() +
  tm_shape(route_cycleway) + 
  tm_lines(col = "blue", lwd = "all", title.lwd = "Number of Trips", scale = 10 , alpha = 0.5) + 
  tm_layout(legend.position = c("LEFT", "TOP"), scale = 0.7)

## Figure 12 shows routes with high levels of car dependecy and highlights opportunities for cycling rail stations. The figure shows 
##that some routes along which cycle paths could be prioritzed from car dependencty and public transport perspectives. But as well , the routes 
##highlighted suggeste that transport systems are intimately linked to the wider economic and social context


tm_basemap("Statmen.TonerLite")
tm_basemap("OpenStreetMap.Mapnik")
?tm_basemap

?st_geometry
