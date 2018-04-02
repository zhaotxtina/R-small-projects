#https://gist.github.com/fileunderjeff/0344a26aa8b8024d9fbb926f58e78e9d

library(rvest)

url <- "http://www.houstontx.gov/parks/tennis.html"


# grab the raw material from the HTML table
tennis.courts <- html_session(url) %>% html_nodes(xpath="//*[@id='main']/div[1]/div[3]/div[1]/table[3]") %>% html_table()

# convert to data frame
tennis.courts <- data.frame(tennis.courts)

# assign column names to the data frame for reference
colnames(tennis.courts) <- c("Facility Name","Address","Key Map","CC","Number","LC","PW")

# remove the unnecessary header row
tennis.courts <- tennis.courts[(tennis.courts$`Facility Name` != "FACILITY NAME"), ]

# the data frame is ready, but the address field is missing some information.
# let's fix that by breaking apart the components of the address, then pasting them back together
# first, we copy the full address field to a new field called `zip.code`
tennis.courts$zip.code <- tennis.courts$Address 

# then we remove the preceding address info, leaving only the zip code
# this is done through gsub and regex
tennis.courts$zip.code <- gsub("^.+\\,","",tennis.courts$zip.code)

# then we remove the zip code from the address field
tennis.courts$Address <- gsub("\\,.+$","",tennis.courts$Address)

# then we paste together a full address for geocoding
# use paste0 to automatically set the sep to ""
tennis.courts$geocoding <- paste0(tennis.courts$Address,", Houston, TX ",tennis.courts$zip.code)

# now we are ready to geocode! we'll use the ggmap library and set up a separate variable to gather the coordinates
library(ggmap)
address <- geocode(tennis.courts$geocoding)

# then we copy the coordinates back into the data frame
tennis.courts$lat <- address$lat
tennis.courts$lon <- address$lon

# sometimes, there will not be any coordinates returned. leaflet does not like NAs. typically, we can fix that
# but for the sake of time, we are going to remove the NAs from this dataset
tennis.courts <- tennis.courts[(!is.na(tennis.courts$lat)), ]

# let's convert the data frame to a SpatialPointsDataFrame. we'll use the library `sf`
library(sf)
tennis.courts.map <- st_as_sf(x = tennis.courts, 
                              coords = c("lon", "lat"),
                              crs = "+proj=longlat +datum=WGS84")
tennis.courts.map <- as(tennis.courts.map, "Spatial")

# now we're ready to map!
# let's test the coordinates with mapview first
library(mapview)
mapview(tennis.courts.map)

# now let's set it up in leaflet
library(leaflet)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addMarkers(data=tennis.courts.map)

# notice that there is no identifying information. let's create a popup with the name of the park!
# for spatialpoints and polys, we must reference the @data slot
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addMarkers(data=tennis.courts.map,
             popup=paste(tennis.courts.map@data$Facility.Name))

# ok, what if we wanted to change the circle size based on the number of tennis courts?
# first, we need to convert the `Number` field from a character to a number
# then we need to set a radius of the circle (in meters) that is tied to the number of tennis courts
tennis.courts.map@data$Number <- as.numeric(as.character(tennis.courts.map@data$Number))
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addCircles(data=tennis.courts.map, 
             popup=paste(tennis.courts.map@data$Facility.Name,"<br/>Number of courts: ",tennis.courts.map@data$Number),
             radius=tennis.courts.map@data$Number * 500)

# let's mess with colors a bit. for 2 or fewer courts, we'll make them red. for 3 or more, we'll make them green.
# pal is a function that can be applied to a column in your data frame
pal <- colorBin(c('#ff0000','#009c1a'), bins = c(0, 3, 5), na.color="transparent")

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addCircles(data=tennis.courts.map, 
             popup=paste(tennis.courts.map@data$Facility.Name,"<br/>Number of courts: ",tennis.courts.map@data$Number),
             radius=tennis.courts.map@data$Number * 500,
             fillColor=pal(tennis.courts.map@data$Number), fillOpacity=0.8, weight=0) # `weight` eliminates the outline

# let's look a little bit more at the data structure
tennis.courts.map@coords %>% head()
tennis.courts.map@data %>% head()
tennis.courts.map@proj4string

# projecting into other coordinates
library(rgdal)
tennis.courts.map.projected <- tennis.courts.map %>% 
  spTransform(CRSobj = "+init=epsg:2278") # texas south central state plane
tennis.courts.map.projected@data %>% head()
tennis.courts.map.projected@coords %>% head()
tennis.courts.map.projected@proj4string

# let's work with polygons!
# what city council districts are these parks located in?
# here's the geojson file: https://opendata.arcgis.com/datasets/7237db114eeb416cb481f4450d8a0fa6_2.geojson
# download that and switch to that directory
setwd("C:/Users")
library(rgdal)
council.districts <- readOGR("City_Council_Districts.geojson")

# test the file with mapview
mapview(council.districts)

# add the polygons to our map!
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addCircles(data=tennis.courts.map, 
             popup=paste(tennis.courts.map@data$Facility.Name,"<br/>Number of courts: ",tennis.courts.map@data$Number),
             radius=tennis.courts.map@data$Number * 500,
             fillColor=pal(tennis.courts.map@data$Number), fillOpacity=0.8, weight=0) %>%
  addPolygons(data=council.districts,
              popup=paste("District: ",council.districts@data$DISTRICT,"<br/>",council.districts@data$MEMBER))

# nice! but a little ugly. let's style those polys.
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addCircles(data=tennis.courts.map, 
             popup=paste(tennis.courts.map@data$Facility.Name,"<br/>Number of courts: ",tennis.courts.map@data$Number),
             radius=tennis.courts.map@data$Number * 500,
             fillColor=pal(tennis.courts.map@data$Number), fillOpacity=0.8, weight=0) %>%
  addPolygons(data=council.districts,
              popup=paste("District: ",council.districts@data$DISTRICT,"<br/>",council.districts@data$MEMBER),
              weight=1, color="#9a9a00", fillColor="#ffff00", fillOpacity=0.2, smoothFactor=0.8)

# cool! but when i click on a circle, i can't see the right popup. we need layers control!
# this means we need to assign each layer to a group
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addCircles(data=tennis.courts.map, 
             popup=paste(tennis.courts.map@data$Facility.Name,"<br/>Number of courts: ",tennis.courts.map@data$Number),
             radius=tennis.courts.map@data$Number * 500,
             fillColor=pal(tennis.courts.map@data$Number), fillOpacity=0.8, weight=0,
             group="Tennis Courts") %>%
  addPolygons(data=council.districts,
              popup=paste("District: ",council.districts@data$DISTRICT,"<br/>",council.districts@data$MEMBER),
              weight=1, color="#9a9a00", fillColor="#ffff00", fillOpacity=0.2, smoothFactor=0.8,
              group="Council Districts") %>%
  addLayersControl(overlayGroups = c("Tennis Courts","Council Districts"), options = layersControlOptions(collapsed = FALSE))

# working with another polygon dataset
# dropbox link to the data: https://www.dropbox.com/sh/6sxuvha0ceouvo9/AADZeZBKveTtu-vz17HPbygia?dl=0
# upzip folder and set as working directory
# read in a shapefile
setwd("C:/Users/mapcensus")
census.tracts <- readOGR(dsn=getwd(), layer="HoustonAreaCensusTracts")
# read in tabular data
census.data <- read.csv("ACS_16_5YR_S1701.csv")
census.metadata <- read.csv("ACS_16_5YR_S1701_metadata.csv")

census.metadata %>% head(10)
census.tracts@data %>% head()
census.data[1:10] %>% head()

library(dplyr)
census.tracts.latlong <- census.tracts %>% 
  spTransform(CRS("+init=epsg:4326")) # lat long coordinates

# join tabular data to geometry
library(dplyr)
census.tracts.latlong@data$GEOID <- census.tracts.latlong@data$GEOID %>% as.character()
census.data$GEO.id2 <- census.data$GEO.id2 %>% as.character()
census.tracts.latlong@data <- census.tracts.latlong@data %>% 
  left_join(census.data %>% 
              select(GEOID = GEO.id2, 
                     pop = HC01_EST_VC01, 
                     pop_pov = HC02_EST_VC01,
                     pct_pov = HC03_EST_VC01),
            by="GEOID")
pal.pov.pct <- colorQuantile(palette = "Blues",
                             census.tracts.latlong@data$pct_pov,
                             n=7)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data=census.tracts.latlong, stroke= FALSE,
             fillOpacity=0.8,
             fillColor= ~pal.pov.pct(census.tracts.latlong@data$pct_pov) )


