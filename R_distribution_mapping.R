setwd("~/Desktop")

#######################plot on google map
library(RgoogleMaps)

#download google map
background <- GetMap(center=center, zoom=7, SCALE = 2, size=c(640, 640), 
                     API_console_key="AIzaSyDuX6ntUpUBOyeFzSIfaaDkJj3UiyWh87s",
                     maptype= "terrain", destfile = "background.png")
#prepare data
apter <- read.csv(file = "Apterotheca_records-2020-07-21.csv")
head(apter)
Lat <- apter$Latitude
Lon <- apter$Longitude
center <- c(mean(Lat), mean(Lon))

#plot
PlotOnStaticMap(background, Lat, Lon, pch=19, cex=1, col= '#FB6A4A80')


#################Plot on oz map
library(maps)
library(mapdata)
library(sp)
library(ggplot2)
library(GISTools)

#download oz map
aus<-map("worldHires", "Australia", fill=TRUE, xlim=c(110,160),
         ylim=c(-45,-5), mar=c(0,0,0,0))

#convert map data to spatial polygons
map2SpatialPolygons <- function(df, proj4string=CRS("+proj=longlat")) {
  Plys <- list()
  i<-1
  mtch <- which(is.na(df$x))
  if(length(mtch)==0) {
    mtch <- length(df$x)+1
  } else {mtch <-mtch}
  shps <- length(mtch)
  #make sure the names are unique
  nms <- df$names
  nms[duplicated(nms)] <- paste(nms[duplicated(nms)],1:length(nms[duplicated(nms)]))
  for (j in 1:shps){
    Plys[[j]] <- Polygons(list(Polygon(cbind(df$x[i:(mtch[j]-1)],
                                             df$y[i:(mtch[j]-1)]))),ID=nms[j])
    i <- mtch[j]+1
  }
  SpatialPolygons(Plys,proj4string=proj4string)
}

aus.sp <- map2SpatialPolygons(aus)
par(mar=c(0,0,0,0))
plot(aus.sp, asp=1)

#prepare record table
heleini <- read.csv(file = "Heleini_records.csv")
head(heleini)
taxa <- subset(heleini, genus=="Aglypta" )
taxa.tmp <- cbind (taxa$Longitude, taxa$Latitude)
taxa.spdf <- SpatialPointsDataFrame(taxa.tmp, data = taxa)

#plot
myplot <- plot(taxa.spdf, add = T, col = "#0000FF60", pch = 16, cex = 1.5)

#save to pdf
pdf("ggplot.pdf")
print(myplot) 
dev.off()


#mapping with ggplot
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

#download world map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#function to generate species distribution
map_gen <- function(taxa, csv){
  #prepare species record dataframe
  t <- subset(csv, Genus==taxa )
  sites <- st_as_sf(t, coords = c("Longitude", "Latitude"), 
                    crs = 4326, agr = "constant")

  #plot
  ggplot(data = world) +
    geom_sf(fill = "antiquewhite1") +
    geom_sf(data = sites, size = 3, 
            shape = 21, fill="red", color="red") +
    coord_sf(xlim=c(110,160), ylim=c(-45,-5), expand = FALSE) +
    labs(title=taxa) + 
    theme(plot.title = element_text(hjust = 0.5, size = 12), panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                                                                         size = 0.2), panel.background = element_rect(fill = "aliceblue"))
}

#read in data
df <- read.csv(file = "Adeliini_records.csv")
head(df)
taxon_list <- unique(df$Genus)
print(taxon_list)

#print Adeliini maps
pdf(file='map.pdf')
map_gen("Adelium", df)
map_gen("Adelodemus", df)
map_gen("Apasis", df)
map_gen("Apocryphodes", df)
map_gen("Bellendenum", df)
map_gen("Blepegenes", df)
map_gen("Bluops", df)
map_gen("Bolusculus", df)
map_gen("Brycopia", df)
map_gen("Cardiothorax", df)
map_gen("Coripera", df)
map_gen("Daedrosis", df)
map_gen("Diaspirus", df)
map_gen("Dicyrtodes", df)
map_gen("Diemenoma", df)
map_gen("Dorrigonum", df)
map_gen("Epomidus", df)
map_gen("Isopteron", df)
map_gen("Leptogastrus", df)
map_gen("Licinoma", df)
map_gen("Monteithium", df)
map_gen("Nolicima", df)
map_gen("Nototrintus", df)
map_gen("Seirotrana", df)
map_gen("Yarranum", df)
dev.off()


#print Heleini maps
pdf(file='map.pdf')
map_gen("Aglypta", df)
map_gen("Amarygmimus", df)
map_gen("Amphianax", df)
map_gen("Asphalus", df)
map_gen("Atoreuma", df)
map_gen("Bassianus", df)
map_gen("Batessia", df)
map_gen("Bolbophanes", df)
map_gen("Boreosaragus", df)
map_gen("Brises", df)
map_gen("Byallius", df)
map_gen("Camponotiphilus", df)
map_gen("Celibe", df)
map_gen("Cillibus", df)
map_gen("Cyphaleus", df)
map_gen("Dysarchus", df)
map_gen("Edylius", df)
map_gen("Emcephalus", df)
map_gen("Helea", df)
map_gen("Hemicyclus", df)
map_gen("Lepispilus", df)
map_gen("Meneristes", df)
map_gen("Mithippia", df)
map_gen("Mitrothorax", df)
map_gen("Nyctozoilus", df)
map_gen("Olisthaena", df)
map_gen("Onotrichus", df)
map_gen("Ospidus", df)
map_gen("Paraphanes", df)
map_gen("Phanechloros", df)
map_gen("Platyphanes", df)
map_gen("Prophanes", df)
map_gen("Pterohelaeus", df)
map_gen("Saragus", df)
map_gen("Sloanea", df)
map_gen("Styrus", df)
map_gen("Sympetes", df)
map_gen("Trichosaragus", df)
dev.off()



#loop function, run into grid error
map_graph <- function(df, na.rm = TRUE, ...){
  taxon_list <- unique(df$genus)
  
  for (i in seq_along(taxon_list)){
    #prepare species record dataframe
    t <- subset(df, df$genus==taxon_list[i] )
    sites <- st_as_sf(t, coords = c("Longitude", "Latitude"), 
                    crs = 4326, agr = "constant")
    #plot
    plot<-
      ggplot(data = world) +
        geom_sf(fill = "antiquewhite1") +
        geom_sf(data = sites, size = 3, 
                shape = 21, fill="red", color="red") +
        coord_sf(xlim=c(110,160), ylim=c(-45,-5), expand = FALSE) +
        labs(title=taxon_list[i]) + 
        theme(plot.title = element_text(hjust = 0.5, size = 12), panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                                                                                  size = 0.2), panel.background = element_rect(fill = "aliceblue"))
    print(plot)
    }
}

map_graph(input)



###############################################################################################
#alternative function to generate species distribution for all records in the csv file
map_gen_all <- function(csv){
  #prepare species record dataframe
  sites <- st_as_sf(csv, coords = c("Longitude", "Latitude"), 
                    crs = 4326, agr = "constant")
  
  #plot
  ggplot(data = world) +
    geom_sf(fill = "antiquewhite1") +
    geom_sf(data = sites, size = 3, 
            shape = 21, fill="red", color="red") +
    coord_sf(xlim=c(110,160), ylim=c(-45,-5), expand = FALSE) +
    theme(plot.title = element_text(hjust = 0.5, size = 12), panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                                                                             size = 0.2), panel.background = element_rect(fill = "aliceblue"))
}

#print Adeliini map
df <- read.csv(file = "Adeliini_records.csv")
head(df)
pdf(file='Adeliini_map.pdf')
map_gen_all(df)
dev.off()

#print Heleini map
df <- read.csv(file = "Heleini_records.csv")
head(df)
pdf(file='Heleini_map.pdf')
map_gen_all(df)
dev.off()
##################################################################################################

#plot multiple species distribution on the same map
asp <- subset(df, subtribe=="Asphalina" )
hel <- subset(df, subtribe=="Heleina" )
cyp <- subset(df, subtribe=="Cyphaleina" )


#prepare species record dataframe
site1 <- st_as_sf(asp, coords = c("Longitude", "Latitude"), 
                    crs = 4326, agr = "constant")
site2 <- st_as_sf(hel, coords = c("Longitude", "Latitude"), 
                    crs = 4326, agr = "constant")
site3 <- st_as_sf(cyp, coords = c("Longitude", "Latitude"), 
                    crs = 4326, agr = "constant")
  
#plot
pdf(file='Heleini_sub_map.pdf')
ggplot(data = world) +
    geom_sf(fill = "antiquewhite1") +
    geom_sf(data = site1, size = 2, 
            shape = 21, fill="green", color="green") +
    geom_sf(data = site2, size = 2, 
            shape = 21, fill="red", color="red") +
    geom_sf(data = site3, size = 2, 
            shape = 21, fill="blue", color="blue") +
    coord_sf(xlim=c(110,160), ylim=c(-45,-5), expand = FALSE) +
    theme(plot.title = element_text(hjust = 0.5, size = 12), panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                                                                             size = 0.2), panel.background = element_rect(fill = "aliceblue"))

dev.off()

#plot Adeliini, separate Isopteron
df <- read.csv(file = "Adeliini_records.csv")
a <- subset(df, cat=="A" )
b <- subset(df, cat=="B" )

site1 <- st_as_sf(a, coords = c("Longitude", "Latitude"), 
                  crs = 4326, agr = "constant")
site2 <- st_as_sf(b, coords = c("Longitude", "Latitude"), 
                  crs = 4326, agr = "constant")

pdf(file='Adeliini_sub_map.pdf')
ggplot(data = world) +
  geom_sf(fill = "antiquewhite1") +
  geom_sf(data = site1, size = 2, 
          shape = 21, fill="blue", color="blue") +
  geom_sf(data = site2, size = 2, 
          shape = 21, fill="red", color="red") +
  coord_sf(xlim=c(110,160), ylim=c(-45,-5), expand = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, size = 12), panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                                                                           size = 0.2), panel.background = element_rect(fill = "aliceblue"))

dev.off()

