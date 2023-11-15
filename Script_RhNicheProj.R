#################################################################################
#
#        Script for Niche projection to predict current and future potential 
#                 invasion of Rhododendron ponticum in France
#
#################################################################################
# by Tristan Ubaldi (tristan.ubaldi@u-picardie.fr)
# EDYSAN (UMR CNRS 7058), Université de Picardie Jules Verne, 1 rue des Louvels, FR-80037 Amiens Cedex, France
# Nov. 2023
#
# Clean R space
graphics.off()
rm(list=ls())
#### Parameters of the analysis ####
## Introduction area/country
intro.name <- "British Isles" # or second case, "France"
intro <- c("UK", "Ireland") # or "France"
## Species to study, e.g.,
species <- "Rhododendron ponticum baeticum"
## Which periods? (folder names)
per <- c("1951-1980", "1981-2010","2011-2040", "2041-2070", "2071-2100")
## Which scenarios? (folder names)
ssp <- c("ssp126", "ssp370", "ssp585")

#### 1. Download shapefile #### 
### 1.1. Country of study with gadm() function
# Function gadm() from geodata package
require(countrycode)
ISO <- countrycode(intro.name, origin = 'country.name', destination = 'iso3c')
require(terra)
require(geodata)
intro.shp <- gadm(country=ISO, level=0, path=tempdir(), version="latest")

### 1.2. Native Range (NR) of studied species
# Create native range shapefile (nr.shp)
# Download species Shapefile on Github:
"https://github.com/tristanubaldi/RhodoNiche-Projection/tree/main/Native_range/" 
dir = "~/Documents/" # indicate directory where shapefile has been saved
if (file.exists(paste(dir, species, sep="/"))) {
      d <- (paste(dir, species, sep="/"))
      file <- list.files(path = d, pattern = "\\.shp$", full.names = F) # found file .shp in folder
      if (length(file) < 0) {
        print("Error, no found")
      } else {
        print(paste(species, "native range shapefile find", sep= " ")) } 
  
  nr.shp <- vect(paste(dir, species, file, sep="/"))
  crs(nr.shp) <- "epsg:4326" # define shapefile projection
  plot(nr.shp) 
} else {
  print(paste("Species native range shapefile", species, "not find", sep=" ")) }

#### 2. Download environmental variables with crop.env() function ####
# Recommended: Download directly CHELSA bioclimatic variables (https://chelsa-climate.org/downloads/) V2.1. for each periods,
# or use Chelsa.CMIP_6.download() function from https://github.com/HelgeJentsch/ClimDatDownloadR (but 1981-2010 period is missing)
# library(ClimDatDownloadR)
# 
# Function crop.env() to crop environmental (bioclimatic or soil) rasters 
source("https://raw.githubusercontent.com/tristanubaldi/NicheOverlapApproach/main/functions/crop.env_function.R")
# in introduction area
int.df <- crop.env(dir.save = "~/Documents/", # indicate directory where the cropped rasters should be saved
                   dir.chelsa = "~/Documents/", # indicate directory where global bioclimatic rasters are saved
                   dir.soil = "~/Documents/", # indicate directory where global soil rasters should be saved
                   type = "both", # type "both", i.e. with bioclimatic and soil variables
                   shp = intro.shp, # introduction area shapefile
                   shp.name = intro.name, # introduction shapefile name (e.g. "France")
                   varbio = c(1:19), # 19 bioclimatic variables
                   varsoil = "phh2o", # soil variables (e.g. soil pH)
                   depth = "0-5", # soil depth (e.g. 0-5 cm)
                   period = per,
                   ssp = ssp,
                   format = "dataframe", # format "raster" or "data.frame"
                   aggregate = T) # 

# in species native range (nr)
nr.df <- crop.env(dir.save = "~/Documents/", # indicate directory where the cropped rasters should be saved
                  dir.chelsa = "~/Documents/", # indicate directory where global bioclimatic rasters are saved
                  dir.soil = "~/Documents/", # indicate directory where global soil rasters should be saved
                  type = "both", # type "both", i.e. with bioclimatic and soil variables
                  shp = nr.shp, # species native range shapefile
                  shp.name = "Iberic Peninsula", # nr shp name I choose to give it
                  varbio = c(1:19), # 19 bioclimatic variables
                  varsoil = "phh2o", # soil variables (e.g. soil pH)
                  depth = "0-5", # soil depth (e.g. 0-5 cm)
                  period = "1951-1980", # only on "1951-1980" period for species nr (as reference)
                  format = "dataframe", # format "raster" or "data.frame"
                  aggregate = T) # 

#### 3. PCA ####
# Function pca.env() representing the environmental space (PCA) between species nr and introduction area
source("https://raw.githubusercontent.com/tristanubaldi/NicheOverlapApproach/main/functions/pca.env_function.R")
# List with all pca plot 
pca.list <- pca.env(int.df, # introduction data frame (intro df first is important!)
                    nr.df, # species nr data frame
                    type = "both", # type "both" i.e. with bioclimatic and soil variables
                    format="plot", # format of list resulting, here "plot"
                    period = per,
                    ssp = ssp)
# List of extracted individual coordinates from PCA (get_pca_ind)
coord <- pca.env(int.df, # introduction data frame (intro df first is important!)
                 nr.df, # species nr data frame
                 type = "both", # type "both" i.e. with bioclimatic and soil variables
                 format="coord", # format of list resulting, here "coord"
                 period = per,
                 ssp = ssp)

#### 4. Niche overlap ####
# Function niche.overlap() to determine the overlap of conditions between species nr and introduction area 
source("https://raw.githubusercontent.com/tristanubaldi/NicheOverlapApproach/main/functions/niche.overlap_function.R")
# list of plots with bioclimatic niche overlap between nr and introduction area for each periods and ssp
dist.list <- niche.overlap(int.df, # introduction data frame (intro df first is important!)
                           nr.df, # species nr data frame
                           md = 0.2, # maximum distance of the points between the two niches to be selected 
                           type = "both", # type "both" i.e. with bioclim and soil variables
                           coordinate = coord, #list of extracted individual coordinates from PCA
                           shp.name = intro.name, # introduction shapefile name
                           sp.name = "Iberic Peninsula", # name give to species nr
                           format = "plot", # format of list resulting, here "plot"
                           period = per,
                           ssp = ssp)
# list of df with only select point corresponding to introduction that overlap with nr 
map.list <- niche.overlap(int.df, # introduction data frame (intro df first is important!)
                          nr.df,# species nr data frame
                          md = 0.2,# maximum distance of the points between the two niches to be selected 
                          type = "both",# type "both" i.e. with bioclim and soil variables
                          coordinate = coord,#list of extracted individual coordinates from PCA
                          shp.name = intro.name, # introduction shapefile name
                          sp.name = "Iberic Peninsula",# name give to species nr
                          format = "map", # format of list resulting, here "map"
                          period = per,
                          ssp = ssp)

#### 5. Mapping the Niche overlap ####
# define not scenarios (ssp) in "1951-1980" and "1981-2010" periods
for (j in per){
  if (isFALSE(str_detect(unique(grep(j, names(map.list), value = TRUE)), "ssp"))){
    map.list[[(grep(j, names(map.list), value = TRUE))]]["Scenario"]= NA
  } 
}
# merge list of df in one df
map <- map.list %>% 
  data.table::rbindlist(.)
# obtain plot of niche projection in country map 
# Function niche.proj() to plot the niche projection in country studied
source("https://raw.githubusercontent.com/tristanubaldi/NicheOverlapApproach/main/functions/niche.proj_function.R")
proj.list <- niche.proj(map, #data frame with all select point corresponding to introduction that overlap with nr 
                        type = "both", # type "both" i.e. with bioclimatic and soil variables
                        intro = intro, #list of introduction countries or country to study
                        period = per, 
                        ssp = ssp)

#### 6. Arrange Maps together ####
require(cowplot)
require(patchwork)
require(sf)
# First, add Native Range in Reference maps
# Obtain World map coordinates
world_coordinates <- map_data("world") %>% 
  st_as_sf(coords = c("long", "lat"), dim = "XY") %>% 
  st_set_crs(4326)
# Find min and max species native range (nr) coordinates
coord_range <- st_as_sf(nr.shp)
coord_range <- sf::st_cast(coord_range, "MULTIPOLYGON")
coord_range <- data.frame(st_coordinates(coord_range$geometry))
# Do border of species native range (nr)
bb <- c("xmin" = min(coord_range$X)-1,
        "xmax" = max(coord_range$X)+1,
        "ymin" = min(coord_range$Y)-1,
        "ymax" = max(coord_range$Y)+1) %>%
  sf::st_bbox(.) %>%
  sf::st_as_sfc(.) %>%
  sf::st_as_sf(crs = 4326) %>%
  sf::st_transform(crs = 4326)

# Plot 1: Species Native range (nr) in world
world <- ggplot() + 
  geom_map(data = map_data("world"), map = map_data("world"), fill = "#ECECEC",
           aes(long, lat, map_id = region)) +
  geom_sf(data = bb, colour = "red", fill = NA, linewidth = 1) +
  tidyterra::geom_spatvector(data = nr.shp, fill = "#92D168FF", linewidth = NA) +
  coord_sf(xlim = c((min(coord_range$X)-20), (max(coord_range$X)+20)), 
           ylim = c((min(coord_range$Y)-20), (max(coord_range$Y)+20)), expand = FALSE) +
  theme(plot.title = element_text(size = 20, face="bold"),
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 15),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),  axis.line = element_line(colour = "black")) +
  labs(y = "Latitude",
       x = "Longitude") #+ ggtitle(paste("Native range of", species, sep= " ")) 
world

# Plot 2: More focus on Species Native Range
prange <- ggplot() +
  geom_map(
    data = map_data("world"), map = map_data("world"), fill = "#ECECEC",
    aes(long, lat, map_id = region) ) +
  tidyterra::geom_spatvector(data = nr.shp, fill = "#92D168FF", linewidth = NA) + 
  theme(plot.title = element_text(size = 20, face = "bold")) +
  coord_sf(xlim = c((min(coord_range$X)-1), (max(coord_range$X)+1)), 
           ylim = c((min(coord_range$Y)-1), (max(coord_range$Y)+1)), expand = FALSE) +
  theme(text = element_text(size = 15), 
        legend.title = element_blank(),
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),  axis.line = element_line(colour = "black")) +
  labs(y = "Latitude",
       x = "Longitude") 
prange

## 6.1. Plot Species native range maps
mapnr <- plot_grid(world, prange, ncol=2) +
  plot_annotation(title = paste("Native range of", species, "in", intro.name, sep= " "),
                  theme = theme(plot.title = element_text(size = 15, face="bold")))
ggsave("Plot1.pdf", # define name of your image
       width=8, height=5, mapnr, 
       path= "~/Documents/") # indicate folder where to save your image

## 6.2. Projection of the potential ecological niche in the geographic space
p1 <- plot_grid(proj.list[[1]], proj.list[[2]], ncol = 2) 
p2 <- plot_grid(proj.list[[3]], proj.list[[6]], proj.list[[9]], ncol = 3) + 
  plot_annotation(title = "SSP1-2.6",
                  theme = theme(plot.title = element_text(size = 30, face="bold")))
p3 <- plot_grid(proj.list[[4]], proj.list[[7]], proj.list[[10]], ncol = 3) + 
  plot_annotation(title = "SSP3-7.0",
                  theme = theme(plot.title = element_text(size = 30, face="bold")))
p4 <- plot_grid(proj.list[[5]], proj.list[[8]], proj.list[[11]], ncol = 3) + 
  plot_annotation(title = "SSP5-8.5",
                  theme = theme(plot.title = element_text(size = 30, face="bold")))

patch1 <- (wrap_elements(p1) / wrap_elements(p2) / wrap_elements(p3) / wrap_elements(p4)) +
  plot_annotation(
    title = paste("Potential environmental distribution of", species, "in", intro.name, sep=" "),
    subtitle = "with bioclimatic variables 1 to 19 (CHELSA) and soil pH in depth 0-5cm",
    theme = theme(plot.title = element_text(size = 30, face="bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 25, hjust = 0.5)))
patch1
ggsave(paste0("Plot2.pdf"), # define name of your image
       width=20, height=31, patch1, 
       path= "~/Documents/") # indicate folder where to save your image

## 6.3. Overlap of environmental spaces defined by the first two axes of PCA
p5 <- plot_grid(pca.list[[1]], pca.list[[2]], ncol=2)
p6 <- plot_grid(pca.list[[3]], pca.list[[6]], pca.list[[9]], ncol = 3) +
  plot_annotation(title = "SSP1-2.6",
                  theme = theme(plot.title = element_text(size = 15, face="bold")))
p7 <- plot_grid(pca.list[[4]], pca.list[[7]], pca.list[[10]], ncol = 3) +
  plot_annotation(title = "SSP3-7.0",
                  theme = theme(plot.title = element_text(size = 15, face="bold")))
p8 <- plot_grid(pca.list[[5]], pca.list[[8]], pca.list[[11]], ncol = 3) +
  plot_annotation(title = "SSP5-8.5",
                  theme = theme(plot.title = element_text(size = 15, face="bold")))
patch2 <- (wrap_elements(p5) / wrap_elements(p6) / wrap_elements(p7) / wrap_elements(p8)) +
  plot_annotation(
    title = paste("Overlap of environmental spaces of", species, "in", intro.name, sep=" "),
    subtitle = "with bioclimatic variables 1 to 19 (CHELSA) and soil pH in depth 0-5cm",
    theme = theme(plot.title = element_text(size = 17, face="bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 15, hjust = 0.5)))
patch2
ggsave(paste0("Plot3.pdf"),# define name of your image
       width=11, height=16, patch2, 
       path= "~/Documents/") # indicate folder where to save your image
#end
