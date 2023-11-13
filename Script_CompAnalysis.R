################################################################################################
#  Complementary analysis - separate bioclimatic and soil variables           
################################################################################################
## See the results when only climatic variables are considered, 
## or conversely only soil variables, i.e. soil pH.
#
# T.Ubaldi (tristan.ubaldi@u-picardie.fr)
# EDYSAN (UMR CNRS 7058), Université de Picardie Jules Verne, 1 rue des Louvels, FR-80037 Amiens Cedex, France
# Nov. 2023
#
# Clean R space
graphics.off()
rm(list=ls())
#
#### Parameters of the analysis ####
## introduction area/country
intro.name <- "British Isles" # or second case, "France"
intro <- c("UK", "Ireland") # or "France"
## List of species or the species to study, e.g.,
species <- "Rhododendron ponticum baeticum"
## Which periods? (folder names)
per <- c("1951-1980", "1981-2010","2011-2040", "2041-2070", "2071-2100")
## Which scenarios? (folder names)
ssp <- c("ssp126", "ssp370", "ssp585")

#### 1. Download shapefile #### 
### 1.1. Country of study with gadm() function
# Function gadm() to download my country or countries Shapefile(s) from gadm.org/index
source("https://raw.githubusercontent.com/tristanubaldi/NicheOverlapApproach/main/functions/gadm_function.R")
intro.shp <- gadm(dir = "~/Documents/", # indicate directory where to save shapefile
                  country = intro, # introduction country name
                  format = "vect") # save in format "vector" or "shapefile"

### 1.2. Native Range (NR) of studied species
# Create native range shapefile (nr.shp)
# Download species Shapefile on Github:
"https://github.com/tristanubaldi/RhodoNiche-Projection/tree/main/Native_range/" 
if (file.exists(paste(dir = "~/Documents/", # indicate directory where shapefile has been saved
                      species, sep="/"))) {
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
# Function crop.env() to crop environmental (bioclimatic or soil) rasters 
source("https://raw.githubusercontent.com/tristanubaldi/NicheOverlapApproach/main/functions/crop.env_function.R")
### 2.1. For soil variables only
nr.soil.df <- crop.env(dir.save = "~/Documents/", #indicate directory where the cropped rasters should be saved
                       dir.soil = "~/Documents/", # indicate directory where global soil rasters should be saved
                       type = "soil", # type "soil", i.e. with only soil variables
                       shp = nr.shp,# species native range shapefile
                       shp.name = species, # nr shp name I choose to give it
                       varsoil = "phh2o", # soil variables (e.g. soil pH)
                       depth = c("0-5", "5-15"),# soil depth (e.g. 0-5 and 5-15 cm here)
                       format = "dataframe",# format "raster" or "data.frame"
                       aggregate = T) 
int.soil.df <- crop.env(dir.save = "~/Documents/",# indicate directory where the cropped rasters should be saved
                        dir.soil = "~/Documents/",# indicate directory where global soil rasters should be saved
                        type = "soil",# type "soil", i.e. with only soil variables
                        shp = intro.shp,# introduction area shapefile
                        shp.name = intro.name,# introduction shapefile name (e.g. "France")
                        varsoil = "phh2o", # soil variables (e.g. soil pH)
                        depth = c("0-5", "5-15"),# soil depth (e.g. 0-5 and 5-15 cm here)
                        format = "dataframe",# format "raster" or "data.frame"
                        aggregate = T) 
### 2.2. For bioclimatic variables only
nr.bclim.df <- crop.env(dir.save = "~/Documents/",# indicate directory where the cropped rasters should be saved
                        dir.chelsa = "~/Documents/",# indicate directory where global bioclimatic rasters are saved
                        type = "clim",#type "clim", i.e. with only bioclimatic variables
                        shp = nr.shp,#species native range shapefile
                        shp.name = species, # nr shp name I choose to give it
                        varbio = c(1:19),# 19 bioclimatic variables
                        period = "1951-1980", # only on "1951-1980" period for species nr (as reference)
                        format = "dataframe",# format "raster" or "data.frame"
                        aggregate = T) 
int.bclim.df <- crop.env(dir.save = "~/Documents/", # indicate directory where the cropped rasters should be saved
                         dir.chelsa = "~/Documents/",# indicate directory where global bioclimatic rasters are saved
                         type = "clim",#type "clim", i.e. with only bioclimatic variables
                         shp = intro.shp,# introduction area shapefile
                         shp.name = intro.name, # introduction shapefile name (e.g. "France")
                         varbio = c(1:19),# 19 bioclimatic variables
                         period = per, 
                         ssp = ssp,
                         format = "dataframe",# format "raster" or "data.frame"
                         aggregate = T) 
#### 3. PCA ####
# Function pca.env() representing the environmental space (PCA) between species nr and introduction area
source("https://raw.githubusercontent.com/tristanubaldi/NicheOverlapApproach/main/functions/pca.env_function.R")
### 3.1. For soil variables only
# List with all pca plot 
pca.soil.list <- pca.env(int.soil.df, # introduction data frame with soil variables (intro df first is important!)
                         nr.soil.df,# species nr data frame with soil variables
                         type = "soil",#type "soil", i.e. with only soil variables
                         format="plot")# format of list resulting, here "plot"
# List of extracted individual coordinates from PCA (get_pca_ind)
coord.soil <- pca.env(int.soil.df, # introduction data frame with soil variables (intro df first is important!)
                      nr.soil.df,# species nr data frame with soil variables
                      type = "soil", #type "soil", i.e. with only soil variables
                      format="coord")# format of list resulting, here "coord"
### 3.2. For bioclimatic variables only
# List with all pca plot 
pca.bclim.list <- pca.env(int.bclim.df, # introduction data frame with bioclimatic var.(intro df first is important!) 
                          nr.bclim.df,# species nr data frame with bioclimatic variables
                          type = "clim", #type "clim", i.e. with bioclimatic soil variables
                          format="plot", # format of list resulting, here "plot"
                          period = per,
                          ssp = ssp)
# List of extracted individual coordinates from PCA (get_pca_ind)
coord.bclim <- pca.env(int.bclim.df,# introduction data frame with bioclimatic var.(intro df first is important!) 
                       nr.bclim.df,# species nr data frame with bioclimatic variables
                       type = "clim", #type "clim", i.e. with bioclimatic soil variables 
                       format="coord",# format of list resulting, here "coord"
                       period = per,
                       ssp = ssp)
#### 4. Niche overlap ####
# Function niche.overlap() to determine the overlap of conditions between species nr and introduction area 
source("https://raw.githubusercontent.com/tristanubaldi/NicheOverlapApproach/main/functions/niche.overlap_function.R")
### 4.1. For soil variables only
# list of plots with bioclimatic niche overlap between nr and introduction area for each periods and ssp
dist.soil.list <- niche.overlap(int.soil.df,# introduction data frame with soil variables (intro df first is important!)
                                nr.soil.df,# species nr data frame with soil variables
                                md = 0.2,# maximum distance of the points between the two niches to be selected 
                                type = "soil",#type "soil", i.e. with soil variables 
                                coordinate = coord.soil,#list of extracted individual coordinates from PCA with only soil var.
                                shp.name = intro.name,# introduction shapefile name
                                sp.name = species,# name give to species nr
                                format = "plot")# format of list resulting, here "plot"
# list of df with only select point corresponding to introduction that overlap with nr 
map.soil.list <- niche.overlap(int.soil.df, # introduction data frame with soil variables (intro df first is important!)
                               nr.soil.df,# species nr data frame with soil variables
                               md = 0.2,# maximum distance of the points between the two niches to be selected 
                               type = "soil",#type "soil", i.e. with soil variables 
                               coordinate = coord.soil,#list of extracted individual coordinates from PCA with only soil var.
                               shp.name = intro.name,# introduction shapefile name
                               sp.name = species,# name give to species nr
                               format = "map")# format of list resulting, here "map"
### 4.2. For bioclimatic variables only
# list of plots with bioclimatic niche overlap between nr and introduction area for each periods and ssp
dist.bclim.list <- niche.overlap(int.bclim.df,#introduction data frame with bioclimatic var.(intro df first is important!) 
                                 nr.bclim.df,# species nr data frame with bioclimatic variables
                                 md = 0.2,# maximum distance of the points between the two niches to be selected
                                 type = "clim",#type "clim", i.e. with bioclimatic variables 
                                 coordinate = coord.bclim,#list of extracted individual coordinates from PCA with only bioclimatic var.
                                 shp.name = intro.name,# introduction shapefile name
                                 sp.name = species,# name give to species nr
                                 format = "plot",# format of list resulting, here "plot"
                                 period = per,
                                 ssp = ssp)
# list of df with only select point corresponding to introduction that overlap with nr 
map.bclim.list <- niche.overlap(int.bclim.df,#introduction data frame with bioclimatic var.(intro df first is important!) 
                                nr.bclim.df,# species nr data frame with bioclimatic variables
                                md = 0.2,# maximum distance of the points between the two niches to be selected
                                type = "clim",#type "clim", i.e. with bioclimatic variables 
                                coordinate = coord.bclim,#list of extracted individual coordinates from PCA with only bioclimatic var.
                                shp.name = intro.name,# introduction shapefile name
                                sp.name = species,# name give to species nr
                                format = "map",# format of list resulting, here "map"
                                period = per,
                                ssp = ssp)
#### 5. Mapping the Niche overlap ####
# define not ssp in "1951-1980" and "1981-2010" periods
for (j in per){
  if (isFALSE(str_detect(unique(grep(j, names(map.bclim.list), value = TRUE)), "ssp"))){
    map.bclim.list[[(grep(j, names(map.bclim.list), value = TRUE))]]["Scenario"]= NA
  } 
}
# merge list of df in one
# bioclim
map.bclim <- map.bclim.list %>% 
  data.table::rbindlist(.)
# soil
map.soil <- map.soil.list %>% 
  data.table::rbindlist(.)
# obtain plot of niche projection in country map 
### 5.1. For soil variables only
# Function niche.proj() to plot the niche projection in country studied
source("https://raw.githubusercontent.com/tristanubaldi/NicheOverlapApproach/main/functions/niche.proj_function.R")
proj.soil.list <- niche.proj(map.soil,#data frame with all select soil point corresponding to introduction that overlap with nr 
                             type = "soil",#type "soil", i.e. with soil variables 
                             intro = intro)#list of introduction countries or country, or area, to study
proj.soil.list$plot.soil
ggsave(paste0("Plot4.pdf"),# define name of your image
       width=10, height=5, proj.soil.list$plot.soil, 
       path= "~/Documents/")# indicate folder where to save your image

### 5.2. For bioclimatic variables only
proj.bclim.list <- niche.proj(map.bclim,#data frame with all select bioclim. point corresponding to introduction that overlap with nr 
                              type = "clim",#type "clim", i.e. with soil variables 
                              intro = intro,#list of introduction countries or country, or area, to study
                              period = per, 
                              ssp = ssp)
#### 6. Arrange Maps together ####
require(cowplot)
require(patchwork)
require(sf)
## 6.2. Projection of the potential ecological niche in the geographic space
p1 <- plot_grid(proj.bclim.list[[1]], proj.bclim.list[[2]], ncol = 2) 
p2 <- plot_grid(proj.bclim.list[[3]], proj.bclim.list[[6]], proj.bclim.list[[9]], ncol = 3) + 
  plot_annotation(title = "SSP1-2.6",
                  theme = theme(plot.title = element_text(size = 30, face="bold")))
p3 <- plot_grid(proj.bclim.list[[4]], proj.bclim.list[[7]], proj.bclim.list[[10]], ncol = 3) + 
  plot_annotation(title = "SSP3-7.0",
                  theme = theme(plot.title = element_text(size = 30, face="bold")))
p4 <- plot_grid(proj.bclim.list[[5]], proj.bclim.list[[8]], proj.bclim.list[[11]], ncol = 3) + 
  plot_annotation(title = "SSP5-8.5",
                  theme = theme(plot.title = element_text(size = 30, face="bold")))

patch1 <- (wrap_elements(p1) / wrap_elements(p2) / wrap_elements(p3) / wrap_elements(p4)) +
  plot_annotation(
    title = paste("Potential bioclimatic distribution of", species, "in", intro.name, sep=" "),
    subtitle = "with bioclimatic variables 1 to 19 (CHELSA)",
    theme = theme(plot.title = element_text(size = 30, face="bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 25, hjust = 0.5)))
patch1
ggsave(paste0("Plot5.pdf"),# define name of your image
       width=20, height=31, patch1, 
       path= "~/Documents/")# indicate folder where to save your image

## 6.3. Overlap of environmental spaces defined by the first two axes of PCA
p5 <- plot_grid(pca.bclim.list[[1]], pca.bclim.list[[2]], ncol=2)
p6 <- plot_grid(pca.bclim.list[[3]], pca.bclim.list[[6]], pca.bclim.list[[9]], ncol = 3) +
  plot_annotation(title = "SSP1-2.6",
                  theme = theme(plot.title = element_text(size = 15, face="bold")))
p7 <- plot_grid(pca.bclim.list[[4]], pca.bclim.list[[7]], pca.bclim.list[[10]], ncol = 3) +
  plot_annotation(title = "SSP3-7.0",
                  theme = theme(plot.title = element_text(size = 15, face="bold")))
p8 <- plot_grid(pca.bclim.list[[5]], pca.bclim.list[[8]], pca.bclim.list[[11]], ncol = 3) +
  plot_annotation(title = "SSP5-8.5",
                  theme = theme(plot.title = element_text(size = 15, face="bold")))
patch2 <- (wrap_elements(p5) / wrap_elements(p6) / wrap_elements(p7) / wrap_elements(p8)) +
  plot_annotation(
    title = paste("Overlap of environmental spaces of", species, "in", intro.name, sep=" "),
    subtitle = "with bioclimatic variables 1 to 19 (CHELSA)",
    theme = theme(plot.title = element_text(size = 17, face="bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 15, hjust = 0.5)))
patch2
ggsave(paste0("Plot6.pdf"),# define name of your image
       width=11, height=16, patch2, 
       path= "~/Documents/")# indicate folder where to save your image
#end