library(terra)
library(rasterVis)
library(ggmap)
library(tidyverse)
library(knitr)
library(sf)

# install.packages("ncdf4")
library(ncdf4)

# Create afolder to hold the downloaded data
dir.create("data",showWarnings = F) #create a folder to hold the data

lulc_url="https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MCD12Q1.051_aid0001.nc?raw=true"
lst_url="https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MOD11A2.006_aid0001.nc?raw=true"

# download them
download.file(lulc_url,destfile="data/MCD12Q1.051_aid0001.nc", mode="wb")
download.file(lst_url,destfile="data/MOD11A2.006_aid0001.nc", mode="wb")

# Load data into R
lulc=rast("data/MCD12Q1.051_aid0001.nc",subds="Land_Cover_Type_1")
lst=rast("data/MOD11A2.006_aid0001.nc",subds="LST_Day_1km")

plot(lulc)

lulc=lulc[[13]]
plot(lulc)

# Assign land cover classes from MODIS website
Land_Cover_Type_1 = c(
  Water = 0, 
  `Evergreen Needleleaf forest` = 1, 
  `Evergreen Broadleaf forest` = 2,
  `Deciduous Needleleaf forest` = 3, 
  `Deciduous Broadleaf forest` = 4,
  `Mixed forest` = 5, 
  `Closed shrublands` = 6,
  `Open shrublands` = 7,
  `Woody savannas` = 8, 
  Savannas = 9,
  Grasslands = 10,
  `Permanent wetlands` = 11, 
  Croplands = 12,
  `Urban & built-up` = 13,
  `Cropland/Natural vegetation mosaic` = 14, 
  `Snow & ice` = 15,
  `Barren/Sparsely vegetated` = 16, 
  Unclassified = 254,
  NoDataFill = 255)

lcd=data.frame(
  ID=Land_Cover_Type_1,
  landcover=names(Land_Cover_Type_1),
  col=c("#000080","#008000","#00FF00", "#99CC00","#99FF99", "#339966", "#993366", "#FFCC99", 
        "#CCFFCC", "#FFCC00", "#FF9900", "#006699", "#FFFF00", "#FF0000", "#999966", "#FFFFFF", 
        "#808080", "#000000", "#000000"),
  stringsAsFactors = F)
# colors from https://lpdaac.usgs.gov/about/news_archive/modisterra_land_cover_types_yearly_l3_global_005deg_cmg_mod12c1
kable(head(lcd))

# Convert LULC raster into a ‘factor’ (categorical) raster
lulc=as.factor(lulc)
# update the RAT with a left join
levels(lulc)=left_join(levels(lulc)[[1]],lcd)[-1,]
activeCat(lulc)=1

# plot it
gplot(lulc)+
  geom_raster(aes(fill=as.factor(value)))+
  scale_fill_manual(values=setNames(lcd$col,lcd$ID),
                    labels=lcd$landcover,
                    breaks=lcd$ID,
                    name="Landcover Type")+
  coord_equal()+
  theme(legend.position = "right")+
  guides(fill=guide_legend(ncol=1,byrow=TRUE))

# Land Surface Temperature
plot(lst[[1:12]])

# Convert LST to Degrees C
scoff(lst)=cbind(0.02,-273.15)
plot(lst[[1:10]])

# Part 1: Extract timeseries for a point
lw= data.frame(x= -78.791547,y=43.007211) %>% 
  st_as_sf(coords=c("x","y"),crs=4326)

# Transform the point to the projection of the raster
lw_transformed <- st_transform(lw, st_crs(lst))

# Extract the LST data
lst_value <- terra::extract(lst,lw_transformed,buffer=1000,fun=mean,na.rm=T)

lst_value_transpose <- t(lst_value)[-1]
