


## Data Source Information and Extraction

## Code accompanying the manuscript:
## Strategic restoration of global wetlands can maximize climatic benefits
## Emily A. Ury et al.



# install.packages("ncdf4")
# install.packages("raster")

library(ncdf4)
library(raster)


### Path to raw data files:
setwd("C:/Users/eury/OneDrive - Environmental Defense Fund - edf.org/Wetland-Restoration-GHG/Paper 1/Data_sources")


##### Wetland extent (2020) and wetland loss layers
#' Data from:
#' Fluet-Chouinard, E. et al. (2023) ‘Extensive global wetland loss over the past three centuries’, Nature, 614(7947), pp. 281–286.
#' https://zenodo.org/records/7616651


wetlands2020 <- brick("wetland_loss_1700-2020_ensemblemean_v1.nc", varname="wetland_area")[[33]]

## Save output raster
writeRaster(wetlands2020, filename = "Extracted_datafiles/Wetland_area_2020.tif", format = "GTiff", overwrite = TRUE)  

## Wetland loss (1700-2020), by land use
forestry <- brick("wetland_loss_1700-2020_ensemblemean_v1.nc", varname="forestry")[[33]]
peatextr <- brick("wetland_loss_1700-2020_ensemblemean_v1.nc", varname="peatextr")[[33]]
ir_rice <- brick("wetland_loss_1700-2020_ensemblemean_v1.nc", varname="ir_rice")[[33]]
cropland <- brick("wetland_loss_1700-2020_ensemblemean_v1.nc", varname="cropland")[[33]]
wetcultiv <- brick("wetland_loss_1700-2020_ensemblemean_v1.nc", varname="wetcultiv")[[33]]
pasture <- brick("wetland_loss_1700-2020_ensemblemean_v1.nc", varname="pasture")[[33]]

## cropland, cultivated wetland and pasture are combined into "Agriculture"
agriculture <- cropland + wetcultiv + pasture


## wetland loss (1700-2010), by land use
forestry10 <- brick("wetland_loss_1700-2020_ensemblemean_v1.nc", varname="forestry")[[32]]
peatextr10 <- brick("wetland_loss_1700-2020_ensemblemean_v1.nc", varname="peatextr")[[32]]
ir_rice10 <- brick("wetland_loss_1700-2020_ensemblemean_v1.nc", varname="ir_rice")[[32]]
cropland10 <- brick("wetland_loss_1700-2020_ensemblemean_v1.nc", varname="cropland")[[32]]
wetcultiv10 <- brick("wetland_loss_1700-2020_ensemblemean_v1.nc", varname="wetcultiv")[[32]]
pasture10 <- brick("wetland_loss_1700-2020_ensemblemean_v1.nc", varname="pasture")[[32]]

agriculture10 <- cropland10 + wetcultiv10 + pasture10

## Recent wetland loss (2010-2020), by land use
agriculture_recent <- agriculture - agriculture10
forestry_recent <- forestry - forestry10
peatextr_recent <- peatextr - peatextr10
ir_rice_recent <- ir_rice - ir_rice10

## Save output rasters
writeRaster(agriculture, filename = "Extracted_datafiles/Wetland_loss_ag_1700-2020.tif", format = "GTiff", overwrite = TRUE)  
writeRaster(forestry, filename = "Extracted_datafiles/Wetland_loss_forestry_1700-2020.tif", format = "GTiff", overwrite = TRUE)  
writeRaster(peatextr, filename = "Extracted_datafiles/Wetland_loss_peatex_1700-2020.tif", format = "GTiff", overwrite = TRUE)  
writeRaster(ir_rice, filename = "Extracted_datafiles/Wetland_loss_rice_1700-2020.tif", format = "GTiff", overwrite = TRUE)  

writeRaster(agriculture_recent, filename = "Extracted_datafiles/Wetland_loss_ag_2010-2020.tif", format = "GTiff", overwrite = TRUE)  
writeRaster(forestry_recent, filename = "Extracted_datafiles/Wetland_loss_forestry_2010-2020.tif", format = "GTiff", overwrite = TRUE)  
writeRaster(peatextr_recent, filename = "Extracted_datafiles/Wetland_loss_peatex_2010-2020.tif", format = "GTiff", overwrite = TRUE)  
writeRaster(ir_rice_recent, filename = "Extracted_datafiles/Wetland_loss_rice_2010-2020.tif", format = "GTiff", overwrite = TRUE)  





##### Global peatland extent, PEAT-ML
#' Data from:
#' Melton, J. R., et al. (2022). A map of global peatland extent created using machine learning (Peat-ML). Geoscientific Model Development, 15(12), 4709–4738.
#' https://zenodo.org/records/7352284

Peat_percent <- raster("Peat-ML_global_peatland_extent.nc", varname="PEATLAND_P")

### Register the peatland map to Fluet-Chouinard wetland map
## Resample to the coarser resolution wetlands map,
## Then find the total area of peatlansd in the larger grid cells
peat_resample <- resample(Peat_percent, wetlands2020, method = 'bilinear')
a <- area(peat_resample)
peat_area <- overlay(peat_resample, a,
                      fun=function(r1, r2){return(r1*r2/100)})
## Area is in km2

### Write out peat area
writeRaster(peat_area, filename = "Extracted_datafiles/Peatland_area.tif", format = "GTiff", overwrite = TRUE)  






##### Climate zone maps
#' Data from:
#' Beck, H. E., et al.(2023). High-resolution (1 km) Köppen-Geiger maps for 1901–2099 based on constrained CMIP6 projections, Scientific Data.
#' https://doi.org/10.6084/m9.figshare.c.6395666.v1 
#' Climate zones for 1991-2020
#' 0.5 degree resolution

cz <- raster("koppen_geiger_0p5.tif") 

##### ORIGINAL CLASSIFICATION                   | color          | - ASSIGNED BIOME

# 1:  Af   Tropical, rainforest                  [0 0 255]         - tropical
# 2:  Am   Tropical, monsoon                     [0 120 255]       - tropical
# 3:  Aw   Tropical, savannah                    [70 170 250]      - tropical
# 4:  BWh  Arid, desert, hot                     [255 0 0]         - tropical
# 5:  BWk  Arid, desert, cold                    [255 150 150]     - temperate
# 6:  BSh  Arid, steppe, hot                     [245 165 0]       - tropical
# 7:  BSk  Arid, steppe, cold                    [255 220 100]     - temperate
# 8:  Csa  Temperate, dry summer, hot summer     [255 255 0]       - temperate
# 9:  Csb  Temperate, dry summer, warm summer    [200 200 0]       - temperate
# 10: Csc  Temperate, dry summer, cold summer    [150 150 0]       - temperate
# 11: Cwa  Temperate, dry winter, hot summer     [150 255 150]     - temperate
# 12: Cwb  Temperate, dry winter, warm summer    [100 200 100]     - temperate
# 13: Cwc  Temperate, dry winter, cold summer    [50 150 50]       - temperate
# 14: Cfa  Temperate, no dry season, hot summer  [200 255 80]      - temperate
# 15: Cfb  Temperate, no dry season, warm summer [100 255 80]      - temperate 
# 16: Cfc  Temperate, no dry season, cold summer [50 200 0]        - temperate
# 17: Dsa  Cold, dry summer, hot summer          [255 0 255]       - temperate
# 18: Dsb  Cold, dry summer, warm summer         [200 0 200]       - temperate
# 19: Dsc  Cold, dry summer, cold summer         [150 50 150]      - boreal
# 20: Dsd  Cold, dry summer, very cold winter    [150 100 150]     - boreal
# 21: Dwa  Cold, dry winter, hot summer          [170 175 255]     - temperate
# 22: Dwb  Cold, dry winter, warm summer         [90 120 220]      - temperate
# 23: Dwc  Cold, dry winter, cold summer         [75 80 180]       - boreal
# 24: Dwd  Cold, dry winter, very cold winter    [50 0 135]        - boreal
# 25: Dfa  Cold, no dry season, hot summer       [0 255 255]       - temperate
# 26: Dfb  Cold, no dry season, warm summer      [55 200 255]      - temperate
# 27: Dfc  Cold, no dry season, cold summer      [0 125 125]       - boreal
# 28: Dfd  Cold, no dry season, very cold winter [0 70 95]         - boreal
# 29: ET   Polar, tundra                         [178 178 178]     - boreal
# 30: EF   Polar, frost                          [102 102 102]     - boreal


## reclassifiication matrix
m <- c(0, 4, 3,     4, 5, 2,     5, 6, 3,     6, 18, 2,     18, 20, 1,   
       20, 22, 2,   22, 24,1,    24, 26, 2,   26, 30,1)
rcmat <- matrix(m, ncol = 3, byrow = TRUE)
cz_reclass <- reclassify(cz, rcmat)

writeRaster(cz_reclass, filename = "Extracted_datafiles/Koppen_CZ_classified.tif", format = "GTiff", overwrite = TRUE)  

### Register to Fluet-Chouinard wetland map
cz_resample <- resample(cz_reclass, wetlands2020, method = 'ngb')

## add a buffer around the cz raster to catch any coastal pixels
w <- matrix(1,3,3)
cz_buffer <- focal(cz_resample, w, modal, na.rm = TRUE, pad = FALSE, NAonly = TRUE)


writeRaster(cz_buffer, filename = "Extracted_datafiles/Koppen_CZ_buffer.tif", format = "GTiff", overwrite = TRUE)  




##### Global wetland methane emissions
#' Data from:
#' Zhang, Z., et al. (2017). Emerging role of wetland methane emissions in driving 21st century climate change. PNAS, 114(36), 9647–9652.
#' https://doi.org/10.1073/pnas.16187651141
#' Data provided by the authors
#' Years = 2020, 2099
#' Scenario = RCP 4.5
#' 0.5 degree resolution
#' Units g/m2/mo -> g/m2/yr


CH4.jan.2020 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[709]]
CH4.feb.2020 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[710]]
CH4.mar.2020 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[711]]
CH4.apr.2020 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[712]]
CH4.may.2020 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[713]]
CH4.jun.2020 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[714]]
CH4.jul.2020 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[715]]
CH4.aug.2020 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[716]]
CH4.sep.2020 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[717]]
CH4.oct.2020 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[718]]
CH4.nov.2020 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[719]]
CH4.dec.2020 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[720]]

CH4.2020 <- sum(CH4.jan.2020, CH4.feb.2020, CH4.mar.2020,
                CH4.apr.2020, CH4.may.2020, CH4.jun.2020,
                CH4.jul.2020, CH4.aug.2020, CH4.sep.2020, 
                CH4.oct.2020, CH4.nov.2020, CH4.dec.2020,
                na.rm = TRUE)

CH4.2020.resample <- resample(CH4.2020, wetlands2020)

writeRaster(CH4.2020.resample, filename = "Extracted_datafiles/rcp45_ch4_flux_annual_sum_2020.tif", format = "GTiff", overwrite = TRUE)  



CH4.jan.2099 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[1657]]
CH4.feb.2099 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[1658]]
CH4.mar.2099 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[1659]]
CH4.apr.2099 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[1660]]
CH4.may.2099 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[1661]]
CH4.jun.2099 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[1662]]
CH4.jul.2099 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[1663]]
CH4.aug.2099 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[1664]]
CH4.sep.2099 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[1665]]
CH4.oct.2099 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[1666]]
CH4.nov.2099 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[1667]]
CH4.dec.2099 <- brick("ensmean_ch4e_rcp45.nc", varname="ch4e")[[1668]]

CH4.2099 <- sum(CH4.jan.2099, CH4.feb.2099, CH4.mar.2099,
                CH4.apr.2099, CH4.may.2099, CH4.jun.2099,
                CH4.jul.2099, CH4.aug.2099, CH4.sep.2099, 
                CH4.oct.2099, CH4.nov.2099, CH4.dec.2099,
                na.rm = TRUE)

CH4.2099.resample <- resample(CH4.2099, wetlands2020)

writeRaster(CH4.2099.resample, filename = "Extracted_datafiles/rcp45_ch4_flux_annual_sum_2099.tif", format = "GTiff", overwrite = TRUE)  




##### Global Lakes and Rivers Database
#' Used for masking out permanent water bodies from the global flood risk map (below)
#' Lehner, B. and Döll, P. (2004): Development and validation of a global database of lakes, reservoirs and wetlands. Journal of Hydrology 296/1-4: 1-22.
#' https://www.worldwildlife.org/publications/global-lakes-and-wetlands-database-lakes-and-wetlands-grid-level-3
#' Level 3
#' 30 second resolution

#' Cell values:
#'  1. Lake
#'  2. Reservoir
#'  3. River
#'  4-12. Various wetland types

GSW <- raster("glwd_3/w001001.adf")

## filter to just 1. Lake, 2. Reservoir, & 3. River
## and reclassify all values to 1, so we are left with a mask for surface water
reclass_df <- c(0, 3.5, 1, 
                3.5, Inf, NA)
reclass_m <- matrix(reclass_df, ncol = 3, byrow = TRUE)
Surface_water_mask <- reclassify(GSW, reclass_m)




##### Global flood risk map
#' Dottori, Francesco; Alfieri, Lorenzo; Salamon, Peter; Bianchi, Alessandra; 
#' Feyen, Luc; Hirpa, Feyera (2016): Flood hazard map of the World - 100-year 
#' return period. European Commission, Joint Research Centre (JRC)
#' https://data.jrc.ec.europa.eu/dataset/jrc-floods-floodmapgl_rp100y-tif
#' 100 year flood return period
#' 30 arc second resolution

flood.risk <-  raster("floodMapGL_rp100y.tif")
plot(flood.risk)

swm_resample <- resample(Surface_water_mask, flood.risk)

### Mask flood risk to areas that are not already permanent surface water
flood.risk.mod <- overlay(
  flood.risk,
  swm_resample,
  fun = function(x, y) {
    x[!is.na(y[])] <- NA
    return(x)
  }
)

### Reclassify flood risk into presence/absence (flood area = 1)
# Then multiply by cell area to get area of flood risk and aggregate
# to 0.5 degree grid cell
reclass_df <- c(0, Inf, 1)
reclass_m <- matrix(reclass_df, ncol = 3, byrow = TRUE)
flood.risk.reclass <- reclassify(flood.risk.mod, reclass_m)
a <- area(flood.risk.mod)  ## calculate area of each grid cell (km2)
flood.risk.area <-overlay(flood.risk.reclass, a, fun=function(x,y){return(x*y)})

## resample to wetland area
res.factor <- res(wetlands2020)/res(flood.risk.area) 
flood.risk.aggregate <- aggregate(flood.risk.area, res.factor, fun = "sum", na.rm = TRUE)
flood.risk.resample <- resample(flood.risk.aggregate, wetlands2020, method = 'bilinear')

## write out final raster
writeRaster(flood.risk.resample, filename = "Extracted_datafiles/flood_risk_map.tif", format = "GTiff", overwrite = TRUE)  



##### Global N fertilizer use
#' Potter, P., et al. (2010). Characterizing the Spatial Patterns of Global Fertilizer Application and Manure Production. Earth Interactions, 14(2), 1–22.
#' Potter, P., et al. (2012). Global Fertilizer and Manure, Version 1: Nitrogen Fertilizer Application [dataset]. https://doi.org/ https://doi.org/10.7927/H4Q81B0R
#' https://sedac.ciesin.columbia.edu/data/set/ferman-v1-nitrogen-fertilizer-application
#' Resolution: 0.5 degree
#' Unit: kg/ha


fert <- raster("nfertilizer_global.tif")
fert.resample <- resample(fert, wetlands2020)
writeRaster(fert.resample, filename = "Extracted_datafiles/global_N.tif", format = "GTiff", overwrite = TRUE)









#### Combine all data inputs to a single raster stack and data frame

cell_area <- area(wetlands2020)
full.stack <- stack(cell_area, wetlands2020, peat_area, cz_buffer, agriculture, agriculture_recent, 
                    forestry, forestry_recent, peatextr, peatextr_recent,
                    ir_rice, ir_rice_recent, CH4.2020.resample, CH4.2099.resample,
                    flood.risk.resample, fert.resample)

names(full.stack)
names(full.stack) <- c("cell_area", "WA2020", "peatlands", "biomes", "WL_ag", "WL_ag_re", 
                    "WL_for", "WL_for_re", "WL_peatx", "WL_peatx_re", 
                    "WL_rice", "WL_rice_re","CH4_2020", "CH4_2099",
                    "flood", "N_fert")

writeRaster(full.stack, filename = "Extracted_datafiles/allData.tif", format = "GTiff", overwrite = TRUE)

### write results as data.frame

options(scipen = 99)
results <- as.data.frame(full.stack, xy = TRUE)
write.csv(results, "Extracted_datafiles/all_Data.csv", row.names = FALSE)













