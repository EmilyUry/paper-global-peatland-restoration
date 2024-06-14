


### Restoration scenarios

#' Create Monte Carlo output (500 iterations) of emissions estimates for
#' drained peatland area under the following conditions
#' 
#' Restore to intact:
#'  1. All drained 1700-2020, SGWP20, CH4 2020
#'  2. All drained 1700-2020, SGWP100, CH4 2020
#'  3. All drained 1700-2020, SGWP20, CH4 2099
#'  4. Only drained post-2010, SGWP20, CH4 2020
#' 5. Rewet (All drained 1700-2020, SGWP20, CH42020)  
#' 
#' For each scenario, emission from drained wetlands are subtracted from the endpoint
#' such that final results represent net emissions outcome following the land use change


setwd("C:/Users/eury/OneDrive - Environmental Defense Fund - edf.org/Wetland-Restoration-GHG/Paper 1")

library(tidyverse)
library(raster)

## Data set-up
EF <- read.csv("Data_sources/Emission_Factors/EF_SD_Summary2.csv")
input <- read.csv("Data_sources/Extracted_datafiles/all_Data.csv")

input$CZ <- as.factor(input$biomes)

## adjust wetland area based on peatland area -- sets cells with very small 
#### wetland/peatland area to zero to avoid inflated CH4 emissions estimates
input$new.wetland.area <- ifelse(input$WA2020 < input$peatlands, input$peatlands, input$WA2020)
input$new.wetland.area <- ifelse(input$WA2020 == 0, 0, input$new.wetland.area)

## convert methane (from g/m2) to kg/km2 wetland using new wetland area
## Methane 2020
CH4_gridcell_2020 <- input$CH4_2020*input$cell_area  ## Mg/gridcell
## check:
sum(CH4_gridcell_2020, na.rm= TRUE)/10^6 ## Global annual emissions in Tg (About 206 is correct)
input$CH4_Zhang_2020 <- CH4_gridcell_2020/input$new.wetland.area*1000    ## kg/km2
input$CH4_Zhang_2020 <- replace(input$CH4_Zhang_2020, is.na(input$CH4_Zhang_2020), 0)

## Find mean and standard deviation of Zhang methane for each climate zone:
Intact.CH4 <- input %>%
  filter(new.wetland.area > 0) %>%
  group_by(CZ) %>%
  summarise(mean_CH4 = mean(CH4_Zhang_2020, na.rm = TRUE),
            sd_CH4 = sd(CH4_Zhang_2020, na.rm = TRUE))
Intact.CH4
## Boreal:    2364 (5397)
## Temperate: 7112 (28163)
## Tropical:  15720 (38993)


## Methane 2099
CH4_gridcell_2099 <- input$CH4_2099*input$cell_area  ## Mg/gridcell
sum(CH4_gridcell_2099, na.rm= TRUE)/10^6 ## Global annual emissions in Tg (About 267 is correct)
input$CH4_Zhang_2099 <- CH4_gridcell_2099/input$new.wetland.area*1000    ## kg/km2
input$CH4_Zhang_2099 <- replace(input$CH4_Zhang_2099, is.na(input$CH4_Zhang_2099), 0)

## convert wetland loss areas to areas of peatland loss
input$peat.frac <- input$peatlands/input$new.wetland.area

input$PL_ag <- input$WL_ag*input$peat.frac
input$PL_ag_re <- input$WL_ag_re*input$peat.frac
input$PL_for <- input$WL_for*input$peat.frac
input$PL_for_re <- input$WL_for_re*input$peat.frac
input$PL_peatx <- input$WL_peatx*input$peat.frac
input$PL_peatx_re <- input$WL_peatx_re*input$peat.frac
input$PL_rice <- input$WL_rice*input$peat.frac
input$PL_rice_re <- input$WL_rice_re*input$peat.frac


## calculate total area of peatland loss
input$peatland_loss <- input$PL_ag + input$PL_for +
  input$PL_peatx + input$PL_rice

input$peatland_loss_2010 <- input$PL_ag_re + input$PL_for_re +
  input$PL_peatx_re + input$PL_rice_re


## Filter to just areas of peatland los
filter <- input[!is.na(input$peatland_loss),]
filter <- filter[filter$peatland_loss > 0.0001,] ## removes pixels with ridiculously small peatland loss or no peatland loss

### mask out coastal pixels without methane emissions estimates
filter <- filter[filter$CH4_Zhang_2020 > 0,] 


rm(CH4_gridcell_2020); rm(CH4_gridcell_2099); rm(Intact.CH4)



### Perform Monte Carlo simulations to draw emissions factors from a distribution 
# based on the mean and standard deviation
# all emissions factors are in kg/km2/year

MC_SGWP20 <- data.frame(matrix(NA, ncol = 1, nrow = nrow(filter)))[-1]
MC_SGWP100 <- data.frame(matrix(NA, ncol = 1, nrow = nrow(filter)))[-1]
MC_2099 <- data.frame(matrix(NA, ncol = 1, nrow = nrow(filter)))[-1]
MC_2010 <- data.frame(matrix(NA, ncol = 1, nrow = nrow(filter)))[-1]
MC_Rewet <- data.frame(matrix(NA, ncol = 1, nrow = nrow(filter)))[-1]


for(i in 1:500) {
  
 ## Generate emission factor for each grid cell based on draw from distribution
  
 MC_EF <- EF %>%
    dplyr::select(CZ, LU, GHG, EF_mean, SD) %>%
    rowwise() %>%
    mutate(rEF = rnorm(1, mean = EF_mean, sd = SD)) %>%  ## generates random EF
    dplyr::select(-c(EF_mean, SD)) %>%
    pivot_wider(names_from = c(GHG, LU), values_from = rEF)
 MC_EF$CZ <- as.factor(MC_EF$CZ)
 MC_EF[is.na(MC_EF)] <- 0
  
  ## Join wetland area layer with EF later  
  data <- filter %>%
    left_join(MC_EF, by = "CZ")
  rm(MC_EF)
  
  ### FOR METHANE from intact wetlands (Zhang) 2020
  CH4 <- data$CH4_Zhang_2020
  methane <- data.frame(CH4, CH4*0.20)  ## 20% of the observation is used as the SD
  names(methane) <- c("OGmethane", "sd")
  methane <- methane %>%
    rowwise() %>%
    mutate(r.methane = rnorm(1, mean = OGmethane, sd = sd))  ## draw emission value from distribution
  methane$r.methane[is.nan(methane$r.methane)]<-NA
  methane$r.methane[is.na(methane$r.methane)]<-0
  
  data$CH4_Wetland <- methane$r.methane     ### the spatial explicit value with random error thrown in
  
  
  ### FOR METHANE from intact wetlands (Zhang) 2099
  CH4 <- data$CH4_Zhang_2099
  methane <- data.frame(CH4, CH4*0.20)  
  names(methane) <- c("OGmethane", "sd")
  methane <- methane %>%
    rowwise() %>%
    mutate(r.methane = rnorm(1, mean = OGmethane, sd = sd))
  methane$r.methane[is.nan(methane$r.methane)]<-NA
  methane$r.methane[is.na(methane$r.methane)]<-0
  
  data$CH4_Wetland_2099 <- methane$r.methane
  
  
  
  #### MAIN ANALYSIS

  ### CLACULATE CO2/CH4/N2O FLUX DRAINED PEATLANDS (DP) (Kg/grid cell)
  data$DP_CO2 <- data$PL_ag*data$CO2_Ag + data$PL_for*data$CO2_Forest +
    data$PL_peatx*data$CO2_Peatx + data$PL_rice*data$CO2_Rice
  data$DP_CH4 <- data$PL_ag*data$CH4_Ag + data$PL_for*data$CH4_Forest +
    data$PL_peatx*data$CH4_Peatx + data$PL_rice*data$CH4_Rice
  data$DP_N2O <- data$PL_ag*data$N2O_Ag + data$PL_for*data$N2O_Forest +
    data$PL_peatx*data$N2O_Peatx + data$PL_rice*data$N2O_Rice
  
  
  
  # 1.  All drained 1700-2020, SGWP20, CH4 2020

    ### CONVERT GHG FLUXES INTO GWP (use sustained GWP20)
  data$GWP20_intact_norm <- data$CO2_Wetland + 96*data$CH4_Wetland + 250*data$N2O_Wetland     # RATE (kg CO2e/km2/yr)
  data$GWP20_drained <- data$DP_CO2 + 96*data$DP_CH4 + 250*data$DP_N2O # FLUX (kg CO2e/yr)
  
  ### RESTORATION SCENARIO  - end point 1, restore to fully intact status
  data$Restore20 <- data$GWP20_intact_norm*data$peatland_loss                    # (kg CO2e/yr)
  
  ### NET EFFECT OF RESTORATION (subtract fluxes from drained land use)
  data$Restore20_net <- data$Restore20 - data$GWP20_drained                # (kg CO2e/yr)
  
  
  MC_SGWP20[,i] <- data$Restore20_net                      # (kg CO2e/yr)
  
 
  
  #'  2. All drained 1700-2020, SGWP100, CH4 2020

  ### CONVERT GHG FLUXES INTO GWP (use sustained GWP100)
  data$GWP100_intact_norm <- data$CO2_Wetland + 45*data$CH4_Wetland + 270*data$N2O_Wetland     # RATE (kg CO2e/km2/yr)
  data$GWP100_drained <- data$DP_CO2 + 45*data$DP_CH4 + 270*data$DP_N2O # FLUX (kg CO2e/yr)
  
  ### RESTORATION SCENARIO  - end member 1, restore to fully intact status
  data$Restore100 <- data$GWP100_intact_norm*data$peatland_loss           # (kg CO2e/yr)
  
  ### NET EFFECT OF REWETTING (subtract fluxes from drained land use)
  data$Restore100_net <- data$Restore100 - data$GWP100_drained         # (kg CO2e/yr)
  
  
  MC_SGWP100[,i] <- data$Restore100_net              # (kg CO2e/yr)
  

  
  
    #'  3. All drained 1700-2020, SGWP20, CH4 2099

  ### CONVERT GHG FLUXES INTO GWP (use sustained GWP20)
  data$GWP20_intact_norm_2099 <- data$CO2_Wetland + 96*data$CH4_Wetland_2099 + 250*data$N2O_Wetland     # RATE (kg CO2e/km2/yr)
  data$GWP20_drained <- data$DP_CO2 + 96*data$DP_CH4 + 250*data$DP_N2O # FLUX (kg CO2e/yr)
  
  ### RESTORATION SCENARIO  - end point 1, restore to fully intact status
  data$Restore_2099 <- data$GWP20_intact_norm_2099*data$peatland_loss                    # (kg CO2e/yr)
  
  ### NET EFFECT OF RESTORATION (subtract fluxes from drained land use)
  data$Restore2099_net <- data$Restore_2099 - data$GWP20_drained                # (kg CO2e/yr)
  
  
  MC_2099[,i] <- data$Restore2099_net                      # (kg CO2e/yr)
  
  
  
  #'  4. Only drained post-2010, SGWP20, CH4 2020

  ### CLACULATE CO2/CH4/N2O FLUX DRAINED PEATLANDS (DP) (Kg/grid cell)
  data$DP_re_CO2 <- data$PL_ag_re*data$CO2_Ag + data$PL_for_re*data$CO2_Forest +
    data$PL_peatx_re*data$CO2_Peatx + data$PL_rice_re*data$CO2_Rice
  data$DP_re_CH4 <- data$PL_ag_re*data$CH4_Ag + data$PL_for_re*data$CH4_Forest +
    data$PL_peatx_re*data$CH4_Peatx + data$PL_rice_re*data$CH4_Rice
  data$DP_re_N2O <- data$PL_ag_re*data$N2O_Ag + data$PL_for_re*data$N2O_Forest +
    data$PL_peatx_re*data$N2O_Peatx + data$PL_rice_re*data$N2O_Rice
  
  ### CONVERT GHG FLUXES INTO GWP (use sustained GWP20)
  data$GWP20_intact_norm <- data$CO2_Wetland + 96*data$CH4_Wetland + 250*data$N2O_Wetland     # RATE (kg CO2e/km2/yr)
  data$GWP20_drained_recent <- data$DP_re_CO2 + 96*data$DP_re_CH4 + 250*data$DP_re_N2O # FLUX (kg CO2e/yr)
  
  ### RESTORATION SCENARIO  - end point 1, restore to fully intact status
  data$Restore20_recent <- data$GWP20_intact_norm*data$peatland_loss_2010     # (kg CO2e/yr)
  
  ### NET EFFECT OF RESTORATION (subtract fluxes from drained land use)
  data$Restore20_recent_net <- data$Restore20_recent - data$GWP20_drained_recent     # (kg CO2e/yr)
  
  
  MC_2010[,i] <- data$Restore20_recent_net                      # (kg CO2e/yr)
  
  
  
  #' 5. Rewet (All drained 1700-2020, SGWP20, CH42020)
  
  ### CONVERT GHG FLUXES INTO GWP (use sustained GWP20)
  data$GWP20_rewet_norm <- data$CO2_Rewetted + 96*data$CH4_Rewetted + 250*data$N2O_Rewetted     # RATE (kg CO2e/km2/yr)
  data$GWP20_drained <- data$DP_CO2 + 96*data$DP_CH4 + 250*data$DP_N2O # FLUX (kg CO2e/yr)
  
  ### RESTORATION SCENARIO  - end point 1, restore to fully intact status
  data$Rewet20 <- data$GWP20_rewet_norm*data$peatland_loss                    # (kg CO2e/yr)
  
  ### NET EFFECT OF RESTORATION (subtract fluxes from drained land use)
  data$Rewet20_net <- data$Rewet20 - data$GWP20_drained                # (kg CO2e/yr)
  
  
  MC_Rewet[,i] <- data$Rewet20_net                      # (kg CO2e/yr)
  
  
  
  print(Sys.time())
  print(i)
}
  


### write out filter data input file and all 5 Monte Carlo Outputs for use in figure making
write.csv(data, "Data_sources/Extracted_datafiles/Data_filtered2.csv", row.names=FALSE)


write.csv(MC_SGWP20, "Data_sources/Extracted_datafiles/MC_SGWP20_2.csv", row.names=FALSE)
write.csv(MC_SGWP100, "Data_sources/Extracted_datafiles/MC_SGWP100_2.csv", row.names = FALSE)
write.csv(MC_2099, "Data_sources/Extracted_datafiles/MC_2099_2.csv", row.names = FALSE)
write.csv(MC_2010, "Data_sources/Extracted_datafiles/MC_2010_2.csv", row.names = FALSE)
write.csv(MC_Rewet, "Data_sources/Extracted_datafiles/MC_Rewet_2.csv", row.names = FALSE)









