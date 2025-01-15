

library(tidyverse)
library(sf)
library(raster)
library(cowplot)


setwd("C:/Users/eury/OneDrive - Environmental Defense Fund - edf.org/Wetland-Restoration-GHG/Paper 1")



##### EXTENDED DATA FIGURE 1

## data setup:
{
  coasts <- st_read("Data_sources/Coastline/ne_110m_coastline.shp", quiet = TRUE)
  
  EF <- read.csv("Data_sources/Emission_Factors/EF_SD_Summary2.csv")
  input <- read.csv("Data_sources/Extracted_datafiles/all_Data.csv")
  
  input$CZ <- as.factor(input$biomes)
  
  ## adjust wetland area based on peatland area 
  input$new.wetland.area <- ifelse(input$WA2020 < input$peatlands, input$peatlands, input$WA2020)
  input <- input[input$new.wetland.area > 0,]
  input <- input[!is.na(input$new.wetland.area),]
  
  ## convert methane (from g/m2) to kg/km2 wetland using new wetland area
  ## Methane 2020
  CH4_gridcell_2020 <- input$CH4_2020*input$cell_area  ## Mg/grid cell
  input$CH4_gridcell_2020 <- input$CH4_2020*input$cell_area  ## Mg/grid cell
  
  ## check:
  sum(CH4_gridcell_2020, na.rm= TRUE)/10^6 ## Global annual emissions in Tg (About 206 is correct)
  input$CH4_Zhang_2020 <- CH4_gridcell_2020/input$new.wetland.area*1000    ## kg/km2
  summary(input$CH4_Zhang_2020)
  
  # ## problem cells - low peatland, low wetlands, mainly in northern africa ## 468 cell total, 0.45 km2 wetlands
  # check <- input[input$CH4_Zhang_2020 > 10 & input$peatlands > input$WA2020 & input$new.wetland.area < 1,]
  # plot(check$x, check$y)
  # sum(check$peatlands)
  # sum(check$WA2020)
  
  ## filter out problem cells with very low peatland/wetlands causing outlier emissions
  input <- input[input$new.wetland.area > 1 | input$CH4_Zhang_2020 < 10 | input$peatlands < input$WA2020,]
  
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
  CH4_gridcell_2099<- input$CH4_2099*input$cell_area  ## Mg/gridcell
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
  
  ### mask out coastal pixels without methane emissions estimates
  filter <- input[input$CH4_Zhang_2020 > 0,] 
  
  rm(CH4_gridcell_2020); rm(CH4_gridcell_2099); rm(Intact.CH4)
  
  ## Generate emission factor for each grid cell based on draw from distribution
  
  MC_EF <- EF %>%
    dplyr::select(CZ, LU, GHG, EF_mean) %>%
    pivot_wider(names_from = c(GHG, LU), values_from = EF_mean)
  MC_EF$CZ <- as.factor(MC_EF$CZ)
  MC_EF[is.na(MC_EF)] <- 0
  
  ## Join wetland area layer with EF later  
  data <- filter %>%
    left_join(MC_EF, by = "CZ")
  rm(MC_EF)
  
  data$CH4_Wetland <- data$CH4_Zhang_2020   
  
}



######### INTACT -- PEATLANDS -- (do not run this section out of order)
{ ### Intact -- peatlands
  
  
  #CO2
  summary(data$CO2_Wetland*data$peatlands/10^6)  #Gg CO2eq/grid cell
  data$cuts <- cut(data$CO2_Wetland*data$peatlands/10^6, breaks = c(-Inf, -100, -10, -1, -0.0001, 0))
  table(data$cuts)
  
  pal <- c( "#2166ac", "#4393c3",  '#92c5de', "#d1e5f0", "#e7e7e7")
  
  ## Filter to just areas of more than 1% peatland (past or present) ### for mapping only
  mask <- data[data$peatlands > 28.8 | data$peatland_loss > 28.8,]
  
  IW_CO2 <- ggplot() +
    geom_tile(data = mask, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab("\nIntact") +
    ggtitle(bquote("CO"[2])) +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "plain"), 
          axis.title = element_text(size = 10, face = 'plain')) +
    annotate("label", x = -50, y = -80, size = 2.5, label = "Temmink et al. 2023")
  
  
  #CH4
  summary(data$CH4_Wetland*data$peatlands/10^6*96)
  data$cuts <- cut(data$CH4_Wetland*data$peatlands/10^6*96, breaks = c(0, 0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c( "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d", "#b2182b", '#67001f')
  mask <- data[data$peatlands > 28.8 | data$peatland_loss > 28.8,]
  
  IW_CH4 <- ggplot() +
    geom_tile(data = mask, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab(" ") +
    ggtitle(bquote("CH"[4])) +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold')) +
    annotate("label", x = -60, y = -80, size = 2.5, label = "Zhang et al. 2017")
  
  #N2O
  summary(data$N2O_Wetland*data$peatlands/10^6*250)
  data$cuts <- cut(data$N2O_Wetland*data$peatlands/10^6*250, breaks = c(-100, -10, -1, -0.0001, 0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c('#92c5de', "#d1e5f0", "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d",  "#b2182b")
  mask <- data[data$peatlands > 28.8 | data$peatland_loss > 28.8,]
  
  IW_N2O <- ggplot() +
    geom_tile(data = mask, aes(x= x, y = y, fill = cuts)) +
    xlab(" ") +
    ylab(" ") +    
    ggtitle(bquote("N"[2]*"O")) +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold')) +
    annotate("label", x = -50, y = -80, size = 2.5, label = "Bahram et al. 2022")
  
  
  
  data$INTACT_GWP20 <- data$peatlands*(data$CO2_Wetland + data$CH4_Wetland*96 + data$N2O_Wetland*250)  #(Kg CO2e/grid cell)
  sum(data$CO2_Wetland)*1000 #g/yr
  sum(data$CH4_Wetland)*1000 #g/yr
  sum(data$N2O_Wetland)*1000 #g/yr
  sum(data$INTACT_GWP20)/10^12
  
  summary(data$INTACT_GWP20/10^6)
  
  subset.negative <- data[data$INTACT_GWP20 < 0,]
  pp.sink <- sum(subset.negative$peatland_loss, na.rm = TRUE)/sum(data$peatland_loss, na.rm = TRUE)*100
  subset.positive <- data[data$INTACT_GWP20 > 0,]
  pp.source <- sum(subset.positive$peatland_loss, na.rm = TRUE)/sum(data$peatland_loss, na.rm = TRUE)*100
  
  
  
  data$cuts <- cut(data$INTACT_GWP20/10^6, breaks = c(-Inf, -100, -10, -1, -0.0001, 
                                                      0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c("#2166ac", "#4393c3", '#92c5de', "#d1e5f0",  "#e7e7e7", "#fddbc7", '#f4a582', "#d6604d", "#b2182b", '#67001f')
  
  mask <- data[data$peatlands > 28.8 | data$peatland_loss > 28.8,]
  
  IW_GWP20 <- ggplot() +
    geom_tile(data = mask, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab(" ") +
    ggtitle("Total (SGWP20)") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold'))
  
  
}



######### DRAINED -- PEATLANDS  
{
  
  ### masked to areas of wetlands loss only 
  data <- data[!is.na(data$peatland_loss),]
  data <- data[data$peatland_loss > 0,]
  
  
  ### CLACULATE CO2/CH4/N2O FLUX DRAINED PEATLANDS (DP) (Kg/grid cell)
  data$DP_CO2 <- data$PL_ag*data$CO2_Ag + data$PL_for*data$CO2_Forest +
    data$PL_peatx*data$CO2_Peatx + data$PL_rice*data$CO2_Rice
  data$DP_CH4 <- data$PL_ag*data$CH4_Ag + data$PL_for*data$CH4_Forest +
    data$PL_peatx*data$CH4_Peatx + data$PL_rice*data$CH4_Rice
  data$DP_N2O <- data$PL_ag*data$N2O_Ag + data$PL_for*data$N2O_Forest +
    data$PL_peatx*data$N2O_Peatx + data$PL_rice*data$N2O_Rice
  
  #CO2
  summary(data$DP_CO2/10^6)
  data$cuts <- cut(data$DP_CO2/10^6, breaks = c(0, 0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c( "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d", "#b2182b", '#67001f')
  
  ## Filter to just areas of more than 1% peatland (past or present) ### for mapping only
  mask <- data[data$peatlands > 28.8 | data$peatland_loss > 28.8,]
  
  DW_CO2 <- ggplot() +
    geom_tile(data = mask, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab("\nDrained") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "plain"), 
          axis.title = element_text(size = 10, face = 'plain')) +
    annotate("label", x = -80, y = -80, size = 2.5, label = "IPCC 2014")
  
  #CH4
  summary(data$DP_CH4/10^6*96)
  data$cuts <- cut(data$DP_CH4/10^6*96, breaks = c(0, 0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c( "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d", "#b2182b", '#67001f')
  
  mask <- data[data$peatlands > 28.8 | data$peatland_loss > 28.8,]
  
  DW_CH4 <- ggplot() +
    geom_tile(data = mask, aes(x= x, y = y, fill = cuts)) +
    xlab(" ") +
    ylab(" ") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold')) +
    annotate("label", x = -80, y = -80, size = 2.5, label = "IPCC 2014")
  
  
  #N2O
  summary(data$DP_N2O/10^6*250)
  data$cuts <- cut(data$DP_N2O/10^6*250, breaks = c(-0.0001, 0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c("#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d", "#b2182b", '#67001f')
  
  mask <- data[data$peatlands > 28.8 | data$peatland_loss > 28.8,]
  
  DW_N2O <- ggplot() +
    geom_tile(data = mask, aes(x= x, y = y, fill = cuts)) +
    xlab(" ") +
    ylab(" ") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold')) +
    annotate("label", x = -80, y = -80, size = 2.5, label = "IPCC 2014")
  
  
  #GWP
  #### sum of all three gases
  data$DRAINED_GWP20 <- data$DP_CO2 + data$DP_CH4*96 + data$DP_N2O*250 #(Kg CO2e/grid cell)
  sum(data$DP_CO2)*1000 #g/yr
  sum(data$DP_CH4)*1000 #g/yr
  sum(data$DP_N2O)*1000 #g/yr
  sum(data$DRAINED_GWP20)/10^12
  
  summary(data$DRAINED_GWP20/10^6)
  data$cuts <- cut(data$DRAINED_GWP20/10^6, breaks = c(0, 0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c( "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d", "#b2182b", '#67001f')
  
  mask <- data[data$peatlands > 28.8 | data$peatland_loss > 28.8,]
  
  DW_GWP20 <- ggplot() +
    geom_tile(data = mask, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab(" ") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "plain"), 
          axis.title = element_text(size = 10, face = 'bold')) 
  
}  



######## REWETTED -- PEATLANDS 
{
  ## These are areas of drained peatlands if hypothetically changed to
  ## recently rewetted peatlands
  
  ### masked to areas of wetlands loss only 
  data <- data[!is.na(data$peatland_loss),]
  data <- data[data$peatland_loss > 0,]
  
  # CO2  
  summary(data$CO2_Rewetted*data$peatland_loss/10^6)
  data$cuts <- cut(data$CO2_Rewetted*data$peatland_loss/10^6, breaks = c(-Inf, -100, -10, -1, -0.0001, 
                                                                         0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c("#2166ac", "#4393c3", '#92c5de', "#d1e5f0",  "#e7e7e7", "#fddbc7", '#f4a582', "#d6604d", "#b2182b", '#67001f')
  
  mask <- data[data$peatlands > 28.8 | data$peatland_loss > 28.8,]
  RW_CO2 <- ggplot() +
    geom_tile(data = mask, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab("'Recently\nRewetted'") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "plain"), 
          axis.title = element_text(size = 10, face = 'plain')) +
    annotate("label", x = -80, y = -80, size = 2.5, label = "IPCC 2014")
  
  #CH4
  summary(data$CH4_Rewetted*data$peatland_loss/10^6*96)
  data$cuts <- cut(data$CH4_Rewetted*data$peatland_loss/10^6*96, breaks = c(0, 0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c( "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d", "#b2182b", '#67001f')
  
  mask <- data[data$peatlands > 28.8 | data$peatland_loss > 28.8,]
  RW_CH4 <- ggplot() +
    geom_tile(data = mask, aes(x= x, y = y, fill = cuts)) +
    xlab(" ") +
    ylab(" ") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold')) +
    annotate("label", x = -80, y = -80, size = 2.5, label = "IPCC 2014")
  
  #N2O
  summary(data$N2O_Rewetted*data$peatland_loss/10^6*250)
  data$cuts <- cut(data$N2O_Rewetted*data$peatland_loss/10^6*250, breaks = c(-Inf, -0.001, 0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c("#e7e7e7", "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d")
  
  #### ALL ZEROS
  mask <- data[data$peatlands > 28.8 | data$peatland_loss > 28.8,]
  RW_N2O <- ggplot() +
    geom_tile(data = mask, aes(x= x, y = y, fill = cuts)) +
    xlab(" ") +
    ylab(" ") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold')) +
    annotate("label", x = -80, y = -80, size = 2.5, label = "IPCC 2014")
  
  #GWP
  data$REWET_GWP20 <- data$peatland_loss*(data$CO2_Rewetted + data$CH4_Rewetted*96 + data$N2O_Rewetted*250)  #(Kg CO2e/grid cell)
  sum(data$REWET_GWP20)/10^12
  
  summary(data$REWET_GWP20/10^6)
  data$cuts <- cut(data$REWET_GWP20/10^6, breaks = c(-0.0001, 
                                                     0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c( "#e7e7e7", "#fddbc7", '#f4a582', "#d6604d", "#b2182b", '#67001f')
  
  mask <- data[data$peatlands > 28.8 | data$peatland_loss > 28.8,]
  RW_GWP20 <- ggplot() +
    geom_tile(data = mask, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab(" ") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold'))
  
  sum(data$REWET_GWP20 - data$DRAINED_GWP20)/10^12
  
  
}



######## RESTORED --- PEATLANDS 
{
  ## These are areas of drained peatlands if hypothetically restored to 
  ## intact conditions (ie. using the emissions estimates from intact peatlands 
  ## in the same grid cell)
  
  ### masked to areas of wetlands loss only 
  data <- data[!is.na(data$peatland_loss),]
  data <- data[data$peatland_loss > 0,]
  
  #CO2
  summary(data$CO2_Wetland*data$peatland_loss/10^6)  #Gg CO2eq/grid cell
  data$cuts <- cut(data$CO2_Wetland*data$peatland_loss/10^6, breaks = c(-Inf, -100, -10, -1, -0.0001, 0))
  table(data$cuts)
  
  pal <- c( "#2166ac", "#4393c3",  '#92c5de', "#d1e5f0", "#e7e7e7")
  
  mask <- data[data$peatlands > 28.8 | data$peatland_loss > 28.8,]
  RS_CO2 <- ggplot() +
    geom_tile(data = mask, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab("'Restored\nto Intact'") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "plain"), 
          axis.title = element_text(size = 10, face = 'plain')) +
    annotate("label", x = -50, y = -80, size = 2.5, label = "Temmink et al. 2023")
  
  #CH4
  summary(data$CH4_Wetland*data$peatland_loss/10^6*96)
  data$cuts <- cut(data$CH4_Wetland*data$peatland_loss/10^6*96, breaks = c(0, 0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c( "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d", "#b2182b", '#67001f')
  
  mask <- data[data$peatlands > 28.8 | data$peatland_loss > 28.8,]
  RS_CH4 <- ggplot() +
    geom_tile(data = mask, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab(" ") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold')) +
    annotate("label", x = -60, y = -80, size = 2.5, label = "Zhang et al. 2017")
  
  #N2O
  summary(data$N2O_Wetland*data$peatland_loss/10^6*250)
  data$cuts <- cut(data$N2O_Wetland*data$peatland_loss/10^6*250, breaks = c(-100, -10, -1, -0.0001, 0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c('#92c5de', "#d1e5f0", "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d",  "#b2182b")
  
  mask <- data[data$peatlands > 28.8 | data$peatland_loss > 28.8,]
  RS_N2O <- ggplot() +
    geom_tile(data = mask, aes(x= x, y = y, fill = cuts)) +
    xlab(" ") +
    ylab(" ") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold')) +
    annotate("label", x = -50, y = -80, size = 2.5, label = "Bahram et al. 2022")
  
  #GWP
  data$RESTORE_GWP20 <- data$peatland_loss*(data$CO2_Wetland + data$CH4_Wetland*96 + data$N2O_Wetland*250)  #(Kg CO2e/grid cell)
  sum(data$CO2_Wetland)*1000 #g/yr
  sum(data$CH4_Wetland)*1000 #g/yr
  sum(data$N2O_Wetland)*1000 #g/yr
  sum(data$RESTORE_GWP20)/10^12
  
  data$cuts <- cut(data$RESTORE_GWP20/10^6, breaks = c(-Inf, -100, -10, -1, -0.0001, 
                                                       0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c("#2166ac", "#4393c3", '#92c5de', "#d1e5f0",  "#e7e7e7", "#fddbc7", '#f4a582', "#d6604d", "#b2182b", '#67001f')
  
  mask <- data[data$peatlands > 28.8 | data$peatland_loss > 28.8,]
  RS_GWP20 <- ggplot() +
    geom_tile(data = mask, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab(" ") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold'))
  
  
  sum(data$RESTORE_GWP20 - data$DRAINED_GWP20)/10^12
  sum(data$RESTORE_GWP20)/10^12 - sum(data$DRAINED_GWP20)/10^12
  
  summary(data$RESTORE_GWP20/10^6)
  
  subset.negative <- data[data$RESTORE_GWP20 < 0,]
  pp.sink <- sum(subset.negative$peatland_loss)/sum(data$peatland_loss)*100
  subset.positive <- data[data$RESTORE_GWP20 > 0,]
  pp.source <- sum(subset.positive$peatland_loss)/sum(data$peatland_loss)*100
  
}


######## Supporting Information --- Figure 1 all put together
{  
  
  top <- plot_grid(IW_CO2, IW_CH4, IW_N2O, IW_GWP20,
                   DW_CO2, DW_CH4, DW_N2O, DW_GWP20,
                   RW_CO2, RW_CH4, RW_N2O, RW_GWP20,
                   RS_CO2, RS_CH4, RS_N2O, RS_GWP20,
                   labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
                              "k", "l", "m", "n", "o", "p"),
                   label_y = 1.02,
                   label_size = 11, ncol = 4, rel_heights = c (1.2, 1, 1, 1))
  
  
  legend.plot <- ggplot() +
    geom_tile(data = data, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab(" ") +
    theme_bw(base_size = 9) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000",
                      labels = c("< -100", "-100 to -10", "-10 to -1", "-1 to -0.0001", "-0.0001 to 0.001",
                                 "0.001 to 1", "1 to 10", "10 to 100", "100-1000", ">1000"))+
    theme(legend.position = c(0.5,0.5), legend.direction="horizontal",
          legend.title = element_blank(), 
          legend.key.size = unit(0.14, "in"),
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold')) +
    guides(fill = guide_legend(ncol = 10))
  
  legend <- cowplot::get_legend(legend.plot)
  
  plot_grid(top, legend, ncol = 1, rel_heights = c(12,1), 
            labels = c(" ", "GHG emissions (Gg CO2eq. per grid cell)"), 
            label_size = 9, label_x = 0.2, label_y = 1.3)
  
  
  ggsave(filename = "Figures/All_inputs_grid2.png",
         plot = last_plot(), bg = "white",
         width = 8, 
         height = 4.8, 
         unit = "in",
         dpi = 300)
  
}










##### FIGURE 1 - NET EMISSIONS DRAINED/RESTORED maps - GWP20

{ ## run after extended data figure 1 code
  
  summary(data$DRAINED_GWP20/10^6)
  data$cuts <- cut(data$DRAINED_GWP20/10^6, breaks = c(0, 0.01, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c( "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d", "#b2182b", '#67001f')
  
  mask <- data[data$peatlands > 28.8 | data$peatland_loss > 28.8,]

  DRAINED_GWP20.v2 <- ggplot() +
    geom_tile(data = mask, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab(" ") +
    theme_bw(base_size = 7) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000",
                      labels = c("0 to 0.01",
                                 "0.01 to 1", "1 to 10", "10 to 100", "100-100", ">1000", " "))+
    labs(fill = bquote("CO"[2]*"eq. Gg yr"^-1))+
    theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold'),
          legend.position = "none", legend.key.height = unit(0.13, "in"),
          plot.margin = margin(t = -0.5, r = 0.1, b = -0.6, l = 0.1, unit = "in")) +
    annotate("label", x = 20, y = -80, size = 2.5, label = "Drained")  
  
  drained_legend <- ggplot() +
    geom_tile(data = data, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab(" ") +
    theme_bw(base_size = 7) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000",
                      labels = c("0 to 0.01",
                                 "0.01 to 1", "1 to 10", "10 to 100", "100 to 100", ">1000", " "))+
    labs(fill = bquote("(CO"[2]*"eq. Gg yr"^-1*")"))+
    theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold'),
          legend.position = c(0.4,0.51), legend.key.size = unit(0.11, "in"),
          plot.margin = margin(t = -0.5, r = 0.1, b = -0.6, l = 0.1, unit = "in")) +
    annotate("label", x = 20, y = -80, size = 2.5, label = "Drained")
  
  
data$net.restore <- data$RESTORE_GWP20 - data$DRAINED_GWP20   #(Kg CO2e/grid cell)
summary(data$net.restore/10^6)
data$cuts <- cut(data$net.restore/10^6, breaks = c(-Inf, -100, -10, -1, -0.01, 
                                                   0.01, 1, 10, 100, 1000, Inf))
table(data$cuts)
pal <- c("#2166ac", "#4393c3", '#92c5de', "#d1e5f0",  "#e7e7e7", "#fddbc7", '#f4a582', "#d6604d", "#b2182b", '#67001f')
#pal2 <- c("#2166ac", "#fddbc7", "#4393c3", '#f4a582',  '#92c5de',"#d6604d", "#d1e5f0", "#b2182b", "#e7e7e7", '#67001f')
legend.plot <- ggplot() +
  geom_tile(data = data, aes(x= x, y = y, fill = cuts))+
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 7) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_fill_manual(values = pal, na.value = "#00000000",
                    labels = c("< -100", "-100 to -10", "-10 to -1", "-1 to -0.01", "-0.01 to 0.01",
                               "0.01 to 1", "1 to 10", "10 to 100", "100 to 1000", ">1000"))+
  labs(fill = bquote("(CO"[2]*"eq. Gg yr"^-1*")"))+
  theme(legend.position = c(0.4,0.5), legend.direction="horizontal",
        legend.title = element_text(size = 8, hjust = 0.5, face = "bold"), 
        legend.key.size = unit(0.11, "in"),
        plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
        axis.title = element_text(size = 10, face = 'bold'),
        plot.margin = margin(t = -0.5, r = 0.3, b = -0.6, l = 1, unit = "in")) +
  guides(fill = guide_legend(ncol = 1, title.position = "top"))

mask <- data[data$peatlands > 28.8 | data$peatland_loss > 28.8,]

map.restore <- ggplot() +
  geom_tile(data = mask, aes(x= x, y = y, fill = cuts))+
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 7) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_fill_manual(values = pal, na.value = "#00000000",
                    labels = c("< -100", "-100 to -10", "-10 to -1", "-1 to -0.01", "-0.01 to 0.01",
                               "0.01 to 1", "1 to 10", "10 to 100", "100 to 1000", ">1000"))+
  labs(fill = bquote("CO"[2]*"eq. Gg yr"^-1))+
  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
        axis.title = element_text(size = 10, face = 'bold'),
        legend.position = "none", legend.key.height = unit(0.13, "in"),
        plot.margin = margin(t = -0.5, r = 0.1, b = -0.6, l = 0.1, unit = "in")) +
  annotate("label", x = 20, y = -80, size = 2.5, label = "'Restored to Intact'")


subset.negative <- data[data$net.restore < 0,]
pp.sink.restore <- sum(subset.negative$peatland_loss)/sum(data$peatland_loss)*100



#### Endpoint: Rewetted  
data$net.rewet <- data$REWET_GWP20 - data$DRAINED_GWP20   #(Kg CO2e/grid cell)
summary(data$net.rewet/10^6)
data$cuts <- cut(data$net.rewet/10^6, breaks = c(-Inf, -100, -10, -1, -0.01, 
                                                   0.01, 1, 10, 100, 1000, Inf))
table(data$cuts)
pal <- c("#2166ac", "#4393c3", '#92c5de', "#d1e5f0",  "#e7e7e7", "#fddbc7", '#f4a582', "#d6604d", "#b2182b", '#67001f')

mask <- data[data$peatlands > 28.8 | data$peatland_loss > 28.8,]

map.rewet <- ggplot() +
  geom_tile(data = mask, aes(x= x, y = y, fill = cuts))+
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 7) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_fill_manual(values = pal, na.value = "#00000000",
                    labels = c("< -100", "-100 to -10", "-10 to -1", "-1 to -0.01", "-0.01 to 0.01",
                               "0.01 to 1", "1 to 10", "10 to 100", "100 to 1000", ">1000"))+
  labs(fill = bquote("CO"[2]*"eq. Gg yr"^-1))+
  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
        axis.title = element_text(size = 10, face = 'bold'),
        legend.position = "none", legend.key.height = unit(0.13, "in"),
        plot.margin = margin(t = -0.5, r = 0.1, b = -0.6, l = 0.1, unit = "in")) +
  annotate("label", x = 20, y = -80, size = 2.5, label = "'Recently Rewetted'")

subset.negative <- data[data$net.rewet < 0,]
pp.sink.rewet <- sum(subset.negative$peatland_loss)/sum(data$peatland_loss)*100


legend <- cowplot::get_legend(drained_legend)

legend2 <- cowplot::get_legend(legend.plot)

# plot_grid(DRAINED_GWP20.v2, map.rewet, map.restore, legend, ncol = 1, 
#           rel_heights = c(1,1,1,0.4),
#           labels = c("a", "b", "c"), label_size = 11)

top <- plot_grid(DRAINED_GWP20.v2, legend, ncol = 2, 
                  rel_widths = c(1,0.3),
                  labels = c(" ", "GHG emissions from\n  drained peatlands"),
                 label_size = 8, label_x = c(0, -0.5))
br <- plot_grid(map.rewet, map.restore, ncol = 1, 
                labels = c("b", "c"), label_size = 11)
bottom <- plot_grid(br, legend2, nrow = 1, rel_widths = c(1,0.3),
                    labels = c(" ", " Net change in GHG \nemissions following \npotenital restoration"),
                    label_size = 8, label_x = -0.5, label_y = 0.85)


plot_grid(top, bottom, ncol = 1, rel_heights = c(1,2), labels = c("a", " "), 
          label_size = 11)



ggsave(filename = "Figures/Drained-Rewetted-Restored2.png",
       plot = last_plot(), bg = "white",
       width = 5, 
       height = 5.7, 
       unit = "in",
       dpi = 300)

}







## metrics
pp.sink ## The percent of peatlands that would be GHG sinks in their own right
        ## following restoration to intact conditions (not even accounting for 
        ## emissions avoided from the drained state)

pp.sink.restore ## The percent of peatlands that would be NET GHG sinks following restoration to intact conditions

pp.sink.rewet ##  The percent of peatland that owuld be NET GHG sinks following rewetting (REWETTED)






