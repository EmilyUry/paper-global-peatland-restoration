

library(tidyverse)
library(sf)
library(raster)
library(cowplot)


setwd("C:/Users/eury/OneDrive - Environmental Defense Fund - edf.org/Wetland-Restoration-GHG/Paper 1")


coasts <- st_read("Data_sources/Coastline/ne_110m_coastline.shp", quiet = TRUE)




## data setup:
{
EF <- read.csv("Data_sources/Emission_Factors/EF_SD_Summary.csv")
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


## Filter to just areas of peatland los
filter <- input[!is.na(input$peatland_loss),]
filter <- filter[filter$peatland_loss > 0.0001,] ## removes pixels with ridiculously small peatland loss or no peatland loss

### mask out coastal pixels without methane emissions estimates
filter <- filter[filter$CH4_Zhang_2020 > 0,] 


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

data$CH4_Wetland <- data$CH4_Zhang_2020     ### the spatial explicit value with random error thrown in

}


##### EXTENDED DATA FIGURE 1 -- maps of all data inputs GWP 20

{
### masked to areas of wetlands loss only 
data <- data[!is.na(data$peatland_loss),]
data <- data[data$peatland_loss > 0,]
## and cut out coasts where methane data is sparse
data <- data[data$CH4_Wetland > 0, ]


summary(data$CO2_Wetland*data$peatland_loss/10^6)
data$cuts <- cut(data$CO2_Wetland*data$peatland_loss/10^6, breaks = c(-Inf, -100, -10, -1, -0.0001, 0))
table(data$cuts)
pal <- c( "#2166ac", "#4393c3",  '#92c5de', "#d1e5f0", "#e7e7e7")

IW_CO2 <- ggplot() +
  geom_tile(data = data, aes(x= x, y = y, fill = cuts))+
  xlab(" ") +
  ylab("Intact") +
  theme_bw(base_size = 10) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_fill_manual(values = pal, na.value = "#00000000")+
  theme(legend.position = "none", 
        plot.title = element_text(size = 10, hjust = 0.5, face = "plain"), 
        axis.title = element_text(size = 10, face = 'plain')) +
  annotate("label", x = -50, y = -80, size = 2.5, label = "Temmink et al. 2023")

summary(data$CH4_Wetland*data$peatland_loss/10^6*96)
data$cuts <- cut(data$CH4_Wetland*data$peatland_loss/10^6*96, breaks = c(0, 0.001, 1, 10, 100, 1000, Inf))
table(data$cuts)
pal <- c( "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d", "#b2182b", '#67001f')

IW_CH4 <- ggplot() +
  geom_tile(data = data, aes(x= x, y = y, fill = cuts))+
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 10) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_fill_manual(values = pal, na.value = "#00000000")+
  theme(legend.position = "none", 
        plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
        axis.title = element_text(size = 10, face = 'bold')) +
  annotate("label", x = -60, y = -80, size = 2.5, label = "Zhang et al. 2017")


summary(data$N2O_Wetland*data$peatland_loss/10^6*250)
data$cuts <- cut(data$N2O_Wetland*data$peatland_loss/10^6*250, breaks = c(-100, -10, -1, -0.0001, 0.001, 1, 10, 100, 1000, Inf))
table(data$cuts)
pal <- c('#92c5de', "#d1e5f0", "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d",  "#b2182b")


IW_N2O <- ggplot() +
  geom_tile(data = data, aes(x= x, y = y, fill = cuts)) +
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 10) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_fill_manual(values = pal, na.value = "#00000000")+
  theme(legend.position = "none", 
        plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
        axis.title = element_text(size = 10, face = 'bold')) +
  annotate("label", x = -50, y = -80, size = 2.5, label = "Bahram et al. 2022")



summary(data$CO2_Rewetted*data$peatland_loss/10^6)
data$cuts <- cut(data$CO2_Rewetted*data$peatland_loss/10^6, breaks = c(-Inf, -100, -10, -1, -0.0001, 
                                              0.001, 1, 10, 100, 1000, Inf))
table(data$cuts)
pal <- c("#2166ac", "#4393c3", '#92c5de', "#d1e5f0",  "#e7e7e7", "#fddbc7", '#f4a582', "#d6604d", "#b2182b", '#67001f')

RW_CO2 <- ggplot() +
  geom_tile(data = data, aes(x= x, y = y, fill = cuts))+
  xlab(" ") +
  ylab("Rewetted") +
  theme_bw(base_size = 10) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_fill_manual(values = pal, na.value = "#00000000")+
  theme(legend.position = "none", 
        plot.title = element_text(size = 10, hjust = 0.5, face = "plain"), 
        axis.title = element_text(size = 10, face = 'plain')) +
  annotate("label", x = -80, y = -80, size = 2.5, label = "IPCC 2014")


summary(data$CH4_Rewetted*data$peatland_loss/10^6*96)
data$cuts <- cut(data$CH4_Rewetted*data$peatland_loss/10^6*96, breaks = c(0, 0.001, 1, 10, 100, 1000, Inf))
table(data$cuts)
pal <- c( "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d", "#b2182b", '#67001f')

RW_CH4 <- ggplot() +
  geom_tile(data = data, aes(x= x, y = y, fill = cuts)) +
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 10) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_fill_manual(values = pal, na.value = "#00000000")+
  theme(legend.position = "none", 
        plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
        axis.title = element_text(size = 10, face = 'bold')) +
  annotate("label", x = -80, y = -80, size = 2.5, label = "IPCC 2014")

summary(data$N2O_Rewetted*data$peatland_loss/10^6*250)
data$cuts <- cut(data$N2O_Rewetted*data$peatland_loss/10^6*250, breaks = c(-Inf, -0.001, 0.001, 1, 10, 100, 1000, Inf))
table(data$cuts)
pal <- c("#e7e7e7", "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d")
#### ALL ZEROS

RW_N2O <- ggplot() +
  geom_tile(data = data, aes(x= x, y = y, fill = cuts)) +
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 10) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_fill_manual(values = pal, na.value = "#00000000")+
  theme(legend.position = "none", 
        plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
        axis.title = element_text(size = 10, face = 'bold')) +
  annotate("label", x = -80, y = -80, size = 2.5, label = "IPCC 2014")



### CLACULATE CO2/CH4/N2O FLUX DRAINED PEATLANDS (DP) (Kg/grid cell)
data$DP_CO2 <- data$PL_ag*data$CO2_Ag + data$PL_for*data$CO2_Forest +
  data$PL_peatx*data$CO2_Peatx + data$PL_rice*data$CO2_Rice
data$DP_CH4 <- data$PL_ag*data$CH4_Ag + data$PL_for*data$CH4_Forest +
  data$PL_peatx*data$CH4_Peatx + data$PL_rice*data$CH4_Rice
data$DP_N2O <- data$PL_ag*data$N2O_Ag + data$PL_for*data$N2O_Forest +
  data$PL_peatx*data$N2O_Peatx + data$PL_rice*data$N2O_Rice



summary(data$DP_CO2/10^6)
data$cuts <- cut(data$DP_CO2/10^6, breaks = c(0, 0.001, 1, 10, 100, 1000, Inf))
table(data$cuts)
pal <- c( "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d", "#b2182b", '#67001f')


DW_CO2 <- ggplot() +
  geom_tile(data = data, aes(x= x, y = y, fill = cuts))+
  xlab(" ") +
  ylab("Drained") +
  ggtitle(bquote("CO"[2])) +
  theme_bw(base_size = 10) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_fill_manual(values = pal, na.value = "#00000000")+
  theme(legend.position = "none", 
        plot.title = element_text(size = 10, hjust = 0.5, face = "plain"), 
        axis.title = element_text(size = 10, face = 'plain')) +
  annotate("label", x = -80, y = -80, size = 2.5, label = "IPCC 2014")



summary(data$DP_CH4/10^6*96)
data$cuts <- cut(data$DP_CH4/10^6*96, breaks = c(0, 0.001, 1, 10, 100, 1000, Inf))
table(data$cuts)
pal <- c( "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d", "#b2182b", '#67001f')

DW_CH4 <- ggplot() +
  geom_tile(data = data, aes(x= x, y = y, fill = cuts)) +
  xlab(" ") +
  ylab(" ") +
  ggtitle(bquote("CH"[4])) +
  theme_bw(base_size = 10) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_fill_manual(values = pal, na.value = "#00000000")+
  theme(legend.position = "none", 
        plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
        axis.title = element_text(size = 10, face = 'bold')) +
  annotate("label", x = -80, y = -80, size = 2.5, label = "IPCC 2014")




summary(data$DP_N2O/10^6*250)
data$cuts <- cut(data$DP_N2O/10^6*250, breaks = c(-0.0001, 0.001, 1, 10, 100, 1000, Inf))
table(data$cuts)
pal <- c("#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d", "#b2182b", '#67001f')


DW_N2O <- ggplot() +
  geom_tile(data = data, aes(x= x, y = y, fill = cuts)) +
  xlab(" ") +
  ylab(" ") +
  ggtitle(bquote("N"[2]*"O")) +
  theme_bw(base_size = 10) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_fill_manual(values = pal, na.value = "#00000000")+
  theme(legend.position = "none", 
        plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
        axis.title = element_text(size = 10, face = 'bold')) +
  annotate("label", x = -80, y = -80, size = 2.5, label = "IPCC 2014")




#### sum of all three gases

data$DRAINED_GWP20 <- data$DP_CO2 + data$DP_CH4*96 + data$DP_N2O*250 #(Kg CO2e/grid cell)
sum(data$DRAINED_GWP20)/10^12

summary(data$DRAINED_GWP20/10^6)
data$cuts <- cut(data$DRAINED_GWP20/10^6, breaks = c(0, 0.001, 1, 10, 100, 1000, Inf))
table(data$cuts)
pal <- c( "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d", "#b2182b", '#67001f')


DRAINED_GWP20 <- ggplot() +
  geom_tile(data = data, aes(x= x, y = y, fill = cuts))+
  xlab(" ") +
  ylab(" ") +
  ggtitle("All GHGs") +
  theme_bw(base_size = 10) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_fill_manual(values = pal, na.value = "#00000000")+
  theme(legend.position = "none", 
        plot.title = element_text(size = 10, hjust = 0.5, face = "plain"), 
        axis.title = element_text(size = 10, face = 'bold')) 



 


data$REWET_GWP20 <- data$peatland_loss*(data$CO2_Rewetted + data$CH4_Rewetted*96 + data$N2O_Rewetted*250)  #(Kg CO2e/grid cell)
sum(data$REWET_GWP20)/10^12

summary(data$REWET_GWP20/10^6)
data$cuts <- cut(data$REWET_GWP20/10^6, breaks = c(-0.0001, 
                                                   0.001, 1, 10, 100, 1000, Inf))
table(data$cuts)
pal <- c( "#e7e7e7", "#fddbc7", '#f4a582', "#d6604d", "#b2182b", '#67001f')


REWET_GWP20 <- ggplot() +
  geom_tile(data = data, aes(x= x, y = y, fill = cuts))+
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 10) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_fill_manual(values = pal, na.value = "#00000000")+
  theme(legend.position = "none", 
        plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
        axis.title = element_text(size = 10, face = 'bold'))


sum(data$REWET_GWP20 - data$DRAINED_GWP20)/10^12


data$RESTORE_GWP20 <- data$peatland_loss*(data$CO2_Wetland + data$CH4_Wetland*96 + data$N2O_Wetland*250)  #(Kg CO2e/grid cell)
sum(data$RESTORE_GWP20)/10^12

summary(data$RESTORE_GWP20/10^6)

subset.negative <- data[data$RESTORE_GWP20 < 0,]
pp.sink <- sum(subset.negative$peatland_loss)/sum(data$peatland_loss)*100
subset.positive <- data[data$RESTORE_GWP20 > 0,]
pp.source <- sum(subset.positive$peatland_loss)/sum(data$peatland_loss)*100



data$cuts <- cut(data$RESTORE_GWP20/10^6, breaks = c(-Inf, -100, -10, -1, -0.0001, 
                                                     0.001, 1, 10, 100, 1000, Inf))
table(data$cuts)
pal <- c("#2166ac", "#4393c3", '#92c5de', "#d1e5f0",  "#e7e7e7", "#fddbc7", '#f4a582', "#d6604d", "#b2182b", '#67001f')


RESTORE_GWP20 <- ggplot() +
  geom_tile(data = data, aes(x= x, y = y, fill = cuts))+
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




top <- plot_grid(DW_CO2, DW_CH4, DW_N2O, DRAINED_GWP20,
          RW_CO2, RW_CH4, RW_N2O, REWET_GWP20,
          IW_CO2, IW_CH4, IW_N2O, RESTORE_GWP20,
          labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"),
          label_y = 1.02,
          label_size = 11, ncol = 4, rel_heights = c (1.13, 1, 1))


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
        legend.key.size = unit(0.16, "in"),
        plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
        axis.title = element_text(size = 10, face = 'bold')) +
  guides(fill = guide_legend(ncol = 10))

legend <- cowplot::get_legend(legend.plot)

plot_grid(top, legend, ncol = 1, rel_heights = c(9,1), 
          labels = c(" ", "GHG emissions (Gg CO2eq. per grid cell)"), 
          label_size = 9, label_x = 0.2, label_y = 1.3)


ggsave(filename = "Figures/All_inputs_grid.png",
       plot = last_plot(), bg = "white",
       width = 8, 
       height = 4, 
       unit = "in",
       dpi = 300)

}

##### EXTENDED DATA FIG. X (same as fig 1, but for GWP100)

{
  ### masked to areas of wetlands loss only 
  data <- data[!is.na(data$peatland_loss),]
  data <- data[data$peatland_loss > 0,]
  ## and cut out coasts where methane data is sparse
  data <- data[data$CH4_Wetland > 0, ]
  
  
  summary(data$CO2_Wetland*data$peatland_loss/10^6)
  data$cuts <- cut(data$CO2_Wetland*data$peatland_loss/10^6, breaks = c(-Inf, -100, -10, -1, -0.0001, 0))
  table(data$cuts)
  pal <- c( "#2166ac", "#4393c3",  '#92c5de', "#d1e5f0", "#e7e7e7")
  
  IW_CO2 <- ggplot() +
    geom_tile(data = data, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab("Intact") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "plain"), 
          axis.title = element_text(size = 10, face = 'plain')) +
    annotate("label", x = -50, y = -80, size = 2.5, label = "Temmink et al. 2023")
  
  summary(data$CH4_Wetland*data$peatland_loss/10^6*45)
  data$cuts <- cut(data$CH4_Wetland*data$peatland_loss/10^6*45, breaks = c(0, 0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c( "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d", "#b2182b", '#67001f')
  
  IW_CH4 <- ggplot() +
    geom_tile(data = data, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab(" ") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold')) +
    annotate("label", x = -60, y = -80, size = 2.5, label = "Zhang et al. 2017")
  
  
  summary(data$N2O_Wetland*data$peatland_loss/10^6*270)
  data$cuts <- cut(data$N2O_Wetland*data$peatland_loss/10^6*270, breaks = c(-100, -10, -1, -0.0001, 0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c('#92c5de', "#d1e5f0", "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d",  "#b2182b")
  
  
  IW_N2O <- ggplot() +
    geom_tile(data = data, aes(x= x, y = y, fill = cuts)) +
    xlab(" ") +
    ylab(" ") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold')) +
    annotate("label", x = -50, y = -80, size = 2.5, label = "Bahram et al. 2022")
  
  
  
  summary(data$CO2_Rewetted*data$peatland_loss/10^6)
  data$cuts <- cut(data$CO2_Rewetted*data$peatland_loss/10^6, breaks = c(-Inf, -100, -10, -1, -0.0001, 
                                                                         0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c("#2166ac", "#4393c3", '#92c5de', "#d1e5f0",  "#e7e7e7", "#fddbc7", '#f4a582', "#d6604d", "#b2182b", '#67001f')
  
  RW_CO2 <- ggplot() +
    geom_tile(data = data, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab("Rewetted") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "plain"), 
          axis.title = element_text(size = 10, face = 'plain')) +
    annotate("label", x = -80, y = -80, size = 2.5, label = "IPCC 2014")
  
  
  summary(data$CH4_Rewetted*data$peatland_loss/10^6*45)
  data$cuts <- cut(data$CH4_Rewetted*data$peatland_loss/10^6*45, breaks = c(0, 0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c( "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d", "#b2182b", '#67001f')
  
  RW_CH4 <- ggplot() +
    geom_tile(data = data, aes(x= x, y = y, fill = cuts)) +
    xlab(" ") +
    ylab(" ") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold')) +
    annotate("label", x = -80, y = -80, size = 2.5, label = "IPCC 2014")
  
  summary(data$N2O_Rewetted*data$peatland_loss/10^6*270)
  data$cuts <- cut(data$N2O_Rewetted*data$peatland_loss/10^6*270, breaks = c(-Inf, -0.001, 0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c("#e7e7e7", "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d")
  #### ALL ZEROS
  
  RW_N2O <- ggplot() +
    geom_tile(data = data, aes(x= x, y = y, fill = cuts)) +
    xlab(" ") +
    ylab(" ") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold')) +
    annotate("label", x = -80, y = -80, size = 2.5, label = "IPCC 2014")
  
  
  
  ### CLACULATE CO2/CH4/N2O FLUX DRAINED PEATLANDS (DP) (Kg/grid cell)
  data$DP_CO2 <- data$PL_ag*data$CO2_Ag + data$PL_for*data$CO2_Forest +
    data$PL_peatx*data$CO2_Peatx + data$PL_rice*data$CO2_Rice
  data$DP_CH4 <- data$PL_ag*data$CH4_Ag + data$PL_for*data$CH4_Forest +
    data$PL_peatx*data$CH4_Peatx + data$PL_rice*data$CH4_Rice
  data$DP_N2O <- data$PL_ag*data$N2O_Ag + data$PL_for*data$N2O_Forest +
    data$PL_peatx*data$N2O_Peatx + data$PL_rice*data$N2O_Rice
  
  
  
  summary(data$DP_CO2/10^6)
  data$cuts <- cut(data$DP_CO2/10^6, breaks = c(0, 0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c( "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d", "#b2182b", '#67001f')
  
  
  DW_CO2 <- ggplot() +
    geom_tile(data = data, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab("Drained") +
    ggtitle(bquote("CO"[2])) +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "plain"), 
          axis.title = element_text(size = 10, face = 'plain')) +
    annotate("label", x = -80, y = -80, size = 2.5, label = "IPCC 2014")
  
  
  
  summary(data$DP_CH4/10^6*45)
  data$cuts <- cut(data$DP_CH4/10^6*45, breaks = c(0, 0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c( "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d", "#b2182b", '#67001f')
  
  DW_CH4 <- ggplot() +
    geom_tile(data = data, aes(x= x, y = y, fill = cuts)) +
    xlab(" ") +
    ylab(" ") +
    ggtitle(bquote("CH"[4])) +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold')) +
    annotate("label", x = -80, y = -80, size = 2.5, label = "IPCC 2014")
  
  
  
  
  summary(data$DP_N2O/10^6*270)
  data$cuts <- cut(data$DP_N2O/10^6*270, breaks = c(-0.0001, 0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c("#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d", "#b2182b", '#67001f')
  
  
  DW_N2O <- ggplot() +
    geom_tile(data = data, aes(x= x, y = y, fill = cuts)) +
    xlab(" ") +
    ylab(" ") +
    ggtitle(bquote("N"[2]*"O")) +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold')) +
    annotate("label", x = -80, y = -80, size = 2.5, label = "IPCC 2014")
  
  
  
  
  #### sum of all three gases
  
  data$DRAINED_GWP100 <- data$DP_CO2 + data$DP_CH4*45 + data$DP_N2O*270 #(Kg CO2e/grid cell)
  sum(data$DRAINED_GWP100)/10^12
  
  summary(data$DRAINED_GWP100/10^6)
  data$cuts <- cut(data$DRAINED_GWP100/10^6, breaks = c(0, 0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c( "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d", "#b2182b", '#67001f')
  
  
  DRAINED_GWP100 <- ggplot() +
    geom_tile(data = data, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab(" ") +
    ggtitle("All GHGs") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "plain"), 
          axis.title = element_text(size = 10, face = 'bold')) 
  
  
  
  data$REWET_GWP100 <- data$peatland_loss*(data$CO2_Rewetted + data$CH4_Rewetted*45 + data$N2O_Rewetted*270)  #(Kg CO2e/grid cell)
  sum(data$REWET_GWP100)/10^12
  
  summary(data$REWET_GWP100/10^6)
  data$cuts <- cut(data$REWET_GWP100/10^6, breaks = c(-0.0001, 
                                                     0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c( "#e7e7e7", "#fddbc7", '#f4a582', "#d6604d", "#b2182b", '#67001f')
  
  REWET_GWP100 <- ggplot() +
    geom_tile(data = data, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab(" ") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold'))
  
  
  sum(data$REWET_GWP100 - data$DRAINED_GWP100)/10^12
  
  
  data$RESTORE_GWP100 <- data$peatland_loss*(data$CO2_Wetland + data$CH4_Wetland*45 + data$N2O_Wetland*270)  #(Kg CO2e/grid cell)
  sum(data$RESTORE_GWP100)/10^12
  
  summary(data$RESTORE_GWP100/10^6)
  data$cuts <- cut(data$RESTORE_GWP100/10^6, breaks = c(-Inf, -100, -10, -1, -0.0001, 
                                                       0.001, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c("#2166ac", "#4393c3", '#92c5de', "#d1e5f0",  "#e7e7e7", "#fddbc7", '#f4a582', "#d6604d", "#b2182b", '#67001f')
  
  
  RESTORE_GWP100 <- ggplot() +
    geom_tile(data = data, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab(" ") +
    theme_bw(base_size = 10) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = "none", 
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold'))
  
  
  sum(data$RESTORE_GWP100 - data$DRAINED_GWP100)/10^12
  sum(data$RESTORE_GWP100)/10^12 - sum(data$DRAINED_GWP100)/10^12
  
  
  
  
  top <- plot_grid(DW_CO2, DW_CH4, DW_N2O, DRAINED_GWP100,
                   RW_CO2, RW_CH4, RW_N2O, REWET_GWP100,
                   IW_CO2, IW_CH4, IW_N2O, RESTORE_GWP100,
                   labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"),
                   label_y = 1.02,
                   label_size = 11, ncol = 4, rel_heights = c (1.13, 1, 1))
  
  
  legend.plot <- ggplot() +
    geom_tile(data = data, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab(" ") +
    theme_bw(base_size = 9) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000")+
    theme(legend.position = c(0.5,0.5), legend.direction="horizontal",
          legend.title = element_blank(), 
          legend.key.size = unit(0.16, "in"),
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold')) +
    guides(fill = guide_legend(ncol = 10))
  
  legend <- cowplot::get_legend(legend.plot)
  
  plot_grid(top, legend, ncol = 1, rel_heights = c(9,1), 
            labels = c(" ", "GHG emissions (Gg CO2eq. per grid cell)"), 
            label_size = 9, label_x = 0.2, label_y = 1.3)
  
  
  ggsave(filename = "Figures/Extended_Figures/All_inputs_grid_SGWP100.png",
         plot = last_plot(), bg = "white",
         width = 8, 
         height = 4, 
         unit = "in",
         dpi = 300)
  
}




##### net restoration maps (Fig 1) - GWP20)

{
  
  summary(data$DRAINED_GWP20/10^6)
  data$cuts <- cut(data$DRAINED_GWP20/10^6, breaks = c(0, 0.01, 1, 10, 100, 1000, Inf))
  table(data$cuts)
  pal <- c( "#e7e7e7", "#fddbc7",  '#f4a582', "#d6604d", "#b2182b", '#67001f')
  
  
  DRAINED_GWP20.v2 <- ggplot() +
    geom_tile(data = data, aes(x= x, y = y, fill = cuts))+
    xlab(" ") +
    ylab(" ") +
    theme_bw(base_size = 7) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    scale_fill_manual(values = pal, na.value = "#00000000",
                      labels = c("0 to 0.01",
                                 "0.01 to 1", "1 to 10", "10 to 100", "100-100", ">1000"))+
    labs(fill = bquote("CO"[2]*"eq. Gg yr"^-1))+
    theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.title = element_text(size = 10, face = 'bold'),
          legend.position = "none", legend.key.height = unit(0.13, "in"),
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
                               "0.01 to 1", "1 to 10", "10 to 100", "100-1000", ">1000"))+
  labs(fill = bquote("GHG emissions (CO"[2]*"eq. Gg yr"^-1*")"))+
  theme(legend.position = c(0.525,0.5), legend.direction="horizontal",
        legend.title = element_text(size = 8, hjust = 0.5, face = "bold"), 
        legend.key.size = unit(0.11, "in"),
        plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
        axis.title = element_text(size = 10, face = 'bold'),
        plot.margin = margin(t = -0.5, r = 0.3, b = -0.6, l = 1, unit = "in")) +
  guides(fill = guide_legend(ncol = 5, title.position = "top"))

map.restore <- ggplot() +
  geom_tile(data = data, aes(x= x, y = y, fill = cuts))+
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 7) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_fill_manual(values = pal, na.value = "#00000000",
                    labels = c("< -100", "-100 to -10", "-10 to -1", "-1 to -0.01", "-0.01 to 0.01",
                               "0.01 to 1", "1 to 10", "10 to 100", "100-1000", ">1000"))+
  labs(fill = bquote("CO"[2]*"eq. Gg yr"^-1))+
  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
        axis.title = element_text(size = 10, face = 'bold'),
        legend.position = "none", legend.key.height = unit(0.13, "in"),
        plot.margin = margin(t = -0.5, r = 0.1, b = -0.6, l = 0.1, unit = "in")) +
  annotate("label", x = 20, y = -80, size = 2.5, label = "End point: Intact")



#### Endpoint: Rewetted  
data$net.rewet <- data$REWET_GWP20 - data$DRAINED_GWP20   #(Kg CO2e/grid cell)
summary(data$net.rewet/10^6)
data$cuts <- cut(data$net.rewet/10^6, breaks = c(-Inf, -100, -10, -1, -0.01, 
                                                   0.01, 1, 10, 100, 1000, Inf))
table(data$cuts)
pal <- c("#2166ac", "#4393c3", '#92c5de', "#d1e5f0",  "#e7e7e7", "#fddbc7", '#f4a582', "#d6604d", "#b2182b", '#67001f')


map.rewet <- ggplot() +
  geom_tile(data = data, aes(x= x, y = y, fill = cuts))+
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 7) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_fill_manual(values = pal, na.value = "#00000000",
                    labels = c("< -100", "-100 to -10", "-10 to -1", "-1 to -0.01", "-0.01 to 0.01",
                               "0.01 to 1", "1 to 10", "10 to 100", "100-1000", ">1000"))+
  labs(fill = bquote("CO"[2]*"eq. Gg yr"^-1))+
  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
        axis.title = element_text(size = 10, face = 'bold'),
        legend.position = "none", legend.key.height = unit(0.13, "in"),
        plot.margin = margin(t = -0.5, r = 0.1, b = -0.6, l = 0.1, unit = "in")) +
  annotate("label", x = 20, y = -80, size = 2.5, label = "End point: Rewetted")


legend <- cowplot::get_legend(legend.plot)

plot_grid(DRAINED_GWP20.v2, map.rewet, map.restore, legend, ncol = 1, 
          rel_heights = c(1,1,1,0.4),
          labels = c("a", "b", "c"), label_size = 11)

ggsave(filename = "Figures/Drained-Rewetted-Restored.png",
       plot = last_plot(), bg = "white",
       width = 3.5, 
       height = 5.8, 
       unit = "in",
       dpi = 300)

}








## metrics
pp.sink ## The percent of peatlands that would be GHG sinks in their own right
        ## following restoration to intact conditions (not even accounting for 
        ## emissions avoided from the drained state)













