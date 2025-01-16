

### code used to generate extended data figures



setwd("C:/Users/eury/OneDrive - Environmental Defense Fund - edf.org/Wetland-Restoration-GHG/Paper 1")

library(ggplot2)
library(tidyverse)
library(sf)
library(cowplot)

###




### Extended Data Figure - Emissions factor bar charts
{
df <- read.csv("Data_sources/Emission_Factors/EF_SD_Summary2.csv")
df$CZ <- factor(df$CZ)
levels(df$CZ) <- c("Boreal", "Temperate", "Tropical")
df$CZ <- factor(df$CZ, levels = c("Tropical", "Temperate", "Boreal"))
df$GHG <- factor(df$GHG, levels = c("CO2", "CH4", "N2O"))
df$LU <- factor(df$LU, levels = c("Wetland", "Ag", "Forest", "Peatx", "Rice", "Rewetted"))

## plot in terms of CO2e (sGWP20)
df$CO2e <- ifelse(df$GHG == "CO2", df$EF_mean, 
                  ifelse(df$GHG == "CH4", 96*df$EF_mean, 250*df$EF_mean))
df$CO2e_SD <- ifelse(df$GHG == "CO2", df$SD, 
                  ifelse(df$GHG == "CH4", 96*df$SD, 250*df$SD))

## rename facet labels
LU.labs <- c("Wetland", "Agriculture", "Forestry", "Peat extraction", "Rice cultivation", "Wetland")
names(LU.labs) <- (c("Wetland", "Ag", "Forest", "Peatx", "Rice", "Rewetted"))


## Truncated yaxis
p <- ggplot(df, aes(x = CZ, y = CO2e/10^6, fill = CZ)) +
  geom_bar(stat = "identity") +
  facet_grid(GHG~LU, labeller = labeller(LU = LU.labs)) +
  theme_bw(base_size = 9) +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_manual(values = c( "aquamarine3" , "darkgreen", "#abababee")) +
  geom_hline(yintercept = 0, linewidth = 0.1) +
  xlab(" ") +
  ylab(expression(Emissions~CO[2]*eq~kg~m^{-2}~year^{-1})) +
  scale_y_continuous(breaks = c(0, 2, 4, 6), labels = c("0", "2", "4", "6")) +
  geom_errorbar(aes(x = CZ, ymin = CO2e/10^6-CO2e_SD/10^6, ymax = CO2e/10^6+CO2e_SD/10^6), width = 0.2, linewidth = 0.2) +
  coord_flip(ylim = c(-0.800000, 5.900000)) 

top <- plot_grid(NULL, NULL, NULL, NULL, NULL,  ncol = 5, rel_widths = c(1, 1, 1, 1,1), label_size = 8,
                 labels = c("                Intact", "     | ",  "        Drained", "                            |", "  Rewetted"))

plot_grid(top, p, ncol = 1, rel_heights = c(1, 14))


ggsave(filename = "Figures/Extended_Figures/EF_bar_plot_v3.png",
       plot = last_plot(), bg = "white",
       width = 6, 
       height = 3, 
       unit = "in",
       dpi = 300)
}



### Extended data figure: map of drained peatland area
{
data <- read.csv("Data_sources/Extracted_datafiles/all_Data.csv")
coasts <- st_read("Data_sources/Coastline/ne_110m_coastline.shp", quiet = TRUE)

### subset to just the proportion of wetlands in each cell that is peatland
data$new.wetland.area <- ifelse(data$WA2020 < data$peatlands, data$peatlands, data$WA2020)
data$peat.frac <- data$peatlands/data$new.wetland.area
data$peat.percent <- data$peatland/data$cell_area

summary(data$peat.percent)
summary(data$peatlands)
summary(data$peat.frac)
data$cuts <- cut(data$peatlands, breaks = c(0, 25, 100, 250, 1000, 3000))
#data$cuts <- cut(data$peat.frac, breaks = c(0, 0.01, 0.25, 0.5, 0.9, 1.1))
#data$cuts <- cut(data$peat.percent, breaks = c(0, 0.01, 0.25, 0.5, 0.75, 1))

pal <- c("#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac")


options(scipen = 9999)
S1A <- ggplot() +
  geom_tile(data = data[!is.na(data$cuts),], aes(x= x, y = y, fill = cuts)) +
  scale_fill_manual(values = pal, na.value = "#00000000", labels = c("< 25", "25 - 100", 
                                                                     "100-250", "250-1000", "> 1000"))+
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 9) +
  labs(fill = bquote("Present day \npeatland area "(km^2))) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_size_identity() +
  theme(legend.position = c(0.14,0.3), legend.key.size = unit(0.18, "in"),
        plot.margin = margin(t = -0.1, r = 0.1, b = -0.2, l = 0.1, unit = "in"))
S1A


## subset area of wetland lost to just proportional area of peat
## Peatland lost
data$PL_ag <- data$WL_ag*data$peat.frac
data$PL_ag_re <- data$WL_ag_re*data$peat.frac
data$PL_for <- data$WL_for*data$peat.frac
data$PL_for_re <- data$WL_for_re*data$peat.frac
data$PL_peatx <- data$WL_peatx*data$peat.frac
data$PL_peatx_re <- data$WL_peatx_re*data$peat.frac
data$PL_rice<- data$WL_rice*data$peat.frac
data$PL_rice_re <- data$WL_rice_re*data$peat.frac

data$PL_at <- data$PL_ag + data$PL_for + data$PL_peatx + data$PL_rice
data$PL_re <- data$PL_ag_re + data$PL_for_re + data$PL_peatx_re + data$PL_rice_re
data$PL_ag_re[data$PL_ag_re < 0] <- 0

summary(data$PL_at)
data$cuts <- cut(data$PL_at, breaks = c(0, 10, 50, 100, 500, 3000))

pal <- c("#f7f7f7", "#fddbc7", "#f4a582", "#d6604d", "#b2182b")

filter <- data[data$peatlands > 28.8,] ## cutoff areas with very few peatlands for visual clarity
options(scipen = 9999)
S1B <- ggplot() +
  geom_tile(data = filter[!is.na(filter$cuts),], aes(x= x, y = y, fill = cuts)) +
  scale_fill_manual(values = pal, na.value = "#00000000", labels = c("< 10", "10-50", 
                                                                     "50-100", "100-500", "> 500"))+
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 9) +
  labs(fill = bquote("Peatland \n drained \n1700-2020 "(km^2))) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_size_identity() +
  theme(legend.position = c(0.12,0.3), legend.key.size = unit(0.18, "in"),
        plot.margin = margin(t = -0.1, r = 0.1, b = -0.2, l = 0.1, unit = "in"))
S1B


data$cuts2 <- cut(data$PL_re, breaks = c(0, 1, 5, 10, 50, 3000))
pal <- c("#f7f7f7", "#fddbc7", "#f4a582", "#d6604d", "#b2182b")

filter <- data[data$peatlands > 28.8,] ## cutoff areas with very few peatlands for visual clarity
S1C <- ggplot() +
  geom_tile(data = filter[!is.na(filter$cuts2),], aes(x= x, y = y, fill = cuts2)) +
  scale_fill_manual(values = pal, na.value = "#00000000",labels = c("< 1", "1-5", 
                                                                   "5-10", "10 - 50", "> 50"))+
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 9) +
  labs(fill = bquote("Peatland \n drained \npost 2010 "(km^2))) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_size_identity() +
  theme(legend.position = c(0.12,0.3), legend.key.size = unit(0.18, "in"),
        plot.margin = margin(t = -0.1, r = 0.1, b = -0.2, l = 0.1, unit = "in"))

plot_grid(S1A, S1B, S1C, ncol = 1, labels = c("a", "b", "c"), label_size = 11)

ggsave(filename = "Figures/Extended_Figures/Drained_peatland_v3.png",
       plot = last_plot(), bg = "white",
       width = 5, 
       height = 7.9, 
       unit = "in",
       dpi = 300)

}


### Auxiliary map data inputs
### 3 panel figure including biomes, N fertilizer application, and flood risk maps

{
### Biomes

biomes <- raster("Data_sources/Extracted_datafiles/Koppen_CZ_classified.tif")
map <- as.data.frame(biomes, xy = TRUE)

CZ <- ggplot() +
  geom_raster(data = map, aes(x= x, y = y, fill = as.factor(Koppen_CZ_classified))) +
  scale_fill_manual(values = c( "#ababab66", "darkgreen", "aquamarine3"), na.value = "#00000000",
                    labels = c("Boreal", "Temperate", "Tropical", " ")) +
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 9) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  theme(legend.position = c(0.1,0.4), legend.key.height = unit(0.18, "in"),
        legend.key.width = unit(0.15, "in"),
        legend.background = element_rect(fill="#FFFFFF"),
        plot.margin = margin(t = -0.1, r = 0.1, b = -0.3, l = 0.1, unit = "in")) +
  labs(fill = " ") 


### Figure S3. Global N fertilizer application map

### global N input  in kg/ha
data$cuts <- cut(data$N_fert, breaks = c(0, 5, 25, 50, 100, 200))

pal <- c("#f7f7f7", "#fee0b6", "#fdb863", "#e08214", "#b35806")

NF <- ggplot() +
  geom_tile(data = data[!is.na(data$cuts),], aes(x= x, y = y, fill = cuts)) +
  scale_fill_manual(values = pal, na.value = "#00000000", labels = c("0 - 5", "5 - 25", "25 - 50",
                                                                     "50 - 100", "> 100"))+
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 9) +
  labs(fill = "Nitrogen fertilizer \n applied (kg/ha)") +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_size_identity() +
  theme(legend.position = c(0.12,0.31), legend.key.size = unit(0.18, "in"),
        plot.margin = margin(t = -0.2, r = 0.1, b = -0.3, l = 0.1, unit = "in"))


### Figure S4. Global flood risk map

summary(data$flood/data$cell_area*100)
data$cuts <- cut(data$flood/data$cell_area*100, breaks = c(0, 20, 40, 60, 80, 100))
table(data$cuts)

pal <- c("#f7f7f7", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2")

FR <- ggplot() +
  geom_tile(data = data[!is.na(data$cuts),], aes(x= x, y = y, fill = cuts)) +
  scale_fill_manual(values = pal, na.value = "#00000000", labels = c("0 - 20", "20 - 40", "40 - 60",
                                                                     "60 - 80", "80 - 100"))+
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 9) +
  labs(fill = "100-year flood \n risk (% area)") +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_size_identity() +
  theme(legend.position = c(0.12,0.31), legend.key.size = unit(0.18, "in"),
        plot.margin = margin(t = -0.2, r = 0.1, b = -0.3, l = 0.1, unit = "in"))




plot_grid(CZ, NF, FR, ncol = 1, labels = c("a", "b", "c"), label_size = 11)

ggsave(filename = "Figures/Extended_Figures/Aux_data_inputs_v3.png",
       plot = last_plot(), bg = "white",
       width = 5, 
       height = 7.9, 
       unit = "in",
       dpi = 300)

}


#### Multi-objective comparison -- recently drained peatland only

{
data <- read.csv("Data_sources/Extracted_datafiles/Data_filtered3.csv")
MC_2010 <- read.csv("Data_sources/Extracted_datafiles/MC_2010_3.csv")


## Carbon first 
output <- data %>%
  dplyr::select(x, y, peatlands, peatland_loss_2010)
output <- cbind(output, MC_2010)
n <- 500## number of iterations in the MC
#### loop through all columns to find the order of restoration by low to high
#### area normalized emissions rate
fig <-  data.frame(matrix(NA, ncol = 1, nrow = 100))[-1]
x <- seq(1,100, 1)
y <- list()
for(j in 1:n) {
  Sub <- output[,c(4,j+4)]
  Sub$norm <- Sub[,2]/Sub$peatland_loss_2010
  Sub <- arrange(Sub, norm)
  Sub$cum_area <- cumsum(Sub$peatland_loss_2010)
  Sub$running_percent <- Sub$cum_area/sum(Sub$peatland_loss_2010, na.rm = TRUE)*100
  y <- list()
  for(i in 1:100) {
    subset <- Sub[Sub$running_percent <= i,]
    y[i] <- sum(subset[,2])/10^12
  }
  fig[,j] <- unlist(y)
  print(j)
  print(Sys.time())
}
fig_long <- fig %>% pivot_longer(everything(), names_to = "series", values_to = "val")
fig_long$percent <- rep(1:100, each = 500)
fig_long$strategy <- "carbon"
fig_long$series <- rep(seq(1,500), 100)
#mean
fig.mean <- rowMeans(fig)
Vstat <- data.frame(x, fig.mean)


### Fertilizer 
output <- data %>%
  dplyr::select(x, y, peatlands, peatland_loss_2010, N_fert)
output <- cbind(output, MC_2010)
crop <-  data.frame(matrix(NA, ncol = 1, nrow = 100))[-1]
x <- seq(1,100, 1)
for(j in 1:n) {
  Sub <- output[,c(4,5, j+5)]  ## 5 = N_fert
  Sub$norm <- Sub[,3]/Sub$peatland_loss_2010
  Sub <- arrange(Sub, norm)
  Sub <- arrange(Sub, desc(N_fert))
  Sub$cum_area <- cumsum(Sub$peatland_loss_2010)
  Sub$running_percent <- Sub$cum_area/sum(Sub$peatland_loss_2010, na.rm = TRUE)*100
  y <- list ()
  for(i in 1:100) {
    subset <- Sub[Sub$running_percent <= i,]
    y[i] <- sum(subset[,3])/10^12
  }
  crop[,j] <- unlist(y)
  print(j)
  print(Sys.time())
}
crop_long <- crop %>% pivot_longer(everything(), names_to = "series", values_to = "val")
crop_long$percent <- rep(1:100, each = n)
crop_long$strategy <- "cropland"
crop_long$series <- rep(seq(501,1000), 100)
Vstat$crop.mean <- rowMeans(crop) 


### Flood risk first
output <- data %>%
  dplyr::select(x, y, peatlands, peatland_loss_2010, flood)
output <- cbind(output, MC_2010)
flood <-  data.frame(matrix(NA, ncol = 1, nrow = 100))[-1]
x <- seq(1,100, 1)
for(j in 1:n) {
  Sub <- output[,c(4,5, j+5)]  ## 5 = floodrisk
  Sub$norm <- Sub[,3]/Sub$peatland_loss_2010
  Sub <- arrange(Sub, norm)
  Sub <- arrange(Sub, desc(flood))
  Sub$cum_area <- cumsum(Sub$peatland_loss_2010)
  Sub$running_percent <- Sub$cum_area/sum(Sub$peatland_loss_2010, na.rm = TRUE)*100
  y <- list ()
  for(i in 1:100) {
    subset <- Sub[Sub$running_percent <= i,]
    y[i] <- sum(subset[,3])/10^12
  }
  flood[,j] <- unlist(y)
  print(j)
  print(Sys.time())
}
flood_long <- flood %>% pivot_longer(everything(), names_to = "series", values_to = "val")
flood_long$percent <- rep(1:100, each = n)
flood_long$strategy <- "flood risk"
flood_long$series <- rep(seq(501,1000), 100)
Vstat$flood.mean <- rowMeans(flood)


#### Figure (top)
fig.a <- rbind(fig_long, crop_long)
FIG.A <- ggplot(fig.a, aes(x = percent, y = val)) + 
  geom_line( aes(group = series, color = strategy), lwd = 0.3) +
  scale_color_manual(values = c("#25252515","#f0d72b15" )) +
  theme_bw(base_size = 9) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = 'none') +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(data = Vstat, aes(x = x, y = fig.mean), color = "black", lwd = 1) +
  geom_line(data = Vstat, aes(x = x, y = crop.mean), color = "#cc9a02", lwd = 1) +
  ylab(bquote("GHG emissions (Pg CO"[2]*"eq. yr"^{-1}*")")) +
  xlab("Peatland restored (%)") +
  annotate("text", x = 30, y = -0.13, size = 3, label = "Climate first") +
  annotate("text", x = 30, y = 0.01, size = 3, col = "#cc9a02", label = "Nitrogen first")

fig.b <- rbind(fig_long, flood_long)
FIG.B <- ggplot(fig.b, aes(x = percent, y = val)) + 
  geom_line( aes(group = series, color = strategy), lwd = 0.3) +
  scale_color_manual(values = c("#25252515","#60a6b515" )) +
  theme_bw(base_size = 9) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = 'none') +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(data = Vstat, aes(x = x, y = fig.mean), color = "black", lwd = 1) +
  geom_line(data = Vstat, aes(x = x, y = flood.mean), color = "#2b6a78", lwd = 1) +
  ylab(bquote("GHG emissions (Pg CO"[2]*"eq. yr"^{-1}*")")) +
  xlab("Peatland restored (%)") +
  annotate("text", x = 30, y = -0.13, size = 3, label = "Climate first") +
  annotate("text", x = 30, y = 0.01, size = 3, col = "#2b6a78", label = "Flood risk first")


top <- plot_grid(FIG.A, FIG.B, ncol = 2, 
                 labels = c('a', 'b'), label_size = 11)
top


#### figure overlapping objectives map -- RECENTLY DRAINED PEATLANDS ONLY


output <- data %>%
  dplyr::select(x,y,peatlands, peatland_loss_2010, N_fert, flood)
output$mean_MC <- rowMeans(MC_2010)     ## emissions in kg CO2e/year/gridcell
output$norm <- output$mean_MC/output$peatland_loss_2010   ## emissions in kg CO2e/km2/yr

#### CARBON FIRST
df <- output
df <- output[df$peatland_loss_2010 > 0,]

sort <- arrange(df, norm)
sort$cum_area <- cumsum(sort$peatland_loss_2010)
sort$running_percent <- sort$cum_area/sum(sort$peatland_loss_2010, na.rm = TRUE)*100
sort$carbon.top <- ifelse(sort$running_percent < 25, 1, 0)
#### N first
sort <- arrange(sort, desc(N_fert))
sort$cum_area <- cumsum(sort$peatland_loss_2010)
sort$running_percent <- sort$cum_area/sum(sort$peatland_loss_2010, na.rm = TRUE)*100
sort$fert.top <- ifelse(sort$running_percent < 25, 1, 0)
#### Flood first
sort <- arrange(sort, desc(flood))
sort$cum_area <- cumsum(sort$peatland_loss_2010)
sort$running_percent <- sort$cum_area/sum(sort$peatland_loss_2010, na.rm = TRUE)*100
sort$flood.top <- ifelse(sort$running_percent < 25, 1, 0)


## map
coasts <- st_read("Data_sources/Coastline/ne_110m_coastline.shp", quiet = TRUE)
sort$intersect <- ifelse(sort$carbon.top == 1 & sort$flood.top == 1 & sort$fert.top == 1, 3,
                         ifelse(sort$fert.top == 1 & sort$carbon.top == 1 , 1,
                                ifelse(sort$flood.top == 1 & sort$carbon.top == 1 , 2, NA)))
map.filter <- sort[!is.na(sort$intersect),]
map.filter <- map.filter[map.filter$peatlands > 28.8,] ## cutoff areas with very few peatlands for visual clarity

map.bottom <- ggplot() +
  geom_tile(data = map.filter, aes(x= x, y = y, height=0.7, width=0.7, fill = as.factor(intersect))) +
  scale_fill_manual(values = c("#f0d72b" , "#578e9c","#b31722" ), na.value = "#00000000",
                    labels = c("Nitrogen + Climate", "Flood + Climate", "All three")) +
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 9) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  theme(legend.position = c(0.15,0.4), legend.key.height = unit(0.15, "in"),
        legend.key.width = unit(0.15, "in"),
        legend.background = element_rect(fill="#FFFFFF00")) +
  labs(fill = " ") 


plot_grid(top, map.bottom, ncol = 1, labels = c(" ", "c"), label_size = 11, 
          rel_heights = c(1.1,1))

ggsave(filename = "Figures/Extended_Figures/Multi-objective-RECENT-loss_v3.png",
       plot = last_plot(), bg = "white",
       width = 5, 
       height = 5, 
       unit = "in",
       dpi = 300)


}


#### Net emissions, rewetted end member (spaghetti plot)

{
data <- read.csv("Data_sources/Extracted_datafiles/Data_filtered3.csv")
MC_Rewet <- read.csv("Data_sources/Extracted_datafiles/MC_Rewet_3.csv")



output <- data %>%
  dplyr::select(x, y, peatlands, peatland_loss)
output <- cbind(output, MC_Rewet)

n <- 500  ## number of iterations in the MC

#### loop through all columns to find the order of restoration by low to high
#### area normalized emissions rate

S1 <-  data.frame(matrix(NA, ncol = 1, nrow = 100))[-1]
x <- seq(1,100, 1)
y <- list()

for(j in 1:n) {
  Sub <- output[,c(4,j+4)]
  Sub$norm <- Sub[,2]/Sub$ peatland_loss
  Sub <- arrange(Sub, norm)
  Sub$cum_area <- cumsum(Sub$ peatland_loss)
  Sub$running_percent <- Sub$cum_area/sum(Sub$ peatland_loss, na.rm = TRUE)*100
  
  y <- list()
  for(i in 1:100) {
    subset <- Sub[Sub$running_percent <= i,]
    y[i] <- sum(subset[,2])/10^12
  }
  S1[,j] <- unlist(y)
  print(j)
  print(Sys.time())
}


## Pivot data to long format and find mean for each % restoration
S1_long <- S1 %>% pivot_longer(everything(), names_to = "series", values_to = "val")
S1_long$x <- rep(1:100, each = n)
#mean
S1.mean <- rowMeans(S1)
Vstat <- data.frame(x, S1.mean)


## Random restoration (rr) -- shuffle output of monte carlo for random draw
rr <- data.frame(matrix(NA, ncol = 1, nrow = 100))[-1]
set.seed(123)
shuffle <- output[sample(1:nrow(output)),]
shuffle$cum_area <- cumsum(shuffle$ peatland_loss)
shuffle$running_percent <- shuffle$cum_area/sum(shuffle$ peatland_loss, na.rm = TRUE)*100

for(j in 1:n) {
  Sub <- shuffle[,c(506,j+4)]
  
  y2 <- list()
  for(i in 1:100) {
    subset <- Sub[Sub$running_percent <= i,]
    y2[i] <- sum(subset[,2])/10^12
  }
  rr[,j] <- unlist(y2)
  print(j)
  print(Sys.time())
}

Vstat$rr.mean <- rowMeans(rr)


## pivot longer to creat a figure with all MCs as a line

S1_long <- S1 %>% pivot_longer(everything(), names_to = "series", values_to = "val")
S1_long$percent <- rep(1:100, each = 500)
S1_long$strategy <- "carbon"
S1_long$series <- rep(seq(501,1000), 100)

rr_long <- rr %>% pivot_longer(everything(), names_to = "series", values_to = "val")
rr_long$percent <- rep(1:100, each = 500)
rr_long$strategy <- "random"
rr_long$series <- rep(seq(1, 500), 100)


S1_rr <- rbind(rr_long, S1_long )

ggplot(S1_rr, aes(x = percent, y = val)) + 
  geom_line( aes(group = series, color = strategy), lwd = 0.3) +
  scale_color_manual(values = c("#25252515", "#ff000015" )) +
  scale_x_continuous(sec.axis = sec_axis(~.*703639/100, name = bquote("Drained peatland restored (km"^2*")"))) +
  theme_bw(base_size = 9) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = 'none') +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(data = Vstat, aes(x = x, y = rr.mean), color = "#bf0404", lwd = 1) +
  geom_line(data = Vstat, aes(x = x, y = S1.mean), color = "black", lwd = 1) +
  ylab(bquote("GHG emissions (Pg CO"[2]*"eq. yr"^{-1}*")")) +
  xlab("Drained peatland restored (%)") +
  ylim(-6.5,5) +
  annotate("text", x = 25, y = -4, size = 3, label = "Strategic restoration") +
  annotate("text", x = 25, y = 2, size = 3, col = "red", label = "Random restoration")

ggsave(filename = "Figures/Extended_Figures/S5_Rewet_v_random_v3.png",
       plot = last_plot(), bg = "white",
       width = 4, 
       height = 4, 
       unit = "in",
       dpi = 300)
}


#### Emissions over 100-year time horizons

{
  
  ## OG WAY _ SGWP20 
  
  data <- read.csv("Data_sources/Extracted_datafiles/Data_filtered3.csv")
  MC_SGWP20 <- read.csv("Data_sources/Extracted_datafiles/MC_SGWP20_3.csv")
  

  output <- data %>%
    dplyr::select(x, y, peatlands, peatland_loss)
  output <- cbind(output, MC_SGWP20)
  
  n <- 500  ## number of iterations in the MC
  
  #### loop through all columns to find the order of restoration by low to high
  #### area normalized emissions rate
  
  S1 <-  data.frame(matrix(NA, ncol = 1, nrow = 100))[-1]
  x <- seq(1,100, 1)
  y <- list()
  
  for(j in 1:n) {
    Sub <- output[,c(4,j+4)]
    Sub$norm <- Sub[,2]/Sub$ peatland_loss
    Sub <- arrange(Sub, norm)
    Sub$cum_area <- cumsum(Sub$ peatland_loss)
    Sub$running_percent <- Sub$cum_area/sum(Sub$ peatland_loss, na.rm = TRUE)*100
    
    y <- list()
    for(i in 1:100) {
      subset <- Sub[Sub$running_percent <= i,]
      y[i] <- sum(subset[,2])/10^12
    }
    S1[,j] <- unlist(y)
    print(j)
    print(Sys.time())
  }
  
  
  ## Pivot data to long format and find mean for each % restoration
  S1_long <- S1 %>% pivot_longer(everything(), names_to = "series", values_to = "val")
  S1_long$x <- rep(1:100, each = n)
  #mean
  S1.mean <- rowMeans(S1)
  Vstat <- data.frame(x, S1.mean)
  
  
  ## Random restoration (rr) -- shuffle output of monte carlo for random draw
  rr <- data.frame(matrix(NA, ncol = 1, nrow = 100))[-1]
  set.seed(123)
  shuffle <- output[sample(1:nrow(output)),]
  shuffle$cum_area <- cumsum(shuffle$ peatland_loss)
  shuffle$running_percent <- shuffle$cum_area/sum(shuffle$ peatland_loss, na.rm = TRUE)*100
  
  for(j in 1:n) {
    Sub <- shuffle[,c(506,j+4)]
    
    y2 <- list()
    for(i in 1:100) {
      subset <- Sub[Sub$running_percent <= i,]
      y2[i] <- sum(subset[,2])/10^12
    }
    rr[,j] <- unlist(y2)
    print(j)
    print(Sys.time())
  }
  
  Vstat$rr.mean <- rowMeans(rr)
  ## pivot longer to creat a figure with all MCs as a line
  S1_long <- S1 %>% pivot_longer(everything(), names_to = "series", values_to = "val")
  S1_long$percent <- rep(1:100, each = 500)
  S1_long$strategy <- "carbon"
  S1_long$series <- rep(seq(501,1000), 100)

  rr_long <- rr %>% pivot_longer(everything(), names_to = "series", values_to = "val")
  rr_long$percent <- rep(1:100, each = 500)
  rr_long$strategy <- "random"
  rr_long$series <- rep(seq(1, 500), 100)

  S1_rr <- rbind(rr_long, S1_long )
  
  
  #### USING SGWP100
  
  
  MC_SGWP100 <- read.csv("Data_sources/Extracted_datafiles/MC_SGWP100_3.csv")
  
  
  
  output <- data %>%
    dplyr::select(x, y, peatlands, peatland_loss)
  output <- cbind(output, MC_SGWP100)
  
  n <- 500  ## number of iterations in the MC
  
  #### loop through all columns to find the order of restoration by low to high
  #### area normalized emissions rate
  
  S1 <-  data.frame(matrix(NA, ncol = 1, nrow = 100))[-1]
  x <- seq(1,100, 1)
  y <- list()
  
  for(j in 1:n) {
    Sub <- output[,c(4,j+4)]
    Sub$norm <- Sub[,2]/Sub$ peatland_loss
    Sub <- arrange(Sub, norm)
    Sub$cum_area <- cumsum(Sub$ peatland_loss)
    Sub$running_percent <- Sub$cum_area/sum(Sub$ peatland_loss, na.rm = TRUE)*100
    
    y <- list()
    for(i in 1:100) {
      subset <- Sub[Sub$running_percent <= i,]
      y[i] <- sum(subset[,2])/10^12
    }
    S1[,j] <- unlist(y)
    print(j)
    print(Sys.time())
  }
  
  
  # ## Pivot data to long format and find mean for each % restoration
  # S1_long <- S1 %>% pivot_longer(everything(), names_to = "series", values_to = "val")
  # S1_long$x <- rep(1:100, each = n)
  # #mean
  # S1.mean <- rowMeans(S1)
  # Vstat <- data.frame(x, S1.mean)
  
  
  ## Random restoration (rr) -- shuffle output of monte carlo for random draw
  rr <- data.frame(matrix(NA, ncol = 1, nrow = 100))[-1]
  set.seed(123)
  shuffle <- output[sample(1:nrow(output)),]
  shuffle$cum_area <- cumsum(shuffle$ peatland_loss)
  shuffle$running_percent <- shuffle$cum_area/sum(shuffle$ peatland_loss, na.rm = TRUE)*100
  
  for(j in 1:n) {
    Sub <- shuffle[,c(506,j+4)]
    
    y2 <- list()
    for(i in 1:100) {
      subset <- Sub[Sub$running_percent <= i,]
      y2[i] <- sum(subset[,2])/10^12
    }
    rr[,j] <- unlist(y2)
    print(j)
    print(Sys.time())
  }
  
  #Vstat$rr.mean <- rowMeans(rr)
  
  
  ## pivot longer to creat a figure with all MCs as a line
  
  S100_long <- S1 %>% pivot_longer(everything(), names_to = "series", values_to = "val")
  S100_long$percent <- rep(1:100, each = 500)
  S100_long$strategy <- "carbon"
  S100_long$series <- rep(seq(501,1000), 100)

  
  rr100_long <- rr %>% pivot_longer(everything(), names_to = "series", values_to = "val")
  rr100_long$percent <- rep(1:100, each = 500)
  rr100_long$strategy <- "random"
  rr100_long$series <- rep(seq(1, 500), 100)

  S1_rr100 <- rbind(rr100_long, S100_long )
  
  top.left <- ggplot(S1_rr100, aes(x = percent, y = val)) + 
    geom_line( aes(group = series, color = strategy), lwd = 0.3) +
    scale_color_manual(values = c("#25252515", "#ff000015" )) +
    scale_x_continuous(sec.axis = sec_axis(~.*703639/100, name = bquote("Drained peatland restored (km"^2*")"))) +
    theme_bw(base_size = 9) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          legend.position = 'none') +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_line(data = Vstat, aes(x = x, y = rr.mean), color = "#bf0404", lwd = 1) +
    geom_line(data = Vstat, aes(x = x, y = S1.mean), color = "black", lwd = 1) +
    ylab(bquote("GHG emissions (Pg CO"[2]*"eq. yr"^{-1}*")")) +
    xlab("Drained peatland restored (%)") +
    ylim(-3,0.2) +
    annotate("text", x = 25, y = -2.4, size = 3, label = "Strategic restoration") +
    annotate("text", x = 75, y = -0.2, size = 3, col = "red", label = "Random restoration")
  
  
  
  ###### box plot - GWP20 - GWP100 compare
  
  
  P1 <- S1_long[S1_long$percent == 1,]
  P3 <- S1_long[S1_long$percent ==3,]
  P10 <- S1_long[S1_long$percent ==10,]
  P30 <- S1_long[S1_long$percent ==30,]
  P100 <- S1_long[S1_long$percent ==100,]
  
  ## reporting metrics -- restor all (100%)
  restore_all_SGWP20 <- mean(P100$val)
  sd_restore_all_SGWP20 <- sd(P100$val)
  
  C1 <- data.frame(rep("Strategic - SGWP20", 500), rep("1%", 500), P1$val) #Pg
  names(C1) <- c("strategy", "percent_restore", "net_emissions")
  C3 <- data.frame(rep("Strategic - SGWP20", 500), rep("3%", 500), P3$val) #Pg
  names(C3) <- c("strategy", "percent_restore", "net_emissions")
  C10 <- data.frame(rep("Strategic - SGWP20", 500), rep("10%", 500), P10$val) #Pg
  names(C10) <- c("strategy", "percent_restore", "net_emissions")
  C25 <- data.frame(rep("Strategic - SGWP20", 500), rep("30%", 500), P30$val) #Pg
  names(C25) <- c("strategy", "percent_restore", "net_emissions")
  
  P1 <- S100_long[S100_long$percent == 1,]
  P3 <- S100_long[S100_long$percent ==3,]
  P10 <- S100_long[S100_long$percent ==10,]
  P30 <- S100_long[S100_long$percent ==30,]
  P100 <- S100_long[S100_long$percent ==100,]
  
  ## reporting metrics -- restor all (100%)
  restore_all_SGWP100 <- mean(P100$val)
  sd_restore_all_SGWP100 <- sd(P100$val)

  
  A1 <- data.frame(rep("Strategic - SGWP100", 500), rep("1%", 500), P1$val) #Pg
  names(A1) <- c("strategy", "percent_restore", "net_emissions")
  A3 <- data.frame(rep("Strategic - SGWP100", 500), rep("3%", 500), P3$val) #Pg
  names(A3) <- c("strategy", "percent_restore", "net_emissions")
  A10 <- data.frame(rep("Strategic - SGWP100", 500), rep("10%", 500), P10$val) #Pg
  names(A10) <- c("strategy", "percent_restore", "net_emissions")
  A25 <- data.frame(rep("Strategic - SGWP100", 500), rep("30%", 500), P30$val) #Pg
  names(A25) <- c("strategy", "percent_restore", "net_emissions")
  
  P1 <- rr_long[rr_long$percent == 1,]
  P3 <- rr_long[rr_long$percent ==3,]
  P10 <- rr_long[rr_long$percent ==10,]
  P30 <- rr_long[rr_long$percent ==30,]
  
  R1 <- data.frame(rep("Random - SGWP20", 500), rep("1%", 500), P1$val) #Pg
  names(R1) <- c("strategy", "percent_restore", "net_emissions")
  R3 <- data.frame(rep("Random - SGWP20", 500), rep("3%", 500), P3$val) #Pg
  names(R3) <- c("strategy", "percent_restore", "net_emissions")
  R10 <- data.frame(rep("Random - SGWP20", 500), rep("10%", 500), P10$val) #Pg
  names(R10) <- c("strategy", "percent_restore", "net_emissions")
  R25 <- data.frame(rep("Random - SGWP20", 500), rep("30%", 500), P30$val) #Pg
  names(R25) <- c("strategy", "percent_restore", "net_emissions")
  
  P1 <- rr100_long[rr100_long$percent == 1,]
  P3 <- rr100_long[rr100_long$percent ==3,]
  P10 <- rr100_long[rr100_long$percent ==10,]
  P30 <- rr100_long[rr100_long$percent ==30,]
  
  F1 <- data.frame(rep("Random - SGWP100", 500), rep("1%", 500), P1$val) #Pg
  names(F1) <- c("strategy", "percent_restore", "net_emissions")
  F3 <- data.frame(rep("Random - SGWP100", 500), rep("3%", 500), P3$val) #Pg
  names(F3) <- c("strategy", "percent_restore", "net_emissions")
  F10 <- data.frame(rep("Random - SGWP100", 500), rep("10%", 500), P10$val) #Pg
  names(F10) <- c("strategy", "percent_restore", "net_emissions")
  F25 <- data.frame(rep("Random - SGWP100", 500), rep("30%", 500), P30$val) #Pg
  names(F25) <- c("strategy", "percent_restore", "net_emissions")
  

  ### put it all together for the figure
  fig <- rbind(C1, C3, C10, C25, A1, A3, A10, A25, R1, R3, R10, R25, F1, F3, F10, F25)
  fig$strategy <- factor(fig$strategy, levels = c("Strategic - SGWP20", "Strategic - SGWP100", "Random - SGWP20", "Random - SGWP100"))
  fig$percent_restore <- factor(fig$percent_restore, levels = c("1%", "3%", "10%", "30%"))
  pal <- c("#666666", "#66666655", "#ff6161","#ff616155") # color blind friendly
  
  top.right <- ggplot(fig, aes(x = percent_restore, y = net_emissions, fill = strategy)) +
    geom_boxplot() +
    theme_bw(base_size = 9) +
    #scale_fill_viridis(discrete = TRUE, option = "viridis") +
    scale_fill_manual(values = pal) +
    ylab(bquote("GHG emissions (Pg CO"[2]*"eq. yr"^{-1}*")")) +
    xlab("Drained peatland restored (%)") +
    geom_hline(yintercept = 0, linetype = 2) +
    theme(legend.position = c(0.3, 0.2), legend.text = element_text(size = 7),
          legend.title = element_blank(),
          plot.margin = margin(t = 0.45, r = 0.2, b = 0.05, l = 0, unit = "in"))
  
  
  
  ###### map of difference between GWP20 and GWP100
  
  coasts <- st_read("Data_sources/Coastline/ne_110m_coastline.shp", quiet = TRUE)
  
  output <- data %>%
    dplyr::select(x,y,peatlands, peatland_loss)
  
  output$mean_MC20 <- rowMeans(MC_SGWP20)     ## emissions in kg CO2e/year/gridcell
  output$norm_20 <- output$mean_MC20/output$peatland_loss   ## emissions in kg CO2e/km2/yr
  
  output$mean_MC100 <- rowMeans(MC_SGWP100)     ## emissions in kg CO2e/year/gridcell
  output$norm_100 <- output$mean_MC100/output$peatland_loss   ## emissions in kg CO2e/km2/yr
  
  output <- output[output$peatlands > 28.8,] ## cutoff areas with very few peatlands for visual clarity
  
  sort <- arrange(output, norm_20)
  sort$cum_area <- cumsum(sort$peatland_loss)
  sort$running_percent <- sort$cum_area/sum(sort$peatland_loss, na.rm = TRUE)*100
  sort$GWP20.top <- ifelse(sort$running_percent < 25, 1, 0)
  
  
  sort <- arrange(sort, norm_100)
  sort$cum_area <- cumsum(sort$peatland_loss)
  sort$running_percent <- sort$cum_area/sum(sort$peatland_loss, na.rm = TRUE)*100
  sort$GWP100.top <- ifelse(sort$running_percent < 25, 1, 0)
  sort$map <- ifelse(sort$GWP20.top == 1 & sort$GWP100.top == 1, 1,
                     ifelse(sort$GWP100.top == 1 & sort$GWP20.top == 0, 2,
                            ifelse(sort$GWP20.top == 1 & sort$GWP100.top == 0, 3, NA)))
  
  table(sort$map)
  
  filter <- sort[!is.na(sort$map),]

  bottom.map <- ggplot() +
    geom_tile(data = filter, aes(x= x, y = y, height=0.7, width=0.7, fill = as.factor(map))) +
    scale_fill_manual(values = c( "#ababab66", "#b31722", "#578e9c"), na.value = "#00000000",
                      labels = c("Minimize emissions,\nboth time horzons", "Minimize emissions, \n100-year time horizon only", "Minimize emissions, \n20-year time horizon only")) +
    xlab(" ") +
    ylab(" ") +
    theme_bw(base_size = 9) +
    geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
    theme(legend.position = c(0.15,0.4), legend.key.height = unit(0.2, "in"),
          legend.key.width = unit(0.2, "in"), legend.spacing.y = unit(0.07, 'in'),
          legend.background = element_rect(fill="#FFFFFF00")) +
    guides(fill = guide_legend(byrow = TRUE))+
    labs(fill = " ") 
  

  top.row <- plot_grid(top.left, top.right, nrow = 1,
            labels = c("a", "b"), label_size = 11)
  
  
  plot_grid(top.row, bottom.map, ncol = 1, labels = c(" ", "c"),
            label_size = 11, rel_heights = c(4,3))
  
  ggsave(filename = "Figures/Extended_Figures/S2_GWP100_v3.png",
         plot = last_plot(), bg = "white",
         width = 6, 
         height = 7, 
         unit = "in",
         dpi = 300)
  
  
  
  metrics <- as.data.frame(t(matrix(c("net GHG, restora all, SGWP20", restore_all_SGWP20,
                                      "SD^", sd_restore_all_SGWP20, 
                                      "net GHG, restore all, SGWO100", restore_all_SGWP100, 
                                      "SD^", sd_restore_all_SGWP100), nrow = 2)))
  metrics
  
  
}



#### Map - difference in wetland methane emissions 2020-2099
{
data <- read.csv("Data_sources/Extracted_datafiles/Data_filtered3.csv")
coasts <- st_read("Data_sources/Coastline/ne_110m_coastline.shp", quiet = TRUE)
  
output <- data

output$feedback <- output$CH4_Wetland_2099 - output$CH4_Wetland
summary(output$feedback/10^3)  ## Gg CO2e/km2/yr
output$cuts <- cut(output$feedback/10^3, breaks = c(-Inf, -10, -1, -0.1, 0.1, 1, 10, Inf))
table(output$cuts)
pal <- c("#4393c3", '#92c5de', "#d1e5f0",  "#e7e7e7", "#fddbc7", '#f4a582', "#d6604d")

output <- output[output$peatlands > 28.8,] ## cutoff areas with very few peatlands for visual clarity

ggplot() +
  geom_tile(data = output, aes(x= x, y = y, fill = cuts)) +
  scale_fill_manual(values = pal, na.value = "#00000000", 
                    labels = c("< -10", "-10 to -1", "-1 to -0.1", 
                               "-0.1 to 0.1", "0.1 to 1", "1 to 10", "> 10"))+
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 8) +
  labs(fill = bquote("(Gg CO"[2]*"eq. yr"^-1*")")) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  scale_size_identity() +
  theme(legend.position = c(0.12,0.3), legend.key.height = unit(0.13, "in")) +
  annotate(geom = "text", x = -155, y = 17, 
         label = "GHG emissions", fontface = "bold",
         size = 2.5) 


ggsave(filename = "Figures/Extended_Figures/Methane_feedback_map_v3.png",
       plot = last_plot(), bg = "white",
       width = 5, 
       height = 2.67, 
       unit = "in",
       dpi = 600)


}
