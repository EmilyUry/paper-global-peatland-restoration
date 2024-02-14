


#### 

setwd("C:/Users/eury/OneDrive - Environmental Defense Fund - edf.org/Wetland-Restoration-GHG/Paper 1")

library(tidyverse)
library(raster)
library(sf)
library(cowplot)




data <- read.csv("Data_sources/Extracted_datafiles/Data_filtered.csv")
sum(data$peatland_loss)

MC_SGWP20 <- read.csv("Data_sources/Extracted_datafiles/MC_SGWP20.csv")
MC_SGWP100 <- read.csv("Data_sources/Extracted_datafiles/MC_SGWP100.csv")
MC_2099 <- read.csv("Data_sources/Extracted_datafiles/MC_2099.csv")
MC_2010 <- read.csv("Data_sources/Extracted_datafiles/MC_2010.csv")
MC_Rewet <- read.csv("Data_sources/Extracted_datafiles/MC_Rewet.csv")



#### summary stats
sum_MC <- colSums(MC_SGWP20)/10^12
S1.summary <- c("SGWP20", round(mean(sum_MC),2), round(sd(sum_MC),2), round(quantile(sum_MC, 0.05),2),  round(quantile(sum_MC, 0.95),2), round(max(sum_MC),2), round(min(sum_MC),2), "Pg CO2eq/year")
names(S1.summary) <- c("Scenario", "mean", "sd", "5th percentile", "95th percentile", "max", "min", "units")
sum_MC <- colSums(MC_SGWP100)/10^12
S2.summary <- c("SGWP100",  round(mean(sum_MC),2), round(sd(sum_MC),2), round(quantile(sum_MC, 0.05),2),  round(quantile(sum_MC, 0.95),2), round(max(sum_MC),2), round(min(sum_MC),2),  "Pg CO2eq/year")
sum_MC <- colSums(MC_2099)/10^12
S3.summary <- c("CH4_2099",  round(mean(sum_MC),2), round(sd(sum_MC),2), round(quantile(sum_MC, 0.05),2),  round(quantile(sum_MC, 0.95),2), round(max(sum_MC),2), round(min(sum_MC),2),  "Pg CO2eq/year")
sum_MC <- colSums(MC_2010)/10^12
S4.summary <- c("Drained 2010",  round(mean(sum_MC),2), round(sd(sum_MC),2), round(quantile(sum_MC, 0.05),2),  round(quantile(sum_MC, 0.95),2), round(max(sum_MC),2), round(min(sum_MC),2),  "Pg CO2eq/year")
sum_MC <- colSums(MC_Rewet)/10^12
S5.summary <- c("Rewet",  round(mean(sum_MC),2), round(sd(sum_MC),2), round(quantile(sum_MC, 0.05),2),  round(quantile(sum_MC, 0.95),2), round(max(sum_MC),2), round(min(sum_MC),2),  "Pg CO2eq/year")
summary.table <- as.data.frame(rbind(S1.summary, S2.summary, S3.summary, S4.summary, S5.summary))

write.csv(summary.table, "Data_sources/Extracted_datafiles/Scenario_summary.csv")





### Figure SGWP20 vs Random + Drained post 2010 vs Random

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
Vstat$S1.sd <- transform(S1, SD=apply(S1,1, sd, na.rm = TRUE))[,501]

emissions1p <- Vstat[1,2]
emissions1p.sd <- Vstat[1,3]


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
Vstat$rr.sd <- transform(rr, SD=apply(rr,1, sd, na.rm = TRUE))[,501]



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

plotx <- ggplot(S1_rr, aes(x = percent, y = val)) + 
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
  ylim(-2.5,0.2) +
  annotate("text", x = 70, y = -2.4, size = 3, label = "Strategic restoration") +
  annotate("text", x = 25, y = 0.1, size = 3, col = "red", label = "Random restoration")



comp <- data.frame(matrix(NA, ncol = 1, nrow = 100))[-1]
y3 <- list()
for(j in 1:n) {
  y3 <- rr[,j] - S1[,j]
  comp[,j] <- y3
} 
comp.mean <- rowMeans(comp)
comp.p <- data.frame(1:100, comp.mean)
## sort comp.p to max comp mean, to find the percent cited in line 155
max.diff <- max(comp.p$comp.mean)

#### peatland drained since 2010 only (S4)

output <- data %>%
  dplyr::select(x, y, peatlands, peatland_loss_2010)
output <- cbind(output, MC_2010)

output <- output[output$peatland_loss_2010 > 0,]

n <- 500  ## number of iterations in the MC

#### loop through all columns to find the order of restoration by low to high
#### area normalized emissions rate

S4 <-  data.frame(matrix(NA, ncol = 1, nrow = 100))[-1]
x <- seq(1,100, 1)
y <- list()

for(j in 1:n) {
  Sub <- output[,c(4,j+4)]
  Sub$norm <- Sub[,2]/Sub$ peatland_loss_2010
  Sub <- arrange(Sub, norm)
  Sub$cum_area <- cumsum(Sub$ peatland_loss_2010)
  Sub$running_percent <- Sub$cum_area/sum(Sub$ peatland_loss_2010, na.rm = TRUE)*100
  
  y <- list()
  for(i in 1:100) {
    subset <- Sub[Sub$running_percent <= i,]
    y[i] <- sum(subset[,2])/10^12
  }
  S4[,j] <- unlist(y)
  print(j)
  print(Sys.time())
}


## Pivot data to long format and find mean for each % restoration
S4_long <- S4 %>% pivot_longer(everything(), names_to = "series", values_to = "val")
S4_long$x <- rep(1:100, each = n)
#mean
S4.mean <- rowMeans(S4)
V4stat <- data.frame(x, S4.mean)


## Random restoration (R4) -- shuffle output of monte carlo for random draw
R4 <- data.frame(matrix(NA, ncol = 1, nrow = 100))[-1]
set.seed(123)
shuffle <- output[sample(1:nrow(output)),]
shuffle$cum_area <- cumsum(shuffle$ peatland_loss_2010)
shuffle$running_percent <- shuffle$cum_area/sum(shuffle$ peatland_loss_2010, na.rm = TRUE)*100

for(j in 1:n) {
  Sub <- shuffle[,c(506,j+4)]
  
  y2 <- list()
  for(i in 1:100) {
    subset <- Sub[Sub$running_percent <= i,]
    y2[i] <- sum(subset[,2])/10^12
  }
  R4[,j] <- unlist(y2)
  print(j)
  print(Sys.time())
}

V4stat$R4.mean <- rowMeans(R4)


## pivot longer to creat a figure with all MCs as a line

S4_long <- S4 %>% pivot_longer(everything(), names_to = "series", values_to = "val")
S4_long$percent <- rep(1:100, each = 500)
S4_long$strategy <- "carbon"
S4_long$series <- rep(seq(501,1000), 100)

R4_long <- R4 %>% pivot_longer(everything(), names_to = "series", values_to = "val")
R4_long$percent <- rep(1:100, each = 500)
R4_long$strategy <- "random"
R4_long$series <- rep(seq(1, 500), 100)


S4_R4 <- rbind(R4_long, S4_long )

plot4 <- ggplot(S4_R4, aes(x = percent, y = val)) + 
  geom_line( aes(group = series, color = strategy), lwd = 0.3) +
  scale_color_manual(values = c("#25252515", "#ff000015" )) +
  scale_x_continuous(sec.axis = sec_axis(~.*703639/100, name = bquote("Peatland drained post-2010 restored (km"^2*")"))) +
  theme_bw(base_size = 9) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = 'none') +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(data = V4stat, aes(x = x, y = R4.mean), color = "#bf0404", lwd = 1) +
  geom_line(data = V4stat, aes(x = x, y = S4.mean), color = "black", lwd = 1) +
  ylab(bquote("GHG emissions (Pg CO"[2]*"eq. yr"^{-1}*")")) +
  xlab("Peatland drained post-2010 restored (%)") +
  ylim(-0.13,0.01) +
  annotate("text", x = 55, y = -0.13, size = 3, label = "Strategic restoration") +
  annotate("text", x = 35, y = 0.005, size = 3, col = "red", label = "Random restoration")

plot4

options(scipen = 99)

plot_grid(plotx, plot4, ncol = 2, labels = c("a", "b"), label_size = 11)

ggsave(filename = "Figures/S1_SGWP20_S4_2010.png",
       plot = last_plot(),
       width = 6, 
       height = 3, 
       unit = "in",
       dpi = 300)




#####################################################
 
## multi-objective comparison (figure 4)
 
#####################################################

## Carbon first 
output <- data %>%
 dplyr::select(x, y, peatlands, peatland_loss)
output <- cbind(output, MC_SGWP20)
n <- 500## number of iterations in the MC
#### loop through all columns to find the order of restoration by low to high
#### area normalized emissions rate
fig <-  data.frame(matrix(NA, ncol = 1, nrow = 100))[-1]
x <- seq(1,100, 1)
y <- list()
for(j in 1:n) {
 Sub <- output[,c(4,j+4)]
 Sub$norm <- Sub[,2]/Sub$peatland_loss
 Sub <- arrange(Sub, norm)
 Sub$cum_area <- cumsum(Sub$peatland_loss)
 Sub$running_percent <- Sub$cum_area/sum(Sub$peatland_loss, na.rm = TRUE)*100
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
  dplyr::select(x, y, peatlands, peatland_loss, N_fert)
output <- cbind(output, MC_SGWP20)
crop <-  data.frame(matrix(NA, ncol = 1, nrow = 100))[-1]
x <- seq(1,100, 1)
for(j in 1:n) {
  Sub <- output[,c(4,5, j+5)]  ## 5 = N_fert
  Sub$norm <- Sub[,3]/Sub$peatland_loss
  Sub <- arrange(Sub, norm)
  Sub <- arrange(Sub, desc(N_fert))
  Sub$cum_area <- cumsum(Sub$peatland_loss)
  Sub$running_percent <- Sub$cum_area/sum(Sub$peatland_loss, na.rm = TRUE)*100
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
  dplyr::select(x, y, peatlands, peatland_loss, flood)
output <- cbind(output, MC_SGWP20)
flood <-  data.frame(matrix(NA, ncol = 1, nrow = 100))[-1]
x <- seq(1,100, 1)
for(j in 1:n) {
  Sub <- output[,c(4,5, j+5)]  ## 5 = floodrisk
  Sub$norm <- Sub[,3]/Sub$peatland_loss
  Sub <- arrange(Sub, norm)
  Sub <- arrange(Sub, desc(flood))
  Sub$cum_area <- cumsum(Sub$peatland_loss)
  Sub$running_percent <- Sub$cum_area/sum(Sub$peatland_loss, na.rm = TRUE)*100
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
  


#### Feedback first
data$CH4_feedback <- data$CH4_Zhang_2099 - data$CH4_Zhang_2020
output <- data %>%
  dplyr::select(x, y, peatlands, peatland_loss, CH4_feedback)
output <- cbind(output, MC_SGWP20)
feedback <-  data.frame(matrix(NA, ncol = 1, nrow = 100))[-1]
x <- seq(1,100, 1)
#y <- list()
for(j in 1:n) {
  Sub <- output[,c(4,5, j+5)]  ## 5 = floodrisk
  Sub$norm <- Sub[,3]/Sub$peatland_loss
  Sub <- arrange(Sub, norm)
  Sub <- arrange(Sub, (CH4_feedback))
  Sub$cum_area <- cumsum(Sub$peatland_loss)
  Sub$running_percent <- Sub$cum_area/sum(Sub$peatland_loss, na.rm = TRUE)*100
  y <- list ()
  for(i in 1:100) {
    subset <- Sub[Sub$running_percent <= i,]
    y[i] <- sum(subset[,3])/10^12
  }
  feedback[,j] <- unlist(y)
  print(j)
  print(Sys.time())
}
feedback_long <- feedback %>% pivot_longer(everything(), names_to = "series", values_to = "val")
feedback_long$percent <- rep(1:100, each = n)
feedback_long$strategy <- "feedback"
feedback_long$series <- rep(seq(501,1000), 100)
Vstat$feedback.mean <- rowMeans(feedback)


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
  annotate("text", x = 30, y = -2, size = 3, label = "Climate first") +
  annotate("text", x = 30, y = 0.5, size = 3, col = "#cc9a02", label = "Nitrogen first")

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
  annotate("text", x = 30, y = -2, size = 3, label = "Climate first") +
  annotate("text", x = 30, y = 0.5, size = 3, col = "#2b6a78", label = "Flood risk first")

fig.c <- rbind(fig_long, feedback_long)
FIG.C <- ggplot(fig.c, aes(x = percent, y = val)) + 
  geom_line( aes(group = series, color = strategy), lwd = 0.3) +
  scale_color_manual(values = c("#25252515","#763c9115" )) +
  theme_bw(base_size = 9) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = 'none') +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(data = Vstat, aes(x = x, y = fig.mean), color = "black", lwd = 1) +
  geom_line(data = Vstat, aes(x = x, y = feedback.mean), color = "#54246b", lwd = 1) +
  ylab(bquote("GHG emissions (Pg CO"[2]*"eq. yr"^{-1}*")")) +
  xlab("Peatland restored (%)") +
  annotate("text", x = 30, y = -2, size = 3, label = "Current methane") +
  annotate("text", x = 30, y = 0.5, size = 3, col = "#54246b", label = "Future methane")

top <- plot_grid(FIG.A, FIG.B, FIG.C, ncol = 3, 
          labels = c('a', 'b', 'c'), label_size = 12)
top



 
### Box plot
output <- data %>%
 dplyr::select(x,y,peatlands, peatland_loss, N_fert, flood)
output$mean_MC <- rowMeans(MC_SGWP20)     ## emissions in kg CO2e/year/gridcell
output$norm <- output$mean_MC/output$peatland_loss   ## emissions in kg CO2e/km2/yr

#### CARBON FIRST
df <- cbind(output, MC_SGWP20)
sort <- arrange(df, norm)
sort$cum_area <- cumsum(sort$peatland_loss)
sort$running_percent <- sort$cum_area/sum(sort$peatland_loss, na.rm = TRUE)*100
P1 <- sort[sort$running_percent <= 1,]
P1MC <- P1[, 9:508]
P1sum.carbon <- colSums(P1MC)
P3 <- sort[sort$running_percent <= 3,]
P3MC <- P3[, 9:508]
P3sum.carbon <- colSums(P3MC)
P10 <- sort[sort$running_percent <= 10,]
P10MC <- P10[, 9:508]
P10sum.carbon <- colSums(P10MC)
P25 <- sort[sort$running_percent <= 25,]
P25MC <- P25[, 9:508]
P25sum.carbon <- colSums(P25MC)
C1 <- data.frame(rep("Climate first", 500), rep("1%", 500), P1sum.carbon/10^12) #Pg
names(C1) <- c("strategy", "percent_restore", "net_emissions")
C3 <- data.frame(rep("Climate first", 500), rep("3%", 500), P3sum.carbon/10^12) #Pg
names(C3) <- c("strategy", "percent_restore", "net_emissions")
C10 <- data.frame(rep("Climate first", 500), rep("10%", 500), P10sum.carbon/10^12) #Pg
names(C10) <- c("strategy", "percent_restore", "net_emissions")
C25 <- data.frame(rep("Climate first", 500), rep("25%", 500), P25sum.carbon/10^12) #Pg
names(C25) <- c("strategy", "percent_restore", "net_emissions")

###### Ag first
df <- cbind(output, MC_SGWP20)
sort <- arrange(df, desc(N_fert))
sort$cum_area <- cumsum(sort$peatland_loss)
sort$running_percent <- sort$cum_area/sum(sort$peatland_loss, na.rm = TRUE)*100
P1 <- sort[sort$running_percent <= 1,]
P1MC <- P1[, 9:508]
P1sum.carbon <- colSums(P1MC)
P3 <- sort[sort$running_percent <= 3,]
P3MC <- P3[, 9:508]
P3sum.carbon <- colSums(P3MC)
P10 <- sort[sort$running_percent <= 10,]
P10MC <- P10[, 9:508]
P10sum.carbon <- colSums(P10MC)
P25 <- sort[sort$running_percent <= 25,]
P25MC <- P25[, 9:508]
P25sum.carbon <- colSums(P25MC)
A1 <- data.frame(rep("Nitrogen first", 500), rep("1%", 500), P1sum.carbon/10^12) #Pg
names(A1) <- c("strategy", "percent_restore", "net_emissions")
A3 <- data.frame(rep("Nitrogen first", 500), rep("3%", 500), P3sum.carbon/10^12) #Pg
names(A3) <- c("strategy", "percent_restore", "net_emissions")
A10 <- data.frame(rep("Nitrogen first", 500), rep("10%", 500), P10sum.carbon/10^12) #Pg
names(A10) <- c("strategy", "percent_restore", "net_emissions")
A25 <- data.frame(rep("Nitrogen first", 500), rep("25%", 500), P25sum.carbon/10^12) #Pg
names(A25) <- c("strategy", "percent_restore", "net_emissions")


#### Flood first

df <- cbind(output, MC_SGWP20)
sort <- arrange(df, desc(flood))
sort$cum_area <- cumsum(sort$peatland_loss)
sort$running_percent <- sort$cum_area/sum(sort$peatland_loss, na.rm = TRUE)*100
P1 <- sort[sort$running_percent <= 1,]
P1MC <- P1[, 9:508]
P1sum.carbon <- colSums(P1MC)
P3 <- sort[sort$running_percent <= 3,]
P3MC <- P3[, 9:508]
P3sum.carbon <- colSums(P3MC)
P10 <- sort[sort$running_percent <= 10,]
P10MC <- P10[, 9:508]
P10sum.carbon <- colSums(P10MC)
P25 <- sort[sort$running_percent <= 25,]
P25MC <- P25[, 9:508]
P25sum.carbon <- colSums(P25MC)
F1 <- data.frame(rep("Flood risk first", 500), rep("1%", 500), P1sum.carbon/10^12) #Pg
names(F1) <- c("strategy", "percent_restore", "net_emissions")
F3 <- data.frame(rep("Flood risk first", 500), rep("3%", 500), P3sum.carbon/10^12) #Pg
names(F3) <- c("strategy", "percent_restore", "net_emissions")
F10 <- data.frame(rep("Flood risk first", 500), rep("10%", 500), P10sum.carbon/10^12) #Pg
names(F10) <- c("strategy", "percent_restore", "net_emissions")
F25 <- data.frame(rep("Flood risk first", 500), rep("25%", 500), P25sum.carbon/10^12) #Pg
names(F25) <- c("strategy", "percent_restore", "net_emissions")


#### Random

df <- cbind(output, MC_SGWP20)
## instead of sort, we shuffle
set.seed(123)
sort <- df[sample(1:nrow(df)),] 
sort$cum_area <- cumsum(sort$peatland_loss)
sort$running_percent <- sort$cum_area/sum(sort$peatland_loss, na.rm = TRUE)*100
P1 <- sort[sort$running_percent <= 1,]
P1MC <- P1[, 9:508]
P1sum.carbon <- colSums(P1MC)
P3 <- sort[sort$running_percent <= 3,]
P3MC <- P3[, 9:508]
P3sum.carbon <- colSums(P3MC)
P10 <- sort[sort$running_percent <= 10,]
P10MC <- P10[, 9:508]
P10sum.carbon <- colSums(P10MC)
P25 <- sort[sort$running_percent <= 25,]
P25MC <- P25[, 9:508]
P25sum.carbon <- colSums(P25MC)
R1 <- data.frame(rep("Random", 500), rep("1%", 500), P1sum.carbon/10^12) #Pg
names(R1) <- c("strategy", "percent_restore", "net_emissions")
R3 <- data.frame(rep("Random", 500), rep("3%", 500), P3sum.carbon/10^12) #Pg
names(R3) <- c("strategy", "percent_restore", "net_emissions")
R10 <- data.frame(rep("Random", 500), rep("10%", 500), P10sum.carbon/10^12) #Pg
names(R10) <- c("strategy", "percent_restore", "net_emissions")
R25 <- data.frame(rep("Random", 500), rep("25%", 500), P25sum.carbon/10^12) #Pg
names(R25) <- c("strategy", "percent_restore", "net_emissions")



## feedback first
output <- data %>%
  dplyr::select(x,y,peatlands, peatland_loss, CH4_feedback)
df <- cbind(output, MC_SGWP20)
sort <- arrange(df, CH4_feedback)
sort$cum_area <- cumsum(sort$peatland_loss)
sort$running_percent <- sort$cum_area/sum(sort$peatland_loss, na.rm = TRUE)*100
P1 <- sort[sort$running_percent <= 1,]
P1MC <- P1[, 6:505]
P1sum.carbon <- colSums(P1MC)
P3 <- sort[sort$running_percent <= 3,]
P3MC <- P3[, 6:505]
P3sum.carbon <- colSums(P3MC)
P10 <- sort[sort$running_percent <= 10,]
P10MC <- P10[, 6:505]
P10sum.carbon <- colSums(P10MC)
P25 <- sort[sort$running_percent <= 25,]
P25MC <- P25[, 6:505]
P25sum.carbon <- colSums(P25MC)
net.feedbackfirst.25p <- mean(P25sum.carbon)/10^12
sd.feedbackfirst.25p <-sd(P25sum.carbon)/10^12
X1 <- data.frame(rep("Feedback", 500), rep("1%", 500), P1sum.carbon/10^12) #Pg
names(X1) <- c("strategy", "percent_restore", "net_emissions")
X3 <- data.frame(rep("Feedback", 500), rep("3%", 500), P3sum.carbon/10^12) #Pg
names(X3) <- c("strategy", "percent_restore", "net_emissions")
X10 <- data.frame(rep("Feedback", 500), rep("10%", 500), P10sum.carbon/10^12) #Pg
names(X10) <- c("strategy", "percent_restore", "net_emissions")
X25 <- data.frame(rep("Feedback", 500), rep("25%", 500), P25sum.carbon/10^12) #Pg
names(X25) <- c("strategy", "percent_restore", "net_emissions")



### put it all together for boxplot (bottom panel)

bplot <- rbind(C1, C3, C10, C25, X1, X3, X10, X25,
             A1, A3, A10, A25, F1, F3, F10, F25, R1, R3, R10, R25)
bplot$strategy <- factor(bplot$strategy, levels = c("Climate first", "Feedback", "Nitrogen first", 
                                                "Flood risk first", "Random"))
bplot$percent_restore <- factor(bplot$percent_restore, levels = c("1%", "3%", "10%", "25%"))
pal <- c("#666666","#54246b", "#f0d72b", "#2b6a78","#ff6161") # color blind friendly

bottom <- ggplot(bplot, aes(x = percent_restore, y = net_emissions, fill = strategy)) +
  geom_boxplot( fatten = 0.5, lwd = 0.2, outlier.size = 0.5) +
  theme_bw(base_size = 9) +
  #scale_fill_viridis(discrete = TRUE, option = "viridis") +
  scale_fill_manual(values = pal) +
  xlab("Peatland restored") +
  ylab(bquote("GHG emissions (Pg CO"[2]*"eq. yr"^{-1}*")")) +
  geom_hline(yintercept = 0, linetype = 2, lwd = 0.2) +
  theme(legend.position = c(0.22, 0.3), legend.text = element_text(size = 8),
        legend.title = element_blank(), 
        legend.direction="horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(ncol = 2))
bottom


plot_grid(top, bottom, labels = c(" ", "d"), nrow = 2, rel_heights = c(1.1,1), 
          label_y = 1.1, label_size = 12)


ggsave(filename = "Figures/Multi-objective-compare.png",
       plot = last_plot(), bg = "white",
       width = 6, 
       height = 5, 
       unit = "in",
       dpi = 300)


### metrics
CF <- bplot[bplot$strategy == "Climate first" & bplot$percent_restore == "10%",]
RR <- bplot[bplot$strategy == "Random" & bplot$percent_restore == "10%",]
median.10p.climate_first <- median(CF$net_emissions) #Pg CO2e/yr
sd.10p.climate_first <- sd(CF$net_emissions)
median.10p.random <- median(RR$net_emissions)
sd.10p.random <- sd(RR$net_emissions)

CF <- bplot[bplot$strategy == "Climate first" & bplot$percent_restore == "25%",]
RR <- bplot[bplot$strategy == "Random" & bplot$percent_restore == "25%",]
median.25p.climate_first <- median(CF$net_emissions) #Pg CO2e/yr
sd.25p.climate_first <- sd(CF$net_emissions)
median.25p.random <- median(RR$net_emissions)
sd.25p.random <- sd(RR$net_emissions)


CF <- bplot[bplot$strategy == "Flood risk first" & bplot$percent_restore == "25%",]
RR <- bplot[bplot$strategy == "Nitrogen first" & bplot$percent_restore == "25%",]
median.25p.flood_first <- median(CF$net_emissions) #Pg CO2e/yr
sd.25p.flood_first <- sd(CF$net_emissions)
median.25p.N_first <- median(RR$net_emissions)
sd.25p.N_first <- sd(RR$net_emissions)


FM <- bplot[bplot$strategy == "Feedback" & bplot$percent_restore == "25%",]
median.25p.feedback <- median(FM$net_emissions) #Pg CO2e/yr
sd.25p.feedback <- sd(FM$net_emissions)


## gains over random

CR.RR.10p <- median.10p.climate_first - median.10p.random
CR.RR.sd.10p <- sqrt(sd.10p.climate_first^2 + sd.10p.random^2)

CR.RR.25p <- median.25p.climate_first - median.25p.random
CR.RR.sd.25p <- sqrt(sd.25p.climate_first^2 + sd.25p.random^2)

## percent difference between climate first and flood first at 25% restoration
percent.dif.CF.FF <- (median.25p.climate_first - median.25p.flood_first)/median.25p.climate_first*100
percent.dif.CF.NF <- (median.25p.climate_first - median.25p.N_first)/median.25p.climate_first*100

summary <- as.data.frame(t(matrix(c("median, climate first, 10%", median.10p.climate_first,
                 "SD, climate first, 10%", sd.10p.climate_first,
                 "median, random, 10%", median.10p.random, 
                 "SD, random, 10%", sd.10p.random,
                 "Climate first minus random, 10%", CR.RR.10p, 
                 "SD, Climate first minus random, 10%", CR.RR.sd.10p,
                 "median, climate first, 25%", median.25p.climate_first,
                 "SD, climate first, 25%", sd.25p.climate_first,
                 "median, random, 25%", median.25p.random, 
                 "SD, random, 25%", sd.25p.random, 
                 "Climate first minus random, 25%", CR.RR.25p, 
                 "SD, Climate first minus random, 25%", CR.RR.sd.25p, 
                 "median, flood first, 25%", median.25p.flood_first,
                 "SD, flood first, 25%", sd.25p.flood_first,
                 "median, N first, 25%", median.25p.N_first,
                 "SD, N first, 25%", sd.25p.N_first, 
                 "median, feedback first, 25%", median.25p.feedback,
                 "SD, feedback first, 25%", sd.25p.feedback,
                 "Climate first minus flood first, percent difference", percent.dif.CF.FF,
                 "Climate first minus n first, percent difference", percent.dif.CF.NF), nrow = 2)))



#### map of top priorities for each objective
## where are the top 25 % of N use, flood risk, and climate benefit from restoration

output <- data %>%
  dplyr::select(x,y,peatlands, peatland_loss, N_fert, flood)
output$mean_MC <- rowMeans(MC_SGWP20)     ## emissions in kg CO2e/year/gridcell
output$norm <- output$mean_MC/output$peatland_loss   ## emissions in kg CO2e/km2/yr

#### CARBON FIRST
df <- output
sort <- arrange(df, norm)
sort$cum_area <- cumsum(sort$peatland_loss)
sort$running_percent <- sort$cum_area/sum(sort$peatland_loss, na.rm = TRUE)*100
sort$carbon.top <- ifelse(sort$running_percent < 25, 1, 0)
#### N first
sort <- arrange(sort, desc(N_fert))
sort$cum_area <- cumsum(sort$peatland_loss)
sort$running_percent <- sort$cum_area/sum(sort$peatland_loss, na.rm = TRUE)*100
sort$fert.top <- ifelse(sort$running_percent < 25, 1, 0)
#### Flood first
sort <- arrange(sort, desc(flood))
sort$cum_area <- cumsum(sort$peatland_loss)
sort$running_percent <- sort$cum_area/sum(sort$peatland_loss, na.rm = TRUE)*100
sort$flood.top <- ifelse(sort$running_percent < 25, 1, 0)


## map
coasts <- st_read("Data_sources/Coastline/ne_110m_coastline.shp", quiet = TRUE)
sort$intersect <- ifelse(sort$carbon.top == 1 & sort$flood.top == 1 & sort$fert.top == 1, 3,
                          ifelse(sort$fert.top == 1 & sort$carbon.top == 1 , 1,
                                 ifelse(sort$flood.top == 1 & sort$carbon.top == 1 , 2, NA)))
map.filter <- sort[!is.na(sort$intersect),]
map.top <- ggplot() +
  geom_tile(data = map.filter, aes(x= x, y = y, fill = as.factor(intersect))) +
  scale_fill_manual(values = c("#f0d72b" , "#578e9c","#b31722" ), na.value = "#00000000",
                    labels = c("Nitrogen + Climate", "Flood + Climate", "All three")) +
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 6) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  theme(legend.position = c(0.15,0.4), legend.key.height = unit(0.15, "in"),
        legend.key.width = unit(0.15, "in"),
        legend.background = element_rect(fill="#FFFFFF00")) +
  labs(fill = " ") 

## metrics
table(map.filter$intersect)
# 1 = both fertilizer and climate #447
# 2 = both flood and climate #2124
# 3 = all three
2124/447







#### map, methane now vs methane future
output <- data %>%
  dplyr::select(x,y,peatlands, peatland_loss, CH4_feedback)
output$mean_MC <- rowMeans(MC_SGWP20)     ## emissions in kg CO2e/year/gridcell
output$norm <- output$mean_MC/output$peatland_loss   ## emissions in kg CO2e/km2/yr
df <- output

## climate in 20 years
sort <- arrange(df, norm)
sort$cum_area <- cumsum(sort$peatland_loss)
sort$running_percent <- sort$cum_area/sum(sort$peatland_loss, na.rm = TRUE)*100
sort$carbon.top <- ifelse(sort$running_percent < 25, 1, 0)

## climate in 2099
sort <- arrange(sort, CH4_feedback)
sort$cum_area <- cumsum(sort$peatland_loss)
sort$running_percent <- sort$cum_area/sum(sort$peatland_loss, na.rm = TRUE)*100
sort$feedback.top <- ifelse(sort$running_percent < 25, 1, 0)

sort$intersect <- ifelse(sort$carbon.top == 1 & sort$feedback.top == 1, 1,
                         ifelse(sort$carbon.top == 1 , 3,
                                ifelse(sort$feedback.top == 1 , 2, NA)))

table(sort$intersect)

filter <- sort[!is.na(sort$intersect),]

map.bottom <- ggplot() +
  geom_tile(data = filter, aes(x= x, y = y, fill = as.factor(intersect))) +
  scale_fill_manual(values = c( "#cdcdcd", "#666666", "#54246b"), na.value = "#00000000",
                    labels = c("No change", "Prioritize", "Deprioritize")) +
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 6) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  theme(legend.position = c(0.11,0.4), legend.key.height = unit(0.15, "in"),
        legend.key.width = unit(0.15, "in"),
        legend.background = element_rect(fill="#FFFFFF00")) +
  labs(fill = " ") 



plot_grid(map.top, map.bottom, nrow = 2, labels = c("a", "b"), label_size = 11)

ggsave(filename = "Figures/Multi-objective-maps.png",
       plot = last_plot(), bg = "white",
       width = 3, 
       height = 2.9, 
       unit = "in",
       dpi = 600)









########### Metrics reported in the paper

global <- colSums(MC_SGWP20)
sum.S1 <- mean(global)/10^12
sd.S1 <- sd(global)/10^12

global <- colSums(MC_Rewet)
sum.S5 <- mean(global)/10^12
sd.S5 <- sd(global)/10^12


metrics <- data %>%
  dplyr::select(x, y, peatlands, peatland_loss)
metrics$mean_MC <- rowMeans(MC_SGWP20)     ## emissions in kg CO2e/year/gridcell
metrics$norm <- metrics$mean_MC/metrics$peatland_loss 
#GHGsinks.intact <- nrow(metrics[metrics$mean_MC < 0,])/nrow(metrics)*100
sinks <- metrics[metrics$mean_MC <0,]
GHGsinks.intact <- sum(sinks$peatland_loss)/sum(metrics$peatland_loss)

metrics <- data %>%
  dplyr::select(x, y, peatlands, peatland_loss)
metrics$mean_MC <- rowMeans(MC_Rewet)     ## emissions in kg CO2e/year/gridcell
metrics$norm <- metrics$mean_MC/metrics$peatland_loss 
#GHGsinks.rewetted <- nrow(metrics[metrics$mean_MC < 0,])/nrow(metrics)*100
sinks <- metrics[metrics$mean_MC <0,]
GHGsinks.rewetted <- sum(sinks$peatland_loss)/sum(metrics$peatland_loss)

summary <- as.data.frame(t(matrix(c("global emissions, restore to intact, GWP20", sum.S1,
                                    "sd emissions, restore to intact, GWP20", sd.S1,
                                    "global emissions, restore to rewet, GWP20", sum.S5,
                                    "sd emissions, restore to rewet, GWP20", sd.S5,
                                    "Percent of restored peatlands, GHG sinks", GHGsinks.intact,
                                    "Percent of rewetted peatlands, GHG sinks", GHGsinks.rewetted),
                                    nrow = 2)))

summary

max.diff # maximum difference between restored strategic and random (fig 3.)
         # occurs at 71% peatland area restored
emissions1p ### emissions from restoring the best 1% of drained peatlands
emissions1p.sd ## standard deviation ^

### Box plot
output <- data %>%
  dplyr::select(x,y,peatlands, peatland_loss, N_fert, flood)
output$mean_MC <- rowMeans(MC_SGWP20)     ## emissions in kg CO2e/year/gridcell
output$mean_sd <- transform(MC_SGWP20, SD=apply(MC_SGWP20,1, sd, na.rm = TRUE))[,501]
output$norm <- output$mean_MC/output$peatland_loss   ## emissions in kg CO2e/km2/yr

sort <- arrange(output, norm)
sort$cum_area <- cumsum(sort$peatland_loss)
sort$running_percent <- sort$cum_area/sum(sort$peatland_loss, na.rm = TRUE)*100
P1 <- sort[sort$running_percent <= 1,]

### cumulative emissions from only restoring top 1% of GHG sinks
sum(P1$mean_MC)/10^12
sum(P1$mean_sd)/10^12

### cumulative emissions from only restoring peatlands with net positive emissions
Ppos <- sort[sort$norm > 0,]
sum(Ppos$mean_MC)/10^12
sum(Ppos$mean_sd)/10^12


### cumulative emissions from only restoring top 10% of GHG sinks
P10 <- sort[sort$running_percent <= 10,]
sum(P10$mean_MC)/10^12
sum(P10$mean_sd)/10^12

### cumulative emissions from 10% restoration -- random fashion
set.seed(123)
shuffle <- sort[sample(1:nrow(sort)),]
shuffle$cum_area <- cumsum(shuffle$peatland_loss)
shuffle$running_percent <- shuffle$cum_area/sum(shuffle$peatland_loss, na.rm = TRUE)*100

R10 <- shuffle[shuffle$running_percent <= 10,]
sum(R10$mean_MC)/10^12
sum(R10$mean_sd)/10^12


### cumulative emissions from only restoring top 10% of GHG sinks
P10 <- sort[sort$running_percent <= 25,]
sum(P10$mean_MC)/10^12
sum(P10$mean_sd)/10^12

### cumulative emissions from 10% restoration -- random fashion
set.seed(123)
shuffle <- sort[sample(1:nrow(sort)),]
shuffle$cum_area <- cumsum(shuffle$peatland_loss)
shuffle$running_percent <- shuffle$cum_area/sum(shuffle$peatland_loss, na.rm = TRUE)*100

R10 <- shuffle[shuffle$running_percent <= 25,]
sum(R10$mean_MC)/10^12
sum(R10$mean_sd)/10^12




net.feedbackfirst.25p #### net emissions from rewetting 35% drained peatlands, feedback first
sd.feedbackfirst.25p ### sd ^




