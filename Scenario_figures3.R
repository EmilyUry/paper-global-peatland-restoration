


#### 

setwd("C:/Users/eury/OneDrive - Environmental Defense Fund - edf.org/Wetland-Restoration-GHG/Paper 1")

library(tidyverse)
library(raster)

library(sf)
library(cowplot)




data <- read.csv("Data_sources/Extracted_datafiles/Data_filtered3.csv")
sum(data$peatland_loss)

MC_SGWP20 <- read.csv("Data_sources/Extracted_datafiles/MC_SGWP20_3.csv")
MC_SGWP100 <- read.csv("Data_sources/Extracted_datafiles/MC_SGWP100_3.csv")
MC_2099 <- read.csv("Data_sources/Extracted_datafiles/MC_2099_3.csv")
MC_2010 <- read.csv("Data_sources/Extracted_datafiles/MC_2010_3.csv")
MC_Rewet <- read.csv("Data_sources/Extracted_datafiles/MC_Rewet_3.csv")



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
summary.table
write.csv(summary.table, "Data_sources/Extracted_datafiles/Scenario_summary3.csv")





### Figure SGWP20 vs Random + Drained post 2010 vs Random

{
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
  Sub$norm <- Sub[,2]/Sub$peatland_loss
  Sub <- arrange(Sub, norm)
  Sub$cum_area <- cumsum(Sub$peatland_loss)
  Sub$running_percent <- Sub$cum_area/sum(Sub$peatland_loss, na.rm = TRUE)*100
  
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
set.seed(1234)
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
  ylim(-2.65,0.25) +
  annotate("text", x = 40, y = -2.4, size = 3, label = "Strategic restoration") +
  annotate("text", x = 25, y = 0.15, size = 3, col = "red", label = "Random restoration")
plotx


### Results reported in Table 1
{
p1 <- S1_rr[S1_rr$percent == 1,] %>%
  group_by(strategy) %>%
  summarize(mean_val = mean(val),
            sd_val = sd(val))

p3 <- S1_rr[S1_rr$percent == 3,] %>%
  group_by(strategy) %>%
  summarize(mean_val = mean(val),
            sd_val = sd(val))

p10 <- S1_rr[S1_rr$percent == 10,] %>%
  group_by(strategy) %>%
  summarize(mean_val = mean(val),
            sd_val = sd(val))

p30 <- S1_rr[S1_rr$percent == 30,] %>%
  group_by(strategy) %>%
  summarize(mean_val = mean(val),
            sd_val = sd(val))

p50 <- S1_rr[S1_rr$percent == 50,] %>%
  group_by(strategy) %>%
  summarize(mean_val = mean(val),
            sd_val = sd(val))

p99 <- S1_rr[S1_rr$percent == 99,] %>%
  group_by(strategy) %>%
  summarize(mean_val = mean(val),
            sd_val = sd(val))

p100 <- S1_rr[S1_rr$percent == 100,] %>%
  group_by(strategy) %>%
  summarize(mean_val = mean(val),
            sd_val = sd(val))


table1 <- data.frame(c(1,3,10,30,50,99,100), unlist(c(p1[1,2], p3[1,2], p10[1,2], p30[1,2], p50[1,2], p99[1,2], p100[1,2])),
                     unlist(c(p1[1,3], p3[1,3], p10[1,3], p30[1,3], p50[1,3], p99[1,3], p100[1,3])),
                     unlist(c(p1[2,2], p3[2,2], p10[2,2], p30[2,2], p50[2,2], p99[2,2], p100[2,2])),
                     unlist(c(p1[2,3], p3[2,3], p10[2,3], p30[2,3], p50[2,3], p99[2,3], p100[2,3])))
names(table1) <- c("percent restored", "mean dGHG C first", "sd dGHG C first", "mean dGHG random", "sd dGHG random")
table1



## anti strategy
as.data.frame(p100[,-1])-as.data.frame(p99[,-1])


anti1 <-  data.frame(matrix(NA, ncol = 1, nrow = 100))[-1]
x <- seq(1,100, 1)
y <- list()

for(j in 1:n) {
  Sub <- output[,c(4,j+4)]
  Sub$norm <- Sub[,2]/Sub$peatland_loss
  Sub <- arrange(Sub, desc(norm))
  Sub$cum_area <- cumsum(Sub$peatland_loss)
  Sub$running_percent <- Sub$cum_area/sum(Sub$ peatland_loss, na.rm = TRUE)*100
  
  y <- list()
  for(i in 1:100) {
    subset <- Sub[Sub$running_percent <= i,]
    y[i] <- sum(subset[,2])/10^12
  }
  anti1[,j] <- unlist(y)
  print(j)
  print(Sys.time())
}


## Pivot data to long format and find mean for each % restoration
anti1_long <- anti1 %>% pivot_longer(everything(), names_to = "series", values_to = "val")
anti1_long$x <- rep(1:100, each = n)

anti1_long <- anti1 %>% pivot_longer(everything(), names_to = "series", values_to = "val")
anti1_long$percent <- rep(1:100, each = 500)
anti1_long$strategy <- "carbon"
anti1_long$series <- rep(seq(501,1000), 100)


anti1_rr <- rbind(rr_long, anti1_long )


p1 <- anti1_rr[anti1_rr$percent == 1,] %>%
  group_by(strategy) %>%
  summarize(mean_val = mean(val),
            sd_val = sd(val))

p3 <- anti1_rr[anti1_rr$percent == 3,] %>%
  group_by(strategy) %>%
  summarize(mean_val = mean(val),
            sd_val = sd(val))

p10 <- anti1_rr[anti1_rr$percent == 10,] %>%
  group_by(strategy) %>%
  summarize(mean_val = mean(val),
            sd_val = sd(val))

p30 <- anti1_rr[anti1_rr$percent == 30,] %>%
  group_by(strategy) %>%
  summarize(mean_val = mean(val),
            sd_val = sd(val))

p50 <- anti1_rr[anti1_rr$percent == 50,] %>%
  group_by(strategy) %>%
  summarize(mean_val = mean(val),
            sd_val = sd(val))

p99 <- anti1_rr[anti1_rr$percent == 99,] %>%
  group_by(strategy) %>%
  summarize(mean_val = mean(val),
            sd_val = sd(val))

p100 <- S1_rr[S1_rr$percent == 100,] %>%
  group_by(strategy) %>%
  summarize(mean_val = mean(val),
            sd_val = sd(val))



anti <- data.frame(unlist(c(p1[1,2], p3[1,2], p10[1,2], p30[1,2], p50[1,2], p99[1,2], p100[1,2])),
                     unlist(c(p1[1,3], p3[1,3], p10[1,3], p30[1,3], p50[1,3], p99[1,3], p100[1,3])))
names(anti) <- c("mean dGHG C last", "sd dGHG C last")
table1 <- cbind(table1, anti)
table1
write.csv(table1, "table1_v3.csv", row.names=FALSE)

}


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
max.diff.percent <- comp.p[which.max(comp.p$comp.mean),1]

ER68 <- Vstat[max.diff.percent,2]
ER68.sd <- Vstat[max.diff.percent,3]
ER68.random <- Vstat[max.diff.percent,4]
ER68.random.sd <- Vstat[max.diff.percent,5]


#### peatland drained since 2010 only (S4)
{

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
set.seed(1234)
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
recent_sd <- transform(R4, SD=apply(R4,1, sd, na.rm = TRUE))[100,501]


recent_restored <- V4stat[100,2] ## complete restoration, recently drained restoration only
recent_sd <- transform(S4, SD=apply(S4,1, sd, na.rm = TRUE))[100,501]

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
  ylim(-0.145,0.015) +
  annotate("text", x = 45, y = -0.13, size = 3, label = "Strategic restoration") +
  annotate("text", x = 35, y = 0.008, size = 3, col = "red", label = "Random restoration")

plot4

options(scipen = 99)

plot_grid(plotx, plot4, ncol = 2, labels = c("a", "b"), label_size = 11)
plot_grid(plotx, plot4, ncol = 2, rel_widths = c(1, 1.05),
          labels = c("a", "b"), label_size = 11)

ggsave(filename = "Figures/S1_SGWP20_S4_2010_v3.png",
       plot = last_plot(),  bg = "white",
       width = 6, 
       height = 3, 
       unit = "in",
       dpi = 300)

}
}

#####################################################
 
## multi-objective comparison (figure 3)
 
#####################################################


{
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
Vstat[1,2] 
 
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
          labels = c('a', 'b', 'c'), label_size = 10.5)
top


##### BOX PLOT VERSION 2

## Carbon first

P1 <- fig_long[fig_long$percent == 1,]
P3 <- fig_long[fig_long$percent ==3,]
P10 <- fig_long[fig_long$percent ==10,]
P30 <- fig_long[fig_long$percent ==30,]


C1 <- data.frame(rep("Climate first", 500), rep("1%", 500), P1$val) #Pg
names(C1) <- c("strategy", "percent_restore", "net_emissions")
C3 <- data.frame(rep("Climate first", 500), rep("3%", 500), P3$val) #Pg
names(C3) <- c("strategy", "percent_restore", "net_emissions")
C10 <- data.frame(rep("Climate first", 500), rep("10%", 500), P10$val) #Pg
names(C10) <- c("strategy", "percent_restore", "net_emissions")
C25 <- data.frame(rep("Climate first", 500), rep("30%", 500), P30$val) #Pg
names(C25) <- c("strategy", "percent_restore", "net_emissions")

###### Ag first

P1 <- crop_long[crop_long$percent ==1,]
P3 <- crop_long[crop_long$percent ==3,]
P10 <- crop_long[crop_long$percent ==10,]
P30 <- crop_long[crop_long$percent ==30,]

A1 <- data.frame(rep("Nitrogen first", 500), rep("1%", 500), P1$val) #Pg
names(A1) <- c("strategy", "percent_restore", "net_emissions")
A3 <- data.frame(rep("Nitrogen first", 500), rep("3%", 500), P3$val) #Pg
names(A3) <- c("strategy", "percent_restore", "net_emissions")
A10 <- data.frame(rep("Nitrogen first", 500), rep("10%", 500), P10$val) #Pg
names(A10) <- c("strategy", "percent_restore", "net_emissions")
A25 <- data.frame(rep("Nitrogen first", 500), rep("30%", 500),P30$val) #Pg
names(A25) <- c("strategy", "percent_restore", "net_emissions")

###### flood first

P1 <- flood_long[flood_long$percent ==1,]
P3 <- flood_long[flood_long$percent ==3,]
P10 <- flood_long[flood_long$percent ==10,]
P30 <- flood_long[flood_long$percent ==30,]

F1 <- data.frame(rep("Flood risk first", 500), rep("1%", 500), P1$val) #Pg
names(F1) <- c("strategy", "percent_restore", "net_emissions")
F3 <- data.frame(rep("Flood risk first", 500), rep("3%", 500), P3$val) #Pg
names(F3) <- c("strategy", "percent_restore", "net_emissions")
F10 <- data.frame(rep("Flood risk first", 500), rep("10%", 500), P10$val) #Pg
names(F10) <- c("strategy", "percent_restore", "net_emissions")
F25 <- data.frame(rep("Flood risk first", 500), rep("30%", 500), P30$val) #Pg
names(F25) <- c("strategy", "percent_restore", "net_emissions")


### random first

P1 <- rr_long[rr_long$percent ==1,]
P3 <- rr_long[rr_long$percent ==3,]
P10 <- rr_long[rr_long$percent ==10,]
P30 <- rr_long[rr_long$percent ==30,]

R1 <- data.frame(rep("Random", 500), rep("1%", 500), P1$val) #Pg
names(R1) <- c("strategy", "percent_restore", "net_emissions")
R3 <- data.frame(rep("Random", 500), rep("3%", 500), P3$val) #Pg
names(R3) <- c("strategy", "percent_restore", "net_emissions")
R10 <- data.frame(rep("Random", 500), rep("10%", 500), P10$val) #Pg
names(R10) <- c("strategy", "percent_restore", "net_emissions")
R25 <- data.frame(rep("Random", 500), rep("30%", 500), P30$val) #Pg
names(R25) <- c("strategy", "percent_restore", "net_emissions")



### FEEDBACK FIRST
P1 <- feedback_long[feedback_long$percent ==1,]
P3 <- feedback_long[feedback_long$percent ==3,]
P10 <- feedback_long[feedback_long$percent ==10,]
P30 <- feedback_long[feedback_long$percent ==30,]

X1 <- data.frame(rep("Feedback", 500), rep("1%", 500), P1$val) #Pg
names(X1) <- c("strategy", "percent_restore", "net_emissions")
X3 <- data.frame(rep("Feedback", 500), rep("3%", 500), P3$val) #Pg
names(X3) <- c("strategy", "percent_restore", "net_emissions")
X10 <- data.frame(rep("Feedback", 500), rep("10%", 500), P10$val) #Pg
names(X10) <- c("strategy", "percent_restore", "net_emissions")
X25 <- data.frame(rep("Feedback", 500), rep("30%", 500), P30$val) #Pg
names(X25) <- c("strategy", "percent_restore", "net_emissions")



### put it all together for boxplot (bottom panel)

bplot <- rbind(C1, C3, C10, C25, X1, X3, X10, X25,
             A1, A3, A10, A25, F1, F3, F10, F25, R1, R3, R10, R25)
bplot$strategy <- factor(bplot$strategy, levels = c("Climate first", "Feedback", "Nitrogen first", 
                                                "Flood risk first", "Random"))
bplot$percent_restore <- factor(bplot$percent_restore, levels = c("1%", "3%", "10%", "30%"))
pal <- c("#666666","#54246b", "#f0d72b", "#2b6a78","#ff6161") # color blind friendly

bottom <- ggplot(bplot, aes(x = percent_restore, y = net_emissions, fill = strategy)) +
  geom_boxplot( fatten = 0.5, lwd = 0.2, outlier.size = 0.5) +
  theme_bw(base_size = 9) +
  #scale_fill_viridis(discrete = TRUE, option = "viridis") +
  scale_fill_manual(values = pal, labels = c("Climate first", "Future methane", "Nitrogen first",
                                             "Flood risk first", "Random")) +
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
          label_y = 1.1, label_size = 11)


ggsave(filename = "Figures/Multi-objective-compare_v3.png",
       plot = last_plot(), bg = "white",
       width = 6, 
       height = 5, 
       unit = "in",
       dpi = 300)


## check 
check <- bplot %>%
  group_by(strategy, percent_restore) %>%
  summarise(median = median(net_emissions))

## check 
check <- bplot %>%
  group_by(strategy, percent_restore) %>%
  summarise(mean = mean(net_emissions))

### metrics
CF <- bplot[bplot$strategy == "Climate first" & bplot$percent_restore == "10%",]
RR <- bplot[bplot$strategy == "Random" & bplot$percent_restore == "10%",]
median.10p.climate_first <- median(CF$net_emissions) #Pg CO2e/yr
sd.10p.climate_first <- sd(CF$net_emissions)
median.10p.random <- median(RR$net_emissions)
sd.10p.random <- sd(RR$net_emissions)

CF <- bplot[bplot$strategy == "Climate first" & bplot$percent_restore == "30%",]
RR <- bplot[bplot$strategy == "Random" & bplot$percent_restore == "30%",]
median.25p.climate_first <- median(CF$net_emissions) #Pg CO2e/yr
sd.25p.climate_first <- sd(CF$net_emissions)
median.25p.random <- median(RR$net_emissions)
sd.25p.random <- sd(RR$net_emissions)


CF <- bplot[bplot$strategy == "Flood risk first" & bplot$percent_restore == "30%",]
RR <- bplot[bplot$strategy == "Nitrogen first" & bplot$percent_restore == "30%",]
median.25p.flood_first <- median(CF$net_emissions) #Pg CO2e/yr
sd.25p.flood_first <- sd(CF$net_emissions)
median.25p.N_first <- median(RR$net_emissions)
sd.25p.N_first <- sd(RR$net_emissions)


FM <- bplot[bplot$strategy == "Feedback" & bplot$percent_restore == "30%",]
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




### check for statistically significant differences between groups
# Function to perform a z-test
z_test <- function(mean1, mean2, sd1, sd2, n1, n2) {
  # Calculate the z-statistic
  z_value <- (mean1 - mean2) / sqrt((sd1^2 / n1) + (sd2^2 / n2))
  
  # Calculate the p-value
  p_value <- 2 * pnorm(-abs(z_value))
  
  # Return the z-value and p-value
  return(list(z_value = z_value, p_value = p_value))
}

#### 10% restoration

group1 <- F10$net_emissions
group2 <- R10$net_emissions

# Sample data
mean1 <- mean(group1)      # Mean of group 1
mean2 <- mean(group2)      # Mean of group 2
sd1 <- sd(group1)         # Standard deviation of group 1
sd2 <- sd(group2)         # Standard deviation of group 2
n1 <- length(group1)         # Sample size of group 1
n2 <- length(group2)         # Sample size of group 2

# Perform the z-test
ztest.flood10 <- z_test(mean1, mean2, sd1, sd2, n1, n2)
ztest.flood10


group1 <- A10$net_emissions
group2 <- R10$net_emissions

# Sample data
mean1 <- mean(group1)      # Mean of group 1
mean2 <- mean(group2)      # Mean of group 2
sd1 <- sd(group1)         # Standard deviation of group 1
sd2 <- sd(group2)         # Standard deviation of group 2
n1 <- length(group1)         # Sample size of group 1
n2 <- length(group2)         # Sample size of group 2

# Perform the z-test
ztest.ag10 <- z_test(mean1, mean2, sd1, sd2, n1, n2)
ztest.ag10

#### 30% restoration

group1 <- F25$net_emissions
group2 <- R25$net_emissions

# Sample data
mean1 <- mean(group1)      # Mean of group 1
mean2 <- mean(group2)      # Mean of group 2
sd1 <- sd(group1)         # Standard deviation of group 1
sd2 <- sd(group2)         # Standard deviation of group 2
n1 <- length(group1)         # Sample size of group 1
n2 <- length(group2)         # Sample size of group 2

# Perform the z-test
ztest.flood25 <- z_test(mean1, mean2, sd1, sd2, n1, n2)
ztest.flood25


group1 <- A25$net_emissions
group2 <- R25$net_emissions

# Sample data
mean1 <- mean(group1)      # Mean of group 1
mean2 <- mean(group2)      # Mean of group 2
sd1 <- sd(group1)         # Standard deviation of group 1
sd2 <- sd(group2)         # Standard deviation of group 2
n1 <- length(group1)         # Sample size of group 1
n2 <- length(group2)         # Sample size of group 2

# Perform the z-test
ztest.ag25 <- z_test(mean1, mean2, sd1, sd2, n1, n2)
ztest.ag25




### future
group1 <- X25$net_emissions
group2 <- R25$net_emissions

# Sample data
mean1 <- mean(group1)      # Mean of group 1
mean2 <- mean(group2)      # Mean of group 2
sd1 <- sd(group1)         # Standard deviation of group 1
sd2 <- sd(group2)         # Standard deviation of group 2
n1 <- length(group1)         # Sample size of group 1
n2 <- length(group2)         # Sample size of group 2

# Perform the z-test
ztest.x25 <- z_test(mean1, mean2, sd1, sd2, n1, n2)
ztest.x25

summary <- as.data.frame(t(matrix(c("median, climate first, 10%", median.10p.climate_first,
                                    "SD, climate first, 10%", sd.10p.climate_first,
                                    "median, random, 10%", median.10p.random, 
                                    "SD, random, 10%", sd.10p.random,
                                    "Climate first minus random, 10%", CR.RR.10p, 
                                    "SD, Climate first minus random, 10%", CR.RR.sd.10p,
                                    "median, climate first, 30%", median.25p.climate_first,
                                    "SD, climate first, 30%", sd.25p.climate_first,
                                    "median, random, 30%", median.25p.random, 
                                    "SD, random, 30%", sd.25p.random, 
                                    "Climate first minus random, 30%", CR.RR.25p, 
                                    "SD, Climate first minus random, 30%", CR.RR.sd.25p, 
                                    "median, flood first, 30%", median.25p.flood_first,
                                    "SD, flood first, 30%", sd.25p.flood_first,
                                    "median, N first, 30%", median.25p.N_first,
                                    "SD, N first, 30%", sd.25p.N_first, 
                                    "median, feedback first, 30%", median.25p.feedback,
                                    "SD, feedback first, 30%", sd.25p.feedback,
                                    "Climate first minus flood first, percent difference", percent.dif.CF.FF,
                                    "Climate first minus n first, percent difference", percent.dif.CF.NF,
                                    "ztest flood 10%, z", ztest.flood10[1],
                                    "ztest flood 10% p", ztest.flood10[2],
                                    "ztest ag 10%, z", ztest.ag10[1],
                                    "ztest ag 10%, p", ztest.ag10[2],
                                    "ztest flood 30%, z", ztest.flood25[1],
                                    "ztest flood 30% p", ztest.flood25[2],
                                    "ztest ag 30%, z", ztest.ag25[1],
                                    "ztest ag 30%, p", ztest.ag25[2],
                                    "ztest future 30%, z", ztest.x25[1],
                                    "ztest future 30%, p", round(unlist(ztest.x25[2],10))), nrow = 2)))
summary

}





#### map of top priorities for each objective
## where are the top 25 % of N use, flood risk, and climate benefit from restoration


{

output <- data %>%
  dplyr::select(x,y, cell_area, peatlands, peatland_loss, N_fert, flood)
output$mean_MC <- rowMeans(MC_SGWP20)     ## emissions in kg CO2e/year/gridcell
output$norm <- output$mean_MC/output$peatland_loss   ## emissions in kg CO2e/km2/yr

output$cutoff <- output$cell_area/100

## restrict priority areas to places with greater that 1% peatland coverage and more than 1ha peatland loss
output <- output[output$peatlands > output$cutoff & output$peatland_loss > 0.01,]


#### CARBON FIRST
df <- output

sort <- arrange(df, norm)
sort$cum_area <- cumsum(sort$peatland_loss)
sort$running_percent <- sort$cum_area/sum(sort$peatland_loss, na.rm = TRUE)*100
sort$carbon.top <- ifelse(sort$running_percent < 30, 1, 0)
#### N first
sort <- arrange(sort, desc(N_fert))
sort$cum_area <- cumsum(sort$peatland_loss)
sort$running_percent <- sort$cum_area/sum(sort$peatland_loss, na.rm = TRUE)*100
sort$fert.top <- ifelse(sort$running_percent < 30, 1, 0)
#### Flood first
sort <- arrange(sort, desc(flood))
sort$cum_area <- cumsum(sort$peatland_loss)
sort$running_percent <- sort$cum_area/sum(sort$peatland_loss, na.rm = TRUE)*100
sort$flood.top <- ifelse(sort$running_percent < 30, 1, 0)


## map
coasts <- st_read("Data_sources/Coastline/ne_110m_coastline.shp", quiet = TRUE)
sort$intersect <- ifelse(sort$carbon.top == 1 & sort$flood.top == 1 & sort$fert.top == 1, 3,
                          ifelse(sort$fert.top == 1 & sort$carbon.top == 1 , 1,
                                 ifelse(sort$flood.top == 1 & sort$carbon.top == 1 , 2, NA)))
map.filter <- sort[!is.na(sort$intersect),]

map.top <- ggplot() +
  geom_tile(data = map.filter, aes(x= x, y = y, height=0.8, width=0.8, fill = as.factor(intersect))) +
  scale_fill_manual(values = c("#f0d72b" , "#578e9c","#b31722" ), na.value = "#00000000",
                    labels = c("Nitrogen + Climate", "Flood + Climate", "All three")) +
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 10) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  theme(legend.position = c(0.13,0.4), legend.key.height = unit(0.2, "in"),
        legend.key.width = unit(0.2, "in"), legend.spacing.y = unit(0.07, 'in'),
        legend.background = element_rect(fill="#FFFFFF00")) +
  guides(fill = guide_legend(byrow = TRUE))+
  labs(fill = " ") 

## metrics
table(map.filter$intersect)
# 1 = both fertilizer and climate  ##287
# 2 = both flood and climate  # 902
# 3 = all three              ## 254

902/287   ## 3.1





#### map, methane now vs methane future
data$CH4_feedback <- data$CH4_Zhang_2099 - data$CH4_Zhang_2020

output <- data %>%
  dplyr::select(x,y, cell_area, peatlands, peatland_loss, CH4_feedback)
output$mean_MC <- rowMeans(MC_SGWP20)     ## emissions in kg CO2e/year/gridcell
output$norm <- output$mean_MC/output$peatland_loss   ## emissions in kg CO2e/km2/yr
output$mean_MC_2099 <- rowMeans(MC_2099)     ## emissions in kg CO2e/year/gridcell
output$norm_2099 <- output$mean_MC_2099/output$peatland_loss 

output$cutoff <- output$cell_area/100
output <- output[output$peatlands > output$cutoff & output$peatland_loss > 0.01,]

df <- output

## emissions in 20 years
sort <- arrange(df, norm)
sort$cum_area <- cumsum(sort$peatland_loss)
sort$running_percent <- sort$cum_area/sum(sort$peatland_loss, na.rm = TRUE)*100
sort$carbon.top <- ifelse(sort$running_percent < 25, 1, 0)




## emissions in 2099
sort <- arrange(sort, norm_2099)
sort$cum_area <- cumsum(sort$peatland_loss)
sort$running_percent <- sort$cum_area/sum(sort$peatland_loss, na.rm = TRUE)*100
sort$future.top <- ifelse(sort$running_percent < 30, 1, 0)

sort$intersect <- ifelse(sort$carbon.top == 1 & sort$future.top == 1, 1,
                         ifelse(sort$carbon.top == 1 , 3,
                                ifelse(sort$future.top == 1 , 2, NA)))
table(sort$intersect)
filter <- sort[!is.na(sort$intersect),]

## old purple #54246b, new purple #8d28bd
map.bottom <- ggplot() +
  geom_tile(data = filter, aes(x= x, y = y, height=1.2, width=1.2, fill = as.factor(intersect))) +   ### exagerated pixel size for visual clarity
  scale_fill_manual(values = c( "#dedede55", "#8d28bd", "#454545"), na.value = "#00000000",
                    labels = c("Minimize emissions,\nboth timelines", "Minimize future \nemissions", "Minimize near-term \nemissions")) +
  xlab(" ") +
  ylab(" ") +
  theme_bw(base_size = 10) +
  geom_sf(data = coasts, color = "gray50", fill = NA,linewidth = 0.25) +
  theme(legend.position = c(0.14,0.4), legend.key.height = unit(0.2, "in"),
        legend.key.width = unit(0.2, "in"), legend.spacing.y = unit(0.07, 'in'),
        legend.background = element_rect(fill="#FFFFFF00")) +
  guides(fill = guide_legend(byrow = TRUE))+
  labs(fill = " ") 



plot_grid(map.top, map.bottom, nrow = 2, labels = c("a", "b"), label_size = 12)

ggsave(filename = "Figures/Multi-objective-maps_v3.png",
       plot = last_plot(), bg = "white",
       width = 3.5*2, 
       height = 3.4*2, 
       unit = "in",
       dpi = 300)



}





########### Metrics reported in the paper


{

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

### cumulative emissions from only restoring peatlands with net positive emissions
output <- data %>%
  dplyr::select(x, y, peatlands, peatland_loss)
output$mean_MC <- rowMeans(MC_SGWP20)     ## emissions in kg CO2e/year/gridcell
output$mean_sd <- transform(MC_SGWP20, SD=apply(MC_SGWP20,1, sd, na.rm = TRUE))[,501]
output$norm <- output$mean_MC/output$peatland_loss   ## emissions in kg CO2e/km2/yr

Ppos <- output[output$norm > 0,]
netpos <- sum(Ppos$mean_MC)/10^12
netpos.sd <- sum(Ppos$mean_sd)/10^12

#### only wetlands with net negative emission
Pneg <- output[output$norm < 0,]
netnegs <- sum(Pneg$mean_MC)/10^12
netnegs.sd <- sum(Pneg$mean_sd)/10^12

sum(Ppos$mean_MC)/10^12 + sum(Pneg$mean_MC)/10^12

emissions1p ### emissions from restoring the best 1% of drained peatlands
emissions1p.sd ## standard deviation ^


summary <- as.data.frame(t(matrix(c("global emissions, restore to intact, GWP20", sum.S1,
                                    "sd emissions, restore to intact, GWP20", sd.S1,
                                    "global emissions, restore to rewet, GWP20", sum.S5,
                                    "sd emissions, restore to rewet, GWP20", sd.S5,
                                    "Percent of restored peatlands, GHG sinks", GHGsinks.intact,
                                    "Percent of rewetted peatlands, GHG sinks", GHGsinks.rewetted, 
                                    "max differece between restored & strategic", max.diff,
                                    "max diff, percent of peatland restored", max.diff.percent, 
                                    "emissions reductions at 68% restored", ER68,
                                    "sd of emissions reductions at 68% restored", ER68.sd,
                                    "emissions reds at 68% restored, randomly", ER68.random,
                                    "sd of ^", ER68.random.sd, 
                                    "emission from restoring all net neg emitters", netnegs,
                                    "sd of ^", netnegs.sd,
                                    "emissions from restoring all net pos emitters", netpos,
                                    "sd of ^", netpos.sd,
                                    "emissions from restoring best 1%", emissions1p, 
                                    "sd of ^", emissions1p.sd,
                                    "emissions from restoring all recently drained", recent_restored,
                                    "sd ^", recent_sd),
                                    nrow = 2)))

summary






}

########### Response to reviewer queries

{
data <- read.csv("Data_sources/Extracted_datafiles/Data_filtered2.csv")
filter <- data[data$biomes == 3,]
trop.peatx <- filter[filter$WL_peatx > 0,]
sum(trop.peatx$WL_peatx)


filter <- data[data$biomes == 3,]
trop.rice <- filter[filter$WL_rice > 0,]
sum(trop.rice$WL_rice)

filter <- data[data$biomes == 2,]
temp.rice <- filter[filter$WL_rice > 0,]
sum(temp.rice$WL_rice)

}








