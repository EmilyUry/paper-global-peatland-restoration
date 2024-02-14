


library(tidyverse)
library(ggplot2)
library(cowplot)


## DATA SET-UP

setwd("C:/Users/eury/OneDrive - Environmental Defense Fund - edf.org/Wetland-Restoration-GHG/Paper 1/Data_sources")

# read in EF and generate mean, se, and standard deviation for each land use group
EF <- read.csv("Emission_Factors/EFs_w_variance.csv", stringsAsFactors = TRUE) %>%
  dplyr::select(CZ, LU, GHG, EF_mean, CI_low, CI_high, n )
EF$EF_mean <- EF$EF_mean*100 ## change to emissions per km2 from emissions per ha (note other unit changes happen below)
EF$CI_low <- EF$CI_low*100
EF$CI_high <- EF$CI_high*100

## Calculate standard deviation from confidence interval
EF$Tinvt <- qt(0.05/2, EF$n-1, lower.tail = FALSE)
EF$se <- (EF$CI_high-EF$CI_low)/2/EF$Tinvt
EF$sd <- EF$se*sqrt(EF$n)

## Assign SD from Temmink and Bahram datasets
EF$sd[72:77] <- c(0.3196, 0.8054, 1.6479, 0.0587, 0.472, 3.658)*100  


## Group by CZ, LU and GHG and find the EF mean and SD
# weighted mean (not used for analysis, just for calculating cumulative sd)
EF_summary <- EF %>%
  group_by(CZ, LU, GHG) %>% 
  summarize(mean_EF = mean(EF_mean),
            w_mean_EF = weighted.mean(x = EF_mean, w = n, na.rm = TRUE), 
            mean_sd = mean(sd),
            w_mean_sd = radiant.data::weighted.sd(x = sd, w = n, na.rm = TRUE)) %>%
  mutate(final_sd = ifelse(w_mean_sd == 0, mean_sd, w_mean_sd))


## fill in sd gaps from most analygous system
## i.e. rewetted-boreal N2O from wetland N2O
{
  
  EF_summary[EF_summary$CZ == 1 & EF_summary$LU == "Rewetted" & EF_summary$GHG == "N2O",]$final_sd <- 
    EF_summary[EF_summary$CZ == 1 & EF_summary$LU == "Wetland" & EF_summary$GHG == "N2O",]$final_sd
  
  EF_summary[EF_summary$CZ == 2 & EF_summary$LU == "Rewetted" & EF_summary$GHG == "N2O",]$final_sd <- 
    EF_summary[EF_summary$CZ == 2 & EF_summary$LU == "Wetland" & EF_summary$GHG == "N2O",]$final_sd
  
  EF_summary[EF_summary$CZ == 3 & EF_summary$LU == "Ag" & EF_summary$GHG == "N2O",]$final_sd <- 
    EF_summary[EF_summary$CZ == 2 & EF_summary$LU == "Ag" & EF_summary$GHG == "N2O",]$final_sd
  
  EF_summary[EF_summary$CZ == 3 & EF_summary$LU == "Peatx" & EF_summary$GHG == "CO2",]$final_sd <- 
    EF_summary[EF_summary$CZ == 2 & EF_summary$LU == "Peatx" & EF_summary$GHG == "CO2",]$final_sd
  
  EF_summary[EF_summary$CZ == 3 & EF_summary$LU == "Peatx" & EF_summary$GHG == "N2O",]$final_sd <- 
    EF_summary[EF_summary$CZ == 2 & EF_summary$LU == "Peatx" & EF_summary$GHG == "N2O",]$final_sd
  
  EF_summary[EF_summary$CZ == 3 & EF_summary$LU == "Rewetted" & EF_summary$GHG == "CO2",]$final_sd <- 
    EF_summary[EF_summary$CZ == 2 & EF_summary$LU == "Rewetted" & EF_summary$GHG == "CO2",]$final_sd
  
  EF_summary[EF_summary$CZ == 3 & EF_summary$LU == "Rewetted" & EF_summary$GHG == "N2O",]$final_sd <- 
    EF_summary[EF_summary$CZ == 3 & EF_summary$LU == "Wetland" & EF_summary$GHG == "N2O",]$final_sd
  
  ## for missing temperate rice, replace with half EF from tropical rice
  EF_summary[42,] 
  EF_summary[46,] <- list(2, "Rice", "CO2", 940/2, NA, NA, NA, 9.62/2)
  EF_summary[41,] 
  EF_summary[47,] <- list(2, "Rice", "CH4", 14350/2, NA, NA, NA, 76.5/2)
  EF_summary[43,] 
  EF_summary[48,] <- list(2, "Rice", "N2O", 40/2, NA, NA, NA, 0.429/2)
  
  }

### Mean emission factor by CZ, LU and GHG grouping and do final unit conversion
Final_EF <- EF_summary %>%
  dplyr::select(CZ, LU, GHG, mean_EF) %>%
  pivot_wider(names_from = c(GHG, LU), values_from = mean_EF)
Final_EF$CZ <- as.factor(Final_EF$CZ)
Final_EF[is.na(Final_EF)] <- 0

# convert from Mg/km2 to Kg per km2 per yr
data <- Final_EF
## these are all the CO2-C in tonnes
data$CO2_Ag <- data$CO2_Ag*1000*44/12   
data$CO2_Forest <- data$CO2_Forest*1000*44/12
data$CO2_Peatx <- data$CO2_Peatx*1000*44/12
data$CO2_Rice <- data$CO2_Rice*1000*44/12
data$CO2_Wetland <- data$CO2_Wetland*1000*44/12
data$CO2_Rewetted <- data$CO2_Rewetted*1000*44/12
## now they are converted to kg of CO2 

## convert from kg of N2O-N to kg of N2O
data$N2O_Ag <- data$N2O_Ag*44/14   
data$N2O_Forest <- data$N2O_Forest*44/14
data$N2O_Peatx <- data$N2O_Peatx*44/14
data$N2O_Rice <- data$N2O_Rice*44/14
data$N2O_Wetland <- data$N2O_Wetland*44/14
data$N2O_Rewetted <- data$N2O_Rewetted*44/14

## converts from kg of CH4-C to kg of CH4
data$CH4_Rewetted <- data$CH4_Rewetted*16/12

EF_mean <- data



### Standard deviation of emission factors by CZ, LU and GHG grouping and do final unit conversion
Final_EF_sd <- EF_summary %>%
  dplyr::select(CZ, LU, GHG, final_sd) %>%
  pivot_wider(names_from = c(GHG, LU), values_from = final_sd)
Final_EF_sd$CZ <- as.factor(Final_EF$CZ)
Final_EF_sd[is.na(Final_EF_sd)] <- 0

data <- Final_EF_sd

# convert from Mg/km2 to Kg per km2 per yr
# data$CH4_rcp45 <- data$CH4_rcp45*1000 
## these are all the CO2-C in tonnes
data$CO2_Ag <- data$CO2_Ag*1000*44/12   
data$CO2_Forest <- data$CO2_Forest*1000*44/12
data$CO2_Peatx <- data$CO2_Peatx*1000*44/12
data$CO2_Rice <- data$CO2_Rice*1000*44/12
data$CO2_Wetland <- data$CO2_Wetland*1000*44/12
data$CO2_Rewetted <- data$CO2_Rewetted*1000*44/12
## now they are converted to kg of CO2 

## convert from kg of N2O-N to kg of N2O
data$N2O_Ag <- data$N2O_Ag*44/14   
data$N2O_Forest <- data$N2O_Forest*44/14
data$N2O_Peatx <- data$N2O_Peatx*44/14
data$N2O_Rice <- data$N2O_Rice*44/14
data$N2O_Wetland <- data$N2O_Wetland*44/14
data$N2O_Rewetted <- data$N2O_Rewetted*44/14

## converts from kg of CH4-C to kg of CH4
data$CH4_Rewetted <- data$CH4_Rewetted*16/12

SD <- data




mean_long <- EF_mean %>%
  pivot_longer(cols = !CZ, names_to = "cat", values_to = "EF_mean") %>%
  separate_wider_delim(cat, "_", names = c("GHG", "LU"))

sd_long <- SD %>%
  pivot_longer(cols = !CZ, names_to = "cat", values_to = "SD") %>%
  separate_wider_delim(cat, "_", names = c("GHG", "LU"))


mean_long$SD <- sd_long$SD


### add missing mean and SD for intact methane from Zhang et al.
mean_long[52,] <- list("1", "CH4", "Wetland", 2364, 5397 )
mean_long[53,] <- list("2", "CH4", "Wetland", 7112, 28163 )
mean_long[54,] <- list("3", "CH4", "Wetland", 15720, 38993 )

mean_long$units <- "kg/km2/year"



## write out to save
write.csv(mean_long, "Emission_Factors/EF_SD_Summary.csv", row.names=FALSE)





