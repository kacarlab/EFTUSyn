### PREP DATA ###

# Clear environment
rm(list = ls())

install.packages("Cairo")

#Load libraries
library(growthcurver)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("/Users/k8tiedid/Documents/Kacar_Lab/Experiments/Growth_Curves/R_data/CH-strains/05March2022")

# Import data with comma-separated vales; replace with appropriate path and file name
data <- read.table(file = "data.csv", header = TRUE, sep = ",")

# Convert the "time" column from minutes to seconds (commented out until needed)
#d$time <- d$time * 60

# Convert the "time" column from minutes to hours (commented out until needed)
data$time <- data_fc$time / 60

# ### ANALYZE DATA FOR SINGLE GROWTH CURVE ###  Commented out until needed
# 
# # Summarize growth curve data and return value summary
# gc_summary_single <- SummarizeGrowth(data$time, data$A1)
# gc_summary_single
# 
# # Plot raw growth curve data and best fit logistic curve
# plot(gc_summary_single)
# 
# # Return growth rate (r, /hr)
# growth_rate_single = gc_summary_single$val$r
# 
# # Return doubling time (t_gen, /hr)
# doubling_time_single = gc_summary_single$val$t_gen
# 
# # Return time of inflection point for best fit logistic curve (t_mid, /hr)
# midpoint_time_single = gc_summary_single$val$t_mid




### ANALYZE DATA FOR 96-WELL-PLATE GROWTH CURVES ###

# Correct data with self-provided blank measurements in "blank" column of data table; outputs fitted growth curvers to pdf in "Output" folder
gc_summary_plate <- SummarizeGrowthByPlate(data_fc, 
                                           plot_fit = TRUE, plot_file = "data.pdf")

write.table(gc_summary_plate, "data.txt", sep = "\t")

# Check for poor fit to logistic curve by looking at "note" column of gc_summary_plate

# Return mean growth rate (r, /hr) across all wells
mean_growth_rate_REL606 <- mean(gc_summary_plate[1:10,"r"])
mean_growth_rate_CH6138 <- mean(gc_summary_plate[11:20,"r"])
mean_growth_rate_CH6139 <- mean(gc_summary_plate[21:30,"r"])
mean_growth_rate_CH6556 <- mean(gc_summary_plate[31:40,"r"])
mean_growth_rate_CH6585 <- mean(gc_summary_plate[41:50,"r"])

# Return mean doubling time (t_gen, /hr) across all wells (except blank in row "96")
mean_DT_REL606 <- mean(gc_summary_plate[1:10,"t_gen"])
mean_DT_CH6138 <- mean(gc_summary_plate[11:20,"t_gen"])
mean_DT_CH6139 <- mean(gc_summary_plate[21:30,"t_gen"])
mean_DT_CH6556 <- mean(gc_summary_plate[31:40, "t_gen"])
mean_DT_CH6585 <- mean(gc_summary_plate[41:50, "t_gen"])

# Return mean time of inflection point for best fit logistic curve (t_mid, /hr) across all wells (except blank in row "96")
mean_midpoint_time_REL606 <- mean(gc_summary_plate[1:10,"t_mid"])
mean_midpoint_time_CH6138 <- mean(gc_summary_plate[11:20,"t_mid"])
mean_midpoint_time_CH6139 <- mean(gc_summary_plate[21:30,"t_mid"])
mean_midpoint_time_CH6556 <- mean(gc_summary_plate[31:40,"t_mid"])
mean_midpoint_time_CH6585 <- mean(gc_summary_plate[41:50,"t_mid"])


#####  GRAPHING #####

## BACKGROUND CORRECTION BEFORE GRAPHING ##

### DRAW MEAN CURVES WITH 95% CONFIDENCE INTERVAL ###

# Reformat data for plotting, adding new column to categorize by well name
data_forcurve <- data %>% gather(well, OD600, -time)

# Add new column for strain name (edit grep arguments to match plate rows for each strain)
Strain=rep(NA, nrow(data_forcurve))
data_forcurve=cbind(data_forcurve,Strain)

data_forcurve$Strain[grep("Rel606", data_forcurve$well)] <- "REL606"
data_forcurve$Strain[grep("CH6138", data_forcurve$well)] <- "Ancestor"
data_forcurve$Strain[grep("CH6139", data_forcurve$well)] <- "Evolved"
data_forcurve$Strain[grep("CH6556", data_forcurve$well)] <- "CH6556"
data_forcurve$Strain[grep("CH6585", data_forcurve$well)] <- "CH6585"

data_forcurve$Strain <- factor(data_forcurve$Strain, levels = c("REL606", "Ancestor", "Evolved", "CH6556", "CH6585"))

# Plot all data, categorized by strain (scatter plot + trend line)
# Color brewer palette can be adjusted; default smoothing is a LOESS regression w/ 95% confidence interval

ggplot(data_forcurve, aes(x = time, y = OD600, color = Strain))+
  geom_smooth()+
  geom_point(data=data_forcurve[data_forcurve$time %in% seq(0,12, by=1),], aes(x=time, y=OD600, color=Strain))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_continuous(limits=c(0,12), expand=c(0,0), breaks=c(0, 2, 4, 6, 8, 10, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48))+
  scale_y_continuous(limits=c(0,1.2), expand=c(0,0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.5, 2.0, 2.5, 3.0))+
  scale_color_manual(breaks = data_forcurve$Strain, 
                     values = c(REL606 = "black", Ancestor = 'gold', Evolved = '#DC3220', CH6556 = 'grey60', CH6585 = '#6db6ff'))+
  guides(color = guide_legend(title = "Lineages"))+
  labs(x ="Time (h)", y = "Abs (OD600)", size=16)+
  theme(plot.title = element_text(hjust = 0.5))