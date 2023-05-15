##### Water Quality: Examining and Comparing Trends over Time #####

# if this is your first time using these packages, you must install them first, otherwise skip this or comment this out
install.packages('ggplot2')
install.packages('tidyr')
install.packages('dplyr')

# load the packages we'll need to work with
library(ggplot2)
library(tidyr)
library(dplyr)

# Set working directory to the folder that holds my data files
setwd("C:/Users/saweiss.LC/OneDrive - Lewis and Clark Community College/UI Field Course/UI-Field-Course-at-NGRREC/")

#### Read in Dataset ####
water <- read.csv("water_example.csv")

# inspect the data 
head(water)

#### Trends across consective days in 2023 #### 

# create subset for year 2023
water23 <- subset(water, water$Year == 2023)
water23

# Telling R the desired order for the Location variable
water23$Location <- factor(water23$Location, levels=c("MitchellCreek_UpperPiasa", "AirportRd_MiddlePiasa", "RockyFork_LowerPiasa")) # following this, you won't notice any visual change in the data frame, except for 'Location' now has the data type, 'factor'.

water23

#### Graphing NH3-N, PO4-P, NO3-N, and TSS with ggplot2 ####

# NH3-N plot
NH3_N_plot <- ggplot(water23, aes(x=Date, y=NH3_N, color=Location, group=Location)) + # indicates the dataframe to use, the x axis variable (Date), y axis variable (NH3_N), and grouping variable (Location) for colors
  geom_point() + # specifies type of plot (points)
  geom_line() + # specifies type of plot (lines to connect the points)
  ylab("NH3-N (mg/L)") # y axis label

NH3_N_plot

# PO4-P plot
PO4_P_plot <- ggplot(water23, aes(x=Date, y=PO4_P, color=Location, group=Location)) + # indicates the dataframe to use, the x axis variable (Date), y axis variable (PO4_P), and grouping variable (Location) for colors
  geom_point() + # specifies type of plot (points)
  geom_line() + # specifies type of plot (lines to connect the points)
  ylab("PO4-P (mg/L)")

PO4_P_plot

# NO3-N plot
NO3_N_plot <- ggplot(water23, aes(x=Date, y=NO3_N, color=Location, group=Location)) + # indicates the dataframe to use, the x axis variable (Date), y axis variable (NO3_N), and grouping variable (Location) for colors
  geom_point() + # specifies type of plot (points)
  geom_line() + # specifies type of plot (lines to connect the points)
  ylab("NO3-N (mg/L)")

NO3_N_plot

# TSS plot
TSS_plot <- ggplot(water23, aes(x=Date, y=TSS, color=Location, group=Location)) + # indicates the dataframe to use, the x axis variable (Date), y axis variable (TSS), and grouping variable (Location) for colors
  geom_point() + # specifies type of plot (points)
  geom_line() + # specifies type of plot (lines to connect the points)
  ylab("TSS (mg/L)")

TSS_plot

#### Compare data with past years ####

# First let's calculate the average value for each variable to the year, since we have multiple observations at some of the sites, and for all the 2023 sites
water_avgs <- water %>% 
  group_by(Location, Year) %>% # designate the grouping variable(s). In this case, we want a unique observation for each Location and Year combo.
  summarize(NH3_N = mean(NH3_N), NO3_N = mean(NO3_N), PO4_P = mean(PO4_P), TSS = mean(TSS)) # provide the formula, in this case, taking the means

water_avgs # return the resulting data frame, which summarizes species richness at each quadrat along each transect.

# go from wide to long format with the function 'pivot_longer' in the tidyr package
water_avgs_long <- pivot_longer(water_avgs, cols = NH3_N:TSS, names_to = "Variable")
head(water_avgs_long)  

# plot all years and all variables within one figure
water_avgs_long$Year <- as.factor(water_avgs_long$Year) # make Year a factor (right now it's 'numeric') so it will be given a discrete color when plotting
water_avgs_long$Location <- factor(water_avgs_long$Location, levels=c("MitchellCreek_UpperPiasa", "AirportRd_MiddlePiasa", "RockyFork_LowerPiasa")) # again telling R the preferred order for these location factors (upper to lower)

water_allyrs_plot <- ggplot(water_avgs_long, aes(x=Location, y=value, group=Year, color=Year)) + # this time we use the Year as our grouping variable
  geom_point() +
  geom_line() +
  facet_grid(rows=vars(Variable), scales="free_y") + # this plots all four graphs on the same figure, with scales set to 'free y' to allow different y axis scales
  ggtitle("NH3, NO3, PO4, and TSS in Piasa Creek (mg/L)")

water_allyrs_plot

