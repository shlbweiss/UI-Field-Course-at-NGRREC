##### Indices of Biodiversity from Wildlife Abundance Data ##### 

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
wildlife <- read.csv("wildlife_example.csv")

# inspect the data 
head(wildlife)

#### Calculating Indices of Biodiversity: Richness, Diversity, and Evenness ####

###### Species Richness ######

# calculating the number of species observed at each site

richness <- wildlife %>% 
  group_by(Site) %>% # designate the grouping variable(s)
  summarize(Richness = n()) # provide the formula, in this case, counting the rows (this is what 'n()' does)

richness # return the resulting transformed data, which summarizes species richness at each site

###### Species Diversity - Shannon-Wiener ######

diversity <- wildlife %>%
  group_by(Site) %>% # designate the grouping variable(s)
  summarize(N = sum(Abundance), # provide formula for calculating N (overall sum of individuals)
            Shannon = -sum((Abundance/sum(Abundance))*log(Abundance/sum(Abundance)))) #provide the formula for the Shannon index, being mindful of where you place your parentheses.

diversity # return the summarized data

###### Species Evenness ######

# join the diversity and richness data frames
bio_indices <- left_join(diversity, richness) # if not specified, will join based on a commonly named variable, here, that variable is 'Site'
bio_indices # return the resulting joined dataset

# calculate evenness
bio_indices$Evenness <- bio_indices$Shannon/log(bio_indices$Richness) # assigning the resulting evenness calculations to a new variable called 'Evenness'
bio_indices # return the updated data frame

#### Plotting the results ####

# first, we need to transform the data such that each observed index value is its own row, and we move the index names to one column. This is known as a 'long' format data frame
bio_indices_long <- pivot_longer(bio_indices, cols = Shannon:Evenness, names_to = "Index")

# Create a bar plot of the three indices, comparing sites
bioplot <- ggplot(data=bio_indices_long, aes(x=Site, y=value, fill=Site)) + # indicates the dataframe to use, the x axis variable (Site), y axis variable (value), and grouping variable (Site) for colors
  geom_bar(stat="identity") + # specifies type of plot
  facet_grid(rows=vars(Index), scales = "free_y") + # separates each index to its own plot in its own row
  ggtitle("Biodiversity Indices") # title

bioplot 
