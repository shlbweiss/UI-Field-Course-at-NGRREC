##### Indices of Biodiversity and Species Percent Cover from Vegetation Transect Data #####

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
veg <- read.csv("vegetation_example.csv")

# inspect the data 
head(veg)

#### Calculating Indices of Biodiversity: Richness, Diversity, and Evenness ####

###### Species Richness ######
 
# calculating the number of species observed across all quadrats in the wetland by distance

unique_veg <- unique(veg[,c("Distance", "Species")]) #list of unique species at each distance
richness <- unique_veg %>% 
  group_by(Distance) %>% # designate the grouping variable(s)
  summarize(Richness = n()) # provide the formula, in this case, counting the rows (this is what 'n()' does)

richness # return the resulting data frame, which summarizes species richness at each quadrat along each transect

###### Species Diversity - Shannon-Wiener ######

avg_cov <- veg %>%
  group_by(Distance, Species) %>% # designate the grouping variable(s)
  summarize(AvgCov = mean(Percent_Cover))           

diversity_table <- avg_cov %>%
  group_by(Distance) %>%
  summarize(N = sum(AvgCov), # provide formula for calculating N (overall sum of individuals)
            pi = AvgCov/sum(AvgCov),
            lnpi = log(pi),
            Shannon = -(pi*lnpi)) 

diversity_table # return the summarized data

# finish calculating overall Shannon's index for the site by distance 
diversity <- diversity_table %>%
  group_by(Distance) %>%
  summarize(Shannon=sum(Shannon))

diversity # return the summarized data 

###### Species Evenness ######

# join the diversity and richness data frames
bio_indices <- left_join(diversity, richness) # if not specified, will join based on a commonly named variable(s), here, those variables are 'Distance' and Transect'.
bio_indices # return the resulting joined dataset

# calculate evenness
bio_indices$Evenness <- bio_indices$Shannon/log(bio_indices$Richness) # assigning the resulting evenness calculations to a new variable called 'Evenness'
bio_indices # return the updated data frame

#### Plotting the results ####

# first, we need to transform the data such that each observed index value is its own row, and we move the index names to one column. This is known as a 'long' format data frame
bio_indices_long <- pivot_longer(bio_indices, cols = Shannon:Evenness, names_to = "Index")

# Create a bar plot of the three indices, comparing sites
bioplot <- ggplot(data=bio_indices_long, aes(x=as.factor(Distance), y=value)) + # indicates the dataframe to use, the x axis variable (Site), y axis variable (value), and grouping variable (Site) for colors
  geom_point() + # specifies type of plot
  facet_grid(rows=vars(Index), scales = "free_y") + # separates each index to its own plot in its own row
  ggtitle("Biodiversity Indices") + # title 
  xlab("Distance (m)") + # x axis label
  ylab("Index Value") #y axis label

bioplot 

#### Average and Relative Percent Cover by Species ####

# We'll want to add in some 0 values to account for not every species being in every quadrat. To do that we'll transform the dataframe into a 'wide' format, with each species now having its own column

veg_wide <- pivot_wider(veg, id_cols = c(Transect, Distance), names_from = Species, values_from = Percent_Cover, values_fill = 0) # we make sure to set values_fill equal to zero so that non observations are counted as '0'.
head(veg_wide) # inspect what the wide format data frame looks like


# we can now move back to long format to finish summarizing. You should notice that our new dataframe is longer than the original 'veg' dataframe since we have now added in those zero observations
veg_wzeros <- pivot_longer(veg_wide, cols = `Phalaris arundinacea`:`Stellaria media`, names_to = "Species", values_to = "Percent_Cover")
head(veg_wzeros) # cols argument is the first:last species colummns. I had to use `` around the names because the names include spaces.

# what is the total percent cover across all quadrats?
tot_cover <- sum(veg_wzeros$Percent_Cover) # assign the value to an object so we can reference later

# summarizing average and relative percent covers by species
avg_perc_cov <- veg_wzeros %>%
  group_by(Species) %>% # this time our grouping variable is "Species"
  summarize(Avg_Perc_Cover = mean(Percent_Cover), 
            Rel_Perc_Cover = (sum(Percent_Cover)/tot_cover)*100) 
head(avg_perc_cov)

# saving as a csv to my working directory
write.csv(avg_perc_cov, "Percent_Cover_by_Species.csv")

