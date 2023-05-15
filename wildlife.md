---
title: "Indices of Biodiversity from Wildlife Abundance Data"
author: "UI Field Course at NGRREC 2023"
date: "`r Sys.Date()`"
output:
  rmarkdown::html_document:
    toc: true
    theme: lumen
---

## Install and load libraries

R works with different packages that provide specific functionalities beyond what is offered from 'base R'. In order to use the tools within a specific package, you must install and load the package within your current R session.

Here, we're loading the packages, 'tidyr', 'dplyr' and 'ggplot2'
tidyr and dplyr are both packages within the tidyverse family. You can read more about tidyverse here: https://www.tidyverse.org/ 

ggplot2 is a package used to create flexible graphics with R.

```{r}
# load the packages we'll need to work with
library(ggplot2)
library(tidyr)
library(dplyr)
```
If this is your first time using these packages within R, you'll need to install them by running the following commands: 

*install.packages('ggplot2')*

*install.packages('tidyr')*

*install.packages('dplyr')*

## Set working directory

The advantage of setting a working directory at the start is that you don't have to define a full file path every time you read in a file, so long as it resides in the defined working directory.

```{r}
# Set working directory to the folder that holds my data files
setwd("C:/Users/saweiss.LC/OneDrive - Lewis and Clark Community College/UI Field Course/UI-Field-Course-at-NGRREC/")
```

## Read in dataset 

In order to read in your data into R, you must use the proper function that will recognize the file format of your data file. Here, we are reading in a .csv file (stands for 'comma separated values'), which is a common file format for storing large datasets.
```{r}
wildlife <- read.csv("wildlife_example.csv")

# inspect the data 
head(wildlife)

```

## Calculating Indices of Biodiversity: Richness, Diversity, and Evenness

### Species Richness

Species richness equals the number of species observed at a site.

$$R = s$$
Where s is the number of different species observed.

To count up the number of unique species at each site, we are using the 'group_by' and 'summarize' functions from the 'dplyr' package. We are also using pipes (noted with '%>%') to allow us to efficiently move the data through both functions in sequence.
```{r}
# calculating the number of species observed at each site
richness <- wildlife %>% 
  group_by(Site) %>% # designate the grouping variable(s)
  summarize(Richness = n()) # provide the formula, in this case, counting the rows (this is what 'n()' does)

richness # return the resulting transformed data, which summarizes species richness at each site
```

### Species Diversity - Shannon-Wiener

A common measure of diversity used in ecology is the Shannon-Wiener Index (H'), calculated as: 

$$H’ = -∑ (p_i * ln[p_i])$$

Where 
$$p_i = n_i/N$$		

n is the number of individuals of a given species and N is the total number of individuals in a sample.

```{r}
diversity <- wildlife %>%
  group_by(Site) %>% # designate the grouping variable(s)
  summarize(N = sum(Abundance), # provide formula for calculating N (overall sum of individuals)
            Shannon = -sum((Abundance/sum(Abundance))*log(Abundance/sum(Abundance)))) #provide the formula for the Shannon index, being mindful of where you place your parentheses.

diversity # return the summarized data
```

### Species Evenness

Evenness is a measure of how similar the abundances of different species are. E values can range from close to 0 (where most species are rare and just a few are abundant) to 1 (where species are equally abundant). Evenness is calculated using both H' and R:

$$E = H’/ln(R)$$

To make use of both our diversity and richness tables in this final calculation, we will join these two datasets together.
```{r}
bio_indices <- left_join(diversity, richness) # if not specified, will join based on a commonly named variable, here, that variable is 'Site'

bio_indices # return the resulting joined dataset
```

Now that we have a data frame with both H' and R, we can add the variable 'Evenness'.

Specific columns within a data set can be called on with the '$' symbol, followed by the name of the column. You can perform calculations using the contents of specific columns in the data frame this way.
```{r}
bio_indices$Evenness <- bio_indices$Shannon/log(bio_indices$Richness) # assigning the resulting evenness calculations to a new variable called 'Evenness'

bio_indices # return the updated data frame
```

## Plotting the results

Let's make some bar plots of these indices, comparing the two sites. To do this, we'll use the ggplot2 package, which is a commonly used R graphing package, that allows for lots of flexible formatting options.

```{r}
# first, we need to transform the data such that each observed index value is its own row, and we move the index names to one column. This is known as a 'long' format data frame
bio_indices_long <- pivot_longer(bio_indices, cols = Shannon:Evenness, names_to = "Index")

# Create a bar plot of the three indices, comparing sites
bioplot <- ggplot(data=bio_indices_long, aes(x=Site, y=value, fill=Site)) + # indicates the dataframe to use, the x axis variable (Site), y axis variable (value), and grouping variable (Site) for colors
  geom_bar(stat="identity") + # specifies type of plot
  facet_grid(rows=vars(Index), scales = "free_y") + # separates each index to its own plot in its own row
  ggtitle("Biodiversity Indices") # title

bioplot 
```

