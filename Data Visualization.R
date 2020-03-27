# Data Visualization
# https://r4ds.had.co.nz/data-visualisation.html

# Clear all variables
rm(list = ls())

# load packages
library(tidyverse)
library(dplyr)
library(politicaldata)
#https://cran.r-project.org/web/packages/politicaldata/politicaldata.pdf

#######################################################################
# This dataset contains responses to Gallup's Most Important Problem # 
# question aggregated at the annual level and coded by major topic  #
######################################################################

# Read in Data from 2000-2017
prob = cap_get_mip(min_year = 2000, max_year = 2017)

# Major Problems in 2017
prob2017 = filter(prob, year==2017)

# Plot percent 
ggplot(prob2017) + aes(y = title_text, x = percent) + geom_bar(stat = 'identity')

# Change color and style
ggplot(prob2017) + aes(y = title_text, x = percent) + geom_bar(stat = 'identity')

ggplot(prob2017) + aes(y = title_text, x = percent) +
                   geom_bar(stat = 'identity', color='blue', fill='white')

ggplot(prob2017) + aes(y = title_text, x = percent) +
  geom_bar(stat = 'identity', fill='steelblue') +
  theme_minimal()

# Add Labels
ggplot(prob2017) + aes(y = title_text, x = 100*percent) +
  geom_bar(stat = 'identity', fill='steelblue') +
  geom_text(aes(label=round(100*percent)), hjust=-0.3, size=3.5) +
  theme_minimal()

####################
# DW Nominate Data #
####################

# Read Data
con116 = get_senate_nominate(congress = 116)

# View Data
str(con116)

# Version 1
con116$Party = ifelse(con116$party_code ==100 ,"Democrat",
                      ifelse(con116$party_code==200, "Republican",
                             "Other"))
# Version #2
con116 = con116 %>% mutate(Party = ifelse(con116$party_code ==100 ,"Democrat",
                                          ifelse(con116$party_code==200, "Republican",
                                                 "Other")))

# Filter Rows with filter()
dems = filter(con116, Party=='Democrat')