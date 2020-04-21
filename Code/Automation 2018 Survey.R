# Automation Survey
library(haven)
library(tidyaverse)

aut <- read_sav("C:/Users/afisher/Documents/R Code/R-Code-for-Political-Science/Data/Zhang and Dafoe/YALE0065_OUTPUT.sav", user_na=TRUE) %>%
  as_factor()
