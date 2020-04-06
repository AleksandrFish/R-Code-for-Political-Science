rm(list = ls())

# Load Packages
library(haven)
library(foreign)
library(tidyverse)
library(naniar)
library(ggthemes)

# Read Data
pew2018 <- read_sav("C:/Users/afisher/Documents/R Code/R-Code-for-Political-Science/Code/Pew Global Attitudes and Trends 2018/Pew Research Global Attitudes Spring 2018 Dataset WEB FINAL.sav", user_na=TRUE) %>%
                  as_factor()

pew2018 = read_sav("C:/Users/afisher/Documents/R Code/R-Code-for-Political-Science/Code/Pew Global Attitudes and Trends 2018/Pew Research Global Attitudes Spring 2018 Dataset WEB FINAL.sav")
pew2018_label = read.spss("C:/Users/afisher/Documents/R Code/R-Code-for-Political-Science/Pew Global Attitudes and Trends 2018/Pew Research Global Attitudes Spring 2018 Dataset WEB FINAL.sav", to.data.frame=TRUE)

# Get country names
pew2018_label = rename(pew2018_label, country = COUNTRY)
pew2018_label = select(pew2018_label, ID, country)
pew2018 = merge(pew2018_label,pew2018,by='ID')

# Select 

pew = pew2018 %>%
  select(COUNTRY, satisfied_democracy, gender20yr, 
             gender20yr_fu, trade_jobs, econ_power, confid_putin, confid_putin,
             cyberattack_infrastructure, cyberattack_elections, cyberattack_natsec,
             sex, age, d_political_scale_us)

# Gender
pew = pew %>%
  mutate(gender = ifelse(sex==1, "Male", "Female"))

# Age Group
pew = pew %>%
  mutate(age_group = ifelse(age >=18 & age <=30, '18-30', 
                     ifelse(age >30 & age <=45, '30-45',
                     ifelse(age >45 & age <=60, '45-60', '60+'))))

# Mean of support for democracy
range(pew$satisfied_democracy) # includes missing values with 8/9

# Change to missing (one column)
pew$satisfied_democracy[pew$satisfied_democracy %in% c(8,9)] = NA 

# Change all columns to missing based on condition
#pew = pew %>% replace_with_na_all(condition = ~.x %in% c(8,9))

# For more on missing see: https://cran.r-project.org/web/packages/naniar/vignettes/replace-with-na.html

# Reverse scale
pew$satisfied_democracy = 5-pew$satisfied_democracy

# Mean by group ( Satifaction with Democracy by Country)
pew_group = pew %>% 
  group_by(country) %>%
  summarize(satdem = mean(satisfied_democracy, na.rm=TRUE))

# Barplot with all the touches
ggplot(pew_group) +
  aes(x=satdem, y=reorder(country, -satdem)) +
  geom_bar(stat='identity', color = 'black', fill= 'steelblue',
           width=0.5, position = position_dodge(width=0.5)) +
  labs(title="Satisfaction with Democracy by Country",
       x ="Satisfaction with Democracy (1-4)", y = "")+
  theme(plot.title = element_text(size=12, face="bold.italic"),
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    axis.text.y = element_text(size=10)) 

# Mean by group (Satifaction with Democracy by Country and Gender)
pew_group_female = pew %>% 
  group_by(country, gender) %>%
  summarize(satdem = mean(satisfied_democracy, na.rm=TRUE))

ggplot(pew_group_female) +
  aes(x=satdem, y=reorder(country, -satdem), fill=gender) +
  geom_col(width=0.7, position = position_dodge(width=0.5)) +
  labs(title="Satisfaction with Democracy \n by Country and Gender",
       x ="Satisfaction with Democracy (1-4)", y = "")+
  theme(plot.title = element_text(size=12, face="bold.italic"),
        axis.title.x = element_text(size=10, face="bold"),
        axis.title.y = element_text(size=10, face="bold"),
        axis.text.y = element_text(size=10)) 



 
 #Difference in satisfaction between men and women
pew_group_female = pew_group_female %>%
  group_by(country) %>%
  mutate(Diff = satdem - lag(satdem))

pew_group_female = na.omit(select(pew_group_female, country,Diff))

ggplot(pew_group_female) +
  aes(x=Diff, y=reorder(country, -Diff)) +
  geom_col(width=0.7, position = position_dodge(width=0.5)) +
  labs(title="Difference Between Men and Women \n Satisfaction with Democracy",
       x ="Difference between Men and Women", y = "")+
  theme(plot.title = element_text(size=12, face="bold.italic"),
        axis.title.x = element_text(size=10, face="bold"),
        axis.title.y = element_text(size=10, face="bold"),
        axis.text.y = element_text(size=10))


# Political ideology (conservative to liberal)
range(pew$d_political_scale_us, na.rm=TRUE)
pew$d_political_scale_us[pew$d_political_scale_us %in% c(8,9)] = NA

range(pew$confid_putin, na.rm=TRUE)
pew$confid_putin[pew$confid_putin %in% c(8,9)] = NA
pew$confid_putin = 5-pew$confid_putin

# Read Data
pew2018 <- read_sav("C:/Users/afisher/Documents/R Code/R-Code-for-Political-Science/Code/Pew Global Attitudes and Trends 2018/Pew Research Global Attitudes Spring 2018 Dataset WEB FINAL.sav", user_na=TRUE) %>%
  as_factor()


pew2018 <-  pew2018 %>%
  mutate(views_on_women = case_when(
    (gender20yr == "Increased" & gender20yr_fu == "Good thing") | 
      (gender20yr == "Decreased" & gender20yr_fu == "Bad thing") ~ "Feminist",
    (gender20yr == "Decreased" & gender20yr_fu == "Good thing") | 
      (gender20yr == "Increased" & gender20yr_fu == "Bad thing") ~ "Anti-Feminist",
  ))

table(pew2018$d_political_scale_us, pew2018$views_on_women)

levels(pew2018$d_educ_us_2017)