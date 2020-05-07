############################################
# Analyzing Election and Polling Data in R #
############################################

# Remove all files
rm(list = ls())

# Dataset Packages
library(politicaldata)
library(poliscidata)

# Load Packages
library(dplyr)
library(readxl)    
library(tibble)
library(purrr)
library(fs)
library(stringr)
library(ggplot2)
library(zoo)
library(lubridate)

# Path to File
path <- "C:/Users/afisher/Documents/R Code/R-Code-for-Political-Science/Data/American Presidency Project - Approval Ratings for POTUS.xlsx"

# Read Excel Sheet Names
names = excel_sheets(path)

# Function to read sheet and add column
read_pres <- function(num) {
  df = read_excel(path, num)
  df$President =names[num]
  df$President<- str_extract(df$President,"\\w+$")
  return(df)
}

# Call Function
trump = read_pres(2)
obama = read_pres(1)
bush = read_pres(3)

# Append Datasets
approval_polls <- do.call("rbind", list(bush, obama, trump))

# Rename
approval_polls = approval_polls %>% rename(Enddate = `End Date`)
approval_polls = approval_polls %>% rename(Startdate = `Start Date`)
 

# Filter Data
approval_polls %>% 
  select(President,Enddate,Approving) %>%
  head()

TrumpPolls = approval_polls %>%
  select(President,Enddate,Approving) %>%
  filter(President == "Trump")


approval_polls %>%
  group_by(President) %>%
  summarise(Approve=mean(Approving))

TrumpApproval <- approval_polls %>%
  select(President, Enddate, Approving) %>%
  filter(President == "Trump") %>%
  pull(Approving)

mean(TrumpApproval)


# Mean by Month
TrumpPolls %>%
  mutate(Month = months(ymd(Enddate))) %>%
  group_by(Month) %>%
  summarise(Approve = mean(Approving))

TrumpApproval = approval_polls %>%
  filter(President=="Trump") %>%
  mutate(Date=ymd(Enddate)) %>%
  arrange(Date)

# use the rollmean() function from the zoo package to get 
# a moving average of the last 10 polls
TrumpApproval <- TrumpApproval %>%
  mutate(AvgApprove = rollmean(Approving, 10, na.pad = TRUE, align = "right"))

ggplot(TrumpApproval) +
  aes(x=Date, y=AvgApprove)+
  geom_line()

#Approval Polls (Days passed in presidency and approval rate)

# Add Date column

approval_polls = approval_polls %>%
  mutate(Date=ymd(Enddate)) %>%
  arrange(Date)

# Get number of days between first date and current date
approval_polls <- approval_polls %>%
  arrange(Date) %>%
  group_by(President) %>%
  mutate(days=Date - min(Date))

# Rolling average
approval_polls <- approval_polls %>%
  mutate(AvgApprove = rollmean(Approving, 10, na.pad = TRUE, align = "right"))

# Plot
ggplot(approval_polls)+
  aes(x=days, y=AvgApprove, col=President)+
  geom_line() +
  xlim(c(0, 1000))