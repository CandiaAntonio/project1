#load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
#Get Data
ig_issuance <- read_excel("ig_issuance.xlsx")
#Correct the Date column to the right month end format
ig_issuance_tidy <- ig_issuance %>%
  mutate(Date = parse_date_time(Date, "ym")) %>%
  mutate(year = year(Date), month = month(Date, label = T, abbr = T)) %>%
  #Volume in Billions
  mutate(volume_u = ig_issuance$`Total Volume`/1000000000) %>%
  mutate(volume = round(volume_u,2))
#Get Summary Data
ig_issuance_tidy %>% 
group_by(year) %>%
summarise(avg = sum(volume))
glimpse(ig_issuance_tidy)


#Plot 
ggplot(ig_issuance_tidy, aes(x = month, y= volume)) +
  geom_bar(stat="identity")
