#load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
#Get Data
ig_issuance <- read_excel("ig_issuance.xlsx")
#Changing variables to have a tidy format
ig_issuance_tidy <- ig_issuance %>%
  #Date as.date format adding 2 days in order to use ceiling
  mutate(Date = parse_date_time(Date, "ym") + days(2)) %>%
  #New date variable in month end format
  mutate(date = ceiling_date(Date, unit = "months") - days(1)) %>% 
  mutate(year = year(date), month = month(date, label = T, abbr = T)) %>%
  #Volume in USD Billions
  mutate(volume_u = ig_issuance$`Total Volume`/1000000000) %>%
  mutate(volume = round(volume_u,2)) %>% 
  #Rename Total Deals
  mutate(deals = ig_issuance$`Total Deals`)
#Select the tidy variables
ig <- ig_issuance_tidy %>% 
  select(date, year, month, volume, deals)

#Plot 1 Box plot 1999-2018
fill <- "#4271AE"
line <- "#1F3552"
ggplot(ig, aes(x = month, y = volume)) +
  geom_boxplot(fill = fill, colour = line, alpha = 0.7, 
               outlier.colour = "#1F3552", outlier.shape = 20,
               outlier.size = 6) + 
  scale_x_discrete(name = "Mes\n1999 - 2018") +
  scale_y_continuous(name = "Total Emision IG\nen Billones de USD",
                     breaks = seq(0,190,25),
                     limits = c(25,190)) +
  ggtitle("Boxplot emisión Corporativa Grado de Inversión") +
  theme_bw()
  

