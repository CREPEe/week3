EMDAT<-read.csv("EMDAT.csv")
library(tidyverse)
library(skimr)
library(dplyr)
glimpse(EMDAT)

Week3 <- EMDAT %>%
  dplyr::select(Entity, Year, deaths_all_disasters, injured_all_disasters, homeless_all_disasters)
table_death <- EMDAT %>%
  group_by(total_damages_all_disasters) %>%
  summarize(avg_death = mean(deaths_all_disasters, na.rm = TRUE))

table_injure <- EMDAT %>%
  group_by(total_damages_all_disasters) %>%
  summarize(avg_injured = mean(injured_all_disasters, na.rm = TRUE))

table_homeless <- EMDAT %>%
  group_by(total_damages_all_disasters) %>%
  summarize(avg_homelessness = mean(homeless_all_disasters, na.rm = TRUE))

Ordered_death <- table_death %>%
  arrange(desc(avg_death)) %>%
  slice(1:10)

Ordered_injure <- table_injure %>%
  arrange(desc(avg_injured)) %>%
  slice(1:10)

Ordered_homelessness <- table_homeless %>%
  arrange(desc(avg_homelessness)) %>%
  slice(1:10)

New <- EMDAT %>%
  select(deaths_all_disasters, injured_all_disasters, homeless_all_disasters, ) %>%
  mutate(Major_death = deaths_all_disasters > 500)
glimpse(New)
NEW.EMDAT<- EMDAT %>%
  mutate(Major_death = deaths_all_disasters)
tablefinaldeath<-Ordered_death%>%
  pivot_wider(names_from = total_damages_all_disasters, 
              values_from = c(total_damages_all_disasters,))
tablefinalinjured<-Ordered_injure%>%
  pivot_wider(names_from = total_damages_all_disasters, 
              values_from = c(total_damages_all_disasters,))
tablefinalhomeless<-Ordered_homelessness%>%
  pivot_wider(names_from = total_damages_all_disasters, 
              values_from = c(total_damages_all_disasters,))

