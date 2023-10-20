
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
  select(deaths_all_disasters, injured_all_disasters, homeless_all_disasters) %>%
  mutate(Major_death = deaths_all_disasters > 500)

New_EMDAT <- EMDAT %>%
  mutate(Major_death = deaths_all_disasters > 500)

New_death <- Ordered_death %>%
  mutate(identifier_column = row_number())

Wide_death <- Ordered_death %>%
  pivot_wider(
  names_from = total_damages_all_disasters,
  values_from = c(total_damages_all_disasters,)
  )

Wide_homelessness <- Ordered_homelessness %>%
  pivot_wider(
    names_from = total_damages_all_disasters,
    values_from = c(total_damages_all_disasters,)
  )

Wide_injure <- Ordered_injure %>%
  pivot_wider(
    names_from = total_damages_all_disasters,
    values_from = c(total_damages_all_disasters,)
  )

rm(Wide_death1)


Long_death <- table_death %>%
  pivot_longer(
    cols = c("1447500", "683200"), 
    names_to = "total_damages_all_disasters",
    values_to = "avg_death"
  )
glimpse(New_EMDAT)

New_EMDAT$Major_death

filter(deaths_all_disasters == "Primates", deaths_all_disasters > 10) %>%
arrange(name) %>%

All_result <- Ordered_death %>%
  left_join(Ordered_injure, by = "")



  summar