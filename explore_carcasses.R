library(tidyverse)
library(dplyr)

df <- read_csv("ML_all_carcasses_2026_April23.csv",
               quote = '"')

glimpse(df)
df |> 
  count(area, sort = TRUE) |> 
  print(n = 27)
df |> 
  count(area, sort = TRUE) |> 
  ggplot(aes(x = reorder(area, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Total carcasses by location",
    x = "Location",
    y = "Number of carcasses"
  ) +
  theme_minimal()

#adding in density column=individuals/area
DensityDS <- X20210221SegementAreaCount %>%
  mutate(density = NUMPOINTS / area_sqm)

view(DensityDS)

#renaming and modifing existing columns
DensityDS <- DensityDS %>% rename(number_individuals = NUMPOINTS, beach_location = Beach)
DensityDS <- DensityDS %>% select(-fid)
DensityDS <- DensityDS %>% rename(number_individuals_alive = number_individuals)

#getting number of dead indivduals from carcass data
df_counts <- ML_all_carcasses_2026_April23 %>%
  group_by(area) %>%
  summarise(number_individuals_dead = n())