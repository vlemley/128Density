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
X20210221SegementAreaCount <- read_csv("20210221SegementAreaCount.csv")
#adding in density column=individuals/area
DensityDS <- X20210221SegementAreaCount %>%
  mutate(density = NUMPOINTS / area_sqm)

view(DensityDS)

#renaming and modifing existing columns
DensityDS <- DensityDS %>% rename(number_individuals = NUMPOINTS, beach_location = Beach)
DensityDS <- DensityDS %>% select(-fid)
DensityDS <- DensityDS %>% rename(number_individuals_alive = number_individuals)

#getting number of dead individuals, merging APGW and APGw into one location
df_counts <- df %>%
  mutate(area = case_when(
    area == "APGW" ~ "APGw",
    TRUE ~ area
  )) %>%
  group_by(area) %>%
  summarise(number_individuals_dead = n())
#joining tables, keeping all locations even if no density match
combined <- DensityDS %>%
  full_join(df_counts, by = c("beach_location" = "area"))
#adding mortality rate column
combined <- combined %>%
  mutate(mortality_rate = number_individuals_dead / number_individuals_alive)
view(combined)
#scatterplot density vs mortality rate
combined %>%
  filter(!is.na(density) & !is.na(mortality_rate)) %>%
  ggplot(aes(x = density, y = mortality_rate, label = beach_location)) +
  geom_point(size = 3, color = "steelblue") +
  geom_text(nudge_y = 0.02, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
  labs(
    title = "Density vs Mortality Rate",
    x = "Seal Density (individuals/sqm)",
    y = "Mortality Rate (dead/alive)",
  ) +
  theme_minimal()
