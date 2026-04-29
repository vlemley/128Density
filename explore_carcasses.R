library(tidyverse)

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
