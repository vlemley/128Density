install.packages("tidyverse")
install.packages("dplyr")
library(tidyverse)
library(dplyr)

#reading in carcass data
carcassdata<- read_csv("ML_all_carcasses_2026_April23.csv",
               quote = '"')

#previewing carcass data
glimpse(carcassdata)
carcassdata |> 
  count(area, sort = TRUE) |> 
  print(n = 27)
carcassdata |> 
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

#renaming and modifing existing columns
DensityDS <- DensityDS %>% rename(number_individuals = NUMPOINTS, beach_location = Beach)
DensityDS <- DensityDS %>% select(-fid)
DensityDS <- DensityDS %>% rename(number_individuals_alive = number_individuals)

#getting number of dead individuals, merging APGW and APGw into one location
carcasscount <- carcassdata %>%
  mutate(area = case_when(
    area == "APGW" ~ "APGw",
    TRUE ~ area
  )) %>%
  group_by(area) %>%
  summarise(number_individuals_dead = n())

#joining tables, keeping all locations even if no density match
combined <- DensityDS %>%
  full_join(carcasscount, by = c("beach_location" = "area"))

#replace Na in number_individuals_dead with 0
combined$number_individuals_dead[is.na(combined$number_individuals_dead)] <- 0

#adding mortality rate column
combined <- combined %>%
  mutate(mortality_rate = number_individuals_dead / number_individuals_alive)

#replacing NaN and inf with 0
combined <- combined %>%
  mutate(across(everything(), ~ifelse(is.nan(.), 0, .)))%>%
  mutate(across(everything(),~ifelse(is.infinite(.), 0, .)))

#deleting repeating rows (17,18,19,20)
combined <- combined %>% slice(-c(17, 18, 19,20)) 

#combining SBW water with SBW and Tar sands with TSB and Mid Bight NS to MBBL
combined$number_individuals_dead[combined$beach_location == "SBW"] <- combined$number_individuals_dead[combined$beach_location == "SBW"] + combined$number_individuals_dead[combined$beach_location == "SBW water"]

combined$number_individuals_dead[combined$beach_location == "TSB"] <- combined$number_individuals_dead[combined$beach_location == "TSB"] + combined$number_individuals_dead[combined$beach_location == "Tar sands"]

combined$number_individuals_dead[combined$beach_location == "MBBL"] <- combined$number_individuals_dead[combined$beach_location == "MBBL"] + combined$number_individuals_dead[combined$beach_location == "Mid Bight NS"]

#remove SBW water and Tar sands
combined <- combined[-c(39,40,37), ]

#initial graph emiting NA
combinedplot <- combined %>%
  filter(!is.na(density) & !is.na(mortality_rate)) %>%
  filter(density > 0 & mortality_rate > 0) %>%
  ggplot(aes(x = density, y = mortality_rate, label = beach_location)) +
  geom_smooth(method = "lm") +
  geom_point() 
combinedplot

#summary stats for combined Density vs MR (not stat sig)
model <- lm(density ~ mortality_rate, data = combined)
summary(model)

##testing for non-linear relationship 
#Kendall's Tau correlation test
cor.test(combined$density, combined$mortality_rate, method = "kendall")

#plotting with LOESS and linear
KTplot <- combined %>%
  filter(!is.na(density) & !is.na(mortality_rate)) %>%
  ggplot(aes(x = density, y = mortality_rate)) +   # close aes() AND ggplot() here
  geom_point(color = "#0072B2") +                                      # blue
  geom_smooth(method = "loess", color = "#D55E00") +                   # orange
  geom_smooth(method = "lm", color = "#3D3D3D", linetype = "dashed") + # vermillion
  labs(
    title = "Elephant Seal Density and Mortality Rate by Beach Region",
    y = "Mortality Rate (# dead / # alive)",
    x = expression("# alive / m"^2)
    )+
  theme_classic() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.caption = element_text(size = 8, color = "grey40"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey95")
  )
KTplot




