library(tidyverse)
library(dplyr)

carcassdata<- read_csv("ML_all_carcasses_2026_April23.csv",
               quote = '"')

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

view(DensityDS)

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

#grouping locations together to see if there is a larger scale trend
unique(combined$beach_location)

combinedlocation <- combined %>%
  mutate(combinedlocation = case_when(
    beach_location %in% c("NP", "NPC", "NP0", "NP1", "NP2", "NP3", "NP4", "NP5", "NPG0", "NPGa", "NPG1", "NPG4", "NPD")          ~ "North Point",
    beach_location %in% c("BBN", "BBNS", "BBNN")    ~ "Bight Beach North",
    beach_location %in% c("MBBL", "MBBU", "Mid Bight NS")              ~ "Mid Bight Beach",
    beach_location %in% c("BMB", "BMNN", "BMD","BMC", "BMS","BMD", "BMN")              ~ "Big Midden Beach",
    beach_location %in% c("BBSL", "BBSU","")              ~ "Bight Beach South",
    beach_location %in% c("AP", "APG","APGw")              ~ "Ano Point",
    beach_location %in% c("SBW", "SBE", "SBW water")              ~ "South Beach",
    beach_location %in% c("TSW", "TSC", "TSD", "TSE","Tar sands")              ~ "Tar Sands Beach",
    beach_location %in% c("FSB")              ~ "Fault Slip Beach",
    TRUE                                    ~ beach_location  # keeps anything not listed as-is
  ))

#combining data for respective location groups

combinedlocation <- combinedlocation %>%
  group_by(combinedlocation) %>%
  summarise(
    number_individuals_dead = sum(number_individuals_dead),
    number_individuals_alive = sum(number_individuals_alive),
    area_sqm = sum(area_sqm),
    mortality_rate = number_individuals_dead / number_individuals_alive,
    density = number_individuals_dead / area_sqm
  )

#making Nan and inf=0 so density and mortality calcs can go through
combinedlocation <- combinedlocation %>%
  mutate(across(everything(), ~ifelse(is.nan(.), 0, .)))%>%
  mutate(across(everything(),~ifelse(is.infinite(.), 0, .)))

view(combinedlocation)

#scatterplot density vs mortality rate with combined (emiting NA)
combined %>%
  filter(!is.na(density) & !is.na(mortality_rate)) %>%
  ggplot(aes(x = density, y = mortality_rate, label=beach_location)) +
  geom_point(size = 3, color = "steelblue") +
  geom_text(nudge_y = 0.02, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
  labs(
    title = "Relationship Between Density and Mortality Rate 02-21-2026",
    x = "Seal Density (individuals/sqm)",
    y = "Mortality Rate (dead/alive)",
  ) +
  theme_minimal()

#scatterplot density vs mortality rate with combined locations (regional)

combinedlocation %>%
  filter(!is.na(density) & !is.na(mortality_rate)) %>%
  ggplot(aes(x = density, y = mortality_rate, label=combinedlocation)) +
  geom_point(size = 3, color = "steelblue") +
  geom_text(nudge_y = 0.02, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
  labs(
    title = "Relationship Between Density and Mortality Rate 02-21-2026",
    x = "Seal Density (individuals/sqm)",
    y = "Mortality Rate (dead/alive)",
  ) +
  theme_minimal()


#summary stats for combined Density vs MR (not stat sig)
model <- lm(density ~ mortality_rate, data = combined)
summary(model)

#summary stats for combined location Density vs MR
combinedlocationmodel <- lm(density ~ mortality_rate, data = combinedlocation)
summary(combinedlocationmodel)

