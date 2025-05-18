#---------------------------------------#
#EDSD 2024/2025
# Course Name: Population Challenges
# Module Name: Measuring the Generational Economy:National Transfer Accounts           
# Lecturer: Bernhard Binder-Hammer 
# Day 2 Assignment
## Tuesday 13th May 2025
##student Name: Gilbert Habaasa


#Load data for population
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tibble)
pop <- read_csv("C:/Users/admin/OneDrive - London School of Hygiene and Tropical Medicine/‌INED 2024/Measuring Generational and Social/Day 2/download_swk4qj.csv")
#View(pop)

#delete unrequired columns
pop <- pop[,-c(4:9)]
#View(pop)

#Load data for population
lbr_consump <- read_csv("C:/Users/admin/OneDrive - London School of Hygiene and Tropical Medicine/‌INED 2024/Measuring Generational and Social/Day 2/download_swpr2a.csv")
#View(lbr_consump)

#delete unrequired columns
lbr_consump <- lbr_consump[,-c(4:9)]
#View(lbr_consump)

#Merge 1 database
names(pop)
names(lbr_consump)

lbr_consump <- lbr_consump %>% 
  left_join(
    pop,
    by = join_by(Country, Year, Age)
  )

View(lbr_consump)

#Save dataset
saveRDS(lbr_consump, "DependencyAfrica.rds")


#First, i filter data for Kenya only

Kenyadata <- lbr_consump %>% 
  filter(Country=="Kenya")

view(Kenyadata)


# Step 2: Filter for 1994 and 2005
Kenyadata_filtered <- Kenyadata %>%
  filter(Year %in% c(1994, 2005)) %>% 
  mutate(Year=as.factor(Year))


### Dependency Rate for Children and elderly

DR <- Kenyadata %>% 
  mutate(
    DR = case_when(Age <= 19 | Age >= 65 ~ "Numerator", TRUE ~ "Denominator")
  ) %>%
  reframe(
    pop = sum(`Population, Total`, na.rm = TRUE),
    .by = c(Year, DR)
  ) %>% 
  pivot_wider(names_from = DR, values_from = pop) %>% 
  mutate(DR = Numerator / Denominator * 100)

view(DR)

YDR <- Kenyadata %>% 
  mutate(
    YDR = case_when(Age <= 19 ~ "Numerator", Age %in% 20:64 ~ "Denominator")
  ) %>% 
  reframe(
    pop = sum(`Population, Total`, na.rm = TRUE),
    .by = c(Year, YDR)
  ) %>% 
  pivot_wider(names_from = YDR, values_from = pop) %>% 
  mutate(YDR = Numerator / Denominator * 100)

view(YDR)

ODR <- Kenyadata %>% 
  mutate(
    ODR = case_when(Age >= 65 ~ "Numerator", Age %in% 20:64 ~ "Denominator")
  ) %>% 
  reframe(
    pop = sum(`Population, Total`, na.rm = TRUE),
    .by = c(Year, ODR)
  ) %>% 
  pivot_wider(names_from = ODR, values_from = pop) %>% 
  mutate(ODR = Numerator / Denominator * 100)

view(ODR)

#test whether YDR+ODR is equal to Total Dependency ratio in Kenya
DR %>% 
  select(Year, DR) %>% 
  left_join(
   YDR %>% select(Year, YDR),
   by = join_by(Year)
  ) %>% 
  left_join(
    ODR %>% select(Year, ODR),
    by = join_by(Year)
  ) %>% 
  mutate(
    DR2 = YDR + ODR
  )

table(DR)

#Draw graph for YDR, DR and ODR by Year

DR <- tibble(
  Year = c(1994, 2005),
  DR = c(168.7, 148),   # Total dependency ratio
  YDR = c(162.0, 147.6),  # Youth dependency ratio
  ODR = c(7.2, 6.8)     # Old-age dependency ratio
)

# Convert to long format
DR_long <- DR %>%
  pivot_longer(cols = c(YDR, ODR), names_to = "RatioType", values_to = "Value")

# Plot

# Bar Graph
ggplot(DR_long, aes(x = factor(Year), y = Value, fill = RatioType)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Dependency Ratios in Kenya (1994–2005)",
    x = "Year",
    y = "Dependency Ratio (%)",
    fill = "Ratio Type"
  ) +
  theme_minimal()

