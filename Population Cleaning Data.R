library(dplyr)
library(readr)
library(stringr)

population_2011 <- read.csv("~/Desktop/Housing Data/population 2011.csv")

names(population_2011)
head(population_2011)
str(population_2011)

population_2011_clean <- population_2011 %>%
  mutate(
    Population = as.numeric(gsub(",", "", Population))
  )

population_2011_clean <- population_2011_clean %>%
  mutate(
    Postcode = gsub("\\s+", " ", trimws(Postcode))
  )