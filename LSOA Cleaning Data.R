library(dplyr)
library(readr)


postcode_lsoa <- read_csv("~/Desktop/Housing Data/Postcode to LSOA 1.csv")

str(postcode_lsoa)
names(postcode_lsoa)

postcode_lsoa_clean <- postcode_lsoa %>%
  select(Postcode = pcds, LSOA_code = lsoa11cd, LSOA_name = lsoa11nm, Town = ladnm)

target_towns <- c("Sheffield", "Rotherham", "Barnsley", "Doncaster", 
                  "Leeds", "Bradford", "Wakefield", "Huddersfield", "Halifax")

postcode_lsoa_clean <- postcode_lsoa_clean %>%
  filter(Town %in% target_towns)