library(readr)
library(dplyr)
library(stringr)

target_towns <- c("Sheffield", "Rotherham", "Barnsley", "Doncaster",
                  "Leeds", "Bradford", "Wakefield", "Huddersfield", "Halifax")

clean_broadband <- function(folder, file_name, year) {
  path <- file.path(folder, file_name)
  df <- read_csv(path)
  
  
  colnames(df) <- tolower(colnames(df))  # make lowercase for easier handling
  
  la_col <- grep("laua_name|local_authority|name", colnames(df), value = TRUE)[1]
  
  df_clean <- df %>%
    filter(str_detect(tolower(!!sym(la_col)), paste(tolower(target_towns), collapse = "|"))) %>%
    mutate(Year = year)
  
  return(df_clean)
}

bb_2021 <- clean_broadband("2021 broadband speed", "202105_fixed_laua_coverage_r01.csv", 2021)
bb_2022 <- clean_broadband("2022 Broadband Speed", "202205_fixed_laua_coverage_r01.csv", 2022)
bb_2023 <- clean_broadband("2023 broadband speed", "202305_fixed_laua_coverage_r02.csv", 2023)
bb_2024 <- clean_broadband("2024 broadband speed", "202401_fixed_laua_coverage_r01.csv", 2024)

broadband_all_years <- bind_rows(bb_2021, bb_2022, bb_2023, bb_2024)

glimpse(broadband_all_years)