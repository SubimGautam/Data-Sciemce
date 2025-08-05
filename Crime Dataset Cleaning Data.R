library(dplyr)
library(readr)
library(stringr)

base_dir <- "~/Desktop/Housing Data/Crime Dataset"


folders <- list.dirs(base_dir, recursive = FALSE)

read_all_crimes <- function(folder_path) {
  files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  csv_data <- lapply(files, read_csv)
  bind_rows(csv_data)
}

crime_data <- bind_rows(lapply(folders, read_all_crimes))

str(crime_data)

names(crime_data)

target_towns <- c("sheffield", "rotherham", "barnsley", "doncaster",
                  "leeds", "bradford", "wakefield", "huddersfield", "halifax")

# Convert to lowercase for reliable matching
crime_data_clean <- crime_data %>%
  mutate(across(c(Location, `LSOA name`), tolower)) %>%
  filter(
    str_detect(Location, paste(target_towns, collapse = "|")) |
      str_detect(`LSOA name`, paste(target_towns, collapse = "|"))
  )

nrow(crime_data_clean)
table(crime_data_clean$`Crime type`)

crime_data_clean$Month <- as.Date(paste0(crime_data_clean$Month, "-01"))

crime_2023 <- crime_data_clean %>%
  filter(format(Month, "%Y") == "2023")


crime_data_clean <- crime_data_clean %>%
  mutate(Town = case_when(
    str_detect(Location, "sheffield") ~ "SHEFFIELD",
    str_detect(Location, "leeds") ~ "LEEDS",
    str_detect(Location, "barnsley") ~ "BARNSLEY",
    str_detect(Location, "bradford") ~ "BRADFORD",
    str_detect(Location, "wakefield") ~ "WAKEFIELD",
    str_detect(Location, "rotherham") ~ "ROTHERHAM",
    str_detect(Location, "doncaster") ~ "DONCASTER",
    str_detect(Location, "huddersfield") ~ "HUDDERSFIELD",
    str_detect(Location, "halifax") ~ "HALIFAX",
    TRUE ~ "OTHER"
  ))

table(crime_data_clean$Town)


