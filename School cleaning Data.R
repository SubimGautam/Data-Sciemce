library(readr)
library(dplyr)
library(stringr)


if (!file.exists("school dataset.csv")) {
  stop("Input file 'school dataset.csv' not found. Please check the file path.")
}


school_data <- read.csv("school dataset.csv", stringsAsFactors = FALSE)


cat("Column names in school_data:\n")
print(colnames(school_data))

school_data_clean <- school_data %>%
  mutate(TOWN = str_trim(TOWN)) %>%
  filter(TOWN != "" & !str_detect(TOWN, "^[0-9]+$"))

cat("Rows after initial cleaning:", nrow(school_data_clean), "\n")


school_data_clean <- school_data_clean %>%
  mutate(TOWN = str_to_title(TOWN))


south_yorkshire_towns <- c("Sheffield", "Barnsley", "Rotherham", "Doncaster")
west_yorkshire_towns <- c("Leeds", "Bradford", "Wakefield", "Huddersfield", "Halifax")
valid_towns <- c(south_yorkshire_towns, west_yorkshire_towns)


school_filtered <- school_data_clean %>%
  filter(TOWN %in% valid_towns)


cat("Rows after filtering for valid towns:", nrow(school_filtered), "\n")
if (nrow(school_filtered) == 0) {
  message("No rows match valid towns. Available towns:")
  print(unique(school_data_clean$TOWN))
}


school_clean <- school_filtered[, c(
  "URN", "SCHNAME", "LEA", "AGERANGE", "TALLPUP_1618", "TOWN", "PCODE"
), drop = FALSE]


cat("Unique towns in final dataset:\n")
print(unique(school_clean$TOWN))

cat("\nMissing values per column:\n")
print(colSums(is.na(school_clean)))

cat("\nTotal schools retained:\n")
print(nrow(school_clean))

str(school_clean)


if (nrow(school_clean) > 0) {
  write.csv(school_clean, "cleaned_school.csv", row.names = FALSE)
  cat("Successfully wrote cleaned_school.csv\n")
} else {
  message("Cannot write CSV: school_clean is empty.")
}

