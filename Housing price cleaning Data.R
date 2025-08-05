library(readr)
library(dplyr)
library(stringr)

# List all relevant CSVs
files <- list.files(pattern = "pp-202[1-4]\\.csv")

# Load all CSVs
housing_raw <- lapply(files, read_csv, col_names = FALSE, col_types = cols(.default = "c"))
housing_data <- bind_rows(housing_raw)

# Assign proper column names
colnames(housing_data) <- c(
  "Transaction_ID", "Price", "Date", "Postcode", "Property_Type", "Old_New",
  "Duration", "PAON", "SAON", "Street", "Locality", "Town", "District",
  "County", "PPD_Category", "Record_Status"
)

ncol(read_csv(files[1], col_names = FALSE))


# Clean up the Town field
housing_data$Town <- str_trim(housing_data$Town)
housing_data$Town <- str_to_title(housing_data$Town)

# Define target towns
target_towns <- c("Sheffield", "Rotherham", "Barnsley", "Doncaster",
                  "Leeds", "Bradford", "Wakefield", "Huddersfield", "Halifax")

# Filter and convert variables
housing_filtered <- housing_data %>%
  filter(Town %in% target_towns)

housing_filtered$Price <- as.numeric(housing_filtered$Price)
housing_filtered$Date <- as.Date(housing_filtered$Date)

# Extract year
housing_filtered$Year <- format(housing_filtered$Date, "%Y")

# Summarise by town and year
housing_summary <- housing_filtered %>%
  group_by(Town, Year) %>%
  summarise(
    Avg_Price = round(mean(Price, na.rm = TRUE)),
    .groups = 'drop'
  )

# Add Region info
south_towns <- c("Sheffield", "Barnsley", "Doncaster", "Rotherham")
west_towns <- c("Leeds", "Bradford", "Wakefield", "Huddersfield", "Halifax")

housing_summary$Region <- ifelse(housing_summary$Town %in% south_towns, "South Yorkshire", 
                                 ifelse(housing_summary$Town %in% west_towns, "West Yorkshire", NA))

# Final aggregation
housing_agg <- housing_summary %>%
  group_by(Region) %>%
  summarise(AvgPrice = round(mean(Avg_Price, na.rm = TRUE)))

print(housing_agg)
