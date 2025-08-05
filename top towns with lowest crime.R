library(dplyr)
library(readr)
library(ggplot2)
library(stringr)

# Set working directory
setwd("~/Desktop/Housing Data")

# Load data
crime_data <- read_csv("cleaned_crime.csv")
population_data <- read_csv("cleaned_population_2011.csv") %>%
mutate(Population = as.numeric(gsub(",", "", Population)))
postcode_lsoa <- read_csv("cleaned_postcode_lsoa.csv")

# Check postcode_lsoa structure
cat("Postcode-LSOA mapping structure:\n")
print(head(postcode_lsoa))

# Placeholder population data (2011 Census estimates)
placeholder_pop <- tibble(
  Town = c("SHEFFIELD", "ROTHERHAM", "BARNSLEY", "DONCASTER", "LEEDS", "BRADFORD", "WAKEFIELD", "HUDDERSFIELD", "HALIFAX"),
  Population = c(552698, 257280, 231626, 302468, 751485, 522452, 325837, 162949, 88134)
)

# Clean crime data
target_towns <- c("sheffield", "rotherham", "barnsley", "doncaster", "leeds", "bradford", "wakefield", "huddersfield", "halifax")

crime_data_clean <- crime_data %>%
  mutate(across(c(Location, `LSOA name`), tolower)) %>%
  mutate(Town = case_when(
    str_detect(Location, "sheffield") | str_detect(`LSOA name`, "sheffield") ~ "SHEFFIELD",
    str_detect(Location, "leeds") | str_detect(`LSOA name`, "leeds") ~ "LEEDS",
    str_detect(Location, "barnsley") | str_detect(`LSOA name`, "barnsley") ~ "BARNSLEY",
    str_detect(Location, "bradford") | str_detect(`LSOA name`, "bradford") ~ "BRADFORD",
    str_detect(Location, "wakefield") | str_detect(`LSOA name`, "wakefield") ~ "WAKEFIELD",
    str_detect(Location, "rotherham") | str_detect(`LSOA name`, "rotherham") ~ "ROTHERHAM",
    str_detect(Location, "doncaster") | str_detect(`LSOA name`, "doncaster") ~ "DONCASTER",
    str_detect(Location, "huddersfield") | str_detect(`LSOA name`, "huddersfield") ~ "HUDDERSFIELD",
    str_detect(Location, "halifax") | str_detect(`LSOA name`, "halifax") ~ "HALIFAX",
    TRUE ~ "OTHER"
  )) %>%
  filter(Town != "OTHER")

# Summarize crimes by town
crime_by_town <- crime_data_clean %>%
  group_by(Town) %>%
  summarise(Total_Crimes = n())

# Join with placeholder population (replace with postcode_lsoa if possible)
crime_with_population <- crime_by_town %>%
  left_join(placeholder_pop, by = "Town")

# Check join result
cat("Crime with population data:\n")
print(crime_with_population)

# Calculate crime rates
crime_rates <- crime_with_population %>%
  mutate(Crime_Rate_Per_1000 = (Total_Crimes / Population) * 1000) %>%
  filter(!is.na(Crime_Rate_Per_1000)) %>%
  arrange(Crime_Rate_Per_1000)

# Verify crime rates
cat("Crime rates by town (lowest to highest):\n")
print(crime_rates)

# Add Region for coloring
crime_rates <- crime_rates %>%
  mutate(Region = case_when(
    Town %in% c("SHEFFIELD", "ROTHERHAM", "BARNSLEY", "DONCASTER") ~ "South Yorkshire",
    Town %in% c("LEEDS", "BRADFORD", "WAKEFIELD", "HUDDERSFIELD", "HALIFAX") ~ "West Yorkshire"
  ))

# Plot crime rates
p <- ggplot(crime_rates, aes(x = reorder(Town, Crime_Rate_Per_1000), y = Crime_Rate_Per_1000, fill = Region)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(values = c("South Yorkshire" = "#66c2a5", "West Yorkshire" = "#fc8d62")) +
  labs(
    title = "Crime Rate by Town in Yorkshire (2023)",
    subtitle = "Crimes per 1,000 People (Lowest to Highest)",
    x = "Town",
    y = "Crimes per 1,000 People",
    caption = "South Yorkshire: Green, West Yorkshire: Orange"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40"),
    panel.grid.major.y = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Display in new window
dev.new()
print(p)

# Save the plot
ggsave("crime_rates_by_town.png", plot = p, width = 8, height = 6, dpi = 300)