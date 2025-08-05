library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(readr)

# After loading your data, add this diagnostic check:
postcode_lsoa <- read_csv("~/Desktop/Housing Data/cleaned_postcode_lsoa.csv")
cat("\nSample of population data before joining:\n")
print(head(lsoa_population))

# Revised drug offenses calculation with proper population handling
drug_offenses <- crime_data_clean %>%
  filter(`Crime type` == "Drugs") %>%
  mutate(LSOA_code = toupper(trimws(`LSOA code`))) %>%
  left_join(lsoa_population, by = c("LSOA_code" = "Postcode")) %>%
  # Add verification of population values
  mutate(Population_Status = ifelse(is.na(Population) | Population == 0, "Missing", "Valid")) %>%
  group_by(County, Year, Population_Status) %>%
  tally() %>%
  pivot_wider(names_from = Population_Status, values_from = n, values_fill = 0)

cat("\nPopulation data status by county/year:\n")
print(drug_offenses)

# Alternative approach using known county populations
county_populations <- data.frame(
  County = c("South Yorkshire", "West Yorkshire"),
  Population = c(1405000, 2300000)  # Approximate 2021 estimates
)

drug_offenses_corrected <- crime_data_clean %>%
  filter(`Crime type` == "Drugs") %>%
  group_by(County, Year) %>%
  summarise(Total_Offenses = n(), .groups = "drop") %>%
  left_join(county_populations, by = "County") %>%
  mutate(Drug_Rate_Per_1000 = (Total_Offenses / Population) * 1000)

cat("\nCorrected drug offenses summary:\n")
print(drug_offenses_corrected)



# 2. Boxplot for Drug Offense Rate by District in West Yorkshire
west_yorks_drug <- crime_data_clean %>%
  filter(`Crime type` == "Drugs", County == "West Yorkshire") %>%
  group_by(Town, Month) %>%
  summarise(Drug_Offenses = n(), .groups = "drop")

ggplot(west_yorks_drug, aes(x = Town, y = Drug_Offenses)) +
  geom_boxplot(fill = "#fc8d62") +
  theme_minimal() +
  labs(title = "Drug Offense Rates by District in West Yorkshire",
       x = "District",
       y = "Monthly Drug Offenses") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





south_yorks_drug <- crime_data_clean %>%
  filter(`Crime type` == "Drugs", 
         County == "South Yorkshire") %>%
  group_by(Town, Month) %>%
  summarise(Drug_Offenses = n(), .groups = "drop")

# Print data summary
cat("Data points per town:\n")
print(table(south_yorks_drug$Town))

cat("\nSummary statistics:\n")
print(south_yorks_drug %>% 
        group_by(Town) %>% 
        summarise(Min = min(Drug_Offenses),
                  Median = median(Drug_Offenses),
                  Max = max(Drug_Offenses),
                  Months = n()))

# 2. Enhanced boxplot with jittered points
ggplot(south_yorks_drug, aes(x = Town, y = Drug_Offenses)) +
  # Boxplot with thicker lines and fill
  geom_boxplot(fill = "#66c2a5", 
               color = "#1f78b4", 
               linewidth = 0.8,
               outlier.shape = NA) + # Hide default outliers
  
  # Add jittered points with transparency
  geom_jitter(width = 0.2, 
              height = 0, 
              alpha = 0.6, 
              color = "#e41a1c", 
              size = 3) +
  
  # Add mean points
  stat_summary(fun = mean, 
               geom = "point", 
               shape = 18, 
               size = 4, 
               color = "black") +
  
  # Improved labels and theme
  labs(title = "Monthly Drug Offenses by District in South Yorkshire",
       subtitle = "Black diamonds show mean values | Points represent individual months",
       x = "District",
       y = "Number of Drug Offenses") +
  
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray50"))

# 3. Alternative: Violin plot if boxplot remains flat
ggplot(south_yorks_drug, aes(x = Town, y = Drug_Offenses)) +
  geom_violin(fill = "#66c2a5", trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.5, color = "#e41a1c") +
  stat_summary(fun = median, geom = "point", size = 3, color = "blue") +
  labs(title = "Distribution of Monthly Drug Offenses",
       subtitle = "Violin shows density | Blue points show medians") +
  theme_minimal()



# Plot with corrected data
ggplot(drug_offenses_corrected, aes(x = Year, y = Drug_Rate_Per_1000, 
                                    color = County, group = County)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("South Yorkshire" = "#E41A1C", 
                                "West Yorkshire" = "#377EB8")) +
  labs(title = "Drug Offense Rates per 1,000 People",
       x = "Year", 
       y = "Offenses per 1,000 people") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))

ggsave(file.path(base_dir, "drug_offenses_corrected.png"), 
       width = 10, height = 6)









library(dplyr)
library(readr)
library(ggplot2)

# Load cleaned crime data
crime_data <- read_csv("~/Desktop/Housing Data/cleaned_crime.csv")

# Load population data (2011 census, cleaned)
population_data <- read_csv("~/Desktop/Housing Data/cleaned_population_2011.csv") %>%
  mutate(Population = as.numeric(gsub(",", "", Population)))  # Ensure numeric population

# Load postcode-LSOA mapping (if needed for granular analysis)
postcode_lsoa <- read_csv("~/Desktop/Housing Data/cleaned_postcode_lsoa.csv")

crime_by_town <- crime_data %>%
  group_by(Town) %>%
  summarise(Total_Crimes = n()) %>%
  filter(Town != "OTHER")  # Remove "OTHER" if present

crime_with_population <- crime_by_town %>%
  left_join(population_data, by = c("Town" = "Postcode")) 



crime_rates <- crime_with_population %>%
  mutate(
    Crime_Rate_Per_1000 = (Total_Crimes / Population) * 1000
  ) %>%
  arrange(Crime_Rate_Per_1000)



ggplot(crime_rates, aes(x = reorder(Town, Crime_Rate_Per_1000), y = Crime_Rate_Per_1000)) +
  geom_bar(stat = "identity", fill = "#66c2a5") +
  coord_flip() +  # Horizontal bars for readability
  labs(
    title = "Crime Rate by Town (Lowest to Highest)",
    x = "Town",
    y = "Crimes per 1,000 people"
  ) +
  theme_minimal()



head(population_data)