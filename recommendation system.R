library(dplyr)
library(ggplot2)
library(readr)

base_dir <- "~/Desktop/Housing Data"

# ---- 1. Load datasets ----
broadband <- read_csv(file.path(base_dir, "cleaned_broadband.csv"))
crime <- read_csv(file.path(base_dir, "cleaned_crime.csv"))
housing <- read_csv(file.path(base_dir, "cleaned_housing.csv"))

# ---- 2. Broadband Speed Aggregation ----
broadband_speed <- broadband %>%
  select(
    Town = laua_name,
    speed_30_300 = `% of premises with 30<300mbit/s download speed`,
    speed_300plus = `% of premises with >=300mbit/s download speed`
  ) %>%
  mutate(
    EstimatedSpeed = (speed_30_300 * 150 + speed_300plus * 500) / 100
  ) %>%
  select(Town, EstimatedSpeed)

# ---- 3. Crime Rate Aggregation (2023 only) ----
crime_rate <- crime %>%
  filter(format(Month, "%Y") == "2023") %>%
  group_by(Town) %>%
  summarise(CrimeCount = n(), .groups = 'drop')

# ---- 4. Housing Price Aggregation ----
housing_price <- housing %>%
  group_by(Town) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE), .groups = 'drop')

# ---- 5. Merge All ----
merged <- broadband_speed %>%
  inner_join(crime_rate, by = "Town") %>%
  inner_join(housing_price, by = "Town")

# ---- 6. Normalize & Composite Score ----
merged <- merged %>%
  mutate(
    NormSpeed = (EstimatedSpeed - min(EstimatedSpeed)) / (max(EstimatedSpeed) - min(EstimatedSpeed)),
    NormCrime = 1 - (CrimeCount - min(CrimeCount)) / (max(CrimeCount) - min(CrimeCount)),  # Inverted for lower crime
    NormPrice = 1 - (AvgPrice - min(AvgPrice)) / (max(AvgPrice) - min(AvgPrice))           # Inverted for cheaper housing
  ) %>%
  mutate(
    CompositeScore = round((NormSpeed + NormCrime + NormPrice) / 3, 3)
  ) %>%
  arrange(desc(CompositeScore))

# ---- 7. Plot ----
ggplot(merged, aes(x = reorder(Town, CompositeScore), y = CompositeScore, fill = CompositeScore)) +
  geom_col(width = 0.7) +
  coord_flip() +
  geom_text(aes(label = CompositeScore), hjust = -0.2, size = 4) +
  labs(
    title = "Best Towns to Live In (Based on Broadband, Crime & Housing Price)",
    x = "Town",
    y = "Composite Score (0 = Worst, 1 = Best)"
  ) +
  theme_minimal()


colSums(is.na(merged))


str(merged)

summary(merged$EstimatedSpeed)
summary(merged$CrimeCount)
summary(merged$AvgPrice)

sum(is.nan(merged$EstimatedSpeed))
sum(is.infinite(merged$EstimatedSpeed))


nrow(housing)
nrow(crime)
nrow(broadband)
nrow(merged)

broadband <- broadband %>%
  rename(Town = laua_name)


housing$Town <- trimws(tolower(housing$Town))
crime$Town <- trimws(tolower(crime$Town))
broadband$Town <- trimws(tolower(broadband$Town))

common_towns <- Reduce(intersect, list(broadband$Town, crime$Town, housing$Town))
print(common_towns)


housing_filtered <- housing %>% filter(Town %in% common_towns)
crime_filtered <- crime %>% filter(Town %in% common_towns)
broadband_filtered <- broadband %>% filter(Town %in% common_towns)


merged <- broadband_filtered %>%
  inner_join(crime_filtered, by = "Town") %>%
  inner_join(housing_filtered, by = "Town")




