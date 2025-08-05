library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

# 1. Load and inspect data -----------------------------------------------
school_data <- read.csv("school dataset.csv", stringsAsFactors = FALSE)

# Show all available columns
cat("Available columns in your dataset:\n")
print(colnames(school_data))

potential_metric_cols <- grep("score|attain|grade|point|result|perform", 
                              names(school_data), 
                              ignore.case = TRUE, value = TRUE)

cat("\nPotential metric columns:\n")
print(potential_metric_cols)

if (length(potential_metric_cols) > 0) {
  cat("\nSample values from potential metrics:\n")
  print(head(school_data[, potential_metric_cols]))
} else {
  cat("\nNo obvious performance metric columns found. Here are all your columns:\n")
  print(names(school_data))
}

# 2. Identify alternative score columns -----------------------------------
# Common alternatives for attainment metrics:
possible_score_cols <- c("ATT8SCR", "ATT8", "P8", "P8SCORE", "KS4_AVERAGE", "AVERAGE")
available_score_cols <- possible_score_cols[possible_score_cols %in% colnames(school_data)]

if (length(available_score_cols) == 0) {
  stop("No recognized score columns found. Please check your dataset for academic performance metrics.")
} else {
  cat("\nUsing score column:", available_score_cols[1], "\n")
}

# 3. Identify year column ------------------------------------------------
possible_year_cols <- c("YEAR", "ACADEMIC_YEAR", "SCHOOLYEAR")
available_year_cols <- possible_year_cols[possible_year_cols %in% colnames(school_data)]

if (length(available_year_cols) == 0) {
  message("\nNo year column found - will assume all data is from same year")
  school_data$YEAR <- 2023  # Default year
} else {
  cat("\nUsing year column:", available_year_cols[1], "\n")
  school_data$YEAR <- school_data[[available_year_cols[1]]]
}

# 4. Clean and prepare data ----------------------------------------------
south_yorkshire_towns <- c("Sheffield", "Barnsley", "Rotherham", "Doncaster")
west_yorkshire_towns <- c("Leeds", "Bradford", "Wakefield", "Huddersfield", "Halifax")

school_clean <- school_data %>%
  mutate(
    TOWN = str_to_title(str_trim(TOWN)),
    SCORE = as.numeric(get(available_score_cols[1])),
    YEAR = as.numeric(str_extract(YEAR, "\\d{4}"))  # Extract year if in format "2022/23"
  ) %>%
  filter(
    TOWN %in% c(south_yorkshire_towns, west_yorkshire_towns),
    !is.na(SCORE)
  ) %>%
  mutate(
    County = case_when(
      TOWN %in% south_yorkshire_towns ~ "South Yorkshire",
      TOWN %in% west_yorkshire_towns ~ "West Yorkshire"
    )
  )

# 5. Create visualizations -----------------------------------------------

# A. Boxplot for South Yorkshire (most recent year)
latest_year <- max(school_clean$YEAR, na.rm = TRUE)

school_clean %>%
  filter(County == "South Yorkshire", YEAR == latest_year) %>%
  ggplot(aes(x = TOWN, y = SCORE)) +
  geom_boxplot(fill = "#66c2a5") +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = paste("Academic Performance - South Yorkshire", latest_year),
       x = "District",
       y = available_score_cols[1]) +
  theme_minimal()

# B. Boxplot for West Yorkshire (earliest year)
earliest_year <- min(school_clean$YEAR, na.rm = TRUE)

school_clean %>%
  filter(County == "West Yorkshire", YEAR == earliest_year) %>%
  ggplot(aes(x = TOWN, y = SCORE)) +
  geom_boxplot(fill = "#fc8d62") +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = paste("Academic Performance - West Yorkshire", earliest_year),
       x = "District",
       y = available_score_cols[1]) +
  theme_minimal()

# C. Line graph of trends
if (length(unique(school_clean$YEAR)) > 1) {
  school_clean %>%
    group_by(County, TOWN, YEAR) %>%
    summarise(Mean_Score = mean(SCORE, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = YEAR, y = Mean_Score, color = TOWN)) +
    geom_line() +
    geom_point() +
    facet_wrap(~County, ncol = 1) +
    labs(title = "Academic Performance Trends",
         y = available_score_cols[1]) +
    theme_minimal()
} else {
  message("\nCannot create trend line graph: Only one year of data available")
}

# Show the structure of your data
cat("\nData structure:\n")
str(school_data)

# Show the first few rows
cat("\nFirst 5 rows:\n")
head(school_data, 5)

# List all column names
cat("\nAll column names:\n")
names(school_data)


# Find potential numeric columns that could represent scores
numeric_cols <- school_data %>%
  select(where(is.numeric)) %>%
  names()

# Find columns with "score", "attain", "grade" etc in their names
metric_cols <- names(school_data)[
  grepl("score|attain|grade|point|result|perform|ks[24]", 
        names(school_data), ignore.case = TRUE)
]

# Find date/year columns
year_cols <- names(school_data)[
  grepl("year|date|academic", names(school_data), ignore.case = TRUE)
]

cat("Potential numeric columns:", head(numeric_cols, 10), "...\n")
cat("Potential metric columns:", metric_cols, "\n")
cat("Potential year columns:", year_cols, "\n")


# Select the most relevant score columns
score_columns <- c("CA_SCORE_TLEV", "CA_SCORE_TECHCERT", "CA_SCORE_18")

# Clean and prepare the data
school_analysis <- school_data %>%
  mutate(
    TOWN = str_to_title(str_trim(TOWN)),
    YEAR = as.numeric(YEAR),
    across(all_of(score_columns), as.numeric)
  ) %>%
  filter(
    TOWN %in% c(south_yorkshire_towns, west_yorkshire_towns),
    !is.na(YEAR),
    if_any(all_of(score_columns), ~!is.na(.))
  ) %>%
  mutate(
    County = case_when(
      TOWN %in% south_yorkshire_towns ~ "South Yorkshire",
      TOWN %in% west_yorkshire_towns ~ "West Yorkshire"
    )
  )



school_analysis %>%
  filter(YEAR == 2023) %>%
  ggplot(aes(x = TOWN, y = CA_SCORE_TLEV, fill = County)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.3) +
  scale_fill_manual(values = c("South Yorkshire" = "#66c2a5", 
                               "West Yorkshire" = "#fc8d62")) +
  labs(title = "Technical Level Scores by District (2023)",
       x = "District",
       y = "Technical Level Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# First, verify which score columns ACTUALLY exist in your data
available_scores <- intersect(
  c("CA_SCORE_TLEV", "CA_SCORE_TECHCERT", "CA_SCORE_18"),
  names(school_data)
)

cat("Available score columns to analyze:", available_scores, "\n")

# If no score columns are available, we'll need to adjust our approach
if(length(available_scores) == 0) {
  stop("No recognized score columns found in the dataset. Please check column names.")
}

# Now use only the columns that exist
school_analysis <- school_data %>%
  mutate(
    TOWN = str_to_title(str_trim(TOWN)),
    YEAR = as.numeric(YEAR),
    SCORE = as.numeric(!!sym(available_scores[1]))  # Use first available score
  ) %>%
  filter(
    TOWN %in% c(south_yorkshire_towns, west_yorkshire_towns),
    !is.na(YEAR),
    !is.na(SCORE)
  ) %>%
  mutate(
    County = case_when(
      TOWN %in% south_yorkshire_towns ~ "South Yorkshire",
      TOWN %in% west_yorkshire_towns ~ "West Yorkshire"
    )
  )

# Create visualization using the available score
ggplot(school_analysis, aes(x = TOWN, y = SCORE, fill = County)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.3) +
  scale_fill_manual(values = c("South Yorkshire" = "#66c2a5", 
                               "West Yorkshire" = "#fc8d62")) +
  labs(
    title = paste("Distribution of", available_scores[1], "Scores"),
    subtitle = "Higher values indicate better performance",
    x = "District",
    y = available_scores[1]
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



west_yorks_data <- school_data %>%
  mutate(
    TOWN = str_to_title(str_trim(TOWN)),
    SCORE = as.numeric(CA_SCORE_TLEV)  # Use your actual TLEV column name
  ) %>%
  filter(
    TOWN %in% c("Leeds", "Bradford", "Wakefield", "Huddersfield", "Halifax"),
    !is.na(SCORE)
  )

ggplot(west_yorks_data, aes(x = TOWN, y = SCORE, fill = TOWN)) +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "gray20") +
  scale_fill_brewer(palette = "Oranges") +  # Orange theme for West Yorkshire
  labs(
    title = "Technical Level (TLEV) Performance in West Yorkshire",
    subtitle = "Higher scores = better achievement (Distinction*/Distinction/Merit/Pass)",
    x = "District",
    y = "TLEV Score",
    caption = "Dots represent individual schools"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "none"
  )

# Filter South Yorkshire data
south_yorks_data <- combined_data %>% 
  filter(COUNTY == "South Yorkshire")

# Create the boxplot with jitter points
ggplot(south_yorks_data, aes(x = TOWN, y = SCORE, fill = TOWN)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +  # Hide default outliers
  geom_jitter(width = 0.2, alpha = 0.4, color = "gray20", size = 2) +
  scale_fill_brewer(palette = "Greens") +  # Green color palette
  labs(
    title = "Technical Level (TLEV) Performance in South Yorkshire",
    subtitle = "Districts: Sheffield, Barnsley, Rotherham, Doncaster",
    x = "District",
    y = "TLEV Score",
    caption = "Each dot represents one school's performance"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "none",  # Remove legend
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40")
  ) +
  ylim(min(south_yorks_data$SCORE, na.rm = TRUE) * 0.9,  # Add 10% padding
       max(south_yorks_data$SCORE, na.rm = TRUE) * 1.1)





# 1. Prepare the data - using TOWN instead of COUNTY/DISTRICT
trend_data <- school_data %>%
  mutate(
    # Create county designation
    COUNTY = case_when(
      TOWN %in% c("Sheffield", "Barnsley", "Rotherham", "Doncaster") ~ "South Yorkshire",
      TOWN %in% c("Leeds", "Bradford", "Wakefield", "Huddersfield", "Halifax") ~ "West Yorkshire"
    ),
    YEAR = as.numeric(YEAR),  # Ensure year is numeric
    SCORE = as.numeric(CA_SCORE_TLEV)  # Use your actual TLEV score column
  ) %>%
  filter(
    !is.na(COUNTY),          # Only keep our target counties
    !is.na(SCORE)            # Remove missing scores
  ) %>%
  group_by(COUNTY, TOWN, YEAR) %>%
  summarise(
    MEAN_SCORE = mean(SCORE, na.rm = TRUE),
    .groups = "drop"
  )
# 2. Create the line graph
ggplot(trend_data, aes(x = YEAR, y = MEAN_SCORE, 
                       color = TOWN, group = TOWN)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~COUNTY, ncol = 1, scales = "free_x") +  
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Technical Level (TLEV) Score Trends",
    subtitle = "Comparison Across Yorkshire Districts",
    x = "Academic Year",
    y = "Mean TLEV Score",
    color = "District",
    caption = "Higher scores indicate better performance"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = unique(trend_data$YEAR))








school_data <- read.csv("school dataset.csv", stringsAsFactors = FALSE)

# Define valid towns
south_yorkshire_towns <- c("Sheffield", "Barnsley", "Rotherham", "Doncaster")
west_yorkshire_towns <- c("Leeds", "Bradford", "Wakefield", "Huddersfield", "Halifax")
valid_towns <- c(south_yorkshire_towns, west_yorkshire_towns)

# Clean and prepare data
top_towns <- school_data %>%
  mutate(
    TOWN = str_to_title(str_trim(TOWN)),
    SCORE = suppressWarnings(as.numeric(CA_SCORE_TLEV))  # Suppress NA coercion warning
  ) %>%
  filter(
    TOWN %in% valid_towns,
    !is.na(SCORE)
  ) %>%
  group_by(TOWN) %>%
  summarise(Mean_Score = mean(SCORE, na.rm = TRUE)) %>%
  arrange(desc(Mean_Score)) %>%
  slice_head(n = 9) %>%
  mutate(
    County = case_when(
      TOWN %in% south_yorkshire_towns ~ "South Yorkshire",
      TOWN %in% west_yorkshire_towns ~ "West Yorkshire"
    )
  )

# Verify data
print(top_towns)

# Create bar plot
ggplot(top_towns, aes(x = reorder(TOWN, Mean_Score), y = Mean_Score, fill = County)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  coord_flip() +  # Horizontal bars for better label readability
  scale_fill_manual(values = c("South Yorkshire" = "#66c2a5", "West Yorkshire" = "#fc8d62")) +
  labs(
    title = "Top 9 Towns by Technical Level (TLEV) Score in Yorkshire",
    subtitle = "Higher scores indicate better performance",
    x = "Town",
    y = "Mean TLEV Score",
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
  scale_y_continuous(limits = c(-1, 1))  # Adjust for negative scores