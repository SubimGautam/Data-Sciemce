library(dplyr)
library(ggplot2)
library(patchwork)

# 1. First, let's CLEANLY recreate the county assignment
broadband_2024_speed <- broadband_2024_speed %>%
  mutate(
    # Clean town names first
    clean_town = trimws(tolower(laua_name)),
    
    # Assign counties with flexible matching
    COUNTY = case_when(
      grepl("sheffield|barnsley|rotherham|doncaster", clean_town) ~ "South Yorkshire",
      grepl("leeds|bradford|wakefield|huddersfield|halifax", clean_town) ~ "West Yorkshire",
      TRUE ~ "Other"
    )
  ) %>%
  # Remove any non-matching towns
  filter(COUNTY != "Other")

# 2. Verify we have data
cat("South Yorkshire rows:", nrow(filter(broadband_2024_speed, COUNTY == "SOUTH YORKSHIRE")), "\n")
cat("West Yorkshire rows:", nrow(filter(broadband_2024_speed, COUNTY == "WEST YORKSHIRE")), "\n")

# 3. Create the visualizations

# South Yorkshire Plot
p_south <- broadband_2024_speed %>%
  filter(COUNTY == "South Yorkshire") %>%
  ggplot(aes(x = reorder(laua_name, -EstimatedAvgSpeed), 
             y = EstimatedAvgSpeed,
             fill = laua_name)) +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_fill_brewer(palette = "Greens") +
  labs(title = "South Yorkshire Broadband Speeds (2024)",
       x = "District",
       y = "Download Speed (Mbps)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

# West Yorkshire Plot
p_west <- broadband_2024_speed %>%
  filter(COUNTY == "West Yorkshire") %>%
  ggplot(aes(x = reorder(laua_name, -EstimatedAvgSpeed), 
             y = EstimatedAvgSpeed,
             fill = laua_name)) +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_fill_brewer(palette = "Oranges") +
  labs(title = "West Yorkshire Broadband Speeds (2024)",
       x = "District",
       y = "Download Speed (Mbps)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Combine plots
p_south + p_west



# Show all unique town names in your dataset
town_names <- unique(broadband_2024_speed$laua_name)
print(town_names)

# Show structure of your data
str(broadband_2024_speed)








library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

# Define target towns, including Kirklees and Calderdale
target_towns <- c("Sheffield", "Rotherham", "Barnsley", "Doncaster", "Leeds", "Bradford", "Wakefield", "Kirklees", "Calderdale")

# Load data
bb_2024 <- read_csv("2024 broadband speed/202401_fixed_laua_coverage_r01.csv")

# Diagnostics: Check town names and speed columns
cat("Unique laua_name values matching target towns:\n")
print(unique(bb_2024$laua_name)[str_detect(tolower(unique(bb_2024$laua_name)), 
                                           "sheffield|rotherham|barnsley|doncaster|leeds|bradford|wakefield|kirklees|calderdale")])
cat("\nSpeed columns available:\n")
print(colnames(bb_2024)[grepl("Mbit/s", colnames(bb_2024))])

# Process data
broadband_2024_speed <- bb_2024 %>%
  mutate(
    laua_name = case_when(
      tolower(laua_name) == "kirklees" ~ "Huddersfield",
      tolower(laua_name) == "calderdale" ~ "Halifax",
      TRUE ~ str_to_title(str_trim(laua_name))
    )
  ) %>%
  filter(tolower(laua_name) %in% tolower(target_towns)) %>%
  mutate(
    EstimatedAvgSpeed = (
      (`% of premises with 0<2Mbit/s download speed` * 1.5) +
        (`% of premises with 2<5Mbit/s download speed` * 3.5) +
        (`% of premises with 5<10Mbit/s download speed` * 7.5) +
        (`% of premises with 10<30Mbit/s download speed` * 20) +
        (`% of premises with 30<300Mbit/s download speed` * 150) +
        (`% of premises with >=300Mbit/s download speed` * 500)
    ) / 100,
    County = case_when(
      tolower(laua_name) %in% tolower(c("Sheffield", "Barnsley", "Rotherham", "Doncaster")) ~ "South Yorkshire",
      tolower(laua_name) %in% tolower(c("Leeds", "Bradford", "Wakefield", "Huddersfield", "Halifax")) ~ "West Yorkshire"
    )
  ) %>%
  filter(!is.na(EstimatedAvgSpeed)) %>%
  group_by(laua_name, County) %>%
  summarise(Mean_Speed = mean(EstimatedAvgSpeed, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Mean_Speed)) %>%
  slice_head(n = 9)

# Verify data
cat("Top towns by broadband speed:\n")
print(broadband_2024_speed)

# Create and save plot
if (nrow(broadband_2024_speed) > 0) {
  n_towns <- nrow(broadband_2024_speed)
  p <- ggplot(broadband_2024_speed, aes(x = reorder(laua_name, Mean_Speed), y = Mean_Speed, fill = County)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    coord_flip() +
    scale_fill_manual(values = c("South Yorkshire" = "#66c2a5", "West Yorkshire" = "#fc8d62")) +
    labs(
      title = paste0("Top ", n_towns, " Towns by Broadband Speed in Yorkshire (2024)"),
      subtitle = "Average Download Speed (Mbps)",
      x = "Town",
      y = "Mean Download Speed (Mbps)",
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
  
  # Display the plot
  print(p)
  
  # Save the plot
  ggsave("top_9_towns_broadband_speed.png", plot = p, width = 8, height = 6, dpi = 300)
} else {
  message("No data available to plot. Check laua_name values or speed columns.")
}