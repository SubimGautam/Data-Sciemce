library(dplyr)
library(ggplot2)
library(broom)
library(patchwork)



# Extract and average TLEV scores by town
tlev_scores <- school_data %>%
  mutate(TOWN = str_to_upper(trimws(TOWN))) %>%  # Ensure consistent casing
  filter(!is.na(CA_SCORE_TLEV)) %>%
  group_by(TOWN) %>%
  summarise(
    TLEV_Score = mean(CA_SCORE_TLEV, na.rm = TRUE),
    .groups = "drop"
  )

# Verify extracted data
print(tlev_scores)




combined_data <- broadband_2024_speed %>%
  # Join with house prices
  left_join(house_prices, by = "laua_name") %>%
  # Join with TLEV scores
  left_join(tlev_scores, by = "laua_name") %>% 
  # Join with drug offense data
  left_join(drug_offenses, by = "laua_name") %>%
  # Create county designation
  mutate(
    COUNTY = case_when(
      laua_name %in% c("SHEFFIELD", "ROTHERHAM", "BARNSLEY", "DONCASTER") ~ "South Yorkshire",
      laua_name %in% c("LEEDS", "BRADFORD", "WAKEFIELD", "HUDDERSFIELD", "HALIFAX") ~ "West Yorkshire"
    )
  ) %>%
  # Remove any rows with missing key data
broadband_2024_speed <- read_csv("cleaned_broadband.csv")
house_prices <- read_csv("cleaned_housing.csv") 
school_data <- read_csv("cleaned_school.csv")
drug_offenses <- read_csv("cleaned_crime.csv")


tlev_scores <- school_data %>%
  select(laua_name = TOWN, TLEV_Score = CA_SCORE_TLEV) %>%
  group_by(laua_name) %>%
  summarise(TLEV_Score = mean(TLEV_Score, na.rm = TRUE))


  filter(
    !is.na(COUNTY),
    !is.na(Median_House_Price),
    !is.na(EstimatedAvgSpeed),
    !is.na(TLEV_Score),
    !is.na(Drug_Offenses_per_100k)
  )

# Verify the merged data
glimpse(combined_data)



# Custom color palette
county_colors <- c("South Yorkshire" = "#66c2a5", 
                   "West Yorkshire" = "#fc8d62")

# Function to create plots with correlations and regression lines
create_corr_plot <- function(data, x_var, y_var, x_label, y_label, title) {
  
  # Calculate correlations by county
  corr_stats <- data %>%
    group_by(COUNTY) %>%
    summarize(
      cor = cor(!!sym(x_var), !!sym(y_var)),
      p_value = cor.test(!!sym(x_var), !!sym(y_var))$p.value,
      .groups = "drop"
    ) %>%
    mutate(
      label = paste0("r = ", round(cor, 2), 
                     "\np = ", ifelse(p_value < 0.001, "<0.001", round(p_value, 3)))
    )
  
  # Generate linear model summaries
  lm_results <- data %>%
    group_by(COUNTY) %>%
    do(tidy(lm(paste(y_var, "~", x_var), data = .))) %>%
    filter(term != "(Intercept)") %>%
    select(COUNTY, estimate, p.value) %>%
    mutate(
      lm_label = paste0("β = ", round(estimate, 2),
                        "\np = ", ifelse(p.value < 0.001, "<0.001", round(p.value, 3)))
    )
  
  # Create plot
  ggplot(data, aes_string(x = x_var, y = y_var, color = "COUNTY")) +
    geom_point(size = 3, alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_text(data = corr_stats,
              aes(x = min(data[[x_var]]), 
                  y = max(data[[y_var]]), 
                  label = label),
              hjust = 0, vjust = 1, color = "black", size = 3.5) +
    geom_text(data = lm_results,
              aes(x = min(data[[x_var]]), 
                  y = max(data[[y_var]]) * 0.9, 
                  label = lm_label),
              hjust = 0, vjust = 1, color = "black", size = 3.5) +
    scale_color_manual(values = county_colors) +
    labs(title = title,
         x = x_label,
         y = y_label) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# 1. House Price vs Download Speed
p1 <- create_corr_plot(combined_data,
                       "Median_House_Price", "EstimatedAvgSpeed",
                       "Median House Price (£)", "Download Speed (Mbps)",
                       "House Price vs Broadband Speed")

# 2. TLEV Scores vs House Price
p2 <- create_corr_plot(combined_data,
                       "Median_House_Price", "TLEV_Score",
                       "Median House Price (£)", "TLEV Score",
                       "House Price vs Technical Education")

# 3. TLEV Scores vs Drug Offense Rates
p3 <- create_corr_plot(combined_data,
                       "Drug_Offenses_per_100k", "TLEV_Score",
                       "Drug Offenses (per 100k)", "TLEV Score",
                       "Community Safety vs Technical Education")

# 4. Download Speed vs TLEV Scores
p4 <- create_corr_plot(combined_data,
                       "EstimatedAvgSpeed", "TLEV_Score",
                       "Download Speed (Mbps)", "TLEV Score",
                       "Digital Infrastructure vs Technical Education")

# 5. Download Speed vs Drug Offense Rates
p5 <- create_corr_plot(combined_data,
                       "EstimatedAvgSpeed", "Drug_Offenses_per_100k",
                       "Download Speed (Mbps)", "Drug Offenses (per 100k)",
                       "Digital Infrastructure vs Community Safety")

# Combine all plots
combined_plots <- (p1 + p2) / (p3 + p4) / (p5 + plot_spacer()) +
  plot_annotation(title = "Yorkshire Regional Indicators Analysis",
                  subtitle = "Technical Level (TLEV) Performance Relationships",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))

# Display the combined plot
print(combined_plots)

# Save to file
ggsave("yorkshire_analysis.png", combined_plots, width = 12, height = 14, dpi = 300)