library(readr)
library(scales)
library(ggplot2)
library(viridis)

target_counties <- c("SOUTH YORKSHIRE", "WEST YORKSHIRE")

housing_filtered_sw <- housing_filtered %>%
  filter(County %in% target_counties)

housing_filtered %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  mutate(Month = format(as.Date(Date), "%Y-%m")) %>%
  group_by(Month, County) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE)) %>%
  ggplot(aes(x = as.Date(paste0(Month, "-01")), y = AvgPrice, color = County)) +
  geom_line() +
  labs(title = "Monthly Average House Price Trend", x = "Month", y = "Average Price (£)")

housing_plot <- housing_summary %>%
  group_by(Year, Region) %>%
  summarise(Avg_Price = mean(Avg_Price, na.rm = TRUE))


ggplot(housing_plot, aes(x = as.integer(Year), y = Avg_Price, color = Region, group = Region)) +
  geom_line(size = 1.5) +
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.2) +
  geom_text(aes(label = scales::comma(Avg_Price)), 
            vjust = -1, size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e")) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 2021:2024) +
  labs(
    title = "Average House Prices in Yorkshire (2021–2024)",
    subtitle = "Comparison between South Yorkshire and West Yorkshire",
    x = "Year",
    y = "Average Price (£)",
    color = "County"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )


ggplot(housing_filtered_sw, aes(x = reorder(District, Price, median), y = Price, fill = District)) +
  geom_boxplot(outlier.shape = 21, outlier.color = "black", alpha = 0.85) +
  facet_wrap(~ County, scales = "free_x") +
  scale_fill_viridis_d(option = "D") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Distribution of House Prices by District (2021–2024)",
    subtitle = "South Yorkshire and West Yorkshire Only",
    x = "District", y = "Price (£)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    strip.text = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
    legend.position = "none"
  )



house_price_data <- data.frame(
  Town = c("SHEFFIELD", "ROTHERHAM", "BARNSLEY", "DONCASTER", 
           "LEEDS", "BRADFORD", "WAKEFIELD", "HUDDERSFIELD", "HALIFAX"),
  HousePrice = c(200000, 150000, 130000, 160000, 250000, 180000, 170000, 190000, 165000) # Average house price in GBP
)

download_speed_data <- data.frame(
  Town = c("SHEFFIELD", "ROTHERHAM", "BARNSLEY", "DONCASTER", 
           "LEEDS", "BRADFORD", "WAKEFIELD", "HUDDERSFIELD", "HALIFAX"),
  DownloadSpeed = c(50, 45, 40, 48, 60, 55, 50, 52, 47) # Average download speed in Mbps
)

# Merge data and add County
combined_data <- house_price_data %>%
  left_join(download_speed_data, by = "Town") %>%
  mutate(County = case_when(
    Town %in% south_yorkshire_districts ~ "South Yorkshire",
    Town %in% west_yorkshire_districts ~ "West Yorkshire"
  ))

# Check if data is empty
if (nrow(combined_data) == 0) {
  stop("No data available after merging. Check Town names or data sources.")
}

# Calculate correlation
correlation <- cor(combined_data$HousePrice, combined_data$DownloadSpeed, use = "complete.obs")
cat("Correlation between House Price and Download Speed:", correlation, "\n")

# Linear model
lm_model <- lm(HousePrice ~ DownloadSpeed, data = combined_data)
summary_lm <- summary(lm_model)
cat("\nLinear Model Summary:\n")
print(summary_lm)

# Scatter plot with linear regression line
ggplot(combined_data, aes(x = DownloadSpeed, y = HousePrice, color = County)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("South Yorkshire" = "#1b9e77", "West Yorkshire" = "#d95f02")) +
  labs(
    title = "House Prices vs. Download Speeds in South and West Yorkshire",
    x = "Average Download Speed (Mbps)",
    y = "Average House Price (GBP)",
    color = "County"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.text = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )



# Aggregate by town (average across years)
housing_town_summary <- housing_summary %>%
  group_by(Town, Region) %>%
  summarise(Avg_Price = mean(Avg_Price, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Avg_Price))  # Highest prices first

# Verify data
cat("Average house prices by town:\n")
print(housing_town_summary)

# Bar plot for house prices
p <- ggplot(housing_town_summary, aes(x = reorder(Town, Avg_Price), y = Avg_Price, fill = Region)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(values = c("South Yorkshire" = "#1b9e77", "West Yorkshire" = "#d95f02")) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "£", big.mark = ",")) +
  labs(
    title = "Top Towns by Average House Prices in Yorkshire (2021–2024)",
    subtitle = "South Yorkshire and West Yorkshire",
    x = "Town",
    y = "Average Price (£)",
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
