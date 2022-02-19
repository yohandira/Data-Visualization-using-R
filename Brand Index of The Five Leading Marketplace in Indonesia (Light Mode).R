# Set desktop as working directory
setwd("~/Desktop")

# Create brand index data frame
brand_index_raw <- data.frame(
  year = (2015:2021),
  tokopedia = c(1.2, 12.1, 13.4, 18.5, 13.4, 15.8, 16.7),
  shopee = c(NA, NA, NA, 14.7, 15.6, 20.0, 41.8),
  bukalapak = c(0.7, 11.8, 6.8, 8.7, 12.7, 12.9, 9.5),
  lazada = c(NA, 19.9, 18.0, 31.8, 31.6, 31.9, 15.2),
  blibli = c(NA, NA, 6.0, 8.0, 6.6, 8.4, 8.1)
)

brand_index_raw

# Import libraries
library(imputeTS)

# Fill in missing values with linear interpolation
for (i in c(3, 5, 6)) {
  brand_index_raw[ , i] <- na_interpolation(brand_index_raw[ , i])
}

brand_index_raw

# Create the avarage brand index data frame
brand_index_avg <- colMeans(x = brand_index_raw[2:6])
brand_index_avg <- round(x = brand_index_avg, digits = 3)
brand_index_avarage <- sort(brand_index_avg, decreasing = TRUE)

brand_index_avarage

# Import libraries
library(tidyr)

# Transform "tokopedia", "shopee", and "bukalapak" to rows
brand_index <- pivot_longer(
  data = brand_index_raw,
  cols = -year,
  names_to = "marketplace",
  values_to = "tbi"
)

brand_index

# Convert "year" and marketplace" columns to factor
brand_index$year <- as.factor(x = brand_index$year)
brand_index$marketplace <- as.factor(x = brand_index$marketplace)

brand_index

# Import libraries
library(ggplot2)
library(scales)
library(hrbrthemes)

# Visualize "brand_index"
brand_index_plot <-
  ggplot(
    data = brand_index,
    mapping = aes(x = year, y = tbi, group = marketplace)
  ) +
  geom_line(
    mapping = aes(color = marketplace, size = marketplace, alpha = marketplace),
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("lazada" = "#7209B7", 
               "shopee" = "#FF9F1C",
               "tokopedia" = "#2EC4B6",
               "bukalapak" = "#EE4266",
               "blibli" = "#4361EE")
  ) +
  scale_size_manual(
    values = c("lazada" = 2.3,
               "shopee" = 2.3,
               "tokopedia" = 2.3,
               "bukalapak" = 2.3,
               "blibli" = 2.3)
  ) +
  scale_alpha_manual(
    values = c("lazada" = 0.8,
               "shopee" = 0.8,
               "tokopedia" = 0.8,
               "bukalapak" = 0.8,
               "blibli" = 0.8)
  ) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40)) +
  coord_cartesian(clip = "off") +
  # Shopee
  annotate(
    geom = "segment",
    x = 7, y = brand_index$tbi[32],
    xend = 7.2, yend = brand_index$tbi[32] + 0.5,
    color = "#ff9f1c",
    size = 0.5
  ) +
  annotate(
    geom = "text",
    x = 7.23, y = brand_index$tbi[32] + 0.9,
    hjust = 0,
    color = "#ff9f1c",
    fontface = "bold",
    label = "Shopee",
  ) +
  annotate(
    geom = "text",
    x = 7.23, y = brand_index$tbi[32] - 0.2,
    hjust = 0,
    color = "#ff9f1c",
    size = 2.5,
    fontface = "bold",
    label = paste("avg:", brand_index_avg[2])
  ) +
  # Lazada
  annotate(
    geom = "segment",
    x = 7, y = brand_index$tbi[34],
    xend = 7.2, yend = brand_index$tbi[34] + 2.8,
    color = "#7209B7",
    size = 0.5
  ) +
  annotate(
    geom = "text",
    x = 7.23, y = brand_index$tbi[34] + 3.4,
    hjust = 0,
    color = "#7209B7",
    fontface = "bold",
    label = "Lazada"
  ) +
  annotate(
    geom = "text",
    x = 7.23, y = brand_index$tbi[34] + 2.3,
    hjust = 0,
    color = "#7209B7",
    size = 2.5,
    fontface = "bold",
    label = paste("avg:", brand_index_avg[4])
  ) +
  # Tokopedia
  annotate(
    geom = "segment",
    x = 7, y = brand_index$tbi[31],
    xend = 7.2, yend = brand_index$tbi[31] - 2.8,
    color = "#2EC4B6",
    size = 0.5
  ) +
  annotate(
    geom = "text",
    x = 7.23, y = brand_index$tbi[34] - 1,
    hjust = 0,
    color = "#2EC4B6",
    fontface = "bold",
    label = "Tokopedia"
  ) +
  annotate(
    geom = "text",
    x = 7.23, y = brand_index$tbi[34] - 2.1,
    hjust = 0,
    color = "#2EC4B6",
    size = 2.5,
    fontface = "bold",
    label = paste("avg:", brand_index_avg[1])
  ) +
  # Bukalapak
  annotate(
    geom = "segment",
    x = 7, y = brand_index$tbi[33],
    xend = 7.2, yend = brand_index$tbi[33] - 0.4,
    color = "#EE4266",
    size = 0.5
  ) +
  annotate(
    geom = "text",
    x = 7.23, y = brand_index$tbi[33],
    hjust = 0,
    color = "#EE4266",
    fontface = "bold",
    label = "Bukalapak"
  ) +
  annotate(
    geom = "text",
    x = 7.23, y = brand_index$tbi[33] - 1.2,
    hjust = 0,
    color = "#EE4266",
    size = 2.5,
    fontface = "bold",
    label = paste("avg:", brand_index_avg[3])
  ) +
  # Blibli
  annotate(
    geom = "segment",
    x = 7, y = brand_index$tbi[35],
    xend = 7.2, yend = brand_index$tbi[35] - 4.2,
    color = "#4361EE",
    size = 0.5
  ) +
  annotate(
    geom = "text",
    x = 7.23, y = brand_index$tbi[35] - 4,
    hjust = 0,
    color = "#4361EE",
    fontface = "bold",
    label = "Blibli"
  ) +
  annotate(
    geom = "text",
    x = 7.23, y = brand_index$tbi[35] - 5,
    hjust = 0,
    color = "#4361EE",
    size = 3,
    fontface = "bold",
    label = paste("avg:", brand_index_avg[5])
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Brand Index of The Five Leading Marketplaces in Indonesia",
    subtitle = "Based on online shopping site category, in percent",
    caption = "Source: Top Brand Index Phase 2 2015-2021, Top Brand Award"
  ) +
  theme_ipsum() +
  theme(
    axis.text = element_text(color = "#000000", size = 14),
    plot.title = element_text(color = "#000000", size = 24, vjust = 6),
    plot.subtitle = element_text(color = "#000000", size = 16, vjust = 10),
    plot.caption = element_text(color = "#000000", size = 10, vjust = -9),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#073B4C", size = 0.2, linetype = "dotted"),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(t = 50, r = 70, b = 70, l = 50, unit = "pt"),
    plot.background = element_rect(fill = "#FFFFFF")
  )
  
brand_index_plot
