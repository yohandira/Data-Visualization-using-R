# Set desktop as working directory
setwd("~/Desktop")

# Load "The Top Six Product Categories on Bukalapak.csv" file
product_category_gender <- read.csv(
  file = "The Top Six Product Categories on Bukalapak.csv",
  sep = ","
)

product_category_gender

str(product_category_gender)

# Import libraries
library(dplyr)

# Convert "male" column into negative and sort the values from the higher to lower (descending)
product_category_gender <- product_category_gender %>%
  mutate(male = -male) %>%
  arrange(desc(male))

product_category_gender

# Convert "product_category" column into factor and order the levels based on the lower to higher values on "male" column
product_category_gender$product_category <- factor(
  x = product_category_gender$product_category,
  levels = c("Lifestyle and Hobbies",
             "Handphone and Electronics",
             "Household Appliances",
             "Vehicle and Accessories",
             "Fashion",
             "Body Care")
)

attributes(product_category_gender$product_category)

product_category_gender

# Import libraries
library(ggplot2)
library(hrbrthemes)

product_category_gender_plot <- product_category_gender %>%
  ggplot(mapping = aes(x = product_category)) +
  # Male chart
  geom_bar(
    mapping = aes(y = male),
    stat = "identity",
    width = 0.75,
    fill = "#FE346E",
    alpha = 0.87
  ) +
  geom_text(
    mapping = aes(x = product_category,
                  y = male + 1.75,
                  label = abs(male)),
    colour = "#FFFFFF"
  ) +
  # Female chart
  geom_bar(
    mapping = aes(y = female),
    stat = "identity",
    width = 0.75,
    fill = "royalblue", 
    alpha = 0.87
  ) +
  geom_text(
    mapping = aes(x = product_category,
                  y = female - 1.75,
                  label = female),
    colour = "#FFFFFF"
  ) +
  # Adjust y axis limit
  ylim(-50, 70.8) +
  # Rotate bar chart direction
  coord_flip() +
  # Male chart annotation
  annotate(
    geom = "text",
    x = 0.5,
    y = -3.85,
    label = "Male",
    colour = "#000000"
  ) +
  # Female chart annotation
  annotate(
    geom = "text",
    x = 0.5,
    y = 5,
    label = "Female",
    colour = "#000000"
  ) +
  # Add title and caption
  labs(
    x = NULL,
    y = NULL,
    title = "The Top Six Product Categories on Bukalapak",
    subtitle = "What do male and female customers buy?",
    caption = "Source: Marketing Mix Factors Influencing Purchase Decisions on Bukalapak, IPB University Scientific Repository"
  ) +
  # Set themes
  theme_ipsum(grid = FALSE, ticks = FALSE) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14, colour = "#000000"),
    axis.title = element_blank(),
    plot.title = element_text(size = 26,colour = "#000000"),
    plot.subtitle = element_text(size = 18, colour = "#000000"),
    plot.caption = element_text(size = 10, colour = "#000000")
  )

product_category_gender_plot
