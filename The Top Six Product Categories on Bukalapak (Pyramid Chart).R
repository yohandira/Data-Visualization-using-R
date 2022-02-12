# Visualize the proportion of respondents based on product categories grouped by gender
category_gender <- category_gender %>%
  mutate(male = -male) %>%
  arrange(desc(male))

print(category_gender)

category_gender_plot <- category_gender %>%
  ggplot(aes(x = factor(product_category,
                        levels = c("Lifestyle and Hobbies", "Handphone and Electronics", "Household Appliances",
                                   "Vehicle and Accessories", "Fashion", "Body Care")))) +
  # Male chart
  geom_bar(aes(y = male),
           stat = "identity",
           width = 0.75,
           fill = "#FE346E") +
  geom_text(aes(x = product_category,
                y = male + 1.75,
                label = abs(male)),
            colour = "#FFFFFF") +
  # Female chart
  geom_bar(aes(y = female),
           stat = "identity",
           width = 0.75,
           fill = "#5402DC") +
  geom_text(aes(x = product_category,
                y = female - 1.75,
                label = female),
            colour = "#FFFFFF") +
  # Adjust y axis limit
  ylim(-50, 70.8) +
  # Rotate bar chart direction
  coord_flip() +
  # Male chart annotation
  annotate(geom = "text",
           x = 0.5,
           y = -3.85,
           label = "Male",
           colour = "#000000") +
  # Female chart annotation
  annotate(geom = "text",
           x = 0.5,
           y = 5,
           label = "Female",
           colour = "#000000") +
  # Add title and caption
  labs(x = NULL,
       y = NULL,
       title = "The Top Six Product Categories on Bukalapak",
       subtitle = "What do male and female customers buy?",
       caption = "Source: Yohandira. (2021). Marketing Mix Factors Influencing Purchase Decisions on Bukalapak. IPB University Scientific Repository. http://repository.ipb.ac.id/handle/123456789/106826") +
  # Set themes
  theme_ipsum(grid = FALSE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14, colour = "#000000"),
        axis.title = element_blank(),
        plot.title = element_text(size = 26, colour = "#000000"),
        plot.subtitle = element_text(size = 16, colour = "#000000"),
        plot.caption = element_text(size = 10, colour = "#000000"))

show(category_gender_plot)
