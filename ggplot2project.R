# ==============================================================
# Project: City vs Highway MPG Visualization
#
# Description: Exploratory data visualization using ggplot2
# ==============================================================
# Load or install necessary libraries
# --------------------------------------------------------------
packages <- c("dplyr", "ggplot2", "ggthemes", "RColorBrewer", "readr", "tidyr", "lubridate")

installed <- rownames(installed.packages())
for (p in packages) {
  if (!(p %in% installed)) install.packages(p, repos = "https://cran.rstudio.com")
}
lapply(packages, library, character.only = TRUE)
rm(packages, installed)


# --------------------------------------------------------------
# 1. Load and Explore Data
# --------------------------------------------------------------
mpg <- read_csv("data/mpg.csv")   # or use data(mpg) for built-in dataset
summary(mpg)
head(mpg)


# --------------------------------------------------------------
# 2. Scatterplot: City vs Highway MPG
# --------------------------------------------------------------
plot_city_hwy <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_jitter(aes(color = factor(cyl)), width = 0.5, size = 1.5) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(
    title = "City vs Highway MPG",
    subtitle = "EPA data (1998–2008)",
    x = "City MPG", y = "Highway MPG", color = "Cylinders"
  ) +
  theme_few()
plot_city_hwy
ggsave("outputs/city_vs_highway.png", plot_city_hwy, width = 8, height = 6)


# --------------------------------------------------------------
# 3. Average City MPG by Manufacturer
# --------------------------------------------------------------
avg_mpg <- mpg %>%
  group_by(manufacturer) %>%
  summarise(avg_cty = mean(cty, na.rm = TRUE)) %>%
  arrange(desc(avg_cty))

avg <- mean(mpg$cty, na.rm = TRUE)

plot_bar <- avg_mpg %>%
  mutate(
    mpg_group = case_when(
      avg_cty > 20 ~ "High",
      avg_cty < 13 ~ "Low",
      TRUE ~ "Mid"
    )
  ) %>%
  ggplot(aes(x = reorder(manufacturer, avg_cty), y = avg_cty, fill = mpg_group)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = avg, color = "grey40", linetype = "dashed") +
  scale_fill_manual(values = c("High" = "deeppink4", "Mid" = "darkgrey", "Low" = "darkslateblue")) +
  labs(
    title = "Average City MPG by Manufacturer",
    x = "Manufacturer", y = "Average City MPG"
  ) +
  theme_few() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )
plot_bar
ggsave("outputs/avg_city_mpg_by_manufacturer.png", plot_bar, width = 8, height = 6)


# --------------------------------------------------------------
# 4. Wrap Up
# --------------------------------------------------------------
message(" Visualizations saved in the 'outputs/' folder.")
