#28/05/2025
country <- c("Angola", "Papua New Guinea", "Pakistan", "Chad", "Ethiopia", "Kenya", "Nigeria", "Liberia", "Burkina Faso", "India")
coverage <- c(-0.14, -0.35, 0.12, 0.14, 0.32, 0.07, 0.2, 0.18, 0.3, 0.35)
inequality <- c(0.1, 0.08, 0.05, 0.01, 0.005, -0.06, -0.07, -0.11, -0.13, -0.16)
df_plot <- data.frame(country, coverage, inequality)
df_plot

library(tidyverse)

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "blue") +
  scale_x_continuous("Weight (1000 lbs)") +
  scale_y_continuous("Miles Per Gallon") +
  ggtitle("MPG vs. Weight") + 
  theme_bw()

ggplot(df_plot,aes(x = coverage,y = inequality)) + 
  geom_point(aes(size = size, color = color)) +
  geom_text(label=country, hjust = -0.2, vjust = 0.3) +
  xlim(c(-0.35, 0.55)) +
  ylim(c(-0.17, 0.1)) +
  theme_bw() +
  scale_color_manual(values = c("blue","purple","yellow"),
                     labels = c("blue","purple","yellow"))

df_plot$size <- c(1.1, 0.6, 3, 1, 2, 1.1, 3, 0.7, 1.1, 8)
df_plot$color <- c("purple", "blue", "yellow", "purple", "purple", "purple", "purple", "purple", "purple", "yellow")
df_plot

set.seed(123)
np <- 50
rd <- data.frame(
  country = rep("", np),
  coverage = rnorm(n = np, mean = 0.1, sd = 0.12),
  inequality = rnorm(n = np, mean = -0.05, sd = 0.04),
  size = rnorm(n = np, mean = 1, sd = 0.4),
  color = sample(
    c("red", "green", "darkblue", "yellow", "blue", "purple"),
    np,
    replace = T
  )
)
head(rd)

df_plot <- rbind(df_plot, rd)
ggplot(df_plot, aes(x = coverage, y = inequality)) +
  geom_point(aes(size = size, color = color)) +
  geom_text(aes(label = country), hjust = -0.2, vjust = 0.2) +
  xlim(c(-0.35, 0.55)) +
  ylim(c(-0.17, 0.1))