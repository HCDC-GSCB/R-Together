library(ggplot2)

#plot 1
ggplot(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "blue", shape = 0) +
  scale_x_continuous("Weight (1000 lbs)") +
  scale_y_continuous("Miles Per Gallon") +
  ggtitle("MPG vs. Weight") +
  theme_bw()

#df_plot dataset
country <- c("Angola", "Papua New Guinea", "Pakistan", "Chad", "Ethiopia", "Kenya", "Nigeria", "Liberia", "Burkina Faso", "India")
coverage <- c(-0.14, -0.35, 0.12, 0.14, 0.32, 0.07, 0.2, 0.18, 0.3, 0.35)
inequality <- c(0.1, 0.08, 0.05, 0.01, 0.005, -0.06, -0.07, -0.11, -0.13, -0.16)
df_plot <- data.frame(country, coverage, inequality)

df_plot

ggplot(data = df_plot, aes(x = coverage, y = inequality)) +
  geom_point() + # add point
  geom_text(aes(label = country), hjust = -0.2, vjust = 0.2) +  # add text lable to each point and adjust the position of the label
  xlim(c(-0.35, 0.55)) + # set limit for x-axis
  ylim(c(-0.17, 0.1))    # set limit for y-axis
df_plot$size <- c(1.1, 0.6, 3, 1, 2, 1.1, 3, 0.7, 1.1, 8)
df_plot$color <- c("purple", "blue", "yellow", "purple", "purple", "purple", "purple", "purple", "purple", "yellow")
df_plot

ggplot(data = df_plot, aes(x = coverage, y = inequality)) +
  geom_point(aes(size = size, color = color)) +
  geom_text(aes(label = country), hjust = -0.2, vjust = 0.2) +
  scale_color_manual(values = c("blue","purple","yellow"),
                     labels = c("blue","purple","yellow")) +
  xlim(c(-0.35, 0.55)) +
  ylim(c(-0.17, 0.1))


# practice
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
  scale_color_manual(values = c("blue","darkblue","green","purple","red","yellow"),
                     labels = c("blue","darkblue","green","purple","red","yellow")) +
  xlim(c(-0.35, 0.55)) +
  ylim(c(-0.17, 0.1))

cols <- c(
  "red" = "#fa8495",
  "green" = "#4ca258",
  "darkblue" = "#6493bb",
  "yellow" = "#d7c968",
  "blue" = "#7dd8f3",
  "purple" = "#bc5c91"
)

ggplot(df_plot, aes(x = coverage, y = inequality)) +
  geom_point(aes(size = size, color = color), alpha = 0.8)+
  geom_text(aes(label = country), hjust = -0.2, vjust = 0.2) +
  geom_vline(xintercept = 0, color = "#999999") +
  geom_hline(yintercept = 0, color = "#999999") + 
  scale_x_continuous(breaks = c(-0.25, 0, 0.25, 0.5),
                     limits = c(-0.4, 0.55)) +
  scale_y_continuous(breaks = c(-0.1,0,0.1),
                     limits = c(-0.16, 0.1)) +
  scale_size_continuous(
    breaks = c(2,4,6,8),
    labels = c("50 million", "100 million", "150 million", "200 million"),
    range = c(0, 8),
    guide = guide_legend(order = 1)
  )
