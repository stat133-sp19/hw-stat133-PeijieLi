# title
# description
# input
# output
library(gtools)
library(dplyr)
library(ggplot2)
library(grid)
library(jpeg)

# 4.1 Shot Charts for each player
df <- read.csv("../data/shots-data.csv", stringsAsFactors = FALSE)
court_file <- "../images/nba-court.jpg"
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))

# a. Klay Thompson
klay <- filter(df, name == "Klay Thompson")
klay_shot_chart <- ggplot(data=klay) + 
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) + 
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()

pdf("../images/klay-thompson-shot-chart.pdf", width=6.5, height=5) 
klay_shot_chart
dev.off()

# b. Graymond Green
green <- filter(df, name == "Draymond Green")
green_shot_chart <- ggplot(data=green) + 
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) + 
  ggtitle('Shot Chart: Draymond Greenn (2016 season)') +
  theme_minimal()
pdf("../images/draymond-green-shot-chart.pdf", width=6.5, height=5) 
green_shot_chart
dev.off()


# c. Andre Iguodala
andre <- filter(df, name == "Andre Iguodala")
andre_shot_chart <- ggplot(data=andre) + 
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) + 
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()
pdf("../images/andre-iguodala-shot-chart.pdf", width=6.5, height=5) 
andre_shot_chart
dev.off()

# d. Kevin Durant
kevin <- filter(df, name == "Kevin Durant")
kevin_shot_chart <- ggplot(data=kevin) + 
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) + 
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()
pdf("../images/kevin-durant-shot-chart.pdf", width=6.5, height=5) 
kevin_shot_chart
dev.off()

# e. Stephen Curry
curry <- filter(df, name == "Stephen Curry")
curry_shot_chart <- ggplot(data=curry) + 
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) + 
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()
pdf("../images/stephen-curry-shot-chart.pdf", width=6.5, height=5) 
curry_shot_chart
dev.off()

# 4.2
shot_charts <- ggplot(data=df) + 
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) + 
  ggtitle('Shot Charts: GSW (2016 season)') +
  theme_minimal() + 
  facet_wrap(~name)

# Save as PDF
pdf("../images/gsw-shot-charts.pdf", width=8, height=7) 
shot_charts
dev.off()

# Save as PNG
png("../images/gsw-shot-charts.png", width=8*96, height=7*96) 
shot_charts
dev.off()
