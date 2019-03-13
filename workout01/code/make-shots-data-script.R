# title:
# description
# input
# output

library(gtools)
library(dplyr)
library(ggplot2)

# step 1: read in the five data sets
curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
iguodala <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)

# step 2: add a column name to each imported data frame, that contains the name of the corresponding player
curry <- mutate(curry, name = "Stephen Curry")
iguodala <- mutate(iguodala, name = "Andre Iguodala")
thompson <- mutate(thompson, name = "Klay Thompson")
green <- mutate(green, name = "Draymond Green")
durant <- mutate(durant, name = "Kevin Durant")

# step 3: change the original values of shot_made_flag to more descriptive values
update_shot_made_flag <- function(df) {
  return(mutate(df, shot_made_flag = ifelse(shot_made_flag == "n", "shot_no", "shot_yes")))
}
curry <- update_shot_made_flag(curry)
iguodala <- update_shot_made_flag(iguodala)
thompson <- update_shot_made_flag(thompson)
green <- update_shot_made_flag(green)
durant <- update_shot_made_flag(durant)

# step 4: add a column minute that contains the minute number where a shot occurred.
add_col_minute <- function(df) {
  return(mutate(df, minute = period * 12 - minutes_remaining))
}
curry <- add_col_minute(curry)
iguodala <- add_col_minute(iguodala)
thompson <- add_col_minute(thompson)
green <- add_col_minute(green)
durant <- add_col_minute(durant)

# step 5: Use sink() to sent the summary() output of each imported data frame into individuals text files
sink("../output/andre-iguodala-summary.txt")
summary(iguodala)
sink()
sink("../output/stephen-curry-summary.txt")
summary(curry)
sink()
sink("../output/klay-thompson-summary.txt")
summary(thompson)
sink()
sink("../output/draymond-green-summary.txt")
summary(green)
sink()
sink("../output/kevin-durant-summary.txt")
summary(durant)
sink()

# step 6: Use the row binding function rbind() to stack the tables into one single data frame
df <- curry
df <- rbind(df, iguodala)
df <- rbind(df, thompson)
df <- rbind(df, green)
df <- rbind(df, durant)

# step 7: Export the assembled table as a CSV file shots-data.csv
write.csv(df, "../data/shots-data.csv", row.names = F)

# step 8 Use sink() to send the summary() output of the assembled table.
sink("../output/shots-data-summary.txt")
summary(df)
sink()


