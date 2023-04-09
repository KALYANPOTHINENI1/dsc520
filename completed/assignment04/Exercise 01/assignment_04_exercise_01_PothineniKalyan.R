# Assignment: ASSIGNMENT 4 Exercise -1
# Name: Pothineni, Kalyan
# Date: 2023-04-04

## Load the ggplot2 package
library(ggplot2)
library(gridExtra)

## Load the dataset from the CSV file
scores_df <- read.csv("data/scores.csv")

## Subset the dataset by section type
sports_section <- subset(scores_df, Section == "Sports")
regular_section <- subset(scores_df, Section == "Regular")

## Create a plot of the scores and number of students for sports section
sports_plot <- ggplot(sports_section, aes(x = Score, y = Count)) + 
  geom_bar(stat = "identity", fill = "blue") +
  ggtitle("Sports Section Scores") +
  xlab("Score") + ylab("Number of Students")

## Display the sports plot
sports_plot

## Create a plot of the scores and number of students for regular section
regular_plot <- ggplot(regular_section, aes(x = Score, y = Count)) + 
  geom_bar(stat = "identity", fill = "gray") +
  ggtitle("Regular Section Scores") +
  xlab("Score") + ylab("Number of Students")

## Display the mixed plot
regular_plot

##Assignment 4(1)
## mean and standard deviation of scores for regular section
mean_regular <- mean(regular_section$Score)
sd_regular <- sd(regular_section$Score)
median_regular <- median(regular_section$Score)
##Print the means, sd and median
print(mean_regular)
print(sd_regular)
print(median_regular)


## mean and standard deviation of scores for sports section
mean_sports <- mean(sports_section$Score)
sd_sports <- sd(sports_section$Score)
median_sports <- median(sports_section$Score)
##Print the means, sd and median
print(mean_sports)
print(sd_sports)
print(median_sports)

## boxplot of scores for sports section
ggplot(data = sports_section, aes(x = "", y = Score)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Distribution of Scores - Sports Section", y = "Score")

## boxplot of scores for regular section
ggplot(data = regular_section, aes(x = "", y = Score)) +
  geom_boxplot(fill = "gray") +
  labs(title = "Distribution of Scores - Regular Section", y = "Score")
