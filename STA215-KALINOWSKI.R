## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2024_1_17
# Who:       Zachary D. Kline

## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
library(readr)
dataset <- read_csv("UPDATED FINAL PROJ DATASET - Sheet1 (1).csv")
View(dataset)

##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
table(dataset$song_length)
mean(dataset$song_length)
sd(dataset$song_length)

table(dataset$number_of_listens)
mean(dataset$number_of_listens)
sd(dataset$number_of_listens)

table(dataset$song_descriptor)

table(dataset$song_topic)

table(dataset$personal_enjoyment)
mean(dataset$personal_enjoyment)
sd(dataset$personal_enjoyment)

table(dataset$album)

##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
lm(personal_enjoyment ~ album, data = dataset)
aov(personal_enjoyment ~ album, data = dataset)
summary(personal_enjoyment ~ album, data = dataset)
boxplot(personal_enjoyment ~ album, data = dataset)

lm(personal_enjoyment ~ number_of_listens, data = dataset)
aov(personal_enjoyment ~ number_of_listens, data = dataset)
summary(personal_enjoyment ~ number_of_listens, data = dataset)

# filter out songs longer than 10 minutes (600 seconds)
dataset_withououtlier <- dataset %>%
  filter(song_length < 600)
#filter out songs with more than 10 million listens
dataset_withoutlistensoutlier <- dataset %>%
  filter(number_of_listens < 10000000)

##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
linear_plot <- plot(dataset_withououtlier$song_length, dataset_withououtlier$personal_enjoyment)
print(linear_plot)
meany <- mean(dataset_withououtlier$song_length)
meanx <- mean(dataset_withououtlier$personal_enjoyment)
abline(h = meanx, col = "black")
abline(v = meany, col = "black")
linear_relationship <- lm(personal_enjoyment ~ song_length, data = dataset)
summary(linear_relationship)
abline(linear_relationship, col = "red")

linear_plot <- plot(dataset_withoutlistensoutlier$number_of_listens, dataset_withoutlistensoutlier$personal_enjoyment)
print(linear_plot)
meany <- mean(dataset_withoutlistensoutlier$number_of_listens)
meanx <- mean(dataset_withoutlistensoutlier$personal_enjoyment)
abline(h = meanx, col = "black")
abline(v = meany, col = "black")
linear_relationship <- lm(personal_enjoyment ~ song_length, data = dataset_withoutlistensoutlier)
summary(linear_relationship)
abline(linear_relationship, col = "red")

##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(dataset$song_length, residuals(linear_relationship))
plot(dataset_withoutlistensoutlier$number_of_listens, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")


##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(dataset$song_descriptor, dataset$song_topic)

chisq.test(table(dataset$song_descriptor, dataset$song_topic))
