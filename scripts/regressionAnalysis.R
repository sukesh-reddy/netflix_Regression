##################################
# Media Company Case Study
#################################

# clearing all the working envoirnment Space
rm(list = ls())

# Setting the working Space
setwd("D:\\github\\NetflixCaseStudy\\sol\\code_for_udemy")

# Importing required libraries
library(tidyverse)

# Loading the data - Data Collection
media <- read_csv('mediacompany.csv') [,-ncol(media)]

# Let's explore the top 5 rows
head(media)

# Converting the first column as date column - Data Preparation
media$Date <- as.Date(media$Date,format = '%m/%d/%Y')

# Creating a column days - Feature Engineering
start.day <- as.Date('2017-02-28')
media$day <- media$Date - start.day
media$day <- as.numeric(gsub(' days',"",media$day))

# Creating weekday column
media$weekday <- wday(media$Date)

# Creating weekend column
media$weekend <- ifelse(media$weekday %in% c(1,7),1,0)

# Data Exploration
ggplot(data=media) +
  geom_line(mapping = aes(x=day,y=Views_show))

ggplot(data=media) +
  geom_point(mapping = aes(x=day,y=Views_show))


# Modelling

library(car)
library(caret)

trainingIndexes <- createDataPartition(media$Views_show,list = FALSE,p=0.8)
trainData <- media[trainingIndexes,-1]
testData <- media[-trainingIndexes,-1]

model1 <- (lm(as.formula('Views_show~.'),data=trainData))
vif((model1))

model1 <- (lm(Views_show ~ .-Visitors,data=trainData))
vif((model1))

model1 <- (lm(Views_show ~ .-Visitors - Views_platform,data=trainData))
vif((model1))
summary(model1)

# Step Wise regression
null = lm(Views_show~1,data=trainData)
extractAIC(null)
full = lm(Views_show ~ .-Visitors - Views_platform,data=trainData)
extractAIC(full)

stepAIC(full,direction = 'backward')
extractAIC()