#clear console and R environment
shell('cls')
#remove from environment
rm(list = ls())

#libraries
library(utils)
#For piping function
library(dplyr)
library(randomForest)
library(magrittr)
library(rattle)
library(janitor)
library(ggplot2)
library(fastDummies)

#Load dataset
dataset <- read.csv('KAG_conversion_data.csv', colClasses = c('integer', 'factor', 
                                                             'factor', 'factor',
                                                             'factor', 'numeric',
                                                             'numeric', 'numeric',
                                                             'numeric', 'numeric',
                                                             'numeric'))

#No labels needed for factors - already there

#Normalize variable names
names(dataset) %<>% normVarNames()

#Missing values
sum(is.na(dataset))

#Make a copy of the data set
ds <- get('dataset')

#Basic Statistics
summary(ds)

#Histogram of clicks
ggplot(ds, aes(x = clicks)) + geom_bar()

ggplot(ds, aes(x = clicks)) + geom_bar() + facet_grid(ds$age)

#Smooth distributions of satisfaction by department
ds %>% ggplot(aes(x = age, 
                  y = clicks, 
                  fill = age)
) + geom_violin() + geom_boxplot(width = .3, position = position_dodge(width = 0)) + theme(legend.position = 'none')

#Are some age groups clicking more than others?
anovaModel.1 <- aov(clicks ~ age, data = ds)
summary(anovaModel.1)

#Clicks and spent visualized in a line
ggplot(ds, aes(x = spent, y = clicks)) + geom_point() + geom_smooth()

regModel.1 <- lm(clicks ~ spent, data = ds)
summary(regModel.1)
#For every Euro spent on campaign, spend 649 more

#See for each department
ggplot(ds, aes(x = clicks)) + geom_bar() + facet_grid(ds$gender)

#Smooth distributions of satisfaction by department
ds %>% ggplot(aes(x = gender, 
                  y = clicks, 
                  fill = gender)
) + geom_violin() + geom_boxplot(width = .3, position = position_dodge(width = 0)) + theme(legend.position = 'none')


regModel.3 <- lm(approved_conversion ~ impressions + clicks + spent, data = ds)
summary(regModel.3)

#Add dummy variables
ds <- dummy_cols(ds, select_columns = c('gender'))
ds <- dummy_cols(ds, select_columns = c('age'))

regModel.3 <- lm(approved_conversion ~ gender_M + gender_F, data = ds)
summary(regModel.3)

ds$segment <- as.factor(ifelse(ds$age == '30-34', 'age_group_1',
                               ifelse(ds$age == '35-40', 'age_group_2',
                                      ifelse(ds$satisfaction > 50, 'c', 'd'
                                      )
                               )
)
)
