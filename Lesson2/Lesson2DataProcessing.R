shell('cls')
rm(list = ls())

#Libraries - Install utils below - if get error then it is installed
#and click No
library(utils)
library(magrittr)
library(rattle)
library(janitor)
library(dplyr)
library(randomForest)
library(ggplot2)

#Load data set - ensure you specify what the delimiter is
#Putting opening and ending parenthesis above and below each other helps
#visibility
#[TRUNCATED] means it has shortened the string because it is very long - is
#not an error
#Should always put an ID in a data set first which is a integer

dataset <- read.csv(file = './Employee.motivation.uk.csv', 
                    sep = ';',
                    colClasses = c('integer', 'factor', 'factor',
                                   'factor', 'factor', 'numeric',
                                   'numeric', 'numeric', 'numeric',
                                   'numeric', 'numeric', 'numeric',
                                   'numeric', 'numeric', 'numeric'
                                   )
                    )

#Define factors
#0 is female and 1 is male
#Data sets should be tidy so each column should be a separate variable
#Each row should be a separate observation
#Make sure variables are explained
dataset$Gender <- factor(dataset$Gender, labels = c('female', 'male'))
dataset$Managerial_responsibility <- factor(dataset$Managerial_responsibility, 
                                            labels = c('No', 'Yes'))
dataset$Position <- factor(dataset$Position, labels = c('salary', 'hourly'))

#Make a copy of the data set
ds <- get('dataset')

#Basic Statistics
summary(ds)
#Summary will also show the NA's
#Look at first and third quantile
#Own_illness shows a big skew
#head(ds) in console to print first six lines of data
#tail(ds) in console to print last six lines

#snake_case is all lowercase with underscores in-between words
#camelCase will put uppercase on new words - example payConditionofEmployment

#Normalizing
#Install magritter, rattle, janitor and add to libraries at the top
#Check names of columns
names(ds)
#Using piping function in console
#Put something into something else
#ds %>% names() puts the dataset into names()
#Normalize the names and pipe back into the dataset
names(ds) %<>% normVarNames()


#Missing values for top_management
missing_target <- ds %>% extract2('top_management') %>% is.na()
#count the missing values - sums TRUE
sum(missing_target)

#New copy of our dataset
dataset_copy1 <- get('ds')

#Remove rows where there is missing values for top_management
#Filters when TRUE
#So need to filter the FALSES (actual data) so negate with a !
ds %<>% filter(!missing_target)
#Can also put in mean or median values for NAs
#Or use MICE - linear relationship in variables so predice what values should be
#Use randomForest to do this
dim(ds)

#immediate_manager impute missing values
#function Impute Missing Values by median/mode.
ds$immediate_manager %<>% na.roughfix()
#Checking that above worked
#Taking immediate_manager from set, seeing if it is na and summing that
#Should get 0 - were removed
ds$immediate_manager %>% is.na() %>% sum()

#Selecting Data
#Select gender and department
ds %>% select(gender, department)
#Include columns with names that end with -tion
ds %>% select(gender, department, ends_with('tion'))
#Include columns containing 'mana' in title
ds %>% select(contains('mana'))

#Grouping Data into new variables
#Make new dataframe with these filters
sat_male <- filter(ds, gender == 'male', satisfaction > 60)
#Get average satisfaction between these two
sat_mana <- (ds$immediate_manager + ds$top_management) / 2
#column bind so it takes a column and binds to the original dataset
#with a new title and as a new column
ds %<>% cbind(sat_mana)

#Make Categorical Variable
#If satisfaction is higher than 90, then a, else above 70 then b, and so on
#Makes a new column segment into ds
ds$segment <- as.factor(ifelse(ds$satisfaction > 90, 'a',
                               ifelse(ds$satisfaction > 70, 'b',
                                      ifelse(ds$satisfaction > 50, 'c', 'd'
                                             )
                                      )
                               )
                        )

#Make new copy
dataset_copy2 <- get('ds')

#New column with average motivation of department - grouping by to get new
#values at categorical level
ds %<>% group_by(department) %>% mutate(avg_mov = mean(motivation)) %>% ungroup()

#Rename columns
ds %<>% rename(pro_per_dev = professional_personal_development)

#Plotting
#Plot a histogram on satisfaction
ggplot(ds, aes(x = satisfaction)) + geom_bar()
#See for each department
ggplot(ds, aes(x = satisfaction)) + geom_bar() + facet_grid(ds$department)
#Average and distribution visualized in a line
ggplot(ds, aes(x = satisfaction, y = motivation)) + geom_point() + geom_smooth()


#Smooth distributions of satisfaction by department
#6 is a binomial distribution - some doing well and some lower down
#1 has a long tail
#4 majority unsatisfied
ds %>% ggplot(aes(x = department, 
                  y = satisfaction, 
                  fill = department)
              ) + geom_violin() + geom_boxplot(width = .3, position = position_dodge(width = 0)) + theme(legend.position = 'none')
#Do the same for gender
ds %>% ggplot(aes(x = gender, 
                  y = satisfaction, 
                  fill = gender)
) + geom_violin() + geom_boxplot(width = .3, position = position_dodge(width = 0)) + theme(legend.position = 'none')
#Does gender affect work?
ds %>% ggplot(aes(x = gender, 
                  y = daily_work, 
                  fill = gender)
) + geom_violin() + geom_boxplot(width = .3, position = position_dodge(width = 0)) + theme(legend.position = 'none')