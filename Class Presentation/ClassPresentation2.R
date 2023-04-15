#clear console and R environment
shell('cls')
#remove from environment
rm(list = ls())

#libraries
library(psych) #for describe function
library(ggplot2) #for plotting data

#Load dataset
dataset <- read.csv('KAG_conversion_data.csv', colClasses = c('integer', 'factor', 
                                                              'numeric', 'factor',
                                                              'factor', 'factor',
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

#Basic Statistics with additional information
describe(ds)


#Smooth distributions of clicks by age
ds %>% ggplot(aes(x = age, 
                  y = clicks, 
                  fill = age)
) + geom_violin() + geom_boxplot(width = .3, position = position_dodge(width = 0)) + theme(legend.position = 'none')

#Smooth distributions of total conversation by age
ds %>% ggplot(aes(x = age, 
                  y = total_conversion, 
                  fill = age)
) + geom_violin() + geom_boxplot(width = .3, position = position_dodge(width = 0)) + theme(legend.position = 'none')

#Smooth distributions of clicks by gender
ds %>% ggplot(aes(x = gender, 
                  y = clicks, 
                  fill = gender)
) + geom_violin() + geom_boxplot(width = .3, position = position_dodge(width = 0)) + theme(legend.position = 'none')

#Smooth distributions of clicks by interest
ds %>% ggplot(aes(x = interest, 
                  y = clicks, 
                  fill = interest)
) + geom_violin() + geom_boxplot(width = .3, position = position_dodge(width = 0)) + theme(legend.position = 'none')

pairs(~clicks + age + gender + interest, data=ds, main = "simple scatterplot matrix")
