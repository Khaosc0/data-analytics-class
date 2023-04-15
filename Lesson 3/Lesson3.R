rm(list = ls())
shell('cls')

#Libraries
library(utils)
library(magrittr)
library(rattle)
library(psych)
library(effsize)
library(fastDummies)

#Load dataset
dataset <- read.csv('tourism.csv', sep = ";", colClasses = c('integer', 'factor', 
                                                           'numeric', 'numeric',
                                                           'numeric', 'numeric',
                                                           'numeric', 'numeric',
                                                           'factor', 'numeric',
                                                           'factor', 'factor',
                                                           'factor', 'factor',
                                                           'numeric', 'factor'))

#Columns are what we measured and rows unique observations
#Factors are numbers so we need to turn them into labels to understand
dataset$skiholiday <- factor(dataset$skiholiday, labels = c('no', 'yes'))
dataset$sex <- factor(dataset$sex, labels = c('male', 'female'))
dataset$country <- factor(dataset$country, labels = c('swi', 'ger', 'aus', 'oth'))

#Make a copy
ds <- get('dataset')

#Normalize variable names
#names(ds) <- normVarNames(names(ds))
#Same as above but pipes in - put in input and take output and assign as this
names(ds) %<>% normVarNames()

#Missing values
sum(is.na(ds))

##Get to know your data
summary(ds)
#Gives summary for each country
#Normal data if median and mean are more or less the same
by(ds, ds$country, summary)

#Cleaner summary to look at
#* indicates factors as some statistics do not really make sense
describe(ds)
#Group summary by sex
#Uses index of the factor, not names
#So 1 is male and 2 is female
describeBy(ds, ds$sex, mat = TRUE)

##Single Sample Tests
##Group Comparisons
##Relationships

##Hypothesis Testing

#two-sided - either smaller than or larger than
#mu is average
#Can see 300 is not contained in 95% confidence interval
#Just write p<0.001 if it is less
#If above, need to write
t.test(ds$expenses, alternative = "two.sided", mu = 300, conf.level = 0.95)

#Null hypothesis is less
#Var equal assumes distributions are the same
t.test(satisfaction ~ sex, data = ds, alternative = 'less', mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)

#What is the Cohen's D between males and females on satisfaction
cohen.d(ds$satisfaction[ds$sex == 'male'], ds$satisfaction[ds$sex == 'female'])

#ANOVA
#Are some countries spending more than others?
#P-value is more so no countries stand out
anovaModel.1 <- aov(expenses ~ country, data = ds)
summary(anovaModel.1)

#P-adjusted takes into account that there are multiple tests
TukeyHSD(anovaModel.1)

#Two factor ANOVA
anovaModel.2 <- aov(expenses ~ country*sex, data = ds)
summary(anovaModel.2)

#Chi-Square
#Count the different subgroups
table(ds$skiholiday, ds$sex)
chisq.test(ds$skiholiday, ds$sex)

##Pearson Correlation
plot(ds$satisfaction, ds$expenses)

#Always an alternative explanation we have no accounted for
#Need a strong theory and test it with data
#Depends on how data is collected and the bias
#Good test to come up with alternative explanations
with(ds, cor.test(expenses, satisfaction, alternative = 'two.sided',
                  method = 'pearson'))
#Exponential relationship with spearman method

#Linear regression
#Assuming two-side and lm - linear model
regModel.1 <- lm(expenses ~ satisfaction, data = ds)
summary(regModel.1)
#For every satisfaction point higher, spend 1.2948 more

regModel.2 <- lm(satisfaction ~ expenses, data = ds)
summary(regModel.2)
#For every dollar spent, satisfaction increases by 0.15437

#fastDummies handles categorical values in regressions
#Assigns Boolean values: From Germany? 0 or 1

#Add dummy variables
ds <- dummy_cols(ds, select_columns = c('country'))

#Multiple linear regression
regModel.3 <- lm(expenses ~ age + stay + satisfaction + country_swi + country_ger 
                 + country_aus + country_oth, data = ds)
summary(regModel.3)
#Distributed (median) around 0 so we are happy - 1Q should be negative and
#3Q should be positive with the median small
#Stay is not correlated
#For every 2.6 years older, spend an extra dollar
#NAs on the other category - if 'all other countries' were the same values
#Determine a ground truth comparing against if you have dummy varaibles
#Other is the ground truth
#Compared to other countries, Swiss are spending 26 dollars more and this is
#significant
#Will take last mutually exclusive one as a baseline

regModel.3 <- lm(expenses ~ age + stay + satisfaction + country_oth + country_swi + country_ger 
                 + country_aus, data = ds)
summary(regModel.3)
#None of them are statistically significant from Austria
#Do ANOVA if want to see differences between countries