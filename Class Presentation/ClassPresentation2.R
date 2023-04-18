#clear console and R environment
shell('cls')
#remove from environment
rm(list = ls())

#libraries
library(utils)
library(magrittr)
library(rattle)
library(janitor)
library(psych) #for describe function
library(ggplot2) #for plotting data
library(dplyr) #for mutate
library(reshape2) #Adds melt function - helps plot stacked bar charts
library(RcmdrMisc)
library(fastDummies) #to make dummy variables

#Load dataset
dataset <- read.csv('KAG_conversion_data.csv', colClasses = c('integer', 'factor', 
                                                              'factor', 'factor',
                                                              'factor', 'factor',
                                                              'integer', 'integer',
                                                              'numeric', 'integer',
                                                              'integer'))

#No labels needed for factors - already there

#Normalize variable names
names(dataset) %<>% normVarNames()

#Missing values
sum(is.na(dataset))

#Make a copy of the data set
ds <- get('dataset')

#Basic Statistics 
summary(ds)

#Basic Statistics with additional information
describe(ds)

# #Facebook IDs
# length(unique(ds$fb_campaign_id))
# length(ds$fb_campaign_id)
# 
# #Unique campaigns
# unique(ds$xyz_campaign_id)

#Scatterplot of spent vs approved conversions
ggplot(ds, aes(x = spent, y = approved_conversion)) + geom_point() + geom_smooth()
#Simple linear regression on amount spent vs approved conversion
regModel.1 <- lm(approved_conversion ~ spent, data = ds)
summary(regModel.1)
#P-value is < 2.2e-16
#For every dollar spent, approved conversion increases by 0.0118

#New column with total amount spent per campaign
ds %<>% group_by(xyz_campaign_id) %>% mutate(total_spent = sum(spent)) %>% ungroup()
ds %<>% group_by(xyz_campaign_id) %>% mutate(total_impressions = sum(impressions)) %>% ungroup()
ds %<>% group_by(xyz_campaign_id) %>% mutate(total_approved_conversion = sum(approved_conversion)) %>% ungroup()

#New columns with per campaign information for cost per conversion and impression to X rates
ds %<>% group_by(xyz_campaign_id) %>% mutate(cost_per_app_conversion = sum(spent)/sum(approved_conversion)) %>% ungroup()
ds %<>% group_by(xyz_campaign_id) %>% mutate(impressions_to_clicks_rate = (sum(clicks)/sum(impressions))*100) %>% ungroup()
ds %<>% group_by(xyz_campaign_id) %>% mutate(impressions_to_enquire_rate = (sum(total_conversion)/sum(impressions))*100) %>% ungroup()
ds %<>% group_by(xyz_campaign_id) %>% mutate(impressions_to_buy_rate = (sum(approved_conversion)/sum(impressions))*100) %>% ungroup()

#summary statistics
by(ds, ds$xyz_campaign_id, summary)

#ANOVA
#Any campaign spending more than others for app conversion?
anovaModel.1 <- aov(cost_per_app_conversion ~ xyz_campaign_id, data = ds)
summary(anovaModel.1)

#P-adjusted takes into account that there are multiple tests
TukeyHSD(anovaModel.1)

#Plot amount spent per app conversion per campaign
ggplot(ds, aes(xyz_campaign_id, cost_per_app_conversion, fill = xyz_campaign_id)) + geom_bar(aes(), position = "dodge", stat = "identity")

#Make new dataframe with these filters - to compare the rates of conversions
campaign_results <- select(ds, xyz_campaign_id, impressions_to_clicks_rate, impressions_to_enquire_rate, impressions_to_buy_rate)

#Prepare data for a stacked bar chart
campaign_results_melt <- melt(campaign_results, id.vars='xyz_campaign_id')

#Plot campaign results per campaign
ggplot(campaign_results_melt, aes(variable, value)) + geom_bar(aes(fill = xyz_campaign_id), position = "dodge", stat = "identity")

#Calculate the ratios between clicks, enquires, and buys
ds %<>% group_by(xyz_campaign_id) %>% mutate(click_rate_to_enquire_rate = impressions_to_enquire_rate/impressions_to_clicks_rate) %>% ungroup()
ds %<>% group_by(xyz_campaign_id) %>% mutate(click_rate_to_buy_rate = impressions_to_buy_rate/impressions_to_clicks_rate) %>% ungroup()

#Make new dataframe with these filters - to compare the rates of conversions
ratio_comparisons <- select(ds, xyz_campaign_id, click_rate_to_enquire_rate, click_rate_to_buy_rate)

#Prepare data for a stacked bar chart
ratio_comparisons_melt <- melt(ratio_comparisons, id.vars='xyz_campaign_id')

#Plot campaign results per campaign
ggplot(ratio_comparisons_melt, aes(variable, value)) + geom_bar(aes(fill = xyz_campaign_id), position = "dodge", stat = "identity")

#Plot gender per campaign for those who bought
ggplot(ds, aes(x = xyz_campaign_id, weight = approved_conversion, fill = gender)) + geom_bar(position = "dodge")

#Plot age per campaign for those who bought
ggplot(ds, aes(x = xyz_campaign_id, weight = approved_conversion, fill = age)) + geom_bar(position = "dodge")

#Plot interest per campaign for those who bought
ggplot(ds, aes(x = xyz_campaign_id, weight = approved_conversion, fill = interest)) + geom_bar(position = "dodge")

#Make new dataframe with these filters
people_buying <- filter(ds, approved_conversion > 0)

people_buying_gender <- filter(ds, gender == 'M')

summary(people_buying_gender)

#Chi-Square
#Is there a relationship between the campaign and gender?
table(people_buying$gender, people_buying$xyz_campaign_id)
chisq.test(people_buying$gender, people_buying$xyz_campaign_id)
#0.09347 = p-value, fail to reject

#Chi-Square
#Is there a relationship between the campaign and gender?
table(people_buying$age, people_buying$xyz_campaign_id)
chisq.test(people_buying$age, people_buying$xyz_campaign_id)
#0.2205 = p-value, fail to reject

#Chi-Square
#Is there a relationship between the campaign and interest?
table(people_buying$interest, people_buying$xyz_campaign_id)
chisq.test(people_buying$interest, people_buying$xyz_campaign_id)
#0.3024 = p-value, fail to reject

#https://www.investopedia.com/terms/c/chi-square-statistic.asp

#______________________________________________________________


#Smooth distributions of approved conversions by gender
ds %>% ggplot(aes(x = gender, 
                  y = approved_conversion, 
                  fill = gender)
) + geom_violin() + geom_boxplot(width = .3, position = position_dodge(width = 0)) + theme(legend.position = 'none')

#Plot gender of approved conversions 
ggplot(ds, aes(x = gender, weight = approved_conversion, fill = gender)) + geom_bar(position = "dodge")

#Smooth distributions of approved conversions by age
ds %>% ggplot(aes(x = age, 
                      y = approved_conversion, 
                      fill = age)
) + geom_violin() + geom_boxplot(width = .3, position = position_dodge(width = 0)) + theme(legend.position = 'none')

#Plot age of approved conversions 
ggplot(ds, aes(x = age, weight = approved_conversion, fill = age)) + geom_bar(position = "dodge")

#Smooth distributions of approved conversions by interest
ds %>% ggplot(aes(x = interest, 
                             y = approved_conversion, 
                             fill = interest)
) + geom_violin() + geom_boxplot(width = .3, position = position_dodge(width = 0)) + theme(legend.position = 'none')

#Plot interest of approved conversions 
ggplot(ds, aes(x = interest, weight = approved_conversion, fill = interest)) + geom_bar(position = "dodge")

#Test for normal distribution in dependent variable
shapiro.test(ds$approved_conversion)
#p-value < 2.2e-16 so hypothesis fails to reject - not normal

#Test equal variance in gender and approved conversions
leveneTest(approved_conversion ~ gender, data=ds, center="mean")
#p-value = 0.9069 - fail to reject - assume equal variance

#Are some genders buying more than others?
anovaModel.1 <- aov(approved_conversion ~ gender, data = ds)
TukeyHSD(anovaModel.1)
summary(anovaModel.1)
#p-value = 0.392, fail to reject hypothesis

#Test equal variance in age and approved conversions
leveneTest(approved_conversion ~ age, data=ds, center="mean")
#p-value = 0.0002475 - reject - can't assume equal variance

#Are some age ranges buying more than others?
anovaModel.2 <- aov(approved_conversion ~ age, data = ds)
TukeyHSD(anovaModel.2)
summary(anovaModel.2)
#p-value = 0.0146, reject hypothesis

#Test equal variance in interest and approved conversions
leveneTest(approved_conversion ~ interest, data=ds, center="mean")
#p-value = 3.844e-05 - reject - can't assume equal variance

#Are some interests buying more than others?
anovaModel.3 <- aov(approved_conversion ~ interest, data = ds)
summary(anovaModel.3)
#p-value = 0.00369, reject hypothesis

##Create dummy variable for gender 
ds <- dummy_cols(ds, select_columns = c("gender"))

##Creating a new factor variable for age  
ds$age <- factor(ds$age, labels =
                        c('age_group_1','age_group_2','age_group_3','age_group_4'))

##Create dummy variable for age
ds <- dummy_cols(ds, select_columns = c("age"))

##Multiple regression for gender, age and approved conversion
reqModel <- lm(data = ds, approved_conversion ~ age_age_group_1 
               + age_age_group_2 + age_age_group_3 + age_age_group_4 
               + gender_F + gender_M
               + age_age_group_1*gender_F + age_age_group_2*gender_F + age_age_group_3*gender_F + age_age_group_4*gender_F
               + age_age_group_1*gender_M + age_age_group_2*gender_M + age_age_group_3*gender_M + age_age_group_4*gender_M
               )
summary(reqModel)

#Smooth distributions of clicks by age
# ds %>% ggplot(aes(x = age, 
#                   y = clicks, 
#                   fill = age)
# ) + geom_violin() + geom_boxplot(width = .3, position = position_dodge(width = 0)) + theme(legend.position = 'none')
# 
# #Smooth distributions of total conversation by age
# ds %>% ggplot(aes(x = age, 
#                   y = total_conversion, 
#                   fill = age)
# ) + geom_violin() + geom_boxplot(width = .3, position = position_dodge(width = 0)) + theme(legend.position = 'none')
# 
# #Smooth distributions of clicks by gender
# ds %>% ggplot(aes(x = gender, 
#                   y = clicks, 
#                   fill = gender)
# ) + geom_violin() + geom_boxplot(width = .3, position = position_dodge(width = 0)) + theme(legend.position = 'none')
# 
# #Smooth distributions of clicks by interest
# ds %>% ggplot(aes(x = interest, 
#                   y = clicks, 
#                   fill = interest)
# ) + geom_violin() + geom_boxplot(width = .3, position = position_dodge(width = 0)) + theme(legend.position = 'none')

#pairs(~clicks + age + gender + interest, data=ds, main = "simple scatterplot matrix")
