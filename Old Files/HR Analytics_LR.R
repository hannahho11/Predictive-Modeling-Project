library(dplyr)
library(tidyr)
library(ggplot2)
library(Amelia)

#Read in files
g_data <- read.csv("C:/Users/rawin/OneDrive/UT AUSTIN MSBA/Predictive Models/Project/general_data.csv")
ms_data <- read.csv("C:/Users/rawin/OneDrive/UT AUSTIN MSBA/Predictive Models/Project/manager_survey_data.csv")
es_data <- read.csv("C:/Users/rawin/OneDrive/UT AUSTIN MSBA/Predictive Models/Project/employee_survey_data.csv")
in_time <- read.csv("C:/Users/rawin/OneDrive/UT AUSTIN MSBA/Predictive Models/Project/in_time.csv")
out_time <- read.csv("C:/Users/rawin/OneDrive/UT AUSTIN MSBA/Predictive Models/Project/out_time.csv")

Master_Data <- merge(g_data,es_data,by = 'EmployeeID',all = F)
Master_Data <- merge(Master_Data,ms_data,by = 'EmployeeID',all = F)

##MISSING DATA
#Missing Data Map
missmap(Master_Data,y.at=c(1),y.labels = c(''),col=c('yellow','black'))
#Omit NA data from dataframe since very little NA
Master_Data <- na.omit(Master_Data)
#Re-run missing data map
missmap(Master_Data,y.at=c(1),y.labels = c(''),col=c('yellow','black'))
#No more missing values


##EXPLORATORY DATA ANALYSIS
ggplot(Master_Data,aes(MonthlyIncome)) + 
    geom_histogram(aes(fill=Attrition),color='black',binwidth=10000) + theme_bw()
#Higher levels of attrition at lower income levels

ggplot(Master_Data,aes(WorkLifeBalance)) +
  geom_bar(aes(fill = Attrition),color = 'black') +
  theme_bw()
#WorkLifeBalance: 1-Bad,2-Good,3-Better,4-Best
#Those with 'better' work life balance seem to leave more - no pattern

ggplot(Master_Data,aes(DistanceFromHome)) + 
  geom_histogram(aes(fill=Attrition),color='black',binwidth=2) + theme_bw()
#People who live close to work seem to leave the company more often

ggplot(Master_Data,aes(JobRole)) +
  geom_bar(aes(fill = Attrition),color = 'black') +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#There seems to be higher attrition among the Lab Technicians, Research Scientists, and Sales Execs
#But theres also more of these 3 job roles - non-conclusive

ggplot(Master_Data,aes(JobLevel)) +
  geom_bar(aes(fill = Attrition),color = 'black') +
  theme_bw()
#People at the lower job levels seem to leave more often

##CLEAN DATA

#Remove columns we dont need in our analysis
#Master_Data <- select(Master_Data,-EmployeeID,-EmployeeCount,-Over18,-StandardHours)
Master_Data$StandardHours <- NULL
Master_Data$Over18 <- NULL
Master_Data$EmployeeCount <- NULL
Master_Data$EmployeeID <- NULL

##SPLIT DATA
library(caTools)

sample <- sample.split(Master_Data$Attrition,SplitRatio = 0.7)

#Training Data
train <- subset(Master_Data,sample==T)
#Testing Data
test <- subset(Master_Data,sample==F)

##TRAIN THE LOGISTIC REGRESSION MODEL
logModel <- glm(Attrition ~., family = binomial('logit'),data = train)
summary(logModel)

##PREDICTIONS
pred_probabilities <- predict(logModel,test,type = 'response')
pred_results <- ifelse(pred_probabilities>0.5,1,0)

#Convert Attrition column in test to 0s and 1s to compare to pred_results
test$AttritionClass <- ifelse(test$Attrition == 'Yes',1,0)

misClassError <- mean(pred_results != test$AttritionClass)
accuracy <- (1-misClassError)

print(misClassError)
print(accuracy)

