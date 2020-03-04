library(dplyr)
library(tidyr)
library(ggplot2)
library(class)

#Read in files
g_data <- read.csv("C:/Users/rawin/OneDrive/UT AUSTIN MSBA/Predictive Models/Project/general_data.csv")
ms_data <- read.csv("C:/Users/rawin/OneDrive/UT AUSTIN MSBA/Predictive Models/Project/manager_survey_data.csv")
es_data <- read.csv("C:/Users/rawin/OneDrive/UT AUSTIN MSBA/Predictive Models/Project/employee_survey_data.csv")
in_time <- read.csv("C:/Users/rawin/OneDrive/UT AUSTIN MSBA/Predictive Models/Project/in_time.csv")
out_time <- read.csv("C:/Users/rawin/OneDrive/UT AUSTIN MSBA/Predictive Models/Project/out_time.csv")

Master_Data <- merge(g_data,es_data,by = 'EmployeeID',all = F)
Master_Data <- merge(Master_Data,ms_data,by = 'EmployeeID',all = F)

#Omit NA data from dataframe since very little NA
Master_Data <- na.omit(Master_Data)

##CLEAN DATA

#Remove columns we dont need in our analysis
Master_Data <- select(Master_Data,-EmployeeID,-EmployeeCount,-Over18,-StandardHours,
                      -BusinessTravel,-Department,-DistanceFromHome,
                      -EducationField,-Gender,-JobRole,-MaritalStatus)

#Target column
attrition <- Master_Data[,2]
#Scale data
scaled_MasterData <- scale(Master_Data[,-2])

#Join scaled data with target column
final_data <- cbind(scaled_MasterData,attrition)

#Train/Test Split
test_index <- 3001:4300
test_data <- scaled_MasterData[test_index,]
test_attrition <- attrition[test_index]

train_data <- scaled_MasterData[-test_index,]
train_attrition <- attrition[-test_index]

#Building the KNN Model
predicted_attrition <- knn(train_data,test_data,train_attrition,k=1)

#Misclassification error
misclass_error <- mean(test_attrition != predicted_attrition)
print(misclass_error)

accuracy <- (1-misclass_error)
print(accuracy)

#######
#Choosing a k value

pred_attrition <- NULL
error_rate <- NULL

for (i in 1:20){
  pred_attrition <- knn(train_data,test_data,train_attrition,k=i)
  error_rate[i] <- mean(test_attrition != pred_attrition)
}
print(error_rate)

#Visualize K Elbow Method
k_values <- 1:20
error_df <- data.frame(error_rate,k_values)

ggplot(error_df,aes(k_values,error_rate)) +
  geom_point() + geom_line(lty = 'dotted',color = 'red')




