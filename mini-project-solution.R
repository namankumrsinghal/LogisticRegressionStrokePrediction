data_total  <- read.csv("datafile.csv", header=T)

# install.packages("tidyverse")
library(tidyverse)  # Tidyverse is a collection of data science packages, including dplyr 

#Cleaning up the data
#-------------------------------------------------------------------------------------------------------------------------------------
#original dataset(of 5110 rows)
#data <- read.csv("C:\\Users\\naman\\Desktop\\Acads\\Spring 2021\\Data Analysis and Visualization - 555\\Project\\healthcare-dataset-stroke-data.csv", header = T)

#remove NAs
#data <- na.omit(data)

#peeking at the dataset
#length(data$id)
#head(data)
#tail(data)

#set.seed(19)
#creating/slicing a dataset for patients who had stroke
#data_stroke <- filter(data, data$stroke==1)
#further slicing to remove "Unknown" values from smoking parameter
#data_stroke <- filter(data_stroke, data_stroke$smoking_status != "Unknown")
#length(data_stroke$id)

#now, create a dataset for patient who did not have stroke(keep length of this dataset equal to the stroke dataset)
#s <- sample(length(data_stroke$id):length(data$id), 10*length(data_stroke$id), replace=FALSE)

#data_non_stroke <- data.frame(data[s[1], ])
#names(data_non_stroke) <- colnames(data)

#for (i in s[-1])
#{
#if smoking status is not unknown, add the row to the dataframe
#if (data[i, ]$smoking_status != "Unknown"){
#df <- data.frame(data[i, ])
#names(df) <- colnames(data)
#data_non_stroke <- rbind(data_non_stroke, df)
#}
#if data frame is complete(equal to the length of stroke ones)
#if (length(data_non_stroke$id) == length(data_stroke$id)){
#break
#}
#}
#length(data_non_stroke$id)
#head(data_non_stroke)

#adding the two dataframes
#data_total <- rbind(data_stroke, data_non_stroke)
#length(data_total$id)

#saving this cleaned dataset in a csv file
#write.csv(data_total, "final_data.csv")
#--------------------------------------------------------------------------------------------------------------

#run the next line to access the data from the cleaned csv file directly
#data_total <- read.csv("file_path\\final_data.csv", header = T)

data_stroke <- filter(data_total, data_total$stroke==1)
data_non_stroke <- filter(data_total, data_total$stroke==0)

attach(data_total)

boxplot(data_total$age, main = "age")
#quantile(data_total$age)
#IQR(data_total$age)
#no outliers in age

boxplot(data_total$avg_glucose_level, main = "Glucose level")
#there are outliers in glucose level(However, we don't want to remove them as it makes sense to keep people with high glucose levels in consideration)

boxplot(data_total$bmi, main = "BMI")
#there are outliers(but we don't need to remove, since there is not much difference between bmi's for people not having stroke and those having stroke - this is evident when summaries of both populations are compared)

#lets inspect and get an idea about the two datasets. Insights for non character variables
summary(data_stroke)
summary(data_non_stroke)

cor(data_total$stroke, data_total$age)
cor(data_total$stroke, data_total$avg_glucose_level)
cor(data_total$stroke, data_total$bmi)
#age has a good correlation, we should adjust for age, by performing ANCOVA

#Graphically drawing some insights about character variables
par(mfrow = c(1, 2))

barplot(table(data_non_stroke$gender), col = 'green', main = "Non-Stroke - Gender")
barplot(table(data_stroke$gender), col = 'red', main = "Stroke - Gender")

barplot(table(data_non_stroke$Residence_type), col = 'green', main = "Non-Stroke - Residence")
barplot(table(data_stroke$Residence_type), col = 'red', main = "Stroke - Residence")

barplot(table(data_non_stroke$ever_married), col = 'green', main = "Non-Stroke - Married")
barplot(table(data_stroke$ever_married), col = 'red', main = "Stroke - Married")

barplot(table(data_non_stroke$work_type), col = 'green', main = "Non-Stroke - Work")
barplot(table(data_stroke$work_type), col = 'red', main = "Stroke - Work")

barplot(table(data_non_stroke$smoking_status), col = 'green', main = "Non-Stroke - Smoking")
barplot(table(data_stroke$smoking_status), col = 'red', main = "Stroke - Smoking")

par(mfrow = c(1,1))

#data_total$ever_married <- ifelse(data_total$ever_married == "Yes", 1, 0)

#splitting the data into training and testing set
dt = sort(sample(nrow(data_total), nrow(data_total)*.8))
train<-data_total[dt,]
test<-data_total[-dt,]

#Building logistic regression classification models(using various columns, one by one)
m <- glm(train$stroke~train$avg_glucose_level, data = train, family = binomial)
summary(m)
#Adjusting for age:
install.packages("car")
library(car)
Anova(glm(train$stroke~train$avg_glucose_level+train$age, data = data_total, family = binomial))

m <- glm(train$stroke~train$hypertension, data = train, family = binomial)
summary(m)
#Adjusting for age:
Anova(glm(train$stroke~train$hypertension+train$age, data = train, family = binomial))

m <- glm(train$stroke~train$heart_disease, data = train, family = binomial)
summary(m)
#Adjusting for age:
Anova(glm(train$stroke~train$heart_disease+train$age, data = train, family = binomial))

m <- glm(train$stroke~train$ever_married, data = train, family = binomial)
summary(m)
#Adjusting for age:
Anova(glm(train$stroke~train$ever_married+train$age, data = train, family = binomial))

#the below code shows that hypertension is also not significant
#model <- glm(train$stroke~train$age+train$hypertension+train$avg_glucose_level, data = train, family = binomial)
#summary(model)

#building the final logistic regression classification model
model <- glm(stroke~age+avg_glucose_level, data = train, family = binomial)
summary(model)

model.probs <- predict(model, test, type = "response")
model.pred <- rep(0, length(model.probs))
model.pred[model.probs > 0.5] <- 1
#confusion matrix
table(model.pred, test$stroke)
#error rate
mean(model.pred != test$stroke)
#ROC curve(c stat)
#install.packages("pROC")
library(pROC)
g <- roc(test$stroke ~ model.probs)
print(g)
plot(g, main = "ROC curve")