rm(list = ls())
setwd("C:/Users/micah/OneDrive/Documents/R/Heart disease")

data1 <- read.csv("~/R/Heart disease/heart_statlog_cleveland_hungary_final.csv")

dim(data1)
colnames(data1)
str(data1)

#Investigate  missing data
colSums(is.na(data1))

#Investigate correlation among predictors.
correlation_matrix <- cor(data1)

library(GGally)
library(ggplot2)
ggcorr(data1, label = TRUE, label_alpha = .7)
#watch for correlation between ST.slope and old peak, exercise angina, max heart rate.
#watch out for correlation between maximum heart rate and chest pain type.

 #Change data Types

data1$sex <- factor(data1$sex,
                    levels=c(0,1),
                    labels = c("Female","Male"))
table(data1$sex)

#table(data1$chest.pain.type)
data1$chest.pain <- factor(data1$chest.pain.type,
                           levels=c(1,2,3,4),
                           labels=c("typical angina","atypical angina",
                                    "non-anginal pain","asymptomatic"))
table(data1$chest.pain)

#table(data1$fasting.blood.sugar)
data1$fasting.sugar <- factor(data1$fasting.blood.sugar,
                              levels=c(0,1),
                              labels=c("False","True"))
table(data1$fasting.sugar)

data1$resting.ecg <- factor(data1$resting.ecg,
                            levels=c(0,1,2),
                            labels=c("Normal","ST-T abnormality",
                                     "Left ventricular hypertrophy"))
table(data1$resting.ecg)

data1$exercise.angina <- factor(data1$exercise.angina,
                                levels=c(0,1),
                                labels=c("No","Yes"))
table(data1$exercise.angina)

data1$ST.slope[data1$ST.slope == 0] <- 1
data1$ST.slope <- factor(data1$ST.slope,
                         levels=c(1,2,3),
                         labels=c("upsloping","flat","downsloping"))
table(data1$ST.slope)

data1$target <- factor(data1$target,
                       levels=c(0,1),
                       labels=c("No disease","Heart disease"))
table(data1$target)

 #Descriptive Statistics 
attach(data1)

par(mfrow=c(3,2))
hist(age)
hist(resting.bp.s, main="Histogram of resting blood pressure")
hist(cholesterol)
hist(max.heart.rate, main="Histogram of maximum heart rate achieved")
hist(oldpeak)

#Calculate means, medians, standard deviation and IQR
library(dplyr)
descriptive <- data1 %>%
  select(age,resting.bp.s,cholesterol,max.heart.rate,oldpeak) %>%
  summarise_all(list(min,max,mean,sd,median,IQR))

print(descriptive)

#categorical descriptives
#Calculate proportions for multiple variables
combined_proportions <- data1 %>%
  tidyr::gather(key = "variable", value = "value", sex, chest.pain, fasting.sugar,resting.ecg, exercise.angina,
                ST.slope,target) %>%
  group_by(variable, value) %>%
  summarize(count = n()) %>%
  group_by(variable) %>%
  mutate(proportion = count / sum(count))

print(combined_proportions)

   ##Logistic regression model 
logit1 <- glm(target~sex+chest.pain+fasting.sugar+resting.ecg+exercise.angina+ age+
                resting.bp.s+cholesterol+max.heart.rate+oldpeak,data=data1,family = binomial)

summary(logit1)

#Not significant - resting.ecg,age, resting.bp.s. 

logit2 <- glm(target~sex+chest.pain+fasting.sugar+exercise.angina+age
              +cholesterol+max.heart.rate+oldpeak,data=data1,family = binomial)
summary(logit2)

anova(logit2,logit1,test = "Chisq")
#P-value > 0.05. So the RSS imporvement is not significant 

# Get the coefficients of the model
coefficients <- summary(logit2)$coefficients

# Transform the coefficients to odds ratios
odds_ratios <- exp(coefficients[,"Estimate"])
odds_ratios
# Combine odds ratios with confidence intervals
confidence_intervals <- exp(confint(model))

#Split into train and test 
set.seed(2)
train_indices <- sample(seq_len(nrow(data1)),size = 0.7*nrow(data1))
train <- data1[train_indices,]
test <- data1[-train_indices,]

glm.fit <- glm(target~sex+chest.pain+fasting.sugar+exercise.angina+age
              +cholesterol+max.heart.rate+oldpeak,data=train,family = binomial)

glm.prob =predict(glm.fit,test, type="response")
#Compute the predictions using test data.
glm.pred=rep("No",357)
#Probability above 0.5 is predicted as Up
glm.pred[glm.prob>.50]="Yes"
table(glm.pred,test$target)

#correctly predicting (83%)
(139+156)/357 
#prediction error (17%)
1 - (139+156)/357

   ##K-NN method
library (class)
train.x <- cbind(train$sex,train$chest.pain,train$fasting.sugar,train$exercise.angina,
                 train$age,train$cholesterol,train$max.heart.rate,train$oldpeak)
test.x <- cbind(test$sex,test$chest.pain,test$fasting.sugar,test$exercise.angina,
                test$age,test$cholesterol,test$max.heart.rate,test$oldpeak)
train.heart <- train$target

#with k=1
set.seed(1)
knn.pred<-knn(train.x,test.x,train.heart,k=1)
table(knn.pred,test$target)
mean(knn.pred == test$target)
#(130+146)/357 = 77% highest KNN prediction accuracy 

#Export Dataset
library(dplyr)
data1 <- data1 %>%
  select(-chest.pain.type,-fasting.blood.sugar)


library(writexl)
write_xlsx(data1,"C:\\Users\\micah\\OneDrive\\Documents\\R\\Heart disease\\data.xlsx")


