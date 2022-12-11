install.packages("gridExtra")
install.packages("mfx")
install.packages("corrplot")
install.packages("ROCR")
library("ggplot2")
library(ROCR)
library(MASS)
library(margins)
library(ggplot2)
library(sjPlot)
library(caTools)
library(mfx)
library(gridExtra)
library(corrplot)

#Location of data online
#"https://www.kaggle.com/datasets/johnsmith88/heart-disease-dataset"
heart_data <- read.csv("location of data")

#converting the columns into factors since they are categorical variables
heart_data$thal <- as.factor(heart_data$thal)
heart_data$cp <- as.factor(heart_data$cp)
heart_data$fbs <- as.factor(heart_data$fbs)
heart_data$restecg <- as.factor(heart_data$restecg)
heart_data$slope <- as.factor(heart_data$slope)
heart_data$ca <- as.factor(heart_data$ca)
heart_data$exang <- as.factor(heart_data$exang)
heart_data$sex <- as.factor(heart_data$sex)

#Summary of imported data
summary(heart_data)

#Creating LOGISTIC REGRESSION MODEL
##Dividing our data set into two sets in a ratio of 4:1; a training dataset and a testing data set.  The training data set is used to create a model and the testing dataset is used to test the model
set.seed(1)
split <- sample.split(heart_data, SplitRatio = 0.8)

#Training dataset we label as "trainingdataset"
trainingdataset <- subset(heart_data, split == "TRUE")

#Testing dataset we label as "testingdataset"
testingdataset <- subset(heart_data, split == "FALSE")


#Building LOGISTIC REGRESSION MODEL from only the training dataset and labeling it as training_model

full_model <-glm(target~.,trainingdataset, family = "binomial")


summary(full_model)

####THIS IS THE MODEL CREATED WITH ALL THE DATA
##########full_model <-glm(target~.,heart_data, family = "binomial")

logit.use <- log(full_model$fitted.values/(1 - full_model$fitted.values))


#PLOTS
#A plot of each indepedent variable vs Response variable
plot(heart_data$age,heart_data$target, main = "Heart Disease vs Age",xlab = "Age", ylab = "(Prob. of) Heart Disease", ylim = c(0,1), las = 1)
plot(heart_data$sex,heart_data$target, main = "Heart Disease vs Sex",xlab = "Sex", ylab = "(Prob. of) Heart Disease", ylim = c(0,1), las = 1)
plot(heart_data$cp,heart_data$target, main = "Heart Disease vs Cp",xlab = "CP", ylab = "(Prob. of) Heart Disease", ylim = c(0,1), las = 1)
plot(heart_data$trestbps,heart_data$target, main = "Heart Disease vs TRestBPS",xlab = "TRestBPS", ylab = "(Prob. of) Heart Disease", ylim = c(0,1), las = 1)
plot(heart_data$chol,heart_data$target, main = "Heart Disease vs Chol",xlab = "Chol", ylab = "(Prob. of) Heart Disease", ylim = c(0,1), las = 1)
plot(heart_data$fbs,heart_data$target, main = "Heart Disease vs Fbs",xlab = "Fbs", ylab = "(Prob. of) Heart Disease", ylim = c(0,1), las = 1)
plot(heart_data$restecg,heart_data$target, main = "Heart Disease vs Restecg",xlab = "Restecg", ylab = "(Prob. of) Heart Disease", ylim = c(0,1), las = 1)
plot(heart_data$thalach,heart_data$target, main = "Heart Disease vs Thalach",xlab = "Thalach", ylab = "(Prob. of) Heart Disease", ylim = c(0,1), las = 1)
plot(heart_data$exang,heart_data$target, main = "Heart Disease vs Exang",xlab = "Exang", ylab = "(Prob. of) Heart Disease", ylim = c(0,1), las = 1)
plot(heart_data$oldpeak,heart_data$target, main = "Heart Disease vs Oldpeak",xlab = "Oldpeak", ylab = "(Prob. of) Heart Disease", ylim = c(0,1), las = 1)
plot(heart_data$slope,heart_data$target, main = "Heart Disease vs Slope",xlab = "Slope", ylab = "(Prob. of) Heart Disease", ylim = c(0,1), las = 1)
plot(heart_data$ca,heart_data$target, main = "Heart Disease vs Ca",xlab = "Ca", ylab = "(Prob. of) Heart Disease", ylim = c(0,1), las = 1)
plot(heart_data$thal,heart_data$target, main = "Heart Disease vs Thal",xlab = "Thal", ylab = "(Prob. of) Heart Disease", ylim = c(0,1), las = 1)


#CHECKING MODEL ASSUMPTIONS
#1) Checking for linear relationship between continuous predictor variables(age, cholesterol, thalach, trestbps, oldpeak) and log of  response variable "disease" or "no disease"
#Response variable is a binary "disease" or "no disease"

lin_chol <- ggplot(trainingdataset, aes(logit.use, trainingdataset$chol))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  ylab("Cholesterol mg/dL")+xlab("Log Odds")+
  theme_bw()
lin_age <- ggplot(trainingdataset, aes(logit.use, trainingdataset$age))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  ylab("Age")+xlab("Log Odds")+
  theme_bw()
lin_thalach <- ggplot(trainingdataset, aes(logit.use, trainingdataset$thalach))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  ylab("Max Heart Rate Achieved")+xlab("Log Odds")+
  theme_bw()
lin_oldpeak <- ggplot(trainingdataset, aes(logit.use, trainingdataset$oldpeak))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  ylab("Diagnostics ST Segment Depression")+xlab("Log Odds")+
  theme_bw()
lin_trestbps <- ggplot(trainingdataset, aes(logit.use, trainingdataset$trestbps))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  ylab("Resting blood pressure mmHg")+xlab("Log Odds")+
  theme_bw()

grid.arrange(lin_trestbps, lin_age, lin_oldpeak,lin_chol,lin_thalach, ncol = 2)

#Checking to see if response variables are correlated to each other for continuous variables.

#Using the heart data set with only continuous variables
reduced_data <- trainingdataset[, c(1,4,5,8,10)]

#Construction correlation matrix
cor_matrix <- cor(reduced_data, method = "pearson")
cor_plot <- corrplot(cor_matrix, method = 'number', type = 'upper')
cor_plot
grid.arrange(cor_plot, ncol = 1)


#Checking to see if response variables are correlated to each other amongst categorical variables
chisq.test(heart_data$sex, heart_data$cp)
chisq.test(heart_data$sex, heart_data$fbs)
chisq.test(heart_data$sex, heart_data$restecg)
chisq.test(heart_data$sex, heart_data$exang)
chisq.test(heart_data$sex, heart_data$slope)
chisq.test(heart_data$sex, heart_data$ca)
chisq.test(heart_data$sex, heart_data$thal)
chisq.test(heart_data$cp, heart_data$fbs)
chisq.test(heart_data$cp, heart_data$restecg)
chisq.test(heart_data$cp, heart_data$exang)
chisq.test(heart_data$cp, heart_data$slope)
chisq.test(heart_data$cp, heart_data$ca)
chisq.test(heart_data$cp, heart_data$thal)
chisq.test(heart_data$fbs, heart_data$restecg)
chisq.test(heart_data$fbs, heart_data$exang)
chisq.test(heart_data$fbs, heart_data$slope)
chisq.test(heart_data$fbs, heart_data$ca)
chisq.test(heart_data$fbs, heart_data$thal)
chisq.test(heart_data$restecg, heart_data$exang)
chisq.test(heart_data$restecg, heart_data$slope)
chisq.test(heart_data$restecg, heart_data$ca)
chisq.test(heart_data$restecg, heart_data$thal)
chisq.test(heart_data$exang, heart_data$slope)
chisq.test(heart_data$exang, heart_data$ca)
chisq.test(heart_data$exang, heart_data$thal)
chisq.test(heart_data$slope, heart_data$ca)
chisq.test(heart_data$slope, heart_data$thal)
chisq.test(heart_data$ca, heart_data$thal)



#Model selection; choosing appropriate predictor variables.
#From our summary above, we removed fbs and slope.

#Now me create our new model with the removed fbs and slope columns which were
#correlated with sex.

#Training dataset we label as "trainingdataset"
MODtrainingdataset <- subset(trainingdataset, select = -c(fbs,slope) )

#Testing dataset we label as "testingdataset"
MODtestingdataset <- subset(testingdataset, select = -c(fbs,slope) )

MODfull_model <-glm(target~.,MODtrainingdataset, family = "binomial")

#The coefficients of our model
coefficients(MODfull_model)

#Calculate average marginal effects of the model.
mfxfull <- margins(MODfull_model)
summary(mfxfull)

#Calculate marginal effects at changes in the sex category for a sequence of ages.
mfxsex <- margins(MODfull_model, variables = c("sex"), at = list(age = seq(30,70, 10)))
summary(mfxsex)

#Exponentiating these coefficients represents the change in Odds ratio associated with
#1 unit increase for continuous variable and for categorical,
#they represent the change in category relative to the base/default category(0)
Oddsratios <- exp(coefficients(MODfull_model))
print(Oddsratios)

#For instance, we analyze the case for a continuous variable first for age, which is continuous
oddsratioforage <- exp(coefficients(MODfull_model))[2]
percentchangeforage <- 100 *(oddsratioforage -1)
print(oddsratioforage)
print(percentchangeforage)

#95% confidence interval for each of the odds ratios:
round(cbind(ORs = exp(coef(MODfull_model)), exp(confint(MODfull_model))), digits = 2)


#LOGIT TRANSFORM PERSPECTIVE OF MODEL AS A LINEAR FUNCTION
#Logit or log odds.  This is a linear function


#TESTING

##Dividing our data set into two sets in a ratio of 4:1; a training dataset and a testing data set.  The training data set is used to create a model and the testing dataset is used to test the model
set.seed(1)


#The predicted probabilities for the training data set
res <- predict(MODfull_model, MODtrainingdataset, type = "response")

#Constructing a table (confusion matrix) based off training data set with 0.5 threshold.
#This table displays the number of false positives and true positives with a 0.5 threshold.
table(Actualvalue = MODtrainingdataset$target, Predictedvalue = res>0.5)


#CHECKING ACCURACY OF MODEL ON "TESTING" dataset.

#WE obtain the fitted values or predicted probability by feeding the testing dataset into our logistic model and we label this as res1
res1 <- predict(MODfull_model, MODtestingdataset, type = "response")

#Confusion Matrix when our Logistic model is fed the testing dataset at 0.5 threshold.
#Displays in a tabular form, the number of accurate predictions and the number of false positives and false negatives.
table1 <- table(Actualvalue = MODtestingdataset$target, predictedValue = res1>0.5)
print(table1)
accuracy1 <- (table1[1,1] + table1[2,2]) / ( table1[1,1] + table1[1,2] + table1[2,1] + table1[2,2] )
print(accuracy1)

sensitivity1 <- table1[2,2]/ (table1[2,2] + table1[2,1])
print(sensitivity1)

specificity1 <- table1[1,1]/ (table1[1,1] + table1[1,2])
print(specificity1)

missRate1 <- table1[2,1]/ (table1[2,1] + table1[2,2])
print(missRate1)

#Confusion Matrix when our Logistic model is fed the testing dataset at 0.3 threshold.
table2 <- table(Actualvalue = MODtestingdataset$target, predictedValue = res1>0.3)
print(table2)

accuracy2 <- (table2[1,1] + table2[2,2]) / ( table2[1,1] + table2[1,2] + table2[2,1] + table2[2,2] )
print(accuracy2)

sensitivity2 <- table2[2,2]/ (table2[2,2] + table2[2,1])
print(sensitivity2)

specificity2 <- table2[1,1]/ (table2[1,1] + table2[1,2])
print(specificity2)

missRate2 <- table2[2,1]/ (table2[2,1] + table2[2,2])
print(missRate2)

#Accuracy, sensitivity, specificity and miss rate from optimial threshold.
table3 <- table(Actualvalue = MODtestingdataset$target, predictedValue = res1>0.55)
print(table3)

accuracy3 <- (table3[1,1] + table3[2,2]) / ( table3[1,1] + table3[1,2] + table3[2,1] + table3[2,2] )
print(accuracy3)

sensitivity3 <- table3[2,2]/ (table3[2,2] + table3[2,1])
print(sensitivity3)

specificity3 <- table3[1,1]/ (table3[1,1] + table3[1,2])
print(specificity3)

missRate3 <- table3[2,1]/ (table3[2,1] + table3[2,2])
print(missRate3)


#ROC curve and AUC
roc.info <- roc(MODtrainingdataset$target, res, plot = TRUE, legacy.axes = TRUE, main = "ROC Curve - Training Data" ,xlab= "False Positive Rate (1- specificity)", ylab = "True Positive Rate (True Positve Percentage)", print.auc = TRUE)

#Finding the intersection of sensitivity and specificity.
plot(roc.info$thresholds, roc.info$specificities, col = "red", xlab = "THRESHOLDS", ylab = "SENSITVITY/SPECIFICITY", type = 'l')
lines(roc.info$thresholds, roc.info$sensitivities, col = "blue")
legend(0.2,0.2, legend = c("Specificities", "Sensitivities"), fill = c("red", "blue"))

#Assessing calibration using Hosmer_Lemeshow Goodness of Fit test
#Divide training data set into 10 groups and compare
hl <- hoslem.test(MODfull_model$y, res, g = 10)
hl_df <- data.frame(cbind(hl$expected, hl$observed))
hl
hl_df

#Psuedo R^2 test

PsuedoR2 <- 1 - (MODfull_model$deviance/MODfull_model$null.deviance)
print(PsuedoR2)



