#### Libraries ####

# Plotting
library(ggplot2) # Plots
library(gridExtra) # Side-by-side plots
library(plotROC) # ROC curves

# Models
library(e1071) # Naive Bayes and Support Vector Machine
library(glmnet) # Logistic Regression - LASSO
library(MASS) # Logistic Regression - Stepwise Selection
#library(caret) # ?
library(party) # Decision Tree

# Misc
#library(dplyr)
library(sqldf) # SQL functions
library(stringr) # String manipulation
library(reshape2) # Reshape datasets
library(cramer) # Compute Cramer's V

#### Functions ####

model_check <- function(title, var){
  Data_Temp <- data.frame(var, get("y", Data_Test))
  colnames(Data_Temp) <- c("Predict", "Actual")
  
  info <- table(Data_Temp$Actual, Data_Temp$Predict)
  info
  
  TP <- info[2,2]
  TN <- info[1,1]
  FN <- info[2,1]
  FP <- info[1,2]
  
  accuracy <- round(100*(TP + TN) / (TP + FN + FP + TN), 1)
  true_positive_rate <- round(100*TP / (TP + FN), 1)
  precision <- round(100*TP / (TP + FP), 1)
  
  y_auc <- ifelse(Data_Temp$Actual == "yes", 1, 0)
  pred_auc <- ifelse(Data_Temp$Predict == "yes", 1, 0)
  auc <- round(auc(y_auc, pred_auc), 2)
  
  outcome <- c(title, accuracy, true_positive_rate, auc, precision)
  names(outcome) <- c("Model", "Accuracy", "TP Rate", "AUC", "Precision")
  return(outcome)
}

#### Data setup ####

set.seed(4444)
k <- 10
us <- 2

Data <- read.csv("C:/Users/Rebecca/Documents/School/MRP/bank-additional-full.csv")

# Test vs. Train Groups

train_size <- floor(nrow(Data)*(2/3))
train_group <- sample(seq_len(nrow(Data)), size = train_size)

Setup_Train <- Data[train_group, ]
Data_Test <- Data[-train_group, ]
Data_Check <- as.data.frame(Data_Test$y)
colnames(Data_Check) <- c("y")

Setup_Train <- subset(Setup_Train, select = -c(duration, year, mmyy, pdays, previous, nr.employed))

# Undersampling

Data_Yes <- subset(Setup_Train, Setup_Train$y == "yes")
Data_No <- subset(Setup_Train, Setup_Train$y == "no")

num <- nrow(Data_Yes)*us

Data_No <- Data_No[sample(nrow(Data_No)), ]
Data_No_US <- Data_No[c(1:num),]

# Final training sets

Data_Train <- Setup_Train
Data_Train_US <- rbind(Data_Yes, Data_No_US)

cat("Training set with all data has ", round(100*sum(Data_Train$y == "yes")/nrow(Data_Train),0), "% acceptance rate. \n", sep = "")
cat("Training set with undersampling has ", round(100*sum(Data_Train_US$y == "yes")/nrow(Data_Train_US),0), "% acceptance rate. \n", sep = "")

# Set up fold variable

Data_Train <- Data_Train[sample(nrow(Data_Train)), ]
Data_Train$fold <- rep(c(1:k), length.out = nrow(Data_Train))

Data_Train_US <- Data_Train_US[sample(nrow(Data_Train_US)), ]
Data_Train_US$fold <- rep(c(1:k), length.out = nrow(Data_Train_US))

#### Remove Temporary Files ####

rm(Setup_Train, Data_No, Data_No_US, Data_Yes)
rm(us, num, train_group, train_size)
