#### Support Vector Machine ####

### Functions ###

svm_train <- function(DF){
  Fold <- Cost <- Gamma <- Accuracy <- Positives <- NULL
  
  for (f in c(1:5)){
    Train <- subset(DF, DF$fold != f)
    Validate <- subset(DF, DF$fold == f)
    y <- Validate$y
    
    for (c in c(-2:2)){
      cost <- 10^c
      for (g in c(-2:2)){
        print(paste(f, c, g, sep = " "))
        gamma <- 10^g
        m_radial <- svm(y ~ ., data = subset(Train, select = -c(fold)), kernel = "radial", cost = cost, gamma = gamma)
        p_radial <- predict(m_radial, Validate)
        acc <- round(100*sum(y == p_radial)/nrow(Validate),1)
        pos <- round(100*sum(y == "yes" & p_radial == "yes")/sum(y == "yes"), 1)
        Fold <- c(Fold, f)
        Cost <- c(Cost, cost)
        Gamma <- c(Gamma, gamma)
        Accuracy <- c(Accuracy, acc)
        Positives <- c(Positives, pos)
      }
    }
  }
  
  Output <- data.frame(Fold, Cost, Gamma, Accuracy, Positives)
  return(Output)
}

svm_plot <- function(DF){
  p1 <- ggplot(DF, aes(as.factor(Cost), as.factor(Gamma), fill= Accuracy)) + 
    geom_tile() +
    labs(title = "Accuracy", x = "Cost", y = "Gamma") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
  
  p2 <- ggplot(DF, aes(as.factor(Cost), as.factor(Gamma), fill= Positives)) + 
    geom_tile() +
    labs(title = "Captured Positives", x = "Cost", y = "Gamma") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
  
  p3 <- grid.arrange(p1, p2, nrow = 1)
  return(p3)
}

Train_SVM <- subset(Data_Train, Data_Train$fold < 6)
Train_SVM_US <- subset(Data_Train_US, Data_Train_US$fold < 6)

SVM_Output <- svm_train(Train_SVM)
SVM_Smry <- sqldf("select Cost, Gamma, sum(Accuracy)/count(*) as Accuracy, sum(Positives)/count(*) as Positives from SVM_Output group by 1, 2")

SVM_Output_US <- svm_train(Train_SVM_US)
SVM_Smry_US <- sqldf("select Cost, Gamma, sum(Accuracy)/count(*) as Accuracy, sum(Positives)/count(*) as Positives from SVM_Output_US group by 1, 2")

svm_plot(SVM_Smry)
svm_plot(SVM_Smry_US)

### Compare Models ###

Cost_All <- c(1, 10, 100)
Gamma_All <- c(0.01, 0.1)
Outcome_All <- NULL
for (cost in Cost_All){
  for (gamma in Gamma_All){
    m <- svm(y ~ ., data = subset(Data_Train, select = -c(fold)), kernel = "radial", cost = cost, gamma = gamma)
    p <- predict(m, Data_Test)
    chk <- model_check(paste("All Data - Cost = ", cost, " - Gamma = ", gamma, sep = ""), p)
    Outcome_All <- rbind(Outcome_All, chk)
  }
}

Cost_US <- c(0.1, 1, 10, 100)
Gamma_US <- c(0.01, 0.1)
Outcome_US <- NULL
for (cost in Cost_US){
  for (gamma in Gamma_US){
    m <- svm(y ~ ., data = subset(Data_Train_US, select = -c(fold)), kernel = "radial", cost = cost, gamma = gamma)
    p <- predict(m, Data_Test)
    chk <- model_check(paste("Undersampled Data - Cost = ", cost, " - Gamma = ", gamma, sep = ""), p)
    Outcome_US <- rbind(Outcome_US, chk)
  }
}

Outcome <- rbind(Outcome_All, Outcome_US)

### ROC Curves ###

# Top 3 Accuracy
m1 <- svm(y ~ ., data = subset(Data_Train, select = -c(fold)), kernel = "radial", cost = 1, gamma = 0.1)
m2 <- svm(y ~ ., data = subset(Data_Train, select = -c(fold)), kernel = "radial", cost = 100, gamma = 0.01)
m3 <- svm(y ~ ., data = subset(Data_Train, select = -c(fold)), kernel = "radial", cost = 1, gamma = 0.01)

# Top 3 TPR
m4 <- svm(y ~ ., data = subset(Data_Train_US, select = -c(fold)), kernel = "radial", cost = 1, gamma = 0.1)
m5 <- svm(y ~ ., data = subset(Data_Train_US, select = -c(fold)), kernel = "radial", cost = 100, gamma = 0.01)
m6 <- svm(y ~ ., data = subset(Data_Train_US, select = -c(fold)), kernel = "radial", cost = 100, gamma = 0.1)

pred1 <- predict(m1, Data_Test)
pred2 <- predict(m2, Data_Test)
pred3 <- predict(m3, Data_Test)
pred4 <- predict(m4, Data_Test)
pred5 <- predict(m5, Data_Test)
pred6 <- predict(m6, Data_Test)

model <- c(rep("#1 Accuracy", nrow(Data_Test)), rep("#2 Accuracy", nrow(Data_Test)), rep("#3 Accuracy", nrow(Data_Test)),
           rep("#1 TPR", nrow(Data_Test)), rep("#2 TPR", nrow(Data_Test)), rep("#3 TPR", nrow(Data_Test)))
y <- rep(Data_Test$y, 6)
pred <- c(pred1, pred2, pred3, pred4, pred5, pred6)

roc_data <- data.frame(model, y, pred)
roc_data$model <- factor(roc_data$model, levels = c("#1 Accuracy", "#2 Accuracy", "#3 Accuracy",
                                                    "#1 TPR", "#2 TPR", "#3 TPR"))

ggplot(roc_data, aes(d = y, m = pred, color = model)) + 
  geom_roc(n.cuts = 0) +
  geom_abline(slope = 1, intercept = 0, size = 1, linetype = "dashed", color = "grey") +
  labs(title = "Support Vector Machine \nROC Curves", x = "False Positive Rate", y = "True Positive Rate") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.title = element_blank()) +
  scale_color_manual(values=c("red", "blue", "green", "orange", "purple", "yellow")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  guides(colour=guide_legend(nrow = 2, byrow = TRUE))

### Save final models and remove files ###

Final_SVM <- cbind(rep("SVM", nrow(Outcome)), Outcome)
colnames(Final_SVM) <- c("Type", "Model", "Accuracy", "TP_Rate", "AUC", "Precision")
keep(var_select, model_check, Data, Data_Test, Data_Train, Data_Train_US, k, Final_NB, Final_LR, Final_DT, Final_SVM, sure = TRUE)
