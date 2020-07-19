#### Logistic Regression ####

### Functions ###

# Find the best threshold by using the geometric mean
find_cutoff <- function(model, DF){
  
  Fold <- Cutoff <- NULL

  for (f in c(1:k)){
    print(f)

    if (model == "All"){ # All variables
      # Split test/train folds
      Train <- subset(DF, DF$fold != f)
      Validate <- subset(DF, DF$fold == f)
      # Identify target and create model
      y <- Validate$y
      m <- glm(y ~ ., data = subset(Train, select = -c(fold)), family = binomial)
    } else if (model == "Step"){ # Stepwise selection
      # Split test/train folds
      Train <- subset(DF, DF$fold != f)
      Validate <- subset(DF, DF$fold == f)
      # Identify target and create model
      y <- Validate$y
      m_tmp <- glm(y ~ ., data = subset(Train, select = -c(fold)), family = binomial)
      m <- stepAIC(m_tmp, direction = "both", trace = FALSE)
    } else if (model == "Lasso"){ # LASSO
      # Split test/train folds
      Train_Tmp <- subset(DF, DF$fold != f)
      Validate_Tmp <- subset(DF, DF$fold == f)
      Train <- model.matrix(y ~ ., subset(Train_Tmp, select = -c(fold)))
      Validate <- model.matrix(y ~ ., subset(Validate_Tmp, select = -c(fold)))
      # Identify test/train folds
      y <- Validate_Tmp$y
      y_glm <- ifelse(Train_Tmp$y == "yes", 1, 0)
      m <- cv.glmnet(Train, y_glm, alpha=1, family="binomial", type.measure = "mse")
    }
    
    # Determine the probability with model
    prob <- predict(m, Validate, type = "response")
    best_cutoff <- best_g <- 0
    
    for (cutoff in seq(0, 1, by = 0.01)){
      # Predict based on cutoff
      p <- ifelse(prob >= cutoff, "yes", "no")
      
      # Assign rates
      TP <- sum(y == "yes" & p == "yes")
      TN <- sum(y == "no" & p == "no")
      FN <- sum(y == "yes" & p == "no")
      FP <- sum(y == "no" & p == "yes")
      
      TPR <- TP / (TP + FN)
      FPR <- FP / (TN + FP)
      
      # Determine geometric mean
      g <- sqrt(TPR * (1 - FPR))
      
      # Assign best geometric mean
      if (best_g < g){
        best_cutoff <- cutoff
        best_g <- g
      }
    }
    
    Fold <- c(Fold, f)
    Cutoff <- c(Cutoff, best_cutoff)
  }
  
  Output <- data.frame(Fold, Cutoff)
  return(Output)
}

### Set up data for LASSO method - requires matrix ###

X_All <- model.matrix(y ~ ., subset(Data_Train, select = -c(fold)))
X_All_Fold <- model.matrix(y ~ ., Data_Train)
y_All <- ifelse(Data_Train$y == "yes", 1, 0)

X_US <- model.matrix(y ~ ., subset(Data_Train_US, select = -c(fold)))
X_US_Fold <- model.matrix(y ~ ., Data_Train_US)
y_US <- ifelse(Data_Train_US$y == "yes", 1, 0)

Setup_Test <- subset(Data_Test, select = -c(duration, year, mmyy, pdays, previous, nr.employed))
X_Test <- model.matrix(y ~ ., Setup_Test)

### Design Models ###

lr_all_1 <- glm(y ~ ., data = subset(Data_Train, select = -c(fold)), family = binomial)
lr_all_2 <- glm(y ~ ., data = subset(Data_Train_US, select = -c(fold)), family = binomial)

lr_step_1 <- stepAIC(lr_all_1, direction = "both", trace = FALSE)
lr_step_2 <- stepAIC(lr_all_2, direction = "both", trace = FALSE)

lr_lasso_1 <- cv.glmnet(X_All, y_All, alpha=1, family="binomial", type.measure = "mse")
lr_lasso_2 <- cv.glmnet(X_US, y_US, alpha=1, family="binomial", type.measure = "mse")

lr_lasso_1_vars <- data.frame(rownames(coef(lr_lasso_1)), coef(lr_lasso_1)[,1])
colnames(lr_lasso_1_vars) <- c("Var", "Coefficient")
lr_lasso_1_vars <- subset(lr_lasso_1_vars, lr_lasso_1_vars$Coefficient != 0)$Var

lr_lasso_2_vars <- data.frame(rownames(coef(lr_lasso_2)), coef(lr_lasso_2)[,1])
colnames(lr_lasso_2_vars) <- c("Var", "Coefficient")
lr_lasso_2_vars <- subset(lr_lasso_2_vars, lr_lasso_2_vars$Coefficient != 0)$Var

### Determine Cutoff ###

cutoff_lr_all_1 <- find_cutoff("All", Data_Train)
cutoff_lr_all_2 <- find_cutoff("All", Data_Train_US)

cutoff_lr_step_1 <- find_cutoff("Step", Data_Train)
cutoff_lr_step_2 <- find_cutoff("Step", Data_Train_US)

cutoff_lr_lasso_1 <- find_cutoff("Lasso", Data_Train)
cutoff_lr_lasso_2 <- find_cutoff("Lasso", Data_Train_US)

# Average across the folds is the cutoff
Cutoff <-c(mean(cutoff_lr_all_1$Cutoff), mean(cutoff_lr_all_2$Cutoff),
            mean(cutoff_lr_step_1$Cutoff), mean(cutoff_lr_step_2$Cutoff),
            mean(cutoff_lr_lasso_1$Cutoff), mean(cutoff_lr_lasso_2$Cutoff))
Cutoff

### Predictions ###

pred_lr_all_1 <- predict(lr_all_1, Data_Test, type = "response")
pred_lr_all_2 <- predict(lr_all_2, Data_Test, type = "response")
pred_lr_step_1 <- predict(lr_step_1, Data_Test, type = "response")
pred_lr_step_2 <- predict(lr_step_2, Data_Test, type = "response")
pred_lr_lasso_1 <- predict(lr_lasso_1, X_Test, type = "response")
pred_lr_lasso_2 <- predict(lr_lasso_2, X_Test, type = "response")

pred <- c(pred_lr_all_1, pred_lr_all_2, pred_lr_step_1, pred_lr_step_2, pred_lr_lasso_1, pred_lr_lasso_2)

pred_lr_all_1 <- ifelse(pred_lr_all_1 >= mean(cutoff_lr_all_1$Cutoff), "yes", "no")
pred_lr_all_2 <- ifelse(pred_lr_all_2 >= mean(cutoff_lr_all_2$Cutoff), "yes", "no")
pred_lr_step_1 <- ifelse(pred_lr_step_1 >= mean(cutoff_lr_step_1$Cutoff), "yes", "no")
pred_lr_step_2 <- ifelse(pred_lr_step_2 >= mean(cutoff_lr_step_2$Cutoff), "yes", "no")
pred_lr_lasso_1 <- ifelse(pred_lr_lasso_1 >= mean(cutoff_lr_lasso_1$Cutoff), "yes", "no")
pred_lr_lasso_2 <- ifelse(pred_lr_lasso_2 >= mean(cutoff_lr_lasso_2$Cutoff), "yes", "no")

Outcome <- rbind(
  model_check("All - All", pred_lr_all_1), model_check("Undersample - All", pred_lr_all_2),
  model_check("All - Stepwise", pred_lr_step_1), model_check("Undersample - Stepwise", pred_lr_step_2),
  model_check("All - Lasso", pred_lr_lasso_1), model_check("Undersample - Lasso", pred_lr_lasso_2)
)
Outcome

### Plot ROC Curves ###

len <- nrow(Data_Test)
type <- rep("LR", len*6)
model <- c(rep("All Data + All Variables", len), rep("Undersampled Data + All Variables", len), 
           rep("All Data + Stepwise Selection", len), rep("Undersampled Data + Stepwise Selection", len),
           rep("All Data + LASSO", len), rep("Undersampled Data + LASSO", len))
y <- rep(ifelse(Data_Test$y == "yes", 1, 0), 6)

roc_data <- data.frame(model, y, pred)
roc_data$model <- factor(roc_data$model, levels = c("All Data + All Variables", "Undersampled Data + All Variables",
                                                    "All Data + Stepwise Selection", "Undersampled Data + Stepwise Selection",
                                                    "All Data + LASSO", "Undersampled Data + LASSO"))

ggplot(roc_data, aes(d = y, m = pred, color = model)) + 
  geom_roc(n.cuts = 0) +
  geom_abline(slope = 1, intercept = 0, size = 1, linetype = "dashed", color = "grey") +
  labs(title = "Logistic Regression \nROC Curves", x = "False Positive Rate", y = "True Positive Rate") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.title = element_blank()) +
  scale_color_manual(values=c("red", "blue", "green", "orange", "purple", "yellow")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  guides(colour=guide_legend(nrow = 3, byrow = TRUE))

### Save final models and remove files ###

Final_LR <- cbind(rep("LR", nrow(Outcome)), Outcome)
colnames(Final_LR) <- c("Type", "Model", "Accuracy", "TP_Rate", "AUC", "Precision")
keep(var_select, model_check, Data, Data_Test, Data_Train, Data_Train_US, k, Final_NB, Final_LR, sure = TRUE)
