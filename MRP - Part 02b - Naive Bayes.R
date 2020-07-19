#### Naive Bayes Classifier ####

### Functions ###

# Forward selection
var_select <- function(DF, title){
  # Set initial parameters
  model_list <- as.data.frame(DF$y)
  colnames(model_list) <- c("y")
  Train <- subset(DF, select = -c(y, fold))
  
  best_accuracy <- 0
  ct_var <- ncol(Train)
  var_list <- c(1:ct_var)
  
  while (TRUE){
    best_var <- 0
    
    # Across all variables that have not yet been added
    for (i in var_list){
      # Create and validate model with added variable
      tmp <- cbind(Train[i], model_list)
      m <- naiveBayes(y ~ ., data = tmp)
      v <- predict(m, DF)
      a <- sum(as.character(v) == as.character(DF$y))/nrow(Train)
      
      # If accuracy improves, set the current variable as the new best
      if (best_accuracy < a){
        best_accuracy <- a
        best_var <- i
      }
    }
    
    # If no added variable improves accuracy or all variables have been selected, stop the selection
    if (best_var == 0 | is.null(var_list)){
      break
    }
    
    # Add the best variable to the list
    model_list <- cbind(model_list, Train[best_var])
    var_list <- var_list[!var_list %in% best_var]
  }
  
  # Note final accuracy, captured positives, and variable list
  acc <- round(best_accuracy, 2)
  pos <- round(sum(v == "yes" & DF$y == "yes")/sum(DF$y == "yes"), 2)
  vars <- paste(names(model_list), collapse = " ")
  t <- c(title, acc, pos, vars)
  
  return(t)
}

### Create Model - All Variables ###

nb_all_1 <- naiveBayes(y ~ ., data = subset(Data_Train, select = -c(fold)))
nb_all_2 <- naiveBayes(y ~ ., data = subset(Data_Train_US, select = -c(fold)))

### Create Model - Forward Selection ###

# Determine variables to use
vars_1 <- var_select(Data_Train, "All Data")
vars_2 <- var_select(Data_Train_US, "Undersample")

# Create models
nb_forward_1 <- naiveBayes(y ~ ., data = subset(Data_Train, select = c(y, poutcome, euribor3m, loan)))
nb_forward_2 <- naiveBayes(y ~ ., data = subset(Data_Train_US, select = c(y, poutcome, month, euribor3m)))

### Model Outcomes ###

pred_nb_all_1 <- predict(nb_all_1, Data_Test)
pred_nb_all_2 <- predict(nb_all_2, Data_Test)
pred_nb_forward_1 <- predict(nb_forward_1, Data_Test)
pred_nb_forward_2 <- predict(nb_forward_2, Data_Test)

Outcome <- rbind(
  model_check("All Data + All Variables", pred_nb_all_1),
  model_check("Undersampled Data + All Variables", pred_nb_all_2),
  model_check("All Data + Forward Selection", pred_nb_forward_1),
  model_check("Undersampled Data + Forward Selection", pred_nb_forward_2)
)
Outcome

### Plot ROC ###

roc_nb_all_1 <- predict(nb_all_1, Data_Test, type = "raw")[,2]
roc_nb_all_2 <- predict(nb_all_2, Data_Test, type = "raw")[,2]
roc_nb_forward_1 <- predict(nb_forward_1, Data_Test, type = "raw")[,2]
roc_nb_forward_2 <- predict(nb_forward_2, Data_Test, type = "raw")[,2]

len <- nrow(Data_Test)
model <- c(rep("All Data + All Variables", len), rep("Undersampled Data + All Variables", len), 
           rep("All Data + Forward Selection", len), rep("Undersampled Data + Forward Selection", len))
y <- rep(ifelse(Data_Test$y == "yes", 1, 0), 4)
pred <- c(roc_nb_all_1, roc_nb_all_2, roc_nb_forward_1, roc_nb_forward_2)

roc_data <- data.frame(type, model, y, pred)
roc_data$model <- factor(roc_data$model, levels = c("All Data + All Variables", "Undersampled Data + All Variables",
                                                    "All Data + Forward Selection", "Undersampled Data + Forward Selection"))

ggplot(roc_data, aes(d = y, m = pred, color = model)) + 
  geom_roc(n.cuts = 0) +
  geom_abline(slope = 1, intercept = 0, size = 1, linetype = "dashed", color = "grey") +
  labs(title = "Naive Bayes Classifier \nROC Curves", x = "False Positive Rate", y = "True Positive Rate") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.title = element_blank()) +
  scale_color_manual(values=c("red", "blue", "green", "orange")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  guides(colour=guide_legend(nrow=2, byrow = TRUE))

### Save final models and remove files ###

Final_NB <- cbind(rep("NB", nrow(Outcome)), Outcome)
colnames(Final_NB) <- c("Type", "Model", "Accuracy", "TP_Rate", "AUC", "Precision")
keep(model_check, Data, Data_Test, Data_Train, Data_Train_US, k, Final_NB, sure = TRUE)
