#### Decision Trees ####

### Functions ###

create_tree <- function(grp, DF, depth){
  Grp <- Depth <- Fold <- Cutoff <- NULL
  
  for (f in c(1:k)){
    Train <- subset(DF, DF$fold != f)
    Validate <- subset(DF, DF$fold == f)
    
    Train <- subset(Train, select = -c(fold))
    
    tree <- ctree(y ~ ., data = Train, control = ctree_control(maxdepth = depth))
    Validate$probs <- predict(tree, Validate, type = "p")
    best_cutoff <- best_g <- 0
    
    for (cutoff in seq(0, 1, by = 0.01)){
      pred <- NULL
      
      for (i in c(1:nrow(Validate))){
        if (cutoff < Validate$probs[[i]][2]){
          pred <- c(pred, "yes")
        }
        else {
          pred <- c(pred, "no")
        }
      }
      
      tmp <- data.frame(Validate$y, pred)
      colnames(tmp) <- c("y", "pred")
      
      TP <- sum(tmp$y == "yes" & tmp$pred == "yes")
      TN <- sum(tmp$y == "no" & tmp$pred == "no")
      FN <- sum(tmp$y == "yes" & tmp$pred == "no")
      FP <- sum(tmp$y == "no" & tmp$pred == "yes")
      
      TPR <- TP / (TP + FN)
      FPR <- FP / (TN + FP)
      
      g <- sqrt(TPR * (1 - FPR))
      
      if (best_g < g){
        best_cutoff <- cutoff
        best_g <- g
      }
    }
    
    Grp <- c(Grp, grp)
    Depth <- c(Depth, depth)
    Fold <- c(Fold, f)
    Cutoff <- c(Cutoff, best_cutoff)
  }
  
  Output <- data.frame(Grp, Depth, Fold, Cutoff)
  return(Output)
}

compare_tree <- function(DF, grp){
  Trees <- NULL
  
  for (d in c(1:16)){
    pred <- NULL
    
    tree <- ctree(y ~ ., data = subset(DF, select = -c(fold)), 
                  control = ctree_control(maxdepth = d))
    
    y <- Data_Test$y
    probs <- predict(tree, Data_Test, type = "p")
    cutoff <- mean(subset(Cutoffs, Cutoffs$Depth == d & Cutoffs$Grp == grp)$Cutoff)
    
    for (i in c(1:nrow(Data_Test))){
      if (cutoff < probs[[i]][2]){
        pred <- c(pred, "yes")
      }
      else {
        pred <- c(pred, "no")
      }
    }
    
    TP <- sum(y == "yes" & pred == "yes")
    TN <- sum(y == "no" & pred == "no")
    FN <- sum(y == "yes" & pred == "no")
    FP <- sum(y == "no" & pred == "yes")
    
    accuracy <- round(100*(TP + TN) / (TP + FN + FP + TN), 1)
    true_positive_rate <- round(100*TP / (TP + FN), 1)
    precision <- round(100*TP / (TP + FP), 1)

    y_auc <- ifelse(y == "yes", 1, 0)
    pred_auc <- ifelse(pred == "yes", 1, 0)
    auc <- round(auc(y_auc, pred_auc), 2)
    
    outcome <- c(grp, d, cutoff, accuracy, true_positive_rate, auc, precision)
    Trees <- rbind(Trees, outcome)
  }
  
  Trees <- data.frame(Trees)
  colnames(Trees) <- c("Grp", "Depth", "Cutoff", "Accuracy", "TPR", "AUC", "Precision")
  return(Trees)
}

### Determine Cutoffs ###

Cutoffs <- NULL
for (d in c(1:16)){
  Cutoffs <- rbind(Cutoffs, 
                   create_tree("All Data", Data_Train, d), 
                   create_tree("Undersample", Data_Train_US, d))
}
Cutoffs

### Create Trees ###

Outcome <- rbind(compare_tree(Data_Train, "All Data"), compare_tree(Data_Train_US, "Undersample"))
Outcome

### ROC Curves ###

# Top 3 accuracy
t1 <- ctree(y ~ ., data = subset(Data_Train, select = -c(fold)), control = ctree_control(maxdepth = 1))
t2 <- ctree(y ~ ., data = subset(Data_Train_US, select = -c(fold)), control = ctree_control(maxdepth = 1))
t3 <- ctree(y ~ ., data = subset(Data_Train_US, select = -c(fold)), control = ctree_control(maxdepth = 7))

# Top 3 TPR
t4 <- ctree(y ~ ., data = subset(Data_Train, select = -c(fold)), control = ctree_control(maxdepth = 2))
t5 <- ctree(y ~ ., data = subset(Data_Train, select = -c(fold)), control = ctree_control(maxdepth = 3))
t6 <- ctree(y ~ ., data = subset(Data_Train, select = -c(fold)), control = ctree_control(maxdepth = 4))

p1 <- predict(t1, Data_Test, type = "p")
p2 <- predict(t2, Data_Test, type = "p")
p3 <- predict(t3, Data_Test, type = "p")
p4 <- predict(t4, Data_Test, type = "p")
p5 <- predict(t5, Data_Test, type = "p")
p6 <- predict(t6, Data_Test, type = "p")

pred1 <- pred2 <- pred3 <- pred4 <- pred5 <- pred6 <- NULL
for (i in c(1:length(p1))){
  pred1 <- c(pred1, p1[[i]][2])
  pred2 <- c(pred2, p2[[i]][2])
  pred3 <- c(pred3, p3[[i]][2])
  pred4 <- c(pred4, p4[[i]][2])
  pred5 <- c(pred5, p5[[i]][2])
  pred6 <- c(pred6, p6[[i]][2])
}

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
  labs(title = "Decision Trees \nROC Curves", x = "False Positive Rate", y = "True Positive Rate") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.title = element_blank()) +
  scale_color_manual(values=c("red", "blue", "green", "orange", "purple", "yellow")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  guides(colour=guide_legend(nrow = 2, byrow = TRUE))

### Plot Trees ###

tree_accuracy <- ctree(y ~ ., data = subset(Data_Train, select = -c(fold)), control = ctree_control(maxdepth = 1))
plot(tree_accuracy, type = "simple", 
     inner_panel=node_inner(tree_accuracy, pval = FALSE),
     terminal_panel=node_terminal(tree_accuracy, digits = 3, fill = c("white")))

tree_tpr <- ctree(y ~ ., data = subset(Data_Train, select = -c(fold)), control = ctree_control(maxdepth = 2))
plot(tree_tpr, type = "simple", 
     inner_panel=node_inner(tree_tpr, pval = FALSE),
     terminal_panel=node_terminal(tree_tpr, digits = 3, fill = c("white")))

### Save final models and remove files ###

Temp <- subset(Outcome, as.numeric(as.character(Outcome$Depth)) < 9)

tree_type <- paste0(Temp$Grp, " - Depth ",Temp$Depth, sep  = "")
Final_DT <- cbind(rep("DT", nrow(Temp)), tree_type, subset(Temp, select = -c(Grp, Depth, Cutoff)))
colnames(Final_DT) <- c("Type", "Model", "Accuracy", "TP_Rate", "AUC", "Precision")
#keep(model_check, Data, Data_Test, Data_Train, Data_Train_US, k, Final_NB, Final_LR, Final_DT, sure = TRUE)
