
### Data ###

Train <- subset(Data_Train, select = -c(fold))
Train_US <- subset(Data_Train_US, select = -c(fold))

Test <- Data_Test

Models <- rbind(Final_NB, Final_LR, Final_DT, Final_SVM)
table(Models$Type)

### Find top ten accuracy ###

Acc_10 <-  head(Models[order(-as.numeric(as.character(Models$Accuracy))),], 10)
Acc_10

### Find top ten accuracy ###

Tpr_10 <-  head(Models[order(-as.numeric(as.character(Models$TP_Rate))),], 10)
Tpr_10

### Runtime - Model Creation ###

# Top ten accuracy

create_acc <- NULL
create_acc <- rbind(create_acc, system.time(m_acc_01 <- svm(y ~ ., data = Train, kernel = "radial", cost = 1, gamma = 0.1)))
create_acc <- rbind(create_acc, system.time(m_acc_02 <- svm(y ~ ., data = Train, kernel = "radial", cost = 100, gamma = 0.01)))

ptm <- proc.time()
var_select(Data_Train, "All Data")
m_acc_03 <- naiveBayes(y ~ ., data = subset(Data_Train, select = c(y, poutcome, euribor3m, loan)))
create_acc <- rbind(create_acc, proc.time() - ptm)

create_acc <- rbind(create_acc, system.time(m_acc_04 <- ctree(y ~ ., data = Train, control = ctree_control(maxdepth = 1))))
create_acc <- rbind(create_acc, system.time(m_acc_05 <- svm(y ~ ., data = Train, kernel = "radial", cost = 1, gamma = 0.01)))
create_acc <- rbind(create_acc, system.time(m_acc_06 <- svm(y ~ ., data = Train, kernel = "radial", cost = 10, gamma = 0.01)))
create_acc <- rbind(create_acc, system.time(m_acc_07 <- svm(y ~ ., data = Train, kernel = "radial", cost = 10, gamma = 0.1)))
create_acc <- rbind(create_acc, system.time(m_acc_08 <- svm(y ~ ., data = Train, kernel = "radial", cost = 100, gamma = 0.1)))
create_acc <- rbind(create_acc, system.time(m_acc_09 <- svm(y ~ ., data = Train_US, kernel = "radial", cost = 10, gamma = 0.01)))
create_acc <- rbind(create_acc, system.time(m_acc_10 <- svm(y ~ ., data = Train_US, kernel = "radial", cost = 1, gamma = 0.01)))
create_acc <- cbind(c(1:10), create_acc)
create_acc

# Top ten true positive rate

create_tpr <- NULL
create_tpr <- rbind(create_tpr, system.time(m_tpr_01 <- ctree(y ~ ., data = Train, control = ctree_control(maxdepth = 2))))
create_tpr <- rbind(create_tpr, system.time(m_tpr_02 <- ctree(y ~ ., data = Train, control = ctree_control(maxdepth = 3))))
create_tpr <- rbind(create_tpr, system.time(m_tpr_03 <- ctree(y ~ ., data = Train, control = ctree_control(maxdepth = 4))))
create_tpr <- rbind(create_tpr, system.time(m_tpr_04 <- naiveBayes(y ~ ., data = Train_US)))
create_tpr <- rbind(create_tpr, system.time(m_tpr_05 <- ctree(y ~ ., data = Train, control = ctree_control(maxdepth = 6))))
create_tpr <- rbind(create_tpr, system.time(m_tpr_06 <- ctree(y ~ ., data = Train, control = ctree_control(maxdepth = 8))))
create_tpr <- rbind(create_tpr, system.time(m_tpr_07 <- ctree(y ~ ., data = Train, control = ctree_control(maxdepth = 7))))
create_tpr <- rbind(create_tpr, system.time(m_tpr_08 <- stepAIC(glm(y ~ ., data = Train_US, family = binomial), direction = "both", trace = FALSE)))
create_tpr <- rbind(create_tpr, system.time(m_tpr_09 <- glm(y ~ ., data = Train, family = binomial)))
create_tpr <- rbind(create_tpr, system.time(m_tpr_10 <- stepAIC(glm(y ~ ., data = Train, family = binomial), direction = "both", trace = FALSE)))
create_tpr <- cbind(c(1:10), create_tpr)
create_tpr

### Runtime - Model Prediction ###

# Top ten accuracy

predict_acc <- NULL
predict_acc <- rbind(predict_acc, system.time(predict(m_acc_01, Test)))
predict_acc <- rbind(predict_acc, system.time(predict(m_acc_02, Test)))
predict_acc <- rbind(predict_acc, system.time(predict(m_acc_03, Test)))
predict_acc <- rbind(predict_acc, system.time(predict(m_acc_04, Test)))
predict_acc <- rbind(predict_acc, system.time(predict(m_acc_05, Test)))
predict_acc <- rbind(predict_acc, system.time(predict(m_acc_06, Test)))
predict_acc <- rbind(predict_acc, system.time(predict(m_acc_07, Test)))
predict_acc <- rbind(predict_acc, system.time(predict(m_acc_08, Test)))
predict_acc <- rbind(predict_acc, system.time(predict(m_acc_09, Test)))
predict_acc <- rbind(predict_acc, system.time(predict(m_acc_10, Test)))
predict_acc <- cbind(c(1:10), predict_acc)
predict_acc

# Top ten true positive rate

predict_tpr <- NULL
predict_tpr <- rbind(predict_tpr, system.time(predict(m_tpr_01, Test)))
predict_tpr <- rbind(predict_tpr, system.time(predict(m_tpr_02, Test)))
predict_tpr <- rbind(predict_tpr, system.time(predict(m_tpr_03, Test)))
predict_tpr <- rbind(predict_tpr, system.time(predict(m_tpr_04, Test)))
predict_tpr <- rbind(predict_tpr, system.time(predict(m_tpr_05, Test)))
predict_tpr <- rbind(predict_tpr, system.time(predict(m_tpr_06, Test)))
predict_tpr <- rbind(predict_tpr, system.time(predict(m_tpr_07, Test)))
predict_tpr <- rbind(predict_tpr, system.time(predict(m_tpr_08, Test)))
predict_tpr <- rbind(predict_tpr, system.time(predict(m_tpr_09, Test)))
predict_tpr <- rbind(predict_tpr, system.time(predict(m_tpr_10, Test)))
predict_tpr <- cbind(c(1:10), predict_tpr)
predict_tpr

### ROC Curves ###

p_acc_01 <- predict(m_acc_01, Test)
p_acc_02 <- predict(m_acc_02, Test)
p_acc_03 <- predict(m_acc_03, Test, type = "raw")[,2]

p1 <- predict(m_tpr_01, Test, type = "p")
p2 <- predict(m_tpr_02, Test, type = "p")
p3 <- predict(m_tpr_03, Test, type = "p")

p_tpr_01 <- p_tpr_02 <- p_tpr_03 <- NULL
for (i in c(1:length(p1))){
  p_tpr_01 <- c(p_tpr_01, p1[[i]][2])
  p_tpr_02 <- c(p_tpr_02, p2[[i]][2])
  p_tpr_03 <- c(p_tpr_03, p3[[i]][2])
}

model <- c(rep("#1 Accuracy", nrow(Data_Test)), rep("#2 Accuracy", nrow(Data_Test)), rep("#3 Accuracy", nrow(Data_Test)),
           rep("#1 TPR", nrow(Data_Test)), rep("#2 TPR", nrow(Data_Test)), rep("#3 TPR", nrow(Data_Test)))
y <- rep(Test$y, 6)
pred <- c(p_acc_01, p_acc_02, p_acc_03, p_tpr_01, p_tpr_02, p_tpr_03)

roc_data <- data.frame(model, y, pred)
roc_data$model <- factor(roc_data$model, levels = c("#1 Accuracy", "#2 Accuracy", "#3 Accuracy",
                                                    "#1 TPR", "#2 TPR", "#3 TPR"))

ggplot(roc_data, aes(d = y, m = pred, color = model)) + 
  geom_roc(n.cuts = 0) +
  geom_abline(slope = 1, intercept = 0, size = 1, linetype = "dashed", color = "grey") +
  labs(title = "Overall Best Accuracy and True Positive Rate Models \nROC Curves", x = "False Positive Rate", y = "True Positive Rate") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.title = element_blank()) +
  scale_color_manual(values=c("red", "blue", "green", "orange", "purple", "yellow")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  guides(colour=guide_legend(nrow = 2, byrow = TRUE))
