# Decision Tree

data_tmp <- read.csv("C:/Users/Rebecca/Documents/School/MRP/bank-additional-full.csv")
data_tmp$Weather <- as.factor(ifelse(data_tmp$poutcome == "success", "Sunny", ifelse(data_tmp$poutcome == "failure", "Cloudy", "Rainy")))
data_tmp$Temperature <- as.factor(ifelse(data_tmp$contact == "telephone", "Cool", "Warm"))

data_tree <- subset(data_tmp, select = c(y, Weather, Temperature))

tree <- ctree(y ~ ., data = data_tree, control = ctree_control(maxdepth = 2))
plot(tree, type = "simple", 
     inner_panel=node_inner(tree, pval = FALSE),
     terminal_panel=node_terminal(tree, digits = 3, fill = c("white")))

# SVM - Linear

x1 <- c(runif(19, 0, 2.5), 4, 1, runif(19, 3.5, 5))
x2 <- c(runif(19, 3.5, 5), 1.5, 4, runif(19, 0, 2.5))
y <- c(rep(1, 20), rep(2, 20))

data_linear <- data.frame(x1, x2, y = as.factor(y))
svm_linear <- svm(y ~ ., data_linear, kernel = "linear")

par(col.main='white')
plot(svm_linear, data_linear, col = c("lightblue", "pink"), symbolPalette = c("red", "blue"))
par(col.main='black')
title("SVM - Linear")

# SVM - Radial - Circle

x1 <- c(runif(10, 1.5, 3.5), runif(10, 0, 1), runif(10, 4, 5), runif(10, 1.5, 3.5))
x2 <- c(runif(10, 1.5, 3.5), runif(5, 0, 2), runif(5, 3, 5), runif(5, 0, 2), runif(5, 3, 5), runif(5, 0, 1), runif(5, 4, 5))
y <- c(rep(1, 10), rep(2, 30))

data_circle <- data.frame(x1, x2, y = as.factor(y))
svm_circle <- svm(y ~ ., data_circle, kernel = "radial")

par(col.main='white')
plot(svm_circle, data_circle, col = c("lightblue", "pink"), symbolPalette = c("red", "blue"))
par(col.main='black')
title("SVM - Non-Linear")

# SVM - Radial - Polynomial

x1 <- c(runif(20, 0, 4), runif(20, 1, 5))
x2 <- c(runif(20, 0, 4), runif(20, 1, 5))
y <- c(rep(1, 20), rep(2, 20))

data_poly <- data.frame(x1, x2, y = as.factor(y))
svm_poly <- svm(y ~ ., data_poly, kernel = "radial")

par(col.main='white')
plot(svm_poly, data_poly, col = c("lightblue", "pink"), symbolPalette = c("red", "blue"))
par(col.main='black')
title("SVM - Non-Linear")