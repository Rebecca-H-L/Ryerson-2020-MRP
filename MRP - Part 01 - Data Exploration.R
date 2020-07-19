#### Rebecca Lalonde - 500893771 ####
#### Data Science and Analytics - MRP ####
#### Data Exploration ####

Data <- read.csv("C:/Users/Rebecca/Documents/School/MRP/bank-additional-full.csv")
Overall_Rate <- sum(Data$y == 'yes')/nrow(Data)

cat("There are ", nrow(Data), " records.", sep = "")
cat("There are ", sum(Data$y == 'yes'), " acceptance.", sep = "")
cat("About ", round(100*sum(Data$y == 'yes')/nrow(Data),1), "% accept the offer.", sep = "")
cat("Assuming 'no' for all gives an accuracy of ", round(100*sum(rep("no", nrow(Data)) == Data$y)/nrow(Data),1), "%", sep = "")

#### Functions ####

# Find Acceptance Rate
find_rate <- function(DF){
  Rate_Count <- table(DF$Var, DF$y)
  if (is.numeric(DF$Var)){
    Rate <- data.frame(as.numeric(rownames(Rate_Count)), Rate_Count[,1], Rate_Count[,2])
  } else {
    Rate <- data.frame(rownames(Rate_Count), Rate_Count[,1], Rate_Count[,2])
  }
  
  colnames(Rate) <- c("Var", "No", "Yes")
  Rate$Total <- Rate$No + Rate$Yes
  Rate$Rate <- Rate$Yes/(Rate$Yes + Rate$No)
  
  return(Rate)
}

# Plot categorial variables by descending volume
plot_cat_desc <- function(name, title, a = 0, h = 0.5, v = 1){
  DF <- data.frame(get(name, Data), get("y", Data))
  colnames(DF) <- c("Var", "y")
  
  Rate <- find_rate(DF)
  
  p1 <- ggplot(DF, aes(x=reorder(Var, -table(Var)[Var]), y = 1)) +
    geom_bar(position="stack", stat="identity", color = "#1CADE4") +
    scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
    labs(title = "", x = "", y = "Count", fill = "") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = a, hjust = h, vjust = v))
  
  p2 <- ggplot(Rate, aes(x=reorder(Var, -Total), y = Rate)) +
    geom_bar(position="stack", stat="identity", fill = "#bddaf0") +
    scale_y_continuous(labels = scales::percent, limits = c(0,NA)) +
    labs(title = paste(title, "\nAcceptance Rate and Volume"), x = "", y = "Acceptance Rate", fill = "") +
    geom_hline(aes(yintercept=Overall_Rate), color="#1caec8", linetype="dashed", size=1) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = a, hjust = h, vjust = v))
  
  p3 <- grid.arrange(p2, p1, ncol = 1)
  return(p3)
}

# Plot categorical values by sorted factor
plot_cat_factor <- function(name, title, a = 0, h = 0.5, v = 1){
  DF <- data.frame(get(name, Data), get("y", Data))
  colnames(DF) <- c("Var", "y")
  
  Rate <- find_rate(DF)
  
  if (name == "month"){
    Rate$Var <- factor(Rate$Var, levels = month_levels)
  } else if (name == "day_of_week"){
    Rate$Var <- factor(Rate$Var, levels = day_levels)
  } else if (name == "mmyy"){
    Rate$Var <- factor(Rate$Var, levels = mmyy_levels)
  }
  
  p1 <- ggplot(DF, aes(x=as.factor(Var), y = 1)) +
    geom_bar(position="stack", stat="identity", color = "#1CADE4") +
    scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
    labs(title = "", x = "", y = "Count", fill = "") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5), legend.position="bottom", 
          axis.text.x = element_text(angle = a, hjust = h, vjust = v)) +
    scale_x_discrete(drop=FALSE)
  
  p2 <- ggplot(Rate, aes(x=as.factor(Var), y = Rate)) +
    geom_bar(position="stack", stat="identity", fill = "#bddaf0") +
    scale_y_continuous(labels = scales::percent, limits = c(0,NA)) +
    labs(title = paste(title, "\nAcceptance Rate and Volume"), x = "", y = "Acceptance Rate", fill = "") +
    geom_hline(aes(yintercept=Overall_Rate), color="#1caec8", linetype="dashed", size=1) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = a, hjust = h, vjust = v))
  
  p3 <- grid.arrange(p2, p1, ncol = 1)
  return(p3)
}

# Plot numeric values
plot_num <- function(name, title, a = 0, h = 0.5, v = 1){
  DF <- data.frame(get(name, Data), get("y", Data))
  colnames(DF) <- c("Var", "y")
  
  Rate <- find_rate(DF)
  
  p1 <- ggplot(DF, aes(x=Var)) + 
    geom_density(alpha = .5, fill="#1CADE4", aes(y = ..count..)) +
    scale_x_continuous(n.breaks = 10, expand = c(0, 0)) +
    scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
    labs(title = "", x = "", y = "Count", fill = "") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size=11), 
          axis.text.x = element_text(size = 11), axis.text.y = element_text(size=11))
  
  p2 <- ggplot(Rate, aes(x=Var, y = Rate, group = 1)) +
    geom_line(color="#1CADE4", size = 1) +
    scale_x_continuous(n.breaks = 10) +
    scale_y_continuous(labels = scales::percent, limits = c(0,NA)) +
    labs(title = paste(title, "\nAcceptance Rate and Volume"), x = "", y = "Acceptance Rate", fill = "") +
    geom_hline(aes(yintercept=Overall_Rate), color="#1caec8", linetype="dashed", size=1) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size=11), 
          axis.text.x = element_text(size = 11), axis.text.y = element_text(size=11))
  
  p3 <- grid.arrange(p2, p1, ncol = 1)
  return(p3)
}

# Plot values by the timeline
plot_timeline <- function(name, title){
  Data_Temp <- data.frame(get(name, Data), get("mmyy", Data))
  colnames(Data_Temp) <- c("Var", "mmyy")
  
  p <- ggplot(data=Data_Temp, aes(x=mmyy, y=Var, group=1)) +
    geom_line(color="#1CADE4", size = 1) +
    geom_point(color="#1CADE4", size = 3) +
    labs(title = title, x = "Month of Contact", y = "") +
    theme_light() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size=11), 
          axis.text.x = element_text(size = 11), axis.text.y = element_text(size=11)) +
    scale_x_discrete(drop=FALSE)
  
  return(p)
}

# One hot encode categorical values
encode <- function(name){
  New <- data.frame(get(name, Data))
  colnames(New) <- c("Var")
  
  List <- NULL
  Names <- NULL
  
  for (v in unique(New$Var)){
    tmp <- NULL
    for (i in c(1:nrow(New))){
      if (New$Var[i] == v){
        tmp <- c(tmp, 1)
      } else {
        tmp <- c(tmp, 0)
      }
    }
    assign(paste(name, "_", str_remove(v, "-"), sep = ""), tmp)
    List <- cbind(List, tmp)
    Names <- c(Names, paste(name, "_", str_remove(v, "-"), sep = ""))
  }
  
  colnames(List) <- Names
  return(List)
}

### Missing/Unknown Values ###

Name <- Count <- Percentage <- Unique_Val <- NULL

for (i in c(1:ncol(Data))){
  tmp <- Data[,i]
  Name <- c(Name, names(Data)[i])
  Unique_Val <- c(Unique_Val, length(unique(tmp)))
  ct <- sum(tmp == "unknown" | is.na(tmp))
  Count <- c(Count, ct)
  Percentage <- c(Percentage, round(100*ct/nrow(Data), 1))
}

Data_Smry <- data.frame(Name, Count, Percentage, Unique_Val)
Data_Smry

rm(i, tmp, Name, Count, Percentage, Unique_Val)

##### High level summary ####

summary(Data)

#### Sorting Factors ####

mmyy_levels <- c('may-08','jun-08','jul-08', 'aug-08','sep-08', 'oct-08','nov-08','dec-08',
                 'jan-09', 'feb-09', 'mar-09','apr-09','may-09','jun-09','jul-09','aug-09','sep-09','oct-09','nov-09','dec-09',
                 'jan-10', 'feb-10', 'mar-10','apr-10','may-10','jun-10','jul-10','aug-10','sep-10','oct-10','nov-10')
month_levels <- c("mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
day_levels <- c("mon","tue","wed","thu","fri")

Data$month <- factor(Data$month, levels = month_levels)
Data$day_of_week <- factor(Data$day_of_week, levels = day_levels)
Data$mmyy <- factor(Data$mmyy, levels = mmyy_levels)

#### Variable Distributions ####

## Setup ##

# Housing Loan, Personal Loan, Default
tmp1 <- data.frame("Housing Loan", Data$housing, Data$y)
tmp2 <- data.frame("Personal Loan", Data$loan, Data$y)
tmp3 <- data.frame("Default", Data$default, Data$y)
colnames(tmp1) <- colnames(tmp2) <- colnames(tmp3) <- c("Variable", "Response", "y")
Loan_Distribution <- rbind(tmp1, tmp2, tmp3)
Loan_Distribution$Response <- factor(Loan_Distribution$Response, levels = c("yes", "no", "unknown"))
rm(tmp1, tmp2, tmp3)

# Average Euribor
Euribor_Setup <- sqldf("select mmyy, euribor3m, count(*) as obs from Data group by 1, 2")
Euribor_Avg <- sqldf("select mmyy, sum(euribor3m)/count(*) as Avg_Euribor from Euribor_Setup group by 1")
Euribor_Avg$mmyy <- factor(Euribor_Avg$mmyy, levels = mmyy_levels)

# One Hot Encode
Age <- Data$age
Job <- encode("job")
Marital <- encode("marital")
Education <- encode("education")
Housing <- encode("housing")
Loan <- encode("loan")
Default <- encode("default")

Cor_Customer_Encode <- data.frame(Age, Job, Marital, Education, Housing, Loan, Default)
Cor_Customer_Plot <- melt(round(cor(Cor_Customer_Encode),2))
Cor_Customer_Plot$val2 <- with(Cor_Customer_Plot, ifelse(substr(Var1,1,3) == substr(Var2,1,3) & Var1 != Var2,NA,value))

## Customer Attributes ##

plot_num("age", "Age")
plot_cat_desc("job", "Job Title", 90, 1, 0.5)
plot_cat_desc("marital", "Marital Status")
plot_cat_desc("education", "Education Status", 90, 1, 0.5)

Loan_Yes <- sqldf("select Variable, Response, count(*) as Yes from Loan_Distribution where y = 'yes' group by 1, 2")
Loan_Count <- sqldf("select Variable, Response, count(*) as Count from Loan_Distribution group by 1, 2")
Rate <- sqldf("select a.Variable, a.Response, b.Yes as Yes, a.Count as Count from Loan_Count a left join Loan_Yes b on a.Variable = b.Variable and a.Response = b.Response")
Rate$Rate <- ifelse(is.na(Rate$Yes), 0, Rate$Yes/Rate$Count)

p1 <- ggplot(Loan_Distribution, aes(x = Response, y = 1)) + 
  geom_bar(stat = 'identity', position = 'stack', fill = "#1CADE4") + 
  facet_grid(~ Variable) +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
  labs(title = "", x = "", y = "Count", fill = "") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  scale_x_discrete(drop=FALSE)

p2 <- ggplot(Rate, aes(x=Response, y = Rate)) +
  geom_bar(position="stack", stat="identity", fill = "#bddaf0") +
  facet_grid(~ Variable) +
  scale_y_continuous(labels = scales::percent, limits = c(0,NA)) +
  labs(title = paste("Loan Statuses \nAcceptance Rate and Volume"), x = "", y = "Acceptance Rate", fill = "") +
  geom_hline(aes(yintercept=Overall_Rate), color="#1caec8", linetype="dashed", size=1) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1))

grid.arrange(p2, p1, ncol = 1)

## Campaign Attributes ##

plot_cat_desc("contact", "Contact Method", 0, 0.5)
plot_cat_factor("mmyy", "Month and Year of Contact", 90, 1, 0.5)
plot_cat_factor("day_of_week", "Day of Contact", 0, 0.5, 0.5)

plot_num("campaign", "Contact Count")
plot_cat_desc("poutcome", "Previous Campaign Outcome")

cat(round(100*sum(Data$pdays != 999)/nrow(Data),1), "% (", sum(Data$pdays != 999), ") of pdays is populated.", sep = "")
cat(round(100*sum(Data$previous != 0)/nrow(Data),1), "% (", sum(Data$previous != 0), ") of pdays is populated.", sep = "")

## Economic Attributes ##

plot_timeline("cons.price.idx", "Consumer Price Index - Updated Monthly \n Contact Month Timeline")
plot_timeline("cons.conf.idx", "Consumer Confidence Index - Updated Monthly \n Contact Month Timeline")
plot_timeline("nr.employed", "Number of Employees - Updated Quarterly \n Contact Month Timeline")
plot_timeline("emp.var.rate", "Employee Variation Rate - Updated Quarterly \n Contact Month Timeline")

ggplot(data=Euribor_Avg, aes(x=mmyy, y=Avg_Euribor, group=1)) +
  geom_line(color="#1CADE4", size = 1) +
  geom_point(color="#1CADE4", size = 3) +
  labs(title = "Euribor 3 Month Rate - Updated Daily - Monthly Average \n Contact Month Timeline", x = "Month of Contact", y = "") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=11), axis.text.x = element_text(size = 11), axis.text.y = element_text(size=11)) +
  scale_x_discrete(drop=FALSE)

### Correlation ###

## Customer ##

ggplot(data = Cor_Customer_Plot, aes(x=Var1, y=Var2, fill=val2)) + 
  geom_tile() +
  scale_fill_gradient(na.value = NA, limit = c(-1,1), low = "#e45b1c", high = "#1CADE4",
                      guide = guide_legend(direction = "horizontal", title.position = "top",label.position = "bottom")) +
  labs(title = "Correlation - Customer Metrics", x = "", y = "", fill = "Pearson's Coefficient") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_vline(xintercept = c(13.5, 17.5, 25.5, 28.5, 31.5), size = 0.5) +
  geom_hline(yintercept = c(13.5, 17.5, 25.5, 28.5, 31.5), size = 0.5) 

Cor_Customer <- with(Data, data.frame(job, marital, education, housing, loan, default))
Cor_Customer_Matrix <- matrix(data = NA, nrow = 6, ncol = 6)
for (i in c(1:6)){
  for (j in c(1:6)){
    Cor_Customer_Matrix[i,j] <- round(cramer(Cor_Customer[,i], Cor_Customer[,j])$value, 2)
  }
}
Cor_Customer_Matrix

## Campaign ##
Cor_Campaign <- Data[c("campaign", "previous", "pdays")]
round(cor(Cor_Campaign), 2)

## Economic ##
Cor_Economic <- Data[c("cons.conf.idx", "cons.price.idx", "emp.var.rate", "nr.employed", "euribor3m")]
round(cor(Cor_Economic),2)
