library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)
library(MASS)
library(xlsx)

# [Moro et al., 2011] S. Moro, R. Laureano and P. Cortez. Using Data Mining for Bank Direct Marketing: An Application of the CRISP-DM Methodology. 
# In P. Novais et al. (Eds.), Proceedings of the European Simulation and Modelling Conference - ESM'2011, pp. 117-121, Guimarães, Portugal, October, 2011. EUROSIS.
# 
#   Available at: [pdf] http://hdl.handle.net/1822/14838
#                 [bib] http://www3.dsi.uminho.pt/pcortez/bib/2011-esm-1.txt

# 1 TASK --------------
   
# DATA MANIPULATION TASK
# Demonstrate your ability to:
# - select random subsample of data set;
# - filter desired rows using simple and more complex conditions;
# - drop unnecessary variables, rename some variables;
# - calculate summarizing statistics (for full sample and by categorical variables as well);
# - create new variables using simple transformation and custom functions;
# - order data set by several variables.
# Data: attached.

######### Data ############
df <- read.csv("bank-full.csv", sep = ";")
head(df)
str(df)

#Select random subsample of data set
random <- df[sample(nrow(df), size = 1000, replace = FALSE), ]
head(random)

#filter desired rows using simple and more complex conditions

# Simple conditions
simple <- random %>% filter(marital == "married", housing == "yes")
head(simple)

# Complex conditions
complex <- random %>%
  filter( (age > 30 & balance > 1000) |     
      (marital == "married" & education == "tertiary")
  )

head(complex)

#drop unnecessary variables, rename some variables;
variables <- df %>% 
  select(-marital, -contact)%>% 
  rename("Amžius" = "age",
         "Darbas" = "job" ,
         "Išsilavinimas" = "education")
head(variables)

#calculate summarizing statistics (for full sample and by categorical variables as well);
df$job<-as.factor(df$job)
df$marital<-as.factor(df$marital)
df$education<-as.factor(df$educatio)
df$housing<-as.factor(df$housing)
df$loan<-as.factor(df$loan)
df$contact<-as.factor(df$contact)
df$poutcome<-as.factor(df$poutcome)
df$y<-as.factor(df$y)
df$default<-as.factor(df$default)
df$month<-as.factor(df$month)

#for full sample
summary(df)

#categorical variables
summary_by_education <- df %>%
  group_by(education) %>%
  summarise(
    mean_age = mean(age),
    median_balance = median(balance),
    min_campaign = min(campaign),
    max_campaign = max(campaign)
  )

summary_by_education

summary_by_job <- df %>%
  group_by(job) %>%
  summarise(
    mean_age = mean(age),
    median_balance = median(balance),
    min_campaign = min(campaign),
    max_campaign = max(campaign)
  )
summary_by_job

#create new variables using simple transformation and custom functions;

# Simple
df$balance_status <- ifelse(df$balance < 0, "Negative balance", "Positive balance")
# Custom function

by_age <- function(age) {
  ifelse(age < 30, "Young",
         ifelse(age >= 30 & age < 60, "Middle-aged", "Elderly"))
}

df <- df %>%
  mutate( 
    age_group = by_age(age))
head(df)

#order data set by several variables
sorted_df <- df %>%
  arrange(month,day, desc(age))

print(sorted_df)



# 2 TASK ------------------------
#     DATA VISUALISATION TASK
# In order to understand the data please visualize it. 
# You are free to select the scope, types of plots, etc.

############ Histogram for age ##########
ggplot(df, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "#AFE1AF", color = "black") +
  labs(title = "Age frequency", x = "Age", y = "Frequency") 
  scale_x_continuous(breaks = seq(20, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 10000, by = 1000)) +
  theme(axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
panel.background = element_rect(fill = "transparent"))



# We can see that the most frequent age is from 30 to 40 
# And from 65 there are  less people in that age group

############# Median, Q1 and Q3 ############
plot_balance_summary <- function(indicator, df, group_column) {
  df[[group_column]] <- factor(df[[group_column]])
  
  summary_stats <- df %>%
    group_by(!!sym(group_column)) %>%
    summarize(
      Q1 = quantile(!!sym(indicator), 0.25),
      median = median(!!sym(indicator)),
      Q3 = quantile(!!sym(indicator), 0.75)
    ) 
  
  
  plot <- ggplot(summary_stats, aes_string(x = group_column, y = "median")) +
    geom_point(color = "blue", size = 5) +
    geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.5, size = 0.9, color = "black") +
    labs(title = paste(indicator, ": Median, Q1, Q3"),
         x = group_column,
         y = indicator) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14))
  
  return(plot)
}

plot_balance_summary("balance", df, "job")
# We can see that retired people had the largest median balance, 
# people with 'unknown' job - second largest;
# People with 'services' and 'entrepreneur' job titles 
# had the smallest median balance;

plot_balance_summary("balance", df, "education")
# People with tertiary education had the largest median balance, 
# and with secondary education - smallest median balance.

plot_balance_summary("balance", df, "age_group")
# By age group we can see that elderly people have largest balance 
# and young people have the smallest balance;

######## Correlation matrix #########
library(corrplot)

numerical_data <- df[, sapply(df, is.numeric)]
correlation_matrix <- cor(numerical_data)
corrplot(correlation_matrix, method = "number", type = "upper", tl.col = "black", col = colorRampPalette(c("red", "grey", "forestgreen"))(10))
# We can see that there is no strong correlation between variables;
# There is moderate positive correlation between pdays and previous;

###################
library(patchwork)

# Function for pie chart
create_binary_pie_chart <- function(df, binary_var) {
  
  proportions <- prop.table(table(df[[binary_var]]))
  
  proportions_df <- as.data.frame(proportions)
  colnames(proportions_df) <- c(binary_var, "Proportion")
  
  pie_chart <- ggplot(proportions_df, aes(x = "", y = Proportion, fill = factor(!!sym(binary_var)))) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    geom_text(aes(label = paste0(round(Proportion * 100), "%")), position = position_stack(vjust = 0.5), size = 5, color = "white") +  # Add rounded percentage labels
    coord_polar("y", start = 0) +
    labs(title = paste("Proportions of", binary_var),
         fill = binary_var) +
    theme_void()
  
  return(pie_chart)
}


# Function for bar chart
create_binary_bar_chart <- function(df, binary_var, categorical_var) {
  
  proportions <- prop.table(table(df[[categorical_var]], df[[binary_var]]), margin = 1)
  
  proportions_df <- as.data.frame(proportions)
  colnames(proportions_df) <- c(categorical_var, binary_var, "Proportion")
  
  stacked_bar_chart <- ggplot(proportions_df, aes(x = reorder(!!sym(categorical_var), Proportion), y = Proportion, fill = factor(!!sym(binary_var)))) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(Proportion * 100), "%")), position = position_stack(vjust = 0.5), size = 3, color = "white") +  # Add rounded percentage labels
    labs(title = paste("Proportions of", binary_var, "by", categorical_var),
         x = categorical_var,
         y = "Proportion",
         fill = binary_var) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(stacked_bar_chart)
}


# Housing and education
binary_variable <- "housing"
categorical_variable <- "education"

pie_chart <- create_binary_pie_chart(df, binary_variable)
bar_chart <- create_binary_bar_chart(df, binary_variable, categorical_variable)
grid_plot <- pie_chart + bar_chart
grid_plot

# We can see form the pie chart that 56% of people had a housing loan;
# From the bar chart we see that more people with secondary education 
# had a housing loan (61%) compared to other education levels;
# And least amount of people had house loans if there education is unknown;

# Loan and educatuion
binary_variable <- "loan"
categorical_variable <- "education"

pie_chart <- create_binary_pie_chart(df, binary_variable)
bar_chart <- create_binary_bar_chart(df, binary_variable, categorical_variable)
grid_plot <- pie_chart + bar_chart
grid_plot

# We can see in th epie chart that 84% of people had no loan;
# People with secondary education had more loans than others;
# 'Unknown' education level people have fewer loans than other education levels;



##################
# Function for non binary pie chart
create_non_binary_pie_chart <- function(df, categorical_var) {
  
  proportions <- prop.table(table(df[[categorical_var]]))
  
  proportions_df <- as.data.frame(proportions)
  colnames(proportions_df) <- c(categorical_var, "Proportion")
  
  pie_chart <- ggplot(proportions_df, aes(x = "", y = Proportion, fill = factor(!!sym(categorical_var)))) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    geom_text(aes(label = paste0(round(Proportion * 100), "%")), position = position_stack(vjust = 0.5), size = 5, color = "white") +  # Add rounded percentage labels
    coord_polar("y", start = 0) +
    labs(title = paste("Proportions of", categorical_var),
         fill = categorical_var) +
    theme_void()
  
  return(pie_chart)
}



# Job and education
binary_variable <- "job"
categorical_variable <- "education"

pie_chart <- create_non_binary_pie_chart(df, categorical_variable)
bar_chart <- create_binary_bar_chart(df, binary_variable, categorical_variable)
grid_plot <- pie_chart + bar_chart
grid_plot

# We can see from the pie chart that more than half (51%) people had secondary education level;
# The least amount (4%) of people had unknown education;
# From bar chart we can see that people with primary education most often had a blue-collar job (55%);
# With secondary education, people most often had blue-collar (23%) or technician (23%) jobs;
# With tertiary education more than half worked in management (59%) and least amount of people had blue-collar and housemaid jobs;
# With unknown education people are more evenly distributed, biggest proportion had blue-collar job (24%);
# The least amount of people were housemaids and unemployed;


# # 3 TASK
#       MODELLING TASK
# Perform a logistic regression to obtain the predicted probability that a customer has subscribed for a term deposit.
# Use continuous variables and dummy variables created for categorical columns. Not necessarily all variables provided in data sample should be used.
# Evaluate model goodness of fit and predictive ability. If needed, data set could be split into training and test sets.
# Data: attached (response variable y).

library(dplyr)
library(ggplot2)
library(car)
#install.packages("QuantPsyc")
library(QuantPsyc)
library(gridExtra)
library("writexl")
set.seed(100)

######### Data ############
duomenys <- read.csv("bank-full.csv", sep = ";")

sample <- sample(c(TRUE, FALSE), nrow(duomenys), replace=TRUE, prob=c(0.8,0.2))

train  <- duomenys[sample, ]
test   <- duomenys[!sample, ]

write_xlsx(test ,"test_data.xlsx")
write_xlsx(train ,"train_data.xlsx")


# Changing no to 0 and yes to 1
train$y <- ifelse(train$y == "no", 0, 1)
table(train$y)

test$y <- ifelse(test$y == "no", 0, 1)
table(test$y) 


train$job<-as.factor(train$job)
train$marital<-as.factor(train$marital)
train$education<-as.factor(train$education)
train$housing<-as.factor(train$housing)
train$loan<-as.factor(train$loan)
train$contact<-as.factor(train$contact)
train$poutcome<-as.factor(train$poutcome)
train$y<-as.factor(train$y)
train$default<-as.factor(train$default)
train$month<-as.factor(train$month)

head(train)

############ Initial analysis #############

# Checking if there is 20% proportion that is required for regression
frequency_proportion <- function(data, column_name) {
  column_name <- rlang::ensym(column_name)
  
  frequency_and_proportion <- data %>%
    group_by(!!column_name) %>%
    summarise(count = n(),
              percent = n() / nrow(data) * 100,
              .groups = 'drop')
  return(frequency_and_proportion)
}
print(frequency_proportion(train, y))
# There isn't


# Changing data set 
n_data <- train %>%
  filter(y == 0) %>%
  slice_sample(n = 15000, replace = FALSE)  

new_data <- bind_rows(n_data, train %>%
                             filter(y == 1))

# Checking for new data set
print(frequency_proportion(new_data, y))
train <- new_data

# Checking for outliers with cooks
plot(cooks.distance(model), pch = 16, col = "black", cex = 1,
     ylab = "Cook's Distance", main="Standardized Residuals")
# All values are < 1, there are no outliers

# Checking outliers with DFBETAS
dfbetas <- dfbetas(model)
influential_obs <- which(abs(dfbetas) > 1, arr.ind = TRUE)
influential_obs
# Also none
# Model
model <- glm(formula = y ~ age + job + marital + education + default +
               balance + housing + loan + contact + day + month + duration +
               campaign + pdays + previous + poutcome,
               family = binomial(logit), data = train)
summary(model)
# AIC: 12571

deviance_per_df <- sum(model$deviance)/model$df.residual
deviance_per_df
# 0.6462062


############## Model #############
# Checking if all regressors are significant
# H0: all regressors are insignificant
# H1: at least one regressor is significant 

model_rd<-glm(y~1, family=binomial(logit), data=train)
anova(model_rd, model, test="Chisq")
# We got that not all are insignificant 
# p < 0.05 we reject H0

ClassLog(model, train$y)
# McFadden is suitable, because > 0,2

stepAIC(model, direction = "both")
# Step:  AIC=12478.53
# y ~ job + marital + education + balance + housing + loan + contact + 
#   day + month + duration + campaign + previous + poutcome

stepAIC(model, direction = "backward")
# Step:  AIC=12478.53
# y ~ job + marital + education + balance + housing + loan + contact + 
#   day + month + duration + campaign + previous + poutcome
# Same results with both ways 

model_refined <- stepAIC(model, direction = "both")
summary(model_refined)
ClassLog(model_refined, train$y)
# McFadden is still > 0,2

# New model
a <- glm(formula = y ~ job + marital + education + balance + housing + loan + contact + 
              day + month + duration + campaign + previous + poutcome, family = binomial(logit), data = train)
summary(a)

# Checking multicollinearity
vif(a)
# All values are < 4, so there is no multicollinearity

exp(coef(a))
exp(0.05*coef(a))


############ Confusion matrix ########
#install.packages("pROC")
library(pROC)
############ Confusion matrix ########

library(QuantPsyc)
ClassLog(a, train$y)

TP <- 2287
FN <- 1941
FP <- 798  
TN <- 14206  


sensitivity <- TP / (TP + FN)
sensitivity
# 0.5409177
specificity <- TN / (TN + FP)
specificity
# 0.9468142
precision <- TP / (TP + FP)
precision
# 0.741329
negative_predictive <- TN / (TN + FN)
negative_predictive
# 0.8795196

accuracy <-(TP + TN )/(TP + FN + TN + FP)
accuracy

# Threshold = specificity + sensitivity -1 (Youden)
t <- specificity + sensitivity - 1
t
# 0.4859292

# For test set
test$job<-as.factor(test$job)
test$marital<-as.factor(test$marital)
test$education<-as.factor(test$education)
test$housing<-as.factor(test$housing)
test$loan<-as.factor(test$loan)
test$contact<-as.factor(test$contact)
test$poutcome<-as.factor(test$poutcome)
test$y<-as.factor(test$y)
test$default<-as.factor(test$default)
test$month<-as.factor(test$month)
predictTest <- predict(a, type = "response", newdata = test)
table(test$y,predictTest > 0.4859292)
TP <- 612
FN <- 449
FP <- 416  
TN <- 7486 
sensitivity <- TP / (TP + FN)
sensitivity
# 0.5837321
specificity <- TN / (TN + FP)
specificity
# 0.9473551
precision <- TP / (TP + FP)
precision
# 0.5953307
negative_predictive <- TN / (TN + FN)
negative_predictive
# 0.9434152

accuracy <-(TP + TN )/(TP + FN + TN + FP)
accuracy
# 0.9034921

f1<-(1+1)*(sensitivity*precision)/(1*precision+sensitivity)
f1
# 0.5859263

prognosis <- predict(a, test, type = "response")

test$y<-as.factor(test$y)
ROC_lr <- roc(test$y, prognosis)
ROC_lr_auc <- auc(ROC_lr)

plot(ROC_lr, col = "green", main = "ROC for logistic regression" )
# We also wanted to check ROC curve we can see that values are 
# close to 1 so the model is good at the diagnostic properties.

r_2<-1-a$deviance/a$null.deviance
r_2
# 0.3828816 model can still be improved
