# Task for Intern in Credit Risk Model Validation

**1 TASK**\
DATA MANIPULATION TASK\
Demonstrate your ability to:
- select random subsample of data set;
- filter desired rows using simple and more complex conditions;
- drop unnecessary variables, rename some variables;
- calculate summarizing statistics (for full sample and by categorical variables as well);
- create new variables using simple transformation and custom functions;
- order data set by several variables.

**2 TASK**\
DATA VISUALISATION TASK\
In order to understand the data please visualize it. \
You are free to select the scope, types of plots, etc.

**3 TASK**\
MODELLING TASK\
Perform a logistic regression to obtain the predicted probability that a customer has subscribed for a term deposit.\
Use continuous variables and dummy variables created for categorical columns. Not necessarily all variables provided in data sample should be used.\
Evaluate model goodness of fit and predictive ability. If needed, data set could be split into training and test sets.

**Solutions for 1 task**
- Select random subsample of data set:
```
random <- df[sample(nrow(df), size = 1000, replace = FALSE), ]
```
- Filter desired rows using simple and more complex conditions:
```
simple <- random %>% filter(marital == "married", housing == "yes")
```
```
complex <- random %>%
  filter( (age > 30 & balance > 1000) |     
      (marital == "married" & education == "tertiary")
  )
```
- Drop unnecessary variables, rename some variables:
```
variables <- df %>% 
  select(-marital, -contact)%>% 
  rename("Amžius" = "age",
         "Darbas" = "job" ,
         "Išsilavinimas" = "education")
```
- Calculate summarizing statistics (for full sample and by categorical variables as well):
```
summary(df)
```
```
summary_by_education <- df %>%
  group_by(education) %>%
  summarise(
    mean_age = mean(age),
    median_balance = median(balance),
    min_campaign = min(campaign),
    max_campaign = max(campaign)
  )
```
```
summary_by_job <- df %>%
  group_by(job) %>%
  summarise(
    mean_age = mean(age),
    median_balance = median(balance),
    min_campaign = min(campaign),
    max_campaign = max(campaign)
  )
```
- Create new variables using simple transformation and custom functions:
```
df$balance_status <- ifelse(df$balance < 0, "Negative balance", "Positive balance")
```
```
by_age <- function(age) {
  ifelse(age < 30, "Young",
         ifelse(age >= 30 & age < 60, "Middle-aged", "Elderly"))
}

df <- df %>%
  mutate( 
    age_group = by_age(age))
```
- Order data set by several variables:
```
sorted_df <- df %>%
  arrange(month,day, desc(age))
```
