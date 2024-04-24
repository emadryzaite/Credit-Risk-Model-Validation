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

**Results for 2 task**

<img width="300" alt="Screenshot 2024-04-24 at 15 35 23" src="https://github.com/emadryzaite/Task-for-Intern-in-Credit-Risk-Model-Validation/assets/113093671/82093c3a-f729-4da6-9f84-f146f909251c">
<img width="300" alt="Screenshot 2024-04-24 at 15 35 41" src="https://github.com/emadryzaite/Task-for-Intern-in-Credit-Risk-Model-Validation/assets/113093671/5b12432e-c098-4b95-bcca-7edc0c9d3b48">
<img width="300" alt="Screenshot 2024-04-24 at 15 36 00" src="https://github.com/emadryzaite/Task-for-Intern-in-Credit-Risk-Model-Validation/assets/113093671/7f4246da-de12-4ddb-911c-6174b34b8640">
<img width="300" alt="Screenshot 2024-04-24 at 15 36 14" src="https://github.com/emadryzaite/Task-for-Intern-in-Credit-Risk-Model-Validation/assets/113093671/3d8ada6f-e3b3-40a7-8d0e-80405a9d9d23">
<img width="300" alt="Screenshot 2024-04-24 at 15 36 28" src="https://github.com/emadryzaite/Task-for-Intern-in-Credit-Risk-Model-Validation/assets/113093671/a4c38db1-2bc8-408d-ab19-f46635e07af5">

<img width="400" alt="Screenshot 2024-04-24 at 15 36 50" src="https://github.com/emadryzaite/Task-for-Intern-in-Credit-Risk-Model-Validation/assets/113093671/e1ac43b9-77c4-4ebe-85b5-18d9e03c6b6b">

<img width="400" alt="Screenshot 2024-04-24 at 15 37 01" src="https://github.com/emadryzaite/Task-for-Intern-in-Credit-Risk-Model-Validation/assets/113093671/6eafafdc-6612-4259-91c7-37edda2fae4f">
<img width="400" alt="Screenshot 2024-04-24 at 15 37 22" src="https://github.com/emadryzaite/Task-for-Intern-in-Credit-Risk-Model-Validation/assets/113093671/0f40e8cf-db42-4169-903b-405738b5882f">

**Results for 3 task**

Firstly, we splited data set into training and testing sets (80:20). Then changed y variable answers yes to 1 and no to 0, and checked if there is 20% proportion that is required for regression.
<img width="133" alt="Screenshot 2024-04-24 at 15 59 19" src="https://github.com/emadryzaite/Task-for-Intern-in-Credit-Risk-Model-Validation/assets/113093671/d592d4f6-f98c-41d0-b96b-e38a19a16af0">
As we can see in the table above it didn't meet the requirement. So we changed the data set so there would be atlest 20% of values. 
<img width="141" alt="Screenshot 2024-04-24 at 16 01 46" src="https://github.com/emadryzaite/Task-for-Intern-in-Credit-Risk-Model-Validation/assets/113093671/5098936a-06a7-499f-8e1e-1deb2e5358c6">

