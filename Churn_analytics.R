setwd("/Users/rocket/Documents/BA/BA\ Additional/Files_26_11_2017/2_Churn_Analytics_Logistic_Regression")

library(ROCR)
# Read the data
logdata = read.csv(file = "ChurnCustData.csv")
summary(logdata)

colnames(logdata)
# Remove Brand_Conscious_Index variable
logdata1=logdata[,-17]

# Remove customers with Age over 100 
logdata<- quantile(logdata$Age, probs = c(.95,.96,.97,.98,.99,1))
logdata2 = subset(logdata1, Age<=100) # This will RETAIN customers with Age <= 100

# Remove rows with Age over 100 and below 18 years
logdata2 = subset(x=logdata1, subset = (Age<=100 & Age>=18)) # Double condition
logdata2 = logdata1[logdata1$Age<=100 & logdata1$Age>=18,] # Does the same thing as above

summary(logdata2)
# Average_value_per_Txn seems to have high Max value. Check the values
quantile(logdata2$Average_value_per_Txn, probs = c(0.75,.9,.95,.97,.99,.999,1))

# Use subset() and remove the rows with Amount per Transaction over 200
logdata3 = subset(x=logdata2, subset=Average_value_per_Txn<=200)

summary(logdata3)

# Alternately, you can do a ifelse() to cap the max value to 200

# Check for NAs
colSums(is.na(logdata3))
summary(logdata3$Income)

logdata3$Income[is.na(logdata3$Income)] = mean(logdata3$Income, na.rm = T)
logdata3$Income[is.na(logdata3$Income)] = 4565

# Create dummy variables for categorical variables
####################################################
# Create dummy flag variables for categorical variables
# 1. Find Factor columns and save it in a variable
# 2. Extract and store the column names where "Factor" is TRUE
# 3. Create the formula syntax by pasting the factor column names
# 4. Use model.matrix() function to create "DUMMY Variable Dataframe"
# 5. Combine the dummy df (REMOVE THE FIRST COLUMN - Intercept) with non-factor (numeric columns) df
str(logdata3)
#Extract Factor variables
Factor_vars = sapply(logdata3, is.factor)
Factor_vars

Factor_vars[Factor_vars == TRUE] # Gives you ONLY those columns which are factor. We will use this to find "Factor Columns"
Factor_Var_Names = names(Factor_vars[Factor_vars == TRUE])
Factor_Var_Names

# Create a dummy variable dataframe for all the factor variables
paste0(Factor_Var_Names, collapse = " + ") # You can write the entire formula manually as well

Dummy_Df = model.matrix(~ Region_cd + Occupation + Gender + Marital_status + 
                          Pymt_type + Loyality_card + Weekend_shopper + Frequent_shopper, 
                        data = logdata3)
colnames(Dummy_Df)

# Combine the dummy df (REMOVE THE FIRST COLUMN - Intercept) with non-factor (numeric column) df
LogData4 = cbind(logdata3[, !Factor_vars], Dummy_Df[,-1])
colnames(LogData4)
#Drop Customer ID
LogData5 = LogData4[,-1]

#Sampling: Train and Test
set.seed(777)
RowNumbers = sample(1:nrow(LogData5), 0.70*nrow(LogData5))
trainset = LogData5[RowNumbers,]
testset = LogData5[-RowNumbers,]

# Check the number of rows
nrow(trainset)
nrow(testset)

# Multicollinearity check (Remove variables having VIF > 10)
colnames(trainset) # Get the dependent variable name
library(car)

Vif_Check = lm(Responder ~ ., data = trainset)
sort(vif(Vif_Check), decreasing = TRUE)

# All variables look good

####################################################
# Modeling Iterations
####################################################

Logistic_M1 = glm(Responder ~ ., family=binomial, data=trainset)
summary(Logistic_M1)

Logistic_M2 = step(Logistic_M1)
summary(Logistic_M2)

Logistic_M3 = glm(Responder ~ Num_Cust_care_negative_exp + Num_Promotions_Sent + Num_Promotions_Used + Cust_satisfaction_score +Marital_statusOther + Marital_statusUnmarried, family = binomial, data = trainset)

M3 = glm(formula = Responder ~ Num_Cust_care_negative_exp + Num_Promotions_Sent + Num_Promotions_Used + Cust_satisfaction_score + Marital_statusOther + Marital_statusUnmarried, family = binomial, data = trainset)

####################################################
# Model Validation
####################################################

# Validation on Test dataset
Predicted_Probs = predict(Logistic_M2, testset, type = "response")
head(Predicted_Probs)
Predicted_Class = ifelse(Predicted_Probs >= 0.5, 1, 0)
head(Predicted_Class)

### Confusion Matrix or Misclassification Table
table(Predicted_Class, Actual = testset$Responder)

sum(diag(table(testset$Responder, 
               Predicted_Class)))/ nrow(testset)

