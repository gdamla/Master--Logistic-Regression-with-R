# ADULT INCOME DATA / LOGISTIC REGRESSION MODEL
# Gülhan Damla Aþýk - 2000136

getwd()
setwd("C:/Users/user/Desktop/BAU Lessons/2- Marketing Analysis/Hands On Exercises/2- Adult Income Dataset R_glm")

# READING # data
AdultIncomeData <- read.csv("adult_income.csv", header =T)

library(dplyr)
library(ggplot2)
library(gplots)
library(gapminder)
library(reshape2)
library(tidyverse)
install.packages("moments")   #Skewness and Kurtosis
library(moments)
install.packages("caTools")   #Split data
library(caTools)  
library(rlang)
install.packages("GGally")
library(GGally)
install.packages("ROSE")     # imbalanced data
library(ROSE)
install.packages("caret")   # Variable importance test
library(caret)


# UNDERSTANDING # data

str(AdultIncomeData)
AdultIncomeData$age <- as.numeric(AdultIncomeData$age)
AdultIncomeData$education_num <- as.factor(AdultIncomeData$education_num)
AdultIncomeData$workclass <- as.factor(AdultIncomeData$workclass)
AdultIncomeData$marital_status <- as.factor(AdultIncomeData$marital_status)
AdultIncomeData$occupation <- as.factor(AdultIncomeData$occupation)
AdultIncomeData$race <- as.factor(AdultIncomeData$race)
AdultIncomeData$sex <- as.factor(AdultIncomeData$sex)
AdultIncomeData$native_country <- as.factor(AdultIncomeData$native_country)
AdultIncomeData$income_high <- as.factor(AdultIncomeData$income_high)
AdultIncomeData$capital_gain <- as.numeric(AdultIncomeData$capital_gain)
AdultIncomeData$capital_loss <- as.numeric(AdultIncomeData$capital_loss)
AdultIncomeData$hours_per_week <- as.numeric(AdultIncomeData$hours_per_week)

AdultIncomeData <- AdultIncomeData[c("age", "workclass", "education_num","marital_status", "occupation", "race", "sex","capital_gain","capital_loss", "hours_per_week", "native_country", "income_high")]
head(AdultIncomeData)
# drop ID
str(AdultIncomeData)
summary(AdultIncomeData)

ggpairs(AdultIncomeData5x[c("age", "capital_loss" , "capital_gain", "hours_per_week", "workclass", "marital_status", "occupation", "race", "sex", "income_high")])

attributes(AdultIncomeData)
head(AdultIncomeData)
glimpse(AdultIncomeData)

unique(AdultIncomeData$age)
table(AdultIncomeData$age)
# No NULL

unique(AdultIncomeData$workclass)
# " State-gov"        " Self-emp-not-inc" " Private"          " Federal-gov"      " Local-gov"        " ?"               
# " Self-emp-inc"     " Without-pay"      " Never-worked"
table(AdultIncomeData$workclass)
# number of 1836 NULL data exist
unique(AdultIncomeData$education_num)
table(AdultIncomeData$education_num)
# No NULL
unique(AdultIncomeData$marital_status)
table(AdultIncomeData$marital_status)
# No NULL 
unique(AdultIncomeData$occupation)
# " Adm-clerical"      " Exec-managerial"   " Handlers-cleaners" " Prof-specialty"    " Other-service"    
# " Sales"             " Craft-repair"      " Transport-moving"  " Farming-fishing"   " Machine-op-inspct"
# " Tech-support"      " ?"                 " Protective-serv"   " Armed-Forces"      " Priv-house-serv"  
table(AdultIncomeData$occupation)
# number of 1843 NULL data exist
unique(AdultIncomeData$race)
table(AdultIncomeData$race)
# No NULL 
unique(AdultIncomeData$sex)
table(AdultIncomeData$sex)
# No NULL 
unique(AdultIncomeData$capital_gain)
table(AdultIncomeData$capital_gain)
# No NULL
unique(AdultIncomeData$capital_loss)
table(AdultIncomeData$capital_loss)
# No NULL
unique(AdultIncomeData$hours_per_week)
table(AdultIncomeData$hours_per_week)
# No NULL
unique(AdultIncomeData$native_country)
table(AdultIncomeData$native_country)
# number of 583 NULL data exist
unique(AdultIncomeData$income_high)
table(AdultIncomeData$income_high)
# No NULL

ggplot(AdultIncomeData, aes(x = income_high , fill = workclass )) +geom_bar(position="dodge")
ggplot(AdultIncomeData, aes(x = age ,fill = income_high)) +geom_histogram(position="identity")
ggplot(AdultIncomeData, aes(x = education_num ,fill = income_high)) +geom_histogram(position="identity")
ggplot(AdultIncomeData, aes(x = income_high , fill = marital_status )) +geom_bar(position="dodge")
ggplot(AdultIncomeData, aes(x = income_high , fill = occupation )) +geom_bar(position="dodge")
ggplot(AdultIncomeData, aes(x = income_high , fill = race )) +geom_bar(position="dodge")
ggplot(AdultIncomeData, aes(x = income_high , fill = sex )) +geom_bar(position="dodge")
ggplot(AdultIncomeData, aes(x = capital_gain ,fill = income_high)) +geom_histogram(position="dodge")
ggplot(AdultIncomeData, aes(x = capital_loss ,fill = income_high)) +geom_histogram(position="dodge")
ggplot(AdultIncomeData, aes(x = hours_per_week ,fill = income_high)) +geom_histogram(position="identity")

## NULL VALUES
is.null(AdultIncomeData)
# Returns FALSE. NULL values "?"

sum(complete.cases(AdultIncomeData))
# 32561 
# complete.cases() returns a logical vector [TRUE or FALSE] indicating if any observations are NA for any rows.Since my data has no NA (but ?), it returns the number of rows.


AdultIncomeData %>% filter(workclass == " ?") %>%
  group_by(workclass, occupation) %>% count(workclass,occupation)
# All ? data in workclass is also ? in occupation
AdultIncomeData %>% filter(occupation == " ?") %>%
  group_by(workclass, occupation) %>% count(workclass,occupation)
# Distribution of occupation ? data
# 1 " ?"            " ?"        1836
# 2 " Never-worked" " ?"           7
# We can remove all occupation == ? rows from AdultIncomeData

glimpse(AdultIncomeData)
# Rows: 32,561
AdultIncomeData2 <- subset(AdultIncomeData, occupation != " ?" & native_country != " ?")
table(AdultIncomeData2$occupation)
table(AdultIncomeData2$workclass)
table(AdultIncomeData2$native_country)
# removed ? from occupation and workclass and native_country
glimpse(AdultIncomeData2)
# Rows: 30,162

str(AdultIncomeData2)

# correlation heatmap
cormat <- round(cor(AdultIncomeData2[c("age", "capital_loss" , "capital_gain", "hours_per_week")]),2)
head(cormat)
melted_cormat <- melt(cormat)
head(melted_cormat)
ggplot(data = melted_cormat, aes(x =Var1 , y =Var2, fill=value )) + 
  geom_tile(color="white") + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white" ,midpoint = 0, limit = c(-1,1), space = "Lab" , name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + 
  geom_text(aes(label = value), color = "black" , size = 4) +
  coord_fixed()
# no significant correlation


# OUTLIERS
str(AdultIncomeData)
summary(AdultIncomeData2)
# plot outliers method 1
boxplot(AdultIncomeData2$age, main= "age" )
boxplot(AdultIncomeData2$capital_gain  , main= "capital_gain")
boxplot(AdultIncomeData2$capital_loss  , main= "capital_loss")
boxplot(AdultIncomeData2$hours_per_week, main= "hours_per_week")

# plot outliers method 2
ggplot(AdultIncomeData2, aes(income_high, age)) +
  geom_boxplot() +
  coord_flip()
ggplot(AdultIncomeData2, aes(income_high, capital_gain)) +
  geom_boxplot() +
  coord_flip()
ggplot(AdultIncomeData2, aes(income_high, capital_loss)) +
  geom_boxplot() +
  coord_flip()
ggplot(AdultIncomeData2, aes(income_high, hours_per_week)) +
  geom_boxplot() +
  coord_flip()

str(AdultIncomeData2)

AdultIncomeData2x <- AdultIncomeData2[AdultIncomeData2$age < 76,]
table(AdultIncomeData2x$age)
boxplot(AdultIncomeData2x$age, main= "age" )
str(AdultIncomeData2x)

AdultIncomeData3x <- AdultIncomeData2x[AdultIncomeData2x$capital_gain < 15025,]
table(AdultIncomeData3x$capital_gain)
boxplot(AdultIncomeData3x$capital_gain, main= "capital_gain" )
ggplot(AdultIncomeData3x, aes(income_high, capital_gain)) +
  geom_boxplot() +
  coord_flip()
str(AdultIncomeData3x)

AdultIncomeData4x <- AdultIncomeData3x[AdultIncomeData3x$capital_loss < 2600 ,]
table(AdultIncomeData4x$capital_loss)
boxplot(AdultIncomeData4x$capital_loss, main= "capital_loss" )
ggplot(AdultIncomeData4x, aes(income_high, capital_loss)) +
  geom_boxplot() +
  coord_flip()
str(AdultIncomeData4x)

AdultIncomeData5x <- AdultIncomeData4x[AdultIncomeData4x$hours_per_week < 71,]
table(AdultIncomeData4x$hours_per_week)
boxplot(AdultIncomeData5x$hours_per_week, main= "hours_per_week" )
ggplot(AdultIncomeData5x, aes(income_high, hours_per_week)) +
  geom_boxplot() +
  coord_flip()
str(AdultIncomeData5x)
# 29291 obs. of  12 variables

cor(AdultIncomeData5x[c("age", "capital_loss" , "capital_gain", "hours_per_week")])
#                 age       capital_loss  capital_gain  hours_per_week
# age            1.00000000   0.06139668   0.10643316     0.12780477
# capital_loss   0.06139668   1.00000000  -0.05258579     0.05999767
# capital_gain   0.10643316  -0.05258579   1.00000000     0.09981533
# hours_per_week 0.12780477   0.05999767   0.09981533     1.00000000

# check correlation heatmap again 
cormat2 <- round(cor(AdultIncomeData5x[c("age", "capital_loss" , "capital_gain", "hours_per_week")]),2)
head(cormat2)
melted_cormat2 <- melt(cormat2)
head(melted_cormat2)
ggplot(data = melted_cormat2, aes(x =Var1 , y =Var2, fill=value )) + 
  geom_tile(color="white") + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white" ,midpoint = 0, limit = c(-1,1), space = "Lab" , name="Pearson\nCorrelation2") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + 
  geom_text(aes(label = value), color = "black" , size = 4) +
  coord_fixed()
# no significant correlation 


# MODELLING 
# Split Data
summary(AdultIncomeData5x)
table(AdultIncomeData5x$income_high)
#    No   Yes 
# 22200  7091

# create training data
input_ones <- AdultIncomeData5x[which(AdultIncomeData5x$income_high == "No"), ] #all No's
input_zeros <- AdultIncomeData5x[which(AdultIncomeData5x$income_high == "Yes"), ] # all Yes's
set.seed(100) # for repeatability of sample
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7 * nrow(input_ones)) #No's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7 * nrow(input_zeros)) #Yes's for training
#pick as many as No's and Yes's
training_ones <- input_ones[input_ones_training_rows, ]
training_zeros <- input_zeros[input_zeros_training_rows, ]
#row bind
trainingData <- rbind(training_ones, training_zeros)
dim(trainingData)
# 20502 12
table(trainingData$income_high)
#    No   Yes 
# 15539  4963 

#create test data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
#row bind
testData <- rbind(test_ones, test_zeros)
dim(testData)
#  8789   12
table(testData$income_high)
#    No   Yes 
#  6661 2128

# MODEL 1
logitMod1 <- glm(income_high ~., data = trainingData, family = binomial(link = "logit"))
pred_logit1 <- predict(logitMod1, testData, type = "response")
summary(logitMod1)

table(Test1$pred_logitA)

confusionMatrix(testData$income_high, pred_logit1)
confusionMatrix(Test1$pred_logitA, Test1$income_high_binaryA)
#     No  Yes
# 0 6401 1093
# 1  260 1035
# 0.96 no's correctly predicted.
# 50% yes's correctly predicted. This is very low rate due to imbalanced data.

Test1 <- testData  %>% mutate(pred_logitA = 1*(pred_logit1 > .53) + 0,income_high_binaryA = 1*(income_high == "Yes") + 0)
head(Test1)
Test1 <- Test1 %>% mutate(accurate = 1*(pred_logitA == income_high_binaryA))
sum(Test1$accurate)/nrow(Test1)
# 0.8453749



# check the IMBALANCE
table(trainingData$income_high)
#    No   Yes 
# 15539  4963

prop.table(table(trainingData$income_high))
# No       Yes 
# 0.7579261 0.2420739

str(AdultIncomeData5x)

library(rpart)
treeMod <- rpart(income_high ~., data = trainingData)
pred_treeMod <- predict(treeMod, newdata = testData)

library(ROSE)
accuracy.meas(testData$income_high, pred_treeMod[,2])
# Examples are labelled as positive when predicted is greater than 0.5 
# precision: 0.776
# recall: 0.507
# F: 0.307

#check accuracy
roc.curve(testData$income_high, pred_treeMod[,2], plotit = F)
# Area under the curve (AUC): 0.842
# With threshold value as 5, Precision = 0.776 says there are few false negatives. 
# Recall = 0.5 is low and indicates that we have some false negatives. 
# F = 0.19 is also low and suggests weak accuracy of this model. 
# AUC is a lightly low too. In this case, the algorithm gets biased toward the majority class and fails to map minority class.


# lets compare some sampling methods
table(trainingData$income_high)
#   No   Yes 
# 15539  4963 
table(testData$income_high)
#   No   Yes 
#  6661 2128 

# Under-sampling
data_balanced_under <- ovun.sample(income_high ~., data = trainingData, method = "under", N = 9926)$data
table(data_balanced_under$income_high)
# No  Yes 
# 4963 4963

data_balanced_under_test <- ovun.sample(income_high ~., data = testData, method = "under", N = 4256)$data
table(data_balanced_under_test$income_high)
# No  Yes 
# 2128 2128


# Over-sampling
data_balanced_over <- ovun.sample(income_high ~., data = trainingData, method = "over", N = 31078)$data
table(data_balanced_over$income_high)
# No   Yes 
# 15539 15539 

# Both
data_balanced_both <- ovun.sample(income_high ~., data = trainingData, method = "both", p=0.5, N = 20502)$data
table(data_balanced_both$income_high)
# No   Yes 
# 10277 10225

# ROSE function does something similar
data_rose <- ROSE(income_high ~., data =  trainingData)$data
table(data_rose$income_high)
# No   Yes 
# 10249 10253

# built decision tree models
tree.under <- rpart(income_high ~., data = data_balanced_under)
tree.over <- rpart(income_high ~., data = data_balanced_over)
tree.both <- rpart(income_high ~., data = data_balanced_both)
tree.rose <- rpart(income_high~., data = data_rose)

# make predictions on test data
pred_tree.under <- predict(tree.under, newdata = testData)
pred_tree.over <- predict(tree.over, newdata = testData)
pred_tree.both <- predict(tree.both, newdata = testData)
pred_tree.rose <- predict(tree.rose, newdata = testData)

# AUC
par(mfrow = c(2,2))
roc.curve(testData$income_high, pred_tree.under[,2], col = "RED", main = "ROC curve of under")
roc.curve(testData$income_high, pred_tree.over[,2], col = "BLUE", main = "ROC curve of over")
roc.curve(testData$income_high, pred_tree.both[,2], col = "ORANGE", main = "ROC curve of both")
roc.curve(testData$income_high, pred_tree.rose[,2], col = "BLACK", main = "ROC curve of rose")
# Area under the curve (AUC): 0.845
# Area under the curve (AUC): 0.845
# Area under the curve (AUC): 0.845
# Area under the curve (AUC): 0.826
# I will go with under sampling since it has high accuracy and my data is enough.

# MODEL 2
logitMod2 <- glm(income_high ~., data = data_balanced_under, family = binomial(link = "logit"))
pred_logit2 <- predict(logitMod2, data_balanced_under_test, type = "response")
summary(logitMod2)

confusionMatrix(data_balanced_under_test$income_high, pred_logit2)
#     No  Yes
# 0 5732  536
# 1  929 1592
# 0.86 no's correctly predicted.
# 0.75 yes's correctly predicted.

Test2 <- data_balanced_under_test  %>% mutate(pred_logitB = 1*(pred_logit2 > .53) + 0,income_high_binaryB = 1*(income_high == "Yes") + 0)
head(Test2)
Test2 <- Test2 %>% mutate(accurate = 1*(pred_logitB == income_high_binaryB))
sum(Test2$accurate)/nrow(Test2)
# 0.818844



# Variable Importance
VarImportance <- varImp(logitMod)
# move index to the column 1
VarImportance <- cbind( Variable = rownames(VarImportance), VarImportance)
rownames(VarImportance) <- 1:nrow(VarImportance)

VarImportance <- VarImportance[order(VarImportance$Overall, decreasing = TRUE),]
head(VarImportance)
#                             Variable   Overall
# 24 marital_status Married-civ-spouse 22.927935
# 47                      capital_gain 16.967933
# 49                    hours_per_week 12.571446
# 1                                age 11.361947
# 48                      capital_loss  9.499026
# 31        occupation Exec-managerial  7.436940







