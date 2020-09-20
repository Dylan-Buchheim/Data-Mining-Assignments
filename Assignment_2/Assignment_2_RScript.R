#Data Mining, Assignment_2
#Dylan Buchheim, R11524739

#libraries
library(rpart)
library(rpart.plot)
library(C50)

#importing the churn dataset
churn <- read.csv("churn")

#Problem 1 -----------------
numTrue <- nrow(churn[which(churn$Churn == "True"),])
resampleSize <- ((0.2 * nrow(churn) - numTrue) / 0.8)
#Rebalancing the data with the resample size.
to.resample <- which(churn$Churn == "True")
resample <- sample(x= to.resample, size= resampleSize, replace= TRUE)                 
resample.records <- churn[resample,]                 
churn_rebal <- rbind(churn,resample.records)

t1 <- table(churn_rebal$Churn)
proportionsTable <- rbind(t1,round(prop.table(t1),4))
#Verifying the proportion of the rebalance
print(proportionsTable)

#Problem 2 -----------------
set.seed(99)
numRows <- nrow(churn_rebal)
#Generating random numbers to get a 67% - 33% split.
indexes <- runif(numRows) < 0.67
#Splitting the rebalanced data set based on the random generation.
churn_training <- churn_rebal[indexes,]
churn_test <- churn_rebal[!indexes,]
#Confirming the proportions using a bar graph.
counts <- c(nrow(churn_training),nrow(churn_test))
barplot(counts, names.arg = c("Training","Test"), ylab = "Entries", col = c("Gray","Black"))
#Validating the training and test set, t-test
t.test(churn_training$Day.Mins,churn_test$Day.Mins)
#Validating the training and test set, z-test
pTrain <- sum(churn_training$Churn == "True") / nrow(churn_training)
pTest <- sum(churn_test$Churn == "True") / nrow(churn_test)
pPooled <-  (sum(churn_training$Churn == "True") + sum(churn_test$Churn == "True")) / 
            (nrow(churn_training) + nrow(churn_test))
z <- (pTrain - pTest) / sqrt(pPooled * (1-pPooled) * (1/nrow(churn_training) + 1/nrow(churn_test)))
print(2*pnorm(-abs(z)))

#Problem 3 -----------------
#Changing the categorical data to factors.
churn_training$Intl.Plan <- factor(churn_training$Intl.Plan)
churn_training$VMail.Plan <- factor(churn_training$VMail.Plan)

#CART Model 1
cart01 <- rpart(formula = Churn ~ Account.Length + Intl.Plan + VMail.Plan, data = churn_training, method = "class")
rpart.plot(cart01,type = 4, extra = 2)
#Model 1 Prediction
x_1 <- data.frame(Account.Length = churn_test$Account.Length,
                  Intl.Plan = churn_test$Intl.Plan,
                  VMail.Plan = churn_test$VMail.Plan)
predChurnCART01 = predict(object = cart01, newdata = x_1, type ="class")
predChurnCART01_df = data.frame(Churn = predChurnCART01)
#Model 1 Confusion Matrix and Accuracy
confMatrixCART01 <- table(churn_test$Churn,predChurnCART01_df$Churn)
row.names(confMatrixCART01) <- c("Actual: False:","Actual: True")
colnames(confMatrixCART01) <- c("Predicted: False","Predicted: True")
confMatrixCART01 <- addmargins(A = confMatrixCART01, FUN=list(Total = sum), quiet = TRUE)
print(confMatrixCART01)
accuracyCART01 <- (confMatrixCART01[1,1] + confMatrixCART01[2,2]) / confMatrixCART01[3,3]

#CART Model 2
cart02 <- rpart(formula = Churn ~ Day.Mins + Eve.Mins + Night.Mins + Intl.Mins, data = churn_training, method = "class")
rpart.plot(cart02,type = 4, extra = 2)
#Model 2 Prediction
x_2 <- data.frame(Day.Mins = churn_test$Day.Mins,
                  Eve.Mins = churn_test$Eve.Mins,
                  Night.Mins = churn_test$Night.Mins,
                  Intl.Mins = churn_test$Intl.Mins)
predChurnCART02 = predict(object = cart02, newdata = x_2, type ="class")
predChurnCART02_df = data.frame(Churn = predChurnCART02)
#Model 2 Confusion Matrix and Accuracy
confMatrixCART02 <- table(churn_test$Churn,predChurnCART02_df$Churn)
row.names(confMatrixCART02) <- c("Actual: False:","Actual: True")
colnames(confMatrixCART02) <- c("Predicted: False","Predicted: True")
confMatrixCART02 <- addmargins(A = confMatrixCART02, FUN=list(Total = sum), quiet = TRUE)
print(confMatrixCART02)
accuracyCART02 <- (confMatrixCART02[1,1] + confMatrixCART02[2,2]) / confMatrixCART02[3,3]

#CART Model 3
cart03 <- rpart(formula = Churn ~ Account.Length + Intl.Plan + VMail.Plan + CustServ.Calls, data = churn_training, method = "class")
rpart.plot(cart03,type = 4, extra = 2)
#Model 3 Prediction
x_3 <- data.frame(Account.Length = churn_test$Account.Length,
                  Intl.Plan = churn_test$Intl.Plan,
                  VMail.Plan = churn_test$VMail.Plan,
                  CustServ.Calls = churn_test$CustServ.Calls)
predChurnCART03 = predict(object = cart03, newdata = x_3, type ="class")
predChurnCART03_df = data.frame(Churn = predChurnCART03)
#Model 3 Confusion Matrix and Accuracy
confMatrixCART03 <- table(churn_test$Churn,predChurnCART03_df$Churn)
row.names(confMatrixCART03) <- c("Actual: False:","Actual: True")
colnames(confMatrixCART03) <- c("Predicted: False","Predicted: True")
confMatrixCART03 <- addmargins(A = confMatrixCART03, FUN=list(Total = sum), quiet = TRUE)
print(confMatrixCART03)
accuracyCART03 <- (confMatrixCART03[1,1] + confMatrixCART03[2,2]) / confMatrixCART03[3,3]

#Model number 2 performed the best overall, it had the higest accuracy.
#It also had a much better ratio of false positives to true positives, whereas the first and third models had a terrible ratio.

#Problem 4 ---------------------

#C50 Model 1
C501 <- C5.0(formula = Churn ~ Account.Length + Intl.Plan + VMail.Plan, data = churn_training,
             control = C5.0Control(minCases=25))
plot(C501)
predChurnC501 <- predict(object = C501, newdata = x_1, type ="class")
predChurnC501_df <- data.frame(Churn = predChurnC501)
#Model 1 Confusion Matrix and Accuracy
confMatrixC501 <- table(churn_test$Churn,predChurnC501_df$Churn)
row.names(confMatrixC501) <- c("Actual: False:","Actual: True")
colnames(confMatrixC501) <- c("Predicted: False","Predicted: True")
confMatrixC501 <- addmargins(A = confMatrixC501, FUN=list(Total = sum), quiet = TRUE)
print(confMatrixC501)
accuracyC501 <- (confMatrixC501[1,1] + confMatrixC501[2,2]) / confMatrixC501[3,3]

#C50 Model 2
C502 <- C5.0(formula = Churn ~ Day.Mins + Eve.Mins + Night.Mins + Intl.Mins, data = churn_training,
             control = C5.0Control(minCases=25))
plot(C502)
predChurnC502 <- predict(object = C502, newdata = x_2, type ="class")
predChurnC502_df <- data.frame(Churn = predChurnC502)
#Model 2 Confusion Matrix and Accuracy
confMatrixC502 <- table(churn_test$Churn,predChurnC502_df$Churn)
row.names(confMatrixC502) <- c("Actual: False:","Actual: True")
colnames(confMatrixC502) <- c("Predicted: False","Predicted: True")
confMatrixC502 <- addmargins(A = confMatrixC502, FUN=list(Total = sum), quiet = TRUE)
print(confMatrixC502)
accuracyC502 <- (confMatrixC502[1,1] + confMatrixC502[2,2]) / confMatrixC502[3,3]

#C50 Model 3
C503 <- C5.0(formula = Churn ~ Account.Length + Intl.Plan + VMail.Plan + CustServ.Calls, data = churn_training, 
             control = C5.0Control(minCases=25))
plot(C503)
predChurnC503 <- predict(object = C503, newdata = x_3, type ="class")
predChurnC503_df <- data.frame(Churn = predChurnC503)
#Model 3 Confusion Matrix and Accuracy
confMatrixC503 <- table(churn_test$Churn,predChurnC503_df$Churn)
row.names(confMatrixC503) <- c("Actual: False:","Actual: True")
colnames(confMatrixC503) <- c("Predicted: False","Predicted: True")
confMatrixC503 <- addmargins(A = confMatrixC503, FUN=list(Total = sum), quiet = TRUE)
print(confMatrixC503)
accuracyC503 <- (confMatrixC503[1,1] + confMatrixC503[2,2]) / confMatrixC503[3,3]

#Once again, model 2 performed the best. 
#It had the highest accuracy and had the best ratio of false positives to true positives.

#I ran the same three formulas with C50 as I did with CART and the results with C50 were very similar.
#With the formulas I ran, there was almost no difference in the two algorithms in regard to accuracy.
