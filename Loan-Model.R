loan <- read.csv("~/Desktop/loan.csv")

head(loan)
tail(loan)
loan$loan_status

str(loan)
summary(loan)

##Delete columns with NA only.

loanna <- is.na(loan)

loanna
#True save as 1, False save as 0
colSums(loanna)
loan <- loan[, colSums(loanna) != 39717]
summary(loan)

loan$application_type


#Delete non related columsn
loan$acc_now_delinq <- NULL
loan$tax_liens <- NULL
loan$chargeoff_within_12_mths <- NULL
loan$delinq_amnt <- NULL
loan$policy_code <- NULL
loan$application_type <- NULL

summary(loan)
loan$collections_12_mths_ex_med <- NULL
loan$policy_code <- NULL
loan$initial_list_status <- NULL
loan$pymnt_plan <- NULL

summary(loan)
loan$url <- NULL

#Now roughly we've finished the data cleaning
str(loan)


loan <- loan[,-c(1,2)]
summary(loan)



#Wants to make a model to predict loan status

#Visualization
library(ggplot2)
ggplot(loan, aes(x=loan_amnt)) + geom_histogram() + labs(title = "Loan Amount")
ggplot(loan, aes(x=loan_amnt)) + geom_density() + labs(title = "Loan Amount")
ggplot(loan, aes(x=loan_amnt, fill = purpose)) + geom_histogram() + labs(title = "Loan Amount")
ggplot(loan, aes(x=loan_amnt, fill = purpose)) + geom_density() + labs(title = "Loan Amount")


ggplot(loan[loan$purpose == "car", ], aes(x=loan_amnt)) + geom_histogram() + labs(title = "Loan Amount for cars")
ggplot(loan[loan$purpose == "house", ], aes(x=loan_amnt)) + geom_histogram() + labs(title = "Loan Amount for house")
ggplot(loan[loan$purpose == "credit_card", ], aes(x=loan_amnt)) + geom_histogram() + 
  labs(title = "Loan Amount for credit card")

ggplot(loan[loan$purpose == "small_business", ], aes(x=loan_amnt)) + geom_histogram() + 
  labs(title = "Loan Amount for small business")


str(loan$int_rate)
loan$int_rate <- as.numeric(loan$int_rate)
str(loan$int_rate)

ggplot(loan, aes(x=int_rate)) + geom_histogram()

str(loan$annual_inc)
ggplot(loan, aes(x=annual_inc)) + geom_histogram()
#There is outlier

boxplot(loan$annual_inc)

ggplot(loan, aes(x=annual_inc)) + geom_histogram() + xlim(0, 600000)

mean(loan$annual_inc)
median(loan$annual_inc)


#Test those variables
#A bit related
ggplot(loan, aes(x=loan_status, y=annual_inc)) + geom_boxplot() + ylim(0, 200000)
ggplot(loan, aes(x=loan_status, y=loan_amnt)) + geom_boxplot() 
ggplot(loan, aes(x=loan_status, y=dti)) + geom_boxplot() 


#10 var

#boruta 
#R automatic selection method

#weight ~ age, model performs well
#weight ~ random number, model performs well goes down

#Y ~ A + B + C
#Y ~ RN + B + C

#y ~ a
#y ~ b
#y ~ c

#Interaction effect


loan2 <- loan[loan$loan_status != "Current", ]
loan2$loan_status <- ifelse(loan2$loan_status == "Fully Paid", "Good", "Bad")
loan2$loan_status <- factor(loan2$loan_status)
table(loan2$loan_status)

index <- sample(38577, 30000)
loan2train <- loan2[index,]
loan2test <- loan2[-index,]

colnames(loan2train)

#Package Boruta

library(Boruta)

set.seed(1)
boruta.result <- Boruta(loan_status ~., data = na.omit(loan2train), doTrace= 2)
boruta.result
boruta.final <- TentativeRoughFix(boruta.result)

boruta.final

boruta.final$finalDecision == "Confirmed"
#Not features but also, loan status
trainlabel <- loan2train$loan_status
testlabel <- loan2test$loan_status
loan2train$loan_status <- NULL
loan2test$loan_status <- NULL
loan2train <- loan2train[, boruta.final$finalDecision == "Confirmed"]
loan2test <- loan2test[, boruta.final$finalDecision == "Confirmed"]


loan2train <- cbind(loan2train, trainlabel)
loan2test <- cbind(loan2test, testlabel)

loan2train <- na.omit(loan2train)
loan2test <- na.omit(loan2test)

str(loan2test)
str(loan2train)



#Model building
library(caret)
controlezc <- trainControl(method = "cv", summaryFunction = twoClassSummary,
                           classProbs = T, savePredictions = T)


set.seed(723)
model.1 <- train(trainlabel ~., data=loan2train, method="glm", trControl = controlezc)

result <- predict(model.1, loan2test)
table(loan2test$testlabel, result)

#80% ~ 90%







