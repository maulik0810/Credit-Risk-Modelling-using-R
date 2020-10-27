##CREDIT RISK MODELLING EXAMPLES##

credit <- read.csv("C:/Users/Dell-pc/Desktop/credit.csv")
View(credit)

testtable = table(credit$defaulter)
prop.table(testtable)


#drop the columns which are not quite useful
drop=c('dependents','phone')
credit= credit[,!names(credit) %in% drop]

#lable encoding
credit$checking_balance = as.integer(factor(credit$checking_balance))
credit$credit_history = as.integer(factor(credit$credit_history))
credit$purpose = as.integer(factor(credit$purpose))
credit$savings_balance = as.integer(factor(credit$savings_balance))
credit$employment_duration = as.integer(factor(credit$employment_duration))
credit$housing = as.integer(factor(credit$housing))
credit$job = as.integer(factor(credit$job))
credit$defaulter = ifelse(credit$defaulter=='yes', 1, 0)
credit$other_credit = as.integer(factor(credit$other_credit))

''
credithiss = table(credit$defaulter,credit$credit_history,dnn=c("Default","History"))
sumbonus=addmargins(credithiss,FUN=sum)
sumbonus
''

#EDA(Exlpratory data analysis)
barplot(credithiss,legend=rownames(credithiss), col=c("black","blue"),ylab = "count",xlab = "Credit History", main="Default with respect to credit history")


#spliting data into test and train
set.seed(42)
smp_size <- floor(0.75 * nrow(credit))
train_ind <- sample(seq_len(nrow(credit)), size = smp_size)
train <- credit[train_ind, ]
test <- credit[-train_ind, ]


#checking proportions across train and test
prop.table(table(train$defaulter))
prop.table(table(test$defaulter))

#training a model using logistic regression#
############################################

#training a model
cr1 <- glm(defaulter ~ checking_balance + months_loan_duration + credit_history + purpose + amount  + savings_balance + percent_of_income , data = train, family = "binomial" ) #removing split feature and dependent variable
summary(cr1) #summary of the model output
cr2 <- glm(defaulter ~ checking_balance + months_loan_duration + credit_history + amount  + savings_balance + percent_of_income , data = train, family = "binomial" )
summary(cr2)
cr3 <- glm(defaulter ~ checking_balance + months_loan_duration + credit_history + savings_balance + percent_of_income , data = train, family = "binomial" )
summary(cr3)
cr4 <- glm(defaulter ~ checking_balance + months_loan_duration + credit_history + savings_balance , data = train, family = "binomial" )
summary(cr4)


#predicing on test data
predCreditLogReg <- predict(cr4, newdata = test, type = "response") #this probability of creidt default for each observation

#obtaining a confusion matrix
table(test$defaulter, predCreditLogReg > 0.5) 


#computing the accuracy of the model
accuracyCreditLogReg <- ((as.matrix(table(test$defaulter, predCreditLogReg > 0.5))[1,1]) + (as.matrix(table(test$default, predCreditLogReg > 0.5))[2,2]))/nrow(test)
print(accuracyCreditLogReg)

#computing the baseline model for comparison
baseLineAccuracy <- max(table(test$default))/nrow(test)
print(baseLineAccuracy)

#assesing the robustness of model
library(ROCR)
library(arulesViz)

ROCRpred=prediction(predCreditLogReg,test$defaulter)
ROCRperf=performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize=T, main= "ROC Curve", ylab = "Sensitivity", xlab = "1-Specificity")
abline(a=0,b=1)

#Note : Closer to 1 is better, 0.78 here is not bad for a first model

