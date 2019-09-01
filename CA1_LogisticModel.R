# Questions to ask:
# 1. What's the difference between closed-end credit and open credit line in this case study? Which 
# one is the point of interest that we are predicting for? 
# 2. Which avariable is the dependent variable? (targetloanstatus?) 
# 3. If it is targetloanstatus, what do 0 and 1 stand for?

# load packages
pacman::p_load(tidyverse, caret, corrplot, caTools, knitr, car, ROCR, IRdisplay, e1071,earth)

# set path to directory
setwd("/Users/xinxinli/Dropbox/EBAC Master/2019 Semester 2/CA1/data")

# load data
loan = read.csv("loans.csv")

# data characteristics
head(loan, 4)
dim(loan)
str(loan)
summary(loan)

# correct data type
loan$targetloanstatus = factor(loan$targetloanstatus)
loan$creditpolicy = factor(loan$creditpolicy)

# remove NAs
new_loan = na.omit(loan)
summary(new_loan)

# variable transformation: annualinc to log(annualinc)
new_loan %>%
  ggplot(aes(annualinc)) + geom_histogram()
new_loan %>%
  ggplot(aes(log(annualinc))) + geom_histogram()
new_loan$annualinc_log = log(new_loan$annualinc)

# explore the predictor variables 
new_loan %>%
  group_by(targetloanstatus) %>%
  summarise(per = n()/nrow(new_loan)) %>%
  ggplot(aes(x=targetloanstatus, y=per, fill=targetloanstatus)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label=round(per,2)),vjust=2)

contrasts(new_loan$targetloanstatus)

# split train and test set
set.seed(123)
splitData = sample.split(new_loan$targetloanstatus, SplitRatio = 0.7)
train = new_loan[splitData,]
test = new_loan[!splitData,]

# check the correlation of dependent variables 
df = new_loan
corrplot(cor(df[, sapply(df, is.numeric)], use = "complete.obs"),
         method = "number", type="upper")

# initial model: removing variables that are not statistically significant 
model1=glm(targetloanstatus ~ creditpolicy + term + grade + annualinc + purpose + inqlast6mths + 
            revolbal + revolutil, data = train, family = binomial)
summary(model1)
vif(model1)
varImp(model1)

# tune model as fitted probabilities numerically 0 or 1 occurred: transform annulinc to log(annulinc)
model2=glm(targetloanstatus ~ creditpolicy + term + grade + annualinc_log + purpose + inqlast6mths + 
             revolbal + revolutil, data = train, family = binomial)
summary(model2)
vif(model2)
varImp(model2)

# further refine model using step function
model=step(model2)
summary(model)
vif(model)
varImp(model)

# overall model fit: p-value
with(model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

# model coeffeicients 
exp(coef(model))

# confusion matrix and test accuracy on the train set 
trainProb = predict(model, newdata = train, type = "response")
# threshold = sum(train$targetloanstatus=="1") / length(train$targetloanstatus)
trainPred = ifelse(trainProb > 0.178 , 1,0)
confus_matrix_train = table(train$targetloanstatus, trainPred)
confus_matrix_train
trainAccu = sum(diag(confus_matrix_train)) / sum(confus_matrix_train)
round(trainAccu, 3)

# confusion matrix and test accuracy on the test set
testProb = predict(model, newdata = test, type = "response")
testPred = ifelse(testProb > 0.178, 1,0)
confus_matrix_test = table(test$targetloanstatus, testPred)
confus_matrix_test
testAccu = sum(diag(confus_matrix_test)) / sum(confus_matrix_test)
round(testAccu, 3)

# lift chart 
pred = prediction(trainProb, train$targetloanstatus)
perf = performance(pred, "lift", "rpp")
plot(perf, main = "lift curve", xlab = "Proportions of Customers (sorted prob)")

