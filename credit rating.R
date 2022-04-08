#setwd("D:/data/A3")
library(rpart)
library(rpart.plot)
library(caret)
library(tree)
library(randomForest)
library(ROCR)

#Q1 read in dataset
cw.all <- read.csv("creditworthiness.csv")

#unknown data is crediting rating = 0, known data is ones with credit rating 1,2,3
cw.k <- subset(cw.all,credit.rating > 0)
cw.unknown <- subset(cw.all,credit.rating == 0)

#spilt data into training and testing
cw.train <- cw.k[1:(nrow(cw.k)/2),] 
cw.test <- cw.k[-(1:(nrow(cw.k)/2)),]

#question 2 Lets create some ratings to use for predicting
median_cust = data.frame()
new_data =
  c(0,1,2,3,3,2,1,2,1,2,3,3,1,3,3,1,3,2,3,3,2,3,1,3,2,1,1,1,2,2,2,3,3,
    3,3,3,3,3,3,3,3,3,3,3,3)
median_cust = rbind(median_cust, new_data)
colnames(median_cust) = names(cw.k)[-46]
head(median_cust)

head(cw.all)

tree <- rpart(credit.rating ~ functionary+re.balanced..paid.back..a.recently.overdrawn.current.acount+FI3O.credit.score
                              +gender+X0..accounts.at.other.banks+credit.refused.in.past.+years.employed+savings.on.other.accounts+self.employed.+max..account.balance.12.months.ago
                              +min..account.balance.12.months.ago+avrg..account.balance.12.months.ago+max..account.balance.11.months.ago+min..account.balance.11.months.ago
                              +avrg..account.balance.11.months.ago+max..account.balance.10.months.ago+min..account.balance.10.months.ago+avrg..account.balance.10.months.ago
                              +max..account.balance.9.months.ago+min..account.balance.9.months.ago+avrg..account.balance.9.months.ago+max..account.balance.8.months.ago
                              +min..account.balance.8.months.ago+avrg..account.balance.8.months.ago+max..account.balance.7.months.ago+min..account.balance.7.months.ago
                              +avrg..account.balance.7.months.ago+max..account.balance.6.months.ago+min..account.balance.6.months.ago+avrg..account.balance.6.months.ago
                              +max..account.balance.5.months.ago+min..account.balance.5.months.ago+avrg..account.balance.5.months.ago+max..account.balance.4.months.ago
                              +min..account.balance.4.months.ago+avrg..account.balance.4.months.ago+max..account.balance.3.months.ago+min..account.balance.3.months.ago
                              +avrg..account.balance.3.months.ago+max..account.balance.2.months.ago+min..account.balance.2.months.ago+avrg..account.balance.2.months.ago
                              +max..account.balance.1.months.ago+min..account.balance.1.months.ago+avrg..account.balance.1.months.ago,cw.train) 
#2(a)
Dtree <- tree(as.factor(credit.rating)~., cw.train)
plot(Dtree)
text(Dtree, pretty = 0)
Pred_tree <- predict(Dtree, cw.test, type = "class")
print(Pred_tree)

#2(b)
tree <- rpart(credit.rating ~ functionary+re.balanced..paid.back..a.recently.overdrawn.current.acount+FI3O.credit.score
              +gender+X0..accounts.at.other.banks+credit.refused.in.past.+years.employed+savings.on.other.accounts+self.employed.+max..account.balance.12.months.ago
              +min..account.balance.12.months.ago+avrg..account.balance.12.months.ago+max..account.balance.11.months.ago+min..account.balance.11.months.ago
              +avrg..account.balance.11.months.ago+max..account.balance.10.months.ago+min..account.balance.10.months.ago+avrg..account.balance.10.months.ago
              +max..account.balance.9.months.ago+min..account.balance.9.months.ago+avrg..account.balance.9.months.ago+max..account.balance.8.months.ago
              +min..account.balance.8.months.ago+avrg..account.balance.8.months.ago+max..account.balance.7.months.ago+min..account.balance.7.months.ago
              +avrg..account.balance.7.months.ago+max..account.balance.6.months.ago+min..account.balance.6.months.ago+avrg..account.balance.6.months.ago
              +max..account.balance.5.months.ago+min..account.balance.5.months.ago+avrg..account.balance.5.months.ago+max..account.balance.4.months.ago
              +min..account.balance.4.months.ago+avrg..account.balance.4.months.ago+max..account.balance.3.months.ago+min..account.balance.3.months.ago
              +avrg..account.balance.3.months.ago+max..account.balance.2.months.ago+min..account.balance.2.months.ago+avrg..account.balance.2.months.ago
              +max..account.balance.1.months.ago+min..account.balance.1.months.ago+avrg..account.balance.1.months.ago,cw.train) 
result <- predict(tree,median_cust)
print(result)


#2(c)
confusiontree = with(cw.test, table(Pred_tree, credit.rating))
print(confusiontree)
sum(diag(confusiontree))/sum(confusiontree)

#2(d) first split is at functionary
beforeCountFreq = table(cw.train$credit.rating)
beforeClassProb = beforeCountFreq/sum(beforeCountFreq)
beforeEntropy = -sum(beforeClassProb * log2(beforeClassProb))
cat(beforeEntropy)
countFreq0 = table(cw.train$credit.rating[cw.train$functionary == 0])
classProb0 = countFreq0/sum(countFreq0)
(functionaryEnt0 = -sum(classProb0 * log2(classProb0)))
cat(functionaryEnt1)

countFreq1 = table(cw.train$credit.rating[cw.train$functionary == 1])
classProb1 = countFreq1/sum(countFreq1)
(functionaryEnt1 = -sum(classProb1 * log2(classProb1)))

ent = (beforeEntropy - (functionaryEnt0 * sum(countFreq0) +
                          functionaryEnt1 * sum(countFreq1)) /
         sum(sum(countFreq0) + sum(countFreq1)))
print(ent)

#2(e)
cw.train$credit.rating <- as.character(cw.train$credit.rating)
cw.train$credit.rating <- as.factor(cw.train$credit.rating)
rf.cw.train = randomForest(credit.rating~., data = cw.train)
rf.pred = predict(rf.cw.train, cw.test[,-46])
rf.predcust = predict(rf.cw.train, median_cust)
head(rf.pred)
print(rf.predcust)
confusionRF = table(rf.pred, cw.test$credit.rating)
print(confusionRF)
sum(diag(confusionRF))/sum(confusionRF)


#2(f)
# Fit to a model using randomForest after the tuning
RFTuned.cw.train = randomForest(credit.rating~., data = cw.train, mtry
                                = 15, ntree=900, stepFactor=2, improve=0.2)
RFTuned.pred = predict(RFTuned.cw.train, cw.test[,-46])
# Produce confusion matrix after the tuning
confusionRFTuned = with(cw.test, table(RFTuned.pred, credit.rating))
print(confusionRFTuned)
# Calculate the accuracy rate after the tuning
sum(diag(confusionRFTuned))/sum(confusionRFTuned)



#3(a)
library(e1071)
svmfit = svm(credit.rating ~ ., data = cw.train, kernel = "radial")
print(svmfit)

predict(svmfit, median_cust, decision.values = TRUE)

#3(b)
svm.pred = predict(svmfit, cw.test[,-46])
confusionSVM = with(cw.test, table(svm.pred, credit.rating))
(confusionSVM)
sum(diag(confusionSVM))/sum(confusionSVM)

#3(c)
summary(tune.svm(credit.rating ~ ., data = cw.train,
                 kernel = "radial",cost = 10^c(0:2), gamma = 10^c(-4:-1)))
# Fit a model using SVM
svmTuned = svm(credit.rating ~ ., data = cw.train, kernel = "radial",
               cost=170,
               gamma = 0.0003,
               tunecontrol = tune.control(sampling = "fix"))
# Predict the values on test set
svmTuned.pred = predict(svmTuned, cw.test[,-46])

# Produce confusion matrix
confusionTunedSVM = with(cw.test, table(svmTuned.pred, credit.rating))
confusionTunedSVM
# Overall accuracy rate
sum(diag(confusionTunedSVM))/sum(confusionTunedSVM)


#4(a)
nb_default <- naiveBayes(cw.train$credit.rating ~., data= cw.train)
default_pred <- predict(nb_default, cw.test, type="class")

table(default_pred, cw.test$credit.rating,dnn=c("Prediction","Actual"))

nb_default <- naiveBayes(credit.rating ~., data= cw.train)
default_pred <- predict(nb_default, median_cust, type="class")
default_raw_pred <- predict(nb_default, median_cust, type="raw")
head(default_pred, 20)
head(default_raw_pred)
nb_default

nb.pred = predict(nb_default, cw.test[,-46])
#produce confusion matrix
confusionNB = with(cw.test, table(nb.pred, credit.rating))
confusionNB
#calculate the accuracy rate
sum(diag(confusionNB))/sum(confusionNB)

# 4(b)
head(nb_default, 20)

#5(a)
sum(diag(confusionNB))/sum(confusionNB)
sum(diag(confusionTunedSVM))/sum(confusionTunedSVM)
sum(diag(confusionRFTuned))/sum(confusionRFTuned)
sum(diag(confusiontree))/sum(confusiontree)


#6(a)
glm.fit <- glm((credit.rating==1)~., data = cw.train, family = binomial)
options(width = 130)
#6(b)
summary(glm.fit)

#6(d)
summary(tune.svm((credit.rating==1) ~ ., data = cw.train,
                 kernel = "radial",cost = 10^c(0:2), gamma = 10^c(-4:-1), type = 'C'))
# Fit a model using SVM
svm2.fit = svm(I(credit.rating == 1)~ ., data = cw.train, type = "C")

svm2.fit.pred = predict(svm2.fit, cw.test[,-46], decision.values =TRUE)
# Predict the values on test set[GLM]
glm.fit.pred = predict(glm.fit, cw.test[,-46])


confusionSVM = prediction(-attr(svm2.fit.pred, "decision.values"),
                          cw.test$credit.rating == 1)

# Create rocs curve based on prediction
rocsSVM <- performance(confusionSVM, "tpr", "fpr")

confusionGLM = prediction(glm.fit.pred, cw.test$credit.rating == 1)
#create rocs curve based on prediction
rocsGLM <- performance(confusionGLM, "tpr", "fpr")

# Plot the graph
plot(rocsGLM, col=1)
plot(rocsSVM, col= 2 ,add=TRUE)
abline(0, 1, lty = 3)
# Add the legend to the graph
legend(0.6, 0.6, c('glm','svm'), 1:2)
