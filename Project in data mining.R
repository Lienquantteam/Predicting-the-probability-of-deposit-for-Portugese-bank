# Project  
setwd("D:/Data analytics at UCD/Data Mining/Project")
getwd
bank <-read.csv("bank.csv")
View(bank)
library("ggplot2")


## Exploration missing data
f_NA <- function(x){
  which(is.na(x))
}
sapply(bank, f_NA) #  have missing data

#The marketing campaigns were based on phonecalls with the aim of assessing if the client would subscribe to a bank term deposit or not
table(bank$y,bank$housing, dnn = c('deposit', 'housingloan'))
table(bank$y,bank$loan, dnn = c('deposit', 'personalloan'))
table(bank$job,bank$y,dnn = c('job', 'deposit'))
table(bank$job,bank$y,dnn = c('job', 'deposit'))/rowSums(table(bank$job,bank$y))
table(bank$marital,bank$y,dnn = c('Marital', 'deposit'))
table(bank$education,bank$y,dnn = c('education', 'deposit'))
table(bank$education,bank$y,dnn = c('education', 'deposit'))/rowSums(table(bank$education,bank$y))
 
bank$Pre_Campaign <- ifelse(bank$pdays == -1, "no","yes") # dung ggplot, chuyen thanh dang factor, giong job # bien doi thanh yes,no
table(bank$Pre_Campaign,bank$y,dnn = c('Pre_Campaign', 'deposit'))

table(bank$previous,bank$y,dnn = c('previous', 'deposit'))
table(bank$poutcome,bank$y,dnn = c('outcome', 'deposit'))
table(bank$poutcome,bank$y,dnn = c('outcome', 'deposit'))/rowSums(table(bank$poutcome,bank$y))
table(bank$default,bank$y,dnn = c('default', 'deposit'))

t <- table(bank$marital,bank$y,dnn = c('Marital', 'deposit'))/rowSums(table(bank$marital,bank$y))
plot(t)

ggplot(data = bank[bank$y =="yes",], aes(x = age) ) + geom_bar(fill="blue") 
ggplot(data = bank[bank$y =="yes",], aes(x = balance) ) + geom_bar(fill="red") 
ggplot(data = bank[bank$y =="yes",], aes(x = previous) ) + geom_bar(fill="green")
ggplot(data = bank[bank$y =="yes",], aes(x = day) ) + geom_bar(fill="purple")
ggplot(data = bank[bank$y =="yes",], aes(x = campaign) ) + geom_bar(fill="pink")
ggplot(data = bank[bank$y =="yes",], aes(x = month) ) + geom_bar(fill="yellow")
ggplot(data = bank[bank$y =="yes",], aes(x = pdays) ) + geom_bar(fill="blue")

# Make sure virables are catagorial 
bank$y <- as.factor(bank$y)
bank$job <- as.factor(bank$job)
bank$housing <- as.factor(bank$housing)
bank$loan <- as.factor(bank$loan)
bank$default <- as.factor(bank$default)
bank$day <- as.factor(bank$day)

#########################################################################################
fit <- glm(y~., data=bank, family = "binomial")
summary(fit)

fit <- glm(y~., data=bank[,-c(1,4,5,6,9,12,13)], family = "binomial", subset = indtrain)
summary(fit)

## Odd ratio
beta <- coef(fit)
odd <- exp(beta)
odd

pred <- predict(fit,type="response")
pred
pred1 <-round(pred)
pred1
N <- nrow(bank$y) 
conf <- table(pred1,bank$y)
acc <- sum(diag(conf))/sum(conf)
acc  # 88% accuracry rate

plot(predict(fit,type="response"),residuals(fit,type="pearson"))

# H-L test
install.packages("ResourceSelection")
library(ResourceSelection)
hl <- hoslem.test(bank$y,pred,g=10)
hl

# Find the total number of observations
N<-length(bank$y)
N
# Create a vector of length N to store the predicted classes

pred.class1<-rep(NA,N)
for(i in 1:N)
{
  print(i)
  if(pred[i]>0.5) #if the predicted probability is > 0.5
    pred.class1[i]=1
  else
    pred.class1[i]=0
}

head(pred.class1)
pred.class1  

# Cross tabulate the true classes with the predicted classes
tab<-table(bank$y,pred.class1)
tab
# Calculate the accuracy
sum(tab[c(1,4)])/sum(tab) #74% accurate


#################################################################################
# Comparing classifiers using training, validation and test

#Sample 50% of the data as training data
#Sample 25% of the data as validation 
#Let the remaining 25% data be test data

N <- nrow(bank)
indtrain <- sample(1:N,size=0.50*N,replace=FALSE)
indtrain
indtrain <- sort(indtrain)
indtrain
indvalid <- sample(setdiff(1:N,indtrain),size=0.25*N) # lay ra 25% ramdom
indvalid <- sort(indvalid)
indvalid
indtest <- setdiff(1:N,union(indtrain,indvalid))
indtest

#Load the rpart and partykit libraries
install.packages("rpart")
library(rpart)
library(partykit)

#Fit a classifier to only the training data (50% data)
fit.r <- rpart(y~.,data=bank,subset=indtrain)
plot(as.party(fit.r))

#Fit a logistic regression to the training data only too (validation step?)
#First load the nnet package
??nnet
library(nnet)
fit.l <- multinom(y~., data=bank,subset=indtrain)

#Classify for ALL of the observations
pred.r <- predict(fit.r,type="class",newdata=bank)
pred.r
pred.l <- predict(fit.l,type="class",newdata=bank)
pred.l

#Look at table for the validation data only (rows=truth, cols=prediction)
tab.r <- table(bank$y[indvalid],pred.r[indvalid])
tab.r
tab.l <- table(bank$y[indvalid],pred.l[indvalid])
tab.l

#Work out the accuracy
acc.r <- sum(diag(tab.r))/sum(tab.r)
acc.l <- sum(diag(tab.l))/sum(tab.l)

acc.r # accuracy: 89% (applied for ALL observation)
acc.l # accuracy: 89% (applied for ALL observation )

#Look at the method that did best on the validation data 
#when applied to the test data
if (acc.r>acc.l)
{
  tab <- table(bank$y[indtest],pred.r[indtest])
}else
{
  tab <- table(bank$y[indtest],pred.l[indtest])
}

tab

sum(diag(tab))/sum(tab)  # accuracy: 89% (TH mo hinh applied cho test data)

#Let's repeat this process 

#Set up res to store results

res<-matrix(NA,100,4)
res

#Start simulation to look at this 
iterlim <- 100
for (iter in 1:iterlim)
{
  # Sample 50% of the data as training data
  #Sample 25% of the data as validation 
  #Let the remaining 25% data be test data
  
  N <- nrow(bank)
  indtrain <- sample(1:N,size=0.50*N,replace=FALSE)
  indtrain <- sort(indtrain)
  indvalid <- sample(setdiff(1:N,indtrain),size=0.25*N)
  indvalid <- sort(indvalid)
  indtest <- setdiff(1:N,union(indtrain,indvalid))
  
  #Fit a classifier to only the training data
  fit.r <- rpart(y~.,data=bank,subset=indtrain)
  
  #Fit a logistic regression to the training data only too
  fit.l <- multinom(y~., data=bank,subset=indtrain)
  
  #Classify for ALL of the observations
  pred.r <- predict(fit.r,type="class",newdata=bank)
  pred.l <- predict(fit.l,type="class",newdata=bank)
  
  #Look at table for the validation data only (rows=truth, cols=prediction)
  tab.r <- table(bank$y[indvalid],pred.r[indvalid])
  tab.l <- table(bank$y[indvalid],pred.l[indvalid])
  
  #Work out the accuracy
  acc.r <- sum(diag(tab.r))/sum(tab.r)
  acc.l <- sum(diag(tab.l))/sum(tab.l)
  
  #Store the results
  res[iter,1] <- acc.r 
  res[iter,2] <- acc.l
  
  #Look at the method that did best on the validation data 
  #when applied to the test data
  if (acc.r>acc.l)
  {
    tab <- table(bank$y[indtest],pred.r[indtest])
    acc <- sum(diag(tab))/sum(tab)
    res[iter,3] <- 1 # if acc.r is greater than acc.l,  then columme 3 is choosen columme that receives # 1 
    res[iter,4] <- acc # shown accuracy on test data set of tree analysis
  }else
  {
    tab <- table(bank$y[indtest],pred.l[indtest])
    acc <- sum(diag(tab))/sum(tab)
    res[iter,3] <- 2 # if  acc.l greater han acc.r, then columme 3 is choosen columme that receives # 2 
    res[iter,4] <- acc # shown accuracy on test data set of logistic analysis
  }
  
} #iter

res[iter,4] <- acc
acc
#Check out the error rate summary statistics.
colnames(res)<-c("valid.r","valid.l","chosen","test")
res
table(res[,3]) # tree analysis is choosen for 27 times, logistic is choosen for 73 times (logistic model gives more accuracy for many tests/trials than tree model)
summary(res[which(res[,3]==1),4]) # times that tree model is chosen, min/lowest accuracy is 87.36%, max/highest accuracy is  90%
summary(res[which(res[,3]==2),4]) # times that logistic model is chosen, min/lowest accuracy is 87.53%, max/highest accuracy is  90%

##############################################################################
##bagging and Random forest
library(adabag)
library(randomForest)

# process data
bank <- bank[, - 12]
bank$Pre_Campaign <- as.factor(bank$Pre_Campaign)

#Split data into three sets
N<-nrow(bank)
trainind<-sort(sample(1:N,size=floor(N*0.70)))
nottestind<-setdiff(1:N,trainind)
validind<-sort(sample(nottestind,size=length(nottestind)/2))
testind<-sort(setdiff(nottestind,validind))

#Fit to test and asses performance on validation
fitcart<-rpart(y~.,data=bank,subset=trainind)
pred<-predict(fitcart,type="class",newdata=bank)
tab<-table(bank$y[validind],pred[validind])
tab

#no yes
#no  583   7
#yes  72  16

sum(diag(tab)) # 599

########################################################################################
#BAGGING

fitbag<-bagging(y~.,data=bank[trainind,]) # BAGGING
pred<-predict(fitbag,type="class",newdata=bank[validind,])
tab<-pred$confusion
tab
sum(diag(tab))/sum(tab) # 88.5% accuracy rate (nho hon 0.4% so voi pp logistic and tree )

#Fit to test and asses performance on validation
fitrf<-randomForest(y~.,data=bank,subset=trainind)
pred<-predict(fitrf,type="class",newdata=bank)
tab<-table(bank$y[validind],pred[validind], dnn = c("actual", "predict"))
tab
sum(diag(tab))/sum(tab) # 88.6% accuracy cho pp ramdomforest (nho hon 0.4% so voi pp logistic and tree )


str(bank)
