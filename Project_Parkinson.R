library(randomForest)
library(ggplot2)
library(rpart)

setwd("~/Desktop/Υπολογιστική Φυσική/C_semister/Βιοφυσική")
p<-read.csv('parkinsons_updrs.data.csv', header=TRUE)
# remove motor UPDRS to make the data sigle variate regression
p<-data.frame(p[,-5])
str(p)
summary(p$subject.) #vriskei to plhthos twn hxografhsewn tou kathe asthenh
table(p$subject.)

##### boxplot for total UPDRS by different subjects#####
fill <- "green"
line <- "black"
ggplot(p, aes(x =as.factor(subject.), y =total_UPDRS)) +
  geom_boxplot(fill = fill, colour = line) +
  scale_y_continuous(name = "total UPDRS",
                     breaks = seq(5, 60, 0.5),
                     limits=c(5, 60)) +
  scale_x_discrete(name = "Subject") +
  ggtitle("Boxplot of totalUPDRS and subject")

##### boxplot for total UPDRS by different ages#####
fill <- "green"
line <- "black"
ggplot(p, aes(x =as.factor(age), y =total_UPDRS)) +
  geom_boxplot(fill = fill, colour = line) +
  scale_y_continuous(name = "total UPDRS",
                     breaks = seq(5, 60, 0.5),
                     limits=c(5, 60)) +
  scale_x_discrete(name = "Age") +
  ggtitle("Boxplot of totalUPDRS and age")



#####Training a model on data#####
p<-data.frame(p[,-1])  # remove subject
set.seed(350)
indx = sample(1:nrow(p), as.integer(0.9*nrow(p)))
indx[1:10]
p_train = p[indx,]
p_test = p[-indx,]

rf<- randomForest(total_UPDRS~ ., data =p_train )
rf
plot(rf)
rfr<- rpart(total_UPDRS~ ., data =p_train )
rfr
library(rpart.plot)
rpart.plot(rfr)

importance(rf)
varImpPlot(rf)


#####Evaluating model performance#####
pred<-predict(rf,p_test,type='response')
head(pred) #emfanizei ta apotelesmata tou panw
plot(pred,p_test$total_UPDRS,main="Comparison chart of predicted 
     & actual total_UPDRS from test sample",
     xlab="Predicted total_UPDRS",
     ylab="Actual total_UPDRS")

#Compare the distribution of predict value and actual value
summary(pred)
summary(p$total_UPDRS)

#Compare the correlation between predicted and actual total UPDRS.
cor(pred,p_test$total_UPDRS)

#####MEAN ABSOLUTE ERROR - CROSS VALIDATION#####
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))  
}
MAE(pred, p_test$total_UPDRS)
#This implies that, on average, the difference between our model’s predictions
#and the true total UPDRS score was about 1.36. On a quality scale from zero to
#10, this seems to suggest that our model is doing fairly well.

#Improve model performance
rf1<- randomForest(total_UPDRS~ ., data =p_train,mtry=10)
rf1
plot(rf1)
importance(rf1)
varImpPlot(rf1)

#####Making predictions:#####
pred1<-predict(rf1,p_test,type='response')
head(pred1)
plot(pred1,p_test$total_UPDRS,main="Comparison chart of predicted 
     & actual total_UPDRS from test sample after improvement",
     xlab="Predicted total_UPDRS",
     ylab="Actual total_UPDRS")
head(p_test$total_UPDRS)

summary(pred1)
summary(p$total_UPDRS)

#Compare the correlation between predicted and actual total UPDRS.
cor(pred1,p_test$total_UPDRS)

#Mean absolute error between predicted and actual values:
MAE(pred1, p_test$total_UPDRS)
summary(p$age)
