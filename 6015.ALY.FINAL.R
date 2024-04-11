chrun1<-read.csv("/Users/architbarua/Desktop/ALY 6015/Churn_Modelling.csv",
                  header=TRUE,sep=",")

complete.cases(chrun1)
which(!complete.cases(chrun1)) 
duplicated(chrun1)
anyDuplicated(chrun1)


chrun1$Balance<-round(chrun1$Balance,0)
chrun1$EstimatedSalary<-round(chrun1$EstimatedSalary,0)

chrun1$Status<-ifelse(chrun1$Exited==
                        "1","Churned","Retained")

chrun1<-chrun1 %>% mutate(HasCrCard = recode(HasCrCard, '0'='No', '1'='Yes'))
chrun1<-chrun1 %>% mutate(IsActiveMember = recode(IsActiveMember, '0'='Inactive', '1'='Active'))

colnames(chrun1)[11]<-"Credit_Card"
colnames(chrun1)[12]<-"Membership"

str(chrun1)

chrun1$Geography<-as.factor(chrun1$Geography)
chrun1$Gender<-as.factor(chrun1$Gender)
chrun1$Credit_Card<-as.factor(chrun1$Credit_Card)
chrun1$Membership<-as.factor(chrun1$Membership)
chrun1$Status<-as.factor(chrun1$Status)


str(chrun1)
view(chrun1)
glimpse(chrun1)
headTail(chrun1)
dim(chrun1)
summary(chrun1)
mean(chrun1$Age)
#Hypothesis Testing 
#one sample test
#null hypothesis: mean credit score is greater than 600
#alternate hypothesis: mean credit score is <= 600
t.test(chrun1$CreditScore, mu=600, alternative = "less")


#null hypothesis: mean age is less than 50
#alternate hypothesis: mean age  is >= 50
t.test(chrun1$Age, mu=50, alternative = "greater")

Male<-subset(chrun1,Gender=="Male")
Female<-subset(chrun1,Gender=="Female")

#null hypothesis: both male and female have same mean credit score.
#alternate hypothesis: both male and female do not have same mean credit score.
t.test(Male$CreditScore, Female$CreditScore, var.equal=TRUE)


#null hypothesis: both male and female have same mean salary.
#alternate hypothesis: both male and female do not have same mean salary.
t.test(Male$EstimatedSalary, Female$EstimatedSalary, var.equal=TRUE)

var.test(Male$EstimatedSalary, Female$EstimatedSalary, alternative = "two.sided")

#Logistic Regression 
set.seed(3456) 
split.data1 <- createDataPartition(chrun1$Exited, p = 0.7, list = FALSE, times = 1) 
data_train <- chrun1[ split.data1,] 
data_test <- chrun1[-split.data1,] 

logistic.m1 <- glm(Exited~CreditScore + Geography + Gender + Age + Tenure +
                     Balance + NumOfProducts + Credit_Card + Membership + EstimatedSalary, data=data_train, family = "binomial")
summary(logistic.m1)

logistic.m2 <- glm(Exited~Balance + Age + Membership + Gender , data=data_train, family = "binomial")
summary(logistic.m2)

#Confusion matrix
#train data
prob.train <- predict(logistic.m2, newdata=data_train, type="response")
predicted <- as.factor(ifelse(prob.train>=0.5, "Yes", "No"))
data_train$Exited <- as.factor(data_train$Exited)
data_train$Exited <- factor(ifelse(data_train$Exited==1, "Yes", "No"))
confusionMatrix(predicted, data_train$Exited, positive = "Yes")
#test data
prob.test <- predict(logistic.m2, newdata = data_test, type="response")                
predicted1<- as.factor(ifelse(prob.test>=0.5, "Yes", "No"))                
data_test$Exited <- as.factor(data_test$Exited)
data_test$Exited <- factor(ifelse(data_test$Exited==1, "Yes", "No"))
confusionMatrix(predicted1, data_test$Exited, positive = "Yes")


ROC <- roc(data_train$Exited, prob.train)
plot(ROC, col="red", ylab="Sensitivity - TP Rate", xlab= "Specificity - Fp Rate")

#AUC
AUC1 <- auc(ROC)










#Data Viz
#Histogram fill by gender (or we can change to fill by other variables)
hist5<-ggplot(chrun1, aes(x=CreditScore, fill=Gender),labels=TRUE) +geom_histogram(binwidth=50, alpha=.5,colour="black") +scale_x_continuous(breaks=0:5)+ggtitle("Histogram of Credit Score by Gender")+geom_vline(aes(xintercept=mean(CreditScore, na.rm=T)),color="red", linetype="dashed", size=1) 
hist6<-ggplot(chrun1, aes(x=Age, fill=Gender),labels=TRUE) +geom_histogram(binwidth=5, alpha=.5,colour="black") +scale_x_continuous(breaks=0:5)+ggtitle("Histogram of Credit Score by Gender")+geom_vline(aes(xintercept=mean(Age, na.rm=T)),color="red", linetype="dashed", size=1)
hist7<-ggplot(chrun1, aes(x=Balance, fill=Gender),labels=TRUE) +geom_histogram(binwidth=10000, alpha=.5,colour="black") +scale_x_continuous(breaks=0:5)+ggtitle("Histogram of balance by Gender")+geom_vline(aes(xintercept=mean(Balance, na.rm=T)),color="red", linetype="dashed", size=1) 
hist8<-ggplot(chrun1, aes(x=EstimatedSalary, fill=Gender),labels=TRUE) +geom_histogram(binwidth=10000, alpha=.5,colour="black") +scale_x_continuous(breaks=0:5)+ggtitle("Histogram of Estimated Salary by Gender")+geom_vline(aes(xintercept=mean(EstimatedSalary, na.rm=T)),color="red", linetype="dashed", size=1) 
grid.arrange(hist5,hist6,hist7,hist8,top=textGrob("Histograms of Variables"))


# QQplots
qqnorm(chrun1$CreditScore, pch = 1, frame = FALSE,main="Q-Q Plot (Credit score)")
qqline(chrun1$CreditScore, col = "yellowgreen", lwd = 2)
qqnorm(chrun1$Age, pch = 1, frame = FALSE,main="Q-Q Plot (Age)")
qqline(chrun1$Age, col = "cornflowerblue", lwd = 2)
qqnorm(chrun1$Balance, pch = 1, frame = FALSE,main="Q-Q Plot (Balance)")
qqline(chrun1$Balance, col = "tomato2", lwd = 2)
qqnorm(chrun1$EstimatedSalary, pch = 1, frame = FALSE,main="Q-Q Plot (Estimated Salary)")
qqline(chrun1$EstimatedSalary, col = "tomato2", lwd = 2)

#Corr
install.packages("GGally")
library(GGally)
install.packages("plotly")
library(plotly)
install.packages("webr")
library(webr)
corr <- select_if(chrun1, is.numeric)
cormatrix<-round(cor(corr,method = "pearson"),digits=2)
corrplot(cormatrix, method="pie")
ggpairs(corr)
ggcorr(corr,hjust = 1,layout.exp=2, size = 3)

#3D
plot_ly(chrun1, x = ~Age, y = ~EstimatedSalary, z = ~CreditScore,mode = 'markers',marker = list(size = 6),color = ~Status,alpha=0.9)

### categorical pie chart = HasCrcard
pie1 <- chrun1 %>%
  group_by(Status, Credit_Card) %>%
  summarize(Freq=n())
PieDonut(pie1, aes(Credit_Card, Status, count=Freq), title = "Churned by HasCrcard")### categorical pie chart = IsActiveMember
pie2 <- chrun1 %>%
  group_by(Status, IsActiveMember) %>%
  summarize(Freq=n())
PieDonut(pie2, aes(IsActiveMember, Status, count=Freq), title = "Churned by IsActiveMember")

Age.hist <- ggplot(chrun1, aes(x=Age, fill=Status, color=Status)) + geom_histogram(position="identity", alpha=0.5)+ theme(axis.title.x=element_blank())+ theme(legend.position = c(0.9, 0.5))
Age.box <- ggplot(chrun1, aes(x=Age, y=Status, fill=Status)) + geom_boxplot(alpha=0.5)+theme(legend.position = "none")
grid.arrange(Age.hist,Age.box,nrow=2)

#Ridge
library(glmnet)
drop<-c("Surname","CustomerId")
churn2<-chrun1[,!(names(chrun1) %in% drop)]

set.seed(3456) 
split.data1 <- createDataPartition(churn2$Exited, p = 0.7, list = FALSE, times = 1) 
data_train <- churn2[ split.data1,] 
data_test <- churn2[-split.data1,] 


train.x <- model.matrix(Exited~., data_train)[,-1]
test.x <- model.matrix(Exited~., data_test)[,-1]
train.y <- data_train$Exited 
test.y <- data_test$Exited

ridge1 <- cv.glmnet(train.x, train.y,alpha=0, nfolds=10)

ridge1$lambda.min
ridge1$lambda.1se
plot(ridge1)

mod.ridge1.min <- glmnet(train.x, train.y, alpha = 0, lambda =ridge1$lambda.min)
coef(mod.ridge1.min)

mod.ridge1.1se <- glmnet(train.x, train.y, alpha = 0, lambda =ridge1$lambda.1se)
coef(mod.ridge1.1se)


#Determine the performance of the fit model against the training set by calculating the root mean square error (RMSE). sqrt(mean((actual - predicted)^2))
#Train set
library(Metrics)
pred.ridge1.min <- predict(mod.ridge1.min, newx = train.x)
train.ridge1.rmse.min <- rmse(train.y, pred.ridge1.min)

pred.ridge1.1se <- predict(mod.ridge1.1se, newx = train.x)
train.ridge1.rmse.1se <- rmse(train.y, pred.ridge1.1se)

#Test set
pred.ridge1.min.test <- predict(mod.ridge1.min, newx = test.x)
ridge1.rmse.min.test <- rmse(test.y, pred.ridge1.min.test)

pred.ridge1.1se.test <- predict(mod.ridge1.1se, newx = test.x)
ridge1.rmse.1se.test <- rmse(test.y, pred.ridge1.1se.test)

#Lasso
lasso1 <- cv.glmnet(train.x, train.y, nfolds=10)

lasso1$lambda.min
lasso1$lambda.1se
plot(lasso1)

install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

D.tree = rpart(Exited ~Age+Balance+Geography+IsActiveMember+NumOfProducts, data = data_train, method = "class")
printcp(D.tree)

prp(D.tree, type = 2, extra = 1, under = TRUE, split.font = 2,border.col = 2, varlen = 0) 
