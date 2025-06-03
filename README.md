# Credit-Data

credit_data <- read.csv(file = "https://xiaoruizhu.github.io/Data-Mining-R/lecture/data/credit_default.csv", header=T)
library(dplyr)
mean(credit_data$default.payment.next.month)
credit_data<- rename(credit_data, default=default.payment.next.month)

str(credit_data)
summary(credit_data)
credit_data$SEX<- as.factor(credit_data$SEX)
credit_data$EDUCATION<- as.factor(credit_data$EDUCATION)
credit_data$MARRIAGE<- as.factor(credit_data$MARRIAGE)
library(boot)

index <- sample(nrow(credit_data),nrow(credit_data)*0.80)
credit_train = credit_data[index,]
credit_test = credit_data[-index,]

credit_glm0 <- glm(default~., family=binomial, data=credit_train)
summary(credit_glm0)




costfunc <- function(obs, pred.p){
  weight1 <- 5 # define the weight for "true=1 but pred=0" (FN)
  weight0 <- 1 # define the weight for "true=0 but pred=1" (FP)
  pcut <- 1/(1+weight1/weight0)
  c1 <- (obs==1)&(pred.p < pcut) # count for "true=1 but pred=0" (FN)
  c0 <- (obs==0)&(pred.p >= pcut) # count for "true=0 but pred=1" (FP)
  cost <- mean(weight1*c1 + weight0*c0) # misclassification with weight
  return(cost) # you have to return to a value when you write R functions
} # end

library(boot)
credit_glm1<- glm(default~. , family=binomial, data=credit_data);
cv_result <- cv.glm(data=credit_data, glmfit=credit_glm1, cost=costfunc, K=
                      10)
cv_result$delta[2]
