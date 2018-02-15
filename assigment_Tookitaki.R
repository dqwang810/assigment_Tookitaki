library(plyr)
library(FSelector)
library(ROSE)
library(unbalanced)
library(rpart)
library(randomForest)

datafile <- 'C:\\Daqing\\Tookitaki\\test data\\raw_data_70_new.csv'
testfile <- 'C:\\Daqing\\Tookitaki\\test data\\raw_data_30_new.csv'

df <- read.csv(datafile,header=T, na.strings=c("","NA"))
df_test <- read.csv(testfile,header=T, na.strings=c("","NA"))
df[,c(1:3)] <- NULL
df_test[,c(1:3)] <- NULL


#---------------filled NA with mean-------------------
impute_mean <- function(data)
{
  for(i in 1:ncol(data))
  {
    data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
  }
  return(data)
}

#---------------main------------------------------
#---------------select numeric-------------------
nums <- sapply(df, is.numeric)
df <- df[,nums]
df_test <- df_test[,nums]
#--------delete missing >=50%-------------------------------
count_na <- colSums(is.na(df))/nrow(df)*100
unused <- c(which(count_na>=50))
df[,unused] <- NULL
df_test[,unused] <- NULL


df = impute_mean(df)
df_test = impute_mean(df_test)
#-------------------------------------------------------------

credit_class <- as.data.frame(df[,c('Bad_label')])
names(credit_class) <- 'class'

summarise(credit_class, 
          "Frequencies"= count(credit_class), 
          "Percent" = count(credit_class)[,2]/ sum(count(credit_class)[2]))

#----------get RIG---------------------

f <-  as.simple.formula(credit_class, "Bad_flag")
RIG <- gain.ratio(Bad_label~., df,unit = "log2")


top_f <- cutoff.k(RIG, 10)
top_rate <- RIG[top_f,]

t <- data.frame(top_f,top_rate)
names(t) <- c('Feacture','RIG')

df1 <- df[,c('Bad_label',top_f)]
df1_test <- df_test[,c('Bad_label',top_f)]


#data_over <- ovun.sample(Bad_label ~ ., data = df1, method = "over",N = 22892*2)$data
#table(data_over$Bad_label)
#--------------- balance data---------------------------------------
class_y <- as.factor(df1[,c('Bad_label')])
data_new<-ubBalance(X= df1[,top_f], Y=class_y, type="ubSMOTE", percOver=200, percUnder=200, verbose=TRUE)
data_b <- cbind(data_new$Y,data_new$X)
names(data_b)[1]<- 'Bad_label'
table(data_b$Bad_label)

# model1 <- rpart(Bad_label ~ ., data = data_b)
# pred_model1 <- predict(model1, newdata = df1_test)
# accuracy.meas(df1_test$Bad_label, pred_model1[,2])
# roc.curve(df1_test$Bad_label, pred_model1[,2])

model2 <- randomForest(Bad_label ~ ., data = data_b)
pred_model2 <- predict(model2, newdata = df1_test)
df1_test[,c('Bad_label')] <- as.factor(df1_test[,c('Bad_label')])
accuracy.meas(df1_test$Bad_label, pred_model2)
roc.curve(df1_test$Bad_label, pred_model2)
