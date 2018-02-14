library(xlsx)
library(KoNLP)

#---------------------#file input#---------------------#
setwd("/Users/hodong/Desktop/kaggle_titanic/raw_data")
tt_train <- data.frame(read.csv("train.csv",header=TRUE))
tt_test <- data.frame(read.csv("test.csv",header=TRUE))
#tt_result <- data.frame(read.csv("gender_submission.csv",header=TRUE))
#---------------------#file input#---------------------#


#---------------------#Data preparation#---------------------#
arrange_feature <- function(x){
  x <- x[!is.na(x$Age),]
  x <- x[!is.na(x$Fare),]
  
  x$Pclass <- ((x$Pclass-min(x$Pclass))/(max(x$Pclass)-min(x$Pclass)))
  x$Sex <- ifelse(x$Sex=="male",1,0)
  x$Age <- ((x$Age-min(x$Age))/(max(x$Age)-min(x$Age)))
  x$SibSp <- ((x$SibSp-min(x$SibSp))/(max(x$SibSp)-min(x$SibSp)))
  x$Parch <- ((x$Parch-min(x$Parch))/(max(x$Parch)-min(x$Parch)))
  x$Fare <- ((x$Fare-min(x$Fare))/(max(x$Fare)-min(x$Fare)))
  x$Che <- ifelse(x$Embarked=="C",1,0)
  x$Sou <- ifelse(x$Embarked=="S",1,0)
  x$Que <- ifelse(x$Embarked=="Q",1,0)
  return(x)
}

tt_train <- tt_train[,-c(4,9,11)]
tt_train <- arrange_feature(tt_train)
tt_train_pi <- tt_train$PassengerId
tt_train <- tt_train[,-1]
tt_train_labels <- as.factor(tt_train$Survived)
tt_train <- tt_train[,-1]
tt_train <- tt_train[,-7]
#tt_train_labels
names(tt_train)

tt_test <- tt_test[,-c(3,8,10)]
tt_ori_pi <- tt_test$PassengerId
tt_test <- arrange_feature(tt_test)
tt_test_pi <- tt_test$PassengerId
tt_test <- tt_test[,-1]
tt_test <- tt_test[,-7]
names(tt_test)
#---------------------#Data preparation#---------------------#


#---------------------#k-NN classification#---------------------#
library(class)
k <- floor(sqrt(nrow(tt_train))) #k값 설정1
#k <- 10 #k값 설정2
tt_pred <- knn(train=tt_train,test=tt_test,cl=tt_train_labels, k=k)

dim(tt_test)
length(tt_test_pi)
length(tt_pred)

#결과 테이블 작성
make_sub <- function(x){
  submission <- data.frame(cbind(tt_ori_pi,x))
  names(submission) <- c("PassengerId","Survived")
  temp <- data.frame(cbind(as.numeric(tt_test_pi),as.numeric(as.vector(tt_pred))))
  names(temp) <- c("PassengerId","Survived")
  
  for(i in 1:nrow(temp)){
    if(temp[i,]$PassengerId %in% submission$PassengerId){
      submission[which(submission$PassengerId==temp[i,]$PassengerId),]$Survived <- temp[i,]$Survived
    }
  }
  
  return(submission)
}

submission1 <- make_sub(0) #k값 설정1, 결측지 0
submission2 <- make_sub(1) #k값 설정1, 결측지 1
submission3 <- make_sub(0) #k값 설정2, 결측지 0
submission4 <- make_sub(1) #k값 설정2, 결측지 1

nrow(submission1[which(submission1$Survived==1),])
nrow(submission1[which(submission1$Survived==0),])

nrow(submission2[which(submission2$Survived==1),])
nrow(submission2[which(submission2$Survived==0),])

nrow(submission3[which(submission3$Survived==1),])
nrow(submission3[which(submission3$Survived==0),])

nrow(submission4[which(submission4$Survived==1),])
nrow(submission4[which(submission4$Survived==0),])


write.csv(submission1, file="/Users/hodong/Desktop/titanic_result1.csv",row.names=FALSE) #0.77990 #BEST
write.csv(submission2, file="/Users/hodong/Desktop/titanic_result2.csv",row.names=FALSE) #0.65550
write.csv(submission3, file="/Users/hodong/Desktop/titanic_result3.csv",row.names=FALSE) #0.77033
write.csv(submission4, file="/Users/hodong/Desktop/titanic_result4.csv",row.names=FALSE) #0.64593
#---------------------#k-NN classification#---------------------#

#---------------------#Decision Tree#---------------------#
library(C50)

#결정트리 모델 생성
tt_dt_model <- C5.0(tt_train, tt_train_labels)
tt_dt_model
summary(tt_dt_model) #결과 확인
submission1 <- predict(tt_dt_model, tt_test) #기본 DT

#모델 성능 개선하기
tt_dt_model_b <- C5.0(tt_train, tt_train_labels,trials=10)
tt_dt_model_b
summary(tt_dt_model_b) #결과 확인
submission2 <- predict(tt_dt_model_b, tt_test) #10번 종합하여 boost

#오류에 따른 비용 가중치 설정하기
matrix_d <- list(c("0","1"),c("0","1"))
names(matrix_d) <- c("predicted","actual")
error_cost <- matrix(c(0,4,1,0),nrow=2,dimnames=matrix_d)
error_cost
tt_dt_model_w <- C5.0(tt_train, tt_train_labels,costs=error_cost)
tt_dt_model_w
summary(tt_dt_model_w)
submission3 <- predict(tt_dt_model_w, tt_test) #오류에 따른 가중치 설정


write.csv(submission1, file="/Users/hodong/Desktop/titanic_result1.csv",row.names=FALSE) #0.77033
write.csv(submission2, file="/Users/hodong/Desktop/titanic_result2.csv",row.names=FALSE) #0.77990 #BEST
write.csv(submission3, file="/Users/hodong/Desktop/titanic_result3.csv",row.names=FALSE) #0.77990 #BEST
#---------------------#Decision Tree#---------------------#















