options(scipen=999999)
options (max.print= 10000)

## Task (a) ##
#create rating_data frame
ratingColname<- c( "userID", "movieID", "rating", "timeRating")
ratingColclass <- c( "factor","factor","numeric","factor")
rating_data <- read.table("H:/.das/Desktop/R file/ml-100k/u.data",
col.names= ratingColname,colClass = ratingColclass)
rating_data$M<- "X"
rating_data$movieID<- paste(rating_data$M,rating_data$movieID,sep="")
summary (rating_data)

#create users_data frame
usersColname <- c("userID", "age","gender","occupation","ZIPcode")
usersColclass<- c("factor","numeric","factor","factor","factor")
users_data <- read.table("H:/.das/Desktop/R file/ml-100k/u.user", sep= "|",
col.names= usersColname,colClass= usersColclass)
summary (users_data)

#create movies_data frame
movieColclass<- c("factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor",
"factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor")
movies_data <- read.table("H:/.das/Desktop/R file/ml-100k/u.item",quote= "",sep ="|",
colClass=movieColclass)
#repair data
movies_data$V5 <- NULL
movies_data$V4<- NULL
#create column names to movie_data
colnames(movies_data)<- c("movieID","titleRelease","videDate","unknown","action","adventure","animation","children","comedy","crime",
"documentary","drama","fantasy"," film-Noir","horror","musical","mystery","romance","sci-Fi","thriller","war","western")
movies_data$M<- "X"
movies_data$movieID<- paste(movies_data$M,movies_data$movieID,sep="")

#extract release data from M_data$titleRelease
#install.packages("stringr", repos='http://cran.us.r-project.org')
library("stringr")
movies_data$relDate<- str_sub(movies_data$titleRelease,-5,-2)
movies_data$relDate<- as.factor(movies_data$relDate)
summary (movies_data )

#create rating_matrix data frame
rating_matrix <- rating_data
rating_matrix$timeRating<- NULL
rating_matrix$M<- NULL
#install.packages("reshape")
library(reshape)
rating_matrix <-cast(rating_matrix,userID~movieID)
summary(rating_matrix)




##Task (b) ##
#create user_expl_data
user_expl_data<- users_data
avgU<- aggregate( rating~userID,data=rating_data,mean)
user_expl_data<- merge(user_expl_data,avgU,by.user_expl_data="userID",by.avgU="userID")
colnames(user_expl_data) [6] <- "avgUserRate"
#install.packages("plyr")
library(plyr)
avgU<- count(rating_data,"userID")
user_expl_data<- merge(user_expl_data,avgU,by.user_expl_data="userID",by.avgU="userID")
colnames(user_expl_data) [7] <- "No.movie"
summary(user_expl_data)

#repair data (outliers) Three upper value of outlier was tested below 
#and we need to check what the data behaved after removing outliers.
boxplot(user_expl_data$No.movie)
boxplot(user_expl_data$avgUserRate)
#Remove data above 278
U1_data <- user_expl_data[user_expl_data$No.movie <=278 ,]
summary(U1_data)
#Remove data above 700
U2_data <- user_expl_data[user_expl_data$No.movie <=700 ,]
summary(U2_data)
#Remove data above 480
U3_data<- user_expl_data[user_expl_data$No.movie <=480 ,]
summary(U3_data)
#we select U3_data as the final data frame of user_expl_data
U_data<- U3_data
boxplot (U_data$No.movie)
U_data <- user_expl_data[user_expl_data$avgUserRate <=4.5 &user_expl_data$avgUserRate >= 2.56,]
boxplot (U_data$avgUserRate)
summary(U_data)


#create movie_expl_data
movie_expl_data<- movies_data
M_data<- count(rating_data,"movieID")
movie_expl_data<- merge(movie_expl_data,M_data, by.movie_expl_data="movieID", by.M_data="movieID")
colnames(movie_expl_data)[24]<- "No.users"
M_data <- aggregate(rating~movieID, data=rating_data,mean)
movie_expl_data<- merge(movie_expl_data,M_data, by.movie_expl_data="movieID", by.M_data="movieID")
colnames(movie_expl_data)[25] <- "avgMovieRate"
summary(movie_expl_data)

#repair data
#remove duplicated rows
movie_expl_data<- movie_expl_data[!duplicated(movie_expl_data[,"titleRelease"]),]
#Three upper value of outliers (number of users)was tested below 
#and we need to check what the data behaved after removing outliers.
#Remove data above 450
M1_data<- movie_expl_data[movie_expl_data$No.users <= 450, ]
summary (M1_data)
#Remove data above 250
M2_data<- movie_expl_data[movie_expl_data$No.users <= 250, ]
summary (M2_data)
boxplot(M2_data$No.users )
#Remove data above 150
M3_data<- movie_expl_data[movie_expl_data$No.users <= 150, ]
summary (M3_data)

#Remove outliers of average movie rate
boxplot(movie_expl_data$avgMovieRate)
#only one outlier was observed which the data below 1
M4_data <- M3_data [M3_data$avgMovieRate >=1, ]
summary (M4_data)
boxplot(M4_data$avgMovieRate)

#remove unkown movies
M5_data <- M4_data[M4_data$unknown!= 1, ]
summary (M5_data)


#create rating_expl_data
rating_expl_data <- rating_data
rating_expl_data<- merge(rating_expl_data ,user_expl_data,by="userID",all.rating_expl_data=TRUE)
rating_expl_data<- merge(rating_expl_data,movie_expl_data,by="movieID",all.rating_expl_data=TRUE)
summary(rating_expl_data)


##Task (c)##
#(i)predict average rating for movie
#Create fucntion of predicting average movie rating 
pre_movie<- function (train_data, test_data){
avgM_train<- aggregate(rating~movieID, data= train_data, mean)
colnames(avgM_train)<- c("movieID","pre")
pre_rat<- merge (avgM_train,test_data, by="movieID",all.pre_rat= TRUE)
pre_rat[pre_rat$pre<1,]<- 1
pre_rat[pre_rat$pre>5,]<-5
return(pre_rat)}


#(ii)predict average rating of the user 
#Create fucntion of predicting average user rating
pre_user<- function (train_data, test_data){
avgU_train<- aggregate(rating~userID, data= train_data, mean)
colnames(avgU_train)<- c("userID","pre")
pre_rat<- merge (avgU_train,test_data, by="userID",all.pre_rat= TRUE)
pre_rat[pre_rat$pre<1,]<- 1
pre_rat[pre_rat$pre>5,]<-5
return(pre_rat)}


#(iii)predict double mean 
#calculate A bar i.e the mean of all ratings in A
Abar<- colMeans(rating_matrix, na.rm= TRUE)
Abar<- mean(Abar)
#we obatin A bar is 3.076045
#Create function for double mean
pre_DM<- function(train_data,test_data){
avgM<- aggregate(rating~movieID,data=train_data,mean)
colnames(avgM)<-c("movieID","avgM")
avgU<- aggregate(rating~userID,data=train_data,mean)
colnames(avgU)<- c("userID","avgU")
pre_avgM<-merge(avgM,test_data, by="movieID")
pre_avgU<- merge(avgU,test_data,by="userID")
pre_rat<- t(sapply(pre_avgM[,"avgM"],function(x) x+ pre_avgU[,"avgU"]-3.076045))
colnames(pre_rat)<- pre_avgM[,"movieID"]
pre_rat<- data.frame(userID= pre_avgU[,"userID"],pre_rat)
pre_rat[pre_rat$avgU <1,]<- 1
pre_rat[pre_rat$avgU>5,]<-5
return(pre_rat)}


#(iv)least square estimation

lse<-function(train_data,test_data){
train_data<- train_data[,c("userID","movieID","rating")]
train_data<- data.matrix( cast(train_data, userID~movieID))	
A<- train_data
A[is.na(A)]<-0
Amat<- data.matrix(A)
M<- ifelse(A==0,0,1) 
M<- data.matrix(M)
v<- as.vector(rep(1,ncol(Amat)))
w<- matrix(rep(1,nrow(Amat)))
Av<- Amat%*%v
Atw<-t(Amat)%*%w
Mv<- M%*%v
Mv<-as.vector(Mv)
Mtw<- t(M)%*%w
Mtw<- as.vector(Mtw)
S<- diag(Mv)
P<- diag(Mtw)
B2<- rbind(Av,Atw)
B1a<- cbind(S,M)
B1b<- cbind (t(M),P)
B1<- rbind (B1a,B1b)
B3<- ginv (B1)
estRS<- B3%*%B2
rHat<- data.matrix (estRS[1:nrow(Amat),])
sHat<- data.matrix(estRS[(nrow(Amat)+1):nrow(estRS),])
AijHat<- data.frame(t(sapply(rHat[,1], function(x) x+sHat[,1])))
AijHat$userID<- rep(1:nrow(Amat))
pre_rat<- ifelse( pre_rat<1, 1, ifelse(pre_rat>5,5,pre_rat))
return(pre_rat)
}


#(v)lest square estimation obtained from minimization of RSS
mlse <- function(train_data,test_data)  {
avgM1 <- aggregate(rating~movieID, data=train_data,mean)
names(avgM1) <- c("movieID","avgMrat")
avgM <- merge(train_data, avgM1, by="movieID")
avgU <- aggregate(rating~userID, data=train_data,mean)
names(avgU) <- c("userID","avgUrat")
data <- merge(avgU, avgM, by="userID")

#create the residual sum of square (RSS) function
rss<- function(x) sum((data[,"rating"]-x[1]*data[,"avgUrat"]- x[2]*data[,"avgMrat"])^2)
ans1 <- nlm(rss,c(1,1))

#use nlm internally to obtain the estimate
 c <- ans1$estimate[1]
 d <- ans1$estimate[2]
test_data1 <- merge(test_data, avgM1, by= "movieID",all.test_data=TRUE)
test_data1[is.na(test_data1$avgMrat),"avgMrat"] <- mean(train_data$rating)
test_data2 <- merge(test_data1, avgU,by="userID",all.test_data=TRUE)
test_data2[is.na(test_data2$avgUrat),"avgUrat"] <- mean(train_data$rating)
test_data2$pre <- c*test_data2[,"avgUrat"] + d*test_data2[,"avgMrat"]
        
test_data2$pre[test_data2$pre<1] <- 1
test_data2$pre[test_data2$pre>5] <- 5 
names(test_data2$pre) <- "pre"
pre_rat<- test_data2

return(pre_rat)

}



## Task 1 (d)##
#Five fold cross-validation with uniform sub-sampling#
#Divide data into 5 chunks
rn1 <- sample(seq_len(nrow(rating_expl_data)), size = 19778 )
dfchunk1 <- rating_expl_data[rn1, ]
dfchunk<- rating_expl_data[-rn1, ]

rn2<- sample(seq_len(nrow(dfchunk)), size = 19778 )
dfchunk2 <- rating_expl_data[rn2, ]
dfchunk<- dfchunk[-rn2, ]

rn3<- sample(seq_len(nrow(dfchunk)), size = 19778 )
dfchunk3 <- rating_expl_data[rn3, ]
dfchunk<- dfchunk[-rn3, ]

rn4<- sample(seq_len(nrow(dfchunk)), size = 19778 )
dfchunk4 <- rating_expl_data[rn4, ]
dfchunk5 <- dfchunk[-rn4, ]


#train/test set one. dfchunk 1 was slected as train set and the rest is test set
trainset1<-dfchunk1
testset1<- rbind(dfchunk2,dfchunk3,dfchunk4,dfchunk5)

#train/test set two. dfchunk 3 was slected as train set and the rest is test set
trainset2<-dfchunk2
testset2<- rbind(dfchunk1,dfchunk3,dfchunk4,dfchunk5)

#train/test set three
trainset3<-dfchunk3
testset3<- rbind(dfchunk1,dfchunk2,dfchunk4,dfchunk5)

#train/test set four.dfchunk 4 was slected as train set and the rest is test set
trainset4<-dfchunk4
testset4<- rbind(dfchunk1,dfchunk2,dfchunk4,dfchunk5)

#train/test set five. dfchunk 5 was slected as train set and the rest is test set
trainset5<-dfchunk5
testset5<- rbind(dfchunk1,dfchunk2,dfchunk4,dfchunk5)



######################################################################
#(i) Error Measures for predictor in (c)(i)(average rating for movie)#
######################################################################

#Predicting the average rating for movie
#Find the residuals, RMSE, MAE, relative residuals, relative RMSE, relative MAE
pre_movie_err<- function (train_data,test_data){
avgM_train<- aggregate(rating~movieID, data= train_data, mean)
colnames(avgM_train)<- c("movieID","pre") # 'pre' represent prediction  
avgM_pre<- merge (avgM_train, test_data, by= "movieID", all.test_data= TRUE)
avgM_pre[avgM_pre$pre<1,]<- 1
avgM_pre[avgM_pre$pre>5,]<- 5
avgM_pre$resd<- avgM_pre$rating- avgM_pre$pre
avgM_pre$REresd<- 1- avgM_pre$pre/avgM_pre$rating
RMSE<- sqrt(mean((avgM_pre$resd)^2))
MAE<- mean(abs(avgM_pre$resd))
reRMSE<- sqrt(mean((avgM_pre$REresd)^2))
reMAE<- mean(abs(avgM_pre$REresd))
return(list(RMSE,MAE,reRMSE,reMAE))
}

#Assign train/test set 
#train/test set one
Mtrainset1<- trainset1
Mtestset1<- testset1

#train/test set two
Mtrainset2<- trainset2
Mtestset2<- testset2

#train/test set three
Mtrainset3<- trainset3
Mtestset3<- testset3

#train/test set four
Mtrainset4<- trainset4
Mtestset4<- testset4

#train/test set five
Mtrainset5<- trainset5
Mtestset5<- testset5

#Five fold cross valiation with uniform sampling
pre_movie_err(Mtrainset1,Mtestset1)
pre_movie_err(Mtrainset2,Mtestset2)
pre_movie_err(Mtrainset3,Mtestset3)
pre_movie_err(Mtrainset4,Mtestset4)
pre_movie_err(Mtrainset5,Mtestset5)

#Load five train/test set given by MoievLens
#load u1.base and u1.test
train1<- read.table ("H:/.das/Desktop/R file/ml-100k/u1.base", sep="\t",
col.names= c("userID","movieID","rating","TimeRealse"))
train1$M<- "X"
train1$movieID <- paste(train1$M,train1$movieID,sep="")
M.train1<-train1


test1<- read.table ("H:/.das/Desktop/R file/ml-100k/u1.test", sep="\t",
col.names= c("userID","movieID","rating","TimeRealse"))
test1$M<- "X"
test1$movieID <- paste(test1$M,test1$movieID,sep="")
M.test1<-test1

#load u2.base and u2.test
train2<- read.table ("H:/.das/Desktop/R file/ml-100k/u2.base", sep="\t",
col.names= c("userID","movieID","rating","TimeRealse"))
train2$M<- "X"
train2$movieID <- paste(train2$M,train2$movieID,sep="")
M.train2<- train2


test2<- read.table ("H:/.das/Desktop/R file/ml-100k/u2.test", sep="\t",
col.names= c("userID","movieID","rating","TimeRealse"))
test2$M<- "X"
test2$movieID <- paste(test2$M,test2$movieID,sep="")
M.test2<- test2


#laod u3.base and u3.test
train3<- read.table ("H:/.das/Desktop/R file/ml-100k/u3.base", sep="\t",
col.names= c("userID","movieID","rating","TimeRealse"))
train3$MID<- "X"
train3$movieID <- paste(train3$MID,train3$movieID,sep="")
M.train3<- train3


test3<- read.table ("H:/.das/Desktop/R file/ml-100k/u3.test", sep="\t",
col.names= c("userID","movieID","rating","TimeRealse"))
test3$MID<- "X"
test3$movieID <- paste(test3$MID,test3$movieID,sep="")
M.test3<- test3


##laod u4.base and u4.test
train4<- read.table ("H:/.das/Desktop/R file/ml-100k/u4.base", sep="\t",
col.names= c("userID","movieID","rating","TimeRealse"))
train4$MID<- "X"
train4$movieID <- paste(train4$MID,train4$movieID,sep="")
M.train4<- train4


test4<- read.table ("H:/.das/Desktop/R file/ml-100k/u4.test", sep="\t",
col.names= c("userID","movieID","rating","TimeRealse"))
test4$MID<- "X"
test4$movieID <- paste(test4$MID,test4$movieID,sep="")
M.test4<- test4

##laod u5.base and u5.test
train5<- read.table ("H:/.das/Desktop/R file/ml-100k/u5.base", sep="\t",
col.names= c("userID","movieID","rating","TimeRealse"))
train5$MID<- "X"
train5$movieID <- paste(train5$MID,train5$movieID,sep="")
M.train5<- train5


test5<- read.table ("H:/.das/Desktop/R file/ml-100k/u5.test", sep="\t",
col.names= c("userID","movieID","rating","TimeRealse"))
test5$MID<- "X"
test5$movieID <- paste(test5$MID,test5$movieID,sep="")
M.test5<- test5


#Five fold cross validation with train/test set given by MovieLens
pre_movie_err(M.train1,M.test1)
pre_movie_err(M.train2,M.test2)
pre_movie_err(M.train3,M.test3)
pre_movie_err(M.train4,M.test4)
pre_movie_err(M.train5,M.test5)

#Load two train/test set data
#load ua.base and ua.test
traina<- read.table ("H:/.das/Desktop/R file/ml-100k/ua.base", sep="\t",
col.names= c("userID","movieID","rating","TimeRealse"))
traina$MID<- "X"
traina$movieID <- paste(traina$MID,traina$movieID,sep="")
M.traina<-traina


testa<- read.table ("H:/.das/Desktop/R file/ml-100k/ua.test", sep="\t",
col.names= c("userID","movieID","rating","TimeRealse"))
testa$MID<- "X"
testa$movieID <- paste(testa$MID,testa$movieID,sep="")
M.testa<- testa


#load ub.base and ub.test
trainb<- read.table ("H:/.das/Desktop/R file/ml-100k/ub.base", sep="\t",
col.names= c("userID","movieID","rating","TimeRealse"))
trainb$MID<- "X"
trainb$movieID <- paste(trainb$MID,trainb$movieID,sep="")
M.trainb<- trainb


testb<- read.table ("H:/.das/Desktop/R file/ml-100k/ub.test", sep="\t",
col.names= c("userID","movieID","rating","TimeRealse"))
testb$MID<- "X"
testb$movieID <- paste(testb$MID,testb$movieID,sep="")
M.testb<- testb


#Two fold cross validation with train/test set given by MovieLens
pre_movie_err(M.traina,M.testa)
pre_movie_err(M.trainb,M.testb)

###################################################################
#(ii) Error Measures for predictor in (c)(ii)(user average rating)#
###################################################################


#Predicting the average rating for users
#Find the residuals, RMSE, MAE, relative residuals, relative RMSE, relative MAE
pre_user_err<- function (train_data,test_data){
avgU_train<- aggregate(rating~userID, data= train_data, mean)
colnames(avgU_train)<- c("userID","pre") # pre represent prediction 
avgU_pre<- merge (avgU_train, test_data, by= "userID", all.avgU_test= TRUE)
avgU_pre[avgU_pre$pre<1,]<- 1
avgU_pre[avgU_pre$pre>5,]<- 5
avgU_pre$resd<- avgU_pre$rating- avgU_pre$pre
avgU_pre$REresd<- 1- avgU_pre$pre/avgU_pre$rating
RMSE<- sqrt(mean((avgU_pre$resd)^2))
MAE<- mean(abs(avgU_pre$resd))
reRMSE<-sqrt(mean((avgU_pre$REresd)^2))
reMAE<- mean(abs(avgU_pre$REresd))
return(list(RMSE,MAE,reRMSE,reMAE))
}


#Assign train/test set to the predictor 
#train/test one 
Utrainset1<- trainset1
Utestset1<- testset1

#train/test two 
Utrainset2<- trainset2
Utestset2<- testset2

#train/test three 
Utrainset3<- trainset3
Utestset3<- testset3

#train/test four 
Utrainset4<- trainset4
Utestset4<- testset4

#train/test five 
Utrainset5<- trainset5
Utestset5<- testset5


#Five fold cross valiation with uniform sampling
pre_user_err(Utrainset1,Utestset1)
pre_user_err(Utrainset2,Utestset2)
pre_user_err(Utrainset3,Utestset3)
pre_user_err(Utrainset4,Utestset4)
pre_user_err(Utrainset5,Utestset5)


#Assign train/test set to predictor
#u1.base and u1.test
U.train1<-train1
U.test1<-test1

#u2.base and u2.test
U.train2<-train2
U.test2<-test2

#u3.base and u3.test
U.train3<-train3
U.test3<-test3

#u4.base and u4.test
U.train4<-train4
U.test4<-test4

#u5.base and u5.test
U.train5<-train5
U.test5<-test5

#Five fold cross validation with train/test given by MovieLens
pre_user_err(U.train1,U.test1)
pre_user_err(U.train2,U.test2)
pre_user_err(U.train3,U.test3)
pre_user_err(U.train4,U.test4)
pre_user_err(U.train5,U.test5)


#Assgin train/test set 
#ua.base and ua.test
U.traina<-traina
U.testa<- testa


#ub.base and ub.test
U.trainb<-trainb
U.testb<- testb

#Two fold cross validation with train/test given by MovieLens
pre_user_err(U.traina,U.testa)
pre_user_err(U.trainb,U.testb)


##############################################################
#(iii) Error Measures for predictor in (c)(iii)(double mean )#
##############################################################
library (reshape)

#Five fold cross validation with uniform sampling (double mean)
#train/test set one
avgM1<- aggregate(rating~movieID, data=Mtrainset1,mean)
colnames(avgM1)[2]<- "estRat"
avgU1<- aggregate(rating~userID, data=Utrainset1,mean)
colnames(avgU1)[2]<- "estRat"
RD.trainset1 <- t(sapply(avgU1[,2],function(x) x+avgM1[,2]-3.076045))
colnames(RD.trainset1) <- avgM1[,1]
RD.trainset1<- data.frame(userID = avgU1[,1], RD.trainset1)
RD.trainset1<-melt(RD.trainset1,id.var="userID")
colnames(RD.trainset1)<- c("userID","movieID","estRat")
RD.testset1<- testset1[c("userID","movieID","rating")]
RD.testset1<-merge(RD.testset1,RD.trainset1,by=c("userID","movieID"))
RD.testset1[RD.testset1$estRat<1,]<-1
RD.testset1[RD.testset1$estRat>5,]<-5


#train/test set two
avgM2<- aggregate(rating~movieID, data=Mtrainset2,mean)
colnames(avgM2)[2]<- "estRat"
avgU2<- aggregate(rating~userID, data=Utrainset2,mean)
colnames(avgU2)[2]<- "estRat"
RD.trainset2 <- t(sapply(avgU2[,2],function(x) x+avgM2[,2]-3.076045))
colnames(RD.trainset2) <- avgM2[,1]
RD.trainset2<- data.frame(userID = avgU2[,1], RD.trainset2)
RD.trainset2<-melt(RD.trainset2,id.var="userID")
colnames(RD.trainset2)<- c("userID","movieID","estRat")
RD.testset2<- testset2[c("userID","movieID","rating")]
RD.testset2<-merge(RD.testset2,RD.trainset2,by=c("userID","movieID"))
RD.testset2[RD.testset2$estRat<1,]<-1
RD.testset2[RD.testset2$estRat>5,]<-5

#train/test set three
avgM3<- aggregate(rating~movieID, data=Mtrainset3,mean)
colnames(avgM3)[2]<- "estRat"
avgU3<- aggregate(rating~userID, data=Utrainset3,mean)
colnames(avgU3)[2]<- "estRat"
RD.trainset3 <- t(sapply(avgU3[,2],function(x) x+avgM3[,2]-3.076045))
colnames(RD.trainset3) <- avgM3[,1]
RD.trainset3<- data.frame(userID = avgU3[,1], RD.trainset3)
RD.trainset3<-melt(RD.trainset3,id.var="userID")
colnames(RD.trainset3)<- c("userID","movieID","estRat")
RD.testset3<- testset3[c("userID","movieID","rating")]
RD.testset3<-merge(RD.testset3,RD.trainset3,by=c("userID","movieID"))
RD.testset3[RD.testset3$estRat<1,]<-1
RD.testset3[RD.testset3$estRat>5,]<-5

#train/test set four
avgM4<- aggregate(rating~movieID, data=Mtrainset4,mean)
colnames(avgM4)[2]<- "estRat"
avgU4<- aggregate(rating~userID, data=Utrainset4,mean)
colnames(avgU4)[2]<- "estRat"
RD.trainset4 <- t(sapply(avgU4[,2],function(x) x+avgM4[,2]-3.076045))
colnames(RD.trainset4) <- avgM4[,1]
RD.trainset4<- data.frame(userID = avgU4[,1], RD.trainset4)
RD.trainset4<-melt(RD.trainset4,id.var="userID")
colnames(RD.trainset4)<- c("userID","movieID","estRat")
RD.testset4<- testset4[c("userID","movieID","rating")]
RD.testset4<-merge(RD.testset4,RD.trainset4,by=c("userID","movieID"))
RD.testset4[RD.testset4$estRat<1,]<-1
RD.testset4[RD.testset4$estRat>5,]<-5

#train/test set five
avgM5<- aggregate(rating~movieID, data=Mtrainset5,mean)
colnames(avgM5)[2]<- "estRat"
avgU5<- aggregate(rating~userID, data=Utrainset5,mean)
colnames(avgU5)[2]<- "estRat"
RD.trainset5 <- t(sapply(avgU5[,2],function(x) x+avgM5[,2]-3.076045))
colnames(RD.trainset5) <- avgM5[,1]
RD.trainset5<- data.frame(userID = avgU5[,1], RD.trainset5)
RD.trainset5<-melt(RD.trainset5,id.var="userID")
colnames(RD.trainset5)<- c("userID","movieID","estRat")
RD.testset5<- testset5[c("userID","movieID","rating")]
RD.testset5<-merge(RD.testset5,RD.trainset5,by=c("userID","movieID"))
RD.testset5[RD.testset5$estRat<1,]<-1
RD.testset5[RD.testset5$estRat>5,]<-5

#Residuals five fold cross validation with uniform sampling (double mean)
RD.testset1$resd<- RD.testset1$rating - RD.testset1$estRat
RD.testset2$resd<- RD.testset2$rating - RD.testset2$estRat
RD.testset3$resd<- RD.testset3$rating - RD.testset3$estRat
RD.testset4$resd<- RD.testset4$rating - RD.testset4$estRat
RD.testset5$resd<- RD.testset5$rating - RD.testset5$estRat

#Relative residuals five fold cross validation with uniform sampling (double mean)
RD.testset1$REresd<- 1- RD.testset1$estRat/RD.testset1$rating
RD.testset2$REresd<- 1- RD.testset2$estRat/RD.testset2$rating
RD.testset3$REresd<- 1- RD.testset3$estRat/RD.testset3$rating
RD.testset4$REresd<- 1- RD.testset4$estRat/RD.testset4$rating
RD.testset5$REresd<- 1- RD.testset5$estRat/RD.testset5$rating

#MSE
RD.testset1$resdsqr<-(RD.testset1$resd)^2
RD1.MSE1<- mean(RD.testset1$resdsqr)

RD.testset2$resdsqr<-(RD.testset2$resd)^2
RD1.MSE2<- mean(RD.testset2$resdsqr)

RD.testset3$resdsqr<-(RD.testset3$resd)^2
RD1.MSE3<- mean(RD.testset3$resdsqr)

RD.testset4$resdsqr<-(RD.testset4$resd)^2
RD1.MSE4<- mean(RD.testset4$resdsqr)

RD.testset5$resdsqr<-(RD.testset5$resd)^2
RD1.MSE5<- mean(RD.testset5$resdsqr)

#Relative MSE
RD.testset1$REresdsqr<-(RD.testset1$REresd)^2
RD1.reMSE1<- mean(RD.testset1$REresdsqr)

RD.testset2$REresdsqr<-(RD.testset2$REresd)^2
RD1.reMSE2<- mean(RD.testset2$REresdsqr)

RD.testset3$REresdsqr<-(RD.testset3$REresd)^2
RD1.reMSE3<- mean(RD.testset3$REresdsqr)

RD.testset4$REresdsqr<-(RD.testset4$REresd)^2
RD1.reMSE4<- mean(RD.testset4$REresdsqr)

RD.testset5$REresdsqr<-(RD.testset5$REresd)^2
RD1.reMSE5<- mean(RD.testset5$REresdsqr)

#RMSE 
RD1.RMSE1<-sqrt(RD1.MSE1)
RD1.RMSE2<-sqrt(RD1.MSE2)
RD1.RMSE3<-sqrt(RD1.MSE3)
RD1.RMSE4<-sqrt(RD1.MSE4)
RD1.RMSE5<-sqrt(RD1.MSE5)

#Relative RMSE
RD1.reRMSE1<-sqrt(RD1.reMSE1)
RD1.reRMSE2<-sqrt(RD1.reMSE2)
RD1.reRMSE3<-sqrt(RD1.reMSE3)
RD1.reRMSE4<-sqrt(RD1.reMSE4)
RD1.reRMSE5<-sqrt(RD1.reMSE5)

#MAE
RD.testset1$resdAbs<-abs(RD.testset1$resd)
RD1.MAE1<- mean(RD.testset1$resdAbs)

RD.testset2$resdAbs<-abs(RD.testset2$resd)
RD1.MAE2<- mean(RD.testset2$resdAbs)

RD.testset3$resdAbs<-abs(RD.testset3$resd)
RD1.MAE3<- mean(RD.testset3$resdAbs)

RD.testset4$resdAbs<-abs(RD.testset4$resd)
RD1.MAE4<- mean(RD.testset4$resdAbs)

RD.testset5$resdAbs<-abs(RD.testset5$resd)
RD1.MAE5<- mean(RD.testset5$resdAbs)

#Relative MAE
RD.testset1$REresdAbs<-abs(RD.testset1$REresd)
RD1.reMAE1<- mean(RD.testset1$REresdAbs)

RD.testset2$REresdAbs<-abs(RD.testset2$REresd)
RD1.reMAE2<- mean(RD.testset2$REresdAbs)

RD.testset3$REresdAbs<-abs(RD.testset3$REresd)
RD1.reMAE3<- mean(RD.testset3$REresdAbs)

RD.testset4$REresdAbs<-abs(RD.testset4$REresd)
RD1.reMAE4<- mean(RD.testset4$REresdAbs)

RD.testset5$REresdAbs<-abs(RD.testset5$REresd)
RD1.reMAE5<- mean(RD.testset5$REresdAbs)

#Five fold cross-validation with non-uniform (average user rating)
#u1.base and u1.test
avgM.train1<- aggregate(rating~movieID, data=M.train1,mean)
colnames(avgM.train1)[2]<- "estRat"
avgU.train1<- aggregate(rating~userID, data=U.train1,mean)
colnames(avgU.train1)[2]<- "estRat"
RD.train1 <- t(sapply(avgU.train1[,2],function(x) x+avgM.train1[,2]-3.076045))
colnames(RD.train1) <- avgM.train1[,1]
RD.train1<- data.frame(userID = avgU.train1[,1], RD.train1)
RD.train1<-melt(RD.train1,id.var="userID")
colnames(RD.train1)<- c("userID","movieID","estRat")
RD.test1<- test1[c("userID","movieID","rating")]
RD.test1<-merge(RD.test1,RD.train1,by=c("userID","movieID"))
RD.test1[RD.test1$estRat<1,]<-1
RD.test1[RD.test1$estRat>5,]<-5

#u2.base and u2.test
avgM.train2<- aggregate(rating~movieID, data=M.train2,mean)
colnames(avgM.train2)[2]<- "estRat"
avgU.train2<- aggregate(rating~userID, data=U.train2,mean)
colnames(avgU.train2)[2]<- "estRat"
RD.train2 <- t(sapply(avgU.train2[,2],function(x) x+avgM.train2[,2]-3.076045))
colnames(RD.train2) <- avgM.train2[,1]
RD.train2<- data.frame(userID = avgU.train2[,1], RD.train2)
RD.train2<-melt(RD.train2,id.var="userID")
colnames(RD.train2)<- c("userID","movieID","estRat")
RD.test2<- test2[c("userID","movieID","rating")]
RD.test2<-merge(RD.test2,RD.train2,by=c("userID","movieID"))
RD.test2[RD.test2$estRat<1,]<-1
RD.test2[RD.test2$estRat>5,]<-5

#u3.base and u3.test
avgM.train3<- aggregate(rating~movieID, data=M.train3,mean)
colnames(avgM.train3)[2]<- "estRat"
avgU.train3<- aggregate(rating~userID, data=U.train3,mean)
colnames(avgU.train3)[2]<- "estRat"
RD.train3 <- t(sapply(avgU.train3[,2],function(x) x+avgM.train3[,2]-3.076045))
colnames(RD.train3) <- avgM.train3[,1]
RD.train3<- data.frame(userID = avgU.train3[,1], RD.train3)
RD.train3<-melt(RD.train3,id.var="userID")
colnames(RD.train3)<- c("userID","movieID","estRat")
RD.test3<- test3[c("userID","movieID","rating")]
RD.test3<-merge(RD.test3,RD.train3,by=c("userID","movieID"))
RD.test3[RD.test3$estRat<1,]<-1
RD.test3[RD.test3$estRat>5,]<-5


#u4.base and u4.test
avgM.train4<- aggregate(rating~movieID, data=M.train4,mean)
colnames(avgM.train4)[2]<- "estRat"
avgU.train4<- aggregate(rating~userID, data=U.train4,mean)
colnames(avgU.train4)[2]<- "estRat"
RD.train4 <- t(sapply(avgU.train4[,2],function(x) x+avgM.train4[,2]-3.076045))
colnames(RD.train4) <- avgM.train4[,1]
RD.train4<- data.frame(userID = avgU.train4[,1], RD.train4)
RD.train4<-melt(RD.train4,id.var="userID")
colnames(RD.train4)<- c("userID","movieID","estRat")
RD.test4<- test4[c("userID","movieID","rating")]
RD.test4<-merge(RD.test4,RD.train4,by=c("userID","movieID"))
RD.test4[RD.test4$estRat<1,]<-1
RD.test4[RD.test4$estRat>5,]<-5

#u5.base and u5.test
avgM.train5<- aggregate(rating~movieID, data=M.train5,mean)
colnames(avgM.train5)[2]<- "estRat"
avgU.train5<- aggregate(rating~userID, data=U.train5,mean)
colnames(avgU.train5)[2]<- "estRat"
RD.train5 <- t(sapply(avgU.train5[,2],function(x) x+avgM.train5[,2]-3.076045))
colnames(RD.train5) <- avgM.train5[,1]
RD.train5<- data.frame(userID = avgU.train5[,1], RD.train5)
RD.train5<-melt(RD.train5,id.var="userID")
colnames(RD.train5)<- c("userID","movieID","estRat")
RD.test5<- test5[c("userID","movieID","rating")]
RD.test5<-merge(RD.test5,RD.train5,by=c("userID","movieID"))
RD.test5[RD.test5$estRat<1,]<-1
RD.test5[RD.test5$estRat>5,]<-5

#Residuals five fold cross validation with uniform sampling (double mean)
RD.test1$resd<- RD.test1$rating- RD.test1$estRat
RD.test2$resd<- RD.test2$rating- RD.test2$estRat
RD.test3$resd<- RD.test3$rating- RD.test3$estRat
RD.test4$resd<- RD.test4$rating- RD.test4$estRat
RD.test5$resd<- RD.test5$rating- RD.test5$estRat

#Relative residuals five fold cross validation with uniform sampling (double mean)
RD.test1$REresd<- 1- RD.test1$estRat/RD.test1$rating
RD.test2$REresd<- 1- RD.test2$estRat/RD.test2$rating
RD.test3$REresd<- 1- RD.test3$estRat/RD.test3$rating
RD.test4$REresd<- 1- RD.test4$estRat/RD.test4$rating
RD.test5$REresd<- 1- RD.test5$estRat/RD.test5$rating

#MSE
RD.test1$resdsqr<-(RD.test1$resd)^2
RD2.MSE1<- mean(RD.test1$resdsqr)

RD.test2$resdsqr<-(RD.test2$resd)^2
RD2.MSE2<- mean(RD.test2$resdsqr)

RD.test3$resdsqr<-(RD.test3$resd)^2
RD2.MSE3<- mean(RD.test3$resdsqr)

RD.test4$resdsqr<-(RD.test4$resd)^2
RD2.MSE4<- mean(RD.test4$resdsqr)

RD.test5$resdsqr<-(RD.test5$resd)^2
RD2.MSE5<- mean(RD.test5$resdsqr)

#Relative MSE
RD.test1$REresdsqr<-(RD.test1$REresd)^2
RD2.reMSE1<- mean(RD.test1$REresdsqr)

RD.test2$REresdsqr<-(RD.test2$REresd)^2
RD2.reMSE2<- mean(RD.test2$REresdsqr)

RD.test3$REresdsqr<-(RD.test3$REresd)^2
RD2.reMSE3<- mean(RD.test3$REresdsqr)

RD.test4$REresdsqr<-(RD.test4$REresd)^2
RD2.reMSE4<- mean(RD.test4$REresdsqr)

RD.test5$REresdsqr<-(RD.test5$REresd)^2
RD2.reMSE5<- mean(RD.test5$REresdsqr)

#RMSE
RD2.RMSE1<- sqrt(RD2.MSE1)
RD2.RMSE2<- sqrt(RD2.MSE2)
RD2.RMSE3<- sqrt(RD2.MSE3)
RD2.RMSE4<- sqrt(RD2.MSE4)
RD2.RMSE5<- sqrt(RD2.MSE5)

#Relative RMSE
RD2.reRMSE1<- sqrt(RD2.reMSE1)
RD2.reRMSE2<- sqrt(RD2.reMSE2)
RD2.reRMSE3<- sqrt(RD2.reMSE3)
RD2.reRMSE4<- sqrt(RD2.reMSE4)
RD2.reRMSE5<- sqrt(RD2.reMSE5)

#MAE
RD.test1$resdAbs<-abs(RD.test1$resd)
RD2.MAE1<- mean(RD.test1$resdAbs)

RD.test2$resdAbs<-abs(RD.test2$resd)
RD2.MAE2<- mean(RD.test2$resdAbs)

RD.test3$resdAbs<-abs(RD.test3$resd)
RD2.MAE3<- mean(RD.test3$resdAbs)

RD.test4$resdAbs<-abs(RD.test4$resd)
RD2.MAE4<- mean(RD.test4$resdAbs)

RD.test5$resdAbs<-abs(RD.test5$resd)
RD2.MAE5<- mean(RD.test5$resdAbs)

#Relative MAE
RD.test1$REresdAbs<-abs(RD.test1$REresd)
RD2.reMAE1<- mean(RD.test1$REresdAbs)

RD.test2$REresdAbs<-abs(RD.test2$REresd)
RD2.reMAE2<- mean(RD.test2$REresdAbs)

RD.test3$REresdAbs<-abs(RD.test3$REresd)
RD2.reMAE3<- mean(RD.test3$REresdAbs)

RD.test4$REresdAbs<-abs(RD.test4$REresd)
RD2.reMAE4<- mean(RD.test4$REresdAbs)

RD.test5$REresdAbs<-abs(RD.test5$REresd)
RD2.reMAE5<- mean(RD.test5$REresdAbs)

#Two fold cross validation with uniform sampling (double mean)
#ua.base and ua.test
avgM.traina<- aggregate(rating~movieID, data=M.traina,mean)
colnames(avgM.traina)[2]<- "estRat"
avgU.traina<- aggregate(rating~userID, data=U.traina,mean)
colnames(avgU.traina)[2]<- "estRat"
RD.traina <- t(sapply(avgU.traina[,2],function(x) x+avgM.traina[,2]-3.076045))
colnames(RD.traina) <- avgM.traina[,1]
RD.traina<- data.frame(userID = avgU.traina[,1], RD.traina)
RD.traina<-melt(RD.traina,id.var="userID")
colnames(RD.traina)<- c("userID","movieID","estRat")
RD.testa<- testa[c("userID","movieID","rating")]
RD.testa<-merge(RD.testa,RD.traina,by=c("userID","movieID"))
RD.testa[RD.testa$estRat<1,]<-1
RD.testa[RD.testa$estRat>5,]<-5

#ub.base and ubb.test
avgM.trainb<- aggregate(rating~movieID, data=M.trainb,mean)
colnames(avgM.trainb)[2]<- "estRat"
avgU.trainb<- aggregate(rating~userID, data=U.trainb,mean)
colnames(avgU.trainb)[2]<- "estRat"
RD.trainb <- t(sapply(avgU.trainb[,2],function(x) x+avgM.trainb[,2]-3.076045))
colnames(RD.trainb) <- avgM.trainb[,1]
RD.trainb<- data.frame(userID = avgU.trainb[,1], RD.trainb)
RD.trainb<-melt(RD.trainb,id.var="userID")
colnames(RD.trainb)<- c("userID","movieID","estRat")
RD.testb<- testb[c("userID","movieID","rating")]
RD.testb<-merge(RD.testb,RD.trainb,by=c("userID","movieID"))
RD.testb[RD.testb$estRat<1,]<-1
RD.testb[RD.testb$estRat>5,]<-5

#Residual two fold cross validation with uniform sampling (double mean)
RD.testa$resd<- RD.testa$rating- RD.testa$estRat
RD.testb$resd<- RD.testb$rating- RD.testb$estRat

#Relative residual 
RD.testa$REresd<- 1- RD.testa$estRat/RD.testa$rating
RD.testb$REresd<- 1- RD.testb$estRat/RD.testb$rating

#MSE
RD.testa$resdsqr<- (RD.testa$resd)^2
RD3.MSEa<- mean(RD.testa$resdsqr)

RD.testb$resdsqr<- (RD.testb$resd)^2
RD3.MSEb<- mean(RD.testb$resdsqr)

#Relative MSE
RD.testa$REresdsqr<- (RD.testa$REresd)^2
RD3.reMSEa<- mean(RD.testa$REresdsqr)

RD.testb$REresdsqr<- (RD.testb$REresd)^2
RD3.reMSEb<- mean(RD.testb$REresdsqr)

#RMSE
RD3.RMSEa<- sqrt(RD3.MSEa)
RD3.RMSEb<- sqrt(RD3.MSEb)

#Relative RMSE
RD3.reRMSEa<- sqrt(RD3.reMSEa)
RD3.reRMSEb<- sqrt(RD3.reMSEb)

#MAE
RD.testa$resdAbs<- abs(RD.testa$resd)
RD3.MAEa<- mean(RD.testa$resdAbs)

RD.testb$resdAbs<- abs(RD.testb$resd)
RD3.MAEb<- mean(RD.testb$resdAbs)

#Relative MAE
RD.testa$REresdAbs<- abs(RD.testa$REresd)
RD3.reMAEa<- mean(RD.testa$REresdAbs)

RD.testb$REresdAbs<- abs(RD.testb$REresd)
RD3.reMAEb<- mean(RD.testb$REresdAbs)


#######################################################################
#(iv) Error Measures for predictor in (c)(iv)(least square estimation)#
#######################################################################
#Create function to find RMSE, MAE, relative RMSE, relative MAE using least square estimation
### to do calculations involving matrix inverses, need the "MASS" package ###
library(MASS)
lse_err<-function(train_data,test_data){
avgM1<- aggregate(rating~movieID, data=train_data,mean)
colnames(avgM1)<- c("movieID","avgMrat")
avgM<- merge(train_data,avgM1, by="movieID")
avgU <- aggregate(rating~userID, data=train_data,mean)
colnames(avgU)<- c("userID","avgUrat")
data<- merge(avgM, avgU, by="userID")
p<- as.matrix(data[,c("avgUrat","avgMrat")])
q<- as.matrix(data[,"rating"])
### matrix calculations ###
par<- ginv(t(p) %*% p) %*% t(p) %*% q
  a <- par[1]
  b <- par[2]
test_data1 <- merge(test_data, avgM1, by="movieID",all.test_data=TRUE)
test_data1[is.na(test_data1$avgMrat),"avgMrat"] <- mean(train_data$rating)
test_data2 <- merge(test_data1,avgU,by="userID",all.test_data=TRUE)
test_data2[is.na(test_data2$avgUrat),"avgUrat"] <- mean(train_data$rating)

test_data2$pre <-a*test_data2[,"avgUrat"] + b*test_data2[,"avgMrat"]
test_data2$pre[test_data2$pre<1] <- 1
test_data2$pre[test_data2$pre>5] <- 5
names(test_data2$pre) <- "pre"
pre_rat<- test_data2
pre_rat$resd<- pre_rat$rating- pre_rat$pre
pre_rat$REresd<- 1- pre_rat$pre/pre_rat$rating
RMSE<- sqrt(mean((pre_rat$resd)^2))
MAE<- mean(abs(pre_rat$resd))
reRMSE<- sqrt(mean((pre_rat$REresd)^2))
reMAE<- mean(abs(pre_rat$REresd))
return(list(RMSE,MAE,reRMSE,reMAE))

}


#Assign train/test set 
lse.trainset1<- trainset1
lse.testset1<- testset1

lse.trainset2<- trainset2
lse.testset2<- testset2

lse.trainset3<- trainset3
lse.testset3<- testset3


lse.trainset4<- trainset4
lse.testset4<- testset4

lse.trainset5<- trainset5
lse.testset5<- testset5

#Five cross validation with uniform sampling
lse_err(lse.trainset1,lse.testset1)
lse_err(lse.trainset2,lse.testset2)
lse_err(lse.trainset3,lse.testset3)
lse_err(lse.trainset4,lse.testset4)
lse_err(lse.trainset5,lse.testset5)

#Assign train/test set given by MovieLens
lse.train1<- train1
lse.test1<- test1

lse.train2<- train2
lse.test2<- test2

lse.train3<- train3
lse.test3<- test3

lse.train4<- train4
lse.test4<- test4

lse.train5<- train5
lse.test5<- test5

#Five cross validation with train/test set given by MovieLens
lse_err(lse.train1,lse.test1)
lse_err(lse.train2,lse.test2)
lse_err(lse.train3,lse.test3)
lse_err(lse.train4,lse.test4)
lse_err(lse.train5,lse.test5)

#Assign train/test set 
lse.traina<- traina
lse.testa<- testa

lse.trainb<- trainb
lse.testb<- testb

#Two fold cross validation with train/test set given by MovieLens
lse_err(lse.traina,lse.testa)
lse_err(lse.trainb,lse.testb)


#############################################################
#(v) Error Measures for predictor in (c)(v)                 #
#(least square estimation obtained from minimization of RSS)#
#############################################################
#Create function to find RMSE, MAE, relative RMSE, relative MAE
mlse_err <- function(train_data,test_data)  {
avgM1 <- aggregate(rating~movieID, data=train_data,mean)
names(avgM1) <- c("movieID","avgMrat")
avgM <- merge(train_data, avgM1, by="movieID")
avgU <- aggregate(rating~userID, data=train_data,mean)
names(avgU) <- c("userID","avgUrat")
data <- merge(avgU, avgM, by="userID")

#create the residual sum of square (RSS) function
rss<- function(x) sum((data[,"rating"]-x[1]*data[,"avgUrat"]- x[2]*data[,"avgMrat"])^2)
ans1 <- nlm(rss,c(1,1))

#use nlm internally to obtain the estimate
 c <- ans1$estimate[1]
 d <- ans1$estimate[2]
test_data1 <- merge(test_data, avgM1, by= "movieID",all.test_data=TRUE)
test_data1[is.na(test_data1$avgMrat),"avgMrat"] <- mean(train_data$rating)
test_data2 <- merge(test_data1, avgU,by="userID",all.test_data=TRUE)
test_data2[is.na(test_data2$avgUrat),"avgUrat"] <- mean(train_data$rating)
test_data2$pre <- c*test_data2[,"avgUrat"] + d*test_data2[,"avgMrat"]
        
test_data2$pre[test_data2$pre<1] <- 1
test_data2$pre[test_data2$pre>5] <- 5 
names(test_data2$pre) <- "pre"
pre_rat<- test_data2

#Find the residuals and relative residuals
pre_rat$resd<- pre_rat$rating- pre_rat$pre
pre_rat$REresd<- 1- pre_rat$pre/pre_rat$rating

#Find the RMSE, MAE, relative RMSE, relative MAE
RMSE<- sqrt(mean((pre_rat$resd)^2))
MAE<- mean(abs(pre_rat$resd))
reRMSE<- sqrt(mean((pre_rat$REresd)^2))
reMAE<- mean(abs(pre_rat$REresd))

return(list(RMSE,MAE,reRMSE,reMAE))

}

#Assign train/test set 
mlse.trainset1<- trainset1
mlse.testset1<- testset1

mlse.trainset2<- trainset2
mlse.testset2<- testset2

mlse.trainset3<- trainset3
mlse.testset3<- testset3

mlse.trainset4<- trainset4
mlse.testset4<- testset4

mlse.trainset5<- trainset5
mlse.testset5<- testset5

#Five fold cross validation with train/test set
mlse_err(mlse.trainset1,mlse.testset1)
mlse_err(mlse.trainset2,mlse.testset2)
mlse_err(mlse.trainset3,mlse.testset3)
mlse_err(mlse.trainset4,mlse.testset4)
mlse_err(mlse.trainset5,mlse.testset5)


#Assign train/test set given by MovieLens
mlse.train1<- train1
mlse.test1<- test1

mlse.train2<- train2
mlse.test2<- test2

mlse.train3<- train3
mlse.test3<- test3

mlse.train4<- train4
mlse.test4<- test4

mlse.train5<- train5
mlse.test5<- test5


#Five fold cross validation with train/test set given by MovieLens
mlse_err(mlse.train1,mlse.test1)
mlse_err(mlse.train2,mlse.test2)
mlse_err(mlse.train3,mlse.test3)
mlse_err(mlse.train4,mlse.test4)
mlse_err(mlse.train5,mlse.test5)

#Assign train/test set given by MovieLens
mlse.traina<- traina
mlse.testa<- testa

mlse.trainb<- trainb
mlse.testb<- testb

#Two fold cross validation with train/test set given by MovieLens
mlse_err(mlse.traina,mlse.testa)
mlse_err(mlse.trainb,mlse.testb)











