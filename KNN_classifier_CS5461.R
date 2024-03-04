install.packages("dplyr")
library(dplyr)
install.packages("readr")
library(readr)

read_csv("DREAM_data.txt")
read_csv("~alle0499/Desktop/HW2-20190326/DREAM_data.txt", col_names = TRUE)

dim(DREAM_data)
standard <- DREAM_data[6:18637,]

## question 1 Everolimus
Everolimus <- standard
Everolimus <- DREAM_data[-2:-5,1:47]

  # clustering using pcc
install.packages("fastcluster")
library(fastcluster)
install.packages("ggplot2")
library(ggplot2)
install.packages("cluster")
library(cluster)
Everolimus.num <- Everolimus[, -1]
install.packages("tidyr")
library(tidyr)
Everolimus.numNA <- Everolimus.num %>% drop_na()
c <- cor((Everolimus.numNA), method="pearson")
as.matrix(c)[1:18,1:18]
c = as.matrix(c)
Everolimus_drucgnames <- Everolimus[1,-1]
E_tot = Everolimus_drucgnames[E_names[Everolimus_sort$ix[2:6]]]
d <- as.dist(1-c)
as.matrix(d)[1:4,1:4]
hr <- hclust(d, method = "complete", members=NULL)
names(hr)
str(hr)
dim(Everolimus)
Everolimus_hr <- as.matrix(hr[["height"]])
Everolimus_hr = as.data.frame(Everolimus_hr)
Everolimus_ID <-  as.matrix(hr[["labels"]])
Everolimus_ID = as.data.frame(Everolimus_ID)

Everolimus_hr$ID = Everolimus_ID$V1
par(mfrow = c(1, 2)); plot(hr, hang = 0.1)
plot(hr, hang = -1) 

install.packages("MASS")
library(MASS)
Everolimus_T = t(Everolimus)
write.csv(Everolimus_T, "Everolimus_T.csv")
Everolimus_T <- Everolimus_T[,-1]

Everolimus.lda <- lda(Everolimus ~., data = Everolimus_T)
Everolimus.lda
Everolimus1 <- fanny(Everolimus, k=4, metric = "euclidean", memb.exp = 1.2)
round(fannyy$membership, 2)[1:4,]



##NEW
Everolimus_drucgnames <- Everolimus[1,]
dim(Everolimus_drucgnames)
Everolimus.numNA <- Everolimus.num %>% drop_na()
c <- cor((Everolimus.numNA), method="pearson")
c = as.matrix(c)
dim(c)
Everolimus_sortall = sort(c[1,], index.return=TRUE, decreasing = TRUE)
E_names <- names(Everolimus_drucgnames)
Everolimus_drucgnames[E_names[Everolimus_sortall$ix[2:6]]]
E_tot = Everolimus_drucgnames[E_names[Everolimus_sort$ix[2:6]]]

Everolimus_sortall2 = sort(c[2,], index.return=TRUE, decreasing = TRUE)
E_names2 <- names(Everolimus_drucgnames)
E_names2=as.data.frame(E_names2)
Everolimus_drucgnames=t(Everolimus_drucgnames)
E_names2$condit = Everolimus_drucgnames


Everolimus_drucgnames[E_names2[Everolimus_sortall2$ix[2:46]]]
E_tot2 = Everolimus_drucgnames[E_names2[Everolimus_sortall2$ix[2:46]]]
E_tot2= t(E_tot2)
E_tot2=as.data.frame(E_tot2)
sum(E_tot2)

f <- function(x,y){
  test <- sort(x, index.return=TRUE, decreasing = TRUE)
  #return(out)
  #out <- data.frame(name = test[1:46,]
                   # number = test[,1:46])
  return(test)
}
Everolimus_final <- sapply(seq(ncol(c)), function(x) f(c[x,]))
Everolimus_final=data.frame(Everolimus_final) ### disregurad "= 1"


E_tot2 = Everolimus_drucgnames[E_names2[try3[2:46,]]]
try3 = rbind(try3[1:2, 1:46])
strsplit(try3[1:2, 1:46], ",")

try31 = as.vector(try3[1:2,1:46])
write.csv(try31, "try.csv")
Everolimus_sortall2 = sort(c[2,], index.return=TRUE, decreasing = TRUE)


## Question 2
install.packages("class")
library(class)
pred <- knn(train =  c, test = c, cl=E_names2$condit, k=5)

f <- function(x,y){
  test <- sort(x, index.return=TRUE, decreasing = TRUE)
  return(test)
}
try3 <- sapply(seq(nrow(c)), function(x) f(c[x,]))
try3=data.frame(try3)

set.seed(1)
res <- sapply(c, function(k) {
  ##try out each version of k from 1 to 12
  res.k <- sapply(seq_along(try3), function(i) {
    ##loop over each of the 10 cross-validation folds
    ##predict the held-out samples using k nearest neighbors
    pred <- knn(train=[try3[[i]],],
                test=[try3[[i]],],
                cl=y[-try3[[i]]], k = k) 
})

##Question 1 
Dis_drug <- DREAM_data[-3:-5,1:47]
Dis_drug <- DREAM_data[-1,1:47]  
Dis_drug <-  Dis_drug[-2:-5,]
Dis_drugNames <- Dis_drug[1,]  

Dis_drug = as.data.frame(Dis_drug)
Dis_drug = as.numeric(Dis_drug)  
Dis_drugNA <- Dis_drug %>% drop_na()
Dis_drugNA <- apply(Dis_drugNA, 1,as.numeric) ## hack
d <- cor(Dis_drugNA, method="pearson")
# d = as.matrix(d)
dim(d)

f <- function(x,y){
  test <- sort(x, index.return=TRUE, decreasing = TRUE)
  #return(out)
  #out <- data.frame(name = test[1:46,]
  # number = test[,1:46])
  return(test)
}
Dis_drug_final <- sapply(seq(ncol(c)), function(x) f(c[x,]))
Dis_drug_final=data.frame(Dis_drug_final) ### disregurad "= 1"


##Question 1 
meth_drug <- DREAM_data[-1:-2,1:47]
#Dis_drug <- DREAM_data[-1,1:47]  
meth_drug <-  meth_drug[-2:-3,]
#meth_drug <- meth_drug[-1,]
meth_drugNames <- meth_drug[1,]  
write.csv(meth_drug, "meth_drug.csv")

#meth_drug <- gsub(",", "", meth_drug) 
#meth_drug <- as.numeric((meth_drug))
#meth_drug = as.numeric(levels(meth_drug))[meth_drug]

install.packages("tidyr")
library(tidyr)
meth_drug = as.data.frame(meth_drug)
meth_drug = as.numeric(meth_drug)
meth_drugNA <- (meth_drug) %>% drop_na()
meth_drugNA <- apply(meth_drugNA, 1,as.numeric) ## hack

meth_drug = as.matrix(meth_drugNA)

e <- cor((as.matrix(as.numeric(meth_drugNA))), method="pearson")
e = as.matrix(e)
dim(e)

f <- function(x,y){
  test <- sort(x, index.return=TRUE, decreasing = TRUE)
  #return(out)
  #out <- data.frame(name = test[1:46,]
  # number = test[,1:46])
  return(test)
}

for(i in 1:46 ) {
  res = f(e[,i])
  scores = f(e[,x])
  sensitivity = f(e[,x])
  k = 46
  scores = c(scores,sensitivity[names(res$x[2:(k+1)])])
}
  scores<- as.matrix(scores)
Meth_drug_final <- sapply(seq(ncol(e)), f(c[x,]))
Meth_drug_final=data.frame(Meth_drug_final) ### disregurad "= 1"







install.packages("bnstruct")
library(bnstruct)

knn <- function(x, k){ 
  ##convert GE to pearson correlation
  #Drug.numNA <- x %>% drop_na()
  #x <- cor((x), method="pearson")
 # x = as.matrix(x)
  ##knn test
  test <- knn.impute(x, k)
  return(test)
}
knn2 <- sapply(c) function(c) f((c, k = 5)))
knn1 <- sapply(seq((c)), function(knn) knn(c, k = 5))

#sapply(seq(nrow(wang.data.subset0)), function(x) f(wang.data.subset0[x,], wang.data.subset1[x,]))

knn_trial= knn.impute(c, k = 5)

minvalue <- 1
n <- 0 #change to whatever.
columns <- c(1:n)
knn_trial[knn_trial[,] < minvalue,columns] <- minvalue

knn_trial = knn_trial[knn_trial="1.0000000"]<-0




##caret
install.packages("ISLR")
install.packages("caret")
library(ISLR)
library(caret)

set.seed(300)
#Spliting data as training and test set. Using createDataPartition() function from caret
indxTrain <- createDataPartition(y = Everolimus_T$Everolimus,p = 0.75,list = FALSE)
training <- Smarket[indxTrain,]
testing <- Smarket[-indxTrain,]

#Checking distibution in origanl data and partitioned data
prop.table(table(training$Direction)) * 100
prop.table(table(testing$Direction)) * 100
prop.table(table(Smarket$Direction)) * 100

trainX <- training[,names(training) != "Direction"]
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
preProcValues

set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- train(Direction ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

install.packages("e1071")
library(e1071)

#Output of kNN fit
knnFit
plot(knnFit)

knnPredict <- predict(knnFit,newdata = testing )
#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict, testing$Direction )

mean(knnPredict == testing$Direction)

ctrl <- trainControl(method="repeatedcv",repeats = 3,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- train(Direction ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
knnFit
plot(knnFit, print.thres = 0.5, type="S")

knnPredict <- predict(knnFit,newdata = testing )
#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict, testing$Direction )

install.packages("pROC")
library(pROC)
knnPredict <- predict(knnFit,newdata = testing , type="prob")
knnROC <- roc(testing$Direction,knnPredict[,"Down"], levels = rev(testing$Direction))
knnROC

