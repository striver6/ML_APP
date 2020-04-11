#################################

data1 <- read.csv("originaltrain.csv")
dataset<-data1


###############################
names(dataset) <- "V1"

dataset$V1 <- gsub(" +$","",dataset$V1)

dataset$V1 <- as.character(dataset$V1)

# parse the input into multiple columns and generate the data frame
m <- sapply(dataset$V1,strsplit, split=" ")
df <- data.frame(matrix(unlist(m), nrow=length(m), byrow=T), stringsAsFactors=FALSE)

# generate column names for the data frame
colnames <- c("id","cycle","setting1","setting2","setting3","s1","s2","s3","s4","s5","s6","s7",
              "s8","s9","s10","s11","s12","s13","s14","s15","s16","s17","s18","s19","s20","s21")
names(df) <- colnames

# genearte outpout data
dataset <- df
################################
dataset=sapply(dataset, as.numeric)
##############################
# user defined variables to set the windows for classifcation
w1 <- 30
w0 <- 15


# generate the column RUL (remaining useful life)


# get the maximum cycle number for each id
d1 <- aggregate(as.numeric(cycle)~id,data = dataset,FUN="max")

d2 <- merge(dataset,d1,by=c("id"))
colnames(d2)<- c("id","cycle","setting1","setting2","setting3","s1","s2","s3","s4","s5","s6","s7",
                 "s8","s9","s10","s11","s12","s13","s14","s15","s16","s17","s18","s19","s20","s21","max")
# generate the column RUL based on the values of columns "max" and "cycle"


d2$RUL <- d2$max - d2$cycle

# exclude column "max" from the data frame
d2 <- d2[,-which(names(d2) == "max")]



# genearte label1 and label2
dataset <- d2



dataset$label1 <- ifelse(d2$RUL <= w1, 1, 0)


dataset$label2 <- ifelse(d2$RUL <= w0, 2, ifelse(d2$RUL <= w1,1,0))


# generate output data
dataset <- dataset
################################################
ltraindata<-dataset#瀛樺偍鏍囩鍚庣殑璁粌鏁版嵁
################################################
# set the required R library
library(zoo)

######################################################
# User input variables

# window size (window_size>=2),  most recent sensor values
window_size = 5 

######################################################
# feature engineering

# find the column index for the last 3 columns: RUL, label1, label2
col_index1 = ncol(dataset)
col_index2 = col_index1 - 1
col_index3 = col_index1 - 2

# exclude comuns id, cycle, setting1,setting2,setting3, and last 3 columns
# only the 21 sensor columns are kept in the data frame
n_pre_sensor_columns = 5 # id, cycle, setting1,setting2,setting3
n_after_sensor_columns = 3 #RUL, label1, label2
n_col = ncol(dataset)

data=dataset[,-c(1:n_pre_sensor_columns,col_index3,col_index2,col_index1)]
n_sensor=ncol(data) # 21 sensors

id=unique(dataset[,1])
n_id=length(id) # 100

# generate column names

a = paste("a",(1:n_sensor),sep="") # average
sd =paste("sd",(1:n_sensor),sep="") # standard deviation



# get rolling mean and rolling sd
rollingmean = c()
rollingsd = c()

# loop for each unique engine id
for(i in 1:length(id)){
  
  # get the subset of the data that only contains the sensor columns for the id i
  sub_data = subset(dataset[,(n_pre_sensor_columns+1):(n_col-n_after_sensor_columns)],dataset$id==id[i])
  
  # window size is adjusted if the it is greater than the number of rows in the data
  n_row_subdata = nrow(sub_data)
  w=ifelse(window_size < n_row_subdata,window_size,n_row_subdata)
  
  # get the rolling mean for all sensors
  rollingmean = rbind(rollingmean,rollapply(sub_data,w,mean,align = "right",partial=1))
  
  # get the rolling sd for all sensors
  rollingsd_i = rollapply(sub_data,w,sd,align = "right",partial=1)
  rollingsd_i[is.na(rollingsd_i)]=0
  rollingsd = rbind(rollingsd,rollingsd_i)
  
  
  
}
data_a = as.data.frame(rollingmean)
data_sd = as.data.frame(rollingsd)

names(data_a) = a
names(data_sd) = sd



df = cbind(data_a,data_sd)

df2=cbind(dataset[,1:(n_pre_sensor_columns+n_sensor)],df)
df2$RUL=dataset$RUL
df2$label1=dataset$label1
df2$label2=dataset$label2


# generate output data
dataset = df2
############################################
dataset <- dataset[,-which(names(dataset) == "id")]
prenomaltraindata<-dataset#淇濆瓨褰掍竴鍖栧墠鐨勬暟鎹?
######################################################
x=dataset


b<-length(x[1,])-3

for(i in 1:b){
  col=x[,i]
  x_min_temp<-min(col)
  x_max_temp<-max(col)
  l<- x_max_temp-x_min_temp
  
  for (j in 1:nrow(x)){
    x[j,i]<-ifelse(l==0,0,(x[j,i]-x_min_temp)/l)
  }
}
################################################
dataset <- x[,colSums(is.na(x))<nrow(x)]
############################################
traindata<-dataset

###########################################














data2 <- read.csv("originaltest.csv")
dataset=data2
##########################################
names(dataset) <- "V1"

# delete the extra space at the end of the lines
dataset$V1 <- gsub(" +$","",dataset$V1)

# convert to character
dataset$V1 <- as.character(dataset$V1)

# parse the input into multiple columns and generate the data frame
m <- sapply(dataset$V1,strsplit, split=" ")
df <- data.frame(matrix(unlist(m), nrow=length(m), byrow=T), stringsAsFactors=FALSE)

# generate column names for the data frame
colnames <- c("id","cycle","setting1","setting2","setting3","s1","s2","s3","s4","s5","s6","s7",
              "s8","s9","s10","s11","s12","s13","s14","s15","s16","s17","s18","s19","s20","s21")
names(df) <- colnames

# genearte outpout data
dataset <- df
############################################
dataset=sapply(dataset, as.numeric)
##########################################
library(zoo)

############################################
# User input variables

# window size (window_size>=2),  most recent sensor values
window_size = 5 

############################################
# feature engineering

# exclude comuns id, cycle, setting1,setting2,setting3, and last 3 columns
# only the 21 sensor columns are kept in the data frame
n_pre_sensor_columns = 5 # id, cycle, setting1,setting2,setting3
n_after_sensor_columns = 0 # RUL, label1, label2  ar not in this data
n_col = ncol(dataset)

data=dataset[,-c(1:n_pre_sensor_columns)]
n_sensor=ncol(data) # 21 sensors

id=unique(dataset[,1])
n_id=length(id) # 100

# generate column names

a = paste("a",(1:n_sensor),sep="") # average
sd =paste("sd",(1:n_sensor),sep="") # standard deviation


# get rolling mean and rolling sd
rollingmean = c()
rollingsd = c()
# loop for each unique engine id
for(i in 1:length(id)){
  # get the subset of the data that only contains the sensor columns for the id i
  sub_data = subset(dataset[,(n_pre_sensor_columns+1):(n_col-n_after_sensor_columns)],dataset[,1]==id[i])
  #sub_data = subset(dataset[,(n_pre_sensor_columns+1):(n_col-n_after_sensor_columns)],dataset$id==id[i])
  # window size is adjusted if the it is greater than the number of rows in the data
  n_row_subdata = nrow(sub_data)
  
  w=ifelse(window_size < n_row_subdata,window_size,n_row_subdata)
  
  # get the rolling mean for all sensors
  rollingmean = rbind(rollingmean,rollapply(sub_data,w,mean,align = "right",partial=1))
  
  # get the rolling sd for all sensors
  rollingsd_i = rollapply(sub_data,w,sd,align = "right",partial=1)
  rollingsd_i[is.na(rollingsd_i)]=0
  rollingsd = rbind(rollingsd,rollingsd_i)
  
  
}
data_a = as.data.frame(rollingmean)
data_sd = as.data.frame(rollingsd)

names(data_a) = a
names(data_sd) = sd



df = cbind(data_a,data_sd)

df2=cbind(dataset[,1:(n_pre_sensor_columns+n_sensor)],df)



# generate output data
dataset = df2
#########################
ltestdata<-dataset#淇濆瓨琛ㄥ鍚堢壒寰佸悗鐨勮缁冩暟鎹?
#########################
# keep only the row with max "cycle"" for each id
dataset<-merge(aggregate(cycle~id,dataset,function(x) x[which.max(x)]),dataset)
dataset =dataset[with(dataset, order(id)), ]
######################################
maxtestdata<-dataset
################################
dataset <- dataset[,-which(names(dataset) == "id")]
##################################
x=dataset

b<-length(x[1,])-3

for(i in 1:b){
  col=prenomaltraindata[,i]
  min_temp<-min(col)
  max_temp<-max(col)
  l<- max_temp-min_temp
  
  for (j in 1:nrow(x)){
    x[j,i]<-ifelse(l==0,0,(x[j,i]-min_temp)/l)
  }
}

#########################################
testdata <- x#淇濆瓨娴嬭瘯鏁版嵁
#################################
# training data without the labels
traindata1= subset(traindata, select=-c(RUL,label1,label2))

# get the feature names 
feature_names = names(traindata1)

# only keep the same feature set in the testing data
testdata = testdata[feature_names]
###################################









data3 <- read.csv("originalRUL.csv")
#######################################
names(data3)="RUL"

# user defined variables to set the windows for classifcation
w1 <- 30
w0 <- 15


data3$label1 <- ifelse(data3$RUL <= w1, 1,0)
data3$label2 <- ifelse(data3$RUL <= w0, 2, ifelse(data3$RUL<=w1,1,0))


data3 <- data3
#######################################
library('dplyr')
testdata<-bind_cols(testdata,data3)
#Read labeled and feature engineered training data

data1 <- traindata
train <- as.data.frame(data1)

data2 <-testdata
test <- as.data.frame(data2)
trainlabel1 <- train["label1"]
testlabel1 <- test["label1"]
#
#Exclude irrelevant label columns from the training data
drops <- c(-49,-51)
train <- as.data.frame(data1)

#Select top 35 features based on Pearson correlation
l=matrix(10,48,2)
for (i in 1:48){
  c=cor(train[,i],train[ ,49])
  l[i,1]=i
  l[i,2]=c
}
l1 <- order(l[,2])
train2 <- train[,l1]
train2 <- train2[,1:35]
train <- train2

feature_names = names(train)

test = test[feature_names]

#Train model using Two-Class Logistic Regression
library(dplyr)
train=bind_cols(train,trainlabel1)
test=bind_cols(test,testlabel1)
lgst <- glm(label1~.,data = train,family = binomial(link = "logit"),control = list(epsilon = 1e-7))
predictions <- predict(lgst, test)
predictions=data.frame(predictions)
predictions<-ifelse(predictions<0.5,0,1)

#Train and evaluate model using Two-Class Boosted Decision Tree
library(gbm)
train=bind_cols(train,trainlabel1)
test=bind_cols(test,testlabel1)
fit <- gbm(label1~.,data = train)

# make predictions
predictions0 <- predict(fit,test,n.tree=100)
predictions0=data.frame(predictions0)
#褰掍竴鍖?
x=predictions0


col=x[,1]
x_min_temp<-min(col)
x_max_temp<-max(col)
l<- x_max_temp-x_min_temp

for (j in 1:nrow(x)){
  x[j,1]<-ifelse(l==0,0,(x[j,1]-x_min_temp)/l)
}
x<-ifelse(x<0.5,0,1)
predictions0=x
predictions0<-ifelse(predictions0<0.5,0,1)

#Train and evaluate model using Two-Class Decision Forest
library(randomForest)

# fit model
fit <- randomForest(label1~., data=train,ntree=10,proximity=TRUE,importance=TRUE)
# summarize the fit
#summary(fit)
# make predictions
predictions1 <- predict(fit, test)
predictions1=data.frame(predictions1)
predictions1<-ifelse(predictions1<0.5,0,1)

#Train and evaluate model using Two-Class Neural Network
train=train[,-which(names(train) == "label1")]
test=test[,-which(names(test) == "label1")]
library(RSNNS)
nn=mlp(train, trainlabel1, size=5, learnFunc="Quickprop", learnFuncParams=c(0.1, 2.0, 0.001, 0.1),maxit=100,inputsTest=test, targetsTest=testlabel1)
predictions2 <- predict(fit, test)
predictions2<-ifelse(predictions2<0.5,0,1)

#################################################
A=data.frame(sapply(testlabel1, as.numeric))
B=data.frame(sapply(predictions, as.numeric))
C=data.frame(sapply(predictions0, as.numeric))
D=data.frame(sapply(predictions1, as.numeric))
E=data.frame(sapply(predictions2, as.numeric))
F=matrix(1:100,nr=100)
F=data.frame(F) 
compare2=bind_cols(F,A,B,C,D,E)
compare2=plyr::rename(compare2, c("F"="ID","label"="Ture value","sapply.predictions..as.numeric."="Two-Class Logistic Regression","sapply.predictions0..as.numeric."="Two-Class Boosted Decision Tree","sapply.predictions1..as.numeric."="Two-Class Decision Forest","sapply.predictions2..as.numeric."="Two-Class Neural Network"))














