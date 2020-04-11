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
ltraindata<-dataset#存储标签后的训练数据
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
prenomaltraindata<-dataset#保存归一化前的数�?
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
ltestdata<-dataset#
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
testdata <- x#保存测试数据
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
























##############################################
#####################读取数据
ortrain <- traindata

ortest <- testdata
#####################删除回归不相关不相关的标签列并单独回归所用的提出RUL标签�?
train=ortrain[,-which(names(ortrain) == "label1")]
train=train[,-which(names(train) == "label2")]

test=ortest[,-which(names(ortest) == "label1")]
test=test[,-which(names(test) == "label2")]

trainRUL=ortrain['RUL']
testRUL=ortest['RUL']

#######################################选取与RUL皮尔逊相关系数最大的前三十五个特�?
n=length(train[2,])-1

l=matrix(10,n,2)

for (i in 1:n){
  c=cor(train[,i],train[,68])
  l[i,1]=i
  l[i,2]=c
}

l1=order(l[,2])

train2=train[,l1]

train2=train2[,1:35]
train=train2

feature_names = names(train)

test = test[feature_names]
##########################################重新将RUL列加入训练数�?
library(dplyr)

train=bind_cols(train,trainRUL)
test=bind_cols(test,testRUL)

######################################回归森林
library(randomForest)

# fit model
fit <- randomForest(RUL~., data=train,ntree=10,proximity=TRUE,importance=TRUE)

# make predictions
predictions <- predict(fit, test)

predictions=data.frame(predictions)

########################################生成评估模型矩阵
evaluate1 <- matrix(0,nr=2,nc=6)

a=sapply(predictions - testRUL, as.numeric)

evaluate1[1,1]='Decision Forest Regression'

evaluate1[1,2]=mean(abs(a))

evaluate1[1,3]<-(mean(a^2))^0.5

mu=sapply(testRUL, as.numeric)
evaluate1[1,4]<-mean(abs(a)/mu)

mu = mean(sapply(testRUL, as.numeric))
b=sapply(mu-testRUL, as.numeric)
evaluate1[1,5] = mean(a^2)/mean(b^2)
y=evaluate1[1,5]
t=as.data.frame(lapply(y,as.numeric))
evaluate1[1,6]=1-t[1,1]
############################################增强树回�?
library(gbm)
fit <- gbm(RUL~., data=train)

# make predictions
predictions0 <- predict(fit,test,n.tree=100)
predictions0=data.frame(predictions0)
#################################################生成评估矩阵
a=sapply(predictions0 - testRUL, as.numeric)

evaluate1[2,1]='Boosted Decision Tree Regression'

evaluate1[2,2]=mean(abs(a))

evaluate1[2,3]<-(mean(a^2))^0.5

mu=sapply(testRUL, as.numeric)
evaluate1[2,4]<-mean(abs(a)/mu)

mu = mean(sapply(testRUL, as.numeric))
b=sapply(mu-testRUL, as.numeric)
evaluate1[2,5] = mean(a^2)/mean(b^2)

y=evaluate1[2,5]
t=as.data.frame(lapply(y,as.numeric))
evaluate1[2,6]=1-t[1,1]
##############################################重命名评估表
evaluate1=data.frame(evaluate1)

evaluate1=plyr::rename(evaluate1, c("X1"="Algorithm","X2"="Mean Absolute Error", "X3"="Root Mean Squared Error","X4"="Relative Absolute Error","X5"="Relative Squared Error","X6"="Coefficient of Determination"))

##################################################泊松回归
output <-glm(formula = RUL~., data = train, family = poisson)
predictions1 <- predict(output,test)

predictions1=exp(predictions1)
################################################生成评估矩阵
evaluate2 <- matrix(0,nr=2,nc=6)

a=sapply(predictions1 - testRUL, as.numeric)

evaluate2[1,1]='Possion'

evaluate2[1,2]=mean(abs(a))

evaluate2[1,3]<-(mean(a^2))^0.5

mu=sapply(testRUL, as.numeric)
evaluate2[1,4]<-mean(abs(a)/mu)

mu = mean(sapply(testRUL, as.numeric))
b=sapply(mu-testRUL, as.numeric)
evaluate2[1,5] = mean(a^2)/mean(b^2)

y=evaluate2[1,5]
t=as.data.frame(lapply(y,as.numeric))
evaluate2[1,6]=1-t[1,1]
###################################################去除RUL�?
train=train[,-which(names(train) == "RUL")]
test=test[,-which(names(test) == "RUL")]
###################################################神经网络算法
library(RSNNS)

nn=mlp(train, trainRUL, size=5, learnFunc="Quickprop", learnFuncParams=c(0.1, 2.0, 0.001, 0.1),maxit=100,inputsTest=test, targetsTest=testRUL)

#nn=elman(train, trainRUL, size=8, learnFuncParams=c(0.1), maxit=100)

#nn=jordan(train, trainRUL, size=8, learnFuncParams=c(0.1), maxit=100)

predictions2 = predict(nn,test)
##################################################
a=sapply(predictions2 - testRUL, as.numeric)

evaluate2[2,1]='Neural Network Regression'

evaluate2[2,2]=mean(abs(a))

evaluate2[2,3]<-(mean(a^2))^0.5 

mu=sapply(testRUL, as.numeric)
evaluate2[2,4]<-mean(abs(a)/mu)

mu = mean(sapply(testRUL, as.numeric))
b=sapply(mu-testRUL, as.numeric)
evaluate2[2,5] = mean(a^2)/mean(b^2)

y=evaluate2[2,5]
t=as.data.frame(lapply(y,as.numeric))
evaluate2[2,6]=1-t[1,1]
#################################################
evaluate2=data.frame(evaluate2)

evaluate2=plyr::rename(evaluate2, c("X1"="Algorithm","X2"="Mean Absolute Error", "X3"="Root Mean Squared Error","X4"="Relative Absolute Error","X5"="Relative Squared Error","X6"="Coefficient of Determination"))


library(dplyr)
evaluate=bind_rows(evaluate1,evaluate2)
A=data.frame(sapply(testRUL, as.numeric))
B=data.frame(sapply(predictions, as.numeric))
C=data.frame(sapply(predictions0, as.numeric))
D=data.frame(sapply(predictions1, as.numeric))
E=data.frame(sapply(predictions2, as.numeric))
F=matrix(1:100,nr=100)
F=data.frame(F) 
compare=bind_cols(F,A,B,C,D)
compare=plyr::rename(compare, c("F"="ID","RUL"="Ture value","predictions"="Decision Forest Regression","predictions0"="Boosted Decision Tree Regression","sapply.predictions1..as.numeric."="Possion"))