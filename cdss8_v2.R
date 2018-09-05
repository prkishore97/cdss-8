setwd("C:/Users/ram.p/Documents/R/CDSS8")

######### Reading Datasets #########
data_train=read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"))
data_test=read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"))
dim(data_train)

######### Removing variables with non-zero variance #########
library(caret)
nrv=nearZeroVar(data_train)
refine_train=data_train[,-nrv]
dim(refine_train)

######### Removing variables which are irrelevant to the model building #########
refine_train=refine_train[,-c(1,2,3,4,5)]
dim(refine_train)

######### Removing variables with more NA values #########
na_col=as.data.frame(colSums(is.na(refine_train)))
na_df=as.data.frame(cbind("col"=rownames(na_col),"nas"=round(as.numeric(na_col[,1]/nrow(refine_train)),2)))
nas=na_df[na_df$nas!=0,]
refine_train=refine_train[,!(names(refine_train) %in% nas[,1])]
dim(refine_train)

######### Correllogram #########
for_cor=refine_train
library(corrgram)
corrgram(for_cor[,!(names(for_cor) %in% "classe")], order=NULL, lower.panel=panel.shade,upper.panel=NULL, text.panel=panel.txt,main="Correllogram")

######### Removing highly correlated variables #########
refine_train2=refine_train
refine_train2=as.data.frame(sapply(refine_train[,!(names(refine_train) %in% "classe")],as.numeric))
m=abs(cor(refine_train2[,!((names(refine_train2)) %in% "classe")]))
m[upper.tri(m)]=0
diag(m)=0
uncor_data=refine_train2[,!apply(m,2,function(x) any(x>0.8))]
dim(uncor_data)

######### Partitioning into test and train datasets #########
uncor_data=data.frame(cbind(uncor_data,"classe"=refine_train$classe))
intrain=createDataPartition(y=uncor_data$classe,p=0.8,list=F)
training=uncor_data[intrain,]
validation=uncor_data[-intrain,]

######### Decision Tree #########
modelfit_dt=train(classe~.,data=training,method="rpart")
fancyRpartPlot(modelfit_dt$finalModel)
confusionMatrix(modelfit_dt)

######### Random Forest #########
library(randomForest)
modelfit_rf=randomForest(classe~.,data=training)
modelfit_rf

######### Prediction on Validation dataset #########
pred_rf1=predict(modelfit_rf,validation)
cm=table(pred_rf1,validation$classe)
n=sum(cm)
diag=diag(cm)
accuracy=sum(diag)/n

######### Refining test dataset #########
refine_test=data_test[,-nrv]
refine_test=refine_test[,-c(1,2,3,4,5)]
refine_test=refine_test[,!(names(refine_test) %in% nas[,1])]
refine_test=refine_test[,!(names(refine_test) %in% "problem_id")]
testing=refine_test[,!apply(m,2,function(x) any(x>0.8))]
dim(testing)

######### Prediction on test dataset #########
final_pred=predict(modelfit_rf,testing)
final_pred






