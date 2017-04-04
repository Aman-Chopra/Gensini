
#Tips before running :
#
#This script assumes that the data is present in a file named "new_data.csv" (please note that xsl format has been converted to csv)
#present in a folr named "R_Files" located in "~/Documents/R_Files"
#If this script is to be run on any other location or file :
#1 . Set the parameters to setwd() as the location of the file (dataset in csv form)
#2 . Rename the file name parameter in read.csv() to the dataset name with file extension (along with quotes to notify it as a string). 

setwd("C:/Users/win-8/Documents/fwdphilipshackaboutteam27")

#PRE-REQUISITE LIBRARIES : 

print("Installing Required Libraries")

#install.packages("caTools")
#install.packages("MASS")
#install.packages("lars")
#install.packages("neuralnet")
#install.packages("NeuralNetTools")
#install.packages("glmnet")
#install.packages("ridge")
#---------------------------------

#FILLING SOME MISSING VALUES

#Note : As there were only 2 missing values in the Entire Dataset (specifically in FI attribute) , only a mean was used for the missing values.

print("Getting Data")

#patient_data <- read.csv("new_data.csv")
patient_data <- read.csv("Dataset.csv")

print("Inserting Missing Values in the Data-Set")

#patient_data$FI[28] <- 47.99
#patient_data$FI[6] <- 35.938666
patient_data$FI[185] <- 35.938666
patient_data$FI[207] <- 47.99
#patient_data$FI[53] <- 35.93866

print(summary(patient_data))

#Shuffling the dataset for unbiased results
aman<-patient_data
set.seed(98)
random<- runif(nrow(patient_data))
patient_data<-aman[order(random),]


#DIVIDING THE DATASET INTO GROUPS

#Note : Check Group Membership according to GROUP variable

print("Dividing the Data-Set into Groups given")

group_1 <- subset(patient_data , GROUP == 1)	
group_2 <- subset(patient_data , GROUP == 2)
group_3 <- subset(patient_data , GROUP == 3)
group_4 <- subset(patient_data , GROUP == 4)

library(corrplot)

x<-group_1[,c(1,2,3,4,5,6,7,8,10,11,12,13,14,16)];
y<-group_1[,15];
M<-cor(x,y)
cor(x,y)
corrplot(M, method="number")



x<-group_2[,c(1,2,3,4,5,6,7,8,10,11,12,13,14,16)];
y<-group_2[,15];
M<-cor(x,y)
cor(x,y)
corrplot(M, method="number")

x<-group_3[,c(1,2,3,4,5,6,7,8,10,11,12,13,14,16)];
y<-group_3[,15];
M<-cor(x,y)
cor(x,y)
corrplot(M, method="number")

x<-group_4[,c(1,2,3,4,5,6,7,8,10,11,12,13,14,16)];
y<-group_4[,15];
M<-cor(x,y)
cor(x,y)
corrplot(M, method="number")


print("Group - 1")
print(summary(group_1))

print("Group - 2")
print(summary(group_2))

print("Group - 3")
print(summary(group_3))

print("Group - 4")
print(summary(group_4))

library("caTools")

#Note : For the second objective , we divide the dataset according to the 4 groups and try to predict the GENSINI scores using other independent variables using 
#Ridge Regression , LASSO Method and Neural Networks. Then we check the importance of the independent variables in predicting the GENSINI scores and 
#hence determine the factors which affect it the most.


#SAMPLE SPLIT AND RIDGE REGRESSION !!

library("MASS")

#LIBRARY "MASS" REQUIRED FOR RIDGE REGRESSION

#GROUP 1

#SET RANDOM NUMBER SEED

print("Dividing the indivisual group datasets into training and testing")

set.seed(101)
group_1_sample <- sample.split(group_1$GENSINI , SplitRatio = 0.75)

#Note : sample.split used to split dataset into training and testing according to the random seed.

group_1_train <- subset(group_1 , group_1_sample == TRUE)
group_1_test <- subset(group_1 , group_1_sample == FALSE)

print("Group - 1 Train")
print(summary(group_1_train))

print("Group - 1 Test ")
print(summary(group_1_test))

#Calculating the mean square error
mse = function(x,y) { mean((x-y)^2)}

#Note : Perform Ridge Regression
library (ridge)
print("Performing Ridge Regression on 1")

#group_1_ridge <- lm.ridge(GENSINI ~ . - GENSINI - GROUP - SYNTAX , lambda = seq(0 , 10 , 0.001) , data = group_1_train)
#print("Lambda")
#Note : Find Lambda which Minimizes cost function.
#print(group_1_ridge$lambda[which.min(group_1_ridge$GCV)])

linRidgeMod <- linearRidge(GENSINI ~ .- GENSINI - GROUP - SYNTAX, data = group_1_train)
predicted <- predict(linRidgeMod,group_1_test)
print(linRidgeMod)
print(predicted)
print(group_1_test$GENSINI)
#compare <- cbind (actual=group_2_test$GENSINI, predicted)
#mean (apply(compare, 1, min)/apply(compare, 1, max))
#mse(predicted,group_2_test$GENSINI)
print(summary(sqrt((predicted - group_1_test$GENSINI)^2)))


#require(glmnet)
#x <- as.matrix(group_1_train[,c(-9,-15,-16)]) # Removes class
#y <- as.double(as.matrix(group_1_train[, 15])) # Only class

# Fitting the model (Ridge: Alpha = 0)
#set.seed(999)
#cv.ridge <- cv.glmnet(x, y, family='multinomial', alpha=0, parallel=TRUE, standardize=TRUE, type.measure='auc')

# Results
#plot(cv.ridge)
#cv.ridge$lambda.min
#cv.ridge$lambda.1se
#coef(cv.ridge, s=cv.ridge$lambda.min)



#Note : Repeat Procedure for other Groups

#GROUP 2

set.seed(101)
group_2_sample <- sample.split(group_2$GENSINI , SplitRatio = 0.75)
group_2_train <- subset(group_2 , group_2_sample == TRUE)
group_2_test <- subset(group_2 , group_2_sample == FALSE)

print("Group - 2 Train")
print(summary(group_2_train))
print("Group - 2 Test ")
print(summary(group_2_test))

print("Performing Ridge Regression on 2")

#group_2_ridge <- lm.ridge(GENSINI ~ . - GENSINI - GROUP - SYNTAX , lambda = seq(0,1, 0.00001) , data = group_2_train )
#print("Lambda")
#print(group_2_ridge$lambda[which.min(group_2_ridge$GCV)])



linRidgeMod <- linearRidge(GENSINI ~ .- GENSINI - GROUP - SYNTAX, data = group_2_train)
predicted <- predict(linRidgeMod,group_2_test)
print(linRidgeMod)
print(predicted)
print(group_2_test$GENSINI)
#compare <- cbind (actual=group_2_test$GENSINI, predicted)
#mean (apply(compare, 1, min)/apply(compare, 1, max))
#mse(predicted,group_2_test$GENSINI)
print(summary(sqrt((predicted - group_2_test$GENSINI)^2)))



#GROUP 3

group_3_sample <- sample.split(group_3$GENSINI , SplitRatio = 0.75)
group_3_train <- subset(group_3 , group_3_sample == TRUE)
group_3_test <- subset(group_3 , group_3_sample == FALSE)
print("Group - 3 Train")
print(summary(group_3_train))
print("Group - 3 Test ")
print(summary(group_3_test))

print("Performing Ridge Regression on 3")

#group_3_ridge <- lm.ridge(GENSINI ~ . - GENSINI - GROUP - SYNTAX , lambda = seq(0,5 ,0.0001) , data = group_3_train)
#print("Lambda")
#print(group_3_ridge$lambda[which.min(group_3_ridge$GCV)])

linRidgeMod <- linearRidge(GENSINI ~ .- GENSINI - GROUP - SYNTAX, data = group_3_train)
predicted <- predict(linRidgeMod,group_3_test)
print(linRidgeMod)
print(predicted)
print(group_3_test$GENSINI)
#compare <- cbind (actual=group_3_test$GENSINI, predicted)
#mean (apply(compare, 1, min)/apply(compare, 1, max))
#mse(predicted,group_3_test$GENSINI)
print(summary(sqrt((predicted - group_3_test$GENSINI)^2)))


#GROUP 4

group_4_sample <- sample.split(group_4$GENSINI , SplitRatio = 0.75)
group_4_train <- subset(group_4 , group_4_sample == TRUE)
group_4_test <- subset(group_4 , group_4_sample == FALSE)

print("Group - 4 Train")
print(summary(group_4_train))
print("Group - 4 Test ")
print(summary(group_4_test))

print("Performing Ridge Regression on 4")

#group_4_ridge <- lm.ridge(GENSINI ~ . - GENSINI - GROUP - SYNTAX , lambda = seq(0,1,0.00001) , data = group_4_train)
#print("Lambda")
#print(group_4_ridge$lambda[which.min(group_4_ridge$GCV)])

linRidgeMod <- linearRidge(GENSINI ~ .- GENSINI - GROUP - SYNTAX, data = group_4_train)
predicted <- predict(linRidgeMod,group_4_test)
print(linRidgeMod)
print(predicted)
print(group_4_test$GENSINI)
#compare <- cbind (actual=group_4_test$GENSINI, predicted)
#mean (apply(compare, 1, min)/apply(compare, 1, max))
#mse(predicted,group_4_test$GENSINI)
print(summary(sqrt((predicted - group_4_test$GENSINI)^2)))

#Note : Trying the LASSO method for prediction from here.

library(lars)

#Note : "lars" - Required library for running the LASSO estimation 

#GROUP 1

print("Trying LASSO Method on 1")
group_1_lasso <- lars(as.matrix(group_1_train[,!(names(group_1_train) %in% c("GROUP" , "GENSINI" , "SYNTAX"))]) , group_1_train$GENSINI)
#class(group_1_train)
#mode()
#Note : Create LASSO Model

#plot(group_1_lasso)

#Note : Plot the LASSO Model Characteristics

#CROSS VALIDATION
print("Cross Validating on 1")
group_1_lasso_cv <-cv.lars(as.matrix(group_1_train[,!(names(group_1_train) %in% c("GROUP" , "GENSINI" , "SYNTAX"))]) , group_1_train$GENSINI)
group_1_bestfraction <- group_1_lasso_cv$index[which.min(group_1_lasso_cv$cv)]

#Note : Run Cross Validation and get th best fraction.
print("Computing Coefficients")
group_1_lasso_coef <- predict(group_1_lasso , as.matrix(group_1_test[,!(names(group_1_test) %in% c("GROUP" , "GENSINI" , "SYNTAX"))]) , s = group_1_bestfraction , type = "coefficient" , mode = "fraction")

group_1_lasso_coef

#Note : Get Coefficients of the predicted model.

#PREDICTION

group_1_lasso_pred <- predict(group_1_lasso , as.matrix(group_1_test[,!(names(group_1_test) %in% c("GROUP" , "GENSINI" , "SYNTAX"))]) , s = group_1_bestfraction , type = "fit" , mode = "fraction")$fit
print("Accuracy According to RMSE : ")
group_1_lasso_pred
group_1_test$GENSINI
print(summary(sqrt((group_1_lasso_pred - group_1_test$GENSINI)^2)))

#Note : Use predict() function to test model on test data created and find the Squared Error

#Note : Repeat for other Groups.

#GROUP 2
print("Trying LASSO Method on 2")

group_2_lasso <- lars(as.matrix(group_2_train[,!(names(group_2_train) %in% c("GROUP" , "GENSINI" , "SYNTAX"))]) , group_2_train$GENSINI)
#plot(group_2_lasso)

#CROSS VALIDATION
print("Cross Validating on 2")

group_2_lasso_cv <-cv.lars(as.matrix(group_2_train[,!(names(group_2_train) %in% c("GROUP" , "GENSINI" , "SYNTAX"))]) , group_2_train$GENSINI)

group_2_bestfraction <- group_2_lasso_cv$index[which.min(group_2_lasso_cv$cv)]
print("Computing Coefficients")

group_2_lasso_coef <- predict(group_2_lasso , as.matrix(group_2_test[,!(names(group_2_test) %in% c("GROUP" , "GENSINI" , "SYNTAX"))]) , s = group_2_bestfraction , type = "coefficient" , mode = "fraction")

group_2_lasso_coef

#PREDICTION

group_2_lasso_pred <- predict(group_2_lasso , as.matrix(group_2_test[,!(names(group_2_test) %in% c("GROUP" , "GENSINI" , "SYNTAX"))]) , s = group_2_bestfraction , type = "fit" , mode = "fraction")$fit
print("Accuracy According to RMSE : ")
group_2_lasso_pred
group_2_test$GENSINI
print(summary(sqrt((group_2_lasso_pred - group_2_test$GENSINI)^2)))


#GROUP 3
print("Trying LASSO Method on 3")

group_3_lasso <- lars(as.matrix(group_3_train[,!(names(group_3_train) %in% c("GROUP" , "GENSINI" , "SYNTAX"))]) , group_3_train$GENSINI)
#plot(group_3_lasso)

#CROSS VALIDATION
print("Cross Validating on 3")

group_3_lasso_cv <-cv.lars(as.matrix(group_3_train[,!(names(group_3_train) %in% c("GROUP" , "GENSINI" , "SYNTAX"))]) , group_3_train$GENSINI)

group_3_bestfraction <- group_3_lasso_cv$index[which.min(group_3_lasso_cv$cv)]
print("Computing Coefficients")

group_3_lasso_coef <- predict(group_3_lasso , as.matrix(group_3_test[,!(names(group_3_test) %in% c("GROUP" , "GENSINI" , "SYNTAX"))]) , s = group_3_bestfraction , type = "coefficient" , mode = "fraction")

group_3_lasso_coef

#PREDICTION

group_3_lasso_pred <- predict(group_3_lasso , as.matrix(group_3_test[,!(names(group_3_test) %in% c("GROUP" , "GENSINI" , "SYNTAX"))]) , s = group_3_bestfraction , type = "fit" , mode = "fraction")$fit
print("Accuracy According to RMSE : ")
group_3_lasso_pred
group_3_test$GENSINI
print(summary(sqrt((group_3_lasso_pred - group_3_test$GENSINI)^2)))

#GROUP 4

print("Trying LASSO Method on 4")

group_4_lasso <- lars(as.matrix(group_4_train[,!(names(group_4_train) %in% c("GROUP" , "GENSINI" , "SYNTAX"))]) , group_4_train$GENSINI)
#plot(group_4_lasso)

#CROSS VALIDATION
print("Cross Validating on 4")

group_4_lasso_cv <-cv.lars(as.matrix(group_4_train[,!(names(group_4_train) %in% c("GROUP" , "GENSINI" , "SYNTAX"))]) , group_4_train$GENSINI)

group_4_bestfraction <- group_4_lasso_cv$index[which.min(group_4_lasso_cv$cv)]
print("Computing Coefficients")

group_4_lasso_coef <- predict(group_4_lasso , as.matrix(group_4_test[,!(names(group_4_test) %in% c("GROUP" , "GENSINI" , "SYNTAX"))]) , s = group_4_bestfraction , type = "coefficient" , mode = "fraction")

group_4_lasso_coef

#PREDICTION
print("Accuracy According to RMSE : ")

group_4_lasso_pred <- predict(group_4_lasso , as.matrix(group_4_test[,!(names(group_4_test) %in% c("GROUP" , "GENSINI" , "SYNTAX"))]) , s = group_4_bestfraction , type = "fit" , mode = "fraction")$fit
group_4_lasso_pred
group_4_test$GENSINI
print(summary(sqrt((group_4_lasso_pred - group_4_test$GENSINI)^2)))

# FINDING THE SQUARED ERROR FOR RIDGE REGRESSION

# The "MASS" package does not provide an inbuilt method for prediction. Hence the code below calculates the predicted values according to
# y_pred = (intercept) + theta_1*x_1 + theta_2*x_2 + ... + theta_n*x_n
# Theta values are in the "coef" variable of group_n_ridge

#GROUP 1

group_1_ridge <- lm.ridge(GENSINI ~ . - GENSINI - GROUP - SYNTAX , lambda = seq(0 , 10 , 0.001) , data = group_1_train)
#print("Lambda")
#Note : Find Lambda which Minimizes cost function.
#print(group_1_ridge$lambda[which.min(group_1_ridge$GCV)])
group_1_ridge_predict <- scale(group_1_test[, !(names(group_2_test) %in% c("GROUP" , "GENSINI" , "SYNTAX"))] , center = F , scale = group_1_ridge$scales)%*%group_1_ridge$coef[,which.min(group_1_ridge$GCV)] + group_1_ridge$ym
print("RIDGE-REGRESSION RMSE VALUES - GROUP 1")
print(summary(sqrt((group_1_ridge_predict - group_1_test$GENSINI)^2)))

#GROUP 2
group_2_ridge <- lm.ridge(GENSINI ~ . - GENSINI - GROUP - SYNTAX , lambda = seq(0 , 10 , 0.001) , data = group_2_train)
group_2_ridge_predict <- scale(group_2_test[, !(names(group_2_test) %in% c("GROUP" , "GENSINI" , "SYNTAX"))] , center = F , scale = group_2_ridge$scales)%*%group_2_ridge$coef[,which.min(group_2_ridge$GCV)] + group_2_ridge$ym
print("RIDGE-REGRESSION RMSE VALUES - GROUP 2")
print(summary(sqrt((group_2_ridge_predict - group_2_test$GENSINI)^2)))

#GROUP 3
group_3_ridge <- lm.ridge(GENSINI ~ . - GENSINI - GROUP - SYNTAX , lambda = seq(0 , 10 , 0.001) , data = group_3_train)
group_3_ridge_predict <- scale(group_3_test[, !(names(group_3_test) %in% c("GROUP" , "GENSINI" , "SYNTAX"))] , center = F , scale = group_3_ridge$scales)%*%group_3_ridge$coef[,which.min(group_3_ridge$GCV)] + group_3_ridge$ym
print("RIDGE-REGRESSION RMSE VALUES - GROUP 3")
print(summary(sqrt((group_3_ridge_predict - group_3_test$GENSINI)^2)))

#GROUP 4
group_4_ridge <- lm.ridge(GENSINI ~ . - GENSINI - GROUP - SYNTAX , lambda = seq(0 , 10 , 0.001) , data = group_4_train)
group_4_ridge_predict <- scale(group_4_test[, !(names(group_4_test) %in% c("GROUP" , "GENSINI" , "SYNTAX"))] , center = F , scale = group_4_ridge$scales)%*%group_4_ridge$coef[,which.min(group_4_ridge$GCV)] + group_4_ridge$ym
print("RIDGE-REGRESSION RMSE VALUES - GROUP 4")
print(summary(sqrt((group_4_ridge_predict - group_4_test$GENSINI)^2)))

#TRYING THE NEURAL NETWORK FOR PRDICTION FROM HERE

library(neuralnet)

#Library "neuralnet" is required for the implementation of the Neural Network.

#GROUP 1
print("Using Neural Network on Group - 1 . Normalizing and splitting data into train and test.")
nn_max_1 <-apply(group_1 , 2, max) 
nn_min_1 <-apply(group_1 , 2, min)

#Note : Find the MAX and MIN of the attributes in the Dataset.

nn_scaled_1 <- as.data.frame(scale(group_1 , center = nn_min_1 , scale = nn_max_1 - nn_min_1))

#Note : Scale the data frame (Normalize)

index_1 <- sample(1:nrow(group_1),round(0.75*nrow(group_1)))
nn_train_1 <- nn_scaled_1[index_1 , ]
nn_test_1 <- nn_scaled_1[-index_1 , ]

#Note : Split the data to a training and a testing set.

n_1 <- names(nn_train_1)
f_1 <- as.formula(paste("GENSINI ~" , paste(n_1[!n_1 %in% c("GENSINI","GROUP","SYNTAX")] , collapse = "+")))

#Note : Generate the formula for the Neural Network in form (Y ~ X)

nn_1 <- neuralnet(f_1 , data = nn_train_1 , hidden = c(5.3) , linear.output = T)
plot(nn_1)

#Note : Implement and plot the Neural Network model.

print("Making Predictions on Test Dataset")
nn_pred_1 <- compute(nn_1,nn_test_1[, !(names(nn_test_1) %in% c("GENSINI" , "SYNTAX" , "GROUP"))])

#Note : Make predictions on the Dataset - TEST

nn_pred_1_ <- nn_pred_1$net.result*(max(group_1$GENSINI) - min(group_1$GENSINI)) + min(group_1$GENSINI)

#Note : As data was Normalized , bring it back to original form

test_r_1 <- (nn_test_1$GENSINI)*(max(group_1$GENSINI) - min(group_1$GENSINI)) + min(group_1$GENSINI)
print("Computing Accuracy as RMSE for Group 1")
sum((test_r_1 - nn_pred_1_)^2)/(nrow(nn_test_1))
print(summary(sqrt(((test_r_1 - nn_pred_1_)^2))))

#Note : Display the Squared Error estimate (mean).

#GROUP 2
print("Using Neural Network on Group - 2 . Normalizing and splitting data into train and test.")

nn_max_2 <-apply(group_2 , 2, max) 
nn_min_2 <-apply(group_2 , 2, min)
nn_scaled_2 <- as.data.frame(scale(group_2 , center = nn_min_2 , scale = nn_max_2 - nn_min_2))
index_2 <- sample(1:nrow(group_2),round(0.75*nrow(group_2)))
nn_train_2 <- nn_scaled_2[index_2 , ]
nn_test_2 <- nn_scaled_2[-index_2 , ]
n_2 <- names(nn_train_2)
f_2 <- as.formula(paste("GENSINI ~" , paste(n_2[!n_2 %in% c("GENSINI","GROUP","SYNTAX")] , collapse = "+")))
nn_2 <- neuralnet(f_2 , data = nn_train_2 , hidden = c(5) , linear.output = T)
plot(nn_2)
print("Making Predictions on Test Dataset")

nn_pred_2 <- compute(nn_2,nn_test_2[, !(names(nn_test_2) %in% c("GENSINI" , "SYNTAX" , "GROUP"))])
nn_pred_2_ <- nn_pred_2$net.result*(max(group_2$GENSINI) - min(group_2$GENSINI)) + min(group_2$GENSINI)
test_r_2 <- (nn_test_2$GENSINI)*(max(group_2$GENSINI) - min(group_2$GENSINI)) + min(group_2$GENSINI)
print("Computing Accuracy as RMSE for Group 2")

sum((test_r_2 - nn_pred_2_)^2)/(nrow(nn_test_2))
print(summary(sqrt((test_r_2 - nn_pred_2_)^2)))

#GROUP 3
print("Using Neural Network on Group - 3 . Normalizing and splitting data into train and test.")

nn_max_3 <-apply(group_3 , 2, max) 
nn_min_3 <-apply(group_3 , 2, min)
nn_scaled_3 <- as.data.frame(scale(group_3 , center = nn_min_3 , scale = nn_max_3 - nn_min_3))
index_3 <- sample(1:nrow(group_3),round(0.75*nrow(group_3)))
nn_train_3 <- nn_scaled_3[index_3 , ]
nn_test_3 <- nn_scaled_3[-index_3 , ]
n_3 <- names(nn_train_3)
f_3 <- as.formula(paste("GENSINI ~" , paste(n_3[!n_3 %in% c("GENSINI","GROUP","SYNTAX")] , collapse = "+")))
nn_3 <- neuralnet(f_3 , data = nn_train_3 , hidden = c(5) , linear.output = T)
plot(nn_3)
print("Making Predictions on Test Dataset")

nn_pred_3 <- compute(nn_3,nn_test_3[, !(names(nn_test_3) %in% c("GENSINI" , "SYNTAX" , "GROUP"))])
nn_pred_3_ <- nn_pred_3$net.result*(max(group_3$GENSINI) - min(group_3$GENSINI)) + min(group_3$GENSINI)
test_r_3 <- (nn_test_3$GENSINI)*(max(group_3$GENSINI) - min(group_3$GENSINI)) + min(group_3$GENSINI)
print("Computing Accuracy as RMSE for Group 3")

sum((test_r_3 - nn_pred_3_)^2)/(nrow(nn_test_3))
print(summary(sqrt((test_r_3 - nn_pred_3_)^2)))

#GROUP 4
print("Using Neural Network on Group - 4 . Normalizing and splitting data into train and test.")

nn_max_4 <-apply(group_4 , 2, max) 
nn_min_4 <-apply(group_4 , 2, min)
nn_scaled_4 <- as.data.frame(scale(group_4 , center = nn_min_4 , scale = nn_max_4 - nn_min_4))
index_4 <- sample(1:nrow(group_4),round(0.75*nrow(group_4)))
nn_train_4 <- nn_scaled_4[index_4 , ]
nn_test_4 <- nn_scaled_4[-index_4 , ]
n_4 <- names(nn_train_4)
f_4 <- as.formula(paste("GENSINI ~" , paste(n_4[!n_4 %in% c("GENSINI","GROUP","SYNTAX")] , collapse = "+")))
nn_4 <- neuralnet(f_4 , data = nn_train_4 , hidden = c(5) , linear.output = T)
plot(nn_4)
print("Making Predictions on Test Dataset")

nn_pred_4 <- compute(nn_4,nn_test_4[, !(names(nn_test_4) %in% c("GENSINI" , "SYNTAX" , "GROUP"))])
nn_pred_4_ <- nn_pred_4$net.result*(max(group_4$GENSINI) - min(group_4$GENSINI)) + min(group_4$GENSINI)
test_r_4 <- (nn_test_4$GENSINI)*(max(group_4$GENSINI) - min(group_4$GENSINI)) + min(group_4$GENSINI)
print("Computing Accuracy as RMSE for Group 4")

sum((test_r_4 - nn_pred_4_)^2)/(nrow(nn_test_4))
print(summary(sqrt((test_r_4 - nn_pred_4_)^2)))


print("Using Neural Network on entire Dataset")

nn_max_entire <-apply(patient_data , 2, max) 
nn_min_entire <-apply(patient_data , 2, min)
nn_scaled_entire <- as.data.frame(scale(patient_data , center = nn_min_entire , scale = nn_max_entire - nn_min_entire))
index_entire <- sample(1:nrow(patient_data),round(0.75*nrow(patient_data)))
nn_train_entire <- nn_scaled_entire[index_entire , ]
nn_test_entire <- nn_scaled_entire[-index_entire , ]
n_entire <- names(nn_train_entire)
f_entire <- as.formula(paste("GENSINI ~" , paste(n_entire[!n_entire %in% c("GENSINI","GROUP","SYNTAX")] , collapse = "+")))
nn_entire <- neuralnet(f_entire , data = nn_train_entire , hidden = c(5) , linear.output = T)
plot(nn_entire)
print("Making Predictions on Test Dataset")
nn_pred_entire <- compute(nn_entire,nn_test_entire[, !(names(nn_test_entire) %in% c("GENSINI" , "SYNTAX" , "GROUP"))])
nn_pred_entire_ <- nn_pred_entire$net.result*(max(patient_data$GENSINI) - min(patient_data$GENSINI)) + min(patient_data$GENSINI)
test_r_entire <- (nn_test_entire$GENSINI)*(max(patient_data$GENSINI) - min(patient_data$GENSINI)) + min(patient_data$GENSINI)

print("Computing Accuracy as RMSE on Entire Dataset")

sum((test_r_entire - nn_pred_entire_)^2)/(nrow(nn_test_entire))
print(summary(sqrt((test_r_entire - nn_pred_entire_)^2)))



#RUNNING ALL MODELS ON TRAIN DATA ITSELF

print("Predicting values on the Train Data itself to check the possibility of an Over-Fit  !! ")


group_1_lasso_pred <- predict(group_1_lasso , as.matrix(group_1_train[,!(names(group_1_train) %in% c("GROUP" , "GENSINI" , "SYNTAX"))]) , s = group_1_bestfraction , type = "fit" , mode = "fraction")$fit
print("Accuracy According to RMSE on Group - 1 (LASSO TRAIN): ")

print(summary(sqrt((group_1_lasso_pred - group_1_train$GENSINI)^2)))

group_2_lasso_pred <- predict(group_2_lasso , as.matrix(group_2_train[,!(names(group_2_train) %in% c("GROUP" , "GENSINI" , "SYNTAX"))]) , s = group_2_bestfraction , type = "fit" , mode = "fraction")$fit
print("Accuracy According to RMSE on Group - 2 (LASSO TRAIN): ")

print(summary(sqrt((group_2_lasso_pred - group_2_train$GENSINI)^2)))

group_3_lasso_pred <- predict(group_3_lasso , as.matrix(group_3_train[,!(names(group_3_train) %in% c("GROUP" , "GENSINI" , "SYNTAX"))]) , s = group_3_bestfraction , type = "fit" , mode = "fraction")$fit
print("Accuracy According to RMSE on Group - 3 (LASSO TRAIN): ")

print(summary(sqrt((group_3_lasso_pred - group_3_train$GENSINI)^2)))

group_4_lasso_pred <- predict(group_4_lasso , as.matrix(group_4_train[,!(names(group_4_train) %in% c("GROUP" , "GENSINI" , "SYNTAX"))]) , s = group_4_bestfraction , type = "fit" , mode = "fraction")$fit
print("Accuracy According to RMSE on Group - 4 (LASSO TRAIN): ")

print(summary(sqrt((group_4_lasso_pred - group_4_train$GENSINI)^2)))


print("Testing Neural Network on the same training data : GROUP 1")
nn_pred_1 <- compute(nn_1,nn_train_1[, !(names(nn_train_1) %in% c("GENSINI" , "SYNTAX" , "GROUP"))])
nn_pred_1_ <- nn_pred_1$net.result*(max(group_1$GENSINI) - min(group_1$GENSINI)) + min(group_1$GENSINI)
test_r_1 <- (nn_train_1$GENSINI)*(max(group_1$GENSINI) - min(group_1$GENSINI)) + min(group_1$GENSINI)
print("Computing Accuracy as RMSE for Group 1")

sum((test_r_1 - nn_pred_1_)^2)/(nrow(nn_train_1))
print(summary(sqrt((test_r_1 - nn_pred_1_)^2)))


print("Testing Neural Network on the same training data : GROUP 2")
nn_pred_2 <- compute(nn_2,nn_train_2[, !(names(nn_train_2) %in% c("GENSINI" , "SYNTAX" , "GROUP"))])
nn_pred_2_ <- nn_pred_2$net.result*(max(group_2$GENSINI) - min(group_2$GENSINI)) + min(group_2$GENSINI)
test_r_2 <- (nn_train_2$GENSINI)*(max(group_2$GENSINI) - min(group_2$GENSINI)) + min(group_2$GENSINI)
print("Computing Accuracy as RMSE for Group 2")

sum((test_r_2 - nn_pred_2_)^2)/(nrow(nn_train_2))
print(summary(sqrt((test_r_2 - nn_pred_2_)^2)))


print("Testing Neural Network on the same training data : GROUP 3")
nn_pred_3 <- compute(nn_3,nn_train_3[, !(names(nn_train_3) %in% c("GENSINI" , "SYNTAX" , "GROUP"))])
nn_pred_3_ <- nn_pred_3$net.result*(max(group_3$GENSINI) - min(group_3$GENSINI)) + min(group_3$GENSINI)
test_r_3 <- (nn_train_3$GENSINI)*(max(group_3$GENSINI) - min(group_3$GENSINI)) + min(group_3$GENSINI)
print("Computing Accuracy as RMSE for Group 3")

sum((test_r_3 - nn_pred_3_)^2)/(nrow(nn_train_3))
print(summary(sqrt((test_r_3 - nn_pred_3_)^2)))

print("Testing Neural Network on the same training data : GROUP 4")
nn_pred_4 <- compute(nn_4,nn_train_4[, !(names(nn_train_4) %in% c("GENSINI" , "SYNTAX" , "GROUP"))])
nn_pred_4_ <- nn_pred_4$net.result*(max(group_4$GENSINI) - min(group_4$GENSINI)) + min(group_4$GENSINI)
test_r_4 <- (nn_train_4$GENSINI)*(max(group_4$GENSINI) - min(group_4$GENSINI)) + min(group_4$GENSINI)
print("Computing Accuracy as RMSE for Group 4")

sum((test_r_4 - nn_pred_4_)^2)/(nrow(nn_train_4))
print(summary(sqrt((test_r_4 - nn_pred_4_)^2)))

#determining th severity of CAD in the four groups (as GENSINI scores is a direct indication of the severity of CAD).


#Note : Variation of GENSINI among Groups.

print("Variation of GENSINI among Groups")
hist(group_1$GENSINI)
hist(group_2$GENSINI)
hist(group_3$GENSINI)
hist(group_4$GENSINI)

print("Finding Confidence Intervals - (95 %)")

print("Lower Bound for - 2")
print((-1)*(sd(group_2$GENSINI)/(sqrt(nrow(group_2)))*2  - mean(group_2$GENSINI)))
print("Upper Bound for - 2")
print(sd(group_2$GENSINI)/(sqrt(nrow(group_2)))*2 + mean(group_2$GENSINI))


print("Lower Bound for - 3")
print((-1)*(sd(group_3$GENSINI)/(sqrt(nrow(group_3)))*2  - mean(group_3$GENSINI)))
print("Upper Bound for - 3")
print(sd(group_3$GENSINI)/(sqrt(nrow(group_3)))*2 + mean(group_3$GENSINI))


print("Lower Bound for - 4")
print((-1)*(sd(group_4$GENSINI)/(sqrt(nrow(group_4)))*2  - mean(group_4$GENSINI)))
print("Upper Bound for - 4")
print(sd(group_4$GENSINI)/(sqrt(nrow(group_4)))*2 + mean(group_4$GENSINI))





