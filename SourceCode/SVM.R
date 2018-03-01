# SVM Model
#Importing caret and e1071 libraries

library(caret)
library(e1071)


#reading the training dataset from the csv
prudentialData_Train <- read.csv(file="/Users/shrutimehta/Desktop/ADS/Midterm/NewCSVFolder/Final_Training_Data.csv", header=TRUE, sep= ',')
# reading the test dataset from the csv
prudentialData_Test <- read.csv(file="/Users/shrutimehta/Desktop/ADS/Midterm/NewCSVFolder/Final_Test_Data.csv", header=TRUE, sep= ',')

#identify the structure of the train dataset
str(prudentialData_Train)

#identify the structure of the test dataset
dim(prudentialData_Train)


#Tuning the svm models. 
#We will get optimal cost and gamma parameter which we will use in next section for creating svm model.
#We are using Linear, Polynomial, Sigmoid and Radial kernels to create svm model, later we will check the accuracy of all the kernels whichever is best in terms of accuracy we will pick it.

#tuning the dataset using "sigmoid kernel"
svm_tune_sigm <- tune(svm, response ~ .,
                      data = prudentialData_Train,
                      kernel="sigmoid",
                      ranges=list(cost=10^(-1:2),
                                  gamma=c(.5,1,2),
                                  scale=F
                      ))
#summary of the tuning
sumry_sigm <- summary(svm_tune_sigm)
#printing the summary
print(sumry_sigm$best.parameters)


#tuning the dataset using "radial kernel"
svm_tune_rad <- tune(svm, response ~ .,
                      data = prudentialData_Train,
                      kernel="radial",
                      ranges=list(cost=10^(-1:2),
                                  gamma=c(.5,1,2),
                                  scale=F
                      ))
#summary of the tuning
sumry_rad <- summary(svm_tune_rad)
#printing the summary
print(sumry_rad$best.parameters)


#tuning the dataset using "polynomial kernel"
svm_tune_poly <- tune(svm, response ~ .,
                      data = prudentialData_Train,
                      kernel="polynomial",
                      ranges=list(cost=10^(-1:2),
                                  gamma=c(.5,1,2),
                                  scale=F
                      ))
#summary of the tuning
sumry_ploy <- summary(svm_tune_poly)
#printing the summary
print(sumry_poly$best.parameters)


#tuning the dataset using "linear kernel"
svm_tune_lin <- tune(svm, response ~ .,
                      data = prudentialData_Train,
                      kernel="linear",
                      ranges=list(cost=10^(-1:2),
                                  gamma=c(.5,1,2),
                                  scale=F
                      ))
#summary of the tuning
sumry_lin <- summary(svm_tune_lin)
#printing the summary
print(sumry_lin$best.parameters)


#Cost is set to 0.1 intitially which is general penalizing parameter, it's a cost of penalizaling for misclassification. So if C is large the bias will be low and variance high. Gamma is parameter of Guassian kernel, used for nonlinear structures. In our same, for linear optimal cost is 1, for radial it is 10 and gamma 0.5. We will use these values for build our svm model.
svm_fit_radial <- svm(response ~ ., kernel="radial", cost = 10, gamma=0.5,data = prudentialData_Train)
print(svm_fit_radial)

svm_fit_sigmoid <- svm(response ~ ., kernel="sigmoid", cost = 10, gamma=0.5,data = prudentialData_Train)
print(svm_fit_sigmoid)

svm_fit_polynomial <- svm(response ~ ., kernel="polynomial", cost = 10, gamma=0.5,data = prudentialData_Train)
print(svm_fit_polynomial)

svm_fit_linear <- svm(response ~ ., kernel="linear", cost = 10,data = prudentialData_Train)
print(svm_fit_linear)




#Now we will predict the Credit Worthiness (response) feature for all the svm models with diffrent kernels and later in next section we will check for the accuracy of the prediction.

predictions <-  predict(svm_fit_linear, prudentialData_Test)
table(prudentialData_Test, predictions)

predictions <-  predict(svm_fit_polynomial, prudentialData_Test)
table(prudentialData_Test, predictions)

predictions <-  predict(svm_fit_sigmoid, prudentialData_Test)
table(prudentialData_Test, predictions)

predictions <-  predict(svm_fit_radial, prudentialData_Test)
table(prudentialData_Test, predictions)

