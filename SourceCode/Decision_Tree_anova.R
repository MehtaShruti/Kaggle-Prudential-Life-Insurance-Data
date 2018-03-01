install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# install.packages("forecast")
# library(forecast)

training_data <- read.csv(file = "Final_Training_Data.csv")
test_data <-read.csv(file = "Final_Test_Data.csv")
tree <- rpart(response~., training_data, method = "anova")

prp(tree)

plotcp(tree)

rsq.rpart(tree)

prune_tree<- prune(tree, cp=0.011)

prp(prune_tree)

predict_data <- round(predict(prune_tree, test_data, type = "vector"))

test_data$response_predicted <- predict_data

# this will give u accuracy
count = 0
for(i in 1:nrow(test_data)){
  if(test_data[i,53] == test_data[i,54]){
    count = count + 1
  }
}
count/9382*100

#accuracy(test_data$response_predicted, test_data$response)
