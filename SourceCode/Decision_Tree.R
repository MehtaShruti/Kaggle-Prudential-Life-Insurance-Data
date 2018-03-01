install.packages("rpart.plot")
library(rpart)
library(rpart.plot)



training_data <- read.csv(file = "Final_Training_Data.csv")
test_data <-read.csv(file = "Final_Test_Data.csv")
tree <- rpart(response~., training_data, method="class")

prp(tree)

plotcp(tree)

rsq.rpart(tree)

prune_tree<- prune(tree, cp=0.012)

prp(prune_tree)

predict_data <- predict(prune_tree, test_data, type = "class")

test_data$response_predicted <- predict_data

# this will give u accuracy
count = 0
for(i in 1:nrow(test_data)){
  if(test_data[i,53] == test_data[i,54]){
    count = count + 1
  }
}
count/9382*100
