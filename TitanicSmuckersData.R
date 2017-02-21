library(tidyr)
library(dplyr)
library(titanic)
library(nnet)
library(caret)

train_data <- titanic_train
test_data <- titanic_test


#Convert age to binary child
train_data <- mutate(train_data, Child = ifelse(Age < 18, 1, 0))
test_data <- mutate(test_data, Child = ifelse(Age < 18, 1, 0))


#setting seed allows repeatable results
set.seed(2)

#create neural net
nnet.fit <- nnet(Survived ~ factor(Pclass)+Sex+factor(Child), data = train_data, size = 3)

#predict who survived
predicted <- predict(nnet.fit, test_data)

predicted <- mutate(as.data.frame(predicted), ifelse(V1 < 0.5, 0, 1))

colnames(predicted)[2] <- "predicted"

test_data_survived <- titanic_gender_class_model

test_data <- inner_join(test_data, test_data_survived, by = "PassengerId")



confusionMatrix(as.factor(predicted$predicted),
                test_data_survived$Survived,
                positive = "1",
                mode = "everything")