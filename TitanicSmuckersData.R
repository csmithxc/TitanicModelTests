library(tidyr)
library(dplyr)
library(titanic)
library(nnet)
library(caret)

train_data <- titanic_train
test_data <- titanic_test


#Convert age to binary child
train_data <- mutate(train_data, Child = ifelse(Age < 18, 1, ifelse(Age > 60, 2, 0)))
test_data <- mutate(test_data, Child = ifelse(Age < 18, 1, ifelse(Age > 45, 2, 0)))
train_data$Child[is.na(train_data$Child)] <- ifelse(grepl("Master", train_data$Name[is.na(train_data$Child)]), 1, ifelse(grepl("Miss", train_data$Name[is.na(train_data$Child)]), 1, 0)) 
test_data$Child[is.na(test_data$Child)] <- ifelse(grepl("Master", test_data$Name[is.na(test_data$Child)]), 1, ifelse(grepl("Miss", test_data$Name[is.na(test_data$Child)]), 1, 0)) 
train_data <- mutate(train_data, Level = substr(Cabin, 1, 1))
test_data <- mutate(test_data, Level = substr(Cabin, 1, 1))
train_data <- mutate(train_data, Family = ifelse(Parch > 1, 1, ifelse(SibSp > 0, 1, 0)))
test_data <- mutate(test_data, Family = ifelse(Parch > 1, 1, ifelse(SibSp > 0, 1, 0)))



#setting seed allows repeatable results
set.seed(2)

#create neural net
nnet.fit <- nnet(Survived ~ factor(Pclass)+Sex+factor(Child)+factor(Family), data = train_data, size = 3)

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

predicted <- data.frame(test_data[c(1:418), 1], predicted$predicted)
colnames(predicted)[1] <- "PassengerId"
colnames(predicted)[2] <- "Survived"
#write.csv(predicted, "NNetTest6.csv", row.names = FALSE)