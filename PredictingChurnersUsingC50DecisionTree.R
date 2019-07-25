# 1. Import data
custchurn <- read.csv("custchurn.csv",header = TRUE,sep=",")

# 2. Examine the structure of the data frame
str(custchurn)

# 3. View the data set in a data sheet 
View(custchurn)

# 4. Examine the number of churners vs non-churners in the data
table(custchurn$churn)

# 5. Examine the proportion of these classifications in the data
prop.table(table(custchurn$churn))

# 6.  Set a seed to create replicatable random data
set.seed(5)

# 7. Generate random numbers
randNum = runif(5000)

# 8. Examine the first few rows in the randomised dataset and compare to the original
custRand <- custchurn[order(randNum), ]

# 9. Split the data into 90% training data and 10% test data
custTrain <- custRand[1:4500, ]
custTest <- custRand[4501:5000, ]

# 10. Check that the proportion of classes in the target value have been maintained in 
# both the training and test data
prop.table(table(custTrain$churn))
prop.table(table(custTest$churn))

# 11. Build the classification model, using the 21st column as the target value
library(C50) 
model <- C5.0(custTrain[-21], custTrain$churn)

# 12. Examine the model 
model

# 13. Confirm the number of observations that were used to create the classifier
summary(model) 

# 14. How many features(variables) were used to create the classifier
# 21

# 15. How many levels (decisions) are in the model
# 38

# 16. View the decision levels


# 17. What was the main splitting criterion? (the first decision)
# number_customer_service_calls

# 18.  How many records are incorrectly classified?
# 175

# 19. How many misclassified ‘No’ values are there in the training model?
# 145

# 20. How many misclassified ‘Yes’ values are there in the training model?
# 30

# 21.  Apply the decision tree model to the test data
predict <- predict(model,custTest)

# 22. Compare the vector of predicted class values to the actual class values
table(custTest$churn,predict)

# 23.  How accurate is the model when performed on the test data?
# Correctly predicted 424 Non-Churns and 45 Churns
# for 94% accuracy

# 24.  What was the error rate?
# 6%

# 25. How does the performance of the model on the test data, compare to that of the model on the training data?
# Training  data accuracy was 96.11%

# 26. Boost the decision tree model by specifying that 10 decision trees be used
boostModel <- C5.0(custTrain[-21], custTrain$churn,trials = 10)

# 27. Examine the resulting model
boostModel

# 28. What size (number of decisions/levels) is the tree now?
# Average tree size = 40.3

# 29. Examine the 10 trees
summary(boostModel)

# 30. How many observations were misclassified?
# 78

# 31. What is the error rate?
# 1.7%

# 32.  Apply the boosted model to the test data and check results
boostPredict <- predict(boostModel,custTest)
table(custTest$churn,boostPredict)

# 33. Has the error rate been reduced, and if so, by how much?
# Error rate now 16 out of 500 or  3.2%

# 34. Apply a cost to false negatives by using a cost matrix
errorCost <- matrix(c(0,1,4,0),nrow=2)
costBoostModel <- C5.0(custTrain[-21],custTrain$churn,costs = errorCost)
costBoostPredict <- predict(costBoostModel,custTest)
# 35. View the error cost matrix
table(custTest$churn,costBoostPredict)

# 36. Recreate the decision tree including the cost matrix
plot(costBoostModel)
plot(boostModel)
plot(model)

# 37.  Apply the new model to the test data and examine the results
costBoostPredict <- predict(costBoostModel,custTest)
table(custTest$churn,costBoostPredict)

# 38. How does this model compare to the previous one?
# 1 less false negative but many more false positives (was 2 now 16)

