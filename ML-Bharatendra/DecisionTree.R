# Prepare data

data <- read.csv("Cardiotocographic.csv")
str(data)

# change NSP into a categorical variable by using factor

data$NSPF <- factor(data$NSP)


# Partition data into Training and Validation datatsets

set.seed(1234)

pd <- sample(2, nrow(data), replace = TRUE, prob = c(0.8,0.2))

train <- data[pd == 1,]

validate <- data[pd == 2,]


# Decision Tree with party

library(party)

tree <- ctree(NSPF~LB+AC+FM, data = train, controls = ctree_control(mincriterion = 0.99, minsplit = 500))

print(tree)

plot(tree)


# Predict

predict(tree, validate)

predict(tree, validate, type = "prob")


# Decision Tree with rpart

library(rpart)

tree1 <- rpart(NSPF ~ LB+AC+FM, train)

library(rpart.plot)

rpart.plot(tree1, extra = 1)


# Prediction 

predict(tree1, validate)


# Misclassification error for 'train' data

tab <- table(predict(tree), train$NSPF)

print(tab)

1 - sum(diag(tab))/sum(tab)     # Misclassification error


# Misclassification error with validate data

testPred <- predict(tree, newdata = validate)

tab <- table(testPred, validate$NSPF)

print(tab)

1 - sum(diag(tab))/sum(tab)
