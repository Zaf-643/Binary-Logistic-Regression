# Binary-Logistic-Regression# Logistic Regression

# Importing the dataset
training_set = read.csv('training_set.csv')

# Encoding the target feature as factor
training_set$Purchased = factor(training_set$Purchased, 
                                levels = c(0, 1))


# Fitting Logistic Regression to the Training set
classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set)

summary(classifier)

# Importing the test dataset
test_set = read.csv('test_set.csv')


# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', 
                    newdata = test_set[-3])
prob_pred
y_pred = ifelse(prob_pred > 0.5, 1, 0)
y_pred


# Making the Confusion Matrix
cm = table(test_set[, 3] > .5, y_pred > .5)
cm
