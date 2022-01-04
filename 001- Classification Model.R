# Installing required libraries
# install.packages(c("datasets","caret","lattice"))
# You can use c("packageX","packageY") to install multiple packages at once.

# Importing installed libraries
library(datasets) # Contains the Iris data set
library(caret) # Package for machine learning algorithms / CARET stands for Classification And REgression Training
library(ggplot2)
library(lattice)

# Importing the Iris data set
data(iris)

# Check to see if there are missing data
sum(is.na(iris))

# To achieve reproducible model; set the random seed number
set.seed(42)

# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(iris$Species, p=0.8, list = FALSE)
# 80% goes to training set
TrainingSet <- iris[TrainingIndex,] # Training Set
TestingSet <- iris[-TrainingIndex,] # Test Set


# Compare scatter plot of the 80 and 20 data subsets
plot(TrainingSet,
     main = "TrainingSet Scatter Plot with Line",
     pch = 1)


plot(TestingSet,
     main = "TrainingSet Scatter Plot with Line",
     pch = 1)



###############################
# SVM model (polynomial kernel)

# Build Training model
Model <- train(Species ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)
# With the training model, model is trained
# with the 80% of the data to predict the ones
# in the test data.

# Basically  model will learn from training data
# and apply what it learned to testing data.

# Build CV model
Model.cv <- train(Species ~ ., data = TrainingSet,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess=c("scale","center"),
                  trControl= trainControl(method="cv", number=10),
                  tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# In CV Model it is 10 folded which means our TrainingSet that have 
# 120 samples is divided to 10 and now each group contains 12 samples
# Model is making one group passive and learn from the other 9
# and predicts the 1 passive group
# and this process goes in a loop for all of the 10 groups.
# This is cross-validation.




# Apply model for prediction
Model.training <-predict(Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model, TestingSet) # Apply model to make prediction on Testing set
Model.cv <-predict(Model.cv, TrainingSet) # Perform cross-validation

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Species)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Species)
Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Species)

print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

# Feature importance
# To see which variable is the most important.
# For Versicolor, sepal lenght was not a noticeable
# feature.

# Petal Width and Length was the most important feature
# for all variables.
Importance <- varImp(Model)
plot(Importance)
plot(Importance, col = "red")

