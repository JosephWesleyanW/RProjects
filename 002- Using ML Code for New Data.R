library(caret)

data(dhfr)
d <- dhfr

head(d)
tail(d)

summary(d)

sum(is.na(d))

#install.packages("skimr")

library(skimr)
skim(d)

library(dplyr)
d %>%
  dplyr::group_by(Y) %>%
  skim()
 


# Data viz

plot(d)
# 228 variables so plot does not work

# Scatter plot

plot(d$moeGao_Abra_L, d$moeGao_Abra_L)

plot(d$Y, d$moeGao_Abra_basicity)

plot(d$Y, d$moe2D_zagreb)

# Feature Plots

featurePlot(x = d[,2:5],
            y = d$Y,
            plot = "box",
            strip= strip.custom(par.strip.text= list(cex=.7)),
            scales = list(x = list(relation="free"),
                          y = list(relation="free")))
 


data(dhfr)

sum(is.na(d))

set.seed(42)

# Performing stratified random split on the data set

TrainingIndex <- createDataPartition(d$Y, p = 0.8, list = FALSE)
TrainingSet <- d[TrainingIndex,]
TestingSet <- d[-TrainingIndex,]



# SVM model (polynomial kernel)

# Build Training model
Model <- train(Y ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Build CV model
Model.cv <- train(Y ~ ., data = TrainingSet,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess=c("scale","center"),
                  trControl= trainControl(method="cv", number=10),
                  tuneGrid = data.frame(degree=1,scale=1,C=1)
)


# Apply model for prediction
Model.training <-predict(Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model, TestingSet) # Apply model to make prediction on Testing set
Model.cv <-predict(Model.cv, TrainingSet) # Perform cross-validation

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Y)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Y)
Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Y)

print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

# Feature importance
Importance <- varImp(Model)
plot(Importance, top = 25)
plot(Importance, col = "red")













