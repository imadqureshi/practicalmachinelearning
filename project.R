data <- read.csv(“pml-training.csv”, header = TRUE, na.string = c(“#DIV!”, “NA”, stringsAsFactors = FALSE)
data <- data[, -(1:6)]
cols <- apply (data, 2, function(x) sum(is.na(x)))
data_na <- data[, cols < (nrow(data) * .2)] 

highlyCorPred <- findCorrelation(cor(data_na[, -54]), cutoff = 0.9)
data_na <- data_na [, -highlyCorPred]

inTrain <- createDataPartition (y=data_na$classe, p=0.7, list=FALSE, t=2)
training <- data_na [,  inTrain]
testing <- data_na[, -inTrain]
x_validation <- data_na[-inTrain[, 2], ] #notice t = 2 in createDataPartition

data_na$classe <- as.factor(data_na$classe)

modelFit <- train (classe ~ ., method = "treebag", data=training)

train_predict <- predict(modelFit, training)

test_predict <- predict(modelFit, testing)

x_validation_predict <- predict(modelFit, x_validation_set)

confusionMatrix(train_predict, training$classe)
confusionMatrix(test_predict, testing$classe)
confusionMatrix(x_validation_predict, x_validation_set$classe)

#now get test data and do similar cleansing
test_data <- read.csv(“pml-testing.csv”, header = TRUE, na.string = c(“#DIV!”, “NA”, stringsAsFactors = FALSE)
test_data <- test_data [, -(1:6)]
cols <- apply (test_data, 2, function(x) sum(is.na(x)))
test_data_na <- test_data[, cols < (nrow(test_data) * .2)] 
#remove the same highly correlated predictors. Important to use the same highlycorPred variable because 
#with only 20 samples, if we try tp reevaluate correlated variables, then answer would be something else.
test_data_na <- test_data_na [, -highlyCorPred]
answers <- predict(modelFit, test_data_na)
