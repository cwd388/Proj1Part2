#Project 1 - Part II
#Chad W. Dunham

#Installing and loading required packages
#For these, I am assuming that the user has the packages installed already, and thus am commenting out the actual process of obtaining the packages
#Also, some of the packages are needed for areas not covered by the assignment, and are commented out for this limited purpose

#install.packages("caret", dependencies = TRUE)
#install.packages("nnet", dependencies=TRUE)
#install.packages("randomForest", dependencies=TRUE)
#install.packages("kernlab", dependencies=TRUE)
#install.packages("corrplot", dependencies=TRUE)
library(caret)
#library(nnet)
#library(randomForest)
#library(kernlab)
library(corrplot)

#Importing the wine data, and saving it as a variable
wine = read.csv("http://www.nd.edu/~mclark19/learn/data/goodwine.csv")


#Create the correlation matrix from the text
corrplot(cor(wine[, -c(13, 15)]), method ="number", tl.cex = .8) #I changed the text size from the text for aesthetic purposes


#Creating the partitions for the training set
set.seed(1234)  #As noted in the text, this is to keep results the same when re-running
trainIndices = createDataPartition(wine$good, p=.8, list = F)
wanted = !colnames(wine) %in% c("free.sulfur.dioxide", "density", "quality", "color", "white")  #As the author notes, these variables all have very high r-squared scores, and thus are being disregarded
wine_train = wine[trainIndices, wanted]
wine_test = wine[-trainIndices, wanted]


#Creating a box plot off of this data
wine_trainplot = predict(preProcess(wine_train[,-10], method="range"), wine_train[,-10])
featurePlot(wine_trainplot, wine_train$good, "box")


#Setting up and running the k-fold validation
set.seed(1234)
cv_opts = trainControl(method="cv", number=10)
knn_opts = data.frame(.k=c(seq(3, 11, 2), 25, 51, 101))
results_knn = train(good~., data=wine_train, method="knn", preProcess="range", trControl=cv_opts, tuneGrid=knn_opts)
results_knn


#Running the model on the test set
preds_knn = predict(results_knn, wine_test[,-10])
confusionMatrix(preds_knn, wine_test[,10], positive="Good")
dotPlot(varImp(results_knn))


#END OF CODE