---
title: "Predicting movement on Weight Lifting Exercise Dataset"
output:
  html_document:
    theme: cerulean
    toc: yes
---
# Overview
The goal of this report is to identify and build a model that will predict motion in a dumbbell weight lifting experiment. The data comes from an experiment performed for the paper titled "Qualitative Activity Recognition of Weight Lifting Exercises". The goal is to detect various types of weight lifting movement (both correct and incorrect). Details about the study can be found at: http://groupware.les.inf.puc-rio.br/har#ixzz3MBxZIS1F

In this report, mutiple model algorithms were evaluated. The most accurate prediction model was created using random forest algorithm. The cross validation model accuracy on the full training data set was quite good at 99.56%. The out of sample error is expected to be approximately 0.44%.

# Method
First, set up the environment. Change the the working directory to the location of the R project files, and load libraries


```r
setwd("E:/_education/_coursera.practicalmachinelearning/project")

if("caret" %in% rownames(installed.packages()) == FALSE) {
  install.packages("caret",
                   repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}
library(caret) #machine learning healper package

if("RWeka" %in% rownames(installed.packages()) == FALSE) {
  install.packages("RWeka",
                   repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}

library(RWeka) #machine learning algorithms
```

Visual inspection of the csv file using Notepad++ indicated that some fields contained NA values but others were empty. In addition, some fields contained the error "#DIV/0!". 

Loaded the data from csv file. Used the function to load the data and standardized some of the values in the data set.

```r
#load data
trg.d <- read.csv("pml-training.csv", head=TRUE, na.strings=c("NA","","#DIV/0!"))
```

Created a temporary data frame to process and saved the data to csv file. Because of the large number of variables and observations, I used GoogleRefine and Weka Explorer to explore the data further.


```r
trg.d.2 <- trg.d
write.csv(trg.d.2,"pml-training-review.csv")
```

Cleaned the data by removing variables with predominantly NA values. In addition, I removed the first 7 variables contained meta data that did not related directly to the predictor class. 

Although exploratory data analysis indicates that the presence of a few skewed data points, I chose not to clean or correct these values. The goal is to choose a model that performs best on slightly imperfect data. This will be the condition when it is implemented in production.


```r
#remove variables with mostly NA values.
colIds <- c(8,9,10,11,37,38,39,40,41,42,43,44,45,46,47,48,49,60,61,62,63,64,65,66,67,68,84,85,86,102,113,114,115,116,117,118,119,120,121,122,123,124,140,151,152,153,154,155,156,157,158,159,160)
trg.d.nona <- subset(trg.d.2,select=colIds)
```

To evaluate the best learning algorithm, I separated data into training and test sets. Also, I created a separate data frame containing the training variables only for prediction.


```r
train.ids <- createDataPartition(y=trg.d.nona$classe,p=0.5,list=FALSE)
trg.nona.train <- trg.d.nona[train.ids,]
trg.nona.test <- trg.d.nona[-train.ids,]

#remove class variable in test data set
trg.nona.test.vars <- trg.nona.test[,-53]
```

Trained multiple models using the pre-processed training data subset using different algorithms (J48, ctree, gbm, LMT, C5.0, ada, rpart2, treebag, blackboost, bstTree). I chose the Random Forest algorithm. Although the time to train is quite long, it results in the best prediction performance. Model accuracy on the pre-processed training data sub-set is approximately 98.98%. Other than variables subsetting, no additional data cleaning or preprocessng was performed.

Used ten fold cross-validation while building the model. However, according to [Random Forest reference documentation](See:http://www.stat.berkeley.edu/~breiman/Using_random_forests_V3.1.pdf), there is no need for cross-validation to get an unbiased estimate of the test set error.


```r
m.trg <- train(classe~., data=trg.nona.train, method="rf",
               trControl = trainControl(method = "cv"))

#predictions using rf model on test set
pred.rf <- predict(m.trg,trg.nona.test.vars)
confusionMatrix(pred.rf,trg.nona.test$classe)
```


# Result

To verify model results on the training data, predictions were made on the test data subset using the model. It was confirmed the model performance in the test data subset was equally strong. The prediction accuracy was 0.9898 with a 95% confidence interval of (0.9876, 0.9917). 


```r
pred.rf <- predict(m.trg,trg.nona.test.vars)
confusionMatrix(pred.rf,trg.nona.test$classe)
```

Note: Because building an rf model is time intensive (particularly in a knitr environment), this output was copied from the original script file.


```r
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 2789   22    0    0    0
         B    0 1871   17    0    0
         C    0    5 1693   33    0
         D    0    0    1 1575   21
         E    1    0    0    0 1782

Overall Statistics
                                          
               Accuracy : 0.9898          
                 95% CI : (0.9876, 0.9917)
    No Information Rate : 0.2844          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9871          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            0.9996   0.9858   0.9895   0.9795   0.9884
Specificity            0.9969   0.9979   0.9953   0.9973   0.9999
Pos Pred Value         0.9922   0.9910   0.9780   0.9862   0.9994
Neg Pred Value         0.9999   0.9966   0.9978   0.9960   0.9974
Prevalence             0.2844   0.1935   0.1744   0.1639   0.1838
Detection Rate         0.2843   0.1907   0.1726   0.1606   0.1817
Detection Prevalence   0.2865   0.1925   0.1765   0.1628   0.1818
Balanced Accuracy      0.9983   0.9918   0.9924   0.9884   0.9941
```

To improve the model accuracy, I retrained the final model on the entire training data set. The full training model accuracy is 99.56%. The out of sample error is expected to be approximately 0.44%.

I decided it was OK to retrain the model on the full training data set because I did not tune the model, nor pre-process the training data based on training or prediction results.


```r
m.trg <- train(classe~., data=trg.d.nona, method="rf",
               trControl = trainControl(method = "cv"))

m.trg.all$finalModel
```

# References

Velloso, E,; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13). Stuttgart, Germany: ACM SIGCHI, 2013. 
Read more: http://groupware.les.inf.puc-rio.br/har#ixzz3MBxZIS1F

