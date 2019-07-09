# Ensayos con datos de prueba

# Name: script1.R
# Date creatin: 03 jun 2019

# recursos:
#https://datascienceplus.com/fitting-neural-network-in-r/
# MULTILABEL CLASIFICATION
#https://www.r-bloggers.com/multilabel-classification-with-neuralnet-package/
# clasification
#https://www.r-bloggers.com/classification-using-neural-net-in-r/
# Notes:

rm(list = ls())
library(tidyverse)
library(caret)
library(neuralnet)
library(pROC)
# Lectura de datos
data <- read.csv("bbdd_test030619csv.csv") # son 176 columnas.
# Peligro que variable clase row 509 venia con un -9 (fue corregido en el excel)
# y = phenotype (variable clase)
# fid = id sujeto
# iid
# pat
# mat
# convertimos la clase en factor
data[,6] <- data[,6] - 1
#data[,6] <- as.factor(data[,6])
apply(data,2,function(x) sum(is.na(x)))

# drop rows with NAs
df <- data[complete.cases(data), ] # quedamos con 477 sujetos 
apply(df,2,function(x) sum(is.na(x)))
str(df)


# formula
n <- names(df[c(7:176)])
f <- as.formula(paste("PHENOTYPE  ~", paste(n[1:length(n)], collapse = " + ") ) )


# Using nural net
model <- neuralnet(formula = f,
                   data= df,
                   hidden = 10,
                   act.fct = "logistic",
                   linear.output = FALSE,
                   threshold = 0.01,
                   lifesign = "minimal")
# Note that I set the argument linear.output to FALSE in order to 
# tell the model that I want to apply the activation function act.fct and that 
# I am not doing a regression task

plot(model) # son muchas columnas no se entiende  mucho.

# Performance
preds <- predict(model, df[,c(7:176)], type="class")
# a binario
umbral=0.5
preds2 <-  ifelse(preds > umbral, 1, 0)

#Calculate Classification accuracy
table(preds2, df[,6])





# # # # # # # # Utilizando 10 fold cross-validation
# recurso: http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/

#set.seed(450)
cv.error <- NULL
k <- 10
library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)
umbral = 0.5
performance <- matrix(NA, ncol=5, nrow=k)

index <- sample(1:nrow(df),round(0.9*nrow(df)))
train.cv <- df[index,]
test.cv <- df[-index,]

fpr_x <- matrix(NA, ncol=k, nrow = nrow(test.cv)+1) # numero de filas no esta claro pero es el nrow del test dataset
tpr_y <- matrix(NA, ncol=k, nrow = nrow(test.cv)+1)
for(i in seq_along(1:k)){
  index <- sample(1:nrow(df),round(0.9*nrow(df)))
  train.cv <- df[index,]
  test.cv <- df[-index,]
  
  # training the NN
  nn <- neuralnet(formula = f, data= train.cv, hidden = 10, act.fct = "logistic", linear.output = FALSE,
                  threshold = 0.01, lifesign = "minimal")
  
  
  # Predict with the Test set
  pr.nn <- predict(nn, test.cv[,c(7:176)], type="class")
  # a binario
  preds2 <-  ifelse(pr.nn  > umbral, 1, 0)
  
  # # # calculo de curva roc promedio.
  # promedio burdo de las curvas roc.
  # clase del test set, mientras que pr.nn es el valor numerico de la prediccion.
  rucurve <- roc(test.cv[,6], as.numeric(pr.nn))
  fpr_x[, i] <- rucurve$specificities
  tpr_y[, i] <- rucurve$sensitivities
  
  #table(preds2, test.cv[,6])
  cm <- as.matrix(table(Actual = test.cv[,6], Predicted = preds2)) # create the confusion matrix
  # calculo de medidas de desempeno
  # https://blog.revolutionanalytics.com/2016/03/com_class_eval_metrics_r.html
  
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diags = sum(diag(cm)) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  
  acc = diags / n 
  rec = cm[1,1]/(cm[1,1]+cm[1,2])
  pre = cm[1,1]/(cm[1,1]+cm[2,1])
  f1 = 2 * pre * rec / (pre + rec)
  performance[i,] <- c(i, acc, pre, rec, f1)
  
  #pbar$step()  # para mostrar barra de progreso
}
colnames(performance) <- c("k", "acc", "pre", "rec", "f1")
# resultados ordenados
resultados <- rbind(apply(performance[,c(2:5)], 2, mean), apply(performance[,c(2:5)], 2, sd))
resultados <- cbind(c("mean", "sd"), resultados)
resultados


# roc curve links:
# https://cran.r-project.org/web/packages/plotROC/vignettes/examples.html
# https://rdrr.io/cran/cvAUC/man/cvAUC.html
# https://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html
# http://rstudio-pubs-static.s3.amazonaws.com/220197_7131fb0b2455404cb95ea8f788d45828.html
# https://stats.stackexchange.com/questions/221409/varying-classification-threshold-to-produce-roc-curves

library(pROC)
test_class <- test.cv[,6] # clase del test set, mientras que pr.nn es el valor numerico de la prediccion.
rucurve <- roc(test.cv[,6], as.numeric(pr.nn)) # lista: nos interesa thresholds, sensitivities y specificities
plot(roc(test.cv[,6], as.numeric(pr.nn)), col="black", lwd=3, main="roc", xlab="fpr - specificity", ylab="tpr - sensitivity")

