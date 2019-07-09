
# Creation: July, 07 2019
# Funcion para hacer cross validation con rd neuronal.
# requisitos: df es una dataframe, y f es la formula del modelo para neural net
# Inputs:
#     df: dataframe completa
#     f = formula de modelo
#     umbral = umbral de corte para probabilidad de prediccion
#     proport = proporcion de los datos para training
#     hidden_number = numero de neuronas de la capa intermedia
#     k = numero de corss validations
# Outputs:
#     performance: data frame con medias de acc, pre, rec y f1 para cada cross validation
cross_neural <- function(df, f, k = 10, proport = 0.8, umbral = 0.5, hidden_number = 10) {
  predictores <- 2:ncol(df) # numero de variables predictoras
  npredic <- ncol(df) # numero de columnas
  performance <- matrix(NA, ncol=7, nrow = k)
  for(i in seq_along(1:k)){
    # generating the train and test sets
    index <- sample(1:nrow(df),round(proport*nrow(df)))
    train.cv <- df[index,]
    test.cv <- df[-index,]
    
    # training the NN
    nn <- neuralnet(formula = f, data= train.cv, hidden = hidden_number, act.fct = "logistic", linear.output = FALSE,
                    threshold = 0.01, lifesign = "minimal")
    
    
    # Predict with the Test set
    pr.nn <- predict(nn, test.cv[ , 2:npredic], type="class")
    # a binario
    preds2 <-  ifelse(pr.nn  > umbral, 1, 0)
    
    # # # calculo de curva roc promedio.
    # promedio burdo de las curvas roc.
    # clase del test set, mientras que pr.nn es el valor numerico de la prediccion.
    #rucurve <- roc(test.cv[, 1], as.numeric(pr.nn))
    #fpr_x[, i] <- rucurve$specificities
    #tpr_y[, i] <- rucurve$sensitivities
    
    # Creating confusion matrix
    #table(preds2, test.cv[,6])
    cm <- as.matrix(table(Actual = test.cv[, 1], Predicted = preds2)) # create the confusion matrix
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
    fpr = cm[2,2]/(cm[2,2]+cm[2,1])   # false positive rate
    ratio = rec/fpr
    performance[i,] <- c(i, acc, pre, rec, f1, fpr, ratio)
    
  }
  colnames(performance) <- c("k", "acc", "pre", "rec", "f1", "fpr", "ratio") # ratio=rec/fpr 
  return(performance)
}
# example
# performance <- cross_neural(df, f, k = 10, proport = 0.8, umbral = 0.5, hidden_number = 10)
# performance
