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
df <- data[complete.cases(data), ] # quedamos con 478 sujetos 
apply(df,2,function(x) sum(is.na(x)))
str(df)


# formula
n <- names(df[c(7:176)])
f <- as.formula(paste("PHENOTYPE  ~", paste(n[1:length(n)], collapse = " + ") ) )


# Using nural net
library("neuralnet")
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

#boolean_integer <- function(arg1, umbral) {
  arg1[arg1 >= umbral] <- 1
  arg1[arg1 < umbral] <- 0
  arg1 <- as.integer(arg1)
}
#preds2 <- boolean_integer(preds, umbral=0.5)

#Calculate Classification accuracy
table(preds2, df[,6])