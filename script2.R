# Ensayos con bbdd total, pero utilizando las 6 primera variables en el mejor ranking
# segun un p value <= 1e-5 (ver mail Pato 190619)  

# ----------> Variables utilizadas alternativa 1
# rs7597221_A
# exm2269383_C
# GSA-rs10974573_G
# GSA-rs564171_G
# rs725124_C
# rs7173347_C



# ----------> Variables utilizadas alternativa 2
# rs7597221_A
# exm2269383_C
# rs725124_C
# rs7173347_C



# Name: script2.R
# Date creating: 24 jun 2019

# recursos:
#https://datascienceplus.com/fitting-neural-network-in-r/
# MULTILABEL CLASIFICATION
#https://www.r-bloggers.com/multilabel-classification-with-neuralnet-package/
# clasification
#https://www.r-bloggers.com/classification-using-neural-net-in-r/


# Notes:
# 07 julio 2019: Para encontrar la performance en funcion del umbral y del numero
#   de nodos en el hidden layer, vemos que a veces la red neuronal se cae, y el for
#   loop se detiene. Para evitar eso, y que el proceso continue, podemos utilizar 
#   el comando try y tryCatch:
#   http://www.win-vector.com/blog/2012/10/error-handling-in-r/
#   https://www.youtube.com/watch?v=_0X8hRcM5YI

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# DATA LOADING
rm(list = ls())
library(tidyr) #para wide to long
library(tidyverse)
library(caret)
library(neuralnet)
library(pROC)
library(data.table)
library(plyr) 
library(ggplot2)
#load("workdata_for_script2.RData")
#load("workdata_for_script2_altn3.RData") # trabajamos con df que esta depurada.
load("workdata_for_script2_altn4.RData") # trabajamos con df que esta depurada.
source("cross_neural_function.R")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 




# Ya no es necesario ejecutar, pues ya generamos el workdata_for_script2.RData con las 6 variables mas importantes.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Lectura de datos selecciuon de variables 
#solucion con data.table (mas rapido)
# myvars <- c("PHENOTYPE", "rs7597221_A", "exm2269383_C", "GSA-rs10974573_G", "GSA-rs564171_G", "rs725124_C", "rs7173347_C", "GSA-rs2899557") #alternativa 1
# myvars <- c("PHENOTYPE", "rs7597221_A", "exm2269383_C", "rs725124_C", "rs7173347_C") #alternativa 2
# myvars <- c("PHENOTYPE", "rs725124_C","rs7173347_C", "rs1060419_A", "rs943099_T", "rs2497446_A", "rs754497_T", "rs7597221_A", "rs11009438_A",
#             "rs750976_T", "rs2050935_C", "rs35935082_T", "rs12975577_T", "exm2269383_C", 
#             "GSA-rs10974573_G", "GSA-rs564171_G", "GSA-rs11096969_T", "GSA-rs11130489_T", "GSA-rs474179_C", "GSA-rs868823_G", "GSA-rs12495172_A")
            # alternativa 3
myvars <- c("PHENOTYPE", "rs725124_C","rs7173347_C", "rs1060419_A", "rs943099_T", "rs2497446_A", "rs754497_T", "rs7597221_A", "rs11009438_A",
            "rs750976_T", "rs2050935_C", "rs35935082_T", "rs12975577_T", "exm2269383_C", 
            "GSA-rs10974573_G", "GSA-rs564171_G", "GSA-rs11096969_T", "GSA-rs11130489_T", "GSA-rs474179_C", "GSA-rs868823_G", "GSA-rs12495172_A",
            "rs2167107_C",
            "GSA-rs10215532_A",
            "rs4949960_C",
            "rs7514649_A",
            "rs12999695_A",
            "rs17432446_T",
            "rs11254946_T",
            "rs36063745_T",
            "rs2836496_C",
            "GSA-rs2899557_A")
            # alternativa 4 - 08-aug-19



DT <- fread("~/Dropbox/Research/PAPER Pato_Genetica/genotypes original pato jun19.csv")
DT2 <- DT[, myvars, with=FALSE]

data <- DT2

rm(myvars, DT, DT2)
# las variables que tienen gion "-" hay que renombrarlas: para alternativa 1
# renombrar <- c("GSA-rs10974573_G", "GSA-rs564171_G")
# names(data)[names(df) %in% renombrar ] <- c("GSA_rs10974573_G", "GSA_rs564171_G")
# rm(renombrar, cross_neural)

# las variables que tienen gion "-" hay que renombrarlas: para alternativa 3
# renombrar <- c("GSA-rs10974573_G", "GSA-rs564171_G", "GSA-rs11096969_T", "GSA-rs11130489_T", "GSA-rs474179_C", "GSA-rs868823_G", "GSA-rs12495172_A")
# names(df)[names(df) %in% renombrar ] <- c("GSA_rs10974573_G", "GSA_rs564171_G", "GSA_rs11096969_T", "GSA_rs11130489_T", "GSA_rs474179_C", "GSA_rs868823_G", "GSA_rs12495172_A")
# rm(renombrar, cross_neural)


# las variables que tienen gion "-" hay que renombrarlas: para alternativa 4 - 08-aug-19
renombrar <- c("GSA-rs10974573_G", "GSA-rs564171_G", "GSA-rs11096969_T", "GSA-rs11130489_T", "GSA-rs474179_C", "GSA-rs868823_G", "GSA-rs12495172_A", "GSA-rs10215532", "GSA-rs2899557")
names(df)[names(df) %in% renombrar ] <- c("GSA_rs10974573_G", "GSA_rs564171_G", "GSA_rs11096969_T", "GSA_rs11130489_T", "GSA_rs474179_C", "GSA_rs868823_G", "GSA_rs12495172_A", "GSA_rs10215532", "GSA_rs2899557")
rm(renombrar, cross_neural)


#save.image("~/Dropbox/Research/PAPER Pato_Genetica/Genome-Wide-Ass_Data/workdata_for_script2.RData") # con alternativa 1
save.image("~/Dropbox/Research/PAPER Pato_Genetica/Genome-Wide-Ass_Data/workdata_for_script2_altn1.RData") # con alternativa 1, DATOS LISTOS PARA USAR en df.
save.image("~/Dropbox/Research/PAPER Pato_Genetica/Genome-Wide-Ass_Data/workdata_for_script2_altn3.RData") # con alternativa 3, DATOS LISTOS PARA USAR en df.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Data PREPROCESSING
data <- as.data.frame(data)


# Peligro que variable clase row 509 venia con un -9 (fue corregido en el excel)
# y = phenotype (variable clase)
# fid = id sujeto
# iid
# pat
# mat
# convertimos la clase en 0 y 1
#data[,1] <- data[,1] - 1
# convertimos la clase en -1 y 1
data[,1] <- 2*data[,1] - 1
# convertimos las VI en -1, 0 y 1
#data[,c(2:ncol(data))] <- data[,c(2:ncol(data))] - 1

#data[,6] <- as.factor(data[,6])
apply(data,2,function(x) sum(is.na(x))) # vemos que hay varios NAs
data <- data[complete.cases(data), ] # quedamos con 586 sujetos 


apply(data,2,function(x) sum(x>2, na.rm=TRUE)) # VEMOS SI HYA VALORES MAYORES QUE DOS
apply(data,2,function(x) sum(x<0, na.rm=TRUE)) # VEMOS SI HYA VALORES MENORES QUE CERO

sum(data[,1] < -1) # vemos si en la variable clase hay valores menores que -1
sum(data[,1] > 1) # vemos si en la variable clase hay valores menores que -1


#data <- data[-c(500), ]  # la variable clase tiene valor -21!!
df <- data

# drop rows with NAs
df <- data[complete.cases(data), ] # quedamos con 608 sujetos 
apply(df,2,function(x) sum(is.na(x)))
str(df)

# formula
# las variables con un signo "-" hay que renombrarlas:
#colnames(df) <- c("PHENOTYPE", "rs7597221_A", "exm2269383_C", "GSA_rs10974573_G", "GSA_rs564171_G", "rs725124_C", "rs7173347_C") # alternativa 1
colnames(df) <- c("PHENOTYPE", "rs725124_C","rs7173347_C", "rs1060419_A", "rs943099_T", "rs2497446_A", "rs754497_T", "rs7597221_A", "rs11009438_A",
                  "rs750976_T", "rs2050935_C", "rs35935082_T", "rs12975577_T", "exm2269383_C", 
                  "GSA_rs10974573_G", "GSA_rs564171_G", "GSA_rs11096969_T", "GSA_rs11130489_T", "GSA_rs474179_C", "GSA_rs868823_G", "GSA_rs12495172_A") # alternativa 3


#paste("PHENOTYPE  ~", paste(n[2:length(n)], collapse = " + ") )
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# CORRELATION AMONG PREDICTORS
library(corrplot)
library(RColorBrewer)
#M <- cor(df[c(2:7)]) # para alternativa 1
M <- cor(df[c(2:21)]) # para alternativa 3
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 






# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# EXAMPLE (not necessary to executing)
#n <- names(df)[2:7] # alternativa 1
n <- names(df)[2:21] # alternativa 3
f <- as.formula(paste("PHENOTYPE  ~", paste(n[2:length(n)], collapse = " + ") ) )
# Using nural net
model <- neuralnet(formula = f,
                   data= df,
                   hidden = 41,
                   act.fct = "logistic",
                   linear.output = FALSE,
                   threshold = 0.01,
                   lifesign = "none")
# Note that I set the argument linear.output to FALSE in order to 
# tell the model that I want to apply the activation function act.fct and that 
# I am not doing a regression task

plot(model) # son muchas columnas no se entiende  mucho.

# Performance
preds <- predict(model, df[, 2:ncol(df)], type="class")

# a binario
umbral=0.4
preds2 <-  ifelse(preds > umbral, 1, -1)

#Calculate Classification accuracy
or <- as.data.frame(df[,1])
table(as.numeric(preds2), as.numeric(or[,1]))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Utilizando 10 fold cross-validation
# recursos:
# http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/
# https://stats.stackexchange.com/questions/181/how-to-choose-the-number-of-hidden-layers-and-nodes-in-a-feedforward-neural-netw
# https://www.analyticsvidhya.com/blog/2018/11/neural-networks-hyperparameter-tuning-regularization-deeplearning/

#set.seed(450)
df <- as.data.frame(df)
cv.error <- NULL
performance <- cross_neural(df, f, k = 10, proport = 0.8, umbral = 0.8, hidden_number = 30)
performance

# resultados ordenados
performance[which(performance==0)] = NA  # en caso que hayan ceros, los reemplazamos por NA
performance[which(performance=="NaN")] = NA # en caso que hayan NaN
resultados <- rbind(apply(performance[,c(2:ncol(performance))], 2, mean, na.rm=TRUE), apply(performance[,c(2:ncol(performance))], 2, sd, na.rm=TRUE))
rownames(resultados) <- c("mean", "sd")
round(resultados,2)
# # # # # # # # # # # #  BOXPLOT RESULTS
performance <- data.frame(performance)
performance$k <- factor(performance$k)
performance_long <- gather(performance, measure, value, acc:ratio, factor_key=TRUE)
performance_long <-  subset(performance_long, measure != "ratio")
ggplot(performance_long, aes(x = measure, y = value)) +
  geom_boxplot() + theme_bw() +
  stat_summary(fun.y=mean, colour="darkred", geom="point", hape=18, size=3,show_guide = FALSE) +
  stat_summary(fun.y=mean, colour="red", geom="text", show_guide = FALSE, 
               vjust=-1.5, aes( label=round(..y.., digits=2)))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 




# 07-jul-19
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# WHICH IS THE BEST THRESHOLD?
# input: df, f, k, proport, umbral, hidden_number
#set.seed(450)
df <- as.data.frame(df)
umbral <- seq(0.2, 0.8, by=0.05)
rondas <- length(umbral)
fin <- matrix(NA, ncol=5, nrow=rondas)  # umbral, mean acc, mean ratio, sd ratio
for (i in 1:rondas) {
  performance <- cross_neural(df, f, k = 10, proport = 0.8, umbral = umbral[i], hidden_number = 12)
  
  # obntencion de resultados
  performance[which(performance==0)] = NA  # en caso que hayan ceros, los reemplazamos por NA
  performance[which(performance=="NaN")] = NA # en caso que hayan NaN
  resultados <- rbind(apply(performance[,c(2:ncol(performance))], 2, mean, na.rm=TRUE), apply(performance[,c(2:ncol(performance))], 2, sd, na.rm=TRUE))
  rownames(resultados) <- c("mean", "sd")
  
  # almacena resultado que nos interesa
  fin[i, ] <- c(umbral[i], resultados[, "acc"], resultados[, "ratio"])
  
}
colnames(fin) <- c("Threshold", "Macc",  "SDacc",  "MRatio", "SDratio")
fin <- as.data.frame(fin)
pd <- position_dodge(0.1) # move them .05 to the left and right
# # Plot of ration recall/FPR vs Threshold
ggplot(fin, aes(x=Threshold, y=MRatio)) + 
  geom_errorbar(aes(ymin=MRatio-SDratio, ymax=MRatio+SDratio), colour="black", width=.02, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Threshold") +
  ylab("Ratio Recall-FPR") +                  # Use darker colors, lightness=40
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:20*4) +         # Set tick every 4
  theme_bw() 
# # # # 
ggplot(fin, aes(x=Threshold, y=Macc)) + 
  geom_errorbar(aes(ymin=Macc-SDacc, ymax=Macc+SDacc), colour="black", width=.02, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  ylim(0.4, 0.7) + 
  xlab("Threshold") +
  ylab("Accuracy mean") +                  # Use darker colors, lightness=40
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:20*4) +         # Set tick every 4
  theme_bw() 
  #theme(legend.justification=c(1,0), legend.position=c(1,0))           
# # # # Find the better Threshold
id <- which(fin[, "MRatio"] == max(fin[, "MRatio"]))
id2 <- which(fin[, "Macc"] == max(fin[, "Macc"]))
fin[id2,"Threshold"]
message("The best Threshold seems to be: ", paste0(fin[id,"Threshold"])) # 0.8 o 0.4
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 





# 07-jul-19
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# WHICH IS THE BEST NUMBER OF NEURONS TO HIDDEN LAYER?
# de acuerdo a Hecht-Nielsen (1987), el hidden layer debiera tener 2n+1 nodos = 2*20+1=41 nodos
nodes <- seq(20,41) # numero de nodos a probar
rondas <- length(nodes)
fin <- matrix(NA, ncol=7, nrow=rondas)  # umbral, mean ratio, sd ratio
for (nd in 1:rondas) {
  #print(nd)
  performance <- cross_neural(df, f, k = 10, proport = 0.8, umbral = 0.4, hidden_number = nodes[nd])
  
  # obntencion de resultados
  performance[which(performance==0)] = NA  # en caso que hayan ceros, los reemplazamos por NA
  performance[which(performance=="NaN")] = NA # en caso que hayan NaN
  resultados <- rbind(apply(performance[,c(2:ncol(performance))], 2, mean, na.rm=TRUE), apply(performance[,c(2:ncol(performance))], 2, sd, na.rm=TRUE))
  rownames(resultados) <- c("mean", "sd")
  
  # almacena resultado que nos interesa
  fin[nd, ] <- c(nodes[nd], resultados[1, ])
  
}
colnames(fin) <- c("nodes", "acc", "pre", "rec", "f1", "fpr", "ratio")
fin <- as.data.frame(fin)
# # # ploteo de las medidas en funcion de numero de nodos
library("reshape2")
library("ggplot2")
fin2 <- fin
fin2$ratio <- NULL
fin_long <- melt(fin2, id="nodes")  # convert to long format
ggplot(data=fin_long, aes(x=nodes, y=value, colour=variable)) +
  geom_line() + 
  xlab("Number of hidden layer nodes") +
  ylab("Performance measures") +                  # Use darker colors, lightness=40
  #scale_y_continuous(breaks=0:20*4) +         # Set tick every 4
  theme_bw() 
# # # Encontrar el mejor accuracy
id <- which(fin[,"acc"] == max(fin[, "acc"]))
message("The best Accuracy es achieved when number of nodes is: ", paste0(fin[id,"nodes"])) # 12

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 







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

