# Hacemos lo mismo que en script2, pero utilizamos otro codigo de 
# cross validation para evitar problemas recurrentes de "script out of bounds"
# encontrados en script2 al ejecutar varias veces neuralnet con cross validation.


# Name: script3.R
# Date creating: 08 aug 2019

# recursos:
#https://datascienceplus.com/fitting-neural-network-in-r/
# MULTILABEL CLASIFICATION
#https://www.r-bloggers.com/multilabel-classification-with-neuralnet-package/
# clasification
#https://www.r-bloggers.com/classification-using-neural-net-in-r/


# Notes:
# 07 julio 2019: 


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
# load("workdata_for_script2_altn1.RData") # trabajamos con df que esta depurada.
# load("workdata_for_script2_altn3.RData") # trabajamos con df que esta depurada.
# load("workdata_for_script2_altn4.RData") # trabajamos con df que esta depurada.
source("cross_neural_function.R")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


