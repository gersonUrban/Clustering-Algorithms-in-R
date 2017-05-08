
rm(list = ls())
setwd("C:/Users/Gerson/Documents/Mestrado/Algoritmos/DataSets")
db2 = read.csv("dbn.csv")
#obtendo o conjunto de dados de treinamento
db1 = read.csv("dadosGerados_FCM2.csv")
fac = as.factor(db1[,3])
plot(db1)
plot(db1[,1:2])
col.list <- c("seagreen","blue","green","black")
palette(col.list)
plot(db1[,1:2], pch = c(15, 16, 17)[fac], col=fac)

setwd("C:/Users/Gerson/Documents/GitHub/Clustering-Algorithms-in-R")

#---------TESTES-----------
C = 3
V = matrix(nrow = C, ncol = 2,sample(1:120,C*2))
source("PCM_function_m.R")
pcm_result = pcm_function(db1[,1:2],V,distance = "euclidian", th = 0.01, iter.max = 200)
w = c(139,221,179,176,282,283)
pcm_result$membershipp[w,]
# source("FCM_function.R")
# pcm_result = fcm_function(db1[,1:2],V,distance = "euclidian", th = 0.01, iter.max = 200)
# 
# pcm_result = kmeans(db1[,1:2],4)

  library(e1071)
  fcm_result = cmeans(db1[,1:2],3)
  distance = "euclidean"
 U = update.partition_matrix.pcm(db1[,1:2],fcm_result$centers,distance)
 # U2 = update.partition_matrix.pcm(db1[,1:2],pcm_result$centers,distance)
 w = c(139,221,179,176,282,283)
 U[w,]
 # 
 # 
 # pcm_result = cmeans(db1[,1:2],10)
 # pcm_result = cmeans(db2,30)
 # family = as.factor(pcm_result$cluster)
 # tam = length(table(family))
 # palette(rainbow(30))
 # plot(db2, col=family)
 # U3 = update.partition_matrix.pcm(db1[,1:2],pcm_result$centers,distance)
 # pcm_result = cmeans(db1[,1:2],4)
#source("FCM_function.R")
#pcm_result = fcm_function(db1[,1:2],V,distance = "euclidian", th = 0.001, iter.max = 200)

#source("FCM.R")
#pcm_result = res
#Plot apenas com os valores classificados

# family = as.factor(pcm_result$cluster)
# tam = length(table(family))
# palette(col.list)
# plot(db1[,1:2], pch = c(15,16,17,18)[family], col=family)

