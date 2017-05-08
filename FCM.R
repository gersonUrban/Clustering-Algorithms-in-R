source("FCM_utils2.R")
db = db1[,1:2]
distance = "euclidian"
th = 0.01
iter.max = 200
#fcm_function <- function(db, V, distance = "mahalanobis", th = 0.01, iter.max = 200){
  #dimens?o da base de dados e seu tamanho
  
  dim = ncol(db)
  N = nrow(db)  
  
  #Renomeando as colunas
  colnames(db) = c(1:dim)
  
  nclasses = nrow(V)
  
  #Criando uma matrix de pertinencia vazia
  U = matrix(ncol = nclasses, nrow = N, 0)
  
  #Inicializando a matriz de pertinencia U
  U = update.partition_matrix.fcm(db,V)
  
  erro = 1 + th
  t = 0
  #Inicializando o algoritmo
  while(erro > th){
    #for(t in 1:iter.max){
    t = t + 1
    V = update.prototypes.fcm(U,db)
    
    U2 = update.partition_matrix.fcm(db,V)
    
    #Verificando a condicao de parada(diferenca entre pesos menor que threshold)
    erro = 0
    for(i in 1:N){
      sum = 0
      for(j in 1:nclasses){
        sum = sum + abs(U2[i,j] - U[i,j])
      }
      erro = erro + sum
    }
    
    U = U2
    cat("\n iteracao: ",t)
    cat("\n Diferenca: ",erro)
  }
  
  #Encontrando a classificacao
  classificados = db
  classificados[,dim+1] = 0
  size = vector(length = nclasses)
  
  #Fiz na "mao" arrumar depois
  for(i in 1:N){
    for(j in 1:nclasses){
      if(U[i,j] == max(U[i,])){
        classificados[i,(dim+1)] = j
        size[j] = size[j] + 1
      }
    }
  }
  
  #Preparando o retorno
  res = NULL
  res$centers = V
  res$size = size
  res$cluster = classificados[,dim+1]
  res$membership = U
  res$iter = t
  res$withinerror = "TODO"
  res$call = "TODO"
  
  #return(res)
#}