
update.prototypes.fcm <- function(w,X){
  #A equa??o foi separada em duas etapas C = A/B
  
  #Inicializando valores
  N = nrow(X)
  k = ncol(w)
  dim = ncol(X)
  V = matrix(ncol = dim, nrow = k)
  for(i in 1:k){
    for(j in 1:dim){
      sum1 = 0
      sum2 = 0
      for(l in 1:N){
        sum1 = sum1 + (w[l,i] * w[l,i]) * X[l,j]
        sum2 = sum2 + (w[l,i] * w[l,i])
      }
      V[i,j] = (sum1/sum2)
    }
  }
  
  return(V)
}

#Fun??o que retorna a matriz de pertinencia utilizando distancia de mahanalobis
update.partition_matrix.fcm <- function(X, V){
  #Encontrando valores principais
  k = nrow(V)
  dim = ncol(X)
  N = nrow(X)
  
  #Renomeando as colunas do DB
  colnames(X) = c(1:dim)
  
  #Criando matriz de coeficiente para cada entrada em relacao a cada cluster
  w = matrix(ncol = k, nrow = N, 0)
  
  #Encontrando os valores de pesos para cada cluster
  for(i in 1:N){
    for(j in 1:k){
      sum = 0
      for(l in 1:k){
        #Calculando Distancia Euclidiana
        distE1 = 0
        distE2 = 0
        for(d in 1:dim){
          distE1 = distE1 + ((X[i,d] - V[j,d])*(X[i,d] - V[j,d]))
          distE2 = distE2 + ((X[i,d] - V[l,d])*(X[i,d] - V[l,d]))
        }
        sum = sum + ((distE1/distE2) * (distE1/distE2))
      }
      w[i,j] = 1/sum
    }
  }
  
  return(w)
}