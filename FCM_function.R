fcm_function <- function(db, V, distance = "mahalanobis", th = 0.01, iter.max = 200){
  #dimensão da base de dados e seu tamanho
  dim = ncol(db)
  N = nrow(db)  
  
  #Renomeando as colunas
  colnames(db) = c(1:dim)
  
  nclasses = nrow(V)
  
  #Criando uma matrix de pertinencia vazia
  U = matrix(ncol = nclasses, nrow = N, 0)
  
  #Inicializando a matriz de pertinencia U
  U = update.partition_matrix.fcm(db,V,distance)
  
  erro = 1 + th
  t = 0
  #Inicializando o algoritmo
  while(erro > th){
    #for(t in 1:iter.max){
    t = t + 1
    V = update.prototypes.fcm(U,db)
    
    U2 = update.partition_matrix.fcm(db,V,distance)
    
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
  
  return(res)
}

update.prototypes.fcm <- function(U,db){
  #A equação foi separada em duas etapas C = A/B
  
  #Inicializando valores
  N = nrow(db)
  k = ncol(U)
  dim = ncol(db)
  
  #Primeira parte
  A = matrix(ncol = dim, nrow = k)
  for(i in 1:k){
    for(j in 1:dim){
      A[i,j] = sum((U[,i]^2) * db[,j])
    }
  }
  
  #Segunda Parte
  B = vector(length = k)
  for(i in 1:k){
    B[i] = sum(U[,i]^2)
  }
  C = A/B
  return(C)
}

#Função que retorna a matriz de pertinencia utilizando distancia de mahanalobis
update.partition_matrix.fcm <- function(db, V, distance){
  #Encontrando valores principais
  k = nrow(V)
  dim = ncol(db)
  N = nrow(db)
  
  #Renomeando as colunas do DB
  colnames(db) = c(1:dim)
  
  #Third Part
  if(distance == "mahalanobis"){
    maha = dist.mahalanobis.fcm(db,V)
  }
  else if(distance == "euclidian"){
    maha = dist.euclidian.fcm(db,V)
  }
  else{
    maha = dist.mahalanobis.fcm(db,V)
  }
  C = matrix(nrow = N, ncol = k)
  
  for(i in 1:N){
    for(j in 1:k){
      C[i,j] = sum(maha[i,j]/maha[i,])
    }
  }
  
  #Finalizing equation 
  U = 1/C
  return(U)
}


#Função que retorna a distancia de mahanaloobis de todos os pontos para todos os clusters
dist.mahalanobis.fcm <- function(db, V){
  average = colMeans(V)
  variance = var(V)
  N = nrow(db)
  k = nrow(V)
  dim = ncol(db)
  
  d2_mahalanobis = matrix(nrow = N, ncol = k, 0)
  
  for(i in 1:N){
    for(j in 1:k){
      d2_mahalanobis[i,j] = mahalanobis(db[i,1:dim], V[j,], variance)
    }
  }
  return(d2_mahalanobis)
}

#Função que retorna a distancia euclidiana de todos os pontos para todos os clusters
#Feito na mão, depois arrumo
dist.euclidian.fcm <- function(db, V){
  N = nrow(db)
  k = nrow(V)
  dim = ncol(db)
  colnames(V) = colnames(db[,1:dim])
  distancia = rbind(db[,1:dim],V)
  distancia = as.matrix(dist(distancia))
  lista = NULL
  for(i in 1:k){
    lista[[i]] = distancia[N+i,]
  }
  d2_euclidian = matrix(nrow = N, ncol = k)
  for(i in 1:N){
    for(j in 1:k){
      d2_euclidian[i,j] = lista[[j]][[i]]
    }
  }
  return(d2_euclidian)
}

inicializeV.fcm <- function(dim,nclasses,db){
  ##Encontrando os chutes iniciais do centroide 
  #Pois eu mesclei cluster com classe, e ele estava procurando a mais perto, 
  #portanto resolvi fazer a media dos pontos classificados como chute inicial do centroides (ARRUMAR DEPOIS)
  V = matrix(ncol = dim, nrow = nclasses, sample(0:130, size=nclasses*dim, replace=TRUE))
  
  #Fazendo na mão, depois arrumo
  lista = NULL
  for(i in 1:nclasses){
    t = db[,dim+1] == i
    lista[[i]] = db[t,1:dim]
  }
  V = matrix(ncol = dim, nrow = nclasses)
  for(i in 1:nclasses){
    V[i,] = colMeans(lista[[i]])
  }
  return(V)
}
