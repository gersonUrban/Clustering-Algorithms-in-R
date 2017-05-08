pcm_function <- function(db, V, distance = "euclidian", th = 0.01, iter.max = 200){
  a <<- 0.05
  b <<- 0.95
  
  #dimens?o da base de dados e seu tamanho
  dim = ncol(db)
  N = nrow(db)  
  
  #Renomeando as colunas
  colnames(db) = c(1:dim)
  
  nclasses = nrow(V)
  
  #Criando uma matrix de pertinencia vazia
  U1 = matrix(ncol = nclasses, nrow = N, 0)
  
  #Criando uma matrix de pertinencia vazia
  U2 = matrix(ncol = nclasses, nrow = N, 0)
  
  #Inicializando a matriz de pertinencia U
  U1 = update.partition_matrix.fcm(db,V)
  
  #Inicializando a matriz de pertinencia U
  U2 = update.partition_matrix.pcm(db,V,distance)
  
  pdf("teste28.pdf")
  erro = 1 + th
  t = 0
  #Inicializando o algoritmo
  while((erro > th)&&(t<iter.max)){
    #for(t in 1:iter.max){
    t = t + 1
    V = update.prototypes.pcm(U1,U2,db)
    
    U1_2 = update.partition_matrix.fcm(db,V)
    U2_2 = update.partition_matrix.pcm(db,V,distance)
    #Verificando a condicao de parada(diferenca entre pesos menor que threshold)
    erro = 0
    for(i in 1:N){
      sum = 0
      for(j in 1:nclasses){
        sum = sum + abs(U2_2[i,j] - U2[i,j])
      }
      erro = erro + sum
    }
    #cat("\n",U)
    #cat("\n",U2)
    U1 = U1_2
    U2 = U2_2
    cat("\n iteracao: ",t)
    cat("\n Diferenca: ",erro)
    plot_func(db,U2,V)
    plot_func(db,U1,V)
  }
  dev.off()
  
  #Encontrando a classificacao
  classificados = db
  classificados[,dim+1] = 0
  size = vector(length = nclasses)
  
  #Fiz na "mao" arrumar depois
  for(i in 1:N){
    for(j in 1:nclasses){
      if(U1[i,j] == max(U1[i,])){
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
  res$membershipc = U1
  res$membershipp = U2
  res$iter = t
  res$withinerror = "TODO"
  res$call = "TODO"
  
  return(res)
}



update.prototypes.pcm <- function(U,U2,db){
  #A equa??o foi separada em duas etapas C = A/B
  
  #Inicializando valores
  N = nrow(db)
  k = ncol(U)
  dim = ncol(db)
  
  # U3 = matrix(ncol = k, nrow = N)
  # 
  # for(i in 1:N){
  #   for(j in 1:k){
  #     U3[i,j] = U[i,j]/sum(U[i,])
  #   }
  # }
  # U2 = U3
  # 
  #Primeira parte
  A = matrix(ncol = dim, nrow = k)
  for(i in 1:k){
    for(j in 1:dim){
      A[i,j] = ((sum(((U[,i])^2 * db[,j])*a)))+((sum(((U2[,i])^2 * db[,j])*b)))
    }
  }
  
  #Segunda Parte
  B = vector(length = k)
  for(i in 1:k){
    B[i] = sum((U[,i]^2)*a)+sum((U2[,i]^2)*b)
  }
  C = A/B
  return(C)
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


#Fun??o que retorna a matriz de pertinencia utilizando distancia de mahanalobis
update.partition_matrix.pcm <- function(db, V, distance){
  #Encontrando valores principais
  k = nrow(V)
  dim = ncol(db)
  N = nrow(db)
  
  #Renomeando as colunas do DB
  colnames(db) = c(1:dim)
  
  # #Third Part
  # if(distance == "mahalanobis"){
  #   maha = dist.mahalanobis.fcm(db,V)
  # }
  # else if(distance == "euclidian"){
  #   maha = dist.euclidian.fcm(db,V)
  # }
  # else{
  #   maha = dist.mahalanobis.fcm(db,V)
  # }
  # C = matrix(nrow = N, ncol = k)
  # 
  # minC = NULL
  # for(i in 1:k){
  #   minC = c(minC,min(maha[,i]))
  # }
  # for(i in 1:N){
  #   for(j in 1:k){
  #     C[i,j] = (minC[j]+1)/(maha[i,j]+1)
  #   }
  # }
  # 
  # # for(i in 1:N){
  # #   minC = rbind(minC,min(maha[i,]))
  # # }
  # # for(i in 1:N){
  # #   for(j in 1:k){
  # #     C[i,j] = (minC[i]+1)/(maha[i,j]+1)
  # #   }
  # # }
  #Criando matriz de coeficiente para cada entrada em relacao a cada cluster
  maha = distanciaNova(db,V)
  C = matrix(nrow = N, ncol = k)

  minC = NULL
  for(i in 1:k){
    minC = c(minC,sqrt(min(maha[,i])))
  }
  for(i in 1:N){
    for(j in 1:k){
      C[i,j] = (minC[j]+1)/sqrt((maha[i,j]+1))
      #C[i,j] = sqrt((minC[j]+1)/sqrt((maha[i,j]+1)))
    }
  }

  #Finalizing equation 
  U = C
  return(U)
}

distanciaNova<-function(X,V){
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
        #Calculando Distancia Euclidiana
        distE1 = 0
        for(d in 1:dim){
          distE1 = distE1 + ((X[i,d] - V[j,d])*(X[i,d] - V[j,d]))
        }
        w[i,j] = distE1
    }
  }
  return(w)
}

#Fun??o que retorna a distancia de mahanaloobis de todos os pontos para todos os clusters
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

#Fun??o que retorna a distancia euclidiana de todos os pontos para todos os clusters
#Feito na m?o, depois arrumo
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
  
  #Fazendo na m?o, depois arrumo
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

plot_func <- function(db,U,V){
 N = nrow(db)
 nclasses = ncol(U)
 k = nclasses
 dim = ncol(db)
  
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
  
  colnames(classificados) = c(1:(dim+1))
  V = cbind(V,k+1)
  colnames(V) = colnames(classificados)
  classificados = rbind(classificados,V)
  #Plot apenas com os valores classificados
  family = as.factor(classificados[,3])
  col.list <- c("red","blue","green","yellow","black")
  tam = length(table(family))
  palette(col.list)
  plot(classificados[,1:dim], pch = c(15,16,17,18,3)[family], col=family)
}