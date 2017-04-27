# Clustering-Algorithms-in-R
-FCM Function
In -> A DataSet as data.frame or matrix,
      The Centroids as data.frame or matrix,
      The Distance as string, containing "euclidian" or "mahalanobis" (optional, "mahalanobis" as defaut)
      A threshold limiar to stop the clustering, (optional, 0.01 as defaut)
      and a iteration max number (optional, using threshold as defaut)
      
Out -> The centroid matrix,
       The number of elements in each centroid (Considering the greater membership value of Ui)
       A matix containing the membership U
       The iteration number of clustering
