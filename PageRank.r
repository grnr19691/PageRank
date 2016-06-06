library("igraph")
library("Matrix")

getAdjMatrix<-function(fname)
{
  Graph<-graph.data.frame(d=read.table(fname),directed = TRUE)
  cat("The number of Vertices are",vcount(Graph))
  cat("\nThe number of edges are",ecount(Graph))
  AdjacencyMatrix<-get.adjacency(Graph,sparse = TRUE)
  return(AdjacencyMatrix)
}

getTransitionMatrix<-function(A)
{
  A<-t(A)
  SummaryMatrix<- within(summary(A), x <- 1/(colSums(A)[j]))
  T<-sparseMatrix(i=SummaryMatrix$i,j=SummaryMatrix$j,dims=c(A@Dim[1],A@Dim[2]),x=(A@x)*(SummaryMatrix$x))
  z<-colSums(T)
  z[z > 0]<-(0.15/ncol(A))
  z[z==0]<-(1/ncol(A))
  Result<-list(T=T,z=z)
  return(Result)
}
myPageRank<-function(T,z,niter){
  e=matrix(1,nrow=nrow(T),ncol=1)
  xold = matrix(1/nrow(T),nrow=nrow(T),ncol=1)
  for(i in 1:niter)
  {
    xnew = (0.85*T) %*% xold + e %*% (z %*% xold)
    xold = xnew;
  }
  return(xnew)
}


p<-proc.time()
AdjacentMatrix<-getAdjMatrix("soc-Epinions1.txt.gz")
TransistionMatrixList<-getTransitionMatrix(AdjacentMatrix)
PageRank<-myPageRank(TransistionMatrixList$T,TransistionMatrixList$z,100)
print(proc.time()-p)
print(colSums(PageRank))
