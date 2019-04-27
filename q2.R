library(mice)
library("glasso")
data<-read.table("data_missing.txt")
#check missing data
print(sapply(data,function(x) sum(is.na(x))))
init = mice(data, maxit=0) 
meth = init$method
predM = init$predictorMatrix
n = ncol(data)

glasso_matrix = matrix(0, nrow=n, ncol=n)
pc_matrix = matrix(0, nrow=n, ncol=n)
for(i in 1:10)
{
  set.seed(i)
  imputed = mice(data, method=meth, predictorMatrix=predM, m=5)
  imputed <- complete(imputed)
  #check missings in imputed dataset
  print(sapply(imputed, function(x) sum(is.na(x))))
  
  #glasso
  cm = cov(imputed, y = NULL, use = "all.obs", method = "pearson")
  g.fit = glasso(cm, 2)
  glasso_matrix <- (glasso_matrix + (g.fit$w !=0))
  
  #pc
  pc.fit <- pc(suffStat = list(C = cor(imputed), n = n),
               indepTest = gaussCItest, ## indep.test: partial correlations
               alpha=0.9, labels = V, verbose = TRUE)
  pc_matrix <- (pc_matrix + as(pc.fit@graph, "Matrix"))
  #pc_matrix <- pc_matrix & (trueCov(pc.fit@graph) !=0)
}
for(i in 1:n)
  glasso_matrix[i,i] <- 0
glasso_matrix = glasso_matrix >= 5
V <- format(1:n)
edL1 <- vector("list", length=n)
names(edL1) <- V
for(i in 1:n)
  edL1[[i]] <- list(edges=which(glasso_matrix[i,]!=0))
glasso_graph = graphNEL(nodes=V , edgeL=edL1, edgemode = "undirected")

pc_matrix = pc_matrix >= 5
edL2 <- vector("list", length=n)
names(edL2) <- V
for(i in 1:n)
  edL2[[i]] <- list(edges=which(pc_matrix[i,]!=0))
pc_graph = graphNEL(nodes=V , edgeL=edL2, edgemode = "directed")

par(mfrow=c(1,2))
plot(glasso_graph)
plot(pc_graph)