library("glasso")
data1<-read.table("data.txt")
cm = cov(data1, y = NULL, use = "all.obs", method = "pearson")
g.fit = glasso(cm, 3)
#number of edges
print((sum(g.fit$w != 0)-20)/2)