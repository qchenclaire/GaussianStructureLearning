#follow examples here: https://www.rdocumentation.org/packages/pcalg/versions/2.6-0/topics/pc
library("pcalg")
data1<-read.table("data.txt")
V <- colnames(data1)
n <- nrow(data1)
pc.fit <- pc(suffStat = list(C = cor(data1), n = n),
             indepTest = gaussCItest, ## indep.test: partial correlations
             alpha=0.9, labels = V, verbose = TRUE)

par(mfrow=c(1,2))
plot(pc.fit, main = "Estimated CPDAG")
print(pc.fit)

