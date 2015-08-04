# Sensitivity Analysis sample test

factors <- c("r", "K", "X0")
q <- c("qnorm", "qnorm", "qunif")
q.arg <- list( list(mean=1.7, sd=0.15), list(mean=40, sd=1),list(min=1, max=50) )

oneRun <- function (r, K, Xo) {
    X <- Xo
    for (i in 0:20) {
        X <- X+r*X*(1-X/K)
    }
    return (X)
}
modelRun <- function (my.data) {
    return(mapply(oneRun, my.data[,1], my.data[,2], my.data[,3]))
}
library(pse)
myLHS <- LHS(modelRun, factors, 200, q, q.arg, nboot=50)
plotecdf(myLHS)
plotscatter(myLHS)
plotprcc(myLHS)
p <- pic(myLHS)
print(p[[1]]$pic)