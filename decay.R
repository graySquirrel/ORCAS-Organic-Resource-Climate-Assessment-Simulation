library(permute)
library(ggplot2)
library(reshape2)
set.seed(42)


XMAX <- 100
x <- 0:XMAX
fitdf <- function(blo, bhi, klo, khi, samps) {
    set.seed(42)
    k1s <- runif(samps, klo,khi)
    b1s <- runif(samps, blo,bhi)
    k2s <- runif(samps, klo,khi)
    b2s <- runif(samps, blo,bhi)
    arow<-NULL
    for (i in 1:samps){
        k1<-k1s[[i]]; k2<-k2s[[i]];b1<-b1s[[i]];b2<-b2s[[i]]
        e1 <- b1*(1-exp(-k1*x))
        e2 <- b2*(1-exp(-k2*x))
        e3 <- e1 + e2
        df <- data.frame(x,e3)
        mod <- nls(e3 ~ b*(1-exp(-k * x)), data = df, start = list(k = 1, b = b1+b2))
        arow <- rbind(arow,matrix(c(XMAX,k1,k2,b1,b2,
                                    e1[[XMAX]]/b1,e2[[XMAX]]/b2,
                                    coef(mod)[1],coef(mod)[2]),
                                  nrow = 1))
    }
    arow
}
df <- fitdf(.025,.1,.25,1,1)
colnames(df)<- c("Samples","K1","K2","B1","B2","max%ofB1","max%ofB2","fitK","fitB")

print(df)



mksamps <- function(blo, bhi, klo, khi, samps) {
    #samps = 10
    bs<-seq(blo,bhi,length.out=samps)
    ks<-seq(klo,khi,length.out=samps)
    # just shuffle the parameter lists independently
    ks <- ks[shuffle(samps)]
    bs <- bs[shuffle(samps)]
    thenms <- c("x",paste0("k",format(ks,digits=1),"b",format(bs,digits=2)))
    x <- 0:100
    # now combine ks and bs into pairs of parameters
    df <- data.frame(x)
    for (i in 1:samps){
        tmp <- bs[[i]]*(1-exp(-ks[[i]]*x))
        df <- cbind(df,tmp)
    }
    colnames(df) <- thenms
    df
}
# plot on same grid, each series colored differently -- 
# good if the series have same scale
df <- mksamps(1,5,0.02,0.1,10)
dfplot <- melt(df ,  id = 'x', variable_name = 'series')
ggplot(dfplot, aes(x,value)) + geom_line(aes(colour = variable))







bs<-seq(.1,5,length.out=100)
x <- 0:100
t1 <- .05
t2 <- .1
t3 <- (t1+t2)/2
e1 <- exp(-t1*x)
e2 <- exp(-t2*x)
e3 <- exp(-t3*x)
df <- data.frame(x,e1,e2,e3)

require(ggplot2)
ggplot(df, aes(x)) +                    
    geom_line(aes(y=e1), colour="red") +  
    geom_line(aes(y=e2), colour="green") +
    geom_line(aes(y=e3), colour="blue")
    
k1=.025;k2=.1;b1=1;b2=.5
x <- 0:100
k3 <- (k1+k2)/2
e1 <- b1*(1-exp(-k1*x))
e2 <- b2*(1-exp(-k2*x))
#e3 <- (b1+b2)*(1-exp(-k3*x))
e3 <- e1 + e2

df <- data.frame(x,e1,e2,e3)
mod <- nls(e3 ~ b*(1-exp(-a * x)), data = df, start = list(a = 1, b = 1))

earr <- exp(-t*x)

eq = function(x){x*x}
plot(eq, 1, 1000)
curve(eq, 1, 1000)
library(permute)
