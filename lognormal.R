library(sn)

xi=3
omega=1.5
alpha=-4
tau=0
x = seq(-5, 6, length.out = 500)
Z = dsn(x,xi=xi,omega=omega,alpha=alpha,tau=tau)
plot(x, Z, type = "l")
