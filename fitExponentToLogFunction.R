x <- 0:100
y <- log(x+1) # exponent like function i want to fit to exponential.  replace with your data.
plot(y)

df <- data.frame(x,y)

model <- nls(y ~ a*(1-exp(-k * x)), data = df,
             start = list(k = 2, a = 4))

model

eres <- coef(model)[2]*(1-exp(-coef(model)[1]*x))

plot(y)
lines(eres)