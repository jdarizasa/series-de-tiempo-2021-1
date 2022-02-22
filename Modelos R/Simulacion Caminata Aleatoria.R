#CAMINATA ALEATORIA####
#pendulo
t = 100
A = rnorm(1,0,1)
B = rnorm(1,0,1)
theta = pi/2
X = NULL
for (i in 1:t) {
  X[i] = A*cos(theta*i) + B*sin(theta*i)
}
plot(seq(1,t,1),X,type = "l")

#caminata
S = rep(0,100)
for (i in 2:100) {
  S[i] = rnorm(1,0,1) + S[i-1]
}
plot(seq(1,100,1),S, type = "l")


#SIMULACION MA(1)####
#theta = 3
library(xts)
Z1 <- rnorm(100) 
Xt_1 <- ts(Z1 + 3*Z1[2:100], start = 1, end = 100)
Xt_1
plot.ts(Xt_1)

Z2 <- rnorm(300)
Xt_2 <- ts(Z2 + 3*Z2[2:300], start = 1, end = 300)
plot.ts(Xt_2)

Z3 <- rnorm(500)
xt_3 <- ts(Z3 + 3*Z3[2:500], start = 1, end=500)
plot.ts(xt_3)

windows()
par(mfrow=c(3,1))
acf(Xt_1)
acf(Xt_2)
acf(xt_3)

#SIMULACIÓN MA(2)####
#theta_1 = 3
#theta_2 = 5
library(xts)
Z1 <- rnorm(100) 
Xt_1 <- ts(Z1 + 3*Z1[2:100] + 5*Z1[3:100], start = 1, end = 100)
Xt_1
plot.ts(Xt_1)

Z2 <- rnorm(300)
Xt_2 <- ts(Z2 + 3*Z2[2:300] + 5*Z2[3:300], start = 1, end = 300)
plot.ts(Xt_2)

Z3 <- rnorm(500)
xt_3 <- ts(Z3 + 3*Z3[2:500] + 5*Z3[3:500], start = 1, end=500)
plot.ts(xt_3)

windows()
par(mfrow=c(3,1))
acf(Xt_1)
acf(Xt_2)
acf(xt_3)
