source("echantillon.R")

N <- 500
K <- 1000
sigma <- 0.01
p <- 4

alpha_MC <- cbind(rep(0,4))

for (k in 1:K) {
    V <- sample_V(N, sigma)
    f <- sample_F(N, sigma)
    SFC <- sample_SFC(N, sigma)
    Mfuel <- (42600+46200+19900)*(exp((10^(-3)*SFC*9.8*3000)/(V*f))-1)

    y <- cbind(Mfuel)
    X <- matrix(rep(0, N*p), N, p)

    X[,1] = rep(1, N)
    X[,2] = V
    X[,3] = f
    X[,4] = SFC

    alpha_MC = alpha_MC + solve(aperm(X)%*%X)%*%aperm(X)%*%y
  
    }

  
alpha_MC = alpha_MC/K

a0 <- alpha_MC[1,1]
a1 <- alpha_MC[2,1]
a2 <- alpha_MC[3,1]
a3 <- alpha_MC[4,1]

s_carre <- 0
SSE <- 0
SST <- 0
SSR <- 0
for (k in 1:K) {
    V <- sample_V(N, sigma)
    f <- sample_F(N, sigma)
    SFC <- sample_SFC(N, sigma)
    Mfuel <- (42600+46200+19900)*(exp((10^(-3)*SFC*9.8*3000)/(V*f))-1)
    
    y <- cbind((Mfuel))
    y_bar <- mean(Mfuel)
    Mfuel_est <- a0 + a1*V + a2*f + a3*SFC 
    y_est <- cbind((Mfuel_est))
    
    e <- y - y_est
    
    s_carre <- s_carre + (aperm(e)%*%e)/(N-p-1)
    SSE <- SSE + aperm(e)%*%e
    SST <- SST + aperm(y-cbind(rep(y_bar, N)))%*%(y-cbind(rep(y_bar, N)))
    SSR <- SSR + aperm(y_est-cbind(rep(y_bar, N)))%*%(y_est-cbind(rep(y_bar, N)))
    }
    
s_carre = s_carre/K
SSE = SSE/K
SST = SST/K
SSR = SSR/K
R_carre <- SSR/SST

SSE
SST
SSR
R_carre
reg_lin<-c(as.numeric(a0),as.numeric(a1),as.numeric(a2),as.numeric(a3),as.numeric(s_carre),as.numeric(R_carre))
names(reg_lin) <-c("a0","a1","a2","a3","s^2","R^2")
reg_lin  



####
V <- sample_V(N, sigma)
f <- sample_F(N, sigma)
SFC <- sample_SFC(N, sigma)
Mfuel <- (42600+46200+19900)*(exp((10^(-3)*SFC*9.8*3000)/(V*f))-1)

y <- cbind(Mfuel)
X <- matrix(rep(0, N*p), N, p)

X[,1] = rep(1, N)
X[,2] = V
X[,3] = f
X[,4] = SFC

alpha_MC = solve(aperm(X)%*%X)%*%aperm(X)%*%y
a0 <- alpha_MC[1,1]
a1 <- alpha_MC[2,1]
a2 <- alpha_MC[3,1]
a3 <- alpha_MC[4,1]

y_bar <- mean(Mfuel)
Mfuel_est <- a0 + a1*V + a2*f + a3*SFC 
y_est <- cbind((Mfuel_est))
    
e <- y - y_est
    
s_carre <- (aperm(e)%*%e)/(N-p-1)

alpha_conf <- 0.05
conf0 <- qt(1-alpha_conf/2, N-4)*sqrt(s_carre*solve(aperm(X)%*%X)[1,1])
conf1 <- qt(1-alpha_conf/2, N-4)*sqrt(s_carre*solve(aperm(X)%*%X)[2,2])   
conf2 <- qt(1-alpha_conf/2, N-4)*sqrt(s_carre*solve(aperm(X)%*%X)[3,3])    
conf3 <- qt(1-alpha_conf/2, N-4)*sqrt(s_carre*solve(aperm(X)%*%X)[4,4])    
    
reg_lin_conf<-c(as.numeric(a0),as.numeric(a1),as.numeric(a2),as.numeric(a3),as.numeric(s_carre))
names(reg_lin_conf) <-c("a0","a1","a2","a3","s^2")
reg_lin_conf   
conf<-c(as.numeric(conf0),as.numeric(conf1),as.numeric(conf2),as.numeric(conf3))
names(conf) <-c("conf0","conf1","conf2","conf3")
conf    
    
    
    
    
    
    
    
    
    
