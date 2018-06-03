source("echantillon.R")

N <- 1000
sigma <- 0

t <- 1:N
Vbar <- vector("numeric", N)
Fbar <- vector("numeric", N)
SFCbar <- vector("numeric", N)
for(i in t) {
    V <- sample_V(i, sigma)
    f <- sample_F(i, sigma)
    SFC <- sample_SFC(i, sigma)
    Vbar[i] <- sum(V)/i
    Fbar[i] <- sum(f)/i
    SFCbar[i] <- sum(SFC)/i
    }

png("LFGN_SFC.png")
#plot(t, Vbar, xlab="taille de l'échantillon", ylab="moyenne empirique de V",pch=3, cex=0.5)
#legend(600, 231, "moyenne théorique", col="red", lty=1)
#abline(h=230,col="red")
#plot(t, Fbar, xlab="taille de l'échantillon", ylab="moyenne empirique de F",pch=3, cex=0.5)
#legend(600, 19, "moyenne théorique", col="red", lty=1)
#abline(h=683/36,col="red")
plot(t, SFCbar, xlab="taille de l'échantillon", ylab="moyenne empirique de SFC",pch=3, cex=0.5)
legend(600, 17.6, "moyenne théorique", col="red", lty=1)
abline(h=17.52,col="red")

dev.off()
    
