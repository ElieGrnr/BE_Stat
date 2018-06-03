N <- 1000
K <- 1000

V<-matrix(runif(N*K, 226, 234), ncol=K, nrow=N)
V1<-apply(V,1,mean) 

f<-matrix((19.05-18.7)*rbeta(N*K, 7, 2)+18.7, ncol=K, nrow=N)
f1<-apply(f,1,mean) 

SFC<-matrix(rexp(N*K, 3.45) + 17.23, ncol=K, nrow=N)
SFC1<-apply(SFC,1,mean) 

pdf("TCLv2_V.pdf")
hist(V1,main="Illustration du TCL avec V", xlab="moyenne de 1000 1000-échantillons de V", ylab="fréquences/densité", prob=TRUE, breaks=30, col=grey(0.9), border=grey(0.2))
curve(dnorm(x,230,sqrt(5.33/N)),add=T, col=3)
legend("topright", "distribution normale \n théorique", col="green", lty=1)
dev.off()

pdf("TCLv2_F.pdf")
hist(f1,main="Illustration du TCL avec F", xlab="moyenne de 1000 1000-échantillons de F", ylab="fréquences/densité",prob=TRUE, breaks=30, col=grey(0.9), border=grey(0.2))
curve(dnorm(x,18.9722,sqrt(2.11 * 10^(-3)/N)),add=T, col=3)
legend("topright", "distribution normale \n théorique", col="green", lty=1)
dev.off()

pdf("TCLv2_SFC.pdf")
hist(SFC1,main="Illustration du TCL avec SFC", xlab="moyenne de 1000 1000-échantillons de SFC", ylab="fréquences/densité",prob=TRUE, breaks=30, col=grey(0.9), border=grey(0.2))
curve(dnorm(x,17.52,sqrt(0.084/N)),add=T, col=3)
legend("topright", "distribution normale \n théorique", col="green", lty=1)
dev.off()
