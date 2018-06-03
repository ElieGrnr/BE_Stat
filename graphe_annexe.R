source("echantillon.R")
N <- 1000

V<-sample_V(N,0)
f<-sample_F(N,0)
SFC<-sample_SFC(N,0)


pdf("V_fctrep.pdf")
plot(sort(V),1:N/N, type="s", xlab="valeur de V (m/s)", ylab="", main="Fonction de répartition de V", col="red", lwd=2)
curve(punif(x,226,234), add=T, lty=3, lwd=4)
legend("topleft", legend=c("fonction de répartition empirique","fonction de répartition théorique"), col=c("red", "black"), lty=c(1,3), lwd=c(2,4))
dev.off()

pdf("F_fctrep.pdf")
plot(sort(f),1:N/N, type="s", xlab="valeur de F", ylab="", main="Fonction de répartition de F", col="red", lwd=2)
curve(pbeta((x-18.7)/(19.05-18.7),7,2), add=T, lty=3, lwd=4)
legend("topleft", legend=c("fonction de répartition empirique","fonction de répartition théorique"), col=c("red", "black"), lty=c(1,3), lwd=c(2,4))
dev.off()

pdf("SFC_fctrep.pdf")
plot(sort(SFC),1:N/N, type="s", xlab="valeur de SFC ( g/(kN*s) )", ylab="", main="Fonction de répartition de SFC", col="red", lwd=2)
curve(pexp(x-17.23,3.45), add=T, lty=3, lwd=4)
legend("bottomright", legend=c("fonction de répartition empirique","fonction de répartition théorique"), col=c("red", "black"), lty=c(1,3), lwd=c(2,4))
dev.off()

M <- sample_Mfuel(N, 0)
M1 <- sample_Mfuel(N, 0.01)
M2 <- sample_Mfuel(N, 0.05)
M3 <- sample_Mfuel(N, 0.1)
M4 <- sample_Mfuel(N, 0.5)
pdf("boxplot.pdf")
boxplot(M,M1,M2,M3,M4,names=c("bruit=0","bruit=0.01","bruit=0.05","bruit=0.1", "bruit=0.5"),outline = FALSE, col=c("blue", "purple","green","orange","red"), ylab="valeurs de Mfuel (kg)", main="Répartition de Mfuel en fonction de certains niveaux de bruit")
dev.off()

Y <- matrix(c(sort(M), sort(M1),sort(M2),sort(M3),sort(M4)), nrow=N, ncol=5)
pdf("M_fctrep_bruit.pdf")
n=length(M)
matplot(Y, 1:n/n, type="s", xlab="valeur de Mfuel (kg)", ylab="", main="Fonction de répartition de Mfuel", lwd=1, col=c("blue", "purple","green","orange","red"))
legend("bottomright",legend=c("bruit=0","bruit=0.01","bruit=0.05","bruit=0.1","bruit=0.5"), col=c("blue", "purple","green","orange","red"), lty=1)
dev.off()

pdf("M_fctrep.pdf")
n=length(M)
matplot(sort(M), 1:n/n, type="s", xlab="valeur de Mfuel (kg)", ylab="", main="Fonction de répartition de Mfuel (non bruitée)", lwd=1, col="blue")
dev.off()

