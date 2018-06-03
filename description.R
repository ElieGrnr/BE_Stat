source("echantillon.R")

N <- 1000
sigma <- 0

V <- sample_V(N, sigma)
f <- sample_F(N, sigma)
SFC <- sample_SFC(N, sigma)
Mfuel <- sample_Mfuel(N, sigma)

descr_V <- c(mean(V), mean(V*V)-mean(V)^2, sqrt(mean(V*V)-mean(V)^2), min(V), max(V))
names(descr_V) <- c("moyenne_V", "variance_V", "ecart-type_V", "min_V", "max_V")
descr_F <- c(mean(f), mean(f*f)-mean(f)^2, sqrt(mean(f*f)-mean(f)^2), min(f), max(f))
names(descr_F) <- c("moyenne_F", "variance_F", "ecart-type_F", "min_F", "max_F")
descr_SFC <- c(mean(SFC), mean(SFC*SFC)-mean(SFC)^2, sqrt(mean(SFC*SFC)-mean(SFC)^2), min(SFC), max(SFC))
names(descr_SFC) <- c("moyenne_SFC", "variance_SFC", "ecart-type_SFC", "min_SFC", "max_SFC")
descr_M <- c(mean(Mfuel), mean(Mfuel*Mfuel)-mean(Mfuel)^2, sqrt(mean(Mfuel*Mfuel)-mean(Mfuel)^2), min(Mfuel), max(Mfuel))
names(descr_M) <- c("moyenne_Mfuel", "variance_Mfuel", "ecart-type_Mfuel", "min_Mfuel", "max_Mfuel")
descr_V
descr_F
descr_SFC
descr_M

pdf("histo_V.pdf")
hist(V, breaks=20, col=grey(0.9), border=grey(0.2), main=paste("Echantillon de", N, "valeurs de V"), xlab="V", ylab="effectifs", tck=0.01)
lines(density(V, na.rm = TRUE), lwd=2)
#plot(function(x) N*dunif(x,226,234),col=3,add=TRUE, xlim=c(226,234))
dev.off()

pdf("histo_F")
hist(f, breaks=20, col=grey(0.9), border=grey(0.2), main=paste("Histogramme de", N, "valeurs de F"), xlab="F", ylab="effectifs", tck=0.01)
#curve((dbeta(x,7,2)*(19.05-18.7)+18.7)*N, add=T)
lines(density(f, na.rm = TRUE), lwd=2)
#boxplot(f, col = grey(0.8),main = paste("Valeurs de", N, "F"),ylab = "F", las = 1)
#rug(f, side = 2)
dev.off()

pdf("histo_SFC")
hist(SFC, breaks=20, col=grey(0.9), border=grey(0.2), main=paste("Echantillon de", N, "valeurs de SFC"), xlab="SFC", ylab="effectifs", tck=0.01)
lines(density(SFC, na.rm = TRUE), lwd=2)
#plot(function(x) dexp(x,3.45),col=3,add=TRUE, xlim=c(17.23, 19.5))
dev.off()

#pdf("histo_Mfuel.pdf")
#hist(Mfuel, breaks="FD", col=grey(0.9), border=grey(0.2), main=paste("Répartition d'un échantillon de", N, "valeurs de Mfuel"), xlab="Mfuel", ylab="fréquences", tck=0.01, freq=FALSE)
#lines(density(Mfuel, na.rm = TRUE), lwd=2)
#legend(14500, 0.001, "densité estimée", col="black", lty=1)
#dev.off()

#Mfuel <- sample_Mfuel(N, 0)
#Mfuel_2 <- sample_Mfuel(N, 0.05)
#Mfuel_3 <- sample_Mfuel(N, 0.08)
#Mfuel_4 <- sample_Mfuel(N, 0.1)
#Mfuel_5 <- sample_Mfuel(N, 0.2)
#Mfuel_6 <- sample_Mfuel(N, 0.3)
#Mfuel_7 <- sample_Mfuel(N, 0.5)
#pdf("histo_Mfuel_b.pdf")
#hist(Mfuel, breaks="FD", col=grey(1), border=grey(1), main=paste("Densité de Mfuel"), xlab="Mfuel", ylab="densité", tck=0.01, #freq=FALSE,xlim=c(12500,15000))
#hist(Mfuel_2, breaks="FD", col=grey(1), border=grey(1), main=paste("Répartition d'un échantillon de", N, "valeurs de Mfuel"), xlab="Mfuel", #ylab="fréquences", tck=0.01, freq=FALSE, add=TRUE)
#hist(Mfuel_3, breaks="FD", col=grey(1), border=grey(1), main=paste("Répartition d'un échantillon de", N, "valeurs de Mfuel"), xlab="Mfuel", ylab="fréquences", tck=0.01, freq=FALSE, add=TRUE)
#hist(Mfuel_4, breaks="FD", col=grey(1), border=grey(1), main=paste("Répartition d'un échantillon de", N, "valeurs de Mfuel"), xlab="Mfuel", ylab="fréquences", tck=0.01, freq=FALSE, add=TRUE)
#hist(Mfuel_5, breaks="FD", col=grey(1), border=grey(1), main=paste("Répartition d'un échantillon de", N, "valeurs de Mfuel"), xlab="Mfuel", ylab="fréquences", tck=0.01, freq=FALSE, add=TRUE)
#hist(Mfuel_6, breaks="FD", col=grey(1), border=grey(1), main=paste("Répartition d'un échantillon de", N, "valeurs de Mfuel"), xlab="Mfuel", ylab="fréquences", tck=0.01, freq=FALSE, add=TRUE)
#hist(Mfuel_7, breaks="FD", col=grey(1), border=grey(1), main=paste("Répartition d'un échantillon de", N, "valeurs de Mfuel"), xlab="Mfuel", ylab="fréquences", tck=0.01, freq=FALSE, add=TRUE)
#lines(density(Mfuel, na.rm = TRUE), lwd=1, col="blue",xlim=c(12500,15000))
#lines(density(Mfuel_2, na.rm = TRUE), lwd=1, col="red",xlim=c(12500,15000))
#lines(density(Mfuel_3, na.rm = TRUE), lwd=1, col="green",xlim=c(12500,15000))
#lines(density(Mfuel_4, na.rm = TRUE), lwd=1, col="grey",xlim=c(12500,15000))
#lines(density(Mfuel_5, na.rm = TRUE), lwd=1, col="yellow",xlim=c(12500,15000))
#lines(density(Mfuel_6, na.rm = TRUE), lwd=1, col="black",xlim=c(12500,15000))
#lines(density(Mfuel_7, na.rm = TRUE), lwd=1, col="orange",xlim=c(12500,15000))
#legend(14300, 0.0012, legend=c("sigma=0", "sigma=0.05","sigma=0.08","sigma=0.1","sigma=0.2","sigma=0.3","sigma=0.5"),col=c("blue","red","green","grey","yellow","black","orange"), lty=1)
#dev.off()
