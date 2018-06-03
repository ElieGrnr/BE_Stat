source("echantillon.R")

N <- 50000
sigma <- 1

Mfuel <- sample_Mfuel(N, sigma)

descr <- c(mean(Mfuel), mean(Mfuel*Mfuel)-mean(Mfuel)^2, sqrt(mean(Mfuel*Mfuel)-mean(Mfuel)^2), min(Mfuel), max(Mfuel), IQR(Mfuel))
names(descr) <- c("moyenne_Mfuel", "variance_Mfuel", "ecart-type_Mfuel", "min_Mfuel", "max_Mfuel", "IQR")
descr

#resultats <- data.frame(moyenne=c(mean(sample_Mfuel(N, 0)),mean(sample_Mfuel(N, 0.01)),mean(sample_Mfuel(N, 0.05)),mean(sample_Mfuel(N, 0.1)),mean(sample_Mfuel(N, 0.5)),mean(sample_Mfuel(N, 1))), variance=c(mean(sample_Mfuel(N, 0)*sample_Mfuel(N, 0))-mean(sample_Mfuel(N,0))^2,mean(sample_Mfuel(N, 0.01)*sample_Mfuel(N, 0.01))-mean(sample_Mfuel(N, 0.01))^2,mean(sample_Mfuel(N, 0.05)*sample_Mfuel(N, 0.05))-mean(sample_Mfuel(N, 0.05))^2,mean(sample_Mfuel(N, 0.1)*sample_Mfuel(N, 0.1))-mean(sample_Mfuel(N, 0.1))^2,mean(sample_Mfuel(N, 0.5)*sample_Mfuel(N, 0.5))-mean(sample_Mfuel(N, 0.5))^2,mean(sample_Mfuel(N, 1)*sample_Mfuel(N, 1))-mean(sample_Mfuel(N, 1))^2)), IQR=c(IQR(sample_Mfuel(N, 0)),IQR(sample_Mfuel(N, 0.01)),IQR(sample_Mfuel(N, 0.05)),IQR(sample_Mfuel(N, 0.1)),IQR(sample_Mfuel(N, 0.5)),IQR(sample_Mfuel(N, 1))), row.names=c("sigma=0", "sigma=0.01", "sigma=0.05", "sigma=0.1", "sigma=0.5", "sigame=1"))
#resultats
