source("echantillon.R")

sigma 0
N <- 1000


sobol <- function(N, sigma) { 
    V <- sample_V(N, sigma)
    f <- sample_F(N, sigma)
    SFC <- sample_SFC(N, sigma)
    Mfuel <- (42600+46200+19900)*(exp((10^(-3)*SFC*9.8*3000)/(V*f))-1)
    var_Mfuel <- mean(Mfuel*Mfuel) - mean(Mfuel)**2
    
    V_I <- sample_V(N, sigma)
    SFC_I <- sample_SFC(N, sigma)
    Mfuel_F <- (42600+46200+19900)*(exp((10^(-3)*SFC_I*9.8*3000)/(V_I*f))-1)

    sobol_F <- (mean(Mfuel*Mfuel_F) - mean(Mfuel)*mean(Mfuel_F))/var_Mfuel
    
    V_I <- sample_V(N, sigma)
    f_I <- sample_F(N, sigma)
    Mfuel_SFC <- (42600+46200+19900)*(exp((10^(-3)*SFC*9.8*3000)/(V_I*f_I))-1)
    
    sobol_SFC <- (mean(Mfuel*Mfuel_SFC) - mean(Mfuel)*mean(Mfuel_SFC))/var_Mfuel
    
    a <- (Cov(Mfuel*Mfuel_F,Mfuel*Mfuel_F)-sobol_F*Cov(Mfuel*Mfuel_F,Mfuel*Mfuel)-sobol_F*Cov(Mfuel*Mfuel_F,Mfuel*Mfuel)+sobol_F*sobol_F*Cov(Mfuel*Mfuel,Mfuel*Mfuel))/(Cov(Mfuel,Mfuel)**2)
    b <- (Cov(Mfuel*Mfuel_F,Mfuel*Mfuel_SFC)-sobol_F*Cov(Mfuel*Mfuel_SFC,Mfuel*Mfuel)-sobol_SFC*Cov(Mfuel*Mfuel_F,Mfuel*Mfuel)+sobol_F*sobol_SFC*Cov(Mfuel*Mfuel,Mfuel*Mfuel))/(Cov(Mfuel,Mfuel)**2)
    c <- (Cov(Mfuel*Mfuel_SFC,Mfuel*Mfuel_F)-sobol_SFC*Cov(Mfuel*Mfuel_F,Mfuel*Mfuel)-sobol_F*Cov(Mfuel*Mfuel_SFC,Mfuel*Mfuel)+sobol_F*sobol_SFC*Cov(Mfuel*Mfuel,Mfuel*Mfuel))/(Cov(Mfuel,Mfuel)**2)
    d <- (Cov(Mfuel*Mfuel_SFC,Mfuel*Mfuel_SFC)-sobol_SFC*Cov(Mfuel*Mfuel_SFC,Mfuel*Mfuel)-sobol_SFC*Cov(Mfuel*Mfuel_SFC,Mfuel*Mfuel)+sobol_SFC*sobol_SFC*Cov(Mfuel*Mfuel,Mfuel*Mfuel))/(Cov(Mfuel,Mfuel)**2)
    
    gamma <- matrix(c(a,b,c,d), 2, 2, byrow=TRUE)
    A <- cbind(-1,1)
    sigma_carre_esti <- A%*%gamma%*%t(A)
    niveau <- 0.05
    seuil <- qnorm(niveau)*sqrt(sigma_carre_esti)/sqrt(N)
    
    indices_sobol <- c(sigma, sobol_F, sobol_SFC, niveau, seuil, sobol_SFC-sobol_F<seuil)
    names(indices_sobol) <- c("bruit", "Sobol_F", "Sobol_SFC", "niveau", "seuil", "rejet")
    return(indices_sobol)
    }
    
sobol(N, sigma)

compteur <- 0
for (i in 1:1000){
    compteur <- compteur + sobol(N, sigma)[6]
    }
(compteur/1000)*100
#sobol(N)
#K <- 1000
#t <- 1:K
#lSobol_F <- vector("numeric", K)
#lSobol_SFC <- vector("numeric", K)
#for (i in t){
 #   lSobol_F[i] <- sobol(i)[1]
  #  lSobol_SFC[i] <- sobol(i)[2]
   # }
#png("Sobol_F.png")
#plot(t, lSobol_F, xlab="taille de l'échantillon", ylab="indice estimé de Sobol de F",pch=3, cex=0.5)
#png("Sobol_SFC.png")
#plot(t, lSobol_SFC, xlab="taille de l'échantillon", ylab="indice estimé de Sobol de SFC",pch=3, cex=0.5)
#dev.off()

#b <- (0:10)/10
#sobol_F_b <- vector("numeric", 11)
#sobol_SFC_b <- vector("numeric", 11)
#for (k in 1:11){
#    sobol_F_b[k] <- sobol(N,b[k])[1]
#    sobol_SFC_b[k] <- sobol(N,b[k])[2]
#    }
#X <- cbind(sobol_F_b, sobol_SFC_b)
#pdf("Sobol_b.pdf")
#matplot(b, X, type="b", pch=20, col=c("blue", "green"), xlab="niveau de bruit sigma", ylab="valeur des indices de Sobol")
#legend("bottomright", inset=0.01, legend=c("indice de Sobol de F", "indice de Sobol de SFC"), col=c("blue","green"),pch=20,bg= ("white"), #horiz=F)
#dev.off()


