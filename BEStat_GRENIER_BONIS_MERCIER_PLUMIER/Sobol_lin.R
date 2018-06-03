source("echantillon.R")
source("reg_lin.R")

sobol <- function(N) { 
    V <- sample_V(N, sigma)
    f <- sample_F(N, sigma)
    SFC <- sample_SFC(N, sigma)
    Mfuel <- a0 + a1*V +a2*f + a3*SFC
    var_Mfuel <- mean(Mfuel*Mfuel) - mean(Mfuel)**2
    
    V_I <- sample_V(N, sigma)
    SFC_I <- sample_SFC(N, sigma)
    Mfuel_F <- a0 + a1*V_I +a2*f + a3*SFC_I

    sobol_F <- (mean(Mfuel*Mfuel_F) - mean(Mfuel)*mean(Mfuel_F))/var_Mfuel
    
    V_I <- sample_V(N, sigma)
    f_I <- sample_F(N, sigma)
    Mfuel_SFC <- a0 + a1*V_I +a2*f_I + a3*SFC
    
    sobol_SFC <- (mean(Mfuel*Mfuel_SFC) - mean(Mfuel)*mean(Mfuel_SFC))/var_Mfuel
    
    a <- (Cov(Mfuel*Mfuel_F,Mfuel*Mfuel_F)-sobol_F*Cov(Mfuel*Mfuel_F,Mfuel*Mfuel)-sobol_F*Cov(Mfuel*Mfuel_F,Mfuel*Mfuel)+sobol_F*sobol_F*Cov(Mfuel*Mfuel,Mfuel*Mfuel))/(Cov(Mfuel,Mfuel)**2)
    b <- (Cov(Mfuel*Mfuel_F,Mfuel*Mfuel_SFC)-sobol_F*Cov(Mfuel*Mfuel_SFC,Mfuel*Mfuel)-sobol_SFC*Cov(Mfuel*Mfuel_F,Mfuel*Mfuel)+sobol_F*sobol_SFC*Cov(Mfuel*Mfuel,Mfuel*Mfuel))/(Cov(Mfuel,Mfuel)**2)
    c <- (Cov(Mfuel*Mfuel_SFC,Mfuel*Mfuel_F)-sobol_SFC*Cov(Mfuel*Mfuel_F,Mfuel*Mfuel)-sobol_F*Cov(Mfuel*Mfuel_SFC,Mfuel*Mfuel)+sobol_F*sobol_SFC*Cov(Mfuel*Mfuel,Mfuel*Mfuel))/(Cov(Mfuel,Mfuel)**2)
    d <- (Cov(Mfuel*Mfuel_SFC,Mfuel*Mfuel_SFC)-sobol_SFC*Cov(Mfuel*Mfuel_SFC,Mfuel*Mfuel)-sobol_SFC*Cov(Mfuel*Mfuel_SFC,Mfuel*Mfuel)+sobol_SFC*sobol_SFC*Cov(Mfuel*Mfuel,Mfuel*Mfuel))/(Cov(Mfuel,Mfuel)**2)
    
    gamma <- matrix(c(a,b,c,d), 2, 2, byrow=TRUE)
    A <- cbind(-1,1)
    sigma_carre_esti <- A%*%gamma%*%t(A)
    seuil <- qnorm(0.05)*sqrt(sigma_carre_esti)/sqrt(N)
    
    indices_sobol <- c(sobol_F, sobol_SFC, seuil, sobol_SFC-sobol_F<seuil)
    names(indices_sobol) <- c("Sobol_F", "Sobol_SFC", "seuil", "rejet")
    return(indices_sobol)
    }
sobol(N)
K <- 1000
t <- 1:K
compteur <- 0
for (k in t){
    compteur <- compteur + sobol(N)[4]
    }
compteur

