sample_V <- function(N, sigma) {
    V <- runif(N, 226, 234) + sigma*rnorm(N)
    return(V)
}

sample_F <- function(N, sigma) {
    inter <- rbeta(N, 7, 2)
    f <- (19.05-18.7)*inter+18.7 + sigma*rnorm(N)
    return(f)
}

sample_SFC <- function(N, sigma) {
    inter <- rexp(N, 3.45)
    SFC <- inter + 17.23 + sigma*rnorm(N)
    return(SFC)
}
 
sample_Mfuel <- function(N, sigma) {
    V <- sample_V(N, sigma)
    f <- sample_F(N, sigma)
    SFC <- sample_SFC(N, sigma)
    Mfuel <- (42600+46200+19900)*(exp((10^(-3)*SFC*9.8*3000)/(V*f))-1)
    return(Mfuel)
}

Cov <- function(X, Y) {
    N <- length(X)
    Xbar <- mean(X)
    Ybar <- mean(Y)
    cov <- (t(X-Xbar)%*%(Y-Ybar))/(N-1)
    return(as.numeric(cov))
    }

