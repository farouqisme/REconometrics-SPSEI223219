# Load required libraries
library(gmm)


####Create vector of moment function
g1 <- function(tet,x)
{
  m1 <- (tet[1]-x)
  m2 <- (tet[2]^2 - (x - tet[1])^2)
  m3 <- x^3-tet[1]*(tet[1]^2+3*tet[2]^2)
  f <- cbind(m1,m2,m3)
  return(f)
}

####Create covariance matrix
Dg <- function(tet,x)
{
  G <- matrix(c( 1,
                 2*(-tet[1]+mean(x)),
                 -3*tet[1]^2-3*tet[2]^2,0,
                 2*tet[2],-6*tet[1]*tet[2]),
              nrow=3,ncol=2)
  return(G)
}

######generate normally distributed random numbers
set.seed(123)
n <- 200
x1 <- rnorm(n, mean = 4, sd = 2)

###### run gmm using the starting values (µ0, σ^2_0) = (0, 0)
print(res <- gmm(g1,x1,c(mu = 0, sig = 0), gradv = Dg))
summary(res)
