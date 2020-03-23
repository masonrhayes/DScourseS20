library(tidyverse)
library(nloptr)
library(gradDescent)
library(stargazer)

set.seed(100)
N <- 100000
K <- 10
sigma <- 0.5

X <- matrix(rnorm(N*K,mean=0,sd=sigma),N,K)
X[,1] <- 1
eps <- rnorm(N,mean=0,sd=0.5)
betas <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
Y <- X%*%betas + eps

# 1. Using lm
estimates <- lm(Y~X -1)
summary(estimates)
stargazer(estimates, summary = TRUE)

# 2. Using closed-form
beta_ols <- solve(t(X) %*% X) %*% t(X) %*% Y
view(beta_ols)

# 3. Using nloptr
objfun <- function(beta,y,X) {
  return (sum((y-X%*%beta)^2))
}

gradient <- function(beta,y,X) {
  return ( as.vector(-2*t(X)%*%(y-X%*%beta)) )
}

initial_betas <- runif(dim(X)[2])

options1 <- list("algorithm"="NLOPT_LD_LBFGS",
                 "xtol_rel"=1.0e-6,
                 "maxeval"=1e3)
options2 <- list("algorithm"="NLOPT_LN_NELDERMEAD",
                 "xtol_rel"=1.0e-6,
                 "maxeval"=1e3)

optimized_results1 <- nloptr(x0=initial_betas,
                             eval_f = objfun,
                             eval_grad_f = gradient,
                             opts = options1,
                             y = Y,
                             X = X)
optimized_results2 <- nloptr(x0=initial_betas,
                             eval_f = objfun,
                             eval_grad_f = gradient,
                             opts = options2,
                             y = Y,
                             X = X)
optimized_results1
optimized_results2

# 4. Using Gradient Descent
alpha <- 0.0000003
b0 <- initial_betas
b1 <- b0 - alpha * gradient(b0, Y, X)
while(sum(b1==b0) != 10) {
  b0 <- b1
  b1 <- b0 - alpha * gradient(b0, Y, X)
}

# Compare all results

compare_results <- data.frame(betas, estimates$coefficients, 
                              beta_ols,
                              optimized_results1$solution,
                              optimized_results2$solution,
                              b1) %>%
  setNames(., c("True_Betas", "LM", "OLS_ClosedForm", 
                "LBFGS_Algo", "NelderMead_Algo", 
                "GradientDescent"))
view(compare_results)
stargazer(compare_results, summary = FALSE)
