##------------------------------------------------------------------------------
## install all required packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos='http://cran.rstudio.com/')
  sapply(pkg, require, character.only = TRUE)
}
packages =c( "tidyverse","knitr", "kableExtra","skimr", "MatchIt", "RItools","optmatch", "ggplot2", "tufte", "tufterhandout", "plotly", "snowfall", "rstan", "gridExtra", "knitr", "gtsummary", "data.table", "GGally", "MASS", "broom", "boot", "foreach", "doParallel", "glmnet", "tidymodels" , "usemodels", "magrittr")
ipak(packages)

##------------------------------------------------------------------------------
## set the working directory to save the data files
## setwd("C:\\Users\\aokutse\\OneDrive - Brown University\\ThesisResults\\data")


##------------------------------------------------------------------------------
## data generating function
set.seed(123)
rm(list = ls())

dgp <- function(n = NULL, ss = NULL)
{
  # treatment variable
  A <- c()
  for (i in 1:n){if (i <= n/2){ A[i] = 1} else {if (i > n/2){A[i] = 0}}}
  # create the variance-covariance matrix
  sigma <- matrix(0, nrow = 4, ncol = 4)
  diag(sigma) <- 1
  # create the vector of Zi's which generate the outcome yi
  mat <- MASS::mvrnorm(n = n, mu = c(0, 0, 0, 0), Sigma = sigma)
  colnames(mat) <- c("z1", "z2", "z3", "z4")
  # generate the error term
  e <- rnorm(n = n, mean = 0, sd = ss)
  # generate the outcome variable based on the specified model
  y <- 210 + 50*A + 27.4*mat[ ,1] + 13.7*mat[, 2] + 13.7*mat[, 3] + 13.7*mat[, 4] + e
  # generate the xi's actually observed by analyst
  x1 <- exp(mat[, 1]/2); x2 <- (mat[, 2]/ (1 + exp(mat[, 1]))) + 10; x3 <- (((mat[, 1]*mat[, 3])/25) + 0.6)^3; x4 <- (mat[,2] + mat[,4] + 20)^2
  # create the missing data variable based on the treatment
  pi <- locfit::expit(-mat[, 1]+0.5*mat[, 2]-0.25*mat[, 3]-0.1*mat[, 4])
  R = rbinom(n = n, size = 1, prob = pi)
  # save variables as a data frame
  df <-data.frame(y, A, x1, x2, x3, x4, R, e, mat)
}

## generate and save the individual data frames under different scenarios [datasets include the desired true outcome dgp]
dt_one <- dgp(n = 500, ss = 1)  ## n= 500, sd = 1
dt_two <- dgp(n = 500, ss = 45) ## n = 500, sd = 45
dt_three <- dgp(n = 2000, ss = 1)  ## n = 2000, sd = 1
dt_four <- dgp(n = 2000, ss = 45)  ## n = 2000, sd = 45


## save the individual data sets
save(dt_one, file = "dt_one.RData")
save(dt_two, file = "dt_two.RData")
save(dt_three, file = "dt_three.RData")
save(dt_four, file = "dt_four.RData")


## create the 1000 replications of each data set and save them
cores <- detectCores()
registerDoParallel(cores-1)
dt1 = foreach(1:1000) %dopar% dgp(n = 500, ss = 1)
dt2 = foreach(1:1000) %dopar% dgp(n = 500, ss = 45)
dt3 = foreach(1:1000) %dopar% dgp(n = 2000, ss = 1)
dt4 = foreach(1:1000) %dopar% dgp(n = 2000, ss = 45)

## save the list of 1000 data frames under different scenarios
save(dt1, file = "dt1.RData")
save(dt2, file = "dt2.RData")
save(dt3, file = "dt3.RData")
save(dt4, file = "dt4.RData")

##------------------------------------------------------------------------------
## checking the true outcome and outcome models based on transformed predictors
dt = dgp(n = 2000, ss = 1)
sd(dt$e) ## residual SD
summary(lm(y ~ A + z1 + z2 + z3 + z4, data = dt))
summary(lm(y ~ A + x1 + x2 + x3 + x4, data = dt))

## explore the regression diagnostics
plot(lm(y ~ A + x1 + x2 + x3 + x4, data = dt))

## end of file
