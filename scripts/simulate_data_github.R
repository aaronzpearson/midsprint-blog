simulate_correlation <- function(data, means, sds, sample.size, sample = TRUE) {
  
  require(corpcor, quietly = TRUE)
  require(rockchalk, quietly = TRUE)
  
  temp = data
  
  dat <- temp[, 4:ncol(temp)]
  dat[upper.tri(dat)] <- NA
  diag(dat) <- 1
  
  tx <- t(dat)
  
  dat[upper.tri(dat)] <- tx[upper.tri(tx)]
  
  cor.matrix <- as.matrix(dat)
  cor.matrix <- corpcor::make.positive.definite(cor.matrix)
  cov.matrix <- sds %*% t(sds) * cor.matrix
  
  
  cor.df <- rockchalk::mvrnorm(n = sample.size, Sigma = cov.matrix, empirical = sample, mu = means)
  cor.df <- as.data.frame(cor.df)
  colnames(cor.df) <- temp$variable
  
  return(cor.df)
  
}

simulate_regression <- function(data, sample.size, means, sds, r2) {
  
  require(rockchalk, quietly = TRUE)
  
  temp = data
  
  pull.means <- temp$mean[1:(nrow(temp) - 1)]
  pull.sds <- temp$sd[1:(nrow(temp) - 1)]
  
  y.intercept <- temp$coefficient[nrow(temp)]
  
  regr.df <- rockchalk::genCorrelatedData3(N = sample.size,
                                           means = pull.means,
                                           sds = pull.sds,
                                           rho = r2, 
                                           beta = c(y.intercept,
                                                    temp$coefficient[2:(nrow(temp) - 1)]))
  
  colnames(regr.df)[1:nrow(temp)] <- temp$variable
  
  return(regr.df)
  
}
