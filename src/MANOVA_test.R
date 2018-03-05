#Multivariate Analysis of Variance (MANOVA) test

method1 <- matrix(c(5.4,6.0,6.3,6.7,
                    5.2,6.2,6.0,5.8,
                    6.1,5.9,6.0,7.0,
                    4.8,5.0,4.9,5.0,
                    5.0,5.7,5.0,6.5,
                    5.7,6.1,6.0,6.6,
                    6.0,6.0,5.8,6.0,
                    4.0,5.0,4.0,5.0,
                    5.7,5.4,4.9,5.0,
                    5.6,5.2,5.4,5.8,
                    5.8,6.1,5.2,6.4,
                    5.3,5.9,5.8,6.0), nrow=12,ncol=4,byrow = TRUE)
method2 <-matrix(c(5.0,5.3,5.3,6.5,
                   4.8,4.9,4.2,5.6,
                   3.9,4.0,4.4,5.0,
                   4.0,5.1,4.8,5.8,
                   5.6,5.4,5.1,6.2,
                   6.0,5.5,5.7,6.0,
                   5.2,4.8,5.4,6.0,
                   5.3,5.1,5.8,6.4,
                   5.9,6.1,5.7,6.0,
                   6.1,6.0,6.1,6.2,
                   6.2,5.7,5.9,6.0,
                   5.1,4.9,5.3,4.8), nrow=12,ncol=4,byrow = TRUE)

method3 <-matrix(c(4.8,5.0,6.5,7.0,
                   5.4,5.0,6.0,6.4,
                   4.9,5.1,5.9,6.5,
                   5.7,5.2,6.4,6.4,
                   4.2,4.6,5.3,6.3,
                   6.0,5.3,5.8,6.4,
                   5.1,5.2,6.2,6.5,
                   4.8,4.6,5.7,5.7,
                   5.3,5.4,6.8,6.6,
                   4.6,4.4,5.7,5.6,
                   4.5,4.0,5.0,5.9,
                   4.4,4.2,5.6,5.5), nrow=12,ncol=4,byrow = TRUE)
method1
method2
method3


colnames(method1) <- c("y1", "y2", "y3", "y4")
colnames(method2) <- c("y1", "y2", "y3", "y4")
colnames(method3) <- c("y1", "y2", "y3", "y4")
rownames(method2) <- c(13:24)
rownames(method3) <- c(25:36)


method1.bar <- colMeans(method1)
method2.bar <- colMeans(method2)
method3.bar <- colMeans(method3)


method.all.bar <- (method1.bar+method2.bar+method3.bar)/3

method1.bar.diff <- method1.bar - method.all.bar
method2.bar.diff <- method2.bar - method.all.bar
method3.bar.diff <- method3.bar - method.all.bar

H <- 12 * unname(method1.bar.diff %*% t(method1.bar.diff) 
              + method2.bar.diff %*% t(method2.bar.diff) 
              + method3.bar.diff %*% t(method3.bar.diff))

#calculate "within" matrices


"compute.within.matrix" <- function(data, mean) {
  ret <- matrix(as.numeric(0), nrow=4, ncol=4) 
  for (i in 1:12) { 
  diff <- as.numeric(unname(data[i,] - mean)) 
  ret <- ret + diff %*% t(diff) 
  } 
  return(ret) 
  } 
E <- compute.within.matrix(method1, method1.bar) + 
  compute.within.matrix(method2, method2.bar) + 
  compute.within.matrix(method3, method3.bar)


#Four MANOVA
# Wilks' Test Statistic ????=|????||????+????|

Lambda <- det(E) / det(E + H)

# Pillai Statistic ????(????)=????????[
install.packages("psych")
library("psych")
V.s <- tr(solve(E + H) %*% H)

# Lawley-Hotelling statistic is defined as ????(????)=????????(???????????????)

U.s <- tr(solve(E) %*% H)

# Roy's largest root test ??=????11+????1
lambda.1 <- eigen(solve(E) %*% H)$values[1] 
theta <- lambda.1 / (1 + lambda.1)
