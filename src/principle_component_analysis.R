#Principal component analysis(PCA) of Beetle species data
#Loading the data set
beetle <- read.table('C:/Users/jaspr/iCloudDrive/QUARTER 6/DA 410/7/beetle.txt', header = TRUE)


#sample covariance matrix S 
#beetle species Haltica_oleracea
b.1 <- beetle[1:19, 2:5]
n <- nrow(b.1)
Xc <- scale(b.1, center=TRUE, scale=FALSE)
S.1<- t(Xc) %*% Xc / (n-1)
S.1

#eigenvalues of S
s.1.eigen <- eigen(S.1)
s.1.eigen

#eigenvectors reprsent the principal component of S.
#Using eigenvalues of S to find the proportion of the total variance explained by the components.
for (S.1 in s.1.eigen$values) {
  print(S.1 / sum(s.1.eigen$values))
}

plot(s.1.eigen$values, xlab = 'Eigenvalue Number', ylab = 'Eigenvalue Size', main = 'Scree Graph')
lines(s.1.eigen$values)

#sample covariance matrix S
#beetle species Haltica_carduorum
b.2 <- beetle[20:39, 2:5]
n <- nrow(b.2)
Xc <- scale(b.2, center=TRUE, scale=FALSE)
S.2 <- t(Xc) %*% Xc / (n-1)
S.2

#eigenvalues of S
s.2.eigen <- eigen(S.2)
s.2.eigen

#eigenvectors reprsent the principal component of S.
#Using eigenvalues of S to find the proportion of the total variance explained by the components.
for (S.2 in s.2.eigen$values) {
  print(S.2 / sum(s.2.eigen$values))
}

plot(s.2.eigen$values, xlab = 'Eigenvalue Number', ylab = 'Eigenvalue Size', main = 'Scree Graph')
lines(s.2.eigen$values)