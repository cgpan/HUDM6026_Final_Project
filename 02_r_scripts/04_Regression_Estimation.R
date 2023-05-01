# this r file is to write a function for regression model coefficient estimation!
# for any multiple linear model!

# this function takes the dependent variable Y vector and the augmented matrix M_aug as 
# input
reg <- function(Y, X){
  # Note RSS is a function of the estimated coefficients
  # first convert the X matrix into a augmented matrix
  Intercept <- rep(1, nrow(X))
  X_aug <- as.matrix(cbind(Intercept, X))
  X_ss_inverse <- solve(t(X_aug) %*% X_aug)
  B_hat <- X_ss_inverse %*% t(X_aug) %*% Y
  return(B_hat)
}

reg(hsls_sub[,1], hsls_sub[,2:3])


# import the data
load("~/Desktop/PhD_Learning/HUDM6026 Computational Statistics/HUDM6026_Final_Project/01_data/02_processed_data/hsls_sub.rdata")
names(hsls_sub)
# run a simple linear model
model <- lm(X3TGPASTEM~., data = hsls_sub)
summary(model)

cov(hsls_sub[,2:3])
X <- hsls_sub[,2:3]
one_vec <- rep(1, nrow(X))
X_aug <- as.matrix(cbind(one_vec, X))
cov(X_aug)
col_means <-apply(X_aug, 2, mean)
col_means_m <- matrix(col_means,nrow = nrow(X_aug),ncol = 3,byrow = T)
X_aug_centered <-  X_aug - col_means_m
head(X_aug_centered)
t(X_aug_centered) %*% X_aug_centered/ (nrow(X_aug_centered)-1)