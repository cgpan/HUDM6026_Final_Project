# since I stuck at the estimation for the alternative method
# I restart this project strictly follow the Final project's requirement


load("~/Desktop/PhD_Learning/HUDM6026 Computational Statistics/HUDM6026_Final_Project/01_data/02_processed_data/hsls_sub.rdata")
names(hsls_sub)
model_lm <- lm(X3TGPASTEM ~ X1TXMTSCOR, data = hsls_sub)
summary(model_lm)
nrow(hsls_sub)
mean(hsls_sub$X1TXMTSCOR)
var(hsls_sub$X1TXMTSCOR)
# the model for this current project would be
# X3TGPASTEM = -0.265 + 0.053*X1TXMTSCOR
# sd for residual: 0.7693
# number of observations: 19948

# write the data gen function for dat_gen

dat_gen <- function(size= 500,
                    betas,
                    iv_mean,
                    iv_var,
                    error_sd){
  X <- rnorm(size, mean = iv_mean, sd= sqrt(iv_var))
  X_aug <- cbind(1, X)
  Error <- rnorm(size, mean=0, sd=error_sd)
  Y <- X_aug %*% as.matrix(betas) + Error
  out <- cbind(Y, X)
  colnames(out) <- c("Y", "X1")
  return(as.data.frame(out))
}


# dat_gen runs well, next write the funciton of coefficient estimation
reg <- function(y,x) {
  x <- as.matrix(x)
  y <- as.matrix(y)
  y_cen <- apply(y, 2, function(x) x-mean(x))
  x_cen <- apply(x, 2, function(x) x-mean(x))
  b1 <- sum(x_cen*y_cen)/sum(x_cen^2)
  b0 <- mean(y - x*b1)
  
  y_hat <- b0 + x*b1
  sse <- sum((y-y_hat)^2)
  sig_sq <- sse/(nrow(x)-2)
  b1_a <- sum(y_cen/x_cen)/nrow(x)
  b0_a <- mean(y - x*b1_a)
  y_hat_a <- b0_a + x*b1_a
  sse_a <- sum((y-y_hat_a)^2)
  sig_sq_a <- sse_a/nrow(x)
  out_ <- cbind(b0, b1, sig_sq, b0_a, b1_a,sig_sq_a)
  return(out_)
}
x <- hsls_sub$X1TXMTSCOR
y <- hsls_sub$X3TGPASTEM

reg(y,x)


test <- dat_gen(size = 50, betas = c(-0.265,0.053), 
                iv_mean = 51.24985, iv_var = 100.6209, error_sd = 0.7693)

model_test <- lm(Y ~ X1, data = test)
summary(model_test)
