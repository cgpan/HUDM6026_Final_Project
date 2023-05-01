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

test <- dat_gen(size = 50, betas = c(-0.265,0.053), 
                iv_mean = 51.24985, iv_var = 100.6209, error_sd = 0.7693)

model_test <- lm(Y ~ X1, data = test)
summary(model_test)
