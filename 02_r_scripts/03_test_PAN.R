# import the data
load("~/Desktop/PhD_Learning/HUDM6026 Computational Statistics/HUDM6026_Final_Project/01_data/02_processed_data/hsls_sub.rdata")
names(hsls_sub)
# run a simple linear model
model <- lm(X3TGPASTEM~., data = hsls_sub)
summary(model)

