## Neural Network 

# install.packages("nnet")
# install.packages("NeuralNetTools")


library(ISLR)
library(nnet)
library(NeuralNetTools)



data("OJ")

df <- OJ
df


nn_fit <- nnet(Purchase ~ ., OJ, size = 10 )

plotnet(nn_fit, nid=F)