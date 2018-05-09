x <- rbind(rnorm(100, mean= 60, sd =2), 130)
x <- runif(n = 100, min=-120, max=50)
  #c(1,3,2,5,50,600,40)

normalized <- (x - min(x))/(max(x)-min(x))


# Histogram of example data and normalized data
par(mfrow=c(1,2))
hist(x,          breaks=10, xlab="Data",            col="lightblue", main="")
hist(normalized, breaks=10, xlab="Normalized Data", col="lightblue", main="")
plot(x, normalized)
