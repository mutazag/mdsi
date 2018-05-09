x <- rbind(rnorm(100, mean= 60, sd =2), 130)
x <- runif(n = 100, min=-120, max=50)
  #c(1,3,2,5,50,600,40)

normalized <- (x - min(x))/(max(x)-min(x))


# Histogram of example data and normalized data
par(mfrow=c(1,2))
hist(x,          breaks=10, xlab="Data",            col="lightblue", main="")
hist(normalized, breaks=10, xlab="Normalized Data", col="lightblue", main="")
plot(x, normalized)


# center x but no scaling 
centered <- scale(x, center = TRUE, scale = FALSE)
zscore <- scale(x, center = TRUE, scale = TRUE)

par(mfrow=c(2,2))
hist(x,          breaks=10, xlab="Data",            col="lightblue", main="")
hist(normalized, breaks=10, xlab="Normalized Data", col="lightblue", main="")
hist(centered,          breaks=10, xlab="centered",            col="lightblue", main="")
hist(zscore, breaks=10, xlab="z-score", col="lightblue", main="")

par(mfrow=c(1,1))
df <- data.frame(x, normalized, centered, zscore)
plot(df)


#plot correlations
#http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
library(corrplot)
xcor <- cor(mtcars)
corrplot(xcor, method = "circle")
corrplot(xcor, method = "pie")
corrplot(xcor, method = "color")
corrplot(xcor, method = "number")
corrplot(xcor, method = "number", type="upper")
corrplot(xcor, method = "number", type="lower", order="hclust")
corrplot(xcor, method = "circle", type="lower", order="hclust", col=c("black", "white"), bg="lightblue")
corrplot(xcor, method = "circle", type="lower", order="hclust", addCoef.col = "black")
