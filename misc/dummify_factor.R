df <- data.frame( x = runif(n = 3), 
                 gender = c("M", "F", "M"))
df$gender <- as.factor(df$gender)
df2 <- cbind(df, model.matrix( ~ gender -1 , data = df))
df2
