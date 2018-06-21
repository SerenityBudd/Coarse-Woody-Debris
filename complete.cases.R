df <- data.frame(x = 1:10, y = 1:10, z = 1:10)
df[1,3] <- NA
df[2,2] <- NA
df[3,1] <- NA
df[3,3] <- NA

complete.cases(df)
 df2 <- df[complete.cases(df),]
 df3 <- df[complete.cases(df[,c("x","y")]),]
 