source("libraries.R")

head(funcdiv3)
str(funcdiv3)

funcdiv4 <- funcdiv3 %>% droplevels()

funcdiv4$Trophic.Guild2 <- relevel(funcdiv4$Trophic.Guild, ref = "1")
test <- multinom(Trophic.Guild2 ~ snag, data = funcdiv4,
                 na.action =  na.exclude)
summary(test)

z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
