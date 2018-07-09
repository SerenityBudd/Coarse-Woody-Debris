funcdiv4 <- funcdiv3 %>% 
  filter(!R.Guild1 == "", !R.Guild1 == "Catadromous", !R.Guild1 == "Bearer") %>%
  droplevels() %>% select(c(snag, R.Guild1))

#funcdiv4 <- funcdiv4[complete.cases(funcdiv4),]

xx <- sample(x = 1:length(funcdiv4$snag), size = length(funcdiv4$snag)/2, replace = F)

train <- funcdiv4[xx,]
test <- funcdiv4[-xx,]

table(train$snag, train$R.Guild1)
table(test$snag, test$R.Guild1)

mylogit <- glm(R.Guild1 ~ snag, data = train, family = "binomial")
summary(mylogit)

anova(mylogit, test="Chisq")

library(pscl)
pR2(mylogit)


fitted.results <- predict(mylogit,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$snag)
print(paste('Accuracy',1-misClasificError))





