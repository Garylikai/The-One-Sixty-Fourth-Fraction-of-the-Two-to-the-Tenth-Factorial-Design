library(FrF2)
dsg <- FrF2(nruns = 16, nfactors = 10, factor.names = LETTERS[1:10],
            generators = c("ABC", "BCD", "ACD", "ABD", "ABCD", "AB"), 
            seed = 123)
summary(dsg)
write.csv(dsg, "dsg.csv")

data <- read.csv("C:/Users/Gary/OneDrive - Stony Brook University/Course/AU21/AMS 582/Project/total22.csv", header = TRUE)
data <- data[, -1]
summary(data)

# cols <- head(colnames(data), -1)
# data[cols] <- lapply(data[cols], factor)
# Don't use factors in lm(). It will give a wrong result for orthogonal coding.
# Encoded as factors or not yield the same result in aov() and anova().

aov1 = aov(y ~ ., data = data)
anova(aov1)
# lm1 = lm(y ~ ., data = data)
# summary(lm1)

MEPlot(aov1, main = NULL)

step(aov1, k = log(nrow(data)))
step(aov1, direction = "backward", k = log(nrow(data)))

aov2 = aov(y ~ C + F + I + C:F + C:I + F:I, data = data)
anova(aov2)

IAPlot(aov2, main = NULL)

aov3 = aov(y ~ C + F + I, data = data)
anova(aov3)

lm3 = lm(y ~ C + F + I, data = data)
summary(lm3)
lm3$coefficients

res = resid(lm3)

plot(fitted(lm3), res, xlab = expression(hat(Y)), ylab = "Residual", pch = 20)
abline(0, 0)

qqnorm(res, main = NULL, pch = 20)
qqline(res, col = 2)

confint(lm3)

pred_data <- expand.grid(C = c(-1, 1), F = c(-1, 1), I = c(-1, 1))
predict(lm3, newdata = pred_data, interval = "confidence")
predict(lm3, newdata = pred_data, interval = "prediction")
