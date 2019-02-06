data(cars)
wt <- rep(NA, dim(cars)[1])
length(wt)	# [1] 50

wt[1:20] <- 100
wt[21:40] <- 10
wt[41:50] <- 1

dt1 <- as.matrix(cars)

apply(dt1, 2, mean)
# speed  dist 
# 15.40 42.98 

apply(dt1 * wt, 2, sum) /sum(wt)
#   speed     dist 
#10.91222 24.53937 

lm.dt1 <- lm(dt1[,2]~dt1[,1])

summary(lm.dt1)

plot(dt1, pch=21, bg="gray", col="black", cex=5)
abline(lm.dt1, col="blue", lwd=4)

lm.dt2 <- lm(dt1[,2]~dt1[,1], weight=wt)
summary(lm.dt2)
abline(lm.dt2, col="red", lwd=4, lty=2)
