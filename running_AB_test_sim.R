#function that performs simulation
ab_sim <- function(n) {
tt <- rep(NA,n)
p <- rep(NA,n)
x1 <- rnorm(1000)
x2 <- rnorm(1000)
	for(i in 3:n){
		test <- t.test(x1[1:i],x2[1:i])
		tt[i] <- test$statistic
		p[i] <- test$p.value
	}
return(data.frame(tt,p))


}

#run simulation on n number of days
n <- 28
sim <- ab_sim(n)

#plot simulation
par(mfrow=c(2,1))
plot(sim[,1],type='l', ylim=c(-2,2), xlab="Day of Test", ylab="t-stat")
abline(h=1.65, col='red')
abline(h=-1.65, col='red')
plot(sim[,2],type='l', ylim=c(0,1), xlab="Day of Test", ylab="p-value")
abline(h=0.1, col='red')
