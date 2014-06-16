# Simulation based power analysis template

# Read in data or use randomly generated values
#setwd("C:/Users/levdavis/Dropbox/SEO/Concept Power Analysis")
#x <- read.table("bhp_2week_clicks.txt")
#dat <- x[,2]
#rm(x)
#head(dat)

# zero inflated exponential dist with expected value = exe*p
m <- 500000; exe <- 500; p <- 0.1
y1 <- rexp(m, rate=1/exe)
y2 <- rbinom(m, size=1, prob=p)
dat <- y1*y2


# outlier removal
dat[dat>= quantile(dat, 0.99)] <-  quantile(dat, 0.99)

# simulation

N <- 100000 #sample size (N must be even)
Ns <- 500 #simulation size
Se <- 0.04 #size of effect
cl <- 0.90 # level of confidence

group <- as.factor(c(rep("trt",N/2),rep("cntr",N/2))) # creates vector of trt and cntrl lables
pwr <- rep(NA,Ns)
pval <- rep(NA,Ns)
effect <- rep(NA,Ns)
prop_effect <- rep(NA,Ns)

plot(0,2,xlim=c(1,Ns),ylim=c(0,1),type='l',xlab="Iteration",ylab="Cumulative Average of Procedure's Results")

for(i in 1:Ns){
	x <- sample(dat,N,replace=TRUE)			# randomly sample from data
	x[1:(N/2-1)] <- x[1:(N/2-1)]*(1+Se)			# apply artificial effect to treatment group
	test <- lm(x~group)						# model
	pval[i] <- summary(test)$coefficients[[8]]		# records p-value (This needs to change with the model)
	effect[i] <- summary(test)$coefficients[[2]]	# Records point estimate
	pwr[i] <- if (effect[i] > 0 & pval[i] < (1-cl)) 1 else 0	# 1 if effect detected, 0 if not
	prop_effect[i] <- effect[i]/mean(x[N/2:N])
	lines(c(i-1,i),c(mean(pwr[1:i-1], na.rm = TRUE),mean(pwr[1:i], na.rm = TRUE)),col="blue")
	#print(i); flush.console() 	
}

mean(pwr) # estimated power