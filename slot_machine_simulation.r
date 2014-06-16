####################################################################
#################### SLOT MACHINE PAYOUT SIMULATION ################
####################################################################

# Payout vector:
# Probability of $1000: 1/10000
# Probability of $500: 6/10000
# Probability of $100: 20/10000
# Probability of $10: 50/10000
# Probability of $5: 60/10000
# Probability of $1: 1200/10000
# Probability of $0: 8663/10000
pay <- -1*c(1000,rep(500,6),rep(100,20),rep(10,50),rep(5,60),
rep(1,1200),rep(0,8663))
# Revenue vector
rev <- rep(1,10000)
# Net profit
net <- pay+rev

# number of slot machines
n <- 100
# number of games
ng <- 1000

slot <- array(NA,c(n,ng))

# simulates ng games played on n slot machines
# and computes the cumulative sum of net profits
for(i in 1:n){
	slot[i,] <- cumsum(sample(net, ng, replace=TRUE))
	}

# mean cumulative sum of net profits per machine
slot_mean <- apply(slot, 2 , mean)

# plots n cumulative net profits
# with (red) mean cumulative net profits
# and (black) break even line
plot(slot[1,], type='l', ylim=c(-2000,800))
for(i in 2:n){
	lines(slot[i,],col=i)
	}
lines(slot_mean, col="red", lwd=3)
lines(rep(0,ng), lwd=2)