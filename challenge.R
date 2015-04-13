# the sum of the dice rolls when it is greater than or equal to M
roll <- function(m) {
        sum <- 0
        num <- 0
        while (sum < m) {
                x <- sample(1:6,1)
                sum <- sum + x
                num <- num + 1       
        }
        return(list(sum=sum,num=num))
}
# run n replications 
run <- function(n,M) {
        sum <- c()
        num <- c()
        for (i in 1:n) {
                sum[i] <- roll(M)$sum
                num[i] <- roll(M)$num
        }
        return(list(sum=sum,num=num))
}
n <- 100000 # run 100000 replications

M <- 20
run1 <- run(n,M)
mean(run1$sum - M) # Q1.1
sd(run1$sum - M) # Q1.2
mean(run1$num) # Q1.5
sd(run1$num) # Q1.6

n <- 10000
M <- 10000
run2 <- run(n,M)
mean(run2$sum - M) # Q1.3
sd(run2$sum - M) # Q 1.4
mean(run2$num) # Q1.7
sd(run2$num) # Q1.8

        