x.bar <- 36.4
mu.0 = 20
n <- 70
sigma <- 40

z <- (x.bar-mu.0)/(sigma/sqrt(n))
z

alpha.5pc.1tail <- .05
quantile <- qnorm(alpha.5pc.1tail,mu.0,sigma/sqrt(n))
quantile
z.crit.val <- qnorm(p=.05,mean=0,sd=1,lower.tail = F)
z.crit.val
z>z.crit.val
p.value <- pnorm(z,mean=1,sd=1,lower.tail=F)
p.value


p.v <- 2*pnorm(-2.47,mean=0, sd =1)
p.v


# Fun with central limit theorem
n <- 100
data<-matrix(runif(1000*n,0,100),ncol=n)
row.xbar <- apply(data,1,mean)
hist(row.xbar,breaks = 20)

pnorm(-.47,0,1)

qchisq(.05,39,lower.tail = F)
pchisq(66.98,39,lower.tail=F)

qf(.05,47,59,lower.tail=F)

