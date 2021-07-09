###########################################################################
#
#  REGRESSION WITH MAXIMUM LIKELIHOOD ESTIMATION
#
###########################################################################

load('famafrench.RData')
attach(famafrench)
xr.msft<- msft -riskfree

# xr.msft = monthly excess return of MSFT
# mkt_rf = monthly excess return of the "market" portfolio

# Step 1:  create a fuction for the NEGATIVE log likelihood (need not be Normal)
neg.ll.mm<- function(alpha,Bm,sig) {
  y<- xr.msft  
  y.hat <- alpha + Bm*mkt_rf
  ll<- dnorm(y,y.hat,sig,log=T)
  -sum(ll)
}
# Step 2:  Use a Solver (I prefer 'nlm') to estimate the parameters
ml.est.mm<- mle2(neg.ll.mm,start=list(alpha=0,Bm=1,sig=sd(xr.msft)), 
                  optimizer='nlm',steptol=1e-16, gradtol=1e-6, stepmax=5000)

summary(ml.est.mm)

alpha <- coef(ml.est.mm)[1]; Bm <- coef(ml.est.mm)[2]; sig <- coef(ml.est.mm)[3]
resid.mm<- xr.msft - alpha-Bm*mkt_rf # Have to model MLE residuals by hand

# Compare MLE coefficients with those from OLS 
OLS.mm<- lm(xr.msft~mkt_rf)
summary(OLS.mm)
sum(resid.mm^2)   #same RSS as OLS
deviance(OLS.mm)  #same RSS as MLE

###########################################################################
#
#  Where do the MLE standard errors come from?
#
###########################################################################

# Here is a function that calculates the log likelihood for each obervation
# for a given set of parameter estimates (alpha, Bm and sig)
ll.mm <- function(alpha, Bm, sig) {
  dnorm(xr.msft,alpha+Bm*mkt_rf,sig,log=1)
}

# This function is a numerical approximation of the partial derivatives
# of the log likelihood with respect to alpha, Bm and sig.
d.ll.mm <- function(alpha, Bm, sig, delta=.0001) {
  f <- 1+delta
  b <- 1-delta
  d.ll.d.alpha <- (ll.mm(alpha*f,Bm,sig)-ll.mm(alpha*b,Bm,sig))/((f-b)*alpha)
  d.ll.d.Bm <- (ll.mm(alpha,Bm*f,sig)-ll.mm(alpha,Bm*b,sig))/((f-b)*Bm)
  d.ll.d.sig <- (ll.mm(alpha,Bm,sig*f)-ll.mm(alpha,Bm,sig*b))/((f-b)*sig)
  
  d.ll <- cbind(d.ll.d.alpha,d.ll.d.Bm,d.ll.d.sig)
  colnames(d.ll) <- c('alpha','Bm','sig')
  return(d.ll)
}

dl<-d.ll.mm(alpha,Bm,sig)

# Compare the coefficient covariance matrix of OLS and MLE using vcov(),
# as well as from the inverse of t(d.ll/d.parameters) %*% d.ll/d.parameters
vcov(OLS.mm);vcov(ml.est.mm); solve(t(dl) %*% dl)

# We can find the coefficient standard errors from this infromation
sqrt(diag(vcov(ml.est.mm)))
sqrt(diag(solve(t(dl) %*% dl)))

# We can find the Hessian of the likelihood function using the 'hessian()' function.
# hessian() requires the 'numDeriv' package
library('numDeriv')
alpha <- coef(ml.est.mm)[1]; Bm <- coef(ml.est.mm)[2]; sig <- coef(ml.est.mm)[3]

# hessian() requires the coefficient inputs to be invector form
matrix.ll.mm <- function(x) {
  neg.ll.mm(alpha=x[1],Bm=x[2],sig=x[3])
}
library('numDeriv')
hess<-hessian(matrix.ll.mm,c(alpha,Bm,sig)) # requires 'numDeriv' package
hess; solve(hess)
sqrt(diag(solve(hess)))
sqrt(diag(vcov(ml.est.mm)))
sqrt(diag(solve(t(dl) %*% dl)))

###########################################################################
#
#  Fun with hypothesis testing in MLE
#
###########################################################################

# Estimate Fama-French betas for MSFT
OLS.ff <- lm(xr.msft ~ mkt_rf+smb+hml)

# Step 1:  create a fuction for the NEGATIVE log likelihood
neg.ll.ff<- function(alpha,Bm,Bs,Bh,sig) {
  y<- xr.msft
  y.hat <- alpha + Bm*mkt_rf +Bs*smb + Bh*hml
  resid.ll<- dnorm(y,y.hat,sig,log=T)
  -sum(resid.ll)
}
# Step 2:  Use a Solver to estimate the parameters
ml.est.ff<- mle2(neg.ll.ff,start=list(alpha=0,
                                      Bm=1,
                                      Bs=0,
                                      Bh=0,
                                      sig=sd(xr.msft)), 
                 optimizer='nlm',
                 steptol=1e-16, gradtol=1e-6, stepmax=5000)
summary(OLS.ff);summary(ml.est.ff)

######################################################################
#  Statistics used for model selection: AIC, BIC, logLik
######################################################################

AIC(ml.est.mm, ml.est.ff,k=2)                  # AIC (lower is better)
AIC(ml.est.mm, ml.est.ff,k=log(length(msft)))  # BIC (lower is better)
logLik(ml.est.mm); logLik(ml.est.ff) #log likelihood (higher is better)         

######################################################################
#  LM, Wald and LR tests for joint hypothesis testing
######################################################################
coef(ml.est.ff)
resid.ff<- xr.msft - coef(ml.est.ff)[1]-coef(ml.est.ff)[2]*mkt_rf -
                        coef(ml.est.ff)[3]*smb - coef(ml.est.ff)[4]*hml
nobs<-length(xr.msft)
#------------------------------------------------------------------------
# Wald test
#------------------------------------------------------------------------
R<- matrix(c(0,0,1,0,0,
             0,0,0,1,0),nrow=2,byrow=T)
b<- matrix(coef(ml.est.ff),ncol=1)
q<- matrix(c(0,0),ncol=1)
omega<- vcov(ml.est.ff)

Wald <- t(R%*% b - q) %*% solve(R %*% omega %*% t(R)) %*% (R %*% b - q)
cat('Wald statistic =', Wald,'df = 2, and pvalue = ',pchisq(Wald,2,lower.tail=0))
# or
nobs*(sum(resid.mm^2)-sum(resid.ff^2))/sum(resid.ff^2)

#------------------------------------------------------------------------
# LR (Likelihood Ratio) test
#------------------------------------------------------------------------
LR <- -2*(logLik(ml.est.mm)-logLik(ml.est.ff))
cat('LR statistic =', LR,'df = 2, and pvalue = ',pchisq(LR,2,lower.tail=0))
# or
anova(ml.est.mm,ml.est.ff)
# or
-nobs*log(sum(resid.ff^2)/sum(resid.mm^2))


#------------------------------------------------------------------------
# LM (Lagrangian Multiplier) Test for Ho: Bs=Bh=0 (m=2 restrictions)
#------------------------------------------------------------------------
aux.reg <- lm(resid.mm ~ mkt_rf+smb+hml)
summary(aux.reg)$r.squared*nobs  # LM = R-squared * T ~ chisq(df=m)
# or
LM <- nobs*(sum(resid.mm^2)-sum(resid.ff^2))/sum(resid.mm^2)
cat('LM statistic =', LM,'df = 2, and pvalue = ',pchisq(LM,2,lower.tail=0))


compare <- c(LM,LR,Wald)
names(compare) <- c('LM','LR','Wald')
compare
AIC(ml.est.mm,ml.est.ff,k=log(length(xr.msft))) # BIC 
AIC(ml.est.mm,ml.est.ff)

detach(famafrench)

###########################################################################
#
#  Modeling Heteroskedasticity with MLE
#
###########################################################################
load("food.RData")
attach(food)
OLS<-lm(food_exp~income)
OLS

# Step 1:  create a fuction for the NEGATIVE log likelihood ignoring het
n.ll <- function(b0,b1,sig){
  y <- food_exp
  y.hat <- b0 + b1*income
  ll <-dnorm(y,y.hat,sig,log=T)
  -sum(ll)
}
# Step 2:  Use a Solver to estimate the parameters
ml.est <- mle2(n.ll,start=list(b0=coef(OLS)[1], b1=coef(OLS)[2],                                 sig=summary(OLS)$sigma),
               optimizer='nlm', steptol=1e-16, gradtol=1e-6, stepmax=5000)

summary(ml.est); summary(OLS)
b0_ <- coef(ml.est)[1]
b1_ <- coef(ml.est)[2]
resid.ml.est <- food_exp - b0_ - b1_*income
plot(income,resid.ml.est,main="Residuals against Income",ylab="Food Expense")


# Step 1:  create a fuction for the NEGATIVE log likelihood accounting for het
n.ll.het<-function(b0,b1,a0,a1) {
  y<- food_exp
  y.hat<- b0 + b1*income
  h.hat<- exp(a0 + a1*income) 
  ll<- dnorm(y,y.hat,sqrt(h.hat),log=T)
  sum(-ll)
}
# Step 2:  Use a Solver to estimate the parameters
ml.est.het <- mle2(n.ll.het,start=list(b0=coef(OLS)[1], b1=coef(OLS)[2],
                                       a0=log(summary(OLS)$sigma^2), a1=0),
                   optimizer='nlm', steptol=1e-16, gradtol=1e-6, stepmax=5000)
summary(ml.est.het)
coeftest(ml.est.het)
b0<-coef(ml.est.het)[1]
b1<-coef(ml.est.het)[2]
a0<-coef(ml.est.het)[3]
a1<-coef(ml.est.het)[4]

res<-food_exp - b0 - b1*income
plot(res~income,main="residuals agaist income")
abline(0,0)

std.res<-(food_exp - b0 - b1*income)/sqrt(exp(a0+a1*income))
plot(std.res~income,main="standardized residuals agaist income")
abline(0,0)

# LR test for exponential heteroskedasticity
anova(ml.est,ml.est.het)

detach(food)

###########################################################################
#
#  Modeling Autocorrelated Residuals with MLE
#
###########################################################################
load('bangla.RData')
attach(bangla)

OLS<-lm(log(a)~log(p),data=bangla)
summary(OLS)
plot(resid(OLS),type='b')
abline(0,0,col='royal blue')

# Step 1
neg.ll<-function(b0,b1,sig) {
  y <- log(a)
  y.hat <- b0 + b1*log(p)
  ll <- dnorm(y,mean=y.hat,sd=sig, log=1)
  -sum(ll)
  }
# Step 2
ml.est.1 <- mle2(neg.ll,start=list(b0=mean(log(a)),b1=1, sig=sd(log(a))),
                 optimizer='nlm',
                 steptol=1e-16, gradtol=1e-6, stepmax=5000)

summary(ml.est.1);summary(OLS)
b0 <- coef(ml.est.1)[1]
b1 <- coef(ml.est.1)[2]
resid.ml <- ts(log(a) - b0 - b1*log(p))
sum(resid.ml^2);deviance(OLS)
vcov(OLS)
vcov(ml.est.1)*34/32

#------------------------------------------------------------------------
# Breusch-Godfrey Test
#------------------------------------------------------------------------
BG<-summary(dynlm(resid.ml~log(p)+L(resid.ml,1)+L(resid.ml,2)))$r.squared*32
cat("LM test stat =", BG, "with df=2 and p.value =",pchisq(BG,2,lower.tail=0))

# Step 1: negative log likelihood function to account for auto correlation
neg.ll.AC<-function(b0,b1,rho,sig) {
  y <- log(a)
  x <- log(p)
  n <- length(a)
  error <- rep(0,n)
  for (i in 1:n) {
    error[i] <- y[i] - (b0 + b1*x[i])
  }
  ll <- dnorm(y[-1],b0+b1*x[-1] +rho*error[-n],sig,log=1)
  sum(-ll)
}
# Step 2:
ml.est.AC <- mle2(neg.ll.AC,start=list(b0=mean(log(a)),b1=1, rho = .4, sig=sd(log(a))),
                 optimizer='nlm',
                 steptol=1e-16, gradtol=1e-6, stepmax=5000)

summary(ml.est.AC)

summary(cochrane.orcutt(OLS)) # 'orcutt' package
cochrane.orcutt(OLS)$rho

#------------------------------------------------------------------------
# LR test for autocorrelation with Ho: rho = 0
#------------------------------------------------------------------------
anova(ml.est.1,ml.est.AC)
-2*(logLik(ml.est.1)-logLik(ml.est.AC))

# Step 1: for AR=2 autocorrelation
neg.ll.AC2<-function(b0,b1,rho1,rho2,sig) {
  y <- log(a)
  x <- log(p)
  n <- length(a)
  nm1 <- n-1
  nm2 <- n-2
  error <- rep(0,n)
  for (i in 1:n) {
    error[i] <- y[i] - (b0 + b1*x[i])
  }
  ll <- dnorm(y[-(1:2)],b0+b1*x[-(1:2)] + 
                rho1*error[2:(n-1)] +
                rho2*error[1:(n-2)],sig,log=1)
  sum(-ll)
}
# Step 2:
ml.est.AC2 <- mle2(neg.ll.AC2,
                   start=list(b0=mean(log(a)),b1=1, rho1=.4,rho2=0, sig=sd(log(a))),
                  optimizer='nlm',
                  steptol=1e-16, gradtol=1e-16, stepmax=5000)


ml.est.AC2
anova(ml.est.AC,ml.est.AC2)

detach(bangla)

###########################################################################
#
#  Introuction to Modeling Volatility with ARCH/GARCH Models
#
#  Introduction to working with the 'zoo' package
#
###########################################################################

load('SP500.RData')
attach(SP500)
# zoo makes the data a time series that dynlm() can work with
library('zoo')
sp500<-zoo(data.frame(SP500)) 
 
sp500
n<-length(Close)
n
ret<-diff(log(Close),1)*100 # calculate returns
head(ret)
plot(Date[-1],ret,type='l',
     main='Daily Returns on S&P 500 Index',
     xlab='date',col='red')

plot(Date[-1],ret^2,type='l',
     main='Daily Squared Returns on S&P 500 Index',
     xlab='date',col='forest green')

T<-length(ret)

#------------------------------------------------------------------------
# LM test for ARCH with Ho: 'No ARCH'
#------------------------------------------------------------------------
library('FinTS')
ArchTest(ret,6,demean = 1)
ArchTest(ret,20,demean = 1)

sqd.error<-zoo(ret-mean(ret))^2
no.ARCH <- lm(sqd.error~1)  # ressing against a constant just finds the mean of y
ARCH.6 <- dynlm(sqd.error~L(sqd.error,1)+L(sqd.error,2)+L(sqd.error,3)
                +L(sqd.error,4)+L(sqd.error,5)+L(sqd.error,6))

summary(ARCH.6)                
summary(ARCH.6)$r.squared*(T-6)
ArchTest(ret,6,demean = 1)

ARCH.20 <- dynlm(sqd.error~L(sqd.error,1)+L(sqd.error,2)+L(sqd.error,3)
                +L(sqd.error,4)+L(sqd.error,5)+L(sqd.error,6) + L(sqd.error,7)
                +L(sqd.error,4)+L(sqd.error,5)+L(sqd.error,6) + L(sqd.error,7)
                +L(sqd.error,8)+L(sqd.error,9)+L(sqd.error,10) + L(sqd.error,11)
                +L(sqd.error,12)+L(sqd.error,13)+L(sqd.error,14) + L(sqd.error,15)
                +L(sqd.error,16)+L(sqd.error,17)+L(sqd.error,18) + L(sqd.error,19)
                +L(sqd.error,20))
summary(ARCH.20)$r.squared*(T-20)
ArchTest(ret,20,demean = 1)

#------------------------------------------------------------------------
# Estimating IGARCH
#------------------------------------------------------------------------
# Step 1:
neg.ll.igarch<-function(a,lambda) {
  nobs<-length(ret)
  y<-ret
  u<-y-a
  h<-rep(var(ret),nobs) # initialize the h.hat vector
  for (i in 2:nobs) {
    h[i]<-(1-lambda)*(u[i-1]^2) + lambda*h[i-1]
  }
  ll<-dnorm(u[2:nobs],0,sqrt(h[2:nobs]),log=T)
  -sum(ll)  
}
# Step 2:
neg.ll.igarch(0,.93)
ml.est.igarch<-mle2(neg.ll.igarch,start=list(a=mean(ret),lambda=.9),
              optimizer='nlm')

summary(ml.est.igarch)

#------------------------------------------------------------------------
# Estimating IGARCH using 'rugarch' package
#------------------------------------------------------------------------ 
library('rugarch')
args(ugarchspec)

# Step 1:  Use ugarchspec to specify the negative logLik function
spec <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(2,1)),
                   mean.model=list(armaOrder=c(2,2),include.mean=TRUE), 
                   fixed.pars=list(omega=0))
# Step 2:  Use ugarchfit as the "solver"
igarch.fit <- ugarchfit(spec, data = ret)
igarch.fit

# Output
coef(igarch.fit)
infocriteria(igarch.fit)
persistence(igarch.fit)
residuals(igarch.fit)
vcov(igarch.fit)
coeftest(igarch.fit)
show(igarch.fit)
sigma(igarch.fit);plot(sigma(igarch.fit))
likelihood(igarch.fit)
logLik(ml.est.igarch)
plot(fitted(igarch.fit))

#------------------------------------------------------------------------
# Compare rugarch with our R program
#------------------------------------------------------------------------
a <-coef(ml.est.igarch)[1]
lambda <- coef(ml.est.igarch)[2]
x<-array(c(a,lambda))
x
matrix.ll.igarch <- function(x) {
  neg.ll.igarch(a=x[1],lambda=x[2])
}
matrix.ll.igarch(x)
hess.i<-hessian(matrix.ll.igarch,c(a,lambda),method='Richardson',
              method.args=list(eps=1e-16,d=.01)) # requires 'numDeriv' package
hess.i; 
stderr.i<-sqrt(diag(solve(hess.i)))
igarch.coeftest<-cbind(coef(ml.est.igarch),stderr.i,coef(ml.est.igarch)/stderr.i)
colnames(igarch.coeftest)<-c('coef','stderr','z')
igarch.coeftest
coeftest(igarch.fit)

#------------------------------------------------------------------------
# GARCH accounts for a lot of the excess kurtosis of stock returns
#------------------------------------------------------------------------
a<- coef(ml.est.igarch)[1]
lambda<- coef(ml.est.igarch)[2]

# Have create my own series of variance forecasts
h.igarch<-rep(var(ret[1:60]),T)
for (i in 2:T) {
  h.igarch[i]<-(1-lambda)*(ret[i-1]-a)^2 + lambda*h.igarch[i-1]
}

plot(Date[-1],(ret-a),col='dark gray',type='l', 
     main='Daily returns and 1 StDev bands using IGARCH',
     xlab='date',ylab='returns')
lines(Date[-1],sqrt(h.igarch),col='red',lwd=2)
lines(Date[-1],-sqrt(h.igarch),col='red',lwd=2)
abline(a,0)

qqnorm(scale(ret),pch=20,main="QQ Plot of Raw Returns", xlim=c(-8,8),   
       ylim=c(-8,8))
abline(0,1,col='red',lwd=3)

qqnorm((ret-a)/sqrt(h.igarch),pch=20,main="QQ Plot of IGARCH scaled returns", xlim=c(-5,5),   
       ylim=c(-5,5))
abline(0,1,col='red',lwd=3)

plot(Date[-1],sqrt(h.igarch),type='l',main='IGARCH volatility',
     xlab='date',ylab='daily volatility')

#------------------------------------------------------------------------
# GARCH(1,1)
#------------------------------------------------------------------------
# Step 1:
neg.ll.garch<-function(a,omega,alpha,beta){
    nobs<-length(ret)
    y<-ret
    h.hat<-rep(var(y[1:60]),nobs)
    for (i in 2:T) {
      h.hat[i]<-omega+alpha*(y[i-1]-a)^2 + beta*h.hat[i-1]
    }
    ll<-dnorm(y[2:nobs],a,sqrt(h.hat[2:nobs]),log=1)
    -sum(ll)
    }
# Step 2:
ml.est.garch1.1<-mle2(neg.ll.garch,start=list(a=mean(ret),
                                              omega=.06*var(ret),
                                              alpha=.04,
                                              beta=.9),
                      optimizer='nlm')               

summary(ml.est.garch1.1)

cat("Persistence = ",coef(ml.est.garch1.1)[3]+coef(ml.est.garch1.1)[4])

a<-coef(ml.est.garch1.1)[1]
omega<-coef(ml.est.garch1.1)[2]
alpha<-coef(ml.est.garch1.1)[3]
beta<-coef(ml.est.garch1.1)[4]

h<-rep(var(ret),T)
for (i in 2:T) {
  h[i]<-omega+alpha*(ret[i-1]-a)^2 + beta*h[i-1]
}
h.garch <- h

qqnorm((ret-a)/sqrt(h.garch),pch=20,main="QQ Plot of GARCH(1,1) standardized returns", xlim=c(-5,5),   
       ylim=c(-5,5))
abline(0,1,col='red',lwd=3)

plot(Date[-1],sqrt(h.garch),type='l',main='GARCH(1,1) [IGARCH in red] volatility',
     xlab='date',ylab='daily volatility')
lines(Date[-1],sqrt(h.igarch),col='red')
abline(sd(ret),0)

#------------------------------------------------------------------------
# GARCH(1,1) versus IGARCH
#------------------------------------------------------------------------
AIC(ml.est.igarch,ml.est.garch1.1)
AIC(ml.est.igarch,ml.est.garch1.1,k = log(T)) #BIC
LR <- -2*(logLik(ml.est.igarch)-logLik(ml.est.garch1.1))
cat('LR stat = ',LR,'with m = 2 and pvalue = ', pchisq(LR,2,lower.tail=0))

#------------------------------------------------------------------------
# GARCH(1,1) using 'rugarch' package
#------------------------------------------------------------------------
args(ugarchspec)
# Step 1:
garch.spec <-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                        mean.model = list(armaOrder = c(0, 0),include.mean=TRUE))
# Step 2:
garch.fit <- ugarchfit(garch.spec, data = ret)
garch.fit

coef(garch.fit); coef(ml.est.garch1.1)
infocriteria(garch.fit)
persistence(garch.fit)
residuals(garch.fit)
vcov(garch.fit)
coeftest(garch.fit); coeftest(ml.est.garch1.1)
show(garch.fit)
sigma(garch.fit);plot(sigma(garch.fit))
likelihood(garch.fit);logLik(ml.est.igarch.1)
plot(fitted(igarch.fit))

#------------------------------------------------------------------------
# GARCH(2,1) using 'rugarch' package
#------------------------------------------------------------------------
# Step 1:
garch.spec <-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)),
                        mean.model = list(armaOrder = c(0, 0),include.mean=TRUE))
# Step 2:
garch.fit2.1 <- ugarchfit(garch.spec, data = ret)
garch.fit2.1

#------------------------------------------------------------------------
# LR test with m = 1 shows GARCH(2,1) is better than GARCH(1,1)
#------------------------------------------------------------------------
LRtest <- -2*(likelihood(garch.fit)-likelihood(garch.fit2.1))
cat("LR stat =", LRtest, "df = 1 with pvalue =",pchisq(LRtest,1,lower.tail=0))

#------------------------------------------------------------------------
# Our home made GARCH(2,1) model
#------------------------------------------------------------------------
# Step 1:
neg.ll.g2.1<-function(a,omega,alpha1,alpha2,beta){
  nobs<-length(ret)
  y<-ret
  h.hat<-rep(var(y[1:60]),nobs)
  for (i in 3:nobs) {
    h.hat[i]<-omega +
              alpha1*(y[i-1]-a)^2 + alpha2*(y[i-2]-a)^2 +
              beta*h.hat[i-1]
  }
  ll<-dnorm(y[3:nobs],a,sqrt(h.hat[3:nobs]),log=T)
  -sum(ll)
}
# Step 2:
ml.est.garch2.1<-mle2(neg.ll.g2.1,start=list(a=mean(ret),
                                              omega=.03*var(ret),
                                              alpha1=.04, alpha2 = .04,
                                              beta=.8),
                      optimizer='nlm',steptol=1e-16, gradtol=1e-6, stepmax=5000)

coeftest(ml.est.garch2.1)
coeftest(garch.fit2.1)
logLik(ml.est.garch2.1);likelihood(garch.fit2.1)

a <- coef(ml.est.garch2.1)[1]
omega<- coef(ml.est.garch2.1)[2]
alpha1<- coef(ml.est.garch2.1)[3]
alpha2<- coef(ml.est.garch2.1)[4]
beta<- coef(ml.est.garch2.1)[5]

#------------------------------------------------------------------------
# Extracting GARCH(2,1) volatility predictions 
#------------------------------------------------------------------------
# Create a series of variance estimates
h.21<-rep(var(ret[1:60]),T)
for (i in 3:T) {
  h.21[i]<-omega +
    alpha1*(ret[i-1]-a)^2 + alpha2*(ret[i-2]-a)^2 +
    beta*h.21[i-1]
}

plot(Date[-1],sqrt(h.21),type='l',main='GARCH(1,1) volatility',
     xlab='date',ylab='daily volatility')
abline(sd(ret),0)

qqnorm((ret-a)/sqrt(h.21),pch=20,main="QQ Plot of GARCH(2,1) standardized returns", xlim=c(-5,5),   
       ylim=c(-5,5))
abline(0,1,col='red',lwd=3)

#------------------------------------------------------------------------
# GJR-GARCH(1,1)--uses a dummy variable is the variance estimate for
#                 negative returns
#
# We will see that volatility is higher following negative returns.
#------------------------------------------------------------------------
# Step 1:
h.hat<-rep(var(y[1:60]),nobs)
neg.ll.GJR<- function(a, omega, alpha, delta, beta) {
  nobs<-length(ret)
  y<-ret
  d<- ifelse(y < 0,1,0)
    for (i in 2:nobs) {
      h.hat[i]<-omega + (alpha+delta*d[i-1])*(y[i-1]-a)^2 + beta*h.hat[i-1] 
    }
  ll<-dnorm(y[2:nobs],a,sqrt(h.hat[2:nobs]),log=1)
            -sum(ll)
}
neg.ll.GJR(mean(ret),.0125,.03,.9,.03)
mean(ret)
# Step 2:
ml.est.gjr.1.1<-mle2(neg.ll.GJR,start=list(a=.005,
                                             omega=.01252,
                                             alpha=.02,
                                             delta=.10,
                                             beta= .91),
                      optimizer='nlm')
                    

a<-coef(ml.est.gjr.1.1)[1]
omega<-coef(ml.est.gjr.1.1)[2]
alpha<-coef(ml.est.gjr.1.1)[3]
delta<-coef(ml.est.gjr.1.1)[4]
beta<-coef(ml.est.gjr.1.1)[5]
# Step 3:
ml.est.gjr.1.1<-mle2(neg.ll.GJR,start=list(a=a,
                                           omega=omega,
                                           alpha=alpha,
                                           delta=delta,
                                           beta= beta),
                     optimizer='nlm')



coeftest(ml.est.gjr.1.1)


#------------------------------------------------------------------------
# Estimating z statistics using the 'hessian' package 
#------------------------------------------------------------------------
a <- coef(ml.est.gjr.1.1)[1]
omega <- coef(ml.est.gjr.1.1)[2]
alpha <- coef(ml.est.gjr.1.1)[3]
delta <- coef(ml.est.gjr.1.1)[4]
beta <- coef(ml.est.gjr.1.1)[5]


x<-array(c(a,omega,alpha,delta,beta))
x
matrix.ll.gjr <- function(x) {
  neg.ll.GJR(a=x[1],omega=x[2],alpha=x[3], delta=x[4], beta=x[5])
}
matrix.ll.gjr(x)

library('numDeriv')
hess.gjr<-hessian(matrix.ll.gjr,c(a,omega,alpha,delta,beta),method='Richardson',
                method.args=list(eps=1e-16,d=.01)) # requires 'numDeriv' package
hess.gjr; 
stderr.gjr<-sqrt(diag(solve(hess.gjr)))
gjr.coeftest<-cbind(coef(ml.est.gjr.1.1),stderr.gjr,coef(ml.est.gjr.1.1)/stderr.gjr)
colnames(gjr.coeftest)<-c('coef','stderr','z')
gjr.coeftest

#------------------------------------------------------------------------
# Extracting GJR volatility predictions 
#------------------------------------------------------------------------
h.gjr<-rep(var(ret[1:60]),T)
for (i in 2:T) {
  h.gjr[i]<-omega +
    alpha*(ret[i-1]-a)^2 +
    beta*h.gjr[i-1] +
    delta*ifelse(ret[i-1] < 0,1,0)*(ret[i-1]-a)^2
}

plot(Date[-1],sqrt(h.gjr),type='l',main='GJR-GARCH(1,1) volatility',
     xlab='date',ylab='daily volatility')
abline(sd(ret),0)

qqnorm((ret-a)/sqrt(h.gjr),pch=20,main="QQ Plot of GJR GARCH(1,1) standardized returns", xlim=c(-5,5),   
       ylim=c(-5,5))
abline(0,1,col='red',lwd=3)
LR<--2*(likelihood(gjr.fit)-logLik(ml.est.gjr.1.1))
pv<-pchisq(LR,5,lower.tail=0)
cat('LR =',LR,'with df = 5 and pvalue =',pv )

#------------------------------------------------------------------------
# GJR estimation with the 'rugarch' package 
#------------------------------------------------------------------------
args(ugarchfit)
# Step 1:
gjr.spec <- ugarchspec(variance.model=list(model='gjrGARCH',garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(0,0),include.mean=TRUE))
# Step 2:
gjr.fit <- ugarchfit(gjr.spec,ret)
coeftest(gjr.fit);gjr.coeftest
logLik(ml.est.gjr.1.1);likelihood(gjr.fit)


detach(SP500)
