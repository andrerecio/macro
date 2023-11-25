
####################################
####################################
###### VECTOR AUTOREGRESSIONS ######
####################################
####################################

library("svars")
var2 = VAR(USA, lag.max=4, ic="SC")
k = ncol(USA)
summary(var2)

# IMPULSE RESPONSE FUNCTIONS
plot(irf(var2, ortho=T,n.ahead=50,cumulative=F,runs=50,ci=0.9),col=1)
# CUMULATIVE IMPULSE RESPONSE FUNCTIONS
plot(irf(var2, ortho=T,n.ahead=50,cumulative=T,runs=50,ci=0.9),col=1)
# FORECAST ERROR VARIANCE DECOMPOSITION
plot(fevd(var2,ortho=T,n.ahead=50,cumulative=F,runs=50,ci=0.9))

### STRUCTURAL VAR MONEY MARKET (SUPPLY AND DEMAND SHOCK)
amat = diag(k)
diag(amat) = NA
amat[2, 1] = NA
amat[3, 1] = NA
amat[3, 2] = NA
svar2 = SVAR(x = var2, estmethod = "scoring", Amat = NULL, Bmat = amat,
             max.iter = 100, maxls = 1000, conv.crit = 1.0e-8) 
summary(svar2) # Interest is only influencing itself in t whereas Real Money Demand is influenced by both in t
plot(irf(svar2, ortho=T,n.ahead=50,cumulative=F,runs=50,ci=0.9),col=1)
plot(irf(svar2, ortho=T,n.ahead=50,cumulative=T,runs=50,ci=0.9),col=1)
plot(fevd(svar2,ortho=T,n.ahead=50,cumulative=F,runs=50,ci=0.9))

### BLANCHARD QUAH STRUCTURAL VAR
bq2 = BQ(var2)
plot(irf(bq2, ortho=T,n.ahead=50,cumulative=F,runs=50,ci=0.9),col=1)
plot(irf(bq2, ortho=T,n.ahead=50,cumulative=T,runs=50,ci=0.9),col=1)
plot(fevd(bq2,ortho=T,n.ahead=50,cumulative=F,runs=50,ci=0.9))

### END
