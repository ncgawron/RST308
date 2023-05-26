##
##you need to install epitools
##install.packages("epitools")
library(epitools)
modernadata = matrix(c(13888,14123,185,11),nrow=2,ncol=2)
##orientation of the data matters
##first row is the placebo (unexposed group), second row is exposed group
##first column is people without the outcome, and column is those with the outcome.
##always perform a sanity check. Calculate it by hand.

##Risk ratio calculations
fullres = riskratio.wald(modernadata)
RRest = riskratio.wald(modernadata)$measure[2]
RRciL = riskratio.wald(modernadata)$measure[4]
RRciU = riskratio.wald(modernadata)$measure[6]

##vaccine effectiveness calculations
VEest = 1-RRest
VEci95 = c(1-RRciU,1-RRciL)

##simulation
##truth p1 = 11/14134 p0 = 185/14073
repl=1000
modernarep = rbinom(n=repl,size=14134,p=11/14134)
placeborep = rbinom(n=repl,size=14073,p=185/14073)


##to check that coverage is good
sVEci95 =NULL
sVEest = rep(0,repl)
for(i in 1:rep)
{
  simuldata = matrix(c(14073-placeborep[i],14134-modernarep[i],placeborep[i],modernarep[i]),nrow=2,ncol=2)
  sfullres = riskratio.wald(simuldata)
  sRRest = riskratio.wald(simuldata)$measure[2]
  sRRciL = riskratio.wald(simuldata)$measure[4]
  sRRciU = riskratio.wald(simuldata)$measure[6]
  
  sVEest[i] = 1-sRRest
  sVEci95 = rbind(sVEci95,c(1-sRRciU,1-sRRciL))
}

##true estimate is VE=1-(11/14134)/(185/14073)
## how many confidence intervals cover the true parameter?
truth = 1-(11/14134)/(185/14073)
empbias = sVEest-truth
summary(empbias)
sum(truth>=sVEci95[,1]&truth<=sVEci95[,2])/repl

