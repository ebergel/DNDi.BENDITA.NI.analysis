
library(epiR)
library(ratesci)
library(PropCIs)



## Outcome variable (FUS) as columns:
dat <- matrix(c(13,2163,5,3349), nrow = 2, byrow = TRUE)
rownames(dat) <- c("DF+", "DF-"); colnames(dat) <- c("FUS+", "FUS-"); dat
epi.2by2(dat = as.table(dat), method = "cross.sectional",
         conf.level = 0.95, units = 100, outcome = "as.columns")

 

#Binomial RD, MOVER-J method:
ratesci::moverci(x1 = 13, n1 = 2176, x2 = 5, n2 = 3354, contrast = 'RD')
ratesci::moverci(x1 = 13, n1 = 2176, x2 = 5, n2 = 3354, contrast = 'RR')
ratesci::moverci(x1 = 13, n1 = 2176, x2 = 5, n2 = 3354, contrast = 'OR')

 

#Binomial RD, SCAS method:
ratesci::scoreci(x1 = 100, n1 = 1000, x2 = 200, n2 = 1000, contrast = 'RD')
res.02 <- ratesci::scoreci(x1 = 100, n1 = 1000, x2 = 200, n2 = 1000, contrast = 'RR')
estimates.02<- res.02$estimates

PropCIs::diffscoreci(x1 = 13, n1 = 2176, x2 = 5, n2 = 3354,  conf.level = 0.95)
PropCIs::riskscoreci(x1 = 13, n1 = 2176, x2 = 5, n2 = 3354,  conf.level = 0.95)
PropCIs::diffscoreci(x1 = 100, n1 = 1000, x2 = 200, n2 = 1000,  conf.level = 0.95)

#Binomial RD, Newcombe method:
moverci(x1 = 5, n1 = 56, x2 = 0, n2 = 29, type = "wilson")
