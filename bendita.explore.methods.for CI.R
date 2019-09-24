
library(epiR)
library(ratesci)
library(PropCIs)


#this is the chose one WILSON
  
ratesci::moverci(x1 = 74, n1 = 87, x2 = 25, n2 = 28, type = "wilson",contrast = 'RR')

  # este puede calcular RD
ratesci::moverci(x1 = 74, n1 = 87, x2 = 25, n2 = 28, type = "wilson",contrast = 'RD')

# que es similar a este:

## Outcome variable (FUS) as columns:
dat <- matrix(c(74,13,25,3), nrow = 2, byrow = TRUE)
rownames(dat) <- c("New", "STD"); colnames(dat) <- c("PCR(-)", "PCR(+)"); dat
res.01 <- epiR::epi.2by2(dat = as.table(dat), method = "cross.sectional",
                         conf.level = 0.95, units = 100, outcome = "as.columns")

RR.95CI <- res.01$res$RR.crude.score
RR.95CI 


#----------------------------------------------------------------------


# get data

# STD vs LOW

## Outcome variable (FUS) as columns:
dat <- matrix(c(74,13,25,3), nrow = 2, byrow = TRUE)
rownames(dat) <- c("New", "STD"); colnames(dat) <- c("PCR(-)", "PCR(+)"); dat
res.01 <- epiR::epi.2by2(dat = as.table(dat), method = "cross.sectional",
         conf.level = 0.95, units = 100, outcome = "as.columns")

RR.95CI <- res.01$res$RR.crude.score
RR.95CI 

#scoreci(x1 = c(12,19,5), n1 = c(16,29,56), x2 = c(1,22,0), n2 = c(16,30,29))
res.02 <- ratesci::scoreci(x1 = 74, n1 = 87, x2 = 25, n2 = 28, contrast = 'RR')
ratesci::moverci(x1 = 74, n1 = 87, x2 = 25, n2 = 28, type = "wilson",contrast = 'RR')
estimates.02<- res.02$estimates
estimates.02


res.03 <-PropCIs::riskscoreci(x1 = 74, n1 = 87, x2 = 25, n2 = 28,  conf.level = 0.95)
res.03$conf.int

RR.95CI
estimates.02
res.03$conf.int


# STD vs  PLACEBO
## Outcome variable (FUS) as columns:
dat <- matrix(c(1,29, 25,3), nrow = 2, byrow = TRUE)
rownames(dat) <- c("Placebo", "STD"); colnames(dat) <- c("PCR(-)", "PCR(+)"); dat
res.01 <- epiR::epi.2by2(dat = as.table(dat), method = "cross.sectional",
                         conf.level = 0.95, units = 100, outcome = "as.columns")
res.01


# FOR RD

 #rateci

   #MOVER
   ratesci::moverci(x1 = 1, n1 = 30, x2 = 25, n2 = 28, type = "wilson",contrast = 'RD')
   ratesci::moverci(x1 = 1, n1 = 30, x2 = 25, n2 = 28,  contrast = 'RD')
   
   #SCORE
   ratesci::scoreci(x1 = 1, n1 = 30, x2 = 25, n2 = 28, contrast = 'RD')
   ratesci::scoreci(x1 = 1, n1 = 30, x2 = 25, n2 = 28, contrast = 'RD', cc = TRUE)
   
   

 #PropCIs
   #SCORE
   PropCIs::diffscoreci(x1 = 1, n1 = 30, x2 = 25, n2 = 28,  conf.level = 0.95)


# FOR RR
ratesci::moverci(x1 = 1, n1 = 30, x2 = 25, n2 = 28, type = "wilson",contrast = 'RR' )
ratesci::moverci(x1 = 1, n1 = 30, x2 = 25, n2 = 28,  contrast = 'RR')
ratesci::moverci(x1 = 1, n1 = 30, x2 = 25, n2 = 28, cc = TRUE , contrast = 'RR')


ratesci::scoreci(x1 = 1, n1 = 30, x2 = 25, n2 = 28,   contrast = 'RR')
ratesci::scoreci(x1 = 1, n1 = 30, x2 = 25, n2 = 28, cc = TRUE,  contrast = 'RR')


dat <- matrix(c(1,29, 25,3), nrow = 2, byrow = TRUE)
rownames(dat) <- c("Placebo", "STD"); colnames(dat) <- c("PCR(-)", "PCR(+)"); dat
res.01 <- epiR::epi.2by2(dat = as.table(dat), method = "cross.sectional",
                         conf.level = 0.95, units = 100, outcome = "as.columns")
 
res.01$res$RR.crude.wald
res.01$res$RR.crude.score



