
library(epiR)
library(ratesci)
library(PropCIs)
library(forestplot)


# STD vs all.LOW
ratesci::moverci(x2 = 74, n2 = 87, x1 = 25, n1 = 28, type = "wilson",contrast = 'RD')
ratesci::moverci(x2 = 74, n2 = 87, x1 = 25, n1 = 28, type = "wilson",contrast = 'RR')

dat <- matrix(c(74,13,25,3), nrow = 2, byrow = TRUE)
rownames(dat) <- c("New", "STD"); colnames(dat) <- c("PCR(-)", "PCR(+)"); dat
res.01 <- epiR::epi.2by2(dat = as.table(dat), method = "cross.sectional",
                         conf.level = 0.95, units = 100, outcome = "as.columns")
RR.95CI <- res.01$res$RR.crude.score
RR.95CI 
res.01

# FOR comparison with table - STS vs PLACEBO
ratesci::moverci(x1 = 1, n1 = 30, x2 = 25, n2 = 28, type = "wilson",contrast = 'RD')
ratesci::moverci(x1 = 1, n1 = 30, x2 = 25, n2 = 28, type = "wilson",contrast = 'RR')



#---------------------------------
# WORKING WITH THE NEGATIVE OUTCOME - FAILURE


# STD vs all.LOW
ratesci::moverci(x1 = 13, n1 = 87, x2 = 3, n2 = 28, type = "wilson",contrast = 'RD')
ratesci::moverci(x1 = 13, n1 = 87, x2 = 3, n2 = 28, contrast = 'RD')
ratesci::scoreci(x1 = 13, n1 = 87, x2 = 3, n2 = 28, contrast = 'RD')
ratesci::scoreci(x1 = 13, n1 = 87, x2 = 3, n2 = 28, contrast = 'RD', cc = TRUE)

ratesci::moverci(x1 = 13, n1 = 87, x2 = 3, n2 = 28, type = "wilson",contrast = 'RR')

dat <- matrix(c(13,74,3,25), nrow = 2, byrow = TRUE)
rownames(dat) <- c("New", "STD"); colnames(dat) <- c("PCR(-)", "PCR(+)"); dat
res.01 <- epiR::epi.2by2(dat = as.table(dat), method = "cross.sectional",
                         conf.level = 0.95, units = 100, outcome = "as.columns")
RR.95CI <- res.01$res$RR.crude.score
RR.95CI 
res.01

# FOR comparison with table - STS vs PLACEBO
ratesci::moverci(x1 = 1, n1 = 30, x2 = 25, n2 = 28, type = "wilson",contrast = 'RD')
ratesci::moverci(x1 = 1, n1 = 30, x2 = 25, n2 = 28, type = "wilson",contrast = 'RR')







# Cochrane data from the 'rmeta'-package
cochrane_from_rmeta <- 
  structure(list(
    mean  = c(NA, NA, 0.578, 0.165, 0.246, 0.700, 0.348, 0.139, 1.017, NA, 0.531), 
    lower = c(NA, NA, 0.372, 0.018, 0.072, 0.333, 0.083, 0.016, 0.365, NA, 0.386),
    upper = c(NA, NA, 0.898, 1.517, 0.833, 1.474, 1.455, 1.209, 2.831, NA, 0.731)),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -11L), 
    class = "data.frame")

tabletext<-cbind(
  c("", "Study", "Auckland", "Block", 
    "Doran", "Gamsu", "Morrison", "Papageorgiou", 
    "Tauesch", NA, "Summary"),
  c("Deaths", "(steroid)", "36", "1", 
    "4", "14", "3", "1", 
    "8", NA, NA),
  c("Deaths", "(placebo)", "60", "5", 
    "11", "20", "7", "7", 
    "10", NA, NA),
  c("", "OR", "0.58", "0.16", 
    "0.25", "0.70", "0.35", "0.14", 
    "1.02", NA, "0.53"))

forestplot(tabletext, 
           cochrane_from_rmeta,new_page = TRUE,
           is.summary=c(TRUE,TRUE,rep(FALSE,8),TRUE),
           clip=c(0.1,2.5), 
           xlog=TRUE, 
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"))



s <- 100
l1 <- -0.1331049 *s
h1 <- 0.1560159  *s
d1 <- 0.04228243 *s

 
l1.r <- round(l1, 1)
h1.r <- round(h1, 1)
d1.r <- round(d1, 1)
 

tabletext<-cbind(
  c(NA, NA   , "STD vs Any Low   ", "STD vs LOW", "Doran" ),
  c("% (n)", "STD ", "89.2 (5)"      , "1"    , "4"     ),
  c(NA, "NEW", "85.1"      , "5"    , "11"    ),
  c(NA, "Difference"       , d1.r    , d1.r , d1.r  )) 




# Cochrane data from the 'rmeta'-package
cochrane_from_rmeta <- 
  structure(list(
    mean  = c(NA, NA, d1, d1, d1  ), 
    lower = c(NA, NA, l1, l1, l1  ),
    upper = c(NA, NA, h1, h1, h1  )),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -5L), 
    class = "data.frame")



forestplot(labeltext = tabletext, 
           mean = cochrane_from_rmeta,
           new_page = TRUE,
           is.summary=c(FALSE,FALSE,rep(FALSE,4)),
          # clip=c(0.1,2.5), 
           xlog=FALSE, 
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue")
          ,xticks =  c(-15, -10, -5, 0, 5, 10, 15, 20, 25)
          , ci.vertices = TRUE
          , xlab = 'Rate Difference'
          , grid = structure(c(10, 20), 
                             gp = gpar(lty = 1, col = "#c41019"))
           ,graph.pos = 4
          , txt_gp = fpTxtGp(label = list(gpar(fontfamily = "",  cex= 0.8),
                                          gpar(fontfamily = "", cex= 0.9, col = "#660000"),
                                          gpar(fontfamily = "",  cex= 0.9),
                                          gpar(fontfamily = "",  cex= 1)
                                          ),
                             ticks = gpar(fontfamily = "", cex= 0.7),
                             xlab  = gpar(fontfamily = "", cex = 0.8)) ,  
          graphwidth = unit(60, 'mm'),
          mar =  unit(c(40,1,50,1), 'mm')
          
          #b, l, t, r
          )


 
forestplot(tabletext, 
           legend_args = fpLegend(pos = list(x=.85, y=0.25), 
                                  gp=gpar(col="#CCCCCC", fill="#F9F9F9")),
           legend = c("Sweden", "Denmark"),
           fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           boxsize = .25, # We set the box size to better visualize the type
           line.margin = .1, # We need to add this to avoid crowding
           mean = cbind(HRQoL$Sweden[, "coef"], HRQoL$Denmark[, "coef"]),
           lower = cbind(HRQoL$Sweden[, "lower"], HRQoL$Denmark[, "lower"]),
           upper = cbind(HRQoL$Sweden[, "upper"], HRQoL$Denmark[, "upper"]),
           clip =c(-.125, 0.075),
           col=fpColors(box=c("blue", "darkred")),
           xlab="EQ-5D index")


