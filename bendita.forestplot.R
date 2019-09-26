
library(epiR)
library(ratesci)
library(PropCIs)
library(forestplot)
library(EQUIVNONINF)
library(BiasedUrn)


#pvalue for non inferiority: no lo uso, es muy dificil de interpretar
if (FALSE) {

#TESTING STATISTICAL HYPOTHESES OF EQUIVALENCE AND NONINFERIORITY
#SECOND EDITION STEFAN WELLEK
# chapter 661

# robhypr(213,106,195,97,.50)   sas
# pFNCHypergeo(12, 25, 32, 20, 2.5) ex R cran

 # example from book:  da bien!!!
 x<-BiasedUrn::pFNCHypergeo(x =97 , m1 =106 , m2 =107 , n =195  , odds = 0.5, lower.tail = TRUE)

 BiasedUrn::pFNCHypergeo(x =24 , m1 =29 , m2 =87 , n =96  , odds = 0.58, lower.tail = FALSE)
 BiasedUrn::pFNCHypergeo(x =24 , m1 =29 , m2 =30 , n =48  , odds = 0.58, lower.tail = FALSE)

# non inferiority margin 83 to 73, OR = 0.58
 # S v 123
 BiasedUrn::pFNCHypergeo(x =72 , m1 =96 , m2 =20 , n =87  , odds = 0.58, lower.tail = FALSE)
 # S v 3
 BiasedUrn::pFNCHypergeo(x =24 , m1 =48 , m2 =11 , n =30  , odds = 0.58, lower.tail = FALSE)
 # S v 23
 BiasedUrn::pFNCHypergeo(x =47 , m1 =71 , m2 =17 , n =59  , odds = 0.58, lower.tail = FALSE)
 # s v 1
 BiasedUrn::pFNCHypergeo(x =25 , m1 =49 , m2 =8  , n =28  , odds = 0.58, lower.tail = FALSE)
 
 
 # non inferiority margin 83 to 33, OR = 0.36 
 BiasedUrn::pFNCHypergeo(x =72 , m1 =96 , m2 =20 , n =87  , odds = 0.36, lower.tail = FALSE)
 BiasedUrn::pFNCHypergeo(x =24 , m1 =48 , m2 =11 , n =30  , odds = 0.36, lower.tail = FALSE) 
 
}

ci <- function(a,b,c){ paste(a, ' [',b, ',' , c, ']', sep = '' )}
nDec <- function(x, k) trimws(format(round(x, k), nsmall=k)) 
rn <- function(a,b){paste(  a ,  ' (',  b , ')', sep = '')}
 
N.STD <- 29
N.NEW <- 87
n.STD <- 24
n.NEW <- 72
niODDS <- 0.58

 

fx <- function(N.STD, N.NEW, n.STD, n.NEW, isNI = FALSE, niODDS = 0.65){

    
    l.N <- list('N.STD' = N.STD, 'N.NEW' = N.NEW, 'n.STD' = n.STD, 'n.NEW' = n.NEW  )    
 
    R.STD <- n.STD / N.STD * 100
    R.NEW <- n.NEW / N.NEW * 100 
    
    l.R     <- list('R.STD' = nDec(R.STD,1), 'R.NEW' = nDec(R.NEW,1)  ) 
    
    # epiR for pValue
    dat <- matrix(c(n.NEW, N.NEW-n.NEW, n.STD, N.STD-n.STD), nrow = 2, byrow = TRUE)
    rownames(dat) <- c("New", "STD"); colnames(dat) <- c("PCR(-)", "PCR(+)"); dat
    t2x2 <- epiR::epi.2by2(dat = as.table(dat), method = "cross.sectional",
                             conf.level = 0.95, units = 100, outcome = "as.columns")
    
    pval <- t2x2$res$chisq.crude$p.value 
     
    if (isNI){
      
      # p value for non inferiority
      
      #BiasedUrn::pFNCHypergeo(x =72 , m1 =96 , m2 =20 , n =87  , odds = 0.58, lower.tail = FALSE)
      
      N <- N.STD + N.NEW
      pval <- BiasedUrn::pFNCHypergeo(  x  = n.NEW , 
                                        m1 = n.STD + n.NEW , 
                                        m2 = N - (n.STD + n.NEW)        , 
                                        n  = N.NEW  , 
                                        odds = niODDS, 
                                        lower.tail = FALSE)
    }
    
    
    if (pval < 0.001){ 
      pval <- '<0.001' 
    } else {
      pval <- nDec(pval, 3)
    }
    
    # ratesci  for RD ci
    RD <- ratesci::moverci(x1 = n.STD, n1 = N.STD, x2 = n.NEW, n2 = N.NEW, type = "wilson",contrast = 'RD')
 
    
    L  <- RD[1] * 100
    H  <- RD[3] * 100
    RD <- RD[2] * 100
    
    L.r  <- nDec(L, 1)
    H.r  <- nDec(H, 1)
    RD.r <- nDec(RD, 1)
    
    l.RD <- list('L' = L,'H'= H,'RD'=RD, 'L.r' = L.r,'H.r'= H.r,'RD.r'=RD.r )
    
    # store in list container
    s <- list('RD' = l.RD , 'N' = l.N , 'R'= l.R,  'pval' = pval )
    
    return(s)

}

 

# fx Generate Forest 
fx.forest <- function(isNI = TRUE){
  
   showStats <- c(NA       , NA    ,
    ci( r$Sv123$RD$RD.r, r$Sv123$RD$L.r, r$Sv123$RD$H.r ),
    ci( r$Sv23$RD$RD.r, r$Sv23$RD$L.r, r$Sv23$RD$H.r ),
    ci( r$Sv1$RD$RD.r, r$Sv1$RD$L.r, r$Sv1$RD$H.r ),
    ci( r$Sv2$RD$RD.r, r$Sv2$RD$L.r, r$Sv2$RD$H.r ),
    ci( r$Sv3$RD$RD.r, r$Sv3$RD$L.r, r$Sv3$RD$H.r ) ) 
   
   
  if (isNI) { 
  pvals <-    c(NA  , NA ,  NA   , NA   ,   NA   ,    NA  , NA )
    
  } else{
    
    pvals <- c(NA       , "p value"    ,  r$Sv123$pval   
      ,  r$Sv23$pval   
      ,  r$Sv1$pval   
      ,  r$Sv2$pval   
      ,  r$Sv3$pval )   
  }   
  
  
  rates <- 
  c('Rate'       , "Difference"   
    , r$Sv123$RD$RD.r 
    , r$Sv23$RD$RD.r  
    , r$Sv1$RD$RD.r
    , r$Sv2$RD$RD.r 
    , r$Sv3$RD$RD.r)
  
 
   
      tabletext  <-  cbind(
        c(NA, NA   , "(1) 300mg 4w, 300mg 2w, 150mg 4w", 
          "(2) 300mg 2w, 150mg 4w", 
          "(3) 300mg 4w" , 
          '(4) 300mg 2w', 
          '(5) 150mg 4w '),
        
        c('STD', "% (n)"             , rn( r$Sv123$R$R.STD,r$Sv123$N$N.STD)
          , rn(r$Sv23$R$R.STD  ,r$Sv23$N$N.STD)  
          , rn(r$Sv1$R$R.STD   ,r$Sv1$N$N.STD)
          , rn(r$Sv2$R$R.STD   ,r$Sv2$N$N.STD)
          , rn(r$Sv3$R$R.STD   ,r$Sv3$N$N.STD) ),
        
        c("NEW" , "% (n)"               ,rn( r$Sv123$R$R.NEW     ,r$Sv123$N$N.NEW)
          ,rn( r$Sv23$R$R.NEW 	,r$Sv23$N$N.NEW)  
          ,rn( r$Sv1$R$R.NEW    	,r$Sv1$N$N.NEW)
          ,rn( r$Sv2$R$R.NEW 	,r$Sv2$N$N.NEW)
          ,rn( r$Sv3$R$R.NEW  	,r$Sv3$N$N.NEW) ),
        
        showStats, pvals
        
        )
 
      
      
      # Cochrane data from the 'rmeta'-package
      cochrane_from_rmeta <- 
        structure(list(
          mean  = c(NA, NA, r$Sv123$RD$RD, r$Sv23$RD$RD , r$Sv1$RD$RD, r$Sv2$RD$RD , r$Sv3$RD$RD  ), 
          lower = c(NA, NA, r$Sv123$RD$L , r$Sv23$RD$L  , r$Sv1$RD$L , r$Sv2$RD$L  , r$Sv3$RD$L),
          upper = c(NA, NA, r$Sv123$RD$H , r$Sv23$RD$H  , r$Sv1$RD$H , r$Sv2$RD$H  , r$Sv3$RD$H)),
          .Names = c("mean", "lower", "upper"), 
          row.names = c(NA, -8L), 
          class = "data.frame")
      
      forestplot(labeltext = tabletext, 
                 mean = cochrane_from_rmeta,
                 new_page = TRUE,
                 is.summary=c(FALSE,FALSE,rep(FALSE,4)),
                 # clip=c(0.1,2.5), 
                 xlog=FALSE, 
                 col=fpColors(box="royalblue",line="darkblue", summary="royalblue")
                 ,xticks =  c(-25,-20,-15, -10, -5, 0, 5, 10, 15, 20, 25)
                 , ci.vertices = TRUE
                 , xlab = 'Rate Difference'
                  #, grid = structure(c(10, 20), gp = gpar(lty = 1, col = "#c41019"))
                  ,graph.pos = 4
                 , txt_gp = fpTxtGp(label = list(gpar(fontfamily = "",  cex= 0.8),
                                                 gpar(fontfamily = "", cex= 0.8), #, col = "#660000"),
                                                 gpar(fontfamily = "",  cex= 0.8),
                                                 gpar(fontfamily = "",  cex= 0.8)
                 ),
                 ticks = gpar(fontfamily = "", cex= 0.7),
                 xlab  = gpar(fontfamily = "", cex = 0.8)) ,  
                 graphwidth = unit(60, 'mm'),
                 mar =  unit(c(40,10,50,10), 'mm')
                 
                 #b, l, t, r
      )
}

 


fx.runAnalysis <- function(isNI = TRUE){
  
  
      if (isNI) { 
        
        # ITT OUTCOME PCR AT 12 month data
        
        l.N <- list( 'tSTD'     = 29, 
                     't300mg4w' = 28,
                     't300mg2w' = 29, 
                     't150mg2w' = 30)
        
        l.n <- list( 'tSTD'     = 24, 
                     't300mg4w' = 25,
                     't300mg2w' = 23, 
                     't150mg2w' = 24) 
        
        l.d <- list(N = l.N, n = l.n)
        
      } else {
        
        # SAFETY - any TEAE leading to discontinuation
        
        l.N <- list( 'tSTD'     = 30, 
                     't300mg4w' = 30,
                     't300mg2w' = 30, 
                     't150mg2w' = 30)
        
        l.n <- list( 'tSTD'     = 6, 
                     't300mg4w' = 1,
                     't300mg2w' = 0, 
                     't150mg2w' = 1)
        
        l.d <- list(N = l.N, n = l.n) 
        
      }
        
        
        
      # [S] STD vs ( [1] 300mg4w, [2]300mg2w, [3]150mg4w )
      
      l.Sv123 <- fx(  N.STD = l.d$N$tSTD , 
                      n.STD = l.d$n$tSTD ,    
                      N.NEW = l.d$N$t300mg4w +  l.d$N$t300mg2w + l.d$N$t150mg2w,
                      n.NEW = l.d$n$t300mg4w +  l.d$n$t300mg2w + l.d$n$t150mg2w )
        
       
      
      # [S] STD vs (   [2]300mg2w, [3]150mg4w )
      
      l.Sv23 <- fx(  N.STD = l.d$N$tSTD ,
                     n.STD = l.d$n$tSTD ,
                     N.NEW = l.d$N$t300mg2w + l.d$N$t150mg2w,            
                     n.NEW = l.d$n$t300mg2w + l.d$n$t150mg2w )
       
      # [S] STD vs ( [1] 300mg4w  )
      
      l.Sv1 <- fx(  N.STD = l.d$N$tSTD ,
                    n.STD = l.d$n$tSTD ,            
                    N.NEW = l.d$N$t300mg4w  ,
                    n.NEW = l.d$n$t300mg4w   )
      
      
      # [S] STD vs (   [2]300mg2w )
      
      l.Sv2  <- fx(   N.STD =    l.d$N$tSTD ,
                      n.STD =    l.d$n$tSTD ,            
                      N.NEW =    l.d$N$t300mg2w  ,
                      n.NEW =    l.d$n$t300mg2w   )
      
      # [S] STD vs (  [3]150mg4w )
      
      l.Sv3 <- fx(    N.STD =  l.d$N$tSTD ,
                      n.STD =  l.d$n$tSTD ,            
                      N.NEW =  l.d$N$t150mg2w,
                      n.NEW =  l.d$n$t150mg2w )
      
      
      # set Container
      
      r <-    list('Sv123'  = l.Sv123,
                   'Sv23'   = l.Sv23,
                   'Sv1'    = l.Sv1 ,
                   'Sv2'    = l.Sv2 ,
                   'Sv3'    = l.Sv3  )  

}


isNI <- FALSE
fx.runAnalysis(isNI = isNI)
fx.forest(isNI = isNI)



