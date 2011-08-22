######################################################
### Define Directories and Load Functions
rm(list=ls())
source('~/Dropbox/programming/R/StandardPackages.R')

######################################################
### Set Seed
set.seed(1234)

######################################################
### Define CP Data Object
setClass("HS1999",
         representation(data="data.frame",
                        e="matrix",
                        se="numeric",
                        N="numeric",
                        T="numeric",
                        z="numeric",
                        j="numeric",
                        k="numeric",
                        gamma="numeric",
                        Q="numeric",
                        tet0="numeric",
                        tet="numeric"))

######################################################
### Load Data
setMethod("initialize",
          "HS1999",
          function(.Object,Z,J) {
            
            ## Define Baseline Parameters
            .Object@N      <- 1
            .Object@Q      <- 1
            .Object@T      <- 500
            .Object@z      <- Z
            .Object@j      <- J
            .Object@k      <- 1
            .Object@gamma  <- 1.2
            .Object@se     <- 1
            
            ## Define List of Shocks
            .Object@e <- ones(.Object@T,.Object@z)
            for (t in 1:.Object@T) {
              .Object@e[t,] <- rnorm(.Object@z, 0, .Object@se/.Object@z)
            }
            
            ## Initialize Data Frame
            .Object@data <- data.frame(t        = seq(1,.Object@T),
                                       p.simple = NA,
                                       p.mom    = NA,
                                       dp.mom   = NA,
                                       div      = NA,
                                       e        = apply(.Object@e,1,sum))
            .Object@data$div <- cumsum(.Object@data$e) + 5
            
            ## Define Price Series with no Momentum Traders
            for (t in 1:(.Object@T-.Object@z-1)) {
              .Object@data$p.simple[t] <- .Object@data$div[t] + t(as.matrix(.Object@data$e[(t+1):(t+.Object@z-1)]))%*%as.matrix(seq(.Object@z-1,1))/.Object@z - .Object@Q
            }

            ## Define Initial Parameter Guess for Momentum Pricing Functional
            .Object@tet0 <- c(p=.Object@data$p.simple[2:(.Object@j+2)],phi=.5)
            
            ## Harvest Results
            return(.Object)
            
          })

######################################################
### Define Error Function for NonLinear Minimization
Error <- function(tet) {

  ## Define Price and Price Growth Seeds from Inputs
  HS1999@data$p.mom[1:(HS1999@j+2)] <- c(HS1999@data$p.simple[1],tet[1:(HS1999@j+1)])
  HS1999@data$dp.mom[1:(HS1999@j+2)] <- c(NA,HS1999@data$p.mom[2:(HS1999@j+2)] - HS1999@data$p.mom[1:(HS1999@j+1)])

  ## Fill in Price and Price Growth Data from Inputs
  for (t in (HS1999@j+3):(HS1999@T-HS1999@z)) {
    HS1999@data$dp.mom[t] <- sum(HS1999@data$e[t:(t+HS1999@z-1)])/HS1999@z + tet[HS1999@j+2]*HS1999@data$dp.mom[t-1] - tet[HS1999@j+2]*HS1999@data$dp.mom[t-(HS1999@j+1)]
    HS1999@data$p.mom[t] <- HS1999@data$dp.mom[t] + HS1999@data$p.mom[t-1]
  }

  ## Compute Long Run Growth at a j Period Horizon
  HS1999@data$djp.mom <- c(HS1999@data$p.mom[(HS1999@j+1):HS1999@T] - HS1999@data$p.mom[1:(HS1999@T-HS1999@j)],rep(NA,HS1999@j))

  ## Compute Covariances
  Cjk <- cov(HS1999@data$djp.mom,HS1999@data$dp.mom, use="pairwise.complete.obs")
  Vj <- var(HS1999@data$djp.mom, na.rm=TRUE)
  Vk <- var(HS1999@data$dp.mom, na.rm=TRUE)

  ## Estimate Raw Error Term
  ERROR <- as.numeric((tet[HS1999@j+2] - HS1999@gamma/Vk * Cjk/Vj)^2)

  ## Replace Error Term with Large Number if NA
  if (!is.finite(ERROR)) ERROR <- 10 +rnorm(1,0,1)

  ## Spew Results
  return(ERROR)
}

######################################################
### Solve for Price Functional and Plot Results for j=c(1,5,10,20), z=c(5,10,20)
df.plotData <- foreach(i=1:25, .combine="rbind") %dopar% {

  ## Define z and j
  z <- 20
  j <- 20
  
  ## Create Data
  HS1999 <- new("HS1999", Z=z, J=j)
  
  ## Solve for Price Functional
  res <- optim(HS1999@tet0, Error, method="L-BFGS-B", lower=c(rep(0,HS1999@j+1),0.01), upper=c(rep(20,HS1999@j+1),0.99))

  ## Plot Prices for a Single Draw
  if (i == 1) {
    HS1999@data$p.mom[1:(HS1999@j+2)] <- c(HS1999@data$p.simple[1],res$par[1:(HS1999@j+1)])
    HS1999@data$dp.mom[1:(HS1999@j+2)] <- c(NA,HS1999@data$p.mom[2:(HS1999@j+2)] - HS1999@data$p.mom[1:(HS1999@j+1)])
    
    for (t in (HS1999@j+3):(HS1999@T-HS1999@z)) {
      HS1999@data$dp.mom[t] <- sum(HS1999@data$e[t:(t+HS1999@z-1)])/HS1999@z + res$par[HS1999@j+2]*HS1999@data$dp.mom[t-1] - res$par[HS1999@j+2]*HS1999@data$dp.mom[t-(HS1999@j+1)]
      HS1999@data$p.mom[t] <- HS1999@data$dp.mom[t] + HS1999@data$p.mom[t-1]
    }
    HS1999@data$djp.mom <- c(HS1999@data$p.mom[(HS1999@j+1):HS1999@T] - HS1999@data$p.mom[1:(HS1999@T-HS1999@j)],rep(NA,HS1999@j))
    
    df.plotData <- HS1999@data
    df.plotData <- df.plotData[!is.na(df.plotData$p.simple),c('t','p.simple','p.mom','div','e')]
    names(df.plotData) <- c('t','Price (Naive)','Price (Naive + Momentum)','Dividend','Shock')
    df.plotData <- melt(df.plotData, c('t'))
    png(paste(getwd(),'/figures/TimeSeriesFacetWrap_HongStein1999_z',z,'j',j,'_PriceDividendAndShocks.png',sep=''), height=500, width=900)
    p <- ggplot(df.plotData, aes(x=t, y=value, group=variable, colour=variable, linetype=variable))
    p <- p + geom_path()
    p <- p + ylab('') + xlab('')
    print(p)
    dev.off()
  }
  
  ## Return Estimte of Theta
  return(z,j,i,res$par[HS1999@j + 2])
}

## Format Resulting Data
df.plotData <- as.data.frame(df.plotData)
names(df.plotData) <- c('z','j','i','phi')

######################################################
### Plot Distribution of Estimated Elasticities
png(paste(getwd(),'/figures/Histogram_HongStein1999_z',z,'j',j,'_MomentumElasticity.png',sep=''), height=500, width=900)
p <- ggplot(df.plotData, aes(x=phi))
p <- p + geom_bar()
p <- p + ylab('') + xlab('')
print(p)
dev.off()







