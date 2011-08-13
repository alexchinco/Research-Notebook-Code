rm(list=ls())  

library(RColorBrewer)
library(foreign)
library(reshape)
library(plyr)
library(ggplot2)
library(matlab)
library(xtable)
library(tikzDevice)
library(sandwich)
library(car)
library(lmtest)

set.seed(12345)















simulateOverlappingReturnsData <- function(NUMBER_OF_YEARS, DELTA_t) {

  MU                <- 0.08
  THETA             <- 0.75
  SIGMA             <- 0.16
  NUMBER_OF_PERIODS <- NUMBER_OF_YEARS / DELTA_t
  
  simulatedData <- data.frame(t            = seq(1, NUMBER_OF_PERIODS),
                              z.sub_t      = rnorm(NUMBER_OF_PERIODS),
                              r.sub_tPlus1 = NA,
                              r.sub_t      = NA
                              )

  simulatedData$r.sub_t[1] <- MU
  for (t in 1:(NUMBER_OF_PERIODS-1)) {
    simulatedData$r.sub_tPlus1[t] <- THETA * (MU - simulatedData$r.sub_t[t]) * DELTA_t + SIGMA * sqrt(DELTA_t) * rnorm(1)
    simulatedData$r.sub_t[t+1]    <- simulatedData$r.sub_tPlus1[t]
  }

  simulatedData$r.sub_tPlus3  <- NA
  simulatedData$r.sub_tPlus6  <- NA
  simulatedData$r.sub_tPlus9  <- NA
  simulatedData$r.sub_tPlus12 <- NA
  for (t in 1:(NUMBER_OF_PERIODS-12)) {
    simulatedData$r.sub_tPlus3[t]  <- sum(simulatedData$r.sub_tPlus1[t:(t+2)])
    simulatedData$r.sub_tPlus6[t]  <- sum(simulatedData$r.sub_tPlus1[t:(t+5)])
    simulatedData$r.sub_tPlus9[t]  <- sum(simulatedData$r.sub_tPlus1[t:(t+8)])
    simulatedData$r.sub_tPlus12[t] <- sum(simulatedData$r.sub_tPlus1[t:(t+11)])
  }

  simulatedData <- simulatedData[is.na(simulatedData$r.sub_tPlus12) == FALSE, ]

  return(simulatedData)

}










computeHodrick1992VCovMatrix <- function(Y.sub_H, Y.sub_1, X, H) {

  
  T       <- dim(X)[1]
  E.sub_1 <- lm(Y.sub_1 ~ 1)$residuals
  
  
  computeLittleM <- function(i,H) {

    m <- zeros(2,1)
    
    for (h in 0:(H-1)) {
      m <- m + X[i-h,]
    }

    m <- E.sub_1[i] * m
    
    return(m)
    
  }

  
  computeS <- function(H) {

    S <- zeros(2,2)
    
    for (i in H:T) {
      S <- S + (computeLittleM(i,H) %*% t(computeLittleM(i,H)))
    }

    S <- (1/T) * S
    
    return(S)
    
  }

  
  computeZ <- function(H) {

    Z <- zeros(2,2)
    
    for (i in 1:T) {
      Z <- Z + (X[i,] %*% t(X[i,]))
    }

    Z <- (1/T) * Z
    
    return(Z)
    
  }

  
  S.sub_H <- computeS(H)
  Z.sub_H <- computeZ(H)

  
  V.sub_H <- solve(Z.sub_H) %*% S.sub_H %*% solve(Z.sub_H) / T

  
  return(V.sub_H)

  
}











YEARS   <- 100
DELTA_t <- 1/12
H       <- 6
N       <- 500

estimates <- data.frame(n       = seq(1, N),
                        beta    = NA,
                        seNaive = NA,
                        seH1992 = NA
                        )

for (n in 1:N) {
  
  simulatedData <- simulateOverlappingReturnsData(YEARS, DELTA_t)
  
  results              <- lm(r.sub_tPlus6 ~ z.sub_t, data = simulatedData)
  estimates$beta[n]    <- summary(results)$coef[2,1]
  estimates$seNaive[n] <- summary(results)$coef[2,2]

  DF      <- summary(results)$df[2]
  Y.sub_H <- simulatedData$r.sub_tPlus6
  Y.sub_1 <- simulatedData$r.sub_tPlus1
  X       <- cbind(1,simulatedData$z.sub_t)
  
  hodrick1992VCovMatrix <- computeHodrick1992VCovMatrix(Y.sub_H, Y.sub_1, X, H)
  estimates$seH1992[n]  <- coeftest(results, df = DF, vcov = hodrick1992VCovMatrix)[2,2]

  print(n)
  
}









plotData        <- estimates[,c("n","beta")]
names(plotData) <- c("n", "$\\beta$")
plotData        <- melt(plotData, c("n"))

BETA_MEAN     <- mean(estimates$beta)
BETA_VOL      <- sd(estimates$beta)

plotData$mean <- BETA_MEAN
plotData$ub   <- BETA_MEAN + BETA_VOL
plotData$lb   <- BETA_MEAN - BETA_VOL

RAW_FILE = 'Plot__Hodrick1992StandardErrorCorrection__SimulationResults__Beta'
TEX_FILE = paste(RAW_FILE,'.tex',sep='')
PDF_FILE = paste(RAW_FILE,'.pdf',sep='')
PNG_FILE = paste(RAW_FILE,'.png',sep='')

tikz(file = TEX_FILE, height = 5, width = 9, standAlone = TRUE)

p = ggplot(plotData)
p = p + geom_histogram(aes(x = value, fill = variable, group = variable), binwidth = 0.0005)
p = p + geom_vline(aes(xintercept = mean), colour = "red", linetype = 1, size = 2)
p = p + geom_vline(aes(xintercept = ub), colour = "red", linetype = 3, size = 2)
p = p + geom_vline(aes(xintercept = lb), colour = "red", linetype = 3, size = 2)
p = p + ylab('') + xlab('$\\beta$')
p = p + opts(legend.position = "none")
print(p)
dev.off()

tools::texi2dvi(TEX_FILE, pdf = TRUE)
OS = Sys.info()["sysname"]
if (OS == "Linux") {
  system(paste('convert -density 450 ', file.path(PDF_FILE), ' ', file.path(PNG_FILE)))
} else if (OS == "Darwin") {
  system(paste('sips -s format png', file.path(PDF_FILE), '--out', file.path(PNG_FILE)))
}












plotData        <- estimates[,c("n","seNaive", "seH1992")]
names(plotData) <- c("n", "$\\hat{\\sigma}_{Naive}$", "$\\hat{\\sigma}_{H1992}$")
plotData        <- melt(plotData, c("n"))

SE_NAIVE_MEAN <- mean(estimates$seNaive)
SE_H1992_MEAN <- mean(estimates$seH1992)

plotData$mean <- NA
plotData[plotData$variable == "$\\hat{\\sigma}_{Naive}$", ]$mean <- SE_NAIVE_MEAN
plotData[plotData$variable == "$\\hat{\\sigma}_{H1992}$", ]$mean <- SE_H1992_MEAN

RAW_FILE = 'Plot__Hodrick1992StandardErrorCorrection__SimulationResults__StdError'
TEX_FILE = paste(RAW_FILE,'.tex',sep='')
PDF_FILE = paste(RAW_FILE,'.pdf',sep='')
PNG_FILE = paste(RAW_FILE,'.png',sep='')

tikz(file = TEX_FILE, height = 7, width = 9, standAlone = TRUE)

p = ggplot(plotData)
p = p + geom_histogram(aes(x = value, fill = variable, group = variable), binwidth = 0.00005)
p = p + geom_vline(aes(xintercept = mean), colour = "red", linetype = 1, size = 2)
p = p + facet_wrap(~ variable, ncol = 1, scales = "free_y")
p = p + ylab('') + xlab('')
p = p + opts(legend.position = "none")
print(p)
dev.off()

tools::texi2dvi(TEX_FILE, pdf = TRUE)
OS = Sys.info()["sysname"]
if (OS == "Linux") {
  system(paste('convert -density 450 ', file.path(PDF_FILE), ' ', file.path(PNG_FILE)))
} else if (OS == "Darwin") {
  system(paste('sips -s format png', file.path(PDF_FILE), '--out', file.path(PNG_FILE)))
}
