## Initialize R workspace
rm(list=ls())  
library(RColorBrewer)
library(foreign)
library(reshape)
library(plyr)
library(ggplot2)
library(matlab)
library(xtable)
library(tikzDevice)
set.seed(12345)




## Define fuction to create random Hermitian matrices
CreateHermitianMatrix <- function(dim) {

  H <- zeros(dim,dim)

  for (row in 1:dim) {
    for (col in dim:1) {
      
      if (row == col) {
        H[row,col] <- rnorm(1,0,1)
      }
      
      if (col > row) {
        H[row,col] <- complex(real = rnorm(1,0,sqrt(1/2)), imaginary = rnorm(1,0,sqrt(1/2)))
      }
      
      if (row > col) {
        H[row,col] <- Conj(H[col,row])
      }
      
    }
  }
  
  return(H)

}




## Test CreateHermitionMatrix() function
TestH <- CreateHermitianMatrix(5)

TestD                 <- diag(eigen(TestH)$values)
TestU                 <- eigen(TestH)$vectors
TestU.sup_star        <- t(Conj(TestU))

TestH.hat     <- TestU %*% TestD %*% TestU.sup_star




## Define economic paramters
numOfAgents   <- 25
numOfPeriods  <- 9
numOfIters    <- 100  
simData       <- zeros(numOfIters * numOfPeriods, 2 + numOfAgents)




## Fill in data frame of eigen-values
for (i in 1:numOfIters) {
  for (t in 1:numOfPeriods) {

  if (t == 1) {
    H <- CreateHermitianMatrix(numOfAgents)
  } else {
    G <- CreateHermitianMatrix(numOfAgents)
    H <- H + G
  }

  simData[numOfPeriods * (i-1) + t, numOfAgents + 1]  <- i
  simData[numOfPeriods * (i-1) + t, numOfAgents + 2]  <- t
  simData[numOfPeriods * (i-1) + t, 1:numOfAgents]    <- eigen(H)$values
  
  }
}




## Plot resulting eigen-value data
plotData      <- as.data.frame(simData)

names(plotData)[numOfAgents + 1]      <- "i"
names(plotData)[numOfAgents + 2]      <- "t"

plotData      <- melt(plotData, c("i","t"))

plotData$t            <- paste("$t = ", plotData$t, "$", sep = "")
plotData$value        <- abs(plotData$value)

RAW_FILE <- 'Plot__PricingPatterns__JohanssonFormula'
TEX_FILE <- paste(RAW_FILE,'.tex',sep='')
PDF_FILE <- paste(RAW_FILE,'.pdf',sep='')
PNG_FILE <- paste(RAW_FILE,'.png',sep='')

tikz(file = TEX_FILE, height = 5, width = 9, standAlone = TRUE)

p <- ggplot(plotData)
p <- p + geom_histogram(aes(x         = value,
                            fill      = t
                            ), 
                        binwidth = 1
                        )
p <- p + facet_wrap(~ t, ncol = 3)
p <- p + ylab('') + xlab('$\\lambda_i(t)$')
p <- p + opts(legend.position = "none")
print(p)
dev.off()

tools::texi2dvi(TEX_FILE, pdf = TRUE)
OS <- Sys.info()["sysname"]
if (OS == "Linux") {
  system(paste('convert -density 450 ', file.path(PDF_FILE), ' ', file.path(PNG_FILE)))
} else if (OS == "Darwin") {
  system(paste('sips -s format png', file.path(PDF_FILE), '--out', file.path(PNG_FILE)))
}
