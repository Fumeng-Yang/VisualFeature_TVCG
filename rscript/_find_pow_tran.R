source('helper_explain.r')
library(lmtest)
library(aod)
library(arm)
library(pscl)

d <- read.csv('../data/judgments_alter.csv', stringsAsFactors = F)

# remove extreme values
d <- subset(d, rv < 1)

d <- d[sample(nrow(d)),]
# weight the instances
d$w <- weight(d)
d$weights <- weight(d)
# encode the judgments
d$correctness <- code_factor(d)
epsilon <- 1E-14
names <- colnames(d)


find_judgments_pow <- function(name){
  lamdas <- seq(-2, 2, 0.1)
  df <- data.frame(powIndex=double(), Rtwo=double(), AICv=double(),ORv=double())
  colnames(df) <- c("powIndex", "Rtwo", "AICv", "ORv")
  for(l in lamdas){
    cat(paste("doing ...... ", l))
    cat("\n")
    tryCatch({
     d$tmp <<- as.numeric(d[,name]^l)
     d <<- na.omit(d)
     m <-  standardize(glm(grade ~ tmp, data = d, family = binomial(link = 'logit'), weights = d$weights))
     row_index <- c(l, pR2(m)[6], AIC(m), exp(cbind(OR = coef(m), confint(m)))[,1][2])
     df[nrow(df)+1,] <- row_index
    }, error=function(e){})
  }
  filename <- paste(paste("output/jg_pow_0.01_", name, sep=""),".csv",sep="")
  write.csv(df, file=filename, quote = F, row.names = F)
}



find_judgments_log <- function(name){
  betas <- c(0.001, 0.1, 0.2, 0.3, 0.4, 0.5, 1, 3, 4, 5, 10, 20, 30, 40, 50, 100, 500, 1000, 5000)
  df <- data.frame(powIndex=double(), Rtwo=double(), AICv=double(),ORv=double())
  colnames(df) <- c("beta", "Rtwo", "AICv", "ORv")
  for(bv in betas){
    cat("\n")
    tryCatch({
      cat(paste("doing ...... ", bv))
      d$tmp <<- log(d[,name] + bv)
      m <- standardize(glm(grade ~  tmp, data = d, family = binomial(link = 'logit'), weights = d$weights))
      row_index <- c(bv, pR2(m)[6], AIC(m), exp(cbind(OR = coef(m), confint(m)))[,1][2])
      df[nrow(df)+1,] <- row_index
    }, error=function(e){})
  }
  
  filename <- paste(paste("output/jg_log_", name, sep=""),".csv",sep="")
  write.csv(df, file=filename, quote = F, row.names = F)
}
find_judgments_pow("r_diff")
find_judgments_log("r_diff")
lapply(vfs, find_judgments_pow)
lapply(vfs, find_judgments_log)
