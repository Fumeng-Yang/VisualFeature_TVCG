source('helper.r')

f <- read.csv('../data/harrison_data.csv', stringsAsFactors = F)
f$a <- code_a(f$approach)
f$a_r <- f$a * as.numeric(f$rbase)
f$r <- f$rbase

f1 <- subset(f, visandsign=="scatterplotpositive")
f2 <- subset(f, visandsign=="scatterplotnegative")

m_linear <- gamlss(jnd ~ r + a + a_r, data = f2) 
m_loglinear <- gamlss(jnd ~ r + a + a_r  + re(random=~1|postId), family = LOGNO, data = f2) 
m_bct <- gamlss(jnd ~ r + a + a_r + re(random=~1|postId),family = BCT(mu.link = powLink(lambda = -0.04)), data = f2) 

lamdas <- seq(-2, 2, 0.01)
index <- 0

df <- data.frame(lambda=double(), r_rsq=double(), r_aic=double(),r_sk=double(), r_kr=double(),r_nm_pvalue=double(), r_hm_pvalue=double())
colnames(df) <- c("lambda", "r_rsq", "r_aic", "r_sk", "r_kr", "r_nm_pvalue", "r_hm_pvalue")

for(l in lamdas){
  cat(paste("doing ...... ", l))
  cat("\n")
  tryCatch({
    suppressMessages(m <- gamlss(jnd ~ r + a + a_r + re(random=~1|postId), family = BCT(mu.link=powLink(lambda=l)), data = f2))
    resid_r <- residuals(m)
    row_index <- c(l, Rsq(m, type = "Cox Snell"), AIC(m), skewness(resid_r), kurtosis(resid_r), shapiro.test(resid_r)$p.value, leveneTest(resid_r, f2$r)$`Pr(>F)`[1])
    df[nrow(df)+1,] <- row_index
  }, error=function(e){})
}

filename <- "output/lambda_selection_2014_negative.csv"

write.csv(df, file=filename, quote = F, row.names = F)
