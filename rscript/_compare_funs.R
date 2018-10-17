source('helper_explain.r')

powLink <- function(lambda){
  inv_lambda <- 1 / lambda
  inv_lambda_sub_one <- inv_lambda - 1
  
  linkfun <- function(mu) mu^lambda
  linkinv <- function(eta) eta^inv_lambda
  mu.eta<-function(eta) inv_lambda * eta^inv_lambda_sub_one
  valideta<-function(eta) TRUE
  link <- "powLink"
  structure(list(linkfun = linkfun, linkinv = linkinv, mu.eta = mu.eta,
                 valideta = valideta, name = link), class="link-gamlss")
}

if(FALSE) {
  r_log2 <- gamlss(jnd_50_r ~ r + re(random=~1|workerId), family = LOGNO(mu.link="log"), data = t_df)
  r_pow1 <- gamlss(jnd_50_r ~ r + re(random=~1|workerId), family = BCT(mu.link="identity"), data = t_df)
  r_pow2 <- gamlss(jnd_50_r ~ r + re(random=~1|workerId), family = BCT(mu.link="log"), data = t_df)
  r_pow3 <- gamlss(jnd_50_r ~ r + re(random=~1|workerId), family = BCT(mu.link="sqrt"), data = t_df)
  r_pow4 <- gamlss(jnd_50_r ~ r + re(random=~1|workerId), family = BCT(mu.link="inverse"), data = t_df)
  
  lambda <- powerTransform(jnd_50_r ~ r, data = t_df)$lambda
  r_pow5 <- gamlss(jnd_50_r ~ r + re(random=~1|workerId), family = BCT(mu.link=powLink(0.5)), data = t_df)
  
  vf_log1 <- gamlss(jnd_50 ~ base_50 + re(random=~1|workerId), family = LOGNO(mu.link="identity"), data = t_df)
  vf_log2 <- gamlss(jnd_50 ~ base_50 + re(random=~1|workerId), family = LOGNO(mu.link="log"), data = t_df)
  vf_pow1 <- gamlss(jnd_50 ~ base_50 + re(random=~1|workerId), family = BCT(mu.link="identity"), data = t_df)
  vf_pow2 <- gamlss(jnd_50 ~ base_50 + re(random=~1|workerId), family = BCT(mu.link="log"), data = t_df)
  vf_pow3 <- gamlss(jnd_50 ~ base_50 + re(random=~1|workerId), family = BCT(mu.link="sqrt"), data = t_df)
  vf_pow4 <- gamlss(jnd_50 ~ base_50 + re(random=~1|workerId), family = BCT(mu.link="inverse"), data = t_df)
  vf_pow5 <- gamlss(jnd_50 ~ base_50 + re(random=~1|workerId), family = BCT(mu.link="mu^2"), data = t_df)
  vf_pow6 <- gamlss(jnd_50 ~ base_50 + re(random=~1|workerId), family = BCT(mu.link=powLink(lambda=0.49)), data = t_df)
}

lamdas <- seq(-2, 2, 0.01)
index <- 0
df <- data.frame(lambda=double(), r_rsq=double(), r_aic=double(),r_sk=double(), r_kr=double(),r_nm_pvalue=double(), r_hm_pvalue=double(), 
                                  vf_rsq=double(), vf_aic=double(),vf_sk=double(), vf_kr=double(),vf_nm_pvalue=double(), vf_hm_pvalue=double())
colnames(df) <- c("lambda", "r_rsq", "r_aic", "r_sk", "r_kr", "r_nm_pvalue", "r_hm_pvalue", "vf_rsq", "vf_aic", "vf_sk", "vf_kr", "vf_nm_pvalue", "vf_hm_pvalue")
r_pow <- gamlss(jnd_50_r ~ 1, data = t_df)
for(l in lamdas){
  cat(paste("doing ...... ", l))
  cat("\n")
  tryCatch({
    suppressMessages(vf_pow <- gamlss(jnd_50 ~ base_50 + a + a_base_50 + re(random=~1|workerId), family = BCCG(mu.link=powLink(lambda=l)), data = t_df))
    #suppressMessages(r_pow <- gamlss(jnd_50_r ~ r + a + a_r + re(random=~1|workerId), family = BCT(mu.link=powLink(lambda=l)), data = t_df))
    resid_r <- residuals(r_pow)
    resid_vf <- residuals(vf_pow)
    row_index <- c(l, Rsq(r_pow, type = "Cox Snell"), AIC(r_pow), skewness(resid_r), kurtosis(resid_r), shapiro.test(resid_r)$p.value, leveneTest(resid_r, t_df$r)$`Pr(>F)`[1],
                      Rsq(vf_pow, type = "Cox Snell"), AIC(vf_pow), skewness(resid_vf), kurtosis(resid_vf), shapiro.test(resid_vf)$p.value, leveneTest(resid_vf, t_df$base_50_agg)$`Pr(>F)`[1])
    df[nrow(df)+1,] <- row_index
  }, error=function(e){})
}

filename <- paste(paste("output/lambda_selection_BCCG_", target_vf, sep=""),".csv",sep="")

write.csv(df, file=filename, quote = F, row.names = F)
