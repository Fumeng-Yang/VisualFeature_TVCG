require("ryouready") #Q-Q plot
source('helper_explain.r')
rlist <- c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8)

lm_r <-lm(jnd_50_r ~ r + a + a_r, data = t_df)
lm_vf <-lm(jnd_50 ~ base_50 + a + a_base_50, data = t_df)

m_r_50_linear <- gamlss(jnd_50_r ~ r + a + a_r, data = t_df)
m_vf_50_linear <- gamlss(jnd_50 ~ base_50 + a + a_base_50, data = t_df) 

r_50_trans_logno <- gamlss(jnd_50_r ~ r + a + a_r + re(random=~1|workerId), family = LOGNO, data = t_df)
vf_50_trans_logno <- gamlss(jnd_50 ~ base_50 + a + a_base_50 + re(random=~1|workerId), family = LOGNO, data = t_df)


r_50_trans_bct <- gamlss(jnd_50_r ~ r + a + a_r + re(random=~1|workerId), family = BCT(mu.link=powLink(lambda_r)), data = t_df)
vf_50_trans_bct <- gamlss(jnd_50 ~ base_50 + a + a_base_50 + re(random=~1|workerId), family = BCT(mu.link=powLink(lambda_vf)), data = t_df)


qqrlevels <- function(m, name){
  resids <- residuals(m)
  approach <- t_df$approach
  rs <- t_df$r
  df <- data.frame(resids, approach)

  # figure out manually encoding is the fastest in
  df1 <- subset(df, rs == 0.3)
  g1 <- "ggplot"(qqnorm_spss(df1$resids, method=4, standardize = F), plottype = 2, line = TRUE, l.col = "black", approach = df1$approach, xtlims=c(-3.7,3.7),ytlims=c(-2,2), xticks=seq(-3, 3, by = 1.5), yticks=seq(-2,2, by = 0.5))+ scale_color_manual(values = cblues) 

  df2 <- subset(df, rs == 0.4)
  g2 <- "ggplot"(qqnorm_spss(df2$resids, method=4, standardize = F), plottype = 2, line = TRUE, l.col = "black", approach = df2$approach, xtlims=c(-3.7,3.7),ytlims=c(-2,2), xticks=seq(-3, 3, by = 1.5), yticks=seq(-2,2, by = 0.5))+ scale_color_manual(values = cblues) 
  
  
  df3 <- subset(df, rs == 0.5)
  g3 <- "ggplot"(qqnorm_spss(df3$resids, method=4, standardize = F), plottype = 2, line = TRUE, l.col = "black", approach = df3$approach, xtlims=c(-3.7,3.7),ytlims=c(-2,2), xticks=seq(-3, 3, by = 1.5), yticks=seq(-2,2, by = 0.5))+ scale_color_manual(values = cblues) 
  
  
  df4 <- subset(df, rs == 0.6)
  g4 <- "ggplot"(qqnorm_spss(df4$resids, method=4, standardize = F), plottype = 2, line = TRUE, l.col = "black", approach = df4$approach, xtlims=c(-3.7,3.7),ytlims=c(-2,2), xticks=seq(-3, 3, by = 1.5), yticks=seq(-2,2, by = 0.5))+ scale_color_manual(values = cblues) 
  
  
  df5 <- subset(df, rs == 0.7)
  g5 <- "ggplot"(qqnorm_spss(df5$resids, method=4, standardize = F), plottype = 2, line = TRUE, l.col = "black", approach = df5$approach, xtlims=c(-3.7,3.7),ytlims=c(-2,2), xticks=seq(-3, 3, by = 1.5), yticks=seq(-2,2, by = 0.5))+ scale_color_manual(values = cblues) 
  
  
  df6 <- subset(df, rs == 0.8)
  g6 <- "ggplot"(qqnorm_spss(df6$resids, method=4, standardize = F), plottype = 2, line = TRUE, l.col = "black", approach = df6$approach, xtlims=c(-3.7,3.7),ytlims=c(-2,2), xticks=seq(-3, 3, by = 1.5), yticks=seq(-2,2, by = 0.5))+ scale_color_manual(values = cblues) 
  
   
  pdf(paste(paste('img/',name,sep=""),'.pdf',sep=""), width = 19.8, height = 3.35, useDingbats=FALSE)
  multiplot(g1, g2, g3, g4, g5, g6,cols = 6)
  dev.off()
}

qqrlevels(m_r_50_linear, "linear_r_qq_r_levels")
qqrlevels(m_vf_50_linear, "linear_vf_qq_r_levels")
qqrlevels(r_50_trans_logno, "logno_r_qq_r_levels")
qqrlevels(vf_50_trans_logno, "logno_vf_qq_r_levels")
qqrlevels(r_50_trans_bct, "bct_r_qq_r_levels")
qqrlevels(vf_50_trans_bct, "bct_vf_qq_r_levels")