#require("ryouready") #Q-Q plot
source('helper_explain.r')

lm_r <-lm(jnd_50_r ~ r + a + a_r, data = t_df)
lm_vf <-lm(jnd_50 ~ base_50 + a + a_base_50, data = t_df)

m_r_50_linear <- gamlss(jnd_50_r ~ r + a + a_r, data = t_df)
m_vf_50_linear <- gamlss(jnd_50 ~ base_50 + a + a_base_50, data = t_df) 

r_50_trans_logno <- gamlss(jnd_50_r ~ r + a + a_r + re(random=~1|workerId), family = LOGNO, data = t_df)
vf_50_trans_logno <- gamlss(jnd_50 ~ base_50 + a + a_base_50 + re(random=~1|workerId), family = LOGNO, data = t_df)

print(lambda_vf)
r_50_trans_bct <- gamlss(jnd_50_r ~ r + a + a_r + re(random=~1|workerId), family = BCT(mu.link=powLink(lambda_r)), data = t_df)
vf_50_trans_bct <- gamlss(jnd_50 ~ base_50 + a + a_base_50 + re(random=~1|workerId), family = BCT(mu.link=powLink(lambda_vf)), data = t_df)


l_r <- "ggplot"(qqnorm_spssmy(residuals(m_r_50_linear), method=4, standardize = F), plottype = 2, line = TRUE, l.col = "black", approach = t_df$approach, xlim.worm = 4.25, ylim.worm = 4.25, xticks=seq(-1.5, 4.5, by = 1.5), yticks=seq(-1, 4, by = 1), ytlims=c(-1,4),xtlims=c(-2,4.5))+ scale_color_manual(values = cblues) 
l_v <- "ggplot"(qqnorm_spssmy(residuals(m_vf_50_linear), method=4, standardize = F), plottype = 2, line = TRUE, l.col = "black", approach = t_df$approach,xlim.worm = 4.25, ylim.worm = 4.25, xticks=seq(-1.5, 4.5, by = 1.5), yticks=seq(-1, 4, by = 1), ytlims=c(-1,4),xtlims=c(-2,4.5))+ scale_color_manual(values = cblues) 
log_r <- "ggplot"(qqnorm_spssmy(residuals(r_50_trans_logno), method=4, standardize = F), plottype = 2, line = TRUE, l.col = "black", approach = t_df$approach)+ scale_color_manual(values = cblues) 
log_v <- "ggplot"(qqnorm_spssmy(residuals(vf_50_trans_logno), method=4, standardize = F), plottype = 2, line = TRUE, l.col = "black", approach = t_df$approach)+ scale_color_manual(values = cblues) 
bct_r <- "ggplot"(qqnorm_spssmy(residuals(r_50_trans_bct), method=4, standardize = F), plottype = 2, line = TRUE, l.col = "black", approach = t_df$approach)+ scale_color_manual(values = cblues) 
bct_v <- "ggplot"(qqnorm_spssmy(residuals(vf_50_trans_bct), method=4, standardize = F), plottype = 2, line = TRUE, l.col = "black", approach = t_df$approach) + scale_color_manual(values = cblues) 


pdf('img/qqgs.pdf', width = 19.8, height = 3.35, useDingbats=FALSE)
multiplot(l_r, l_v, log_r, log_v, bct_r, bct_v, cols = 6)
dev.off()


print(3*mean(cooks.distance(lm_r)))
print(3*mean(cooks.distance(lm_vf)))

gs_r <- lmDiagPlot(lm_r)
gs_v <- lmDiagPlot(lm_vf)

ck <- multiplot(gs_r$cdPlot, gs_v$cdPlot, cols = 2)

pdf('img/cooksd.pdf', width = 6.6, height = 3.3, useDingbats=FALSE)
ck
dev.off()

