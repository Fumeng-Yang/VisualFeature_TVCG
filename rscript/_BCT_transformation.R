library(car)
library(statmod)
source('helper_explain.r')

logno_r_coefs <- c(-1.4137, -2.0152) # checked on 10/22/2017
logno_vf_coefs <- c(0.1903, 0.0297) # checked on 10/22/2017
sub_bct_coefs <- c(0.6672,-0.2549) # checked on 10/23/2017
vf_agg <- t_df$base_50_agg
cat("###########################################################################################################\n")

cat('############################ the BCT model of r + random intercept with a ar ##############################\n')
r_50_trans_bct <- gamlss(jnd_50_r ~ r + a + a_r + re(random=~1|workerId), family = BCT(mu.link=powLink(lambda_r)), data = t_df)
l1 <- powerTransform(jnd_50_r ~ r + a + a_r +workerId, data = t_df)$lambda
print(l1)
mySummary(r_50_trans_bct, t_df$r)

r_50_trans_bct_coefs <- r_50_trans_bct$mu.coefficients

t_df$predict_r <- (predict(r_50_trans_bct))^(1.0/lambda_r)
t_df$resid_r <- t_df$predict_r - t_df$jnd_50_r
resid_x <- aggregate(r ~ as.factor(r), data = t_df, mean)$r
predict_mean <- aggregate(predict_r ~ r, data = t_df, mean)$predict_r
resid_sd <- aggregate(resid_r ~ r, data = t_df, sd)$resid_r

limits_r_50_bct <- aes(x = as.numeric(resid_x), ymax = predict_mean + resid_sd, ymin = predict_mean - resid_sd)

rlist <- c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
predlist <- predict_mean
path <- data.frame(rlist, predlist)

g_r_50_trans_bct  <- ggplot() %>%
  + theme(axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"), panel.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none") %>%
  + scale_color_manual(values = cblues) %>%
  + geom_point(data = t_df, aes(x =  r, y = jnd_50_r, fill = approach, color = approach), alpha = 0.5, size = 3.3, stroke = 0) %>%
  + geom_errorbar(color = "black", limits_r_50_bct, width = 0.0, size = 0.5) %>%
  + scale_x_continuous(breaks=seq(0.3, 0.8, by = 0.1), limits=c(0.25,0.85)) %>%
  + scale_y_continuous(breaks=seq(0, 0.5, by = 0.1), limits=c(0, 0.5)) %>%
  + stat_function(size = 0.5, linetype = 'solid', color = "steelblue", data = t_df, fun = function(x) exp(logno_r_coefs[1] + x * logno_r_coefs[2])) %>%
  + stat_function(size = 0.5, linetype = 'solid', data = t_df, fun = function(x) ((r_50_trans_bct_coefs[1] + x * r_50_trans_bct_coefs[2])^(1.0/lambda_r))) %>%
  + stat_function(size = 0.5, linetype = "solid", color = color_sub, data = t_df, fun = function(x) ((sub_bct_coefs[1] + x * sub_bct_coefs[2])^(1.0/lambda_r))) %>%
  + xlab('r') + ylab('JNDr') + ggtitle("Trans Linear regresssion for JND of correlation") 


pdf('img/bct_r.pdf', width = 3.3, height = 3.3, useDingbats=FALSE)
g_r_50_trans_bct
dev.off()

cat('######################## the residual analysis of bct model of r + random intercept #######################\n')
bct_resid_r_50<- residuals(r_50_trans_bct)
bct_fitted_r_50 <- fitted(r_50_trans_bct)
r_in_models <- t_df$r

df <- data.frame(bct_resid_r_50, bct_fitted_r_50, r_in_models)
input_x <- aggregate(bct_fitted_r_50 ~  as.factor(r_in_models), data = df, mean)$bct_fitted_r_50
input_sd <- aggregate(bct_resid_r_50 ~ as.factor(r_in_models), data = df, sd)['bct_resid_r_50'][,1]

plot_residuals(seq(0.0, 0.2, by = 0.05), c(0.0, 0.2), "bct_resid_r_50", "bct_fitted_r_50", input_x, input_sd)

cat('########################### the BCT model of vf + random intercept with a ar ##############################\n')
vf_50_trans_bct <- gamlss(jnd_50 ~ base_50 + a + a_base_50 + re(random=~1|workerId), family = BCT(mu.link=powLink(lambda_vf)), data = t_df)
mySummary(vf_50_trans_bct, vf_agg)

l2 <- powerTransform(jnd_50 ~ base_50 + a + a_base_50 +workerId, data = t_df)$lambda
print(l2)

#cat('################################# the BCT model of vf + random intercept ###################################\n')

vf_50_trans_bct_coefs <- vf_50_trans_bct$mu.coefficients

t_df$predict_vf <- (predict(vf_50_trans_bct)^(1/lambda_vf))
t_df$resid_vf <- t_df$predict_vf - t_df$jnd_50
resid_x <- aggregate(base_50 ~ as.factor(r), data = t_df, mean)$base_50
predict_mean <- aggregate(predict_vf ~ r, data = t_df, mean)$predict_vf
resid_sd <- aggregate(resid_vf ~ r, data = t_df, sd)$resid_vf

limits_vf_50_bct <- aes(x = as.numeric(resid_x), ymax = predict_mean + resid_sd, ymin = predict_mean - resid_sd)

rlist <- c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
predlist <- predict_mean
path <- data.frame(rlist, predlist)

g_vf_50_trans_bct  <- ggplot() %>%
  + theme(axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"), panel.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none") %>%
  + scale_color_manual(values = cblues) %>%
  + geom_point(data = t_df, aes(x =  base_50, y = jnd_50, fill = approach, color = approach), alpha = 0.5, size = 3.3, stroke = 0) %>%
  + geom_errorbar(size = 0.5, color = "black", limits_vf_50_bct, width = 0.0) %>%
  + stat_function(size = 0.5, linetype = 'solid', color = 'black', data = t_df, fun = function(x) ((vf_50_trans_bct_coefs[1] + x * vf_50_trans_bct_coefs[2])^(1.0/lambda_vf))) %>%
  + stat_function(size = 0.5, linetype = 'solid', color = 'steelblue', data = t_df, fun = function(x) exp(logno_vf_coefs[1] + x * logno_vf_coefs[2])) %>%
  + scale_x_reverse(breaks=seq(15, 30, by = 3), limits=c(31,14)) %>%
  + scale_y_continuous(breaks=seq(0, 10, by = 2), limits=c(0, 10)) %>%
  + xlab('dist_line_sd') + ylab('JNDv') + ggtitle("BCT regresssion for JND of vf")

pdf('img/bct_vf.pdf', width = 3.3, height = 3.3, useDingbats=FALSE)
g_vf_50_trans_bct
dev.off()

####################################################################################################
# residuals of BCT model of visual feature
####################################################################################################
cat('################################# the residual analysis of BCT model of vf + random intercept ###################################\n')
bct_resid_vf_50_trans <- residuals(vf_50_trans_bct)
fitted_resid_vf_50_trans <- fitted(vf_50_trans_bct)

df <- data.frame(bct_resid_vf_50_trans, fitted_resid_vf_50_trans)
input_x <- aggregate(fitted_resid_vf_50_trans ~  as.factor(vf_agg), data = df, mean)$fitted_resid_vf_50_trans
input_sd <- aggregate(bct_resid_vf_50_trans ~ as.factor(vf_agg), data = df, sd)['bct_resid_vf_50_trans'][,1]

plot_residuals(seq(0.75, 5, by = 1), c(0.75, 5), "bct_resid_vf_50_trans", 'fitted_resid_vf_50_trans', input_x, input_sd)

####################################################################################################
# BCT transform of each JND
####################################################################################################
cat('################################# the relation between transformed JND ###################################\n')
r_vf_trans_bct <- gamlss(predict(vf_50_trans_bct) ~ predict(r_50_trans_bct) + re(random=~1|workerId), data = t_df)
mySummary(r_vf_trans_bct, t_df$r)

