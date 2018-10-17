################################################################################################
# build a model for each visual feature
# author Fumeng Yang
################################################################################################

library(lmtest)
library(aod)
library(arm)
library(pscl)
#library(standardize)

source('helper_explain.r')
source('models.r')

# load visual feature list
vf_list <- read.csv('../data/vf_list.csv', header = T)
vf_list <- vf_list$vf_list
#vf_list <- c('ellipse_minor', 'ellipse_area', 'conf_bounding_box_perp', 'dist_line_sd')
  
d <- d[sample(nrow(d)),]
# weight the instances
d$w <- weight(d)
d$weights <- weight(d)
# encode the judgments
d$grade <- code_factor(d)
epsilon <- 1E-14
  
# 0. null model
print('-------------------------------------------------')
print('null model')
m.null <- glm(grade ~ 1, data = d, weights = w, family = binomial(link = "logit"))
# m.null <- standardize(m.null)
print(summary(m.null))
print(BIC(m.null))
print(exp(cbind(OR = coef(m.null), confint(m.null))))
pred.m.null.num <- predict(m.null)
pred.m.null <- sapply(pred.m.null.num, function(x) {if (x < -epsilon)  "FALSE" else if (x > epsilon) "TRUE" else "UNKNOWN"})
print(paste("Predict # of judgments", sum(d$grade==pred.m.null)))

# 1. r model
print('-------------------------------------------------')
print('model of r')
m.r <- glm(grade ~ r, data = d, weights = w, family = binomial(link = "logit"))
m.r <- standardize(m.r)
print(summary(m.r))
print(BIC(m.r))
print(exp(cbind(OR = coef(m.r), confint(m.r))))
print(lrtest(m.r, m.null))
pred.m.r.num <- predict(m.r)
pred.m.r <- sapply(pred.m.r.num, function(x) {if (x * m.r_diff$coefficients[1] < -epsilon)  "FALSE" else if (x * m.r_diff$coefficients[1] > epsilon) "TRUE" else "UNKNOWN"})
print(paste("Predict # of judgments", sum(d$grade==pred.m.r)))

# 2. r + approach model
print('-------------------------------------------------')
print('model of r + approach')
m.r.approach <- glm(grade ~ r + approach, data = d, weights = w, family = binomial(link = "logit"))
m.r.approach <- standardize(m.r.approach)
print(summary(m.r.approach))
print(BIC(m.r.approach))
print(exp(cbind(OR = coef(m.r.approach), confint(m.r.approach))))
print(lrtest(m.r, m.r.approach))
print(lrtest(m.null, m.r.approach))

# 3. r + approach + r_diff model
print('-------------------------------------------------')
print('model of r + approach + r_diff')
m.r_diff <- glm(grade ~ r + approach + r_diff, data = d, weights = w, family = binomial(link = "logit"))
m.r_diff <- standardize(m.r_diff)
print(summary(m.r_diff))
print(BIC(m.r_diff))
print(exp(cbind(OR = coef(m.r_diff), confint(m.r_diff))))
print(lrtest(m.r.approach, m.r_diff))
print(lrtest(m.null, m.r_diff))

pred.m.r_diff.num <- predict(m.r_diff)
pred.m.r_diff <- sapply(pred.m.r_diff.num, function(x) {if (x * m.r_diff$coefficients[1] < -epsilon)  "FALSE" else if(x * m.r_diff$coefficients[1] > epsilon) "TRUE"})
print(paste("Predict # of judgments", sum(d$grade==pred.m.r_diff)))

source('models.r')
# source('log_models.r')

sink("output/solo_visual_feature_model.txt")
for(vf in vf_list){
   print('-------------------------------------------------')
   frml <- paste('grade ~', paste(vf, " + r"))
   print(frml)
   m <- get(paste('m.', vf, sep =''))
   m <- standardize(m)
   # odd ratio
   print(exp(cbind(OR = coef(m), confint(m))))
   print(summary(m))
   print(BIC(m))
   print(lrtest(m.null, m))
   print(lrtest(m.r.approach, m))
   print(lrtest(m.r_diff, m))
   # R2
   print(pR2(m))
   print(coxtest(m.r_diff, m))
   # predict
   pred.m.num <- predict(m)
   pred.m <- sapply(pred.m.num, function(x) {if (x * m$coefficients[1] < -epsilon)  "FALSE" else if (x * m$coefficients[1] > epsilon) "TRUE" else "UNKNOWN"})
   print(paste("Predict # of judgments", sum(d$grade==pred.m)))
}
sink()
