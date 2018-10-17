source('helper.r')

library(ggplot2)
library(dplyr)
library(car)

# read values
#d <- read.csv('../data/more_judgments.csv', stringsAsFactors = F)
d <- read.csv('../data/judgments_data_cbxpara.csv', stringsAsFactors = F)

# remove extreme values
d <- subset(d, rv < 1)

apps <- c("above", "below")
#vfs <- c("ellipse_minor","dist_line_sd")
vfs <- c("conf_bounding_box_para") # "ellipse_area", "conf_bounding_box_perp", 

# get all workerIds
dw <- data.frame(d$workerId)
names(dw) <- c("workerId")
dw <- dw[!duplicated(dw$workerId),]

results <- c()          # you don't know levels yet
i <- 1

for(ivf in vfs){
  for(w in dw){
  dwo <- subset(d, workerId == w)
  dwr <- data.frame(dwo$r)
  names(dwr) <- c("r")
  dwr <- dwr[!duplicated(dwr$r),]
  for(ir in dwr){
    for(iapp in apps){
    ivf_base <- paste(ivf, "_base", sep = "")
    d$factor <- code_factor(d)
    dws_50 <- subset(d, workerId == w & r == ir & approach == iapp)
    dws_24 <- subset(dws_50, judgmentId > cnvgIndex - 23 & judgmentId <= cnvgIndex + 1)
    dws_26 <- subset(dws_50, judgmentId <= cnvgIndex - 23 | judgmentId > cnvgIndex + 1)
    
    dws_50$w <- weight(dws_50) 
    # m_50 <- glm(paste('factor ~ ', ivf), dws_50, family = binomial(link = 'logit'), weights = w)
     m_50 <- glm(paste('factor ~ sqrt(', paste(ivf,")",sep="")), dws_50, family = binomial(link = 'logit'), weights = w)
    dbase_50 <- subset(d, r == ir & approach == iapp)
    target_50 <- logitJND(0.5, m_50$coefficients[1], m_50$coefficients[2])

    m_50_r <- glm(paste('factor ~ r_diff'), dws_50, family = binomial(link = 'logit'), weights = w)
    target_50_r <- logitJND(0.5, m_50_r$coefficients[1], m_50_r$coefficients[2])
    
    dws_24$w <- weight(dws_24) 
    m_24 <- glm(paste('factor ~ ', ivf), dws_24, family = binomial(link = 'logit'), weights = w)
    dbase_24 <- subset(d, r == ir & approach == iapp & judgmentId > cnvgIndex - 23 & judgmentId <= cnvgIndex + 1)
    target_24 <- logitJND(0.5, m_24$coefficients[1], m_24$coefficients[2])

    m_24_r <- glm(paste('factor ~ r_diff'), dws_24, family = binomial(link = 'logit'), weights = w)
    target_24_r <- logitJND(0.5, m_24_r$coefficients[1], m_24_r$coefficients[2])

    dws_26$w <- weight(dws_26) 
    m_26 <- glm(paste('factor ~ ', ivf), dws_26, family = binomial(link = 'logit'), weights = w)
    target_26 <- logitJND(0.5, m_26$coefficients[1], m_26$coefficients[2])
    
    l <- c(ivf, w, ir, iapp, as.numeric(target_50_r), as.numeric(target_24_r), as.numeric(target_50), as.numeric(mean(dws_50[,ivf_base])), as.numeric(mean(dbase_50[,ivf_base])), as.numeric(target_24), as.numeric(target_26), as.numeric(mean(dws_24[,ivf_base])), as.numeric(mean(dbase_24[,ivf_base])), as.numeric(dws_50$jnd[1]))
    results <- rbind(results, l)
    i <- i + 1
    } # end of approach
  } # end of r
 } # end of workerID
} # end of vf



results <- data.frame(results)
titles <- c('vf', 'workerId', 'r', 'approach', 'jnd_50_r', 'jnd_24_r', 'jnd_50', 'base_50', 'base_50_agg', 'jnd_24',  'jnd_26', 'base_24', 'base_24_agg', 'jnd_r')
names(results) <- titles

write.csv(results, file = '../data/individual_jnd_cbxpara.csv')

# sub_1 <- subset(results, vf == 'dist_line_sd', select = c('workerId', 'r', 'approach', 'jnd_r'))
# titles_sub <- c('participant', 'r', 'approach', 'jnd')
# names(sub_1) <- titles_sub
# sub_1$visandsign <- rep(x = "scatterplot-positive", length(sub_1$r))
# write.csv(sub_1, file = '../data/2015_r.csv')

# sub_2 <- subset(results, vf == 'dist_line_sd', select = c('workerId', 'r', 'approach', 'jnd_50_r'))
# names(sub_2) <- titles_sub
# sub_2$visandsign <- rep(x = "scatterplot-positive", length(sub_2$r))
# write.csv(sub_2, file = '../data/2015_r_from50.csv')
