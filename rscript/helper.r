library(lmtest)
library(aod)
library(arm)
library(pscl)
library(gamlss)
library(lme4)
library(ggplot2)
library(dplyr)
library(car)
library(MuMIn)
library(nlme)
library(e1071)
library(ryouready)

# font_import(pattern="[P/p]alatino") # Doesn't work


# misc: functions that are used
# # # # # # # # # # # # # # # # # # # # # # # #


rmsd <- function(x){
  sqrt(mean((x - mean(x)) ^2))
}

code_a <- function(e){
  tmp <- rep(1, length(e))
  tmp[e == 'above'] <- -1
  
  return (tmp)
}


# encode data
code_factor <- function(x){
    code <- c(1:length(x$grade))
    for(i in 1:length(x$grade)){
        if(x$grade[i] == "FALSE" | x$grade[i] == "false"){
            code[i] <- 0 #"wrong"
        }else{
            code[i] <- 1 #"correct"
        }
    }
    return (code)
}

# weight function
weight <- function(dws){
  f <- length(subset(dws, grade == "false" | dws$grade == "FALSE")$grade)
  t <- length(subset(dws, grade == "true" | dws$grade == "TRUE")$grade)
  fw <- f / (f + t)
  tw <- t / (f + t)
  iweights <- c(1:length(dws$grade))
  for(i in 1:length(dws$grade)){
    if(dws$grade[i] == "FALSE" | dws$grade[i] == "false"){
      iweights[i] <- tw
    }else{
      iweights[i] <- fw
    }
  }
  return (iweights)
}

# steal from http://eurekastatistics.com/using-the-median-absolute-deviation-to-find-outliers
double_mad <- function(x, zero.mad.action="warn"){
   # The zero.mad.action determines the action in the event of an MAD of zero.
   # Possible values: "stop", "warn", "na" and "warn and na".
   x         <- x[!is.na(x)]
   m         <- median(x)
   abs.dev   <- abs(x - m)
   left.mad  <- median(abs.dev[x<=m])
   right.mad <- median(abs.dev[x>=m])
   if (left.mad == 0 | right.mad == 0){
      if (zero.mad.action == "stop") stop("MAD is 0")
      if (zero.mad.action %in% c("warn", "warn and na")) warning("MAD is 0")
      if (zero.mad.action %in% c(  "na", "warn and na")){
         if (left.mad  == 0) left.mad  <- NA
         if (right.mad == 0) right.mad <- NA
      }
   }
   return(c(left.mad, right.mad))
}

# steal from http://eurekastatistics.com/using-the-median-absolute-deviation-to-find-outliers
double_mad_from_median <- function(x, zero.mad.action="warn"){
   # The zero.mad.action determines the action in the event of an MAD of zero.
   # Possible values: "stop", "warn", "na" and "warn and na".
   two.sided.mad <- double_mad(x, zero.mad.action) #mad(x, constant = 1)
   m <- median(x, na.rm=TRUE)
   x.mad <- rep(two.sided.mad[1], length(x))
   x.mad[x > m] <- two.sided.mad[2]
   mad.distance <- abs(x - m) / x.mad
   mad.distance[x==m] <- 0
   return(mad.distance)
}


adj <- function(data, attr, flag){
  rs <- c("0.3", "0.4", "0.5", "0.6", "0.7", "0.8")
  apps <- c("above", "below")
  avg_jnds <- c()
  
  for(ir in rs){
    tmp <- subset(data, r == ir)
    avg_jnd <- mean(as.numeric(tmp[,attr]))
    avg_jnds <- c(avg_jnds, avg_jnd)
  }

  data$avg_jnds <- as.numeric(avg_jnds)
  # adjust
  # print(data)
  adj_offsets <- c()
  for(i in 1:length(data$r)){
    l <- data[i,]
    adj_offset <- -1
    if(l$approach == 'above'){
      adj_offset <- 0.5 * l$avg_jnds * flag
    } else {
      adj_offset <- -1 * 0.5 * l$avg_jnds * flag
    }
    adj_offsets <- c(adj_offsets, adj_offset)
  }
  return (adj_offsets)
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
   require(grid)
   # Make a list from the ... arguments and plotlist
   plots <- c(list(...), plotlist)
   numPlots = length(plots)
   # If layout is NULL, then use 'cols' to determine layout
   if (is.null(layout)) {
     # Make the panel
     # ncol: Number of columns of plots
     # nrow: Number of rows needed, calculated from # of cols
     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
   }
  if (numPlots==1) {
     print(plots[[1]])
   } else {
     # Set up the page
     grid.newpage()
     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
     # Make each plot, in the correct location
     for (i in 1:numPlots) {
       # Get the i,j matrix positions of the regions that contain this subplot
       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                       layout.pos.col = matchidx$col))
     }
   }
 }

 logitJND <- function(c, b0, b1){
   return ((-1 * log(1 / c - 1) - b0) / b1)
}



RsqGLM <- function(obs = NULL, pred = NULL, model = NULL) {
  # version 1.2 (3 Jan 2015)
  model.provided <- ifelse(is.null(model), FALSE, TRUE)
  
  if (model.provided) {
    if (!("glm" %in% class(model))) stop ("'model' must be of class 'glm'.")
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values
    
  } else { # if model not provided
    if (is.null(obs) | is.null(pred)) stop ("You must provide either 'obs' and 'pred', or a 'model' object of class 'glm'")
    if (length(obs) != length(pred)) stop ("'obs' and 'pred' must be of the same length (and in the same order).")
    if (!(obs %in% c(0, 1)) | pred < 0 | pred > 1) stop ("Sorry, 'obs' and 'pred' options currently only implemented for binomial GLMs (binary response variable with values 0 or 1) with logit link.")
    logit <- log(pred / (1 - pred))
    model <- glm(obs ~ logit, family = "binomial")
  }
  
  null.mod <- glm(obs ~ 1, family = family(model))
  loglike.M <- as.numeric(logLik(model))
  loglike.0 <- as.numeric(logLik(null.mod))
  N <- length(obs)
  
  # based on Nagelkerke 1991:
  CoxSnell <- 1 - exp(-(2 / N) * (loglike.M - loglike.0))
  Nagelkerke <- CoxSnell / (1 - exp((2 * N ^ (-1)) * loglike.0))
  
  # based on Allison 2014:
  McFadden <- 1 - (loglike.M / loglike.0)
  Tjur <- mean(pred[obs == 1]) - mean(pred[obs == 0])
  sqPearson <- cor(obs, pred) ^ 2
  
  return(list(CoxSnell = CoxSnell, Nagelkerke = Nagelkerke, McFadden = McFadden, Tjur = Tjur, sqPearson = sqPearson))
}

####################################################################################################
#
# plot residuals
#
####################################################################################################

plot_residuals <- function(x_scale_ticks, x_scale, target_y, target_x, input_x = null, input_sd = null, exp_flag = 0){
  
  limits <- aes(x = input_x, ymax = input_sd, ymin = -1 * input_sd)
  
  dots <- ggplot() %>%
    + theme(legend.position = "none", axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"), panel.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) %>%
    + geom_point(aes(y = df[target_y][,1], x = df[target_x][,1], fill = t_df$approach, color = t_df$approach), alpha = 0.5, size = 3.5, stroke = 0) %>%
    + geom_hline(yintercept = 0, linetype="dashed", alpha = 0.5) %>%
    + scale_color_manual(values = cblues) %>%
    + geom_errorbar(color = "black", limits, width = 0.0, size = 0.5) %>%
    + scale_x_continuous(breaks = x_scale_ticks, limits=x_scale) %>%
    + scale_y_continuous(breaks = seq(-6, 6, by = 2), limits=c(-6.25,6.25)) %>%
    + xlab('Fitted Values') + ylab("Normalized Quantile Residuals")
  
  density <- ggplot(df, aes(x = df[target_y][,1]))  %>%
    + theme(axis.ticks.y = element_blank(),  axis.text.y = element_blank(), axis.line.x = element_blank(), axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"), panel.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) %>%
    + stat_density(adjust = 1.5, fill = '#bbbbbb', stroke = 0)  %>%
    + geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) %>%  
    + scale_y_continuous(breaks = seq(0, 3.5, by = 0.5), limits = c(0, 3.5)) %>%
    + scale_x_continuous(breaks = seq(-6, 6, by = 2), limits = c(-6.25,6.25)) %>%
    + scale_color_manual(values = cblues) %>%
    + xlab("") %>%
    + coord_flip()
  
  pdf(paste(paste('img/', target_y, sep = ""), '.pdf', sep = ""), width = 6, height = 3, useDingbats=FALSE)
  multiplot(dots, density, cols = 2)
  dev.off()
}

myRsq <- function(obs, pred, yavg){
  sstot <- sum((obs - yavg)^2)
  ssres <- sum((obs-pred)^2)
  return (1 - ssres/sstot)
}

# adapt from the gamlss code
Rsq <- function (object, type = c("Cox Snell", "Cragg Uhler", "both")) 
{
  type <- match.arg(type)
  if (!is.gamlss(object)) 
    stop("this is design for gamlss objects only")
  Y <- if (object$family[1] %in% .gamlss.bi.list) 
    cbind(object$y, object$bd - object$y)
  else object$y
  suppressWarnings(m0 <- gamlss(Y ~ 1, family = object$family[1]))
  #######################################################################
  # gamlssML -> gamlss
  # the original code is m0 <- gamlssML(Y ~ 1, family = object$family[1])
  # however, this results in an error "mu must be positive"
  # according to their doc at https://cran.r-project.org/web/packages/gamlss/gamlss.pdf
  # using gamlss is valid
  # "The function gamlssML() could be for large data faster than the equivalent gamlss() function which is designed for regression type of models."
  #######################################################################
  rsq1 <- 1 - exp((2/object$N) * (logLik(m0)[1] - logLik(object)[1]))
  rsq2 <- rsq1/(1 - exp((2/object$N) * logLik(m0)[1]))
  if (type == "Cox Snell") 
    return(rsq1)
  if (type == "Cragg Uhler") 
    return(rsq2)
  if (type == "both") 
    return(list(CoxSnell = rsq1, CraggUhler = rsq2))
}

mySummary <- function(m, aggs){
  if (!is.gamlss(m)) 
    stop("this is design for gamlss objects only")
  resid <- residuals(m)
  print(paste("sk is ", skewness(resid)))
  print(paste("kur is ", kurtosis(resid)))
  print(shapiro.test(resid))
  resid_m <- residuals(m)
  fitted_m <- fitted(m)
  print(leveneTest(resid_m, aggs)) # only from gamlss!
  print(summary(m))
  print("R sq is")
  print(Rsq(m), type="both")
}

agg_fitted_values <- function(d, attr, x = 'vf_agg'){
  fitted_values <- aggregate(as.formula(paste(paste(attr, '~'),x)), data = d, mean)
  tmp <- c(NA, nrow(d))
  for(i in 1:nrow(d)){
    # print(subset(agg_fitted_values, vf_agg == d$vf_agg[i]))
    # t <- subset(fitted_values, vf_agg == d$vf_agg[i])[attr]
    tmp[i] <- subset(fitted_values, vf_agg == d$vf_agg[i])[attr][1,]
  }
  # print(tmp)
  return (tmp)
}

# adapt from gamlss code
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

lmDiagPlot<-function(model){
  p1<-ggplot(model, aes(.fitted, .resid))+geom_point()
  p1<-p1+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
  p1<-p1+xlab("Fitted values")+ylab("Residuals")
  p1<-p1+ggtitle("Residual vs Fitted Plot")+theme_bw()
  
  p2<-ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm = TRUE)
  p2<-p2+geom_abline(aes(qqline(.stdresid)))+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
  p2<-p2+ggtitle("Normal Q-Q")+theme_bw()
  
  p3<-ggplot(model, aes(.fitted, sqrt(abs(.stdresid))))+geom_point(na.rm=TRUE)
 
  p3<-p3+stat_smooth(method="loess", na.rm = TRUE)+xlab("Fitted Value")
  p3<-p3+ylab(expression(sqrt("|Standardized residuals|")))
  p3<-p3+ggtitle("Scale-Location")+theme_bw()
  
  p4<-ggplot(model, aes(seq_along(.cooksd), .cooksd))+geom_bar(stat="identity", position="identity")
  p4<-p4+xlab("Obs. Number")+ylab("Cook's distance")
  p4<-p4+ggtitle("Cook's distance")+theme_bw()
  print(3*mean(model$.cooksd))
  
  p5<-ggplot(model, aes(.hat, .stdresid))+geom_point(aes(size=.cooksd), na.rm=TRUE)
  p5<-p5+stat_smooth(method="loess", na.rm=TRUE)
  p5<-p5+xlab("Leverage")+ylab("Standardized Residuals")
  p5<-p5+ggtitle("Residual vs Leverage Plot")
  p5<-p5+scale_size_continuous("Cook's Distance", range=c(1,5))
  p5<-p5+theme_bw()+theme(legend.position="bottom")
  
  p6<-ggplot(model, aes(.hat, .cooksd))+geom_point(na.rm=TRUE)+stat_smooth(method="loess", na.rm=TRUE)
  p6<-p6+xlab("Leverage hii")+ylab("Cook's Distance")
  p6<-p6+ggtitle("Cook's dist vs Leverage hii/(1-hii)")
  p6<-p6+geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed")
  p6<-p6+theme_bw()
  
  return(list(rvfPlot=p1, qqPlot=p2, sclLocPlot=p3, cdPlot=p4, rvlevPlot=p5, cvlPlot=p6))
}



# adapt from https://github.com/markheckmann/ryouready/blob/master/R/qq-plot.R
# overwrite the default one
ggplot.qqnorm.spss <- function(x, plottype=2, line=TRUE,level = 0.95, xlim.worm = 4, ylim.worm = 1.0, 
                               l.col="black", ..., xtlims=c(-3.8,3.8),ytlims=c(-1.05,1.05), xticks=seq(-3, 3, by = 1.5), yticks=seq(-1.0, 1.0, by = 0.5), approach) 
{
  qq <- x
  x <- qq$x
  y <- qq$y
  main <- paste("Normal Q-Q plot of", qq$xname) 
  xlab <- "Unit normal quantile"
  ylab <- "Expected normal value"

  if (plottype == 2) {        # convert to detrended data
    main <-  paste("Detrended normal Q-Q plot of", qq$xname) 
    ylab <- "Difference between normalized residual\n and unit normal quantile"
    y <- y - x
  }
  
  lz <- -xlim.worm
  hz <- xlim.worm
  dz <- 0.05
  z <- seq(lz, hz, dz)
  p <- pnorm(z)
  se <- (1/dnorm(z)) * (sqrt(p * (1 - p)/length(qq$y)))
  low <- qnorm((1 - level)/2) * se
  high <- -low

  d <- data.frame(x, y)
  g <- ggplot2::ggplot() + theme(axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"), panel.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none") +
    geom_line(aes(x = z, y= low),  size = 0.3, linetype = "dashed") +
    geom_line(aes(x = z, y= high),  size = 0.3, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, size = 0.3, linetype = "dashed") + 
    ggplot2::geom_point(data=d, aes(x,y, color = approach), alpha = 0.5, size = 3.25, stroke = 0) + 
    scale_x_continuous(limits=xtlims,breaks=xticks) + #breaks=seq(-3, 3, by = 1), 
    scale_y_continuous(limits=ytlims,breaks=yticks) +
    ggplot2::xlab(xlab) + ggplot2::ylab(ylab) + ggplot2::ggtitle(main)
  
  if (line) {
    if (plottype == 2)          # detrended plot
      gline <- ggplot2::geom_abline(intercept = 0, slope=0, colour=l.col, size = 0.3, linetype = "dashed")
    else                        # standard plot
      gline <- ggplot2::geom_abline(intercept=0, slope=1, colour=l.col,  size = 0.3,linetype = "dashed")   # slope of 1
    g <- g + gline
  }  
  g
}

# https://github.com/markheckmann/ryouready/blob/master/R/qq-plot.R
qqnorm_spssmy <- function(x, standardize=F, method=4, 
                        ties.method="first") 
{ 
  xname <- deparse(substitute(x))
  x <- na.omit(x)
  methods <- c('Blom'=1, 'Rankit / Hazen'=2, 'Tukey'=3, 'Van der Waerden / Weibull'=4,
               'Benard and Bos-Levenbach'=5, 'Gringorten'=6, 'Yu and Huang'=7)
  method.name <- names(methods[method])
  #p <- qq_get_p(x, method=method, ties.method=ties.method)
  qq <- as.data.frame(qqnorm(x, plot = FALSE))
  y <- qq$y
  x <- qq$x 
  x.std <- as.vector(scale(x))
  y.std <- qq$y * sd(x) + mean(x)
  l <- list(x=x, y=y, x.std=x.std, y.std=y.std, 
            method.name=method.name,
            standardize=standardize,
            ties.method=ties.method,
            xname=xname)
  class(l) <- "qqnorm.spss"
  l
} 

qq_get_p <- function(x, method=1, ties.method = "average")
{
  if (method < 1 | method > 7) 
    stop("'method' must be a number between 1 and 7:", 
         "\n\t1 = Blom \n\t2 = Rankit / Hazen \n\t3 = Tukey",
         "\n\t4 = van de Waerden / Weibull \n\t5 = Benard and Bos-Levenbach",
         "\n\t6 = Gringorten\n\t7 = Yu and Huang", call. = FALSE) 
  n <- length(x) 
  #i <- order(order(x))   # to recreate original order from sorted data
  #xs <- sort(x)
  #r <- rank(x, ties.method = ties.method)
  #r <- order(order(x))  
  r <- x
  p <- switch(method,
              "1" = (r - 3/8) / (n + 1/4),      # Blom
              "2" = (r - 1/2) / n,              # Rankit / Hazen
              "3" = (r - 1/3) / (n + 1/3),      # Tukey   
              "4" = r / (n + 1),                # van de Waerden / Weibull
              "5" = (r - 3/10) / (n + 4/10),    # Benard and Bos-Levenbach
              "6" = (r - 0.44) / (n + 0.12),    # Gringorten
              "7" = (r - 0.326) / (n + 0.348))  # Yu and Huang 
  #p[i]
  p
}