# the helper function in explaining section

source('helper.r')

# read values
d <- read.csv('../publicdata/judgments_data.csv', stringsAsFactors = F)

# remove extreme values
d <- subset(d, rv < 1)

rs <- c("0.3", "0.4", "0.5", "0.6", "0.7", "0.8")
apps <- c("above", "below")
vfs <- c("ellipse_minor", "ellipse_area", "conf_bounding_box_perp", "dist_line_sd")
cblues <- c("#0066cc", "#f9b703")#

color_rensink <- "#000000"
color_harrison <- "#000000"
color_kay <- "#ec5489"
color_red <- "#ec5489"
color_sub <- "gray"
color_lastone <- "steelblue"
color_logno <- "#000000"

lambda_r <- 0.26
lambda_vf <- -0.17

indi <- read.csv('../data/individual_jnd.csv')
target_vf_ <- 'dist_line_sd'
target_vf <- 'dist_line_sd'

df <- read.csv('../data/individual_jnd.csv', header = T)
# df <- df[-c(1221),] #outlier
t_df <- subset(df, vf == target_vf)
t_df$r <- as.numeric(t_df$r)
t_df$a <- code_a(t_df$approach)
t_df$a_r <- t_df$a * as.numeric(t_df$r)
t_df$a_base_50 <- t_df$a * as.numeric(t_df$base_50)
t_df$a_base_24 <- t_df$a * as.numeric(t_df$base_24)

t_df$mad_dist <- double_mad_from_median(t_df$jnd_50_r) # compute double mad
t_vf_df <- subset(t_df, mad_dist < 3) # filter based on double mad # not used

vf_agg <- t_df$base_50_agg