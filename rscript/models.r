# all the models
# will be loaded by other scripts
m.ellipse_minor.log <- glm(grade ~ approach + r + log(ellipse_minor^2 + ellipse_minor), data = d, family = binomial(link = 'logit'), weights = d$weights)
m.ellipse_area.log <- glm(grade ~ approach + r + log(ellipse_area^2 + ellipse_area ), data = d, family = binomial(link = 'logit'), weights = d$weights)
m.dist_line_sd.log <- glm(grade ~ approach + r + log(dist_line_sd^2 + dist_line_sd), data = d, family = binomial(link = 'logit'), weights = d$weights)
# m.conf_bounding_box_perp.log <- glm(grade ~ approach + r + log(conf_bounding_box_perp^2 + conf_bounding_box_perp), data = d, family = binomial(link = 'logit'), weights = d$weights)

m.r_diff <- glm(grade ~ approach + r + r_diff, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.ellipse_major <- glm(grade ~ approach + r + ellipse_major, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.ellipse_minor <- glm(grade ~ approach + r + ellipse_minor, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.ellipse_area <- glm(grade ~ approach + r + ellipse_area, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.ellipse_ratio <- glm(grade ~ approach + r + ellipse_ratio, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.ellipse_ratio_reverse <- glm(grade ~ approach + r + ellipse_ratio_reverse, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.bounding_box_perp <- glm(grade ~ approach + r + bounding_box_perp, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.bounding_box_para <- glm(grade ~ approach + r + bounding_box_para, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.bounding_box_area <- glm(grade ~ approach + r + bounding_box_area, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.bounding_box_ratio <- glm(grade ~ approach + r + bounding_box_ratio, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.bounding_box_ratio_reverse <- glm(grade ~ approach + r + bounding_box_ratio_reverse, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.conf_bounding_box_perp <- glm(grade ~ approach + r + conf_bounding_box_perp, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.conf_bounding_box_para <- glm(grade ~ approach + r + conf_bounding_box_para, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.conf_bounding_box_ratio <- glm(grade ~ approach + r + conf_bounding_box_ratio, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.conf_bounding_box_area <- glm(grade ~ approach + r + conf_bounding_box_area, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.conf_bounding_box_ratio_reverse <- glm(grade ~ approach + r + conf_bounding_box_ratio_reverse, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.convexhull <- glm(grade ~ approach + r + convexhull, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.dist_line_avg <- glm(grade ~ approach + r + dist_line_avg, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.dist_line_sd <- glm(grade ~ approach + r + dist_line_sd, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.dist_line_skewness <- glm(grade ~ approach + r + dist_line_skewness, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.pairwise_dist_max <- glm(grade ~ approach + r + pairwise_dist_max, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.pairwise_dist_avg <- glm(grade ~ approach + r + pairwise_dist_avg, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.pairwise_dist_sd <- glm(grade ~ approach + r + pairwise_dist_sd, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.pairwise_dist_skewness <- glm(grade ~ approach + r + pairwise_dist_skewness, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.pairwise_dist_50 <- glm(grade ~ approach + r + pairwise_dist_50, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.pairwise_dist_75 <- glm(grade ~ approach + r + pairwise_dist_75, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.pairwise_dist_95 <- glm(grade ~ approach + r + pairwise_dist_95, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.knn3_avg <- glm(grade ~ approach + r + knn3_avg, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.knn3_sd <- glm(grade ~ approach + r + knn3_sd, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.knn3_skewness <- glm(grade ~ approach + r + knn3_skewness, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.knn5_avg <- glm(grade ~ approach + r + knn5_avg, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.knn5_sd <- glm(grade ~ approach + r + knn5_sd, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.knn5_skewness <- glm(grade ~ approach + r + knn5_skewness, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.knn7_avg <- glm(grade ~ approach + r + knn7_avg, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.knn7_sd <- glm(grade ~ approach + r + knn7_sd, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.knn7_skewness <- glm(grade ~ approach + r + knn7_skewness, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.knn9_avg <- glm(grade ~ approach + r + knn9_avg, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.knn9_sd <- glm(grade ~ approach + r + knn9_sd, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.knn9_skewness <- glm(grade ~ approach + r + knn9_skewness, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.mst_avg <- glm(grade ~ approach + r + mst_avg, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.mst_sd <- glm(grade ~ approach + r + mst_sd, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.mst_skewness <- glm(grade ~ approach + r + mst_skewness, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.dist_line_avg_reverse <- glm(grade ~ approach + r + dist_line_avg_reverse, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.dist_line_sd_reverse <- glm(grade ~ approach + r + dist_line_sd_reverse, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.dist_line_skewness_reverse <- glm(grade ~ approach + r + dist_line_skewness_reverse, data = d, family = binomial(link = 'logit'), weights = d$weights)
