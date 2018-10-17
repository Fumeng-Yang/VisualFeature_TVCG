m.ellipse_minor.log <- glm(grade ~as.factor(approach) + as.factor(r) + log(ellipse_minor^2 + 1), data = d, family = binomial(link = 'logit'), weights = d$weights)
m.ellipse_area.log <- glm(grade ~as.factor(approach) + as.factor(r) + log(ellipse_area^2 + 10), data = d, family = binomial(link = 'logit'), weights = d$weights)
m.dist_line_sd.log <- glm(grade ~as.factor(approach) + as.factor(r) + log(dist_line_sd^2 + 0.1), data = d, family = binomial(link = 'logit'), weights = d$weights)
m.conf_bounding_box_perp.log <- glm(grade ~as.factor(approach) + as.factor(r) + log(conf_bounding_box_perp^2 + 1), data = d, family = binomial(link = 'logit'), weights = d$weights)
m.r_diff.log <- glm(grade ~as.factor(approach) + as.factor(r) + log(r_diff^2 + 0.01), data = d, family = binomial(link = 'logit'), weights = d$weights)

d$ellipse_minor_pow03 <- (d$ellipse_minor)^0.3
d$ellipse_area_pow03 <- (d$ellipse_area)^0.3
d$dist_line_sd_pow03 <- (d$dist_line_sd)^0.3
d$conf_bounding_box_perp_pow03 <- (d$conf_bounding_box_perp)^0.3
d$r_diff_pow03 <- (d$r_diff)^0.3

m.ellipse_minor.pow03 <- glm(grade ~as.factor(approach) + as.factor(r) + ellipse_minor_pow03, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.ellipse_area.pow03 <- glm(grade ~as.factor(approach) + as.factor(r) + ellipse_area_pow03, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.dist_line_sd.pow03 <- glm(grade ~as.factor(approach) + as.factor(r) + dist_line_sd_pow03, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.conf_bounding_box_perp.pow03 <- glm(grade ~as.factor(approach) + as.factor(r) + conf_bounding_box_perp_pow03, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.r_diff.pow03 <- glm(grade ~as.factor(approach) + as.factor(r) + r_diff_pow03, data = d, family = binomial(link = 'logit'), weights = d$weights)

d$ellipse_minor_pow04 <- (d$ellipse_minor)^0.4
d$ellipse_area_pow04 <- (d$ellipse_area)^0.4
d$dist_line_sd_pow04 <- (d$dist_line_sd)^0.4
d$conf_bounding_box_perp_pow04 <- (d$conf_bounding_box_perp)^0.4
d$r_diff_pow04 <- (d$r_diff)^0.4

m.ellipse_minor.pow04 <- glm(grade ~as.factor(approach) + as.factor(r) + ellipse_minor_pow04, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.ellipse_area.pow04 <- glm(grade ~as.factor(approach) + as.factor(r) + ellipse_area_pow04, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.dist_line_sd.pow04 <- glm(grade ~as.factor(approach) + as.factor(r) + dist_line_sd_pow04, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.conf_bounding_box_perp.pow04 <- glm(grade ~as.factor(approach) + as.factor(r) + conf_bounding_box_perp_pow04, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.r_diff.pow04 <- glm(grade ~as.factor(approach) + as.factor(r) + r_diff_pow04, data = d, family = binomial(link = 'logit'), weights = d$weights)


d$ellipse_minor_pow05 <- (d$ellipse_minor)^0.5
d$ellipse_area_pow05 <- (d$ellipse_area)^0.5
d$dist_line_sd_pow05 <- (d$dist_line_sd)^0.5
d$conf_bounding_box_perp_pow05 <- (d$conf_bounding_box_perp)^0.5
d$r_diff_pow05 <- (d$r_diff)^0.5

m.ellipse_minor.pow05 <- glm(grade ~as.factor(approach) + as.factor(r) + ellipse_minor_pow05, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.ellipse_area.pow05 <- glm(grade ~as.factor(approach) + as.factor(r) + ellipse_area_pow05, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.dist_line_sd.pow05 <- glm(grade ~as.factor(approach) + as.factor(r) + dist_line_sd_pow05, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.conf_bounding_box_perp.pow05 <- glm(grade ~as.factor(approach) + as.factor(r) + conf_bounding_box_perp_pow05, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.r_diff.pow05 <- glm(grade ~as.factor(approach) + as.factor(r) + r_diff_pow05, data = d, family = binomial(link = 'logit'), weights = d$weights)


d$conf_bounding_box_perp_pow06 <- (d$conf_bounding_box_perp)^0.6
d$ellipse_minor_pow06 <- (d$ellipse_minor)^0.6
d$ellipse_area_pow06 <- (d$ellipse_area)^0.6
d$dist_line_sd_pow06 <- (d$dist_line_sd)^0.6
d$conf_bounding_box_perp_pow06 <- (d$conf_bounding_box_perp)^0.6
d$r_diff_pow06 <- (d$r_diff)^0.6

m.ellipse_minor.pow06 <- glm(grade ~as.factor(approach) + as.factor(r) + ellipse_minor_pow06, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.ellipse_area.pow06 <- glm(grade ~as.factor(approach) + as.factor(r) + ellipse_area_pow06, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.dist_line_sd.pow06 <- glm(grade ~as.factor(approach) + as.factor(r) + dist_line_sd_pow06, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.conf_bounding_box_perp.pow06 <- glm(grade ~as.factor(approach) + as.factor(r) + conf_bounding_box_perp_pow06, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.r_diff.pow06 <- glm(grade ~as.factor(approach) + as.factor(r) + r_diff_pow06, data = d, family = binomial(link = 'logit'), weights = d$weights)


d$conf_bounding_box_perp_pow5 <- (d$conf_bounding_box_perp)^5
d$ellipse_minor_pow5 <- (d$ellipse_minor)^5
d$ellipse_area_pow5 <- (d$ellipse_area)^5
d$dist_line_sd_pow5 <- (d$dist_line_sd)^5
d$conf_bounding_box_perp_pow5 <- (d$conf_bounding_box_perp)^5
d$r_diff_pow5 <- (d$r_diff)^5

m.ellipse_minor.pow5 <- glm(grade ~as.factor(approach) + as.factor(r) + ellipse_minor_pow5, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.ellipse_area.pow5 <- glm(grade ~as.factor(approach) + as.factor(r) + ellipse_area_pow5, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.dist_line_sd.pow5 <- glm(grade ~as.factor(approach) + as.factor(r) + dist_line_sd_pow5, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.conf_bounding_box_perp.pow5 <- glm(grade ~as.factor(approach) + as.factor(r) + conf_bounding_box_perp_pow5, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.r_diff.pow5 <- glm(grade ~as.factor(approach) + as.factor(r) + r_diff_pow5, data = d, family = binomial(link = 'logit'), weights = d$weights)

d$r_diff_pow <- (d$r_diff)^-1

d$conf_bounding_box_perp_pown2 <- (d$conf_bounding_box_perp)^-2
d$ellipse_minor_pown2 <- (d$ellipse_minor)^-2
d$ellipse_area_pown2 <- (d$ellipse_area)^-2
d$dist_line_sd_pown2 <- (d$dist_line_sd)^-2
d$conf_bounding_box_perp_pown2 <- (d$conf_bounding_box_perp)^-2
d$r_diff_pown2 <- (d$r_diff)^-2

m.ellipse_minor.pown2 <- glm(grade ~as.factor(approach) + as.factor(r) + ellipse_minor_pown2, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.ellipse_area.pown2 <- glm(grade ~as.factor(approach) + as.factor(r) + ellipse_area_pown2, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.dist_line_sd.pown2 <- glm(grade ~as.factor(approach) + as.factor(r) + dist_line_sd_pown2, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.conf_bounding_box_perp.pown2 <- glm(grade ~as.factor(approach) + as.factor(r) + conf_bounding_box_perp_pown2, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.r_diff.pown2 <- glm(grade ~as.factor(approach) + as.factor(r) + r_diff_pown2, data = d, family = binomial(link = 'logit'), weights = d$weights)

m.r_diff <- glm(grade ~as.factor(approach) + as.factor(r) + r_diff, data = d, family = binomial(link = 'logit'), weights = d$weights)
m.r_diff <- standardize(m.r_diff)
logs <- function(m){
  m <- standardize(m)
  print(exp(cbind(OR = coef(m), confint(m))))
  print(summary(m))
  print(pR2(m))
  print(coxtest(m.r_diff, m))
}