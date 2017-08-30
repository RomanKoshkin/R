require(jtrans)
require(nortest)

xtrans <- jtrans(df2$mu_N1, test = "ad.test", exclude_original = TRUE, z_lim = c(0.25, 1.25))
# ad.test(xtrans$transformed) # don't use it, because it is too sensitive to minute deviations 
                              # from the perfect normal distribution
lillie.test(xtrans$original)
lillie.test(xtrans$transformed)

xtrans <- jtrans(df2$mu_P1, test = "ad.test", exclude_original = TRUE, z_lim = c(0.25, 1.25))
# ad.test(xtrans$transformed) # don't use it, because it is too sensitive to minute deviations 
# from the perfect normal distribution
lillie.test(xtrans$original) # Kolmogorov-Smirnov test is more appropriate to large samples
lillie.test(xtrans$transformed)

