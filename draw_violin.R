rm(list=ls())

load(file="~/Dropbox/research/_output/CKMRmig/res.Rdata")
library(tidyverse)
theme_set(theme_classic(base_size = 12, base_family = "Helvetica"))
setwd("~/git/CKMRmig/")

p = 1 # set ID for parameter set
result_grid[p,]
migration_number = result_grid$N_0[p]/2 * result_grid$m[p]
df_tmp = tibble("M1" = M_est1[p,], 
            "M2" = M_est2[p,], 
            "M3" = M_est3[p,], 
            "M4" = M_est4[p,],
            "M5" = M_est5[p,]) %>%
  gather(M1, M2, M3, M4, M5, key = "M", value = "value") %>%
  dplyr::mutate(relative_bias = (value - 2*migration_number) / (2*migration_number))

gp <- ggplot(df_tmp, aes(x = M, y = relative_bias)) +
  geom_violin(adjust=1,trim=T) +
  geom_hline(yintercept=0,size=0.5,alpha=.5,linetype=1) +
  stat_summary(fun = "mean", size=2, geom="point",colour ="black",
               position=position_dodge(width = 0.9))+
  ylab("Relative bias") +
  coord_cartesian(ylim=c(-1,3)) +
  theme(axis.title = element_text(size=16), 
        axis.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10))
plot(gp)
