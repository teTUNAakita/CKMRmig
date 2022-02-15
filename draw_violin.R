rm(list=ls())

load(file="~/Dropbox/research/_output/CKMRmig/res.Rdata")
library(tidyverse)
theme_set(theme_classic(base_size = 12, base_family = "Helvetica"))
setwd("~/git/CKMRmig/")

#1. Only one parameter set is shown

p = 5 # set ID for parameter set
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

#2. Nine parameter sets are shown by grid (Figure 2)

out_grid = result_grid %>% 
  tibble::rownames_to_column(var = "rowname") %>%
  dplyr::filter(flag_invasive==1)
for (j in 1:9) {
  
  if (j==1|j==2|j==3) {
    tmp_use_grid = dplyr::filter(out_grid, (N_0==10^3)&(N_1==10^3) )
  } else if (j==4|j==5|j==6) {
    tmp_use_grid = dplyr::filter(out_grid, (N_0==10^4)&(N_1==10^4) )
  } else {
    tmp_use_grid = dplyr::filter(out_grid, (N_0==10^5)&(N_1==10^5) )
  }
  
  if (j==1|j==4|j==7) {
    tmp_use_grid = dplyr::filter(tmp_use_grid, m==0.05)
  } else if (j==2|j==5|j==8) {
    tmp_use_grid = dplyr::filter(tmp_use_grid, m==0.1)
  }  else {
    tmp_use_grid = dplyr::filter(tmp_use_grid, m==0.2)
  }
  
  use_id = tmp_use_grid %>%
    dplyr::select(rowname) %>%
    dplyr::pull() %>% as.numeric()  
  n_index = tmp_use_grid %>%
    dplyr::select(nP_0) %>%
    dplyr::pull() %>% as.numeric() 
  migration_number = tmp_use_grid$N_0[1]/2 * tmp_use_grid$m[1]
  
  for ( i in 1:5){
    tmp_use_M_est = get(paste0("M_est",i))[use_id,]
    rownames(tmp_use_M_est) = paste0("n = ",n_index)
    df = tmp_use_M_est %>% t() %>% as_tibble() 
    tb_tmp = tidyr::gather(df) %>% 
      dplyr::mutate(M = paste0("M",i)) %>%
      dplyr::mutate(relative_bias = (value - 2*migration_number) / (2*migration_number))
    assign(paste0("tb", i), tb_tmp)
  }
  tb = dplyr::full_join(tb1,tb2) %>%
    dplyr::full_join(tb3) %>%
    dplyr::full_join(tb4) %>%
    dplyr::full_join(tb5)
  
  gp <- ggplot(tb, aes(x = as_factor(M), y = relative_bias, fill = as_factor(key))) +
    scale_fill_grey(start = 0.6, end = 0.9) + theme_classic() +
    geom_violin(adjust=1,trim=T) +
    geom_hline(yintercept=0,size=0.5,alpha=.5,linetype=1) +
    stat_summary(fun = "mean", size=2, geom="point",colour ="black",
                 position=position_dodge(width = 0.9))+
    xlab("Estimator of migration number") +
    ylab("Relative bias") + labs(fill="Sample size: ") +
    coord_cartesian(ylim=c(-1,3)) +
    theme(axis.title = element_text(size=12), 
          axis.text = element_text(size=10),
          legend.title = element_text(size=10),
          legend.text = element_text(size=8),
          legend.position="top"
          )
  assign(paste0("gp", j), gp)
}

gp_grid = gridExtra::grid.arrange(gp1, gp2, gp3, gp4, gp5, gp6, gp7, gp8, gp9, nrow = 3)
ggsave("~/Dropbox/research/_output/CKMRmig/res2.pdf", gp_grid, width = 20, height = 10, dpi = 300)
