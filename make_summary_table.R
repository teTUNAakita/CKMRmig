rm(list=ls())

load(file="~/Dropbox/research/_output/CKMRmig/res.Rdata")
library(tidyverse)
theme_set(theme_classic(base_size = 12, base_family = "Helvetica"))
setwd("~/git/CKMRmig/")

# migration_number = result_grid$N_0[p]/2 * result_grid$m[p]
# dplyr::mutate(relative_bias = (value - 2*migration_number) / (2*migration_number))

summary_table = result_grid %>%
  dplyr::mutate(migration_number = N_0 * m) %>% # this is not input migration_number (not divided by 2)
  dplyr::mutate(M_mean1 = apply(M_est1, 1, mean)) %>%
  dplyr::mutate(M_mean2 = apply(M_est2, 1, mean)) %>%
  dplyr::mutate(M_mean3 = apply(M_est3, 1, mean)) %>%
  dplyr::mutate(M_mean4 = apply(M_est4, 1, mean)) %>%
  dplyr::mutate(M_mean5 = apply(M_est5, 1, mean)) %>%
  dplyr::mutate(m_mean1 = apply(m_est1, 1, mean)) %>%
  dplyr::mutate(m_mean2 = apply(m_est2, 1, mean)) %>%
  dplyr::mutate(m_mean3 = apply(m_est3, 1, mean)) %>%
  dplyr::mutate(m_mean4 = apply(m_est4, 1, mean)) %>%
  dplyr::mutate(M_var1 = apply(M_est1, 1, var)) %>%
  dplyr::mutate(M_var2 = apply(M_est2, 1, var)) %>%
  dplyr::mutate(M_var3 = apply(M_est3, 1, var)) %>%
  dplyr::mutate(M_var4 = apply(M_est4, 1, var)) %>%
  dplyr::mutate(M_var5 = apply(M_est5, 1, var)) %>%
  dplyr::mutate(m_var1 = apply(m_est1, 1, var)) %>%
  dplyr::mutate(m_var2 = apply(m_est2, 1, var)) %>%
  dplyr::mutate(m_var3 = apply(m_est3, 1, var)) %>%
  dplyr::mutate(m_var4 = apply(m_est4, 1, var)) %>%
  dplyr::mutate(M_bias1 = (M_mean1 - migration_number) / migration_number) %>%
  dplyr::mutate(M_bias2 = (M_mean2 - migration_number) / migration_number) %>%
  dplyr::mutate(M_bias3 = (M_mean3 - migration_number) / migration_number) %>%
  dplyr::mutate(M_bias4 = (M_mean4 - migration_number) / migration_number) %>%
  dplyr::mutate(M_bias5 = (M_mean5 - migration_number) / migration_number) %>%
  dplyr::mutate(m_bias1 = (m_mean1 - m) / m) %>%
  dplyr::mutate(m_bias2 = (m_mean2 - m) / m) %>%
  dplyr::mutate(m_bias3 = (m_mean3 - m) / m) %>%
  dplyr::mutate(m_bias4 = (m_mean4 - m) / m) %>%
  dplyr::mutate(M_cv1 = sqrt(M_var1) / M_mean1) %>%
  dplyr::mutate(M_cv2 = sqrt(M_var2) / M_mean2) %>%
  dplyr::mutate(M_cv3 = sqrt(M_var3) / M_mean3) %>%
  dplyr::mutate(M_cv4 = sqrt(M_var4) / M_mean4) %>%
  dplyr::mutate(M_cv5 = sqrt(M_var5) / M_mean5) %>%
  dplyr::mutate(m_cv1 = sqrt(m_var1) / m_mean1) %>%
  dplyr::mutate(m_cv2 = sqrt(m_var2) / m_mean2) %>%
  dplyr::mutate(m_cv3 = sqrt(m_var3) / m_mean3) %>%
  dplyr::mutate(m_cv4 = sqrt(m_var4) / m_mean4)

summary_table_final = summary_table %>%
  dplyr::select(N_0,N_1,m,migration_number,flag_constant,nP_0,nP_1,nO_0,nO_1,M_bias1,M_cv1,M_bias2,M_cv2,M_bias3,M_cv3,M_bias4,M_cv4,M_bias5,M_cv5)
write_csv(summary_table_final, file="~/Dropbox/research/_output/CKMRmig/big_table.csv")
