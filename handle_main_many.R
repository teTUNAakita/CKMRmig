# ---
# many-run and drawing result
# 1: simulation 6 estimators with various sets of fixed parameters
# 2: draw
# ---

# 1: simulation 6 estimators with various sets of fixed parameters

library(tidyverse)
setwd("~/git/CKMRmig/")
rm(list=ls())
#start.time = proc.time()
options(readr.show_progress = FALSE)
options(scipen=5)

result_grid = tidyr::crossing(
  N_0 = c(1000, 10000, 100000), 
  N_1 = c(1000, 10000, 100000), 
  m = c(0.05, 0.1, 0.2),
  lambda_0 = 2,
  lambda_1 = 5,
  flag_constant = c(0, 1),
  nP_0 = c(50,100,200,400,800,1600),
  nP_1 = c(50,100,200,400,800,1600),
  nO_0 = c(50,100,200,400,800,1600),
  nO_1 = c(50,100,200,400,800,1600)
) %>% 
  dplyr::filter((N_0==1000 & nP_0<=200 & nO_0<=200)
                | (N_0==10000 & nP_0>=100 & nP_0<=400 & nO_0>=100 & nO_0<=400)
                | (N_0==100000 & nP_0>=400 & nO_0>=400)
                ) %>%
  dplyr::filter((N_1==1000 & nP_1<=200 & nO_1<=200)
                | (N_1==10000 & nP_1>=100 & nP_1<=400 & nO_1>=100 & nO_1<=400)
                | (N_1==100000 & nP_1>=400 & nO_1>=400)
                ) %>%
  dplyr::filter((N_0==N_1 & nP_0==nP_1 & nO_0==nO_1)
                | (N_0==10000 & N_1==1000 & nP_0==(2*nP_1) & nO_0==(2*nO_1))
                | (N_0==100000 & N_1==1000 & nP_0==(8*nP_1) & nO_0==(8*nO_1))
                | (N_0==100000 & N_1==10000 & nP_0==(4*nP_1) & nO_0==(4*nO_1))
  ) %>%
  dplyr::filter((nP_0==nO_0) & (nP_1==nO_1)) %>%
  dplyr::filter(N_0>=N_1)
para_len <- nrow(result_grid)
REP = 100
system("g++ model_2.cpp -Wall -Wextra -o3 -std=c++17 -o model_2")
HS_01 = PO_0 = PO_1 = PO_01 = pi_est_PO = pi_est_HS = M_est1 = M_est2 = M_est3 = M_est4 = M_est5 = N_0_est = N_1_est = m_est1 = m_est2 = m_est3 = m_est4 = matrix(NA,nrow = para_len, ncol = REP)

for (i in 1:para_len) {
#  kokomade!!!!!!!!!!!
  
  
  
  
}








init_parent_pair_number_0 = init_parent_pair_number_1 = 1000
sampled_child_number_0 = sampled_child_number_1 = 100
sampled_father_number_0 = sampled_father_number_1 = 50
sampled_mother_number_0 = sampled_mother_number_1 = 50
migration_number = 100 # true migrant number is multiplied twice
migration_rate = migration_number/init_parent_pair_number_0
lambda_0 = 3
lambda_1 = 3
flag_constant = 1
REP = 100