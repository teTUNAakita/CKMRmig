# ---
# simulation 11 estimators with various sets of fixed parameters
# ---


library(tidyverse)
setwd("~/git/CKMRmig/")
rm(list=ls())
start.time = proc.time()
options(readr.show_progress = FALSE)
options(scipen=5)

result_grid = tidyr::crossing(
  N_0 = c(1000, 10000, 100000), 
  N_1 = c(1000, 10000, 100000), 
  m = c(0.05, 0.1, 0.2),
  lambda_0 = 2,
  lambda_1 = 5,
  flag_constant = 0,
  flag_invasive = c(0, 1),
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
REP = 1000
system("g++ model_3.cpp -Wall -Wextra -o3 -std=c++17 -o model_3")
HS_01 = PO_0 = PO_1 = PO_01 = pi_est_PO = pi_est_HS = M_est1 = M_est2 = M_est3 = M_est4 = M_est5 = N_0_est = N_1_est = m_est1 = m_est2 = m_est3 = m_est4 = matrix(NA,nrow = para_len, ncol = REP)

for (p in 1:para_len) {
  INLINE = paste("./model_3",
                 result_grid$N_0[p]/2, #init_parent_pair_number_0, 
                 result_grid$N_1[p]/2, #init_parent_pair_number_1, 
                 result_grid$nO_0[p],#sampled_child_number_0,
                 result_grid$nO_1[p],#sampled_child_number_1,
                 result_grid$nP_0[p]/2,#sampled_father_number_0,
                 result_grid$nP_1[p]/2,#sampled_father_number_1,
                 result_grid$nP_0[p]/2,#sampled_mother_number_0,
                 result_grid$nP_1[p]/2,#sampled_mother_number_1,
                 result_grid$N_0[p]/2 * result_grid$m[p], #migration_number
                 result_grid$lambda_0[p],
                 result_grid$lambda_1[p],  
                 result_grid$flag_constant[p], 
                 result_grid$flag_invasive[p], collapse = " ")
  for (rep in 1:REP) {
    system(INLINE)
    
    init_parent_pair_number_0 = result_grid$N_0[p]/2 
    init_parent_pair_number_1 = result_grid$N_1[p]/2 
    sampled_child_number_0 = result_grid$nO_0[p]
    sampled_child_number_1 = result_grid$nO_1[p]
    sampled_father_number_0 = result_grid$nP_0[p]/2
    sampled_father_number_1 = result_grid$nP_1[p]/2
    sampled_mother_number_0 = result_grid$nP_0[p]/2
    sampled_mother_number_1 = result_grid$nP_1[p]/2
    migration_number = result_grid$N_0[p]/2 * result_grid$m[p]
    
    # 1: HSP_between_12
    sample_child_0 = read.table("0sample_child.txt",sep = "\t",header = TRUE)
    sample_child_1 = read.table("1sample_child.txt",sep = "\t",header = TRUE)
    PS_tmp = MS_tmp = 0
    for (i in 1:sampled_child_number_0) {
      PS_tmp = PS_tmp + sum(sample_child_1$father %in% sample_child_0$father[i])
      MS_tmp = MS_tmp + sum(sample_child_1$mother %in% sample_child_0$mother[i])
    }
    HS_01[p,rep] = MS_tmp + PS_tmp
    
    # 2: POP_within_0
    sample_father_0 = read.table("0sample_father.txt",sep = "\t",header = TRUE)
    sample_mother_0 = read.table("0sample_mother.txt",sep = "\t",header = TRUE)
    FC_tmp = MC_tmp = 0
    for (i in 1:sampled_child_number_0) {
      FC_tmp = FC_tmp + sum(sample_father_0$id %in% sample_child_0$father[i])
      MC_tmp = MC_tmp + sum(sample_mother_0$id %in% sample_child_0$mother[i])
    }
    PO_0[p,rep] = FC_tmp + MC_tmp
    
    # 2: POP_within_1
    sampled_parent_number = 2
    sample_father_1 = read.table("1sample_father.txt",sep = "\t",header = TRUE)
    sample_mother_1 = read.table("1sample_mother.txt",sep = "\t",header = TRUE)
    FC_tmp = MC_tmp = 0
    for (i in 1:sampled_child_number_1) {
      FC_tmp = FC_tmp + sum(sample_father_1$id %in% sample_child_1$father[i])
      MC_tmp = MC_tmp + sum(sample_mother_1$id %in% sample_child_1$mother[i])
    }
    PO_1[p,rep] = FC_tmp + MC_tmp
    
    # 2: POP_between_12
    FC_tmp = MC_tmp = 0
    for (i in 1:sampled_child_number_0) {
      FC_tmp = FC_tmp + sum(sample_father_1$id %in% sample_child_0$father[i])
      MC_tmp = MC_tmp + sum(sample_mother_1$id %in% sample_child_0$mother[i])
    }
    PO_01[p,rep] = FC_tmp + MC_tmp
    
    pi_est_HS[p,rep] = (HS_01[p,rep]) / (sampled_child_number_0 * sampled_child_number_1) 
    pi_est_PO[p,rep] = (PO_01[p,rep]) / (sampled_child_number_0 * (sampled_father_number_1+sampled_mother_number_1))  
    N_0_est[p,rep] = (2*sampled_child_number_0 * (sampled_father_number_0+sampled_mother_number_0) + 1) / (PO_0[p,rep] + 1)
    N_1_est[p,rep] = (2*sampled_child_number_1 * (sampled_father_number_1+sampled_mother_number_1) + 1) / (PO_1[p,rep] + 1)
    
    M_est1[p,rep] = pi_est_HS[p,rep] * (2*init_parent_pair_number_0) * (2*init_parent_pair_number_1) / 4
    M_est2[p,rep] = pi_est_PO[p,rep] * (2*init_parent_pair_number_0) * (2*init_parent_pair_number_1) / 2
    M_est3[p,rep] = ( (2*init_parent_pair_number_0) * (2*init_parent_pair_number_1) / 2 / sampled_child_number_0 ) * ( (HS_01[p,rep]+PO_01[p,rep]) / (2*sampled_child_number_1 + (sampled_father_number_1+sampled_mother_number_1)) )
    #M_est_HS[rep] = pi_est_HS[rep] * N_0_est[rep] * N_1_est[rep] / 4
    #M_est_PO[rep] = pi_est_PO[rep] * N_0_est[rep] * N_1_est[rep] / 2
    M_est4[p,rep] = ( N_0_est[p,rep] * (2*init_parent_pair_number_1) / 2 / sampled_child_number_0 ) * ( (HS_01[p,rep]+PO_01[p,rep]) / (2*sampled_child_number_1 + (sampled_father_number_1+sampled_mother_number_1)) )
    M_est5[p,rep] = ( N_0_est[p,rep] * N_1_est[p,rep] / 2 / sampled_child_number_0 ) * ( (HS_01[p,rep]+PO_01[p,rep]) / (2*sampled_child_number_1 + (sampled_father_number_1+sampled_mother_number_1)) )
    
    m_est1[p,rep] = pi_est_HS[p,rep] * (2*init_parent_pair_number_1) / 4
    m_est2[p,rep] = pi_est_PO[p,rep] * (2*init_parent_pair_number_1) / 2
    m_est3[p,rep] = ( (2*init_parent_pair_number_1) / 2 / sampled_child_number_0 ) * ( (HS_01[p,rep]+PO_01[p,rep]) / (2*sampled_child_number_1 + (sampled_father_number_1+sampled_mother_number_1)) )
    m_est4[p,rep] = ( N_0_est[p,rep] / 2 / sampled_child_number_0 ) * ( (HS_01[p,rep]+PO_01[p,rep]) / (2*sampled_child_number_1 + (sampled_father_number_1+sampled_mother_number_1)) )
  }
  cat(p,"/",para_len,INLINE,"\n")
}
end.time = proc.time()
(end.time-start.time)
save(list=ls(), file="out/res.Rdata")
