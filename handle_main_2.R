# ---
# one test-run and drawing result
# 1: simulation 5 estimators with fixed parameters
# 2: draw
# ---

# 1: simulation 5 estimators with fixed parameters

setwd("~/git/CKMRmig/")
rm(list=ls())
#library(tidyverse)
start.time = proc.time()
options(readr.show_progress = FALSE)
options(scipen=1)
init_parent_pair_number_0 = 5000
init_parent_pair_number_1 = 500
sampled_child_number_0 = 100
sampled_child_number_1 = 50
sampled_father_number_0 = 50
sampled_father_number_1 = 25
sampled_mother_number_0 = 50
sampled_mother_number_1 = 25
migration_number = 250 # true migrant number is multiplied twice
migration_rate = migration_number/init_parent_pair_number_0
lambda_0 = 3
lambda_1 = 7
flag_constant = 0
flag_invasive = 1
REP = 1000
HS_01 = PO_0 = PO_1 = PO_01 = pi_est_PO = pi_est_HS = M_est0_PO = M_est0_HS = M_est0_both = M_est1_both= M_est2_both = N_0_est = N_1_est = m_est1 = m_est2 = m_est3 = m_est4 = rep(NA, REP)
INLINE = paste("./model_3",
               init_parent_pair_number_0, 
               init_parent_pair_number_1,
               sampled_child_number_0,
               sampled_child_number_1,
               sampled_father_number_0,
               sampled_father_number_1,
               sampled_mother_number_0,
               sampled_mother_number_1,
               migration_number,
               lambda_0,
               lambda_1, 
               flag_constant, 
               flag_invasive, collapse = " ")
system("g++ model_3.cpp -Wall -Wextra -o3 -std=c++17 -o model_3")
for (rep in 1:REP) {
  system(INLINE)
  
  # 1: HSP_between_12
  sample_child_0 = read.table("0sample_child.txt",sep = "\t",header = TRUE)
  sample_child_1 = read.table("1sample_child.txt",sep = "\t",header = TRUE)
  PS_tmp = MS_tmp = 0
  for (i in 1:sampled_child_number_0) {
    PS_tmp = PS_tmp + sum(sample_child_1$father %in% sample_child_0$father[i])
    MS_tmp = MS_tmp + sum(sample_child_1$mother %in% sample_child_0$mother[i])
  }
  HS_01[rep] = MS_tmp + PS_tmp
  
  # 2: POP_within_0
  sample_father_0 = read.table("0sample_father.txt",sep = "\t",header = TRUE)
  sample_mother_0 = read.table("0sample_mother.txt",sep = "\t",header = TRUE)
  FC_tmp = MC_tmp = 0
  for (i in 1:sampled_child_number_0) {
    FC_tmp = FC_tmp + sum(sample_father_0$id %in% sample_child_0$father[i])
    MC_tmp = MC_tmp + sum(sample_mother_0$id %in% sample_child_0$mother[i])
  }
  PO_0[rep] = FC_tmp + MC_tmp
  
  # 2: POP_within_1
  sampled_parent_number = 2
  sample_father_1 = read.table("1sample_father.txt",sep = "\t",header = TRUE)
  sample_mother_1 = read.table("1sample_mother.txt",sep = "\t",header = TRUE)
  FC_tmp = MC_tmp = 0
  for (i in 1:sampled_child_number_1) {
    FC_tmp = FC_tmp + sum(sample_father_1$id %in% sample_child_1$father[i])
    MC_tmp = MC_tmp + sum(sample_mother_1$id %in% sample_child_1$mother[i])
  }
  PO_1[rep] = FC_tmp + MC_tmp
  
  # 2: POP_between_12
  FC_tmp = MC_tmp = 0
  for (i in 1:sampled_child_number_0) {
    FC_tmp = FC_tmp + sum(sample_father_1$id %in% sample_child_0$father[i])
    MC_tmp = MC_tmp + sum(sample_mother_1$id %in% sample_child_0$mother[i])
  }
  PO_01[rep] = FC_tmp + MC_tmp
  
  pi_est_HS[rep] = (HS_01[rep]) / (sampled_child_number_0 * sampled_child_number_1) 
  pi_est_PO[rep] = (PO_01[rep]) / (sampled_child_number_0 * (sampled_father_number_1+sampled_mother_number_1))  
  N_0_est[rep] = (2*sampled_child_number_0 * (sampled_father_number_0+sampled_mother_number_0) ) / (PO_0[rep] + 1)
  N_1_est[rep] = (2*sampled_child_number_1 * (sampled_father_number_1+sampled_mother_number_1) ) / (PO_1[rep] + 1)
  M_est0_HS[rep] = pi_est_HS[rep] * (2*init_parent_pair_number_0) * (2*init_parent_pair_number_1) / 4
  M_est0_PO[rep] = pi_est_PO[rep] * (2*init_parent_pair_number_0) * (2*init_parent_pair_number_1) / 2
  M_est0_both[rep] = ( (2*init_parent_pair_number_0) * (2*init_parent_pair_number_1) / 2 / sampled_child_number_0 ) * ( (HS_01[rep]+PO_01[rep]) / (2*sampled_child_number_1 + (sampled_father_number_1+sampled_mother_number_1)) )
  #M_est_HS[rep] = pi_est_HS[rep] * N_0_est[rep] * N_1_est[rep] / 4
  #M_est_PO[rep] = pi_est_PO[rep] * N_0_est[rep] * N_1_est[rep] / 2
  M_est1_both[rep] = ( N_0_est[rep] * (2*init_parent_pair_number_1) / 2 / sampled_child_number_0 ) * ( (HS_01[rep]+PO_01[rep]) / (2*sampled_child_number_1 + (sampled_father_number_1+sampled_mother_number_1)) )
  M_est2_both[rep] = ( N_0_est[rep] * N_1_est[rep] / 2 / sampled_child_number_0 ) * ( (HS_01[rep]+PO_01[rep]) / (2*sampled_child_number_1 + (sampled_father_number_1+sampled_mother_number_1)) )
  
  m_est1[rep] = pi_est_HS[rep] * (2*init_parent_pair_number_1) / 4
  m_est2[rep] = pi_est_PO[rep] * (2*init_parent_pair_number_1) / 2
  m_est3[rep] = ( (2*init_parent_pair_number_1) / 2 / sampled_child_number_0 ) * ( (HS_01[rep]+PO_01[rep]) / (2*sampled_child_number_0 + (sampled_father_number_1+sampled_mother_number_1)) )
  m_est4[rep] = ( N_0_est[rep] / 2 / sampled_child_number_0 ) * ( (HS_01[rep]+PO_01[rep]) / (2*sampled_child_number_0 + (sampled_father_number_1+sampled_mother_number_1)) )
  
}
#hist(M_est_both)
#hist(M_est_HS)
#hist(M_est_PO)
cat("mean estimated migrant_number: ",mean(M_est0_HS), ", True migrant_number: ", migration_number*2,", CV of estimated migrant_number: ",(var(M_est0_HS))^0.5/mean(M_est0_HS), "\n")
cat("mean estimated migrant_number: ",mean(M_est0_PO), ", True migrant_number: ", migration_number*2,", CV of estimated migrant_number: ",(var(M_est0_PO))^0.5/mean(M_est0_PO), "\n")
cat("mean estimated migrant_number: ",mean(M_est0_both), ", True migrant_number: ", migration_number*2,", CV of estimated migrant_number: ",(var(M_est0_both))^0.5/mean(M_est0_both), "\n")
cat("mean estimated migrant_number: ",mean(M_est1_both), ", True migrant_number: ", migration_number*2,", CV of estimated migrant_number: ",(var(M_est1_both))^0.5/mean(M_est1_both), "\n")
cat("mean estimated migrant_number: ",mean(M_est2_both), ", True migrant_number: ", migration_number*2,", CV of estimated migrant_number: ",(var(M_est2_both))^0.5/mean(M_est2_both), "\n")

cat("mean estimated migrant_rate: ",mean(m_est1), ", True migrant_rate: ", migration_rate,", CV of estimated migrant_rate: ",(var(m_est1))^0.5/mean(m_est1), "\n")
cat("mean estimated migrant_rate: ",mean(m_est2), ", True migrant_rate: ", migration_rate,", CV of estimated migrant_rate: ",(var(m_est2))^0.5/mean(m_est2), "\n")
cat("mean estimated migrant_rate: ",mean(m_est3), ", True migrant_rate: ", migration_rate,", CV of estimated migrant_rate: ",(var(m_est3))^0.5/mean(m_est3), "\n")
cat("mean estimated migrant_rate: ",mean(m_est4), ", True migrant_rate: ", migration_rate,", CV of estimated migrant_rate: ",(var(m_est4))^0.5/mean(m_est4), "\n")

#end.time = proc.time()
#(end.time-start.time)
#save(list=ls(), file="out/res.Rdata")

# 2: draw
library(tidyverse)
theme_set(theme_classic(base_size = 12, base_family = "Helvetica"))
#setwd("~/git/CKMRmig/")
#rm(list=ls())
#load(file="out/res.Rdata")

df = tibble("M1" = M_est0_HS, 
            "M2" = M_est0_PO, 
            "M3" = M_est0_both, 
            "M4" = M_est1_both, 
            "M5" = M_est2_both) %>%
  gather(M1, M2, M3, M4, M5, key = "M", value = "value") %>%
  dplyr::mutate(relative_bias = (value - 2*migration_number) / (2*migration_number))

gp <- ggplot(df, aes(x = M, y = relative_bias)) +
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

if(0){
  df = tibble("m1" = m_est1, 
              "m2" = m_est2, 
              "m3" = m_est3, 
              "m4" = m_est4) %>%
    gather(m1, m2, m3, m4, key = "m", value = "value") %>%
    dplyr::mutate(relative_bias = (value - migration_rate) / (migration_rate))
  
  gp <- ggplot(df, aes(x = m, y = relative_bias)) +
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
}
end.time = proc.time()
(end.time-start.time)
#----------------------------------------------------------------------------------------------------
# 

