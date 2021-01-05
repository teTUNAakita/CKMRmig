setwd("~/git/CKMRmig/")
rm(list=ls())
library(tidyverse)
init_parent_pair_number = 50
sampled_number = 30
migration_rate = 0.3
lambda_1 = 5
lambda_2 = 5


REP = 100
m = M = PHS = MHS = rep(NA, REP)
INLINE = paste("./a.out",
               init_parent_pair_number, 
               sampled_number,
               migration_rate,
               lambda_1,
               lambda_2, collapse = " ")
system("make")
for (rep in 1:REP) {
  system(INLINE)
  
  sample_0 = read_tsv("0sample.txt",col_names = FALSE) %>% 
    dplyr::select(id = X1, father = X2, mother = X3)
  sample_1 = read_tsv("1sample.txt",col_names = FALSE) %>% 
    dplyr::select(id = X1, father = X2, mother = X3)
  
  PHS_tmp = MHS_tmp = 0
  for (i in 1:sampled_number) {
    PHS_tmp = PHS_tmp + sum(sample_1$father %in% sample_0$father[i])
    MHS_tmp = MHS_tmp + sum(sample_1$mother %in% sample_0$mother[i])
  }
  MHS[rep] = MHS_tmp
  #M[rep] = init_parent_pair_number * init_parent_pair_number * MHS_tmp / sampled_number / sampled_number
  #m[rep] = init_parent_pair_number * MHS_tmp / sampled_number / sampled_number
  m[rep] = 2 * init_parent_pair_number * (PHS_tmp + MHS_tmp) / 4 / sampled_number / sampled_number
}



if(0){
  tmp1 = c(1,2,3,4,5)
  tmp2 = c(2,3,6,7,3)
  sum = 0
  for (i in 1:5){
    sum = sum + sum(tmp2 %in% tmp1[i])
  }
  sum
}