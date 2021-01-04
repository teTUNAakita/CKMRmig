setwd("~/git/CKMRmig/")
rm(list=ls())
library(tidyverse)
init_parent_pair_number = 50
sampled_number = 20
migration_rate = 0.3
lambda_1 = 3
lambda_2 = 10


REP = 100
m = rep(NA,REP)
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
  
  PHS = MHS = 0
  for (i in 1:sampled_number) {
    PHS = PHS + sum(sample_1$father %in% sample_0$father[i])
    MHS = MHS + sum(sample_1$mother %in% sample_0$mother[i])
  }
  
  m[rep] = 2 * init_parent_pair_number * (PHS + MHS) / 4 / sampled_number / sampled_number
}