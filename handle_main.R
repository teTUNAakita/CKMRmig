setwd("~/git/CKMRmig/")
rm(list=ls())
library(tidyverse)
start.time = proc.time()
options(readr.show_progress = FALSE)
init_parent_pair_number = 500
sampled_number = 30
migration_rate = 0.3
lambda_1 = 5
lambda_2 = 5
REP = 1000
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
  sample_0 = read.table("0sample.txt",sep = "\t",header = TRUE)
  sample_1 = read.table("1sample.txt",sep = "\t",header = TRUE)
  PHS_tmp = MHS_tmp = 0
  for (i in 1:sampled_number) {
    PHS_tmp = PHS_tmp + sum(sample_1$father %in% sample_0$father[i])
    MHS_tmp = MHS_tmp + sum(sample_1$mother %in% sample_0$mother[i])
  }
  MHS[rep] = MHS_tmp
  m[rep] = 2 * init_parent_pair_number * (PHS_tmp + MHS_tmp) / 4 / sampled_number / sampled_number
}

end.time = proc.time()
(end.time-start.time)

if(0){
  tmp1 = c(1,2,3,4,5)
  tmp2 = c(2,3,6,7,3)
  sum = 0
  for (i in 1:5){
    sum = sum + sum(tmp2 %in% tmp1[i])
  }
  sum
}