setwd("~/git/CKMRmig/")
rm(list=ls())
#library(tidyverse)
start.time = proc.time()
options(readr.show_progress = FALSE)
init_parent_pair_number = 500
sampled_number = 30
migration_rate = 0.3
lambda_1 = 3
lambda_2 = 10
REP = 100
m = M = PHS = MHS = rep(NA, REP)
INLINE = paste("./model_1",
               init_parent_pair_number, 
               sampled_number,
               migration_rate,
               lambda_1,
               lambda_2, collapse = " ")
system("g++ model_1.cpp -Wall -Wextra -o3 -std=c++17 -o model_1")
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
cat("Estimated mig rate: ",mean(m), ", True mig rate: ", migration_rate)
end.time = proc.time()
(end.time-start.time)