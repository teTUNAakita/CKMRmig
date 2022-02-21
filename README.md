<!--
ctrl + shift + M: show preview
-->
# CKMRmig
**CKMRmig** is a package which is used for a scientific paper, entitiled "Estimating contemporary migration numbers of adults for iteroparous species based on kinship relationships found between populations" by Tetsuya Akita, deposited in https://www.biorxiv.org/. This package contains individual-based model, pipeline to conduct the simulator, and visualization code for the paper.  

## Individual-based model (model_3)
The individual-based model that tracks (Half-Sibling) HS and (Parent-Offspring) PO relationships where the population 1 and 2 comprised N1 and N2 parents with an equal sex ratio, and their offspring number was assumed to follow the geometric distribution with mean lambda1 and lambda2, respectively. Migrant parents were randomly chosen from the population 1 at the end of the first year. Each offspring retained the parent's ID, making it possible to trace an HS and PO relationship.

## Download and compilation
You can download the most up to date version of **CKMRmig** via GitHub:
```
git clone https://github.com/teTUNAakita/CKMRmig
```

The source codes of the program are written in C++ and R, and these program is intended to be run on UNIX, or a UNIX-like operating systems, such as Linux or Mac OS X. After downloading, change the directory by: `cd CKMRmig`; and then compile the program:
```
g++ model_3.cpp -Wall -Wextra -o3 -std=c++17 -o model_3
```

## Running model_3
The following command line shows the simplest usage of model_3  (total 13 parameters):
```
./model_3 init_parent_pair_number_0 init_parent_pair_number_1 sampled_child_number_0 sampled_child_number_1 sampled_father_number_0 sampled_father_number_1 sampled_mother_number_0 sampled_mother_number_1 migration_pair_number lambda_0 lambda_1 flag_constant  flag_invasive-sampling
```
Subscript 0 and 1 indicates the population 1 and 2, respectively.

Here is an example of a command line:
```
./model_3 10 10 3 3 2 2 2 2 3 3 5 0 1
```
In this example, N1 = 10 * 2 = 20, N2 =  10 * 2 = 20, n_O1 = 3, n_O2 = 3, n_P1 = 2(father) + 2(mother) = 4, n_P2 = 2(father) + 2(mother) = 4, M = 3 * 2 = 6, lambda1 = 3, lambda1 = 5 and assuming equal sex ratio, variable offspring number (flag_constant = 0), and invasive-sampling (flag_invasive-sampling). Currently, no-equal sex ratio option is not implemented in the model.

## Outputs
There are six output files (.txt), which is named `0sample_child.txt`,`0sample_father.txt`,`0sample_mother.txt`,`1sample_child.txt`,`0sample_father.txt`, and `0sample_mother.txt`. Initial number of these file names indicates the sampled population number.  The output format of `Xsample_child.txt` is as follows :
```
id	father	mother
100074	39216	50037
100140	22894	50075
100172	43194	50100
100241	13337	50121
```
The first column is ID of sampled offspring. The second and third columns are IDs of the father and mother of sampled offspring with the ID noted in the first column. The output format of `Xsample_father.txt` is as follows :
```
id
94
130
231
321
354
```
This is the ID of sampled father. `Xsample_mother.txt` shows similar file structure.

## Pipeline
### handle_main_2.R
This file compiles and runs `model_3` repeatedly (1000 times). Then, it reads the outputs and draws violin plots of M1-M5 estimators for a specified parameter set.  

### handle_main_many.R
This file covers many of parameter sets. The outputs are used for Table S1 and Figure 2 (partially). It took over two weeks by a single-thread execute in my environment (2.7 GHz, 64GB), although it does not take time except the settings of a very large N (e.g., N = 100,000)

### draw_violin.R
This file draws several violin plots for specified parameter sets from the outputs, which is used for Figure 2.

### make_summary_table.R
This file makes a summary table as .CSV file (Table S1) from the outputs.  
