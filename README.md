# _FiTnEss_
Finding Tn-Seq Essential genes (_FiTnEss_)

_FiTnEss_ is a package using Transposon insertion sequencing data to identify essential genes in the genome. 

Original paper on bioRxiv: [Defining the core essential genome of Pseudomonas aeruginosa](https://www.biorxiv.org/content/early/2019/01/12/396689)


#### Quick start

After installing FiTnEss package, run main FiTnEss function by ```FiTnEss_Run```

Arguments in this function include: 
- _strain_
- _file_location_: path and name of tally file for run: 
e.g. "/home/your_folder/your_tally.txt"
- path and name of non-permissive TA site file that generated from genomic pre-processing step: 
e.g. "/home/your_folder/non_permissive_TA_sites.txt"
- path and name of homologous TA site file that generated from pre-processing step: 
e.g. "/home/your_folder/homologous_TA_sites.txt"
- path and name of GFF3 gene file that downloaded from [Pseudomonas Genome Database](http://www.pseudomonas.com/strain/show?id=109): 
e.g. "/home/your_folder/PA14_gene_file.txt"
- path and name of where to save final results file: 
e.g. "/home/results_folder/results.xlsx"
- how many times to run the pipeline in order to obtain best results: by default, we run 3 times.

```
devtools::install_github("ruy204/FiTnEss")
require(FiTnEss)
library(dplyr)
library(tidyr)
library(openxlsx)
FiTnEss_Run("UCBPP",
            "/home/TnSeq/data/test_data/PA14_M9_rep1_tally.txt",
            "/home/TnSeq/data/test_data/nonpermissive_TA_sites.txt",
            "/home/TnSeq/data/test_data/homologous_TA_sites.txt",
            "/home/TnSeq/data/test_data/PA14_gene_file.txt",
            "/home/TnSeq/test_result/test_results.xlsx",
            repeat_time = 3)
```















