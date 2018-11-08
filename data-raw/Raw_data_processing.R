#Raw_data_processing
#Nov6_raw_data
library(openxlsx)
devtools::use_data_raw()

#raw data processing

support_list<-list()
strains<-c("UCBPP","X13273","PS75","CF77","BWH015","BWH013","BWH005","BL23","19660")
for(i in seq_len(length(strains))){
  sts<-strains[i]
  print(sts)
  strain=st=strains[i]
  supp<-list.files("data-raw")
  homo<-paste("data-raw/50bp_homologous_TAsites_",st,"_easy_method.txt",sep="") #difficult method for UCBPP
  homo<-read.delim(homo,header = FALSE)
  np<-supp[grep(strain,supp)][grep("nonpermissive",supp[grep(strain,supp)])]
  nonPermissiveTA<-read.delim(paste("data-raw",np,sep="/"),header = FALSE)
  gnlist<-supp[grep("genelist",supp)][grep(strain,supp[grep("genelist",supp)])]
  genelist<-read.table(paste("data-raw",gnlist,sep="/"))
  strclu<-list.files("data-raw")
  strclu<-strclu[grep("strand_type",strclu)]
  strclu<-strclu[grep(st,strclu)]
  geneinfo<-read.table(paste("data-raw",strclu,sep="/"))
  support_list[[i]]<-list(nonPermissiveTA,homo,genelist,geneinfo)
}
names(support_list)<-strains
# save(support_list,file="data/support_files.RData")
devtools::use_data(support_list)

#prepare cluster data

cluster<-read.xlsx("data-raw/clusters_compiled.xlsx",sheet = 1, startRow = 1, colNames = TRUE)
cluster$strain<-gsub("Pseudomonas aeruginosa ","",cluster$Genome)
cluster$strain<-gsub("PSA","",cluster$strain)
colnames(cluster)<-c("gene_name","patric_id","Locus.CIA","length","cluster","desc","strain")
cluster$strain[which(cluster$strain=="UCBPP-PA14")]<-"UCBPP"
devtools::use_data(cluster)

#raw tally file example

rawtally<-read.table("data-raw/raw_tally_example.txt")
rawtally<-rawtally[c(1:10,100:110),]
devtools::use_data(rawtally)


