#tallyprepfun

#1. usable tally file preparation

tallyprepfun<-function(strain,file_location){

  ####################### Usable tally file preparation #######################

  print("Starting Phase I: preparing usable tally files")

  usable_tally_list<-list()
  file_location_list<-unlist(strsplit(file_location,","))

  #1. load in supporting files

  if (strain %in% c("UCBPP","PA14","pa14","ucbpp")){
    strain<-"UCBPP"
    st<-"UCBPP-PA14"
    supp<-list.files("/idi/hunglabusers/poulsen/pmar_allstrains/UCBPP/rui")
    supp<-supp[!supp %in% supp[grep("_tally.txt",supp)]]
    homo<-read.delim("/idi/hunglabusers/poulsen/pmar_allstrains/UCBPP/rui/50bp_homologous_TAsites_UCBPP_difficult_method.txt",header = FALSE)
    np<-supp[grep("nonpermissive",supp[grep(strain,supp)])]
    nonPermissiveTA<-read.delim(paste("/idi/hunglabusers/poulsen/pmar_allstrains/UCBPP/rui",np,sep="/"),header = FALSE)
    print(st)
    print(np)
  }else{
    supp<-list.files("/idi/hunglabusers/poulsen/pmar_allstrains/LB_20171130/new_fasta_alignments/Rui_analysis_files/")
    st<-strain
    homo<-paste("/idi/hunglabusers/poulsen/pmar_allstrains/LB_20171130/new_fasta_alignments/Rui_analysis_files/new_homo/50bp_homologous_TAsites_",st,"_easy_method.txt",sep="")
    homo<-read.delim(homo,header = FALSE)
    np<-supp[grep(strain,supp)][grep("nonpermissive",supp[grep(strain,supp)])]
    nonPermissiveTA<-read.delim(paste("/idi/hunglabusers/poulsen/pmar_allstrains/LB_20171130/new_fasta_alignments/Rui_analysis_files",np,sep="/"),header = FALSE)
    print(st)
    print(np)
  }

  cluster<-read.xlsx("/idi/hunglabusers/poulsen/pmar_allstrains/LB_20171130/new_fasta_alignments/Rui_analysis_files/clusters_compiled.xlsx",sheet = 1, startRow = 1, colNames = TRUE)
  cluster$strain<-gsub("Pseudomonas aeruginosa ","",cluster$Genome)
  cluster$strain<-gsub("PSA","",cluster$strain)
  colnames(cluster)<-c("gene_name","patric_id","Locus.CIA","length","cluster","desc","strain")
  cluster2<-cluster %>% dplyr::filter(strain==st)
  print(st)

  if (strain=="UCBPP"){
    genelist<-read.table("/idi/hunglabusers/poulsen/pmar_allstrains/UCBPP/rui/genelist_UCBPP.txt")
    geneinfo<-read.table("/idi/hunglabusers/poulsen/pmar_allstrains/UCBPP/rui/UCBPP_strand_type_information.txt")
  }else{
    gnlist<-supp[grep("genelist",supp)][grep(strain,supp[grep("genelist",supp)])]
    genelist<-read.table(paste("/idi/hunglabusers/poulsen/pmar_allstrains/LB_20171130/new_fasta_alignments/Rui_analysis_files/",gnlist,sep="/"))
    strclu<-list.files("/idi/hunglabusers/poulsen/pmar_allstrains/LB_20171130/new_fasta_alignments/Rui_analysis_files")
    strclu<-strclu[grep("strand_type",strclu)]
    strclu<-strclu[grep(st,strclu)]
    geneinfo<-read.table(paste("/idi/hunglabusers/poulsen/pmar_allstrains/LB_20171130/new_fasta_alignments/Rui_analysis_files",strclu,sep="/"))
    print(strclu)
  }
  colnames(genelist)<-c("V1","Locus.CIA","gene_start","gene_stop")
  genelist$strain<-st
  genelist<-genelist %>% left_join(cluster2,by=c("Locus.CIA","strain"))
  colnames(geneinfo)<-c("type","strand","Locus.CIA")
  genelist<-genelist %>% full_join(geneinfo,by="Locus.CIA")
  genelist<-genelist %>% dplyr::select(Locus.CIA,strain,type,gene_start,gene_stop,strand,gene_name,cluster,desc)

  if (strain=="UCBPP"){
    genelist$strain<-"UCBPP"
  }else{
    genelist$strain<-genelist$strain
  }
  print("Finished loading supporting files")

  #2. prepare usable tally files

  usable_tally_list<-lapply(file_location_list,function(x){
    unique_file=x
    print(unique_file)

    #a) Import raw tally files and annotate sites with homologous:

    unique_map_tally <- import_raw_tally(unique_file,CIA=FALSE)
    unique_map_tally<-find_homo(unique_map_tally,homo)
    unique_map_tally2<-unique_map_tally
    unique_map_tally2$Locus.CIA<-gsub("IG_","",unique_map_tally2$Locus.CIA)
    unique_map_tally2$strain<-strain

    #b) Import raw tally files and annotate sites with multialignment:

    unique_map_tally <- calc_TApos(unique_map_tally2, genelist)
    ## removed many TA sites
    unique_map_tally <- dplyr::filter(unique_map_tally, type == 'CDS')
    tally.w <- unique_map_tally

    #c) Find sites affected by non-permissive bias:

    tally.w$non_permissive <- (tally.w$TA_start %in% nonPermissiveTA$V2)
    table(tally.w$non_permissive) #TRUE is number of non-permissive TA sites

    #d) Denote TAs for edge trimming

    tally.w <- denote_coreTA(tally.w, 50)
    ## denote TAs as TRUE it is not in the first and last 50bp of the gene
    table(tally.w$coreTA) #false is number of TA sites found near edges

    #e) process to list for downstream analysis
    x=tally.w

  })
  return(usable_tally_list)
}
