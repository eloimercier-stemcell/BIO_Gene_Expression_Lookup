
setwd("C:/Users/emercier/Documents/GitHub/BIO_Gene_Expression_Lookup/")

library(googlesheets4)
library(DESeq2)
library(tximport)
library(ggplot2)
library(org.Hs.eg.db)
library(openxlsx)

assemblies_dir <- "S:/RND - Research and Development/10 - Work in Progress/k - ES Research/Bioinformatics/Eloi/Datasets/Assemblies"
sample_info_file <- "inst/app/data/Sample info.xlsx"

# source("C:/Users/emercier/Documents/Scripts/R/convertGeneID2GeneNames.R")
# ens2symbol <- geneID2GeneNames(method="Ensembl")

#########################
#Read sample info
#########################

##############
all_sample_info <- as.data.frame(read.xlsx(sample_info_file, sheet=1)) #run this to get token
############# 

all_data_quant_files <- file.path(all_sample_info$Location,"quant.sf")
all_data_quant_files_exist <- all(file.exists(all_data_quant_files)) & length(all_data_quant_files)>0
if(!all_data_quant_files_exist){stop(paste0("Missing quant files!"))}

all_conditions <- unique(all_sample_info[,c("Experiment","Condition")])
all_conditions <- all_conditions[all_conditions$Condition!="hPSC",]

##################################################
##################################################
#Process each condition, compared to hPSC
##################################################
##################################################

plot_qc <- vector("list", nrow(all_conditions))

for(i in seq_len(nrow(all_conditions))){

    #########################
    #create sample table
    #########################
    experiment_i <- all_conditions[i,"Experiment"]    
    cond_i <- all_conditions[i,"Condition"]
    print(paste0(experiment_i,": ",cond_i, " vs ","hPSC"))

    sample_table <- as.data.frame(all_sample_info[all_sample_info$Condition %in% c("hPSC", cond_i) & all_sample_info$Experiment %in% c("hPSC", experiment_i),])
    data_quant_files <- file.path(sample_table$Location,"quant.sf")
    sample_table <- sample_table[,-which(colnames(sample_table)=="Location")]
    rownames(sample_table) <- sample_table$Sample

    #########################
    #read quants
    #########################

    tx2gene <- read.table(file.path(assemblies_dir,"GRCh38/Homo_sapiens.GRCh38.96.tx2gene.tsv"), sep="\t", header=T)
    txi <- tximport(data_quant_files, type="salmon", tx2gene=tx2gene)

    #########################
    #create dds
    #########################

    dds <- try(DESeqDataSetFromTximport(txi, colData = sample_table, design = ~Condition) )
    if (class(dds)=="try-error") {stop("Error with design!")}
    keep <- rowSums(counts(dds)) >= ncol(dds) #removing low expressed genes
    dds <- dds[keep,]
    dds <- DESeq(dds)

    #########################
    #get results
    #########################

    comparison_contrast <- c("Condition", cond_i ,"hPSC")
    res <- as.data.frame(results(dds, contrast=comparison_contrast))
    res[,"Gene ID"] <- rownames(res)
    res[,"Gene"]  <- mapIds(org.Hs.eg.db,keys=rownames(res),column="SYMBOL",keytype="ENSEMBL",multiVals="first")
    res[,"Full Name"] <- mapIds(org.Hs.eg.db,keys=rownames(res),column="GENENAME",keytype="ENSEMBL",multiVals="first")
    res$Aliases  <- mapIds(org.Hs.eg.db,keys=rownames(res),column="ALIAS",keytype="ENSEMBL",multiVals="list")

    #########################
    #get counts
    #########################

    count_val <- counts(dds)
    colnames(count_val) <- sample_table$Sample

    #########################
    #meta info
    #########################

    meta <- list(
        date=date(),
        organism="GRCh38.96",
        sampleTable=sample_table,
        session=sessionInfo()
    )

    #########################
    #PCA for QC
    #########################

    rld <- vst(dds)
    plot_qc[[i]] <- plotPCA(rld, intgroup=c("Condition")) + ggtitle( paste0(experiment_i,"_",cond_i, "_vs_","hPSC"))

    #########################
    #save data
    #########################

    res_file <- paste0(experiment_i,"_",cond_i, "_vs_","hPSC",".Rds")
    allDat <- list(results=res, counts=count_val, sampleTable=sample_table, metadata=meta)
    saveRDS(allDat,file=file.path("inst/app/data/datasets",res_file))

}


#########################
#QC
#########################

#check PCA plots
for (i in seq_along(plot_qc)){
    print(plot_qc[[i]])
    readline(prompt="Press [enter] to continue.")
}

#########################
#Intra PSC variance
#########################

sample_table <- as.data.frame(all_sample_info[all_sample_info$Condition %in% c("hPSC"),])
data_quant_files <- file.path(sample_table$Location,"quant.sf")
sample_table <- sample_table[,-which(colnames(sample_table)=="Location")]
rownames(sample_table) <- sample_table$Sample

tx2gene <- read.table(file.path(assemblies_dir,"GRCh38/Homo_sapiens.GRCh38.96.tx2gene.tsv"), sep="\t", header=T)
txi <- tximport(data_quant_files, type="salmon", tx2gene=tx2gene)

dds <- try(DESeqDataSetFromTximport(txi, colData = sample_table, design = ~Cell_Line) )
if (class(dds)=="try-error") {stop("Error with design!")}
keep <- rowSums(counts(dds)) >= ncol(dds) #removing low expressed genes
dds <- dds[keep,]
dds <- DESeq(dds)

rld <- vst(dds)
p <- plotPCA(rld, intgroup=c("Condition", "Cell_Line")) + ggtitle( paste0(experiment_i,"_",cond_i, "_vs_","hPSC"))
ggplotly(p)