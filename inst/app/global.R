##############################################################
##############################################################
#                         EXPRESSION LOOKUP APP                           #
##############################################################
##############################################################



suppressMessages(library(shiny))
suppressMessages(library(shinysky))
suppressMessages(library(shinydashboard))
suppressMessages(library(shinyBS))
suppressMessages(library(shinycssloaders))
suppressMessages(library(shinyBS))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shinyjs))
suppressMessages(library(shinyalert))
suppressMessages(library(DT))
suppressMessages(library(plotly))
suppressMessages(library(org.Hs.eg.db))


# gene_keys <- c("ENSG00000000003", "BRICD4", "HLA","AKJAHKJASHJKSHKA")
# df=res
# search_columns <- c("Gene ID", "Gene","Aliases", "stat" )

# geneKeys2Ensembl <- function(gene_keys, df, search_columns){
#     #gene_keys: a vector of key ID, symbol, keyword
#     #to convert to Ensembl ID by searching within column data
#     if(!all(search_columns %in% colnames(df))){stop("Not all search columns found in df object!")}
#     if(!all("Gene ID" %in% colnames(df))){stop("Not all Gene ID not found in df object!")}

#     gene_keys2 <- sapply(gene_keys, function(x){paste0("\\b",x,"\\b")}) #add word boundary to find exact word
#     match_gene_key <- rep(FALSE, nrow(df))
#     for (i in seq_along(search_columns)){
#         genes_found <- sapply(gene_keys2 , function(x){grep(x, df[,search_columns[i]], ignore.case=TRUE)})
#         match_gene_key[unlist(genes_found)] <- TRUE
#     }

#     # gene_keys <- toupper(gene_keys)
#     # match_gene_key <- rep(FALSE, nrow(df))
#     # for (i in seq_along(search_columns)){
#     #     genes_found <- toupper(df[,search_columns[i]]) %in% gene_keys
#     #     match_gene_key <- match_gene_key | genes_found
#     # }

#     gene_ensembl <- df[match_gene_key,"Gene ID"]
#     gene_ensembl
# }

# g1 <- geneKeys2Ensembl(gene_keys, df, search_columns)

# genes <- c("ENSG00000000003", "BRICD4", "DP","AKJAHKJASHJKSHKA")
# convert_to="ENSEMBL"
# org_data.db=org.Hs.eg.db
# g2 <- smartFindAl(genes, convert_to, org_data.db, mVals="list") 

# geneKeys2Ensembl <- function(genes, convert_to, org_data.db, mVals="first", ...) #choose what you want to convert into regardless of key type; support mixed key types (e.g. "ENSG..." and "POU5F1" in same input)
# {
#     genes <- toupper(genes)
#     mapColumns <- c("SYMBOL", "ENSEMBL", "ALIAS")
#     mapTable <- vector("list", length(genes))
#     names(mapTable) <- genes
#     mapToColumns <- mapColumns[mapColumns!=convert_to]

#     for (gType in mapToColumns) {
#       isThisKeyType <- genes %in% keys(org_data.db, keytype=gType) #select input of that key type
#       gKey.current <- genes[isThisKeyType]
#       if (any(isThisKeyType)){
#         suppressMessages(mapTable[gKey.current] <- mapIds(org_data.db,keys=gKey.current,column=convert_to,keytype=gType,multiVals=mVals))
#       }
#     }
#     isThisKeyType <- genes %in% keys(org_data.db, keytype=convert_to)
#     gKey.current <- genes[isThisKeyType]
#     mapTable[gKey.current] <- gKey.current #add back keys that don't need conversion
#     mapTable[sapply(mapTable, function(x){all(is.null(x))})] <- NA

#     if (mVals!="list"){mapTable <- unlist(mapTable)}
#     return(mapTable)
# }


searchKeywordInColumns <- function(keyword, search_df, search_columns=c("Gene ID", "Gene", "Full Name", "Aliases"), fixed=FALSE, whole_word=FALSE){
#given a keyword, returns ensembl IDs for which keywords found in search_columns
  if(!all(search_columns %in% colnames(search_df))){stop("Not all search columns found in data frame!")}
  if(whole_word){keyword <- paste0("\\b", keyword,"\\b")} #hack to search for exact word (no substring), only works when fixed=FALSE

  returns_gene_idsL <- vector("list", length(search_columns))
  names(returns_gene_idsL) <- search_columns
  for (i in seq_along(search_columns)){
    returns_gene_idsL[[i]] <- search_df[grepl(keyword, search_df[,search_columns[i]], fixed=fixed), "Gene ID"]
  }
  returns_gene_ids <- unique(unlist(returns_gene_idsL))
  return(returns_gene_ids)
}
