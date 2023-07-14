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
suppressMessages(library(ipify)) #FOR IP
suppressMessages(library(conductor)) 


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
