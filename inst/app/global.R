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


smartFindAl <- function(genes, convert_to, org_data.db, mVals="first", ...) #choose what you want to convert into regardless of key type; support mixed key types (e.g. "ENSG..." and "POU5F1" in same input)
{
    mapColumns <- c("SYMBOL", "ENSEMBL", "ALIAS")
    mapTable <- vector("list", length(genes))
    names(mapTable) <- genes
    mapToColumns <- mapColumns[mapColumns!=convert_to]

    for (gType in mapToColumns) {
      isThisKeyType <- genes %in% keys(org_data.db, keytype=gType) #select input of that key type
      gKey.current <- genes[isThisKeyType]
      if (any(isThisKeyType)){
        suppressMessages(mapTable[gKey.current] <- mapIds(org_data.db,keys=gKey.current,column=convert_to,keytype=gType,multiVals=mVals))
      }
    }
    isThisKeyType <- genes %in% keys(org_data.db, keytype=convert_to)
    gKey.current <- genes[isThisKeyType]
    mapTable[gKey.current] <- gKey.current #add back keys that don't need conversion
    mapTable[sapply(mapTable, function(x){all(is.null(x))})] <- NA

    if (mVals!="list"){mapTable <- unlist(mapTable)}
    return(mapTable)
}
