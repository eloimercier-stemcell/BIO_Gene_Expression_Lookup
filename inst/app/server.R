##############################################################
##############################################################
#                         EXPRESSION LOOKUP APP                           #
##############################################################
##############################################################


#setwd("~/GitHub/BIO_Gene_Expression_Lookup/inst/app")

shinyServer <- function(input, output, session)
{

    ###################################
    #App set up and options
    ###################################

    data.dir <- "data"
    dataListFile <- file.path(data.dir,"sale_app_data_list.xlsx")
    datasetList <- reactiveValues(table=NA, file=dataListFile)


    ###################################
    #Get list of data sets
    ###################################

    output$datasetListDT <- renderDT({
        validate(need(file.exists(datasetList$file),"Sorry, we cannot find the list of data sets."))

        dataListTable <- openxlsx::read.xlsx(datasetList$file)
        datasetList$table <- dataListTable

        dataListTable <- dataListTable[,colnames(dataListTable)!="Location"]
        DT::datatable(dataListTable,
                rownames=FALSE,
                selection = 'single',
                filter=list(position='top',clear = TRUE),
                options=list(autoWidth = TRUE, pageLength=10))
    })

    ###################################
    #Data set info
    ###################################

    output$datasetInfoUI <- renderUI(
        fluidRow(          
            HTML(paste('<p style="font-size:15px">','<font color="#000000">','<b>DATASET: </b>', "BLA",'</font>','</p>','<br>')),
            HTML(paste('<p style="font-size:15px">','<font color="#FE0400">','<b>COMP1: </b>', "BLA",'</font>','</p>','<br>')),
            HTML(paste('<p style="font-size:15px">','<font color="#008BFF">','<b>COMP2: </b>', "BLA",'</font>','</p>','<br>'))
        )
)

    ###################################
    #Load data
    ###################################

    getDataTable <- reactive({
        # Initial value of allDat while waiting for data to be submitted
        if(!is.null(input$datasetListDT_rows_selected)){
          dataset_selected <- datasetList$table[input$datasetListDT_rows_selected,]
          data_file <- file.path(data.dir,dataset_selected$Location)
          validate(need(file.exists(data_file),"Sorry, the data set you selected is not available."))
          data <- openxlsx::read.xlsx(data_file)

          #format data
          colnames(data)[1] <- "Gene ID"
          data$log2FoldChange <- round(data$log2FoldChange,3)
          data$padj <- round(data$padj,3)
          column_order <- c("Gene ID", "fullName", "alias", "log2FoldChange", "padj")
          data <- data[,column_order]

          return(data)
        }
    })

    ###################################
    #Result table
    ###################################

    output$resultTable <- renderDT({

        data <- getDataTable()
        validate(need(!all(is.null(data)),"Please select a data set!"))
        data <- data[!is.na(data$log2FoldChange),,drop=FALSE] #remove gene with no logFC

        max_val <- max(abs(data$log2FoldChange), na.rm=T)
        brks <- seq(from=-max_val, to=max_val, length.out=100)
        clrs <- colorRampPalette(c("#008BFF","#FFFFFF","#FE0400"))(length(brks)+1) #blue=negative logFC, red=positive

        DT::datatable(data,
                rownames=FALSE,
                selection = 'single',
                filter=list(position='top',clear = TRUE),
                options=list(autoWidth = TRUE, pageLength=100)) %>% 
              formatStyle( #color by logFC
                'log2FoldChange',
                backgroundColor = styleInterval(brks, clrs)
              )


    })

}


#TODO:
#have legend indicating blue=higher in PSC, red=higher in media
#fix icon on server


