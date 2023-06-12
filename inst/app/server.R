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
    dataSetList <- reactiveValues(table=NA, datasets=NA) #store all the info about the data sets

    ###################################
    #Get list of data sets
    ###################################

    observe({
        #validate(need(file.exists(dataListFile),"Sorry, we cannot find the list of data sets."))
        if(file.exists(dataListFile)){
            dataTable <- openxlsx::read.xlsx(dataListFile)
            if(!all(c("Dataset","Description", "Condition1", "Condition2", "Location") %in% colnames(dataTable))){
                stop("Missing columns in dataListFile!")
            }
            rownames(dataTable) <- dataTable[,"Dataset"]
            dataSetList$table <- dataTable
            dataSetList$datasets <- rownames(dataTable)
        }
    })

    ###################################
    #Create data set selection menu
    ###################################

    output$dataSetSelectionUI <- renderUI({
        selectizeInput(
            inputId="dataSetSelected",
                label="Available Data Set:",
                choices=dataSetList$datasets, selected = "", multiple = FALSE, 
                options = list(placeholder = 'Select a data set',onInitialize = I('function() { this.setValue(""); }'))
            )
        })



    ###################################
    #Data set info
    ###################################

    output$datasetInfoUI <- renderUI({
        if(!is.null(input$dataSetSelected) & !identical(input$dataSetSelected,"")){

            description <- dataSetList$table[input$dataSetSelected,"Description"]
            group1 <- dataSetList$table[input$dataSetSelected,"Condition1"]
            group2 <- dataSetList$table[input$dataSetSelected,"Condition2"]

            dataset_info_text <- paste0('
                <p style="font-size:12px">
                <font color="#000000"><b>',description,'</b></font><br>
                <font color="#FE0400">Higher in ', group1,'</font>;
                <font color="#008BFF">Higher in ', group2,'</font>
                </p>
                ')

            HTML(dataset_info_text)
        }
    })

    ###################################
    #Load data
    ###################################

    getDataTable <- reactive({
        if(!is.null(input$dataSetSelected) & !identical(input$dataSetSelected,"")){
            data_file <- dataSetList$table[input$dataSetSelected,"Location"]
            data_file_loc <- file.path(data.dir, data_file)
            validate(need(file.exists(data_file_loc),"Sorry, the data set you selected is not available."))
            data <- openxlsx::read.xlsx(data_file_loc)

            #format data
            colnames(data)[1] <- "Gene ID"
            data$log2FoldChange <- round(data$log2FoldChange,3)
            data$padj <- round(data$padj,3)
            data <- data[order(data$log2FoldChange,decreasing=TRUE),]
            column_order <- c("symbol", "alias","fullName","Gene ID", "log2FoldChange", "padj")
            data <- data[,column_order]

            return(data)
        }
    })

    ###################################
    #Result table
    ###################################

    output$resultTable <- renderDT({

        data <- getDataTable()
        if(!all(is.null(data))){
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
        }

    })

}


#TODO:
#how to deal with padj=NA, usually from sample outliers

