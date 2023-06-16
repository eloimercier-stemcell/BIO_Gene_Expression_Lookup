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
    dataListFile <- file.path(data.dir,"data_set_list.xlsx")
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
                label="Available Data Sets:",
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
                <font color="#FE0400">Higher expression in ', group1,'</font>;
                <font color="#008BFF">Higher expression in ', group2,'</font> <br>
                <font color="#000000"><em>','Note: missing pvalue indicates gene expression outliers.','</em></font>
                </p>
                ')

            HTML(dataset_info_text)
        }
    })

    ###################################
    #Load data
    ###################################

    getData <- reactive({
        if(!is.null(input$dataSetSelected) & !identical(input$dataSetSelected,"")){
            data_file <- dataSetList$table[input$dataSetSelected,"Location"]
            # data_file_loc <- file.path(data.dir, data_file)
            data_file_loc <- file.path(data_file)
            validate(need(file.exists(data_file_loc),"Sorry, the data set you selected is not available. Please contact the MBBG team."))
            data <- readRDS(data_file_loc)

            ###############
            #Format tables
            ###############

            #result table
            res_table <- as.data.frame(data[["results"]])
            res_table$Gene <- rownames(res_table)
            res_table$log2FoldChange <- round(res_table$log2FoldChange,3)
            res_table$padj <- round(res_table$padj,3)
            res_table <- res_table[order(res_table$log2FoldChange,decreasing=TRUE),]
            res_table <- res_table[!is.na(res_table$log2FoldChange),,drop=FALSE] #remove gene with no logFC
            column_order <- c("Gene", "log2FoldChange", "padj")
            res_table <- res_table[,column_order]
            data[["results"]] <- res_table

            #count table
            data[["count"]] <- data[["count"]][rownames(res_table),]

            return(data)
        }
    })

    ###################################
    #Result table
    ###################################

    output$resultTable <- renderDT({

        res_table <- getData()[["results"]]
        if(!all(is.null(res_table))){

            max_val <- max(abs(res_table$log2FoldChange), na.rm=T)
            brks <- seq(from=-max_val, to=max_val, length.out=100)
            clrs <- colorRampPalette(c("#008BFF","#FFFFFF","#FE0400"))(length(brks)+1) #blue=negative logFC, red=positive
            DT::datatable(res_table,
                    rownames=FALSE,
                    selection = 'single',
                    filter=list(position='top',clear = TRUE),
                    options=list(autoWidth = FALSE, pageLength=50, scrollX = TRUE)) %>% 
                  formatStyle( #color by logFC
                    'log2FoldChange',
                    backgroundColor = styleInterval(brks, clrs)
                  )
        }

    })


    ###################################
    #Expression plot
    ###################################

    output$expressionPlot <- renderPlotly({
        res_count <- getData()[["counts"]]
        res_table <- getData()[["results"]]
        sample_table <- getData()[["sampleTable"]]
        if(!all(is.null(res_count)) & !is.null(input$resultTable_rows_selected)){

            symbolSel <- rownames(res_table[input$resultTable_rows_selected,,drop=FALSE])
            mat <- res_count[symbolSel,,drop=FALSE]
            df <- reshape2::melt(mat)
            colnames(df) <- c("Gene","Sample","Expression")

            df$Group <- sample_table[df$Sample,"Condition"]
            group_colors <- c("#FE0400","#008BFF")
            ugroups <- unique(sample_table[df$Sample,"Condition"]) #get the name of the condition that is not hPSC
            names(group_colors) <- c(ugroups[ugroups!="hPSC"], "hPSC") 
            df$Cell_Line <- sample_table[df$Sample,"Cell_Line"]

            p <- ggplot(df, aes(x=Group, y=Expression, label=Sample, color=Group, group=Cell_Line)) + geom_point() + ggtitle(symbolSel)
            p <- p + xlab("") + ylab("Expression") + theme(legend.position = "none", axis.text.x = element_text(angle = -45))
            p <- p +  scale_colour_manual(values=group_colors)
            ggplotly(p) %>% config(displayModeBar = F)

        }

    })



} #end shinyServer


#TODO:
#how to deal with padj=NA, usually from sample outliers

