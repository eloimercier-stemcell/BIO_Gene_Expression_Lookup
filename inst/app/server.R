##############################################################
##############################################################
#                         EXPRESSION LOOKUP APP                           #
##############################################################
##############################################################


#setwd("~/GitHub/BIO_Gene_Expression_Lookup/inst/app")


# TODO: 
# show sample outliers with crook distance
# move data to server
# save group1 and group2 in reactive values

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
            if(!all(c("Dataset","Description", "Condition1", "Condition2", "Input_group", "Location") %in% colnames(dataTable))){
                stop("Missing columns in dataListFile!")
            }
            rownames(dataTable) <- dataTable[,"Dataset"]
            dataSetList$table <- dataTable

            #split datasets into groups
            input_groups <- unique(dataTable$Input_group)
            datasetL <- lapply(input_groups, function(x){dataTable[dataTable$Input_group==x,"Dataset"]})
            names(datasetL) <- input_groups
            dataSetList$datasets <- datasetL
        }
    })

    ###################################
    #Create data set selection menu
    ###################################

    output$dataSetSelectionUI <- renderUI({
        selectizeInput(width="500px",
            inputId="dataSetSelected",
                label=h3("Available Data Sets:"),
                choices=dataSetList$datasets, selected = "", multiple = FALSE, 
                options = list(placeholder = 'Select a data set',onInitialize = I('function() { this.setValue(""); }'))
            )
        })

    ###################################
    #Create gene search box
    ###################################

    output$geneListSelectionUI <- renderUI({
        tagList(
        textInput("geneList_selected", width="500px",
            label=h3("Input gene list"),
            placeholder= "Your, gene, list"),
        actionButton("submitBtn.geneListSel", label="Submit", icon.library="font awesome",css.class='sc-button')
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
                <font color="#000000"><em>','Note: missing pvalues indicate presence of outliers and associated logFC should be considered with care.','</em></font>
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
            data_file_loc <- file.path(data_file)
            validate(need(file.exists(data_file_loc),"Sorry, the data set you selected is not available. Please contact the MBBG team."))
            data <- readRDS(data_file_loc)

            ###############
            #Format tables
            ###############

            #result table
            res_table <- as.data.frame(data[["results"]])
            res_table$log2FoldChange <- round(res_table$log2FoldChange,3)
            res_table$padj <- round(res_table$padj,3)
            res_table <- res_table[order(res_table$log2FoldChange,decreasing=TRUE),]
            res_table <- res_table[!is.na(res_table$log2FoldChange),,drop=FALSE] #remove gene with no logFC
            res_table <- res_table[!is.na(res_table$Gene),,drop=FALSE] #remove gene with no symbol
            res_table$Aliases <- sapply(res_table$Aliases, paste0, collapse=", ") #convert list to vector
            attr(res_table, "max_val") <- max(abs(res_table$log2FoldChange), na.rm=T) #remember max logFC value even when table is subsetted
            # column_order <- c("Gene","Full Name","Aliases","Gene ID","log2FoldChange","padj")
            # res_table <- res_table[,column_order]
            # colnames(res_table) <- c("Gene", "Full Name", "Alternate Gene Names","Gene ID", "log2 Fold Change vs. hPSC","Adjusted P-Value")
            data[["results"]] <- res_table

            #count table
            data[["count"]] <- data[["count"]][rownames(res_table),]

            return(data)
        }
    })

    ###################################
    #subset selected genes if any
    ###################################

    getResultsForSelectedGenes <- eventReactive(c(input$dataSetSelected,input$submitBtn.geneListSel),{
        res_table <- getData()[["results"]]
        if(!all(is.null(res_table))){
            #subset selected genes if any
            if(!all(is.null(input$geneList_selected)) & input$geneList_selected!=""){
                #parse gene list
                gene_vec <- unlist(trimws(strsplit(input$geneList_selected,",| ")[[1]]))
                gene_vec <- gene_vec[gene_vec!=""]
                #find ensembl ID
                genes_id <- smartFindAl(genes=gene_vec, convert_to="ENSEMBL", org_data.db=org.Hs.eg.db, mVals="list") 
                genes_id <- na.omit(unlist(genes_id))
                genes_id <- unique(intersect(rownames(res_table), genes_id)) #it can happen that the gene ids are not in the table

                if(length(genes_id)){
                    return(res_table[genes_id,])
                }
            } else { #otherwise return the whole table
                return(res_table)
            }
        }

    })


    ###################################
    #Result table
    ###################################

    output$resultTable <- renderDT({
        res_table <- getResultsForSelectedGenes()
        if(!all(is.null(res_table))){

            max_val <- attr(res_table, "max_val") 
            brks <- seq(from=-max_val, to=max_val, length.out=100)
            clrs <- colorRampPalette(c("#008BFF","#FFFFFF","#FE0400"))(length(brks)+1) #blue=negative logFC, red=positive

            column_order <- c("Gene","Full Name","Aliases","Gene ID","log2FoldChange","padj")
            res_table <- res_table[,column_order]
            colnames(res_table) <- c("Gene", "Full Name", "Alternate Gene Names","Gene ID", "log2 Fold Change vs. hPSC","Adjusted P-Value")

            DT::datatable(res_table,
                    rownames=FALSE,
                    selection = 'single',
                    filter=list(position='top',clear = TRUE),
                    options=list(autoWidth = FALSE, pageLength=25, scrollX = TRUE, width = "100%")) %>% 
                  formatStyle( #color by logFC
                    'log2 Fold Change vs. hPSC',
                    backgroundColor = styleInterval(brks, clrs)
                  )
        }
    })


    ###################################
    #Expression plot
    ###################################

    output$expressionPlot <- renderPlotly({
        res_count <- getData()[["counts"]]
        res_table <- getResultsForSelectedGenes()        
        sample_table <- getData()[["sampleTable"]]
        if(!all(is.null(res_count)) & !is.null(input$resultTable_rows_selected) & !identical(NA,input$resultTable_rows_selected)){ #row selected is NULL on initialization, NA if gene is not found in the table anymore
            geneID_selected <- res_table[input$resultTable_rows_selected,"Gene ID"]
            symbol_selected <- res_table[input$resultTable_rows_selected,"Gene"]
            mat <- res_count[geneID_selected,,drop=FALSE]
            df <- reshape2::melt(mat)
            DF <<- df
            colnames(df) <- c("Gene","Sample","Expression")

            ugroups <- unique(sample_table[df$Sample,"Condition"]) 
            ugroups <- c("hPSC", ugroups[ugroups!="hPSC"])  #fix the order of the group, hPSC always last
            df$Group <- factor(sample_table[df$Sample,"Condition"], levels=c(ugroups))
            group_colors <- c("#FE0400","#008BFF")
            names(group_colors) <- ugroups 
            df$Cell_Line <- sample_table[df$Sample,"Cell_Line"]

            group1 <- dataSetList$table[input$dataSetSelected,"Condition1"] #more verbose name for condition 1
            group2 <- dataSetList$table[input$dataSetSelected,"Condition2"] #hPSC

            plot_title <- paste0("Normalized ", symbol_selected ," Expression \nin ",group1," and ", group2) #stringr::str_wrap

            p <- ggplot(df, aes(x=Group, y=Expression, label=Sample, color=Group, group=Cell_Line)) + geom_point() + geom_jitter(width = 0.25) 
            p <- p + ggtitle(plot_title) + xlab("") + ylab("Expression") + theme(legend.position = "none", axis.text.x = element_text(angle = -45), plot.title = element_text(size = 10))
            p <- p + scale_colour_manual(values=group_colors)
            p <- p + scale_x_discrete(labels= c(group2, group1)) #fix x labels
            ggplotly(p) # %>% config(displayModeBar = F)
        }

    })



} #end shinyServer

