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
    #create a complicated name for customer facing app
    # app_name <- "BIO_Gene_Expression_Lookup"
    # random_chars <- "7F(eMHesQiN7V.BxQ^7k_W_Ndv"
    # if(nchar(app_name)!=nchar(random_chars)){strop("Must be the same length!")}
    # new_app_name <- paste0(mapply(function(x,y){paste0(x,y)},strsplit(app_name,""),strsplit(random_chars,"") ), collapse = "")

    ###################################
    #App set up and options
    ###################################

    data.dir <- "data" #internal app folder
    dataListFile <- file.path(data.dir,"data_set_list.xlsx")
    dataSetList <- reactiveValues(table=NA, datasets=NA) #store all the info about the data sets
    searchData <- reactiveValues(all_searches=NULL)

    if(version$os=="linux-gnu"){ #check if we are live of shiny server
        session_logs.dir <- "/home/rstudio/to-rnd-private/Session_logs/BIO_Gene_Expression_Lookup" #default to client facing Rstudio
        if(!file.exists(session_logs.dir)){
            session_logs.dir <- "/home/rstudio/to-rnd-public/Session_logs/BIO_Gene_Expression_Lookup" #but perhaps we are on the internal Rstudio?
        }
    } else { #we are on local machine
        session_logs.dir <- "/Users/eloi.mercier/Documents" #"/Users/eloi.mercier/Library/CloudStorage/GoogleDrive-eloi.mercier@stemcell.com/My Drive/Projects/App_development/Gene_expression_lookup"
    }

    search_logs.file <- file.path(session_logs.dir,"search_logs.txt") #keep track of all searches
    ip_logs.file <- file.path(session_logs.dir,"ip_logs.txt")

   ###################################
    #Get user's IP
    ###################################

    session_ip <- get_ip(format="json")$ip
    line=paste0(date(),"\t",session_ip)
    write(line,file=ip_logs.file ,append=TRUE) #save IP in a file

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
            fluidRow(            
                column(8,textInput("geneList_selected", width="500px", label=h3("Input gene list"), placeholder= "Your, gene, list")),
                column(4,radioButtons("search_options",h3("Search Options"), choices=list("Fixed","Regexp","Whole word"), inline=T)) #values have to be strings
            ),
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
            res_table[is.na(res_table$padj),"padj"] <- 1 #set NA values to 1, happen when sample outliers
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

                #set up search options
                if(input$search_options=="Fixed"){
                    fixed <- TRUE
                    whole_word <- FALSE
                } else if(input$search_options=="Regexp"){
                    fixed <- FALSE
                    whole_word <- FALSE
                } else if(input$search_options=="Whole word"){
                    fixed <- FALSE
                    whole_word <- TRUE
                }

                #find ensembl IDs 
                genes_id <- unique(unlist(sapply(gene_vec, function(x){searchKeywordInColumns(keyword=x, search_df=res_table, search_columns=c("Gene ID", "Gene", "Full Name", "Aliases"), fixed=fixed, whole_word=whole_word)})))

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

    output$resultTable <- renderDataTable({
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
                    options=list(autoWidth = FALSE, pageLength=25, scrollX = TRUE, width = "100%",
                     search = list(regex = TRUE, caseInsensitive = TRUE))) %>% 
                  formatStyle( #color by logFC
                    'log2 Fold Change vs. hPSC',
                    backgroundColor = styleInterval(brks, clrs)
                  )
        }
    })

    search_logs <- observe({ #get search log and print in a file
        #capture all search fields
        input_search <- input$geneList_selected
        table_search <- input$resultTable_state$search$search
        col1_search <- input$resultTable_state$columns[[1]]$search$search
        col2_search <- input$resultTable_state$columns[[2]]$search$search
        col3_search <- input$resultTable_state$columns[[3]]$search$search
        col4_search <- input$resultTable_state$columns[[4]]$search$search
        col5_search <- input$resultTable_state$columns[[5]]$search$search
        col6_search <- input$resultTable_state$columns[[6]]$search$search
        current_search <- c(input_search,table_search,col1_search,col2_search,col3_search,col4_search,col5_search,col6_search)

        if(any(current_search!="")){
            current_search.string <- paste0(current_search, collapse="\t")   
            if(!current_search.string %in% searchData$all_searches) { #has this exact search been done this session already?     
                searchData$all_searches <- c(searchData$all_searches,current_search.string) #save new search                
                line <- paste0(c(date(),session_ip,input$dataSetSelected,current_search.string ), collapse="\t") #add ip and dataset
                write(line,file=search_logs.file,append=TRUE)
            }
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
            colnames(df) <- c("Gene","Sample","Expression")

            ugroups <- unique(sample_table[df$Sample,"Condition"]) 
            ugroups <- c("hPSC", ugroups[ugroups!="hPSC"])  #fix the order of the group, hPSC always last
            df$Group <- factor(sample_table[df$Sample,"Condition"], levels=c(ugroups))
            group_colors <- c("#008BFF","#FE0400")
            names(group_colors) <- ugroups 
            df$Cell_Line <- sample_table[df$Sample,"Cell_Line"]

            group1 <- dataSetList$table[input$dataSetSelected,"Condition1"] #more verbose name for condition 1
            group2 <- dataSetList$table[input$dataSetSelected,"Condition2"] #hPSC

            plot_title <- paste0("Normalized ", symbol_selected ," Expression \nin ",group1," and ", group2) #stringr::str_wrap

            p <- ggplot(df, aes(x=Group, y=Expression, label=Sample, color=Group, group=Cell_Line)) + geom_jitter(width = 0.25, height=0) 
            p <- p + ggtitle(plot_title) + xlab("") + ylab("Expression") + theme(legend.position = "none", axis.text.x = element_text(angle = -45), plot.title = element_text(size = 9))
            p <- p + scale_colour_manual(values=group_colors)
            p <- p + scale_x_discrete(labels= c(group2, group1)) #fix x labels
            ggplotly(p) %>% config(modeBarButtonsToRemove = c("drawcircle","eraseshape","zoomIn2d", "zoomOut2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "pan2d", "lasso2d", "select2d", "zoom2d"), displaylogo = FALSE)# %>% config(displayModeBar = F)
        }

    })



} #end shinyServer

