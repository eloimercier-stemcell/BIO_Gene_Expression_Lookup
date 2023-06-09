##############################################################
##############################################################
#                         SALE APP                           #
##############################################################
##############################################################




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


      output$datasetListDT <-  renderDT({
        validate(need(file.exists(datasetList$file),"Sorry, we cannot find the list of data sets."))

        dataListTable <- openxlsx::read.xlsx(datasetList$file)
        datasetList$table <- dataListTable

        DT::datatable(dataListTable,
                rownames=FALSE,
                selection = 'single',
                filter=list(position='top',clear = TRUE),
                options=list(autoWidth = TRUE, pageLength=10))

      })


}




