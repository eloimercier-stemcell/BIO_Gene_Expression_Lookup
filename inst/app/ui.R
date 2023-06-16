##############################################################
###                    KARYOTYPE APP                       ###
##############################################################
##############################################################

############################################################################
####  ######  ######    ####################################################
####  ######  #######  #####################################################
####  ######  #######  #####################################################
#####  ####  ########  #####################################################
######      ########    ####################################################
############################################################################
#Add elements as arguments

css_dir <- "./css"

shinyUI <- fluidPage(

    ###############################
    ###############################
    #              CSS            #
    ###############################
    ###############################    
  
    includeCSS(file.path(css_dir,"styles.css")),
    includeCSS(file.path(css_dir,"widgets.css")),
    includeCSS(file.path(css_dir,"styles_sc.css")),
    includeCSS(file.path(css_dir,"header.css")),
    includeCSS(file.path(css_dir,"footer.css")),
    includeCSS(file.path(css_dir,"responsive.css")),
    includeCSS(file.path(css_dir,"custom_grid.css")),
    includeCSS(file.path(css_dir,"grid_responsive.css")),
    includeCSS(file.path(css_dir,"font-awesome.min.css")),
    includeCSS(file.path(css_dir,"skin.css")),
    includeCSS(file.path(css_dir,"fancybox.css")),
    includeCSS(file.path(css_dir,"workflow.css")),
    includeCSS(file.path(css_dir,"brand.css")),
    includeCSS(file.path(css_dir,"research_areas.css")),
    includeCSS(file.path(css_dir,"print.css")),
  
  
    ###############################
    ###############################
    #            STYLE            #
    ###############################
    ############################### 

    tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"), #to load all fonts
    tags$style(type='text/css', '.sc-button {margin-top: 0px;}'),
    tags$style(".wrapper {
                background-color: #F7F7F5; 
                padding-top: 25px; 
                padding-bottom: 25px; 
                }"),
    tags$style(HTML('.irs-bar {border-top-color: #E47C23; border-bottom-color: #E47C23;}')),
    tags$style(HTML('.irs-bar-edge {border-color: #E47C23;}')),
    tags$style(HTML('.irs-single, .irs-bar-edge, .irs-bar {background: #E47C23;}')),
    tags$style(HTML('.swal-button{
                   background-color: #E47C23 !important;
                   color: white}')),

    # tags$style(".selectize-input {margin-top: 0px;}"),
    # tags$style(".selectize-input {border-radius: 0px;}"),
    # tags$style(".selectize-input.dropdown-active {border-radius:0px}"),
    # tags$style(".selectize-dropdown {border-radius:0px}"),
    # tags$style(".handsontable table thead th{
    #             color: white; 
    #             background-color: #E47C23;  
    #             font-weight:bold;}"),
    # tags$style(".wtBorder{ background-color: #53585A !important;}"),
    # tags$style(".introjs-helperNumberLayer {background: #E47C23;}"),
    # tags$style(HTML(".thumbnail:hover {
    #               position:relative;
    #               top:-25px;
    #               left:-35px;
    #               width:800px;
    #               height:auto;
    #               display:block;
    #               z-index:999;
    #               }")), 

    tags$style(type='text/css', "#useSampDat button{background: #E47C23; color: white; !important;}"),
    tags$head(tags$style(".progress-bar{background-color:#E47C23;}
                       .col-sm-12{padding:0;margin-bottom:30px;}")),

    ###############################
    ###############################
    #            HEADER           #
    ###############################
    ###############################
  
  headerPanel(windowTitle="STEMCELL Technologies Gene Expression Lookup", 
      HTML("<div class='row'>
              <div class='wrapper top'>
                <div class='col-sm-2'>
                </div>
              <div class='col-sm-5'>
                  <img src='//tbcdn.talentbrew.com/company/8172/v1_0/img/stemcell_logo.png' 
                  alt='STEMCELL Technologies'>
              </div>
              <div class='clear'>
              </div>
           </div>
           <ul class='link-list' aria-hidden='false' aria-expanded='true'>
           </ui>
          </div>"
        )
    ),
  
  
    ###############################
    ###############################
    #          SIDEPANEL          #
    ###############################
    ###############################
    
  # sidebarPanel( 
  #   width=2, fuild=FALSE,

  #       ##############
  #       # STYLE
  #       ##############

  #       ##############
  #       # MENU
  #       ##############

  #       # uiOutput("dataSetSelectionUI")                     
    

  # ),
  
    ###############################
    ###############################
    #            BODY          #
    ###############################
    ###############################

  
    mainPanel(

        ##############
        # STYLE
        ##############

        tags$style(".navbar-default {background-color:#FFF;}
                .navbar-brand {padding:0;}
                .navbar .container-fluid {padding:0;}"),
        #box color: only workd with box(..., status="primary", solidHeader =T)
        tags$style(HTML(" 
                    .box.box-solid.box-primary>.box-header {
                    }
                    .box.box-solid.box-primary{
                    background:#D6D6D6
                    }
        ")),   
    
        ##############
        # PANELS
        ##############

        fluidRow(
            column(12,
                box(width=5, height="120px",
                    uiOutput("dataSetSelectionUI"),
                    status="primary", solidHeader =T
                ),
                box(width=7, height=120,
                    uiOutput("geneListSelectionUI"),

                    # actionButton("submitBtn.geneListSel", label="Submit", icon.library="font awesome",css.class='sc-button'),
   
                    status="primary", solidHeader =T
                )
                # ,
                # box(width=2,
                #     actionButton("submitBtn.geneListSel", label="Submit", icon.library="font awesome",css.class='sc-button'),
                #     status="primary", solidHeader =T
                # )                
            ),


            column(12,
                    htmlOutput("datasetInfoUI")
                 ),

            column(9,
                withSpinner(DTOutput("resultTable"),
                          type=7,
                          color="#E47C23",
                          size=0.9)                    
            ),
            column(3,
                withSpinner(plotlyOutput("expressionPlot", height = 807),
                          type=7,
                          color="#E47C23",
                          size=0.9)                    
            )
        )
    , width = 11) #end mainPanel

  
)
