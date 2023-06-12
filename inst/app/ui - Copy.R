##############################################################
##############################################################
#                         EXPRESSION LOOKUP APP                           #
##############################################################
##############################################################


css_dir <- "./css"

# logifySlider javascript function
# JS.logify <-
#   "
# // function to logify a sliderInput
# function logifySlider (sliderId, sci = false) {
# if (sci) {
# // scientific style
# $('#'+sliderId).data('ionRangeSlider').update({
# 'prettify': function (num) { return ('10<sup>'+num+'</sup>'); }
# })
# } else {
# // regular number style
# $('#'+sliderId).data('ionRangeSlider').update({
# 'prettify': function (num) { return (Math.pow(10, num)); }
# })
# }
# }"

dashboardPage(

    ###############################
    ###############################
    #            HEADER           #
    ###############################
    ###############################

    dashboardHeader(title="STEMCELL Expression Lookup App", titleWidth=350),

    ###############################
    ###############################
    #            SIDEBAR          #
    ###############################
    ###############################

    dashboardSidebar(width=350,

        ##############
        # CSS
        ##############

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

        ##############
        # STYLE
        ##############

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

        #THIS CODE STOPS AN ERROR WHERE SWEET ALERT DOESN'T RENDER ON FIREFOX
        tags$style(HTML('window.swal = this.swal = this.Sweetalert;')),
        tags$style(HTML('$.fn.modal.Constructor.prototype.enforceFocus = function() {};')),
        tags$style(HTML('<script src="https://cdnjs.cloudflare.com/ajax/libs/core-js/2.4.1/core.js"></script>')),

        #OPTION TO DISPLAY ONE VALUE PER ROW IN SELECTIZEINPUT
        tags$head(tags$style(HTML(" 
              .selectize-control.multi .selectize-input > div {
                display: block; 
            }"))),

        tags$style(".selectize-input {margin-top: 0px;}"),
        tags$style(".selectize-input {border-radius: 0px;}"),
        tags$style(".selectize-input.dropdown-active {border-radius:0px}"),
        tags$style(".selectize-dropdown {border-radius:0px}"),
        tags$style(".handsontable table thead th{
                    color: white; 
                    background-color: #E47C23;  
                    font-weight:bold;}"),
        tags$style(".wtBorder{ background-color: #53585A !important;}"),
        tags$style(".introjs-helperNumberLayer {background: #E47C23;}"),
        tags$style(HTML(".thumbnail:hover {
                      position:relative;
                      top:-25px;
                      left:-35px;
                      width:800px;
                      height:auto;
                      display:block;
                      z-index:999;
                      }")), 
        tags$style(type='text/css', "#useSampDat button{background: #E47C23; color: white; !important;}"),
        tags$head(tags$style(".progress-bar{background-color:#E47C23;}
                           .col-sm-12{padding:0;margin-bottom:30px;}")),

        headerPanel(windowTitle="STEMCELL Technologies Genetic Analysis Tool", 
                  HTML("<div class='row'>
                          <div class='wrapper top'>
                            <div class='col-sm-2'>
                            </div>
                          <div class='col-sm-5'>
                            <a href='/' class='logo'>
                              <img src='//tbcdn.talentbrew.com/company/8172/v1_0/img/stemcell_logo.png' 
                              alt='STEMCELL Technologies'>
                            </a>
                          </div>
                          <div class='clear'>
                          </div>
                       </div>
                       <ul class='link-list' aria-hidden='false' aria-expanded='true'>
                       </ui>
                      </div>"
                  )),

        ##############
        # MENU
        ##############

        sidebarPanel(id="menu1",

            

        #Select data set
            menuItem("Select Data Set",
                tabName="data_selection_tab",
                icon=icon("hand-pointer", "fa-lg")
            ),

        #Result table
            menuItem("Results table",
                tabName="res_table_tab",
                icon=icon("table", "fa-lg")
            )            
        ) #end sidebarMenu


    ), #end dashboardSidebar

    ###############################
    ###############################
    #            BODY          #
    ###############################
    ###############################

    dashboardBody(

        ##############
        # STYLE
        ##############

        # tags$head(        
        #     tags$style(".selectize-input {border-radius: 0px;}"),
        #     tags$style(".selectize-input.dropdown-active {border-radius:0px}"),
        #     tags$style(".selectize-dropdown {border-radius:0px}")),
    
        # tags$head(tags$style(".progress-bar{background-color:#E47C23;}")),
        
        # Also add some custom CSS to make the title background area the same
        # color as the rest of the header.
        tags$head(tags$style(HTML('
                                  /* logo */
                                  .skin-blue .main-header .logo {
                                  background-color: #E47C23;
                                  }
                                  
                                  /* logo when hovered */
                                  .skin-blue .main-header .logo:hover {
                                  background-color: #E47C23;
                                  }
                                  
                                  /* navbar (rest of the header) */
                                  .skin-blue .main-header .navbar {
                                  background-color: #E47C23;
                                  }        
                                  
                                  /* main sidebar */
                                  .skin-blue .main-sidebar {
                                  background-color: #53585A;
                                  }
                                  
                                  /* menuItem when not in sidebarMenu */
                                  .skin-blue .sidebar a{
                                  color: #ffffff;
                                  }
                                  
                                  /* active selected tab in the sidebarmenu */
                                  .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                  background-color: #E47C23;
                                  border-left-color: #f7f7f5;
                                  }
                                  
                                  /* other links in the sidebarmenu */
                                  .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                  background-color: #53585A;
                                  color: #FFFFFF;
                                  }
                                  
                                  /* other links in the sidebarmenu when hovered */
                                  .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                  background-color: #E47C23;
                                  border-left-color: #f7f7f5;
                                  }
                                  
                                  /* top tab colour when active */
                                  .nav-tabs-custom>.nav-tabs>li.active{
                                  border-top-color: #e47c23;
                                  }
                                  
                                  /*  Make page colour continue when you scroll past end of page */
                                  .skin-blue .wrapper {
                                  background-color: #ecf0f5;
                                  }
                                  
                                  /* toggle button when hovered  */                    
                                  .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                  background-color: #53585A;
                                  }'))),
        
        
      #   tags$style(type='text/css',
      #              '.sc-button {margin-top: 0px; text-align: left;}'),


      #   tags$style(type = "text/css",  #slider label and ticks color
      #              ".irs-grid-text:nth-child(n) {color: grey}",
      #               ".irs-grid-pol:nth-of-type(n) {background:grey}"),
        
      #   tags$style(type='text/css',
      #              '#rName {font-size: 8px;}'),
        
      #   tags$style(".handsontable table thead th{
      #              color: white;
      #              background-color: #E47C23;
      #              font-weight:bold;
      #                             }"),
      #   tags$style(".wtBorder{
      #            background-color: #53585A !important;
      # }"),
      #   shinyjs::useShinyjs(),
      #   tags$head(tags$script(HTML(JS.logify))),
      #   tags$head(tags$script(HTML("$(document).ready(function() {
      #                                             // wait a few ms to allow other scripts to execute
      #                                             setTimeout(function() {
      #                                             // include call for each slider
      #                                             logifySlider('pValCutoff', sci = true)
      #                                             }, 5)})"))),



        # tags$style(type='text/css', "#memoryCalc {width:500px;}"),

        ##############
        # BANNER
        ##############

        box(width=12,
            collapsible=FALSE,
            fluidRow(
                htmlOutput("datasetInfoUI")
                )
        ),

        ##############
        # DISPLAY TABS
        ##############

        tabItems(

        #Select data set
            tabItem(tabName="data_selection_tab",
                box(title="Data Sets", width=12, 
                    DTOutput("datasetListDT")
                 )
            ),

        #Results table
            tabItem(tabName="res_table_tab",
                box(title="Results Table", width=12, 
                    DTOutput("resultTable")
                 )
            )

            

        ) #end tabItems


    ) #end dashboardBody



)