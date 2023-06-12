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

    # #THIS CODE STOPS AN ERROR WHERE SWEET ALERT DOESN'T RENDER ON FIREFOX
    # tags$style(HTML('window.swal = this.swal = this.Sweetalert;')),
    # tags$style(HTML('$.fn.modal.Constructor.prototype.enforceFocus = function() {};')),
    # tags$style(HTML('<script src="https://cdnjs.cloudflare.com/ajax/libs/core-js/2.4.1/core.js"></script>')),

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



    ###############################
    ###############################
    #            HEADER           #
    ###############################
    ###############################
  
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
  
  
    ###############################
    ###############################
    #          SIDEPANEL          #
    ###############################
    ###############################
    
  sidebarPanel( 
    width=4,

        ##############
        # STYLE
        ##############

        #Inherit CSS  
        # tags$style(type='text/css', 
        #            "#ctrlNum {border-radius: 0px}"),
        # tags$style(type='text/css', 
        #            "#testNum {border-radius: 0px}"),   
        # tags$style(type='text/css', 
        #            "#replicates {border-radius: 0px}"),   
        
        # tags$style(type="text/css", 
        #            "form.well {max-width: 300px;
        #               background: #F7F7F7;
        #          aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa     width: 350px;
        #               float: right;}"),
        # tags$style(".navbar-default {background-color:#FFF;}
        #             .navbar-brand {padding:0;}
        #             .navbar .container-fluid {padding:0;}"),
        # tags$style(HTML(".multicol .shiny-options-group{
        #                    -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
        #                    -moz-column-count: 2;    /* Firefox */ 
        #                    column-count: 2; 
        #                    -moz-column-fill: balanced;
        #                    -column-fill: balanced;
        #                    }
        #                    .checkbox{
        #                    margin-top: 3px !important;
        #                    -webkit-margin-after: 3px !important;
        #                    }")),
        # tags$style("#SexTab table.dataTable thead th, table.dataTable thead td {
        #     border-bottom: 1px solid #ddd; border-top: 1px solid #ddd}"),
        # tags$style("#SexTab table.dataTable {border-bottom: 1px solid #ddd}"),
        # tags$style(HTML('#SexTab table.dataTable tr.selected td, table.dataTable td.selected {background-color: #E47C23 !important;}')),
        
        # # STYLE: makes toggle sliders orange
        # tags$style(HTML(".bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-StemCellOrange,
        #     .bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-StemCellOrange {
        #     color: #fff;
        #     background:#E47C23;}")),
        
        # #Change color for dropdown menu for customer data email
        # tags$style(HTML(' .dropdown-toggle:hover{color:#E47C23; background:#fff;}')),
        # tags$style(HTML('.dropdown-toggle {color: #fff; background:#E47C23;}')),
        # tags$style(HTML('.dropdown-toggle:active, .open .dropdown-toggle{color: #fff !important; background:#E47C23 !important;}')),
        # tags$style(HTML('.dropdown-toggle:focus, .open .dropdown-toggle{color: #fff !important; background:#E47C23 !important;}')),
        
        # tags$style(HTML(".tooltip > .tooltip-inner {background-color: #E47C23;}")),
        # tags$style(HTML(".tooltip + .tooltip.top > .tooltip-arrow {border-top-color: #E47C23;}")),
        
        
        # # STYLE: makes radio buttons orange
        # tags$style(HTML("input[type=radio]:checked + label:before,
        #     input[type=checkbox]:checked + label:before {background: #E47C23 !important; 
        #     border-color: #53585A !important; 
        #     border-radius: 100px !important;
        #     -webkit-transition: all .5s ease;
        #     -moz-transition: all .5s ease;
        #     -o-transition: all .5s ease;
        #     -ms-transition: all .5s ease;
        #     transition: all .5s ease;}")), 
        # tags$style(HTML("input[type=radio] + label:before,
        #     input[type=checkbox] + label:before {border-color: #53585A !important;
        #     border-radius: 100px !important;
        #     -webkit-transition: all .5s ease;
        #     -moz-transition: all .5s ease;
        #     -o-transition: all .5s ease;
        #     -ms-transition: all .5s ease;
        #     transition: all .5s ease;}")),
    
    #Adjust replicate number
    #   fluidPage(tags$img(height=42, width=296, src="stemCell2.png")),
    
    conditionalPanel(condition='input.analysisTabs == "dataIn"',
                     

      selectInput(
      inputId="TEST1",
      label="Choose",
      choices=LETTERS[1:5],
      selected = NULL,
      multiple = FALSE,
      selectize = TRUE,
      width = NULL,
      size = NULL
    )


                     
    ),
    

  ),
  
    ###############################
    ###############################
    #            BODY          #
    ###############################
    ###############################

  
  mainPanel(

    tags$style(".navbar-default {background-color:#FFF;}
                .navbar-brand {padding:0;}
                .navbar .container-fluid {padding:0;}"),
    
    
    navbarPage(title='', id="analysisTabs", collapsible=TRUE,
               tabPanel(title="Input Data Table", value="dataIn",


               )
               
               #Outputs are what users see.  Use Output()
    ), class="tab-menu content-tab"
  ),
  
  conditionalPanel(condition="input.analysisTabs != 'dataPlot'",
                   fluidRow(column(width=12, 
                                   HTML("<div class='row'><div><div class='col-sm-2'></div>
                       <div class='col-sm-5'><hr>Copyright &copy 2023 by STEMCELL Technologies Inc. All rights reserved.</div>
                       </div></div>"
                                   ))))
  
  
)
