# PhyloTree
# Version 1.0.0
# Phylogenetic Visualization in a Shiny App
# Author: Marian Freisleben
# Date: 18.09.2023

if (!require(shiny))
  install.packages('shiny')
library(shiny)

if (!require(BiocManager))
  install.packages('BiocManager')
library(BiocManager)

if (!require(shinyWidgets))
  install.packages('shinyWidgets')
library(shinyWidgets)

if (!require(shinydashboard))
  install.packages('shinydashboard')
library(shinydashboard)

if (!require(dashboardthemes))
  install.packages('dashboardthemes')
library(dashboardthemes)

if (!require(ggplot2))
  install.packages('ggplot2')
library(ggplot2)

if (!require(ggplotify))
  install.packages('ggplotify')
library(ggplotify)

if (!require(ape))
  install.packages('ape')
library(ape)

if (!require(treeio))
  install('treeio')
library(treeio)

if (!require(ggtree))
  install('ggtree')
library(ggtree)

if (!require(ggtreeExtra))
  install('ggtreeExtra')
library(ggtreeExtra)

if (!require(tidyverse))
  install.packages('tidyverse')
library(tidyverse)

if (!require(rlang))
  install.packages('rlang')
library(rlang)

if (!require(tidytree))
  install.packages('tidytree')
library(tidytree)

if (!require(shinyFiles))
  install.packages('shinyFiles')
library(shinyFiles)

if (!require(dplyr))
  install.packages('dplyr')
library(dplyr)

if (!require(downloader))
  install.packages('downloader')
library(downloader)

if (!require(rvest))
  install.packages('rvest')
library(rvest)

if (!require(rmarkdown))
  install.packages('rmarkdown')
library(rmarkdown)

if (!require(knitr))
  install.packages('knitr')
library(knitr)

if (!require(kableExtra))
  install.packages('kableExtra')
library(kableExtra)

if (!require(fs))
  install.packages('fs')
library(fs)

if (!require(data.table))
  install.packages('data.table')
library(data.table)

if (!require(zoo))
  install.packages('zoo')
library(zoo)

if (!require(ggnetwork))
  install.packages('ggnetwork')
library(ggnetwork)

if (!require(igraph))
  install.packages('igraph')
library(igraph)

if (!require(KneeArrower))
  install.packages('KneeArrower')
library(KneeArrower)

if (!require(rhandsontable))
  install.packages('rhandsontable')
library(rhandsontable)

country_names <- c(
  "Afghanistan",
  "Albania",
  "Algeria",
  "Andorra",
  "Angola",
  "Antigua and Barbuda",
  "Argentina",
  "Armenia",
  "Australia",
  "Austria",
  "Azerbaijan",
  "Bahamas",
  "Bahrain",
  "Bangladesh",
  "Barbados",
  "Belarus",
  "Belgium",
  "Belize",
  "Benin",
  "Bhutan",
  "Bolivia",
  "Bosnia and Herzegovina",
  "Botswana",
  "Brazil",
  "Brunei",
  "Bulgaria",
  "Burkina Faso",
  "Burundi",
  "Côte d'Ivoire",
  "Cabo Verde",
  "Cambodia",
  "Cameroon",
  "Canada",
  "Central African Republic",
  "Chad",
  "Chile",
  "China",
  "Colombia",
  "Comoros",
  "Congo (Congo-Brazzaville)",
  "Costa Rica",
  "Croatia",
  "Cuba",
  "Cyprus",
  "Czechia (Czech Republic)",
  "Democratic Republic of the Congo (Congo-Kinshasa)",
  "Denmark",
  "Djibouti",
  "Dominica",
  "Dominican Republic",
  "Ecuador",
  "Egypt",
  "El Salvador",
  "Equatorial Guinea",
  "Eritrea",
  "Estonia",
  'Eswatini (fmr. "Swaziland")',
  "Ethiopia",
  "Fiji",
  "Finland",
  "France",
  "Gabon",
  "Gambia",
  "Georgia",
  "Germany",
  "Ghana",
  "Greece",
  "Grenada",
  "Guatemala",
  "Guinea",
  "Guinea-Bissau",
  "Guyana",
  "Haiti",
  "Holy See",
  "Honduras",
  "Hungary",
  "Iceland",
  "India",
  "Indonesia",
  "Iran",
  "Iraq",
  "Ireland",
  "Israel",
  "Italy",
  "Jamaica",
  "Japan",
  "Jordan",
  "Kazakhstan",
  "Kenya",
  "Kiribati",
  "Kuwait",
  "Kyrgyzstan",
  "Laos",
  "Latvia",
  "Lebanon",
  "Lesotho",
  "Liberia",
  "Libya",
  "Liechtenstein",
  "Lithuania",
  "Luxembourg",
  "Madagascar",
  "Malawi",
  "Malaysia",
  "Maldives",
  "Mali",
  "Malta",
  "Marshall Islands",
  "Mauritania",
  "Mauritius",
  "Mexico",
  "Micronesia",
  "Moldova",
  "Monaco",
  "Mongolia",
  "Montenegro",
  "Morocco",
  "Mozambique",
  "Myanmar (formerly Burma)",
  "Namibia",
  "Nauru",
  "Nepal",
  "Netherlands",
  "New Zealand",
  "Nicaragua",
  "Niger",
  "Nigeria",
  "North Korea",
  "North Macedonia (formerly Macedonia)",
  "Norway",
  "Oman",
  "Pakistan",
  "Palau",
  "Palestine State",
  "Panama",
  "Papua New Guinea",
  "Paraguay",
  "Peru",
  "Philippines",
  "Poland",
  "Portugal",
  "Qatar",
  "Romania",
  "Russia",
  "Rwanda",
  "Saint Kitts and Nevis",
  "Saint Lucia",
  "Saint Vincent and the Grenadines",
  "Samoa",
  "San Marino",
  "Sao Tome and Principe",
  "Saudi Arabia",
  "Senegal",
  "Serbia",
  "Seychelles",
  "Sierra Leone",
  "Singapore",
  "Slovakia",
  "Slovenia",
  "Solomon Islands",
  "Somalia",
  "South Africa",
  "South Korea",
  "South Sudan",
  "Spain",
  "Sri Lanka",
  "Sudan",
  "Suriname",
  "Sweden",
  "Switzerland",
  "Syria",
  "Tajikistan",
  "Tanzania",
  "Thailand",
  "Timor-Leste",
  "Togo",
  "Tonga",
  "Trinidad and Tobago",
  "Tunisia",
  "Turkey",
  "Turkmenistan",
  "Tuvalu",
  "Uganda",
  "Ukraine",
  "United Arab Emirates",
  "United Kingdom",
  "United States of America",
  "Uruguay",
  "Uzbekistan",
  "Vanuatu",
  "Venezuela",
  "Vietnam",
  "Yemen",
  "Zambia",
  "Zimbabwe"
)

sel_countries <-
  c("Austria",
    "Germany",
    "Switzerland",
    "United Kingdom",
    "United States of America")

# User Interface ----

ui <- dashboardPage(
  # Title
  dashboardHeader(title = span(
    div(
      class = "img_logo",
      img(
        src = "PhyloTree.jpg", width = 190
      )
    )
  ),
  disable = FALSE),
  
  ## Sidebar ----
  dashboardSidebar(
    tags$style("label{color: white;}"),
    tags$style("file.select{background-color: white;}"),
    tags$style(
      HTML("#include_edge {width: 20px; height: 20px; margin-top: 13px}")
    ),
    tags$style(
      HTML("#include_node {width: 20px; height: 20px; margin-top: 13px}")
    ),
    tags$style(".selectize-control.single .selectize-input:after {right: 10px}"),
    tags$style("#scheme_db .selectize-control {font-size: 12px, }"),
    tags$style(".scheme_start {margin-left: -20px}"),
    tags$style("i.fas.fa-rotate {position: relative; left: -5px;}"),
    tags$style("button#reload_db.btn.btn-default.action-button.shiny-bound-input {height: 30px; width: 30px; position: relative; left: -20px}"),
    tags$style("button#edit_button.btn.btn-default.action-button.shiny-bound-input {background: #20E6E5; color: #000000}"),
    br(), br(),
    uiOutput("loaded_scheme"),
    sidebarMenu(
      id = "tabs",
      uiOutput("menu_sep1"),
      sidebarMenuOutput("menu"),
      uiOutput("menu_sep2"),
      conditionalPanel(
        "input.tabs==='database'",
        column(
          width = 12,
          align = "center",
          br(), br(),
          p(
            HTML(
              paste(
                tags$span(style='color: white; font-size: 18px; margin-bottom: 0px', 'Save database changes')
              )
            )
          ),
          uiOutput("edit_button")
        )
      ),
      conditionalPanel(
        "input.tabs==='typing'",
        column(
          width = 12,
          align = "left",
          br(),
          prettyRadioButtons(
            inputId = "typing_mode",
            label = "Typing Mode",
            choices = c("Single", "Multi"),
            selected = "Single"
          ),
          br()
        )
      ),
      conditionalPanel(
        "input.tabs==='report'",
        column(
          width = 12,
          align = "left",
          br(),
          uiOutput("selProfile"),
          hr(),
          textInput(
            inputId = "rep_profilename",
            label = "Save current settings as",
            width = "100%"
          ),
          br()
        ),
        column(
          width = 12,
          align = "center",
          actionButton(
            "save_rep_profile",
            label = "Save Profile"
          )
        )
      ),
      conditionalPanel(
        "input.tabs==='visualization'",
        column(
          width = 12,
          br(),
          prettyRadioButtons(
            "tree_algo",
            choices = c("Minimum-Spanning", "Neighbour-Joining"),
            label = "Tree Type"
          ),
          conditionalPanel(
            "input.tree_algo=='Minimum-Spanning'",
            selectInput(
              "algo_mode",
              label = "Interpretation Mode",
              choices = c("directed", "undirected", "max", "min", "upper", "lower", "plus"),
              selected = "undirected"
            ),
            selectInput(
              "ggnetwork_layout",
              label = "Layout Algorithm",
              choices = c(
                "Davidson-Harel",
                "DrL",
                "Fruchterman-Reingold",
                "GEM",
                "Graphopt",
                "Kamada-Kawai",
                "Large Graph Layout",
                "Multidimensional Scaling",
                "Sugiyama"
              ),
              selected = "Fruchterman-Reingold"
            ),
            br()
          ),
          fluidRow(
            tags$style("button#save_plot {height: 34px; background: #20E6E5; color: #000000}"),
            column(
              width = 6,
              actionButton("create_tree",
                           "Create Tree")
            ),
            column(
              width = 6,
              actionBttn(
                "save_plot",
                style = "simple",
                label = "",
                size = "sm",
                icon = icon("download"),
                color = "primary"
                
              )
            )
          ),
          fluidRow(
            br(),
            column(
              width = 12,
              conditionalPanel(
                "input.create_tree>0",
                box(
                  solidHeader = TRUE,
                  status = "primary",
                  width = "100%",
                  fluidRow(
                    column(
                      width = 3,
                      tags$style("#slot_select {margin-top: -25px; margin-left: -5px;}"),
                      prettyRadioButtons(
                        "slot_select",
                        choices = c("Slot 1", "Slot 2", "Slot 3", "Slot 4"),
                        label = ""
                      )  
                    ),
                    column(2),
                    column(
                      width = 5,
                      align = "right",
                      uiOutput("slot1_status"),
                      uiOutput("slot2_status"),
                      uiOutput("slot3_status"),
                      uiOutput("slot4_status"),
                    )
                  ),
                  br(),
                  fluidRow(
                    column(width = 1),
                    column(
                      width = 1,
                      align = "left",
                      actionBttn(
                        "add_slot",
                        style = "simple",
                        size = "sm",
                        icon = icon("plus"),
                        color = "primary"
                      )
                    ),
                    column(1),
                    column(
                      width = 2,
                      align = "left",
                      actionBttn(
                        "delete_slot",
                        style = "simple",
                        size = "sm",
                        icon = icon("trash"),
                        color = "danger"
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$style(
      HTML(
        '
                .shiny-input-container input[type="text"] {
                border-radius: 5px;
                }

                .box.box-solid.box-primary>.box-header {
                background:#282F38
                }

                .box.box-solid.box-primary{
                background:#282F38
                }

                '
      )
    ),
    
    shinyDashboardThemeDIY(
      ### general
      appFontFamily = "Tahoma"
      ,
      appFontColor = "#000000"
      ,
      primaryFontColor = "#ffffff"
      ,
      infoFontColor = "rgb(0,0,0)"
      ,
      successFontColor = "rgb(0,0,0)"
      ,
      warningFontColor = "rgb(0,0,0)"
      ,
      dangerFontColor = "rgb(0,0,0)"
      ,
      bodyBackColor = cssGradientThreeColors(
        direction = "down"
        ,
        colorStart = "#282f38"
        ,
        colorMiddle = "#384454"
        ,
        colorEnd = "#495d78"
        ,
        colorStartPos = 0
        ,
        colorMiddlePos = 50
        ,
        colorEndPos = 100
      )
      
      ### header
      ,
      logoBackColor = "#282f38"
      
      ,
      headerButtonBackColor = "#282f38"
      ,
      headerButtonIconColor = "#18ece1"
      ,
      headerButtonBackColorHover = "#282f38"
      ,
      headerButtonIconColorHover = "#ffffff"
      
      ,
      headerBackColor = "#282f38"
      ,
      headerBoxShadowColor = "#aaaaaa"
      ,
      headerBoxShadowSize = "0px 0px 0px"
      
      ### sidebar
      ,
      sidebarBackColor = cssGradientThreeColors(
        direction = "down"
        ,
        colorStart = "#282f38"
        ,
        colorMiddle = "#384454"
        ,
        colorEnd = "#495d78"
        ,
        colorStartPos = 0
        ,
        colorMiddlePos = 50
        ,
        colorEndPos = 100
      )
      ,
      sidebarPadding = 0
      
      ,
      sidebarMenuBackColor = "transparent"
      ,
      sidebarMenuPadding = 0
      ,
      sidebarMenuBorderRadius = 0
      
      ,
      sidebarShadowRadius = "5px 5px 5px"
      ,
      sidebarShadowColor = "#282f38"
      
      ,
      sidebarUserTextColor = "#ffffff"
      
      ,
      sidebarSearchBackColor = "rgb(55,72,80)"
      ,
      sidebarSearchIconColor = "rgb(153,153,153)"
      ,
      sidebarSearchBorderColor = "rgb(55,72,80)"
      
      ,
      sidebarTabTextColor = "rgb(255,255,255)"
      ,
      sidebarTabTextSize = 15
      ,
      sidebarTabBorderStyle = "none none solid none"
      ,
      sidebarTabBorderColor = "rgb(35,106,135)"
      ,
      sidebarTabBorderWidth = 0
      
      ,
      sidebarTabBackColorSelected = cssGradientThreeColors(
        direction = "right"
        ,
        colorStart = "rgba(44,222,235,1)"
        ,
        colorMiddle = "rgba(44,222,235,1)"
        ,
        colorEnd = "rgba(0,255,213,1)"
        ,
        colorStartPos = 0
        ,
        colorMiddlePos = 30
        ,
        colorEndPos = 100
      )
      ,
      sidebarTabTextColorSelected = "rgb(0,0,0)"
      ,
      sidebarTabRadiusSelected = "0px 0px 0px 0px"
      
      ,
      sidebarTabBackColorHover = cssGradientThreeColors(
        direction = "right"
        ,
        colorStart = "rgba(44,222,235,1)"
        ,
        colorMiddle = "rgba(44,222,235,1)"
        ,
        colorEnd = "rgba(0,255,213,1)"
        ,
        colorStartPos = 0
        ,
        colorMiddlePos = 30
        ,
        colorEndPos = 100
      )
      ,
      sidebarTabTextColorHover = "rgb(50,50,50)"
      ,
      sidebarTabBorderStyleHover = "none none solid none"
      ,
      sidebarTabBorderColorHover = "rgb(75,126,151)"
      ,
      sidebarTabBorderWidthHover = 1
      ,
      sidebarTabRadiusHover = "0px 0px 0px 0px"
      
      ### boxes
      ,
      boxBackColor = "#ffffff"
      ,
      boxBorderRadius = 7
      ,
      boxShadowSize = "0px 0px 0px"
      ,
      boxShadowColor = "#ffffff"
      ,
      boxTitleSize = 20
      ,
      boxDefaultColor = "#00a65a"
      ,
      boxPrimaryColor = "#ffffff"
      ,
      boxInfoColor = "#00a65a"
      ,
      boxSuccessColor = "#00a65a"
      ,
      boxWarningColor = "#ffffff"
      ,
      boxDangerColor = "#ffffff"
      
      ,
      tabBoxTabColor = "#ffffff"
      ,
      tabBoxTabTextSize = 14
      ,
      tabBoxTabTextColor = "rgb(0,0,0)"
      ,
      tabBoxTabTextColorSelected = "rgb(0,0,0)"
      ,
      tabBoxBackColor = "#ffffff"
      ,
      tabBoxHighlightColor = "#ffffff"
      ,
      tabBoxBorderRadius = 5
      
      ### inputs
      ,
      buttonBackColor = "#282F38"
      ,
      buttonTextColor = "#ffffff"
      ,
      buttonBorderColor = "#282F38"
      ,
      buttonBorderRadius = 5
      
      ,
      buttonBackColorHover = cssGradientThreeColors(
        direction = "right"
        ,
        colorStart = "rgba(44,222,235,1)"
        ,
        colorMiddle = "rgba(44,222,235,1)"
        ,
        colorEnd = "rgba(0,255,213,1)"
        ,
        colorStartPos = 0
        ,
        colorMiddlePos = 30
        ,
        colorEndPos = 100
      )
      ,
      buttonTextColorHover = "#000000"
      ,
      buttonBorderColorHover = "transparent"
      
      ,
      textboxBackColor = "#ffffff"
      ,
      textboxBorderColor = "#000000"
      ,
      textboxBorderRadius = 5
      ,
      textboxBackColorSelect = "#ffffff"
      ,
      textboxBorderColorSelect = "#000000"
      
      ### tables
      ,
      tableBackColor = "rgb(255,255,255)"
      ,
      tableBorderColor = "rgb(240,240,240)"
      ,
      tableBorderTopSize = 1
      ,
      tableBorderRowSize = 1
    ),
    tags$style(".image {height: 300px;}"),
    tags$style(".select_start_radio {position: relative; right: -55px;}"),
    uiOutput("start_message"),
    
    tabItems(
      ## Tab Database ----
      
      tabItem(
        tabName = "database",
        fluidRow(column(
          width = 3,
          align = "center",
          h2(p("Browse Local Database"), style = "color:white"),
        )),
        hr(), br(),
        br(),
        br(),
        fluidRow(
          column(
            width = 8,
            column(
              width = 12,
              align = "center",
              uiOutput("db_no_entries"),
              uiOutput("no_db")
            ),
            uiOutput("db_entries_table")
          ),
          column(width = 1),
          column(
            width = 2,
            align = "left",
            uiOutput("delete_box"),
            uiOutput("compare_allele_box")
          )
        ),
        fluidRow(
          br(),
          br(),
          column(width = 1),
          column(width = 11,
                 align = "center",
                 uiOutput("edit_field"))
        ),
        br(),
        uiOutput("db_line"),
        br(),
        fluidRow(
          tags$style(".test .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
    color: #ffffff !important;}"),
          column(
            width = 5,
            align = "center",
            uiOutput("scheme_header"),
            br(),
            br(),
            uiOutput("scheme_info")
          ),
          column(
            width = 7,
            align = "center",
            uiOutput("loci_header"),
            br(),
            div(class = "test",
                dataTableOutput("db_loci"))
          )
        )
      ),
      
      ## Tab Add Scheme  ----  
      
      tabItem(
        tabName = "init",
        fluidRow(column(
          width = 3,
          align = "center",
          h2(p("Select cgMLST Scheme"), style = "color:white"),
        )),
        hr(),
        fluidRow(
          column(
            width = 3,
            align = "center",
            br(),
            br(),
            br(),
            selectInput(
              inputId = "select_cgmlst",
              label = NULL,
              choices = list(
                "Acinetobacter baumanii",
                "Bacillus anthracis",
                "Bordetella pertussis",
                "Brucella melitensis",
                "Brucella spp.",
                "Burkholderia mallei (FLI)",
                "Burkholderia mallei (RKI)",
                "Burkholderia pseudomallei",
                "Campylobacter jejuni/coli",
                "Clostridioides difficile",
                "Clostridium perfringens",
                "Corynebacterium diphtheriae",
                "Cronobacter sakazakii/malonaticus",
                "Enterococcus faecalis",
                "Enterococcus faecium",
                "Escherichia coli",
                "Francisella tularensis",
                "Klebsiella pneumoniae sensu lato",
                "Legionella pneumophila",
                "Listeria monocytogenes",
                "Mycobacterium tuberculosis complex",
                "Mycobacteroides abscessus",
                "Mycoplasma gallisepticum",
                "Paenibacillus larvae",
                "Pseudomonas aeruginosa",
                "Salmonella enterica",
                "Serratia marcescens",
                "Staphylococcus aureus",
                "Staphylococcus capitis",
                "Streptococcus pyogenes"
              ),
              selected = "Bordetella pertussis",
              width = "300px"
            )
          ),
          column(
            width = 2,
            align = "center",
            br(),
            br(),
            br(),
            actionButton(inputId = "download_cgMLST", label = "Download")
          ),
          column(width = 1),
          column(
            width = 5,
            br(),
            br(),
            br(),
            align = "center",
            h4(p("Downloaded Loci"), style = "color:white"),
          )
        ),
        fluidRow(
          column(
            width = 5,
            align = "center",
            br(),
            br(),
            tableOutput("cgmlst_scheme")
          ),
          column(width = 1),
          column(
            width = 5,
            align = "right",
            br(),
            br(),
            conditionalPanel(
              "input.download_cgMLST >= 1",
              addSpinner(
                dataTableOutput("cgmlst_targets"),
                spin = "dots",
                color = "#ffffff"
              )
            )
          )
        )
      ),
      
      
      
      ## Tab Allelic Typing ----------------------------------------------
      
      
      tabItem(
        tabName = "typing",
        fluidRow(column(
          width = 3,
          align = "center",
          h2(p("Generate Allelic Profile"), style = "color:white"),
        )),
        hr(),
        conditionalPanel(
          "input.typing_mode == 'Single'",
          fluidRow(
            tags$style("span#progress_bar-title.progress-text {color: white; font-size: 13px; font-weight: normal;}"),
            tags$style("div#progress_bar.progress-bar {font-size:13px; line-height: 30px;}"),
            tags$style(".progress {border-radius: 5px; height: 30px; line-height: 30px}"),
            uiOutput("initiate_typing_ui"),
            uiOutput("single_typing_progress"),
            column(1),
            uiOutput("metadata_single_box"),
            column(width = 1),
            uiOutput("start_typing_ui")
          )
        ),
        conditionalPanel(
          "input.typing_mode == 'Multi'",
          conditionalPanel(
            condition = "output.pending_multi=='no'",
            fluidRow(
              column(
                width = 3,
                align = "center",
                br(),
                br(),
                uiOutput("initiate_typing_header"),
                br(),
                br(),
                p(
                  HTML(
                    paste(
                      tags$span(style='color: white; font-size: 15px; margin-bottom: 0px', 'Select Assembly Folder')
                    )
                  )
                ),
                column(
                  width = 12,
                  align = "center",
                  uiOutput("genome_file_multi_bttn"),
                  br(),
                  br()
                ),
                column(
                  width = 12,
                  align = "left",
                  uiOutput("assembly_files_table")
                )
              ),
              column(1),
              column(
                width = 3,
                align = "center",
                tags$style(".append_table {margin-top: 0px; margin-left: 3px}"),
                tags$style("div#append_isodate_multi.shiny-date-input.form-group.shiny-input-container.shiny-bound-input input.form-control.shinyjs-resettable {text-align: center}"),
                tags$style("div#append_analysisdate_multi.shiny-date-input.form-group.shiny-input-container.shiny-bound-input input.form-control.shinyjs-resettable {text-align: center}"),
                tags$style(".append_table_country .btn {height: 32px}"),
                tags$style(".append_table_country {margin-top: 23px; margin-bottom: 5px}"),
                tags$style("button#conf_meta_multi {background: #20e6e5; color: black}"),
                br(),
                br(),
                uiOutput("header_declare_metadata"),
                br(), br(),
                uiOutput("metadata_multi_box")
              ),
              column(1),
              uiOutput("start_multi_typing_ui")
            )
          ),
          conditionalPanel(
            condition = "output.pending_multi=='yes'",
            fluidRow(
              br(), br(),
              column(width = 1),
              column(
                width = 2,
                uiOutput("multi_typing_progress_header")
              ),
              column(
                width = 2,
                br(),
                uiOutput("multi_typing_progress_symbol") 
              )
            ),
            fluidRow(
              column(width = 1),
              column(
                width = 2,
                br(), br(),
                actionButton(
                  "reset_multi",
                  "Cancel"
                )
              )
            ),
            br(), br(),
            fluidRow(
              column(width = 1),
              column(
                width = 6,
                verbatimTextOutput("logText")
              )  
            )
          )
        )
      ),
      
      
      ## Tab Visualization -------------------------------------------------------
      
      
      tabItem(
        tabName = "visualization",
        
        fluidRow(
          column(width = 1),
          column(
            width = 10,
            br(),
            addSpinner(
              plotOutput("tree_local", width = "100%", height = "600px"),
              spin = "dots",
              color = "#ffffff"
            )  
          ),
          column(width = 1)
        ),
        
        br(),
        hr(),
        ##### Control Panels
        
        ####### Show Control for Trees generated from Local
        
        conditionalPanel(
          "input.tree_algo=='Minimum-Spanning'",
          fluidRow(
            tags$style("button#node_menu {height: 34px; background: #20E6E5; color: #000000; border-radius: 5px}"),
            tags$style("button#edge_menu {height: 34px; background: #20E6E5; color: #000000; border-radius: 5px}"),
            tags$style("button#nodelabel_menu {height: 34px; background: #20E6E5; color: #000000; border-radius: 5px}"),
            tags$style("button#nodepanel_menu {height: 34px; background: #20E6E5; color: #000000; border-radius: 5px}"),
            tags$style("button#edgelabel_menu {height: 34px; background: #20E6E5; color: #000000; border-radius: 5px}"),
            tags$style("button#edgepanel_menu {height: 34px; background: #20E6E5; color: #000000; border-radius: 5px}"),
            tags$style("button#add_metadata {height: 34px; background: #20E6E5; color: #000000; border-radius: 5px}"),
            tags$style("button#delete_meta {height: 34px; font-size: 19px}"),
            tags$style("i.fas.fa-xmark {vertical-align: text-top;}"),
            tags$style("input.form-control.pickr-color {text-align: center; font-size: 11px;}"),
            column(width = 2,
                   fluidRow(
                     column(
                       width = 6,
                       align = "center",
                       box(
                         solidHeader = TRUE,
                         status = "primary",
                         width = "100%",
                         h3(p("Nodes"), style = "color:white"),
                         colorPickr(
                           inputId = "color_node",
                           width = "100%",
                           selected = "#058C31",
                           label = "",
                           update = "changestop",
                           interaction = list(clear = FALSE,
                                              save = FALSE),
                           position = "right-start"
                         ),
                         br(),
                         dropMenu(
                           actionBttn(
                             "node_menu",
                             label = "",
                             color = "default",
                             size = "sm",
                             style = "material-flat",
                             icon = icon("sliders")
                           ),
                           placement = "top-start",
                           theme = "translucent",
                           numericInput(
                             "alpha_node",
                             label = h5("Alpha", style = "color:white; margin-bottom: 0px;"),
                             value = 1,
                             step = 0.1,
                             min = 0,
                             max = 1,
                             width = "90%"
                           ),
                           numericInput(
                             inputId = "size_node",
                             label = h5("Size", style = "color:white; margin-bottom: 0px;"),
                             value = 10,
                             min = 1,
                             max = 20,
                             step = 1,
                             width = "90%"
                           )
                         ),
                         br()
                       )
                     ),
                     column(
                       align = "center",
                       width = 6,
                       box(
                         solidHeader = TRUE,
                         status = "primary",
                         width = "100%",
                         h3(p("Edges"), style = "color:white"),
                         colorPickr(
                           inputId = "color_edge",
                           width = "100%",
                           selected = "#000000",
                           label = "",
                           update = "changestop",
                           interaction = list(clear = FALSE,
                                              save = FALSE),
                           position = "right-start"
                         ),
                         br(),
                         dropMenu(
                           actionBttn(
                             "edge_menu",
                             label = "",
                             color = "default",
                             size = "sm",
                             style = "material-flat",
                             icon = icon("sliders")
                           ),
                           theme = "translucent",
                           placement = "top-start",
                           numericInput(
                             "size_edge",
                             label = h5("Size", style = "color:white; margin-bottom: 0px;"),
                             value = 0.7,
                             step = 0.1,
                             min = 0.5,
                             max = 1.5,
                             width = "90%"
                           ),
                           numericInput(
                             "alpha_edge",
                             label = h5("Alpha", style = "color:white; margin-bottom: 0px;"),
                             value = 0.7,
                             step = 0.1,
                             min = 0,
                             max = 1,
                             width = "90%"
                           ),
                           numericInput(
                             "curvature_edge",
                             label = h5("Curves", style = "color:white; margin-bottom: 0px;"),
                             value = 0,
                             step = 0.1,
                             min = -0.5,
                             max = 0.5,
                             width = "90%"
                           )
                         ),
                         br()
                       )
                     )
                   ),
                   fluidRow(column(
                     width = 12,
                     align = "center",
                     box(
                       solidHeader = TRUE,
                       status = "primary",
                       width = "100%",
                       h3(p("Other"), style = "color:white"),
                       column(
                         width = 6,
                         colorPickr(
                           inputId = "color_bg",
                           width = "100%",
                           selected = "#ffffff",
                           label = h5("Background", style = "color:white; margin-bottom: 0px;"),
                           update = "changestop",
                           interaction = list(clear = FALSE,
                                              save = FALSE),
                           position = "right-start"
                         )
                       ),
                       column(width = 6,
                              br(),
                              uiOutput("cluster_start"))
                     )
                   ))),
            column(
              width = 2,
              align = "center",
              box(
                solidHeader = TRUE,
                status = "primary",
                width = "100%",
                fluidRow(
                  column(
                    width = 9,
                    h3(p("Label Node"), style = "color:white"),
                  ),
                  column(
                    width = 3,
                    checkboxInput("include_node",
                                  label = "",
                                  value = TRUE)
                  )
                ),
                selectInput(
                  "node_label",
                  label = "",
                  choices = c(
                    Index = "index",
                    `Assembly ID` = "assembly_id",
                    `Assembly Name` = "assembly_name",
                    Scheme = "scheme",
                    `Isolation Date` = "isolation_date",
                    Host = "host",
                    Country = "country",
                    City = "city"
                  ),
                  selected = c(`Assembly Name` = "assembly_name"),
                  width = "80%"
                ),
                fluidRow(
                  column(width = 1),
                  column(
                    width = 6,
                    align = "center",
                    colorPickr(
                      inputId = "label_color",
                      width = "95%",
                      selected = "#000000",
                      label = "",
                      update = "changestop",
                      interaction = list(clear = FALSE,
                                         save = FALSE),
                      position = "right-start"
                    )
                  ),
                  column(
                    width = 3,
                    align = "center",
                    br(),
                    dropMenu(
                      actionBttn(
                        "nodelabel_menu",
                        label = "",
                        color = "default",
                        size = "sm",
                        style = "material-flat",
                        icon = icon("sliders")
                      ),
                      theme = "translucent",
                      placement = "top-start",
                      maxWidth = "1000px",
                      column(
                        width = 6,
                        numericInput(
                          "label_size",
                          label = h5("Size", style = "color:white; margin-bottom: 0px;"),
                          value = 4,
                          min = 2,
                          max = 10,
                          step = 0.5,
                          width = "170px"
                        ),
                        numericInput(
                          "x_nudge",
                          label = h5("X Position", style = "color:white; margin-bottom: 0px;"),
                          value = 0,
                          min = -10,
                          max = 10,
                          step = 0.5,
                          width = "170px"
                        ),
                        numericInput(
                          "y_nudge",
                          label = h5("Y Position", style = "color:white; margin-bottom: 0px;"),
                          value = 0,
                          min = -10,
                          max = 10,
                          step = 0.5,
                          width = "170px"
                        ),
                      ),
                      column(
                        width = 6,
                        numericInput(
                          "label_alpha",
                          label = h5("Alpha", style = "color:white; margin-bottom: 0px;"),
                          value = 1,
                          step = 0.1,
                          min = 0,
                          max = 1,
                          width = "170px"
                        ),
                        numericInput(
                          "box_padding",
                          label = h5("Box Padding", style = "color:white; margin-bottom: 0px;"),
                          value = 0.25,
                          min = 0,
                          max = 1,
                          step = 0.05,
                          width = "170px"
                        ),
                        numericInput(
                          "point_padding",
                          label = h5("Point Padding", style = "color:white; margin-bottom: 0px;"),
                          value = 0.25,
                          min = 0,
                          max = 1,
                          step = 0.05,
                          width = "170px"
                        )
                      ),
                      h5(".", style = "color:black; font-size: 1px; margin-bottom: 0px;")
                    )
                  ),
                  column(width = 2)
                ),
                fluidRow(column(
                  width = 12,
                  br(),
                  checkboxInput("label_rect",
                                label = "Show Panel",
                                value = TRUE)
                )),
                fluidRow(
                  column(width = 1),
                  column(
                    width = 6,
                    colorPickr(
                      inputId = "label_fillcolor",
                      width = "95%",
                      selected = "#ffffff",
                      label = "",
                      update = "changestop",
                      interaction = list(clear = FALSE,
                                         save = FALSE),
                      position = "right-start"
                    )
                  ),
                  column(
                    width = 3,
                    br(),
                    dropMenu(
                      actionBttn(
                        "nodepanel_menu",
                        label = "",
                        color = "default",
                        size = "sm",
                        style = "material-flat",
                        icon = icon("sliders")
                      ),
                      theme = "translucent",
                      placement = "top-start",
                      maxWidth = "200px",
                      numericInput(
                        "panel_padding",
                        label = h5("Label Padding", style = "color:white; margin-bottom: 0px;"),
                        value = 0.3,
                        min = 0.2,
                        max = 1,
                        step = 0.1,
                        width = "70px"
                      ),
                      numericInput(
                        "panel_radius",
                        label = h5("Radius", style = "color:white; margin-bottom: 0px;"),
                        value = 0.2,
                        min = 0,
                        max = 1,
                        step = 0.1,
                        width = "70px"
                      ),
                      numericInput(
                        "panel_bordersize",
                        label = h5("Size", style = "color:white; margin-bottom: 0px;"),
                        value = 0.5,
                        min = 0,
                        max = 4,
                        step = 0.25,
                        width = "70px"
                      )
                    )
                  )
                ),
                br(),
                br()
              )
            ),
            column(
              width = 2,
              align = "center",
              box(
                solidHeader = TRUE,
                status = "primary",
                width = "100%",
                fluidRow(
                  column(
                    width = 9,
                    h3(p("Label Edge"), style = "color:white")
                  ),
                  column(
                    width = 3,
                    checkboxInput("include_edge",
                                  label = "",
                                  value = FALSE)
                  )
                ),
                selectInput(
                  "edge_label",
                  label = "",
                  choices = c(
                    Index = "index",
                    `Assembly ID` = "assembly_id",
                    `Assembly Name` = "assembly_name",
                    Scheme = "scheme",
                    `Isolation Date` = "isolation_date",
                    Host = "host",
                    Country = "country",
                    City = "city"
                  ),
                  selected = c(City = "city"),
                  width = "80%"
                ),
                fluidRow(
                  column(width = 1),
                  column(
                    width = 6,
                    align = "center",
                    colorPickr(
                      inputId = "edgelabel_color",
                      width = "95%",
                      selected = "#000000",
                      label = "",
                      update = "changestop",
                      interaction = list(clear = FALSE,
                                         save = FALSE),
                      position = "right-start"
                    ),
                  ),
                  column(
                    width = 3,
                    br(),
                    dropMenu(
                      actionBttn(
                        "edgelabel_menu",
                        label = "",
                        color = "default",
                        size = "sm",
                        style = "material-flat",
                        icon = icon("sliders")
                      ),
                      theme = "translucent",
                      placement = "top-start",
                      maxWidth = "250px",
                      fluidRow(
                        column(
                          width = 6,
                          numericInput(
                            "edgelabel_size",
                            label = h5("Size", style = "color:white; margin-bottom: 0px;"),
                            value = 3,
                            min = 2,
                            max = 10,
                            step = 0.5,
                            width = "170px"
                          ),
                          numericInput(
                            "edge_x_nudge",
                            label = h5("X Position", style = "color:white; margin-bottom: 0px;"),
                            value = 0,
                            min = -10,
                            max = 10,
                            step = 0.5,
                            width = "170px"
                          ),
                          numericInput(
                            "edge_y_nudge",
                            label = h5("Y Position", style = "color:white; margin-bottom: 0px;"),
                            value = 0,
                            min = -10,
                            max = 10,
                            step = 0.5,
                            width = "170px"
                          )
                        ),
                        column(
                          width = 6,
                          numericInput(
                            "edgelabel_alpha",
                            label = h5("Alpha", style = "color:white; margin-bottom: 0px;"),
                            value = 0.7,
                            step = 0.1,
                            min = 0,
                            max = 1,
                            width = "170px"
                          ),
                          numericInput(
                            "edge_box_padding",
                            label = h5("Box Padding", style = "color:white; margin-bottom: 0px;"),
                            value = 0.25,
                            min = 0,
                            max = 1,
                            step = 0.05,
                            width = "170px"
                          ),
                          numericInput(
                            "edge_point_padding",
                            label = h5("Point Padding", style = "color:white; margin-bottom: 0px;"),
                            value = 0.25,
                            min = 0,
                            max = 1,
                            step = 0.05,
                            width = "170px"
                          )
                        )
                      ),
                      label = h5(".", style = "color:white; font-size: 1px; margin-bottom: 0px;")
                    )
                  )
                ),
                fluidRow(column(
                  width = 12,
                  br(),
                  checkboxInput("edge_rect",
                                label = "Show Panel",
                                value = TRUE)
                )),
                fluidRow(
                  column(width = 1),
                  column(
                    width = 6,
                    align = "center",
                    colorPickr(
                      inputId = "edgelabel_fillcolor",
                      width = "95%",
                      selected = "#A5A315",
                      label = "",
                      update = "changestop",
                      interaction = list(clear = FALSE,
                                         save = FALSE),
                      position = "right-start"
                    ),
                  ),
                  column(
                    width = 3,
                    align = "center",
                    br(),
                    dropMenu(
                      actionBttn(
                        "edgepanel_menu",
                        label = "",
                        color = "default",
                        size = "sm",
                        style = "material-flat",
                        icon = icon("sliders")
                      ),
                      theme = "translucent",
                      maxWidth = "100px",
                      placement = "top-start",
                      numericInput(
                        "edge_panel_padding",
                        label = h5("Label Padding", style = "color:white; margin-bottom: 0px;"),
                        value = 0.3,
                        min = 0.2,
                        max = 1,
                        step = 0.1,
                        width = "70px"
                      ),
                      numericInput(
                        "edge_panel_radius",
                        label = h5("Radius", style = "color:white; margin-bottom: 0px;"),
                        value = 0.2,
                        min = 0,
                        max = 1,
                        step = 0.1,
                        width = "70px"
                      ),
                      numericInput(
                        "edge_panel_bordersize",
                        label = h5("Size", style = "color:white; margin-bottom: 0px;"),
                        value = 0.5,
                        min = 0,
                        max = 4,
                        step = 0.25,
                        width = "70px"
                      )
                    )
                  )
                ),
                br(),
                br()
              )
            ),
            column(
              width = 2,
              align = "center",
              box(
                solidHeader = TRUE,
                status = "primary",
                width = "100%",
                h3(p("Add Metadata"), style = "color:white"),
                selectInput(
                  "which_metadata",
                  label = "",
                  choices = c(
                    Index = "index",
                    `Assembly ID` = "assembly_id",
                    `Assembly Name` = "assembly_name",
                    Scheme = "scheme",
                    `Isolation Date` = "isolation_date",
                    Host = "host",
                    Country = "country",
                    City = "city"
                  ),
                  selected = c(Host = "host"),
                  width = "80%"
                ),
                selectInput(
                  "element_metadata",
                  label = h5("Select Element", style = "color:white; margin-bottom: 0px;"),
                  choices = c("Node Shape", "Node Size"),
                  width = "80%"
                ),
                br(),
                fluidRow(
                  column(width = 3),
                  column(
                    width = 2,
                    actionButton(
                      "add_metadata",
                      label = "Add"
                    )
                  ),
                  column(width = 1),
                  column(
                    width = 2,
                    actionBttn(
                      "delete_meta",
                      label = "",
                      color = "danger",
                      size = "sm",
                      style = "material-circle",
                      icon = icon("xmark")
                    )
                  )
                ),
                br(),
                br(),
                br(),
                br(),
                br()
              )
            ),
          )
        ),
        
        # Plot Control Neighbor Joining
        
        
        conditionalPanel(
          "input.tree_algo=='Neighbour-Joining'",
          fluidRow(
            column(
              width = 2,
              align = "center",
              box(
                solidHeader = TRUE,
                status = "primary",
                width = "100%",
                h3(p("Layout"), style = "color:white"),
                selectInput(
                  inputId = "nj_layout",
                  h5("Select Theme", style = "color:white"),
                  choices = list(
                    Linear = list(
                      "Rectangular" = "rectangular",
                      "Roundrect" = "roundrect",
                      "Slanted" = "slanted",
                      "Ellipse" = "ellipse"
                    ),
                    Circular = list("Circular" = "circular",
                                    "Fan" = "fan"),
                    Unrooted = list("Daylight" = "daylight",
                                    "Equal Angle" = "equal_angle")
                  ),
                  selected = "rectangular",
                  width = "70%"
                ),
                br(),
                conditionalPanel(
                  "input.nj_layout=='circular' | input.nj_layout=='fan'",
                  checkboxInput("circ_inward",
                                label = "Inward Layout",
                                value = FALSE)
                )
              )
            ),
            column(
              width = 2,
              align = "center",
              box(
                solidHeader = TRUE,
                status = "primary",
                width = "100%",
                h3(p("Tip Label"), style = "color:white"),
                selectInput(
                  "nj_tiplab",
                  label = "",
                  choices = c(
                    Index = "index",
                    `Assembly ID` = "assembly_id",
                    `Assembly Name` = "assembly_name",
                    `Isolation Date` = "isolation_date",
                    Host = "host",
                    Country = "country",
                    City = "city"
                  ),
                  selected = c(`Assembly Name` = "assembly_name"),
                  width = "75%"
                ),
                numericInput(
                  "nj_tip_offset",
                  label = h5("Offset", style = "color:white"),
                  min = -5,
                  max = 5,
                  step = 0.25,
                  value = 0,
                  width = "35%"
                )
              )
            )
          )
        )
      ),
      
      ## Tab Report --------------------------------------------------------------
      
      
      tabItem(
        tabName = "report",
        fluidRow(column(
          width = 3,
          align = "center",
          h2(p("Create Report"), style = "color:white"),
        )),
        hr(),
        fluidRow(
          br(),
          br(),
          column(
            width = 3,
            align = "left",
            br(),
            box(
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              HTML(
                paste(
                  tags$span(style='color: white; font-size: 24px;', 'General')
                )
              ),
              br(),
              fluidRow(
                tags$style("#date_general_select .form-control {height: 28px; position: relative; right: -22px} "),
                tags$style("#date_general {margin-top: 17px} .form-group {margin-bottom: 0px;}"),
                column(
                  width = 3,
                  checkboxInput(
                    "date_general", 
                    label = h5("Date", style = "color:white; font-size: 17px; margin-top: 16px;"),
                    value = TRUE
                  )
                ),
                column(
                  width = 7,
                  dateInput(
                    "date_general_select",
                    "",
                    width = "50%"
                  )
                )
              ),
              fluidRow(
                tags$style("#operator_general_select {height: 28px; margin-top: -15px; position: relative; right: -22px}"),
                tags$style("#operator_general {margin-top: 0px;}"),
                column(
                  width = 3,
                  checkboxInput(
                    "operator_general", 
                    label = h5("Operator", style = "color:white; font-size: 17px; margin-top: -1px;"),
                    value = TRUE
                  )
                ),
                column(
                  width = 8,
                  textInput(
                    "operator_general_select",
                    ""
                  ) 
                )
              ),
              fluidRow(
                tags$style("#institute_general_select {height: 28px; margin-top: -15px; position: relative; right: -22px}"),
                tags$style("#institute_general {margin-top: 0px;}"),
                column(
                  width = 3,
                  checkboxInput(
                    "institute_general", 
                    label = h5("Institute", style = "color:white; font-size: 17px; margin-top: -1px;"),
                    value = TRUE
                  )
                ),
                column(
                  width = 8,
                  textInput(
                    "institute_general_select",
                    ""
                  ) 
                )
              ),
              fluidRow(
                tags$style("#comm_general_select {margin-top: -15px; border-radius: 5px; position: relative; right: -22px}"),
                tags$style("#comm_general {margin-top: 0px;}"),
                column(
                  width = 3,
                  checkboxInput(
                    "comm_general", 
                    label = h5("Comment", style = "color:white; font-size: 17px; margin-top: -1px;")
                  )
                ),
                column(
                  width = 8,
                  textAreaInput(
                    inputId = "comm_general_select",
                    label = "",
                    width = "100%",
                    height = "60px",
                    cols = NULL,
                    rows = NULL,
                    placeholder = NULL,
                    resize = "vertical"
                  ) 
                )
              )
            ),
            br(),
            box(
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              HTML(
                paste(
                  tags$span(style='color: white; font-size: 24px;', 'Analysis')
                )
              ),
              br(),
              fluidRow(
                tags$style(".choosechannel .btn {height: 31px;}"),
                tags$style("#cgmlst_analysis {margin-top: 19px}"),
                column(
                  width = 3,
                  checkboxInput(
                    "cgmlst_analysis",
                    label = h5("Scheme", style = "color:white; font-size: 17px; margin-top: 18px"),
                    value = TRUE
                  )
                ),
                column(
                  width = 8,
                  align = "right",
                  div(
                    class = "choosechannel",
                    uiOutput("cgmlst_select_analysis")
                  )
                )
              ),
              fluidRow(
                tags$style("#tree_analysis {margin-top: 0px}"),
                column(
                  width = 3,
                  checkboxInput(
                    "tree_analysis",
                    label = h5("Tree", style = "color:white; font-size: 17px; margin-top: -1px")
                  )
                ),
                column(
                  width = 8,
                  HTML(
                    paste(
                      tags$span(style='color: white; font-size: 15px; position: relative; top: 9px; right: -23px', 'Tree Algorithm & Layout Info')
                    )
                  )
                )
              ),
              fluidRow(
                tags$style("#kma_analysis {margin-top: 0px}"),
                column(
                  width = 3,
                  checkboxInput(
                    "kma_analysis",
                    label = h5("Typing", style = "color:white; font-size: 17px; margin-top: -1px")
                  )
                ),
                column(
                  width = 8,
                  HTML(
                    paste(
                      tags$span(style='color: white; font-size: 15px; position: relative; top: 9px; right: -23px', 'Typing Algorithm Parameters')
                    )
                  )
                )
              ),
              br()
            ),
            br(),
          ),
          column(1),
          column(
            width = 2,
            br(),
            tags$style("#slot1_legend {margin-top: 17px;}"),
            tags$style("#slot2_legend {margin-top: 17px;}"),
            tags$style(".choosechannel .btn {height: 31px;}"),
            uiOutput("slot1_box"),
            uiOutput("slot2_box"),
          ),
          column(
            width = 2,
            br(),
            tags$style("#slot3_legend {margin-top: 17px;}"),
            tags$style("#slot4_legend {margin-top: 17px;}"),
            tags$style(".choosechannel .btn {height: 31px;}"),
            uiOutput("slot3_box"),
            uiOutput("slot4_box")
          ),
          column(1),
          column(
            width = 2,
            br(),
            uiOutput("save_rep"),
          )
        )
      )
    ) # End tabItems
  ) # End dashboardPage
) # end UI



# Server ----

server <- function(input, output, session) {
  
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  shinyjs::removeClass(selector = "body", class = "sidebar-toggle")
  
  renderstart <- reactiveValues(sidebar = TRUE, header = TRUE)
  
  set.seed(1)
  
  ## Render Menu ----
  
  observe({
    if (renderstart$sidebar == FALSE) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      shinyjs::addClass(selector = "body", class = "sidebar-toggle")
    }
  })
  
  output$start_message <- renderUI({
    column(
      width = 12, 
      align = "center",
      br(), br(),
      div(
        class = "image",
        imageOutput("imageOutput")
      ),
      p(
        HTML(
          paste(
            tags$span(style='color: white; font-size: 24px;', 'Welcome to PhyloTree!')
          )
        )
      ),
      br(), br(),
      p(
        HTML(
          paste(
            tags$span(style='color: white; font-size: 16px;', 'Proceed by loading a locally available typing database.')
          )
        )
      ),
      br(),
      fluidRow(
        column(
          width = 12,
          column(width = 5),
          uiOutput("scheme_db"),
          column(width = 5),
        ),
      ),
      br(), br(),
      uiOutput("load"),
      br(), br(), br(), br(), br(), br(), br()
    )
  })
  
  observeEvent(input$reload_db, {
    showModal(
      modalDialog(
        selectInput(
          "scheme_db",
          label = "",
          choices = database$available),
        title = "Select a local database to load.",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("load", "Load", class = "btn btn-default")
        )
      )
    )
  })
  
  output$imageOutput <- renderImage({
    # Path to your PNG image with a transparent background
    image_path <- paste0(getwd(), "/www/PhyloTree.png")
    
    # Use HTML to display the image with the <img> tag
    list(src = image_path)
  }, deleteFile = FALSE)
  
  observeEvent(input$load, {
    
    removeModal()
    
    renderstart$sidebar <- FALSE
    renderstart$header <- FALSE
    
    output$menu_sep1 <- renderUI(hr())
    output$menu_sep2 <- renderUI(hr())
    
    output$loaded_scheme <- renderUI({
      fluidRow(
        column(width = 2),
        column(
          width = 6,
          div(
            class = "scheme_start",
            p(
              HTML(
                paste(
                  tags$span(style='color: white; font-size: 15px;', strong("Selected scheme:"))
                )
              )
            ),
            p(
              HTML(
                paste(
                  tags$span(style='color: white; font-size: 15px; font-style: italic', DF1$scheme)
                )
              )
            )
          )
        ),
        column(
          width = 2,
          div(
            class = "reload-bttn",
            actionButton(
              "reload_db",
              label = "",
              icon = icon("rotate")
            )
          )
        )
      )
    })
    
    output$menu <- renderMenu(
      sidebarMenu(
        menuItem(
          text = "Database Browser",
          tabName = "database",
          icon = icon("database")
        ),
        menuItem(
          text = "Add Scheme",
          tabName = "init",
          icon = icon("plus")
        ),
        menuItem(
          text = "Allelic Typing",
          tabName = "typing",
          icon = icon("dna")
        ),
        menuItem(
          text = "Visualization",
          tabName = "visualization",
          icon = icon("chart-line")
        ),
        menuItem(
          text = "Download Report",
          tabName = "report",
          icon = icon("download")
        )
      )
    )
    
    output$start_message <- NULL
  })
  
  ## Database ----
  DF1 <- reactiveValues()
  database <- reactiveValues()
  
  observe({
    database$available <-
      gsub("_", " ", basename(dir_ls(paste0(
        getwd(), "/Database"
      ))))
    database$exist <-
      (length(dir_ls(paste0(
        getwd(), "/Database/"
      ))) == 0)
    
  })
  
  ### Conditional Rendering of Database UI Elements ----
  
  observe({
    if (!database$exist) {
      
      output$scheme_db <- renderUI({
        if (length(database$available) > 5) {
          column(
            width = 2,
            align = "center",
            div(
              class = "select_start",
              selectInput(
                "scheme_db",
                label = "",
                choices = database$available
              )
            )
          )
        } else {
          column(
            width = 2,
            align = "left",
            div(
              class = "select_start_radio",
              prettyRadioButtons(
                "scheme_db",
                label = "",
                choices = database$available
              )
            )
          )
        }
      })
      
      # Dont Show 'No Database' message
      output$no_db <- NULL
      
      # Show Load Database Button
      output$load <- renderUI(actionButton("load",
                                           "Load"))
      
      if (!class(DF1$schemeinfo) == "NULL") {
        
        output$scheme_info <- renderTable({
          DF1$schemeinfo
        })
        
        output$scheme_header <-
          renderUI(h3(p("cgMLST Scheme"), style = "color:white"))
        
      } else {
        
        output$scheme_info <- NULL
        output$scheme_header <- NULL
        
      }
      
      if (!class(DF1$loci_info) == "NULL") {
        output$db_loci <- renderDataTable(DF1$loci_info,
                                          options = list(pageLength = 10,
                                                         columnDefs = list(
                                                           list(searchable = FALSE,
                                                                targets = "_all")
                                                         )))
        
        output$loci_header <-
          renderUI(h3(p("Loci"), style = "color:white"))
      } else {
        output$db_loci <- NULL
        output$loci_header <- NULL
      }
      
    } else {
      output$no_db <- renderUI(HTML(
        paste(
          "<span style='color: white;'>",
          "No local Schemes or Entries available.",
          "Download a cgMLST Scheme in the Section <strong>Add Scheme</strong>.",
          sep = '<br/>'
        )
      ))
      
      # Dont show Database Selector if no Database available
      output$scheme_db <- NULL
      
      # Dont show Load Database Button if no Database available
      output$load <- NULL
    }
    
  })
  
  ### Load database Event ----
  observeEvent(input$load, {
    if (any(grepl("Typing.rds", dir_ls(paste0(
      getwd(), "/Database/", gsub(" ", "_", input$scheme_db)
    ))))) {
      Data <-
        readRDS(paste0(
          getwd(),
          "/Database/",
          gsub(" ", "_", input$scheme_db),
          "/Typing.rds"
        ))
      
      output$edit_button <- renderUI({
        actionButton("edit_button",
                     "",
                     icon = icon("bookmark"))
      })
      
      typing <- Data[["Typing"]]
      
      DF1$data <- typing
      
      DF1$scheme <- input$scheme_db
      
      output$compare_select <- renderUI({
        pickerInput(
          inputId = "compare_select",
          label = h4("Display Locus", style = "color:white; margin-bottom: 10px;"),
          choices = names(select(typing, -(1:12))),
          selected = names(select(typing, -(1:12)))[1:20],
          options = list(
            `live-search` = TRUE,
            `actions-box` = TRUE,
            size = 10,
            style = "background-color: white; border-radius: 5px;"
          ),
          multiple = TRUE
        )
      })
      
      # Render Separating hr() line
      output$db_line <- renderUI(hr())
      
      
      #### Render Entry Data Table ----
      
      output$db_entries_table <- renderUI({
        addSpinner(
          rHandsontableOutput("db_entries"),
          spin = "dots",
          color = "#ffffff"
        )
      })
      
      # Determine Entry Table Height 
      height_table <- reactive({
        if (nrow(DF1$data > 25)) {
          800
        } else {
          nrow(DF1$data) * 50 + 75
        }
      })
      
      if (!class(DF1$data) == "NULL") {
        
        observe({
          
          if (nrow(DF1$data) == 1) {
            if (length(input$compare_select) > 0) {
              output$db_entries <- renderRHandsontable({
                rhandsontable(
                  select(DF1$data, 1:12, input$compare_select),
                  rowHeaders = NULL,
                  height = height_table()
                ) %>%
                  hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
                  hot_col(2,
                          halign = "htCenter",
                          valign = "htTop",
                          width = "auto") %>%
                  hot_context_menu(allowRowEdit = FALSE,
                                   allowColEdit = FALSE,
                                   allowReadOnly = FALSE) %>%
                  hot_rows(rowHeights = 30,
                           fixedRowsTop = 0)
              })
            } else {
              output$db_entries <- renderRHandsontable({
                rhandsontable(
                  select(DF1$data, 1:12),
                  rowHeaders = NULL,
                  height = height_table()
                ) %>%
                  hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
                  hot_col(2,
                          halign = "htCenter",
                          valign = "htTop",
                          width = "auto") %>%
                  hot_context_menu(allowRowEdit = FALSE,
                                   allowColEdit = FALSE,
                                   allowReadOnly = FALSE) %>%
                  hot_rows(rowHeights = 30,
                           fixedRowsTop = 0)
              })
            }
          } else {
            if (length(input$compare_select) > 0) {
              output$db_entries <- renderRHandsontable({
                rhandsontable(
                  select(DF1$data, 1:12, input$compare_select),
                  col_highlight = diff_allele()-1,
                  rowHeaders = NULL,
                  height = height_table()
                ) %>%
                  hot_col(1:(12+length(input$compare_select)), valign = "htMiddle") %>%
                  hot_context_menu(allowRowEdit = FALSE,
                                   allowColEdit = FALSE,
                                   allowReadOnly = FALSE) %>%
                  hot_col(2,
                          halign = "htCenter",
                          valign = "htTop",
                          width = "auto") %>%
                  hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
                  hot_rows(fixedRowsTop = 0) %>%
                  hot_col(diff_allele(),
                          renderer = "
                function(instance, td, row, col, prop, value, cellProperties) {
                  Handsontable.renderers.NumericRenderer.apply(this, arguments);

                  if (instance.params) {
                        hcols = instance.params.col_highlight;
                        hcols = hcols instanceof Array ? hcols : [hcols];
                      }

                  if (instance.params && hcols.includes(col)) {
                    td.style.background = '#FF8F8F';
                  }
              }"
                  ) 
              })
            } else {
              output$db_entries <- renderRHandsontable({
                rhandsontable(
                  select(DF1$data, 1:12),
                  rowHeaders = NULL,
                  height = height_table()
                ) %>%
                  hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
                  hot_col(1:12, valign = "htMiddle") %>%
                  hot_col(2,
                          halign = "htCenter",
                          valign = "htTop",
                          width = "auto") %>%
                  hot_context_menu(allowRowEdit = FALSE,
                                   allowColEdit = FALSE,
                                   allowReadOnly = FALSE) %>%
                  hot_rows(rowHeights = 30,
                           fixedRowsTop = 0)
              })
            }
          }
        })
        
        output$db_no_entries <- NULL
        
      } else {
        output$db_entries_table <- NULL
      }
      
      ### Render Allele Differences as Highlights ----
      
      
      diff_allele <- reactive({
        if (!class(DF1$data) == "NULL") {
          # Function to find columns with varying values
          var_alleles <- function(dataframe) {
            varying_columns <- c()
            
            for (col in 1:ncol(dataframe)) {
              if(class(dataframe[, col]) == "integer") {
                unique_values <- unique(dataframe[, col])
                
                if (length(unique_values) > 1) {
                  varying_columns <- c(varying_columns, col)
                }
              }
            }
            
            return(varying_columns)
          }
          var_alleles(select(DF1$data, input$compare_select)) + 12
        }
      })
      
      
      ### Edit Entry Editing Elements ----
      
      output$delete_box <- renderUI({
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          column(
            width = 12,
            align = "center",
            uiOutput("del_text")
          ),
          fluidRow(
            br(),
            column(width = 1),
            column(
              width = 2,
              align = "right",
              br(),
              h5("Index", style = "color:white; margin-bottom: 0px;")
            ),
            column(width = 5,
                   align = "center",
                   uiOutput("delete_select")),
            column(width = 2,
                   align = "center",
                   br(),
                   uiOutput("del_bttn"))
          ),
          br()
        )
      })
      
      output$compare_allele_box <- renderUI({
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          column(
            width = 12,
            align = "center",
            uiOutput("compare_select"),
            br()
          ),
          br()
        )
      })
      
      output$del_text <- renderUI({
        h4(p("Delete Entries"), style = "color:white")
      })
      
      output$delete_select <- renderUI({
        pickerInput("select_delete",
                    label = "",
                    choices = DF1$data[, "Index"],
                    options = list(
                      `live-search` = TRUE,
                      size = 10,
                      style = "background-color: white; border-radius: 5px;"
                    ),
                    multiple = TRUE)
      })
      
      output$del_bttn <- renderUI({
        actionBttn(
          "del_button",
          label = "",
          color = "danger",
          size = "sm",
          style = "material-circle",
          icon = icon("xmark")
        )
      })
      
    } else if (!any(grepl("Typing.rds", dir_ls(paste0(
      getwd(), "/Database/", gsub(" ", "_", DF1$scheme)
    ))))) {
      output$db_no_entries <- renderUI(HTML(
        paste(
          "<span style='color: white;'>",
          "No Entries for this scheme available.",
          "Type a genome in the section <strong>Allelic Typing</strong> and add the result to the local database.",
          sep = '<br/>'
        )
      ))
      
      output$db_line <- renderUI(hr())
      output$db_entries <- NULL
      output$edit_index <- NULL
      output$edit_scheme_d <- NULL
      output$edit_entries <- NULL
      output$edit_field <- NULL
      output$compare_select <- NULL
      output$delete_select <- NULL
      output$del_bttn <- NULL
      output$compare_allele_box <- NULL
      output$delete_box <- NULL
      
    }
    
    # Produce Scheme Info Table
    schemeinfo <-
      read_html(paste0(
        getwd(),
        "/Database/",
        gsub(" ", "_", DF1$scheme),
        "/scheme_info.html"
      )) %>%
      html_table(header = FALSE) %>%
      as.data.frame(stringsAsFactors = FALSE)
    names(schemeinfo) <- NULL
    DF1$schemeinfo <- schemeinfo
    
    # Produce Loci Info Table
    DF1$loci_info <- read.csv(
      paste0(
        getwd(),
        "/Database/",
        gsub(" ", "_", DF1$scheme),
        "/targets.csv"
      ),
      header = TRUE,
      sep = "\t",
      row.names = NULL,
      colClasses = c(
        "NULL",
        "character",
        "character",
        "integer",
        "integer",
        "character",
        "integer",
        "NULL"
      )
    )
    
  })
  
  ### Other Database Events ----
  
  # Save Edits Button
  
  observeEvent(input$edit_button, {
    showModal(
      modalDialog(
        paste0(
          "Overwriting previous metadata of local ",
          DF1$scheme,
          " database.",
          " Continue?"
        ),
        title = "Save Database",
        fade = TRUE,
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("conf_db_save", "Save", class = "btn btn-default")
        )
      )
    )
  })
  
  observeEvent(input$Cancel, {
    removeModal()
  })
  
  observeEvent(input$conf_db_save, {
    Data <- readRDS(paste0(
      getwd(),
      "/Database/",
      gsub(" ", "_", DF1$scheme),
      "/Typing.rds"
    ))
    
    attach_meta <- hot_to_r(input$db_entries)
    attach_meta <- select(attach_meta, 1:12)
    
    if(!is.null(DF1$delete)) {
      Data[["Typing"]] <- cbind(attach_meta, DF1$delete)
    } else {
      Data[["Typing"]][, 1:12] <- attach_meta
    }
    
    saveRDS(Data, paste0(
      getwd(),
      "/Database/",
      gsub(" ", "_", DF1$scheme),
      "/Typing.rds"
    ))
    
    removeModal()
    
    show_toast(
      title = "Database successfully saved",
      type = "success",
      position = "top-end",
      timer = 4000
    )
  })
  
  observeEvent(input$del_button, {
    showModal(
      modalDialog(
        paste0(
          "Confirmation will lead to irreversible removal of selected entries. Continue?"
        ),
        title = "Deleting Entry",
        fade = TRUE,
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            "conf_delete", 
            "Delete", 
            class = "btn btn-danger")
        )
      )
    )
  })
  
  observeEvent(input$conf_delete, {
    delete <- select(DF1$data, -(1:12))
    DF1$delete <-  delete[-as.integer(input$select_delete),]
    DF1$data <- DF1$data[-as.integer(input$select_delete),]
    rownames(DF1$data) <- 1:nrow(DF1$data)
    DF1$data <- mutate(DF1$data, Index = as.character(rownames(DF1$data)))
    removeModal()
    if(length(input$select_delete) > 1) {
      show_toast(
        title = "Entries successfully deleted",
        type = "success",
        position = "top-end",
        timer = 4000
      )
    } else {
      show_toast(
        title = "Entry successfully deleted",
        type = "success",
        position = "top-end",
        timer = 4000
      )
    }
    
  })
  
  
  
  
  ## Download cgMLST ----
  
  myReactives <- reactiveValues()
  
  observe(
    if (input$select_cgmlst == "Acinetobacter baumanii") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/3956907/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/3956907/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/3956907/locus/?content-type=csv"
      folder_name <<- "Acinetobacter_baumanii"
    } else if (input$select_cgmlst == "Bacillus anthracis") {
      link_cgmlst <<-
        "https://www.cgmlst.org/ncs/schema/19008694/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/19008694/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/19008694/locus/?content-type=csv"
      folder_name <<- "Bacillus_anthracis"
    } else if (input$select_cgmlst == "Bordetella pertussis") {
      link_cgmlst <<-
        "https://www.cgmlst.org/ncs/schema/29412358/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/29412358/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/29412358/locus/?content-type=csv"
      folder_name <<- "Bordetella_pertussis"
    } else if (input$select_cgmlst == "Brucella melitensis") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/6398355/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/6398355/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema//6398355/locus/?content-type=csv"
      folder_name <<- "Brucella_melitensis"
    } else if (input$select_cgmlst == "Brucella spp.") {
      link_cgmlst <<-
        "https://www.cgmlst.org/ncs/schema/24062474/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/24062474/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/24062474/locus/?content-type=csv"
      folder_name <<- "Brucella_spp"
    } else if (input$select_cgmlst == "Burkholderia mallei (FLI)") {
      link_cgmlst <<-
        "https://www.cgmlst.org/ncs/schema/23721348/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/23721348/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/23721348/locus/?content-type=csv"
      folder_name <<- "Burkholderia_mallei_FLI"
    } else if (input$select_cgmlst == "Burkholderia mallei (RKI)") {
      link_cgmlst <<-
        "https://www.cgmlst.org/ncs/schema/23643739/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/23643739/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/23643739/locus/?content-type=csv"
      folder_name <<- "Burkholderia_mallei_RKI"
    } else if (input$select_cgmlst == "Burkholderia pseudomallei") {
      link_cgmlst <<-
        "https://www.cgmlst.org/ncs/schema/18876117/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/18876117/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/18876117/locus/?content-type=csv"
      folder_name <<- "Burkholderia_pseudomallei"
    } else if (input$select_cgmlst == "Campylobacter jejuni/coli") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/145039/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/145039/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/145039/locus/?content-type=csv"
      folder_name <<- "Campylobacter_jejuni_coli"
    } else if (input$select_cgmlst == "Clostridioides difficile") {
      link_cgmlst <<-
        "https://www.cgmlst.org/ncs/schema/12556067/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/12556067/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/12556067/locus/?content-type=csv"
      folder_name <<- "Clostridioides_difficile"
    } else if (input$select_cgmlst == "Clostridium perfringens") {
      link_cgmlst <<-
        "https://www.cgmlst.org/ncs/schema/15017225/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/15017225/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/15017225956907/locus/?content-type=csv"
      folder_name <<- "Clostridium_perfringens"
    } else if (input$select_cgmlst == "Corynebacterium diphtheriae") {
      link_cgmlst <<-
        "https://www.cgmlst.org/ncs/schema/30589266/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/30589266/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/30589266/locus/?content-type=csv"
      folder_name <<- "Corynebacterium_diphtheriae"
    } else if (input$select_cgmlst == "Cronobacter sakazakii/malonaticus") {
      link_cgmlst <<-
        "https://www.cgmlst.org/ncs/schema/29420227/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/29420227/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/29420227/locus/?content-type=csv"
      folder_name <<- "Cronobacter_sakazakii_malonaticus"
    } else if (input$select_cgmlst == "Enterococcus faecalis") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/3887469/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/3887469/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/3887469/locus/?content-type=csv"
      folder_name <<- "Enterococcus_faecalis"
    } else if (input$select_cgmlst == "Enterococcus faecium") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/991893/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/991893/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/991893/locus/?content-type=csv"
      folder_name <<- "Enterococcus_faecium"
    } else if (input$select_cgmlst == "Escherichia coli") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/5064703/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/5064703/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/5064703/locus/?content-type=csv"
      folder_name <<- "Escherichia_coli"
    } else if (input$select_cgmlst == "Francisella tularensis") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/260204/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/260204/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/260204/locus/?content-type=csv"
      folder_name <<- "Francisella_tularensis"
    } else if (input$select_cgmlst == "Klebsiella pneumoniae sensu lato") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/2187931/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/2187931/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/2187931/locus/?content-type=csv"
      folder_name <<- "Klebsiella_pneumoniae_sensu_lato"
    } else if (input$select_cgmlst == "Legionella pneumophila") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/1025099/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/1025099/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/1025099/locus/?content-type=csv"
      folder_name <<- "Legionella_pneumophila"
    } else if (input$select_cgmlst == "Listeria monocytogenes") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/690488/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/690488/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/690488/locus/?content-type=csv"
      folder_name <<- "Listeria_monocytogenes"
    } else if (input$select_cgmlst == "Mycobacterium tuberculosis complex") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/741110/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/741110/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/741110/locus/?content-type=csv"
      folder_name <<- "Mycobacterium_tuberculosis_complex"
    } else if (input$select_cgmlst == "Mycobacteroides abscessus") {
      link_cgmlst <<-
        "https://www.cgmlst.org/ncs/schema/22602285/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/22602285/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/22602285/locus/?content-type=csv"
      folder_name <<- "Mycobacteroides_abscessus"
    } else if (input$select_cgmlst == "Mycoplasma gallisepticum") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/6402012/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/6402012/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/6402012/locus/?content-type=csv"
      folder_name <<- "Mycoplasma_gallisepticum"
    } else if (input$select_cgmlst == "Paenibacillus larvae") {
      link_cgmlst <<-
        "https://www.cgmlst.org/ncs/schema/17414003/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/17414003/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/17414003/locus/?content-type=csv"
      folder_name <<- "Paenibacillus_larvae"
    } else if (input$select_cgmlst == "Pseudomonas aeruginosa") {
      link_cgmlst <<-
        "https://www.cgmlst.org/ncs/schema/16115339/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/16115339/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/16115339/locus/?content-type=csv"
      folder_name <<- "Pseudomonas_aeruginosa"
    } else if (input$select_cgmlst == "Salmonella enterica") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/4792159/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/4792159/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/4792159/locus/?content-type=csv"
      folder_name <<- "Salmonella_enterica"
    } else if (input$select_cgmlst == "Serratia marcescens") {
      link_cgmlst <<-
        "https://www.cgmlst.org/ncs/schema/24616475/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/24616475/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/24616475/locus/?content-type=csv"
      folder_name <<- "Serratia_marcescens"
    } else if (input$select_cgmlst == "Staphylococcus aureus") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/141106/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/141106/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/141106/locus/?content-type=csv"
      folder_name <<- "Staphylococcus_aureus"
    } else if (input$select_cgmlst == "Staphylococcus capitis") {
      link_cgmlst <<-
        "https://www.cgmlst.org/ncs/schema/26824796/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/26824796/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/26824796/locus/?content-type=csv"
      folder_name <<- "Staphylococcus_capitis"
    } else if (input$select_cgmlst == "Streptococcus pyogenes") {
      link_cgmlst <<-
        "https://www.cgmlst.org/ncs/schema/30585223/alleles/"
      myReactives$link_scheme <<-
        "https://www.cgmlst.org/ncs/schema/30585223/"
      link_targets <<-
        "https://www.cgmlst.org/ncs/schema/30585223/locus/?content-type=csv"
      folder_name <<- "Streptococcus_pyogenes"
    }
    
  )
  
  observeEvent(input$download_cgMLST, {
    myReactives$target_table <- NULL
    
    # Download Loci Fasta Files
    download(link_cgmlst, dest = "dataset.zip", mode = "wb")
    
    unzip(
      zipfile = "dataset.zip",
      exdir = paste0(
        getwd(),
        "/Database/",
        folder_name,
        paste0("/", folder_name, "_alleles")
      )
    )
    
    unlink("dataset.zip")
    
    # Download Scheme Info
    download(
      myReactives$link_scheme,
      dest = paste0(getwd(), "/Database/", folder_name, "/scheme_info.html"),
      mode = "wb"
    )
    
    # Download Loci Info
    download(
      link_targets,
      dest = paste0(getwd(), "/Database/", folder_name, "/targets.csv"),
      mode = "wb"
    )
    
    # Send downloaded scheme to datbase browser overview
    database$available <-
      gsub("_", " ", basename(dir_ls(paste0(
        getwd(), "/Database"
      ))))
    
    myReactives$target_table <-
      read.csv(
        paste0(getwd(), "/Database/", folder_name, "/targets.csv"),
        header = TRUE,
        sep = "\t",
        row.names = NULL,
        colClasses = c(
          "NULL",
          "character",
          "character",
          "integer",
          "integer",
          "character",
          "integer",
          "NULL"
        )
      )
    
    database$exist <-
      (length(dir_ls(paste0(
        getwd(), "/Database/"
      ))) == 0)
    
    show_toast(
      title = "Download successful",
      type = "success",
      position = "top-end",
      timer = 5000
    )
    
  })
  
  # Download Target Info (CSV Table)
  
  
  output$cgmlst_scheme <- renderTable({
    scheme_overview <- read_html(myReactives$link_scheme) %>%
      html_table(header = FALSE) %>%
      as.data.frame(stringsAsFactors = FALSE)
    names(scheme_overview) <- NULL
    scheme_overview
  })
  
  ### Display Target Table ----
  
  output$cgmlst_targets <- renderDataTable({
    targets_overview <- myReactives$target_table
  },
  options = list(pageLength = 10,
                 columnDefs = list(
                   list(searchable = FALSE, targets = "_all")
                 )))
  
  
  ## Visualization ----
  
  ### Render Slot Allocation Elements ----
  
  output$slot1_status <- renderUI({
    if(is.null(plot_loc$slot1_getname)) {
      h5("Empty", style = "color:white; margin-top: 6px")
    } else {
      h5(plot_loc$slot1_getname, style = "color:white; margin-top: 6px")
    }
  })
  
  output$slot2_status <- renderUI({
    if(is.null(plot_loc$slot2_getname)) {
      h5("Empty", style = "color:white; margin-top: -2px")
    } else {
      h5(plot_loc$slot2_getname, style = "color:white; margin-top: -2px")
    }
  })
  
  output$slot3_status <- renderUI({
    if(is.null(plot_loc$slot3_getname)) {
      h5("Empty", style = "color:white; margin-top: -2px")
    } else {
      h5(plot_loc$slot3_getname, style = "color:white; margin-top: -2px")
    }
  })
  
  output$slot4_status <- renderUI({
    if(is.null(plot_loc$slot4_getname)) {
      h5("Empty", style = "color:white; margin-top: -2px")
    } else {
      h5(plot_loc$slot4_getname, style = "color:white; margin-top: -2px")
    }
  })
  
  ### Render Plot Reactive ----
  plot_loc <- reactiveValues(cluster = NULL, metadata = list())
  
  plot_input <- reactive({
    ggplot(plot_loc$gg, aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    )) +
      geom_edges(
        color = edge_color(),
        linewidth = edge_size(),
        alpha = edge_alpha(),
        curvature = edge_curvature()
      ) +
      nodes() +
      label_edge() +
      label_node() +
      theme_blank() +
      theme(
        panel.background = element_rect(fill = bg_color()),
        plot.background = element_rect(fill = bg_color())
      )
  })
  
  observeEvent(input$create_tree, {
    
    set.seed(1)
    
    # Load local database
    Database <<-
      readRDS(paste0(
        getwd(),
        "/Database/",
        gsub(" ", "_", DF1$scheme),
        "/Typing.rds"
      ))
    
    
    
    # get allele profile of included entries
    allelic_profile <- dplyr::select(Database$Typing, -(1:12))
    allelic_profile <-
      allelic_profile[which(Database[["Typing"]]$Include == TRUE),]
    
    # get metadata without include boolean variable
    meta <- dplyr::select(Database$Typing, 1, 3:12)
    meta <- meta[which(Database[["Typing"]]$Include == TRUE),]
    
    # Calculate distance matrix
    dist_matrix <- dist(allelic_profile)
    
    if (input$tree_algo == "Neighbour-Joining") {
      plot_loc$meta_nj <- meta
      
      colnames(plot_loc$meta_nj) <-
        c(
          "index",
          "assembly_id",
          "assembly_name",
          "scheme",
          "isolation_date",
          "host",
          "country",
          "city",
          "typing_date",
          "successes",
          "errors"
        )
      
      plot_loc$meta_nj$taxa <- rownames(plot_loc$meta_nj)
      
      
      # Create phylogenetic tree
      plot_loc$nj <- ape::nj(dist_matrix)
      
      plot_loc$nj_plot <-
        ggtree(plot_loc$nj, layout = layout_nj()) %<+% plot_loc$meta_nj +
        tiplab_nj() +
        inward()
      
      # Visualize the tree with metadata annotations
      output$tree_local <- renderPlot({
        print(plot_input())
      })
      
    } else {
      
      mst <- ape::mst(dist_matrix)
      
      plot_loc$gr_adj <- graph.adjacency(mst, mode = mode_algo())
      
      plot_loc$netw <- ggnetwork(plot_loc$gr_adj, arrow.gap = 0, layout = ggnet_layout())
      
      ## add metadata
      plot_loc$gg <- plot_loc$netw %>% mutate(
        index = meta[plot_loc$netw$name, "Index"],
        assembly_id = meta[plot_loc$netw$name, "Assembly ID"],
        assembly_name = meta[plot_loc$netw$name, "Assembly Name"],
        scheme = meta[plot_loc$netw$name, "Scheme"],
        isolation_date = meta[plot_loc$netw$name, "Isolation Date"],
        host = meta[plot_loc$netw$name, "Host"],
        country = meta[plot_loc$netw$name, "Country"],
        city = meta[plot_loc$netw$name, "City"],
        typing_date = meta[plot_loc$netw$name, "Typing Date"],
        successes = meta[plot_loc$netw$name, "Successes"],
        errors = meta[plot_loc$netw$name, "Errors"]
      )
      
      output$tree_local <- renderPlot({
        print(plot_input())
      })
      
      output$cluster_start <- renderUI(actionButton("cluster_start",
                                                    "Add Clusters"))
      
    }
    
  })
  
  # NJ Tree Layout
  layout_nj <- reactive({
    input$nj_layout
  })
  
  # NJ inward circular
  inward <- reactive({
    if (input$circ_inward == TRUE) {
      layout_inward_circular()
    } else {
      NULL
    }
  })
  
  # NJ Tip Labs
  tiplab_nj <- reactive({
    if (input$nj_tiplab == "index") {
      geom_tiplab(aes(label = taxa), offset = tiplab_offset())
    } else if (input$nj_tiplab == "assembly_id") {
      geom_tiplab(aes(label = assembly_id), offset = tiplab_offset())
    } else if (input$nj_tiplab == "assembly_name") {
      geom_tiplab(aes(label = assembly_name), offset = tiplab_offset())
    } else if (input$nj_tiplab == "host") {
      geom_tiplab(aes(label = host), offset = tiplab_offset())
    } else if (input$nj_tiplab == "country") {
      geom_tiplab(aes(label = country), offset = tiplab_offset())
    } else if (input$nj_tiplab == "city") {
      geom_tiplab(aes(label = city), offset = tiplab_offset())
    } else if (input$nj_tiplab == "isolation_date") {
      geom_tiplab(aes(label = isolation_date), offset = tiplab_offset())
    }
  })
  
  # NJ Tip Lab Offset
  
  tiplab_offset <- reactive({
    input$nj_tip_offset
  })
  
  # Set Minimum-Spanning Tree Appearance
  ggnet_layout <- reactive({
    if (input$ggnetwork_layout == "Davidson-Harel") {
      layout_with_dh(plot_loc$gr_adj)
    } else if (input$ggnetwork_layout == "DrL") {
      layout_with_drl(plot_loc$gr_adj)
    } else if (input$ggnetwork_layout == "Fruchterman-Reingold") {
      layout_with_fr(plot_loc$gr_adj)
    } else if (input$ggnetwork_layout == "GEM") {
      layout_with_gem(plot_loc$gr_adj)
    } else if (input$ggnetwork_layout == "Graphopt") {
      layout_with_graphopt(plot_loc$gr_adj)
    } else if (input$ggnetwork_layout == "Kamada-Kawai") {
      layout_with_kk(plot_loc$gr_adj)
    } else if (input$ggnetwork_layout == "Large Graph Layout") {
      layout_with_lgl(plot_loc$gr_adj)
    } else if (input$ggnetwork_layout == "Multidimensional Scaling") {
      layout_with_mds(plot_loc$gr_adj)
    } else if (input$ggnetwork_layout == "Sugiyama") {
      layout_with_sugiyama(plot_loc$gr_adj)
    }
  })
  
  # Set Interpretation Mode
  mode_algo <- reactive({
    input$algo_mode
  })
  
  # Set edge color
  edge_color <- reactive({
    input$color_edge
  })
  
  # Set edge color
  edge_size <- reactive({
    input$size_edge
  })
  
  # Set edge transparency
  edge_alpha <- reactive({
    input$alpha_edge
  })
  
  # Set edge curvature
  edge_curvature <- reactive({
    input$curvature_edge
  })
  
  # Set node color
  node_color <- reactive({
    input$color_node
  })
  
  # Set node transparency
  node_alpha <- reactive({
    input$alpha_node
  })
  
  # Set node size
  node_size <- reactive({
    input$size_node
  })
  
  # Set Background
  bg_color <- reactive({
    input$color_bg
  })
  
  # Set Node Label
  label_node <- reactive({
    if (input$include_node) {
      if (input$label_rect == FALSE) {
        geom_nodetext_repel(
          aes_string(label = input$node_label),
          color = input$label_color,
          size = input$label_size,
          alpha = input$label_alpha,
          box.padding = input$box_padding,
          point.padding = input$point_padding,
          nudge_x = input$x_nudge,
          nudge_y = input$y_nudge,
          max.overlaps = 18
        )
      } else {
        geom_nodelabel_repel(
          aes_string(label = input$node_label),
          color = input$label_color,
          fill = input$label_fillcolor,
          size = input$label_size,
          alpha = input$label_alpha,
          box.padding = input$box_padding,
          point.padding = input$point_padding,
          nudge_x = input$x_nudge,
          nudge_y = input$y_nudge,
          label.padding = input$panel_padding,
          label.r = input$panel_radius,
          label.size = input$panel_bordersize,
          max.overlaps = 18
        )
      }
    }
    
    
  })
  
  # Set Edge Label
  
  label_edge <- reactive({
    if (input$include_edge) {
      if (input$edge_rect == TRUE) {
        geom_edgelabel_repel(
          aes_string(label = input$edge_label),
          color = input$edgelabel_color,
          fill = input$edgelabel_fillcolor,
          size = input$edgelabel_size,
          alpha = input$edgelabel_alpha,
          box.padding = input$edge_box_padding,
          point.padding = input$edge_point_padding,
          nudge_x = input$edge_x_nudge,
          nudge_y = input$edge_y_nudge,
          label.padding = input$edge_panel_padding,
          label.r = input$edge_panel_radius,
          label.size = input$edge_panel_bordersize,
          max.overlaps = 18
        )
      } else {
        geom_edgetext_repel(
          aes_string(label = input$edge_label),
          color = input$edgelabel_color,
          size = input$edgelabel_size,
          alpha = input$edgelabel_alpha,
          box.padding = input$edge_box_padding,
          point.padding = input$edge_point_padding,
          nudge_x = input$edge_x_nudge,
          nudge_y = input$edge_y_nudge,
          max.overlaps = 18
        )
      }
    } else {
      NULL
    }
  })
  
  # Set Node Appearance
  
  nodes <- reactive({
    if (is.null(plot_loc$gg$cluster) &
        length(plot_loc$metadata[["which"]]) < 1) {
      geom_nodes(color = node_color(),
                 size = node_size(),
                 alpha = node_alpha())
    } else if (is.null(plot_loc$gg$cluster) &
               length(plot_loc$metadata[["which"]]) == 1 &
               plot_loc$metadata[["element"]] == "Node Shape") {
      geom_nodes(
        aes_string(shape = plot_loc$metadata[["which"]][1]),
        color = node_color(),
        size = node_size(),
        alpha = node_alpha()
      )
    } else if (is.null(plot_loc$gg$cluster) &
               length(plot_loc$metadata[["which"]]) == 1 &
               plot_loc$metadata[["element"]] == "Node Size") {
      geom_nodes(
        aes_string(size = plot_loc$metadata[["which"]][1]),
        color = node_color(),
        alpha = node_alpha()
      )
    } else if (!is.null(plot_loc$gg$cluster) &
               length(plot_loc$metadata[["which"]]) < 1) {
      geom_nodes(aes_string(color = "cluster"),
                 size = node_size(),
                 alpha = node_alpha())
    } else if (!is.null(plot_loc$gg$cluster) &
               length(plot_loc$metadata[["which"]]) == 1 &
               plot_loc$metadata[["element"]] == "Node Shape") {
      geom_nodes(
        aes_string(color = "cluster",
                   shape = plot_loc$metadata[["which"]][1]),
        size = node_size(),
        alpha = node_alpha()
      )
    } else if (!is.null(plot_loc$gg$cluster) &
               length(plot_loc$metadata[["which"]]) == 1 &
               plot_loc$metadata[["element"]] == "Node Size") {
      geom_nodes(
        aes_string(color = "cluster",
                   size = plot_loc$metadata[["which"]][1]),
        size = node_size(),
        alpha = node_alpha()
      )
    }
  })
  
  # Add Metadata
  
  observeEvent(input$add_metadata, {
    if (length(plot_loc$metadata) == 0) {
      plot_loc$metadata <- list(which = input$which_metadata,
                                element = input$element_metadata)
      #    } else if (length(plot_loc$metadata[["which"]]) == 1){
      #      plot_loc$metadata[["which"]] <- append(plot_loc$metadata[["which"]], input$which_metadata)
      #      plot_loc$metadata[["element"]] <- append(plot_loc$metadata[["element"]], input$element_metadata)
    } else {
      show_toast(
        "Error",
        text = "Maximum number of attached metadata is reached.",
        type = "error",
        timer = 3000,
        timerProgressBar = TRUE,
        position = "bottom-end"
      )
    }
    
  })
  
  # Delete Metadata
  observeEvent(input$delete_meta, {
    if (length(plot_loc$metadata)[["which"]] == 1) {
      plot_loc$metadata[["which"]] <- character(0)
    } else {
      show_toast(
        "Error",
        text = "No metadata attached",
        type = "error",
        timer = 3000,
        timerProgressBar = TRUE,
        position = "top-end"
      )
    }
  })
  
  
  ### Save Tree Plot ----
  
  # Define download handler to save the plot
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0(input$plot_filename, ".", input$filetype_plot)
    },
    content = function(file) {
      if (input$filetype_plot == "png") {
        png(file, width = 1365, height = 600)
        if (input$tree_algo == "Minimum-Spanning") {
          print(plot_input())
        } else if (input$tree_algo == "Neighbour-Joining") {
          print(plot_loc$nj_plot)
        }
        dev.off()
      } else if (input$filetype_plot == "jpeg") {
        jpeg(file, width = 2730, height = 1200, quality = 100)
        if (input$tree_algo == "Minimum-Spanning") {
          print(plot_input())
        } else if (input$tree_algo == "Neighbour-Joining") {
          print(plot_loc$nj_plot)
        }
        dev.off()
      } else if (input$filetype_plot == "svg") {
        plot <- print(plot_input())
        ggsave(file=file, plot=plot, device = svg(), width = 50, height = 22, units = "cm")
      }
    }
  )
  
  
  ### Reactive Events ----
  
  # Download plot as picture
  observeEvent(input$save_plot, {
    showModal(
      modalDialog(
        textInput(
          "plot_filename",
          label = "",
          placeholder = "Plot1"
        ),
        radioGroupButtons(
          "filetype_plot",
          label = "",
          choices = c("png", "jpeg", "svg")
        ),
        title = "Save plot",
        fade = TRUE,
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          downloadBttn(
            "download_plot",
            style = "simple",
            label = "",
            size = "sm",
            icon = icon("download")
          )
          
        )
      )
    )
  })
  
  observeEvent(input$download_plot, {
    removeModal()
  })
  
  
  ### Cluster Analysis ----
  
  observeEvent(input$cluster_start, {
    # Create a function to calculate total within-cluster sum of squares
    set.seed(8)
    
    wss <- function(k) {
      kmeans_result <- kmeans(dist_matrix, centers = k)
      return(kmeans_result$tot.withinss)
    }
    
    # Determine a suitable number of clusters using the elbow method
    k_values <- 1:20  # You can adjust the range as needed
    wss_values <- sapply(k_values, wss)
    
    # Use the elbow method to select the number of clusters
    elbow_point <-
      findCutoff(k_values,
                 wss_values,
                 method = "first",
                 frac.of.steepest.slope = 0.5)
    
    # Perform K-Means clustering with the recommended number of clusters
    kmeans_opt <-
      kmeans(dist_matrix, centers = round(as.numeric(elbow_point$x)))
    
    
    cluster_add <- function(vector) {
      cluster <<- numeric()
      
      for (i in 1:length(vector)) {
        if (length(unname(kmeans_opt$cluster[which(names(kmeans_opt$cluster) == vector[i])])) > 0) {
          cluster[i] <<-
            unname(kmeans_opt$cluster[which(names(kmeans_opt$cluster) == vector[i])])
        } else {
          cluster[i] <<- 0
        }
      }
    }
    
    cluster_add(gg$name)
    
    plot_loc$gg <-
      plot_loc$gg %>% mutate(cluster = as.character(cluster))
    
    
    
  })
  
  ## Report ----
  
  
  ### Report UI Elements ----
  
  output$cgmlst_select_analysis <- renderUI({
    pickerInput(
      "cgmlst_select_analysis",
      label = "",
      choices = DF1$schemeinfo[,1][DF1$schemeinfo[,1] != ""],
      selected = DF1$schemeinfo[,1][DF1$schemeinfo[,1] != ""],
      options = list(
        `actions-box` = TRUE,
        size = 10,
        style = "background-color: white; border-radius: 5px;"
      ),
      multiple = TRUE,
      width = "190px"
    )
  })
  
  # Render Profile Selection Box
  
  output$save_rep <- renderUI({
    box(
      solidHeader = TRUE,
      status = "primary",
      width = "100%",
      column(
        width = 12,
        align = "left",
        HTML(
          paste(
            tags$span(style='color: white; font-size: 24px;', 'Save Report')
          )
        ),
        br(), br(),
        textInput(
          inputId = "report_dir",
          label = h5("Open after download", style = "color:white; font-size: 15px; margin-top: 0px; margin-bottom: 0px"),
          placeholder = paste0(getwd())
        ),
        br(),
        textInput(
          inputId = "rep_name",
          label = h5("Name", style = "color:white; font-size: 15px; margin-top: 0px; margin-bottom: 0px"),
          placeholder = paste0("Report_", Sys.Date())
        ),
        br(),
        checkboxInput(
          "open_report", 
          label = h5("Open after download", style = "color:white; font-size: 15px; margin-top: 2px")
        ),
        column(
          width = 12,
          align = "center",
          br(),
          actionBttn(
            "save_report",
            style = "simple",
            label = "Download",
            icon = icon("download"),
            size = "sm",
            color = "primary" 
          ),
          br()
        )
      )
    )
  })
  
  #### Render Slot Boxes ----
  
  observe({
    if(is.null(plot_loc$slot1_getname)) {
      output$slot1_box <- NULL
    } else {
      output$slot1_box <- renderUI({
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          HTML(
            paste(
              tags$span(style='color: white; font-size: 24px;', 
                        paste('Slot 1 -', plot_loc$slot1_getname))
            )
          ),
          br(),
          checkboxInput(
            "slot1_legend", 
            label = h5("Include entry legend", style = "color:white; font-size: 17px; margin-top: 15px"),
            value = TRUE
          ),
          div(
            class = "choosechannel",
            pickerInput(
              inputId = "slot1_legend_sel",
              label = h4("Display metadata", style = "color:white; margin-bottom: 5px; font-size: 14px"),
              choices = names(select(DF1$data, 3:12)),
              selected = c("Assembly Name", "Isolation Date", "Host", "Country", "City", "Typing Date"),
              options = list(
                `actions-box` = TRUE,
                size = 10,
                style = "background-color: white; border-radius: 5px;"
              ),
              multiple = TRUE
            )
          )
        )
      })
    }
    
    if(is.null(plot_loc$slot2_getname)) {
      output$slot2_box <- NULL
    } else {
      output$slot2_box <- renderUI({
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          HTML(
            paste(
              tags$span(style='color: white; font-size: 24px;', 
                        paste('Slot 2 -', plot_loc$slot2_getname))
            )
          ),
          br(), br(),
          checkboxInput(
            "slot2_legend", 
            label = h5("Include entry legend", style = "color:white; font-size: 17px; margin-top: 15px;"),
            value = TRUE
          ),
          div(
            class = "choosechannel",
            pickerInput(
              inputId = "slot2_legend_sel",
              label = h4("Display metadata", style = "color:white; margin-bottom: 5px; font-size: 14px"),
              choices = names(select(DF1$data, 3:12)),
              selected = c("Assembly Name", "Isolation Date", "Host", "Country", "City", "Typing Date"),
              options = list(
                `actions-box` = TRUE,
                size = 10,
                style = "background-color: white; border-radius: 5px;"
              ),
              multiple = TRUE
            )
          )
        )
      })
    }
    
    if(is.null(plot_loc$slot3_getname)) {
      output$slot3_box <- NULL
    } else {
      output$slot3_box <- renderUI({
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          HTML(
            paste(
              tags$span(style='color: white; font-size: 24px;', 
                        paste('Slot 3 -', plot_loc$slot3_getname))
            )
          ),
          br(),
          checkboxInput(
            "slot3_legend", 
            label = h5("Include entry legend", style = "color:white; font-size: 17px; margin-top: 15px;"),
            value = TRUE
          ),
          div(
            class = "choosechannel",
            pickerInput(
              inputId = "slot3_legend_sel",
              label = h4("Display metadata", style = "color:white; margin-bottom: 5px; font-size: 14px"),
              choices = names(select(DF1$data, 3:12)),
              selected = c("Assembly Name", "Isolation Date", "Host", "Country", "City", "Typing Date"),
              options = list(
                `actions-box` = TRUE,
                size = 10,
                style = "background-color: white; border-radius: 5px;"
              ),
              multiple = TRUE
            )
          )
        )
      })
    }
    
    if(is.null(plot_loc$slot4_getname)) {
      output$slot4_box <- NULL
    } else {
      output$slot4_box <- renderUI({
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          HTML(
            paste(
              tags$span(style='color: white; font-size: 24px;', 
                        paste('Slot 4 -', plot_loc$slot4_getname))
            )
          ),
          br(), 
          checkboxInput(
            "slot4_legend", 
            label = h5("Include entry legend", style = "color:white; font-size: 17px; margin-top: 15px;"),
            value = TRUE
          ),
          div(
            class = "choosechannel",
            pickerInput(
              inputId = "slot4_legend_sel",
              label = h4("Display metadata", style = "color:white; margin-bottom: 5px; font-size: 14px"),
              choices = names(select(DF1$data, 3:12)),
              selected = c("Assembly Name", "Isolation Date", "Host", "Country", "City", "Typing Date"),
              options = list(
                `actions-box` = TRUE,
                size = 10,
                style = "background-color: white; border-radius: 5px;"
              ),
              multiple = TRUE
            )
          )
        )
      })
    }
  })
  
  
  ### Send Plot to Report ----
  
  # Add to Slot
  observeEvent(input$add_slot, {
    showModal(
      modalDialog(
        paste0(
          "Adding plot to ", input$slot_select, ". Name the slot content and confirm (max. 10 characters)."
        ),
        textInput(
          "slot_name",
          label = "",
          value = "",
          placeholder = "Plot name"
        ),
        title = "Add to Slot",
        fade = TRUE,
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("conf_slot", "Confirm")
        )
      )
    )
  })
  
  observeEvent(input$conf_slot, {
    if(input$slot_select == "Slot 1") {
      if (length(input$slot_name) > 0) {
        plot_loc$slot1_getname <- substr(input$slot_name, 1, 10)
      } else {
        plot_loc$slot1_getname <- paste("Number One")
        test <<- "Number One"
      }
      jpeg(paste0(getwd(), "/Report/slot1_plot.jpeg"), width = 1365, height = 600,
           quality = 100)
      print(plot_input())
      dev.off()
      elements_data$slot1_include <- TRUE
    } else if(input$slot_select == "Slot 2") {
      if (length(input$slot_name) > 0) {
        plot_loc$slot2_getname <- substr(input$slot_name, 1, 10)
      } else {
        plot_loc$slot2_getname <- "Slot 2"
      }
      jpeg(paste0(getwd(), "/Report/slot2_plot.jpeg"), width = 1365, height = 600,
           quality = 100)
      print(plot_input())
      dev.off()
      elements_data$slot2_include <- TRUE
    } else if(input$slot_select == "Slot 3") {
      if (length(input$slot_name) > 0) {
        plot_loc$slot3_getname <- substr(input$slot_name, 1, 10)
      } else {
        plot_loc$slot3_getname <- "Slot 3"
      }
      plot_loc$slot3_getname <- substr(input$slot_name, 1, 10)
      jpeg(paste0(getwd(), "/Report/slot3_plot.jpeg"), width = 1365, height = 600,
           quality = 100)
      print(plot_input())
      dev.off()
      elements_data$slot3_include <- TRUE
    } else if(input$slot_select == "Slot 4") {
      if (length(input$slot_name) > 0) {
        plot_loc$slot4_getname <- substr(input$slot_name, 1, 10)
      } else {
        plot_loc$slot4_getname <- "Slot 4"
      }
      jpeg(paste0(getwd(), "/Report/slot4_plot.jpeg"), width = 1365, height = 600,
           quality = 100)
      print(plot_input())
      dev.off()
      elements_data$slot4_include <- TRUE
    }
    removeModal()
  })
  
  # Delete Slot
  observeEvent(input$delete_slot, {
    showModal(
      modalDialog(
        paste0(
          "Delete content of ", input$slot_select, ". Name the slot content and confirm."
        ),
        title = "Delete Slot",
        fade = TRUE,
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("conf_slot_del", "Delete")
        )
      )
    )
  })
  
  observeEvent(input$conf_slot_del, {
    if(input$slot_select == "Slot 1") {
      plot_loc$slot1_getname <- NULL
    } else if(input$slot_select == "Slot 2") {
      plot_loc$slot2_getname <- NULL
    } else if(input$slot_select == "Slot 3") {
      plot_loc$slot3_getname <- NULL
    } else if(input$slot_select == "Slot 4") {
      plot_loc$slot4_getname <- NULL
    }
    removeModal()
  })
  
  
  
  ### Save Report ----
  
  # Create a reactiveValues to store selected elements and their content
  elements_data <- reactiveValues(slot1_include = FALSE, slot2_include = FALSE, 
                                  slot3_include = FALSE, slot4_include = FALSE)
  
  #### Get Report values ----
  
  observe({
    if(input$date_general == TRUE) {
      elements_data$general_date <- input$date_general_select
    } else {
      elements_data$general_date <- NULL
    }
    
    if(input$operator_general == TRUE) {
      elements_data$general_operator <- input$operator_general_select
    } else {
      elements_data$general_operator <- NULL
    }
    
    if(input$institute_general == TRUE) {
      elements_data$general_institute <- input$institute_general_select
    } else {
      elements_data$general_institute <- NULL
    }
    
    if(input$comm_general == TRUE) {
      elements_data$general_comm <- input$comm_general_select
    } else {
      elements_data$general_comm <- NULL
    }
    
    if(input$cgmlst_analysis == TRUE) {
      elements_data$analysis_cgmlst <- input$cgmlst_select_analysis
    } else {
      elements_data$analysis_cgmlst <- NULL
    }
    
    if(input$tree_analysis == TRUE) {
      elements_data$analysis_tree <- TRUE
    } else {
      elements_data$analysis_tree <- NULL
    }
    
    if(input$kma_analysis == TRUE) {
      elements_data$analysis_kma <- TRUE
    } else {
      elements_data$analysis_kma <- NULL
    }
    
    if(elements_data$slot1_include == TRUE) {
      meta <- select(Database$Typing, 1:12)
      meta <- select(meta[which(meta$Include == TRUE),], -2)
      elements_data$slot1_meta <- meta
      elements_data$slot1_legend_sel <- input$slot1_legend_sel
      elements_data$slot1_legend <- input$slot1_legend
    }
    
    if(elements_data$slot2_include == TRUE) {
      meta <- select(Database$Typing, 1:12)
      meta <- select(meta[which(meta$Include == TRUE),], -2)
      elements_data$slot2_meta <- meta
      elements_data$slot2_legend_sel <- input$slot2_legend_sel
      elements_data$slot2_legend <- input$slot2_legend
    }
    
    if(elements_data$slot3_include == TRUE) {
      meta <- select(Database$Typing, 1:12)
      meta <- select(meta[which(meta$Include == TRUE),], -2)
      elements_data$slot3_meta <- meta
      elements_data$slot3_legend_sel <- input$slot3_legend_sel
      elements_data$slot3_legend <- input$slot3_legend
    }
    
    if(elements_data$slot4_include == TRUE) {
      meta <- select(Database$Typing, 1:12)
      meta <- select(meta[which(meta$Include == TRUE),], -2)
      elements_data$slot4_meta <- meta
      elements_data$slot4_legend_sel <- input$slot4_legend_sel
      elements_data$slot4_legend <- input$slot4_legend
    }
    
  })
  
  
  #### Event Save Report ----
  
  observeEvent(input$save_report, {
    
    # Get Scheme Info
    
    schemeinfo <-
      read_html(paste0(
        getwd(),
        "/Database/",
        gsub(" ", "_", DF1$scheme),
        "/scheme_info.html"
      )) %>%
      html_table(header = FALSE) %>%
      as.data.frame(stringsAsFactors = FALSE)
    names(schemeinfo) <- NULL
    
    # Filter and save data for the selected elements
    selected_data <- list(
      general_date = elements_data$general_date,
      general_operator = elements_data$general_operator,
      general_institute = elements_data$general_institute,
      general_comm = elements_data$general_comm,
      analysis_cgmlst = elements_data$analysis_cgmlst,
      cgmlst_info = schemeinfo,
      scheme = DF1$scheme,
      analysis_tree = elements_data$analysis_tree,
      analysis_kma = elements_data$analysis_kma,
      slot1 = elements_data$slot1_include,
      slot1_meta = elements_data$slot1_meta,
      slot1_legend = elements_data$slot1_legend,
      slot1_legend_sel = elements_data$slot1_legend_sel,
      slot2 = elements_data$slot2_include,
      slot2_meta = elements_data$slot2_meta,
      slot2_legend = elements_data$slot2_legend,
      slot2_legend_sel = elements_data$slot2_legend_sel,
      slot3 = elements_data$slot3_include,
      slot3_meta = elements_data$slot3_meta,
      slot3_legend = elements_data$slot3_legend,
      slot3_legend_sel = elements_data$slot3_legend_sel,
      slot4 = elements_data$slot4_include,
      slot4_meta = elements_data$slot4_meta,
      slot4_legend = elements_data$slot4_legend,
      slot4_legend_sel = elements_data$slot4_legend_sel
    )
    
    # Save data to an RDS file if any elements were selected
    if (length(selected_data) > 0) {
      saveRDS(selected_data, file = paste0(getwd(), "/Report/selected_elements.rds"))
    }
    
    rmarkdown::render(paste0(getwd(), "/Report/Report.Rmd"))
    
    
    if (input$open_report == TRUE) {
      system(paste("open", paste0(getwd(), "/Report/Report.html")))
    }
    
  })
  
  ### Save Report Profile ----
  
  observeEvent(input$save_rep_profile, {
    
    # save profile except dates or times
    report_profile <- list(
      selected_general = selected_general,
      selected_sampleinfo = selected_sampleinfo,
      selected_sequencing = selected_sequencing,
      selected_analysis = selected_analysis,
      general_date = NULL,
      general_author = elements_data$general_author,
      general_com = elements_data$general_com,
      sample_date = NULL,
      sample_loc = elements_data$sample_loc,
      sample_op = elements_data$sample_op,
      sample_com = elements_data$sample_com,
      seq_device = elements_data$seq_device,
      seq_flowcell = elements_data$seq_flowcell,
      seq_start = NULL,
      seq_end = NULL,
      seq_op = elements_data$seq_op,
      seq_com = elements_data$seq_com,
      ana_date = NULL,
      ana_com = elements_data$ana_com
    )
    
    # Save data to an RDS file
    if (length(report_profile) > 0) {
      saveRDS(
        report_profile,
        file = paste0(
          getwd(),
          "/rep_profiles/",
          input$rep_profilename,
          ".rds"
        )
      )
    }
    
    rep_profile$profile_names <-
      list.files(paste0(getwd(), "/Report/rep_profiles"), full.names = TRUE)
  })
  
  ### Load Report Profile ----
  
  rep_profile <- reactiveValues()
  
  observe(rep_profile$profile_names <-
            list.files(paste0(getwd(), "/Report/rep_profiles"), full.names = TRUE))
  
  output$selProfile <- renderUI(
    selectInput(
      inputId = "sel_rep_profile",
      label = "Select report profile",
      choices = append(
        c("None"),
        gsub(".*/(.*).rds", "\\1", rep_profile$profile_names)
      )
    )
  )
  
  # General Tickbox
  general_selected <- reactive({
    if (input$sel_rep_profile %in% "None") {
      c("Analysis Date", "Author")
    } else {
      readRDS(paste0(
        getwd(),
        "/rep_profiles/",
        input$sel_rep_profile,
        ".rds"
      ))[[1]]
    }
  })
  
  output$include_general <- renderUI(
    checkboxGroupInput(
      inputId = "include_general",
      label = "",
      choices = c("Analysis Date", "Author", "Experiment Info"),
      selected = general_selected()
    )
  )
  
  # Sample Info Tickbox
  sampleinfo_selected <- reactive({
    if (input$sel_rep_profile %in% "None") {
      c("Sampling Date", "Sampling Location")
    } else {
      readRDS(paste0(
        getwd(),
        "/rep_profiles/",
        input$sel_rep_profile,
        ".rds"
      ))[[2]]
    }
  })
  
  output$include_sampleinfo <- renderUI(
    checkboxGroupInput(
      inputId = "include_sampleinfo",
      label = "",
      choices = c(
        "Sampling Date",
        "Sampling Location",
        "Taken by (Name)",
        "Comment"
      ),
      selected = sampleinfo_selected()
    )
  )
  
  # Sequencing Tickbox
  sequencing_selected <- reactive({
    if (input$sel_rep_profile %in% "None") {
      c("Device", "Fow Cell", "Operator")
    } else {
      readRDS(paste0(
        getwd(),
        "/rep_profiles/",
        input$sel_rep_profile,
        ".rds"
      ))[[3]]
    }
  })
  
  output$include_sequencing <- renderUI(
    checkboxGroupInput(
      inputId = "include_sequencing",
      label = "",
      choices = c(
        "Device",
        "Flow Cell",
        "Run Start",
        "Run Finished",
        "Operator",
        "Output Size",
        "Comment"
      ),
      selected = sequencing_selected()
    )
  )
  
  # Analysis Tickbox
  analysis_selected <- reactive({
    if (input$sel_rep_profile %in% "None") {
      c("Analysis Date", "cgMLST Scheme")
    } else {
      readRDS(paste0(
        getwd(),
        "/rep_profiles/",
        input$sel_rep_profile,
        ".rds"
      ))[[4]]
    }
  })
  
  output$include_analysis <- renderUI(
    checkboxGroupInput(
      inputId = "include_analysis",
      label = "",
      choices = c(
        "Analysis Date",
        "Assembly Parameters",
        "cgMLST Scheme",
        "Comment"
      ),
      selected = analysis_selected()
    )
  )
  
  
  
  ## Typing  ----
  
  ### Single Typing ----
  
  typing_reactive <- reactiveValues(table = data.frame(), single_path = data.frame(), progress = 0, progress_pct = 0, progress_format_start = 0, progress_format_end = 0)
  
  
  #### Render UI Elements ----
  
  # Render Initiate Typing UI
  output$initiate_typing_ui <- renderUI({
    column(
      width = 3,
      align = "center",
      br(),
      br(),
      h3(p("Initiate Typing"), style = "color:white"),
      br(),
      br(),
      p(
        HTML(
          paste(
            tags$span(style='color: white; font-size: 15px; margin-bottom: 0px', 'Select Assembly File (.fasta)')
          )
        )
      ),
      shinyFilesButton(
        "genome_file",
        "Browse" ,
        title = "Please select the genome in .fasta format:",
        multiple = FALSE,
        buttonType = "default",
        class = NULL
      ),
      br(),
      br(),
      br(),
      uiOutput("genome_path")
    )
  })
  
  # Render Declare Metadata UI
  
  observe({
    if (nrow(typing_reactive$single_path) < 1) {
      output$genome_path <- renderUI(HTML(
        paste("<span style='color: white;'>", "No file selected.")
      ))
      
      output$metadata_single_box <- NULL
      
    } else if (nrow(typing_reactive$single_path) > 0) {
      
      # Render selected assembly path
      output$genome_path <- renderUI({
        HTML(
          paste(
            "<span style='color: white; font-weight: bolder'>",
            as.character(typing_reactive$single_path$name)
          )
        )
      })
      
      # Render metadata declaration box
      output$metadata_single_box <- renderUI({
        column(
          width = 3,
          align = "center",
          tags$style(".append_table {margin-top: 0px; margin-left: 3px}"),
          tags$style("div#append_isodate.shiny-date-input.form-group.shiny-input-container.shiny-bound-input input.form-control.shinyjs-resettable {text-align: center}"),
          tags$style("div#append_analysisdate.shiny-date-input.form-group.shiny-input-container.shiny-bound-input input.form-control.shinyjs-resettable {text-align: center}"),
          tags$style(".append_table_country .btn {height: 32px}"),
          tags$style(".append_table_country {margin-top: 23px; margin-bottom: 5px}"),
          tags$style("button#conf_meta_single {background: #20e6e5; color: black}"),
          br(),
          br(),
          h3(p("Declare Metadata"), style = "color:white"),
          br(),
          br(),
          box(
            solidHeader = TRUE,
            status = "primary",
            width = "90%",
            fluidRow(
              column(
                width = 5,
                align = "left",
                h5("Assembly ID", style = "color:white; margin-top: 30px; margin-left: 15px")
              ),
              column(
                width = 7,
                align = "left",
                div(
                  class = "append_table",
                  textInput("assembly_id",
                            label = "",
                            width = "80%")
                )
              )
            ),
            fluidRow(
              column(
                width = 5,
                align = "left",
                h5("Assembly Name", style = "color:white; margin-top: 30px; margin-left: 15px")
              ),
              column(
                width = 7,
                align = "left",
                div(
                  class = "append_table",
                  textInput("assembly_name",
                            label = "",
                            width = "80%")
                )
              )
            ),
            fluidRow(
              column(
                width = 5,
                align = "left",
                h5("Isolation Date", style = "color:white; margin-top: 30px; margin-left: 15px")
              ),
              column(
                width = 7,
                align = "left",
                div(
                  class = "append_table",
                  dateInput("append_isodate",
                            label = "",
                            width = "60%")
                )
              )
            ),
            fluidRow(
              column(
                width = 5,
                align = "left",
                h5("Host", style = "color:white; margin-top: 30px; margin-left: 15px")
              ),
              column(
                width = 7,
                align = "left",
                div(
                  class = "append_table",
                  textInput("append_host",
                            label = "",
                            width = "80%")
                )
              )
            ),
            fluidRow(
              column(
                width = 5,
                align = "left",
                h5("Country", style = "color:white; margin-top: 30px; margin-left: 15px")
              ),
              column(
                width = 7,
                align = "left",
                div(
                  class = "append_table_country",
                  pickerInput(
                    "append_country",
                    label = "",
                    choices = list("Common" = sel_countries,
                                   "All Countries" = country_names),
                    options = list(
                      `live-search` = TRUE,
                      `actions-box` = TRUE,
                      size = 10,
                      style = "background-color: white; border-radius: 5px;"
                    ),
                    width = "90%"
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 5,
                align = "left",
                h5("City", style = "color:white; margin-top: 30px; margin-left: 15px")
              ),
              column(
                width = 7,
                align = "left",
                div(
                  class = "append_table",
                  textInput(
                    "append_city",
                    label = "",
                    width = "80%"
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 5,
                align = "left",
                h5("Typing Date", style = "color:white; margin-top: 30px; margin-left: 15px")
              ),
              column(
                width = 7,
                align = "left",
                div(
                  class = "append_table",
                  dateInput(
                    "append_analysisdate",
                    label = "",
                    value = Sys.Date(),
                    width = "60%"
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                align = "center",
                br(), br(),
                actionButton(
                  inputId = "conf_meta_single",
                  label = "Confirm"
                ),
                br()
              )
            ),
            br()
          )
        )
      })
    }
  })
  
  # Get genome datapath
  
  volumes = getVolumes()
  
  observe({
    # Get selected Genome in Single Mode
    shinyFileChoose(input,
                    "genome_file",
                    roots = c(wd = "/home"),
                    session = session)
    typing_reactive$single_path <- parseFilePaths(roots = c(wd = "/home"), input$genome_file)
  })
  
  #### Run KMA ----
  
  observeEvent(input$typing_start, {
    
    # Remove UI 
    output$initiate_typing_ui <- NULL
    output$metadata_single_box <- NULL
    output$start_typing_ui <- NULL
    
    # Locate folder containing cgMLST scheme
    search_string <-
      paste0(gsub(" ", "_", DF1$scheme), "_alleles")
    
    scheme_folders <-
      dir_ls(paste0(getwd(), "/Database/", gsub(" ", "_", DF1$scheme)))
    
    if (any(grepl(search_string, scheme_folders))) {
      # KMA initiate index
      
      scheme_select <-
        as.character(scheme_folders[which(grepl(search_string, scheme_folders))])
      
      show_toast(
        title = "Typing Initiated",
        type = "success",
        position = "top-end",
        timer = 12000
      )
      
      ### Run KMA Typing
      
      kma_run <- paste0(
        "#!/bin/bash\n",
        'cd execute', '\n',
        'echo 0 > ',
        shQuote(paste0(getwd(), "/execute/progress.fifo")),
        "\n",
        'base_path="/home/marian/Documents/Projects/Masterthesis"', '\n',
        'kma_database="$base_path/PhyloTree/execute/kma_database/"', shQuote(paste0(gsub(" ", "_", DF1$scheme))), '\n',
        "genome=",
        shQuote(typing_reactive$single_path$datapath),
        "\n",
        '/home/marian/miniconda3/bin/kma index -i "$genome" -o "$kma_database"', '\n',
        "query_folder=",
        shQuote(paste0(
          getwd(),
          "/Database/",
          gsub(" ", "_", DF1$scheme),
          "/",
          search_string
        )),
        "\n",
        '# Directory name', '\n',
        'results=', shQuote(paste0(getwd(),
                                   "/Database/",
                                   gsub(" ", "_", DF1$scheme), 
                                   "/results")), '\n',
        '# Remove the existing directory (if it exists)', '\n',
        'if [ -d "$results" ]; then', '\n',
        '    rm -r "$results"', '\n',
        'fi', '\n',
        '# Create a new directory', '\n', 
        'mkdir "$results"', '\n',
        'count=0',
        "\n",
        'for query_file in "$query_folder"/*.fasta; do',
        "\n",
        'if [ -f "$query_file" ]; then',
        "\n",
        'query_filename=$(basename "$query_file")',
        "\n",
        'query_filename_noext="${query_filename%.*}"',
        "\n",
        'output_file="$results/$query_filename_noext"',
        "\n",
        '/home/marian/miniconda3/bin/kma -i "$query_file" -o "$output_file" -t_db "$kma_database" -nc -status',
        "\n",
        '((count++))',
        "\n",
        'echo $count > ',
        shQuote(paste0(getwd(), "/execute/progress.fifo")),
        "\n",
        'fi',
        "\n",
        'done', '\n',
        'echo 888888 >> ',
        shQuote(paste0(getwd(), "/execute/progress.fifo")), '\n',
        'Rscript ', shQuote(paste0(getwd(), "/execute/single_typing.R")), '\n',
        'echo 999999 >> ',
        shQuote(paste0(getwd(), "/execute/progress.fifo")) 
      )
      
      # Specify the path to save the script
      kma_run_path <- paste0(getwd(), "/execute", "/kma_run.sh")
      
      # Write the script to a file
      cat(kma_run, file = kma_run_path)
      
      # Make the script executable
      system(paste("chmod +x", kma_run_path))
      
      # Execute the script
      system(paste(kma_run_path), wait = FALSE)
      
      scheme_loci <-
        list.files(path = scheme_select, full.names = TRUE)
      
      # Filter the files that have the ".fasta" extension
      typing_reactive$scheme_loci_f <-
        scheme_loci[grep(".fasta$", scheme_loci, ignore.case = TRUE)]
      
      output$single_typing_progress <- renderUI({
        fluidRow(
          br(), br(), 
          column(width = 1),
          column(
            width = 2,
            h3(p("Pending Single Typing ..."), style = "color:white")
          ),
          column(
            width = 2,
            br(),
            HTML(paste('<i class="fa fa-spinner fa-spin" style="font-size:24px;color:white"></i>'))
          ),
          br(), br(), br(),
          fluidRow(
            column(width = 1),
            column(
              width = 4,
              br(), br(), br(),
              uiOutput("reset_single_typing"),
              progressBar(
                "progress_bar",
                value = 0,
                display_pct = TRUE,
                title = ""
              )
            )
          ),
          fluidRow(
            column(width = 1),
            uiOutput("typing_formatting"),
            uiOutput("typing_fin")
          )
        )
      })
      
    } else {
      show_alert(
        title = "Error",
        text = paste0(
          "Folder containing cgMLST alleles not in working directory.",
          "\n",
          "Download cgMLST Scheme for selected Organism first."
        ),
        type = "error"
      )
    }
  })
  
  # Function to update Progress Bar
  update <- reactive({
    invalidateLater(3000, session)
    progress <- readLines(paste0(getwd(), "/execute", "/progress.fifo"))[1]
    if(!is.na(readLines(paste0(getwd(), "/execute", "/progress.fifo"))[2])) {
      typing_reactive$progress_format_start <- as.numeric(readLines(paste0(getwd(), "/execute", "/progress.fifo"))[2])
    }
    if(!is.na(readLines(paste0(getwd(), "/execute", "/progress.fifo"))[3])) {
      typing_reactive$progress_format_end <- as.numeric(readLines(paste0(getwd(), "/execute", "/progress.fifo"))[3])
    }
    progress <- as.numeric(progress)
    typing_reactive$progress <- progress
    typing_reactive$progress_pct <-
      floor((as.numeric(progress) / length(typing_reactive$scheme_loci_f)) * 100)
    progress_pct <-
      floor((as.numeric(progress) / length(typing_reactive$scheme_loci_f)) * 100)
  })
  
  # Observe Typing Progress
  observe({
    
    # Update Progress Bar
    updateProgressBar(
      session = session,
      id = "progress_bar",
      value = update(),
      total = 100,
      title = paste0(as.character(typing_reactive$progress), "/", length(typing_reactive$scheme_loci_f))
    )
    
    if (typing_reactive$progress_format_start == 888888) {
      output$typing_formatting <- renderUI({
        column(
          width = 2,
          align = "center",
          HTML(paste("Transforming data...", '<i class="fa-solid fa-spinner fa-spin-pulse fa-lg" style="color: #ffffff;"></i>'))
        )
      })
    } else {
      output$typing_formatting <- NULL
    }
    
    # Render when finalized  
    if (typing_reactive$progress_format_end == 999999) {
      output$typing_formatting <- NULL
      
      output$typing_fin <- renderUI({
        column(
          width = 2,
          align = "center",
          br(), br(),
          HTML(paste("<span style='color: white;'>", "Typing finalized.", "Reset to start another typing process.", sep = '<br/>')),
          br(), br(),
          actionButton(
            "reset_single_typing",
            "Reset",
            icon = icon("arrows-rotate")
          )
        )
      })
      
    } else {
      output$typing_fin <- NULL
    }
    
  })
  
  #### Declare Metadata  ----
  
  observeEvent(input$conf_meta_single, {
    
    meta_info <- data.frame(assembly_id = input$assembly_id,
                            assembly_name = input$assembly_name,
                            cgmlst_typing = DF1$scheme,
                            append_isodate = input$append_isodate,
                            append_host = input$append_host,
                            append_country = input$append_country,
                            append_city = input$append_city,
                            append_analysisdate = input$append_analysisdate,
                            db_directory = paste0(getwd(), "/Database/", gsub(" ", "_", DF1$scheme)))
    
    saveRDS(meta_info, paste0(
      getwd(),
      "/execute/meta_info_single.rds"
    ))
    
    show_toast(
      title = "Metadata declared",
      type = "success",
      position = "top-end",
      timer = 3000
    )
    
    # Render Start Typing UI
    
    output$start_typing_ui <- renderUI({
      column(
        width = 3,
        align = "center",
        br(),
        br(),
        h3(p("Start Typing"), style = "color:white"),
        br(),
        br(),
        HTML(
          paste(
            "<span style='color: white;'>",
            "Typing by <strong>",
            DF1$scheme,
            "</strong> scheme."
          )
        ),
        br(), br(),
        actionButton(
          inputId = "typing_start",
          label = "Start",
          icon = icon("circle-play")
        )
      )
    })
    
  })
  
  ####  Events Single Typing ----
  
  observeEvent(input$reset_single_typing, {
    
    typing_reactive$progress <- 0
    
    typing_reactive$progress_pct <- 0
    
    typing_reactive$progress_format <- 900000
    
    output$single_typing_progress <- NULL
    
    output$typing_fin <- NULL
    
    output$typing_formatting <- NULL
    
    typing_reactive$single_path <- data.frame()
    
    # Resetting Progress.fifo 
    
    reset_kma <- paste0(
      "#!/bin/bash\n",
      'cd execute', '\n',
      'echo 0 > ',
      shQuote(paste0(getwd(), "/execute/progress.fifo"))
      )
    
    # Specify the path to save the script
    reset_kma_path <- paste0(getwd(), "/execute", "/reset_kma.sh")
    
    # Write the script to a file
    cat(reset_kma, file = reset_kma_path)
    
    # Make the script executable
    system(paste("chmod +x", reset_kma_path))
    
    # Execute the script
    system(paste(reset_kma_path), wait = FALSE)
    
    output$initiate_typing_ui <- renderUI({
      column(
        width = 3,
        align = "center",
        br(),
        br(),
        h3(p("Initiate Typing"), style = "color:white"),
        br(),
        br(),
        p(
          HTML(
            paste(
              tags$span(style='color: white; font-size: 15px; margin-bottom: 0px', 'Select Assembly File (.fasta)')
            )
          )
        ),
        shinyFilesButton(
          "genome_file",
          "Browse" ,
          title = "Please select the genome in .fasta format:",
          multiple = FALSE,
          buttonType = "default",
          class = NULL
        ),
        br(),
        br(),
        br(),
        uiOutput("genome_path")
      )
    })
  })
  
  ### Multi Typing ----
  
  #### Render Multi Typing UI Elements ----
  
  # Check if ongoing Multi Typing - Render accordingly
  
  output$pending_multi <- reactive({
    invalidateLater(3000, session)
    if(grepl("Multi Typing", tail(readLines(paste0(getwd(),"/execute/script_log.txt")), n = 1))) {
      return('no') 
    } else if(!grepl("Multi Typing", tail(readLines(paste0(getwd(),"/execute/script_log.txt")), n = 1))) {
      return('yes')
    } 
  })
  
  outputOptions(output, "pending_multi", suspendWhenHidden = FALSE) 
  
  observe({
    # Get selected Genome in Multi Mode
    shinyDirChoose(input,
                   "genome_file_multi",
                   roots = c(wd = "/home"),
                   session = session)
    
    typing_reactive$table <-
      data.frame(Include = rep(TRUE, length(list.files(
        as.character(parseDirPath(
          roots = c(wd = "/home"), input$genome_file_multi
        ))
      ))),
      Files = list.files(as.character(
        parseDirPath(roots = c(wd = "/home"), input$genome_file_multi)
      )))
    
    if (nrow(typing_reactive$table) > 0) {
      output$multi_select_table <- renderRHandsontable({
        rhandsontable(typing_reactive$table, height = 500) %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          hot_cols(columnSorting = TRUE) %>%
          hot_rows(rowHeights = 25) %>%
          hot_col(1,
                  halign = "htCenter",
                  valign = "htTop",
                  width = "auto")
      })
    } else {
      output$multi_select_table <- NULL
    }
    
  })
  
  output$assembly_files_table <- renderUI({
    rHandsontableOutput("multi_select_table")
  })
  
  output$genome_file_multi_bttn <- renderUI({
    shinyDirButton(
      "genome_file_multi",
      "Browse" ,
      title = "Please select the folder containing the genome assemblies in .fasta format",
      buttonType = "default"
    )
  })
  
  output$initiate_typing_header <- renderUI(h3(p("Initiate Typing"), style = "color:white"))
  
  observeEvent(input$conf_meta_multi, {
    meta_info <- data.frame(cgmlst_typing = DF1$scheme,
                            append_isodate = input$append_isodate_multi,
                            append_host = input$append_host_multi,
                            append_country = input$append_country_multi,
                            append_city = input$append_city_multi,
                            append_analysisdate = input$append_analysisdate_multi,
                            db_directory = paste0(getwd(), "/Database/", gsub(" ", "_", DF1$scheme)))
    
    saveRDS(meta_info, paste0(
      getwd(),
      "/execute/meta_info.rds"
    ))
    
    show_toast(
      title = "Metadata declared",
      type = "success",
      position = "top-end",
      timer = 3000
    )
    
    Sys.sleep(0.5)
    
    output$start_multi_typing_ui <- renderUI({
      column(
        width = 3,
        align = "center",
        br(),
        br(),
        h3(p("Start Typing"), style = "color:white"),
        br(),
        br(),
        HTML(
          paste(
            "<span style='color: white;'>",
            "Typing by <strong>",
            DF1$scheme,
            "</strong> scheme."
          )
        ),
        br(), br(),
        actionButton(
          "start_typ_multi",
          "Start",
          icon = icon("circle-play")
        )
      )
    })
    
  })
  
  #### Events Multi Typing ----
  
  observeEvent(input$reset_multi, {
    if(!grepl("Multi Typing", tail(readLines(paste0(getwd(),"/execute/script_log.txt")), n = 1))) {
      showModal(
        modalDialog(
          paste0(
            "A Multi Typing process is still pending. Stopping this process will cancel the processing."
          ),
          title = "Reset Multi Typing",
          fade = TRUE,
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("conf_multi_kill", "Stop", class = "btn btn-danger")
          )
        )
      )
    }
  })
  
  # Confirm Reset after 
  observeEvent(input$conf_multi_kill, {
    removeModal()
    
    show_toast(
      title = "Execution cancelled",
      type = "warning",
      position = "top-end",
      timer = 6000
    )
    kill_multi <- paste0(
      '#!/bin/bash', '\n',
      'log_file=', shQuote(paste0(getwd(), "/execute/script_log.txt")), '\n',
      'TARGET_SCRIPT=', shQuote(paste0(getwd(), "/execute/kma_multi.sh")), '\n',
      '# Function to log messages to the file', '\n',
      'log_message() {', '\n',
      '    echo "$(date +"%Y-%m-%d %H:%M:%S") - $1" >> "$log_file"', '\n',
      '}', '\n',
      '# Find the process ID (PID) of the script', '\n',
      'PID=$(pgrep -f "$TARGET_SCRIPT")', '\n',
      'if [ -z "$PID" ]; then', '\n',
      '  echo "No process found for $TARGET_SCRIPT"', '\n',
      'else', '\n',
      '  # Kill the process', '\n',
      '  echo "Killing process $PID for $TARGET_SCRIPT"', '\n',
      '  kill "$PID"', '\n',
      'fi', '\n',
      'log_message "Multi Typing cancelled"'
    )
    
    # Specify the path to save the script
    kill_multi_path <-
      paste0(getwd(), "/execute/kill_multi.sh")
    
    # Write the script to a file
    cat(kill_multi, file = kill_multi_path)
    
    # Make the script executable  
    system(paste("chmod +x", kill_multi_path))
    
    # Execute the script
    system(paste(kill_multi_path), wait = FALSE)
    
    
  })
  
  observeEvent(input$start_typ_multi, {
    showModal(
      modalDialog(
        paste0(
          "Typing multiple assemblies will take a while. Continue?"
        ),
        title = "Start Multi Typing",
        fade = TRUE,
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("conf_start_multi", "Start", class = "btn btn-default")
        )
      )
    )
    
    observeEvent(input$Cancel, {
      removeModal()
    })
  })
  
  observeEvent(input$conf_start_multi, {
    
    removeModal()
    
    show_toast(
      title = "Multi Typing started",
      type = "success",
      position = "top-end",
      timer = 6000
    )
    
    # Remove Allelic Typing Controls
    
    output$genome_file_multi_bttn <- NULL
    output$initiate_typing_header <- NULL
    output$assembly_files_table <- NULL
    output$header_declare_metadata <- NULL
    output$metadata_multi_box <- NULL
    output$start_multi_typing_ui <-NULL
    
    # List Genome Assemblies Included in Analysis in Vector
    genome_selected <- hot_to_r(input$multi_select_table)
    
    genome_names <<- genome_selected$Files[which(genome_selected$Include == TRUE)]
    
    kma_multi <- paste0(
      '#!/bin/bash', '\n',
      'cd execute', '\n',
      '# Get Genome Folder', '\n',
      'genome_folder=', shQuote(as.character(parseDirPath(roots = c(wd = "/home"), 
                                                          input$genome_file_multi))), '\n',
      'selected_genomes=', shQuote(paste0(getwd(), "/execute/selected_genomes")), '\n',
      'log_file=', shQuote(paste0(getwd(), "/execute/script_log.txt")), '\n',
      '# Function to log messages to the file', '\n',
      'log_message() {', '\n',
      '    echo "$(date +"%Y-%m-%d %H:%M:%S") - $1" >> "$log_file"', '\n',
      '}', '\n',
      '# Create a log file or truncate if it exists', '\n',
      'echo "Script Log" > "$log_file"', '\n',
      '# Remove the existing directory (if it exists)', '\n',
      'if [ -d "$selected_genomes" ]; then', '\n',
      '    rm -r "$selected_genomes"', '\n',
      'fi', '\n',
      'mkdir $selected_genomes', '\n',
      '# List of file names to copy', '\n',
      'file_names=(', paste(shQuote(genome_names), collapse= " "), ')', '\n',
      '# Loop through the list of file names and copy them to the new folder', '\n',
      'for file in "${file_names[@]}"; do', '\n',
      '    if [ -f "$genome_folder/$file" ]; then', '\n',
      '        cp "$genome_folder/$file" "$selected_genomes/"', '\n',
      '        log_message "Initiated $file"', '\n',
      '    else', '\n', 
      '        log_message "$file not found in $genome_folder"', '\n',
      '    fi', '\n',
      'done', '\n',
      '# Directory name', '\n',
      'results=', shQuote(paste0(getwd(),
                                 "/Database/",
                                 gsub(" ", "_", DF1$scheme), 
                                 "/results")), '\n',
      '# Remove the existing directory (if it exists)', '\n',
      'if [ -d "$results" ]; then', '\n',
      '    rm -r "$results"', '\n',
      'fi', '\n',
      '# Create a new directory', '\n', 
      'mkdir "$results"', '\n',
      '#INDEXING GENOME AS DATABASE', '\n',
      'database_name=', shQuote(gsub(" ", "_", DF1$scheme)), '\n',
      '#RUNNING KMA', '\n',
      'query_folder=', shQuote(paste0(getwd(), 
                                      "/Database/", 
                                      gsub(" ", "_", DF1$scheme), 
                                      "/",
                                      gsub(" ", "_", DF1$scheme), 
                                      "_alleles")), '\n',
      'genome_filename_noext=""', '\n',
      '#Indexing Loop', '\n',
      'for genome in "$selected_genomes"/*; do', '\n',
      '    if [ -f "$genome" ]; then', '\n',
      '    genome_filename=$(basename "$genome")', '\n',
      '    genome_filename_noext="${genome_filename%.*}"', '\n',
      '    log_message "Processing $genome_filename"', '\n',
      '    /home/marian/miniconda3/bin/kma index -i "$genome" -o "$database_name"', '\n',
      '    fi', '\n',
      '    mkdir "$results/$genome_filename_noext"', '\n',
      '#Running Loop', '\n',
      '    for query_file in "$query_folder"/*.fasta; do', '\n',
      '        if [ -f "$query_file" ]; then', '\n',
      '        query_filename=$(basename "$query_file")', '\n',
      '        query_filename_noext="${query_filename%.*}"', '\n',
      '        output_file="$results/$genome_filename_noext/$query_filename_noext"', '\n',
      '        /home/marian/miniconda3/bin/kma -i "$query_file" -o "$output_file" -t_db "$database_name" -nc -status', '\n',
      '        fi', '\n',
      '    done', '\n',
      '    Rscript ', shQuote(paste0(getwd(), "/execute/automatic_typing.R")), '\n',
      '    log_message "Successful typing of $genome_filename"', '\n',
      'done', '\n',
      'log_message "Multi Typing finalized."'
    )
    
    # Specify the path to save the script
    kma_multi_path <-
      paste0(getwd(), "/execute/kma_multi.sh")
    
    # Write the script to a file
    cat(kma_multi, file = kma_multi_path)
    
    # Make the script executable  
    system(paste("chmod +x", kma_multi_path))
    
    # Execute the script
    system(paste("nohup", kma_multi_path, "> script.log 2>&1"), wait = FALSE)
    
  })
  
  #### Render Elements in order ----
  
  observe({
    if (nrow(typing_reactive$table) > 0) {
      output$header_declare_metadata <- renderUI({
        h3(p("Declare Metadata"), style = "color:white")
      })
      
      output$metadata_multi_box <- renderUI({
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "90%",
          fluidRow(
            column(
              width = 5,
              align = "left",
              h5("Assembly ID", style = "color:white; margin-top: 30px; margin-left: 15px")
            ),
            column(
              width = 7,
              align = "left",
              h5("Assembly filename", style = "color:white; margin-top: 30px; margin-left: 5px; font-style: italic")
            )
          ),
          fluidRow(
            column(
              width = 5,
              align = "left",
              h5("Assembly Name", style = "color:white; margin-top: 30px; margin-left: 15px")
            ),
            column(
              width = 7,
              align = "left",
              h5("Assembly filename", style = "color:white; margin-top: 30px; margin-left: 5px; font-style: italic")
            )
          ),
          fluidRow(
            column(
              width = 5,
              align = "left",
              h5("Isolation Date", style = "color:white; margin-top: 30px; margin-left: 15px")
            ),
            column(
              width = 7,
              align = "left",
              div(
                class = "append_table",
                dateInput("append_isodate_multi",
                          label = "",
                          width = "60%")
              )
            )
          ),
          fluidRow(
            column(
              width = 5,
              align = "left",
              h5("Host", style = "color:white; margin-top: 30px; margin-left: 15px")
            ),
            column(
              width = 7,
              align = "left",
              div(
                class = "append_table",
                textInput("append_host_multi",
                          label = "",
                          width = "80%")
              )
            )
          ),
          fluidRow(
            column(
              width = 5,
              align = "left",
              h5("Country", style = "color:white; margin-top: 30px; margin-left: 15px")
            ),
            column(
              width = 7,
              align = "left",
              div(
                class = "append_table_country",
                pickerInput(
                  "append_country_multi",
                  label = "",
                  choices = list("Common" = sel_countries,
                                 "All Countries" = country_names),
                  options = list(
                    `live-search` = TRUE,
                    `actions-box` = TRUE,
                    size = 10,
                    style = "background-color: white; border-radius: 5px;"
                  ),
                  width = "90%"
                )
              )  
            )
          ),
          fluidRow(
            column(
              width = 5,
              align = "left",
              h5("City", style = "color:white; margin-top: 30px; margin-left: 15px")
            ),
            column(
              width = 7,
              align = "left",
              div(
                class = "append_table",
                textInput("append_city_multi",
                          label = "",
                          width = "80%")
              )
            )
          ),
          fluidRow(
            column(
              width = 5,
              align = "left",
              h5("Typing Date", style = "color:white; margin-top: 30px; margin-left: 15px")
            ),
            column(
              width = 7,
              align = "left",
              div(
                class = "append_table",
                dateInput(
                  "append_analysisdate_multi",
                  label = "",
                  value = Sys.Date(),
                  width = "60%"
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              align = "center",
              br(), br(),
              actionButton(
                inputId = "conf_meta_multi",
                label = "Confirm"
              ),
              br()
            )
          ),
          br()
        )
      })
      
    } else {
      output$header_declare_metadata <- NULL
      output$metadata_multi_box <- NULL
      output$start_multi_typing_ui <- NULL
    }
  })
  
  #### User Feedback ----
  
  readLogFile <- reactive({
    invalidateLater(5000, session)
    readLines(paste0(getwd(), "/execute/script_log.txt"))
  })
  
  # Render log content
  output$logText <- renderPrint({
    readLogFile()
  })
  
  output$multi_typing_progress_header <- renderUI(
    h3(p("Pending Multi Typing ..."), style = "color:white"))
  
  output$multi_typing_progress_symbol <- renderUI(
    HTML(paste('<i class="fa fa-spinner fa-spin" style="font-size:24px;color:white"></i>'))
  )  
  
} # end server


# Shiny ----

shinyApp(ui = ui, server = server)