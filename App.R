# PhyloTrace

library(shiny)
library(R.utils)
library(igraph)
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes)
library(ggplot2)
library(ggnewscale)
library(ggplotify) 
library(ape)
library(treeio)
library(ggtree)
library(ggtreeExtra)
library(tidyverse)
library(rlang)
library(tidytree)
library(shinyFiles)
library(dplyr)
library(downloader)
library(rvest)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(fs)
library(data.table)
library(zoo)
library(ggnetwork)
library(rhandsontable)
library(visNetwork)
library(proxy)
library(phangorn)

schemes <- c("Acinetobacter_baumanii", "Bacillus_anthracis", "Bordetella_pertussis", 
             "Brucella_melitensis", "Brucella_spp", "Burkholderia_mallei_FLI", 
             "Burkholderia_mallei_RKI", "Burkholderia_pseudomallei", "Campylobacter_jejuni_coli", 
             "Clostridioides_difficile", "Clostridium_perfringens", "Corynebacterium_diphtheriae",
             "Cronobacter_sakazakii_malonaticus", "Enterococcus_faecalis", "Enterococcus_faecium", 
             "Escherichia_coli", "Francisella_tularensis", "Klebsiella_pneumoniae_sensu_lato", 
             "Legionella_pneumophila", "Listeria_monocytogenes", "Mycobacterium_tuberculosis_complex", 
             "Mycobacteroides_abscessus", "Mycoplasma_gallisepticum", "Paenibacillus_larvae",
             "Pseudomonas_aeruginosa", "Salmonella_enterica", "Serratia_marcescens", 
             "Staphylococcus_aureus", "Staphylococcus_capitis", "Streptococcus_pyogenes"
             )

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
  
  title = "PhyloTrace 1.1.0",
  
  # Title
  dashboardHeader(title = span(
    div(
      class = "img_logo",
      img(
        src = "PhyloTrace.jpg", width = 190
      )
    )
  ),
  disable = FALSE),
  
  ## Sidebar ----
  dashboardSidebar(
    tags$style(includeCSS("www/mycss.css")),
    tags$style(HTML(
      "@keyframes pulsate {
        0% { transform: scale(1); }
        50% { transform: scale(1.1); }
        100% { transform: scale(1); }
      }
      .pulsating-button {
        animation: pulsate 1s ease infinite;
      }
      .pulsating-button:hover {
        animation: none;
      }")),
    br(), br(),
    uiOutput("loaded_scheme"),
    sidebarMenu(
      id = "tabs",
      uiOutput("menu_sep1"),
      sidebarMenuOutput("menu"),
      uiOutput("menu_sep2"),
      conditionalPanel(
        "input.tabs==='db_browse_entries'",
        uiOutput("entrytable_sidebar")
      ),
      conditionalPanel(
        "input.tabs==='db_distmatrix'",
        uiOutput("distmatrix_sidebar")
      ),
      conditionalPanel(
        "input.tabs==='db_missing_values'",
        column(
          width = 12,
          align = "center",
          br(), br(),
          uiOutput("missing_values_sidebar")
        )
      ),
      conditionalPanel(
        "input.tabs==='typing'",
        uiOutput("typing_sidebar")
      ),
      conditionalPanel(
        "input.tabs=='visualization'",
        uiOutput("visualization_sidebar")
      )
    )
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    
    shinyDashboardThemeDIY(
      ### general
      appFontFamily = "Liberation Sans"
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
      sidebarTabBorderWidthHover = 0
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
      textboxBorderColor = "#ffffff"
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
    uiOutput("start_message"),
    uiOutput("start_message_no_db"),
    
    tabItems(
      
      ## Tab Database ----
      
      ### Tab Browse Entries ----
      
      tabItem(
        tabName = "db_browse_entries",
        fluidRow(
          column(
            width = 3,
            align = "center",
            h2(p("Browse Local Database"), style = "color:white")
          )
        ),
        hr(), br(),
        br(),
        br(),
        uiOutput("no_scheme_entries"),
        uiOutput("db_no_entries"),
        uiOutput("entry_table_controls"),
        br(), br(),
        fluidRow(
          column(
            width = 8,
            uiOutput("db_entries_table")
          ),
          column(
            width = 3,
            align = "left",
            uiOutput("delete_box"),
            uiOutput("compare_allele_box"),
            uiOutput("download_entries")
          )
        ),
        br()
      ),
      
      ### Tab Scheme Info  ----  
      
      tabItem(
        tabName = "db_schemeinfo",
        fluidRow(
          column(
            width = 3,
            align = "center",
            h2(p("Scheme Info"), style = "color:white")
          )
        ),
        hr(), br(), br(), br(),
        uiOutput("no_scheme_info"),
        fluidRow(
          column(
            width = 5,
            align = "center",
            fluidRow(
              column(
                width = 7,
                align = "right",
                uiOutput("scheme_header")
              ),
              column(
                width = 2,
                align = "left",
                uiOutput("download_scheme_info")
              )
            ),
            br(),
            br(),
            uiOutput("scheme_info")
          ),
          column(
            width = 7,
            align = "center",
            fluidRow(
              column(
                width = 6,
                align = "right",
                uiOutput("loci_header")
              ),
              column(
                width = 2,
                align = "left",
                uiOutput("download_loci")
              )
            ),
            br(),
            div(class = "test",
                dataTableOutput("db_loci"))
          )
        )
      ),
      
      ### Tab Distance Matrix  ----  
      
      tabItem(
        tabName = "db_distmatrix",
        fluidRow(
          column(
            width = 3,
            align = "center",
            h2(p("Distance Matrix"), style = "color:white")
          )
        ),
        hr(), br(), br(), br(),
        uiOutput("no_scheme_distancematrix"),
        uiOutput("distancematrix_no_entries"),
        fluidRow(
          column(1),
          uiOutput("distmatrix_show")
        ),
        br(), br()
      ),
      
      ### Tab Missing Values ----
      
      tabItem(
        tabName = "db_missing_values",
        fluidRow(
          column(
            width = 3,
            align = "center",
            h2(p("Missing Values"), style = "color:white")
          )
        ),
        hr(), br(), br(), br(),
        fluidRow(
          column(
            width = 3,
            uiOutput("missing_values")
          ),
          column(
            width = 8,
            rHandsontableOutput("table_missing_values")
          )
        )
      ),
      
      ## Tab Add Scheme  ----  
      
      tabItem(
        tabName = "init",
        fluidRow(
          column(
            width = 3,
            align = "center",
            h2(p("Select cgMLST Scheme"), style = "color:white")
          )
        ),
        hr(),
        fluidRow(
          column(
            width = 3,
            align = "center",
            br(),
            br(),
            br(),
            pickerInput(
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
              width = "300px",
              options = list(
                `live-search` = TRUE,
                `actions-box` = TRUE,
                size = 10,
                style = "background-color: white; border-radius: 5px;"
              ),
              multiple = FALSE
            )
          ),
          column(
            width = 2,
            align = "center",
            br(),
            br(),
            br(),
            actionButton(
              "download_cgMLST",
              label = "Download",
              icon = icon("download")
            )
          ),
          column(
            width = 7,
            br(),
            br(),
            br(),
            align = "center",
            conditionalPanel(
              "input.download_cgMLST >= 1",
              h4(p("Downloaded Loci"), style = "color:white")
            )
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
          column(
            width = 7,
            align = "center",
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
          h2(p("Generate Allelic Profile"), style = "color:white")
        )),
        hr(),
        uiOutput("typing_no_db"),
        conditionalPanel(
          "input.typing_mode == 'Single'",
          fluidRow(
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
          fluidRow(
            uiOutput("initiate_multi_typing_ui"),
            uiOutput("test_yes_pending"),
            uiOutput("multi_stop"),
            column(1),
            uiOutput("metadata_multi_box"),
            column(1),
            uiOutput("start_multi_typing_ui")
          )
        )
      ),
      
      
      ## Tab Visualization -------------------------------------------------------
      
      
      tabItem(
        tabName = "visualization",
        
        fluidRow(
          tags$script(src = "javascript_functions.js"),
          column(
            width = 12,
            align = "center",
            br(),
            conditionalPanel(
              "input.tree_algo=='Minimum-Spanning'",
              uiOutput("mst_field")
            ),
            conditionalPanel(
              "input.tree_algo=='Neighbour-Joining'",
              uiOutput("nj_field")
            ),
            conditionalPanel(
              "input.tree_algo=='UPGMA'",
              uiOutput("upgma_field")
            )
          )
        ),
        br(),
        hr(),
        
        ### Control panels MST ----
        conditionalPanel(
          "input.tree_algo=='Minimum-Spanning'",
          fluidRow(
            column(
              width = 4,
              align = "center",
              box(
                solidHeader = TRUE,
                status = "primary",
                width = "100%",
                h3(p("Layout"), style = "color:white"),
                hr(),
                fluidRow(
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Title"), style = "color:white; position: relative; right: -15px"),
                        column(
                          width = 12,
                          align = "center",
                          textInput(
                            "mst_title",
                            label = "",
                            width = "100%",
                            placeholder = "Plot Title"
                          ),
                          fluidRow(
                            column(
                              width = 7,
                              colorPickr(
                                inputId = "mst_title_color",
                                selected = "#000000",
                                label = "",
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 5,
                              dropMenu(
                                actionBttn(
                                  "mst_title_menu",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "top-start",
                                theme = "translucent",
                                numericInput(
                                  "mst_title_size",
                                  label = h5("Size", style = "color:white; margin-bottom: 0px;"),
                                  value = 30,
                                  min = 15,
                                  max = 40,
                                  step = 1,
                                  width = "80px"
                                )
                              )
                            )
                          ),
                          br()
                        )
                      )
                    )
                  ),
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Subtitle"), style = "color:white; position: relative; right: -15px"),
                        column(
                          width = 12,
                          align = "center",
                          textInput(
                            "mst_subtitle",
                            label = "",
                            width = "100%",
                            placeholder = "Plot Subtitle"
                          ),
                          fluidRow(
                            column(
                              width = 7,
                              colorPickr(
                                inputId = "mst_subtitle_color",
                                selected = "#000000",
                                label = "",
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 5,
                              dropMenu(
                                actionBttn(
                                  "mst_subtitle_menu",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "top-start",
                                theme = "translucent",
                                numericInput(
                                  "mst_subtitle_size",
                                  label = h5("Size", style = "color:white; margin-bottom: 0px;"),
                                  value = 30,
                                  min = 15,
                                  max = 40,
                                  step = 1,
                                  width = "80px"
                                )
                              )
                            )
                          ),
                          br()
                        )
                      )
                    )
                  )
                ),
                hr(),
                fluidRow(
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Footer"), style = "color:white; position: relative; right: -15px"),
                        column(
                          width = 12,
                          align = "center",
                          textInput(
                            "mst_footer",
                            label = "",
                            width = "100%",
                            placeholder = "Plot Footer"
                          ),
                          fluidRow(
                            column(
                              width = 7,
                              colorPickr(
                                inputId = "mst_footer_color",
                                selected = "#000000",
                                label = "",
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 5,
                              dropMenu(
                                actionBttn(
                                  "mst_footer_menu",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "top-start",
                                theme = "translucent",
                                numericInput(
                                  "mst_footer_size",
                                  label = h5("Size", style = "color:white; margin-bottom: 0px;"),
                                  value = 15,
                                  min = 10,
                                  max = 30,
                                  step = 1,
                                  width = "80px"
                                )
                              )
                            )
                          ),
                          br()
                        )
                      )
                    )
                  ),
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Background"), style = "color:white; position: relative; right: -15px"),
                        column(
                          width = 12,
                          align = "center",
                          fluidRow(
                            column(
                              width = 3,
                              div(
                                class = "checkbox_bg",
                                checkboxInput(
                                  "mst_background_transparent",
                                  "Transparent",
                                  value = FALSE
                                )
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 7,
                              colorPickr(
                                inputId = "mst_background_color",
                                width = "100%",
                                selected = "#ffffff",
                                label = "",
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start"
                              )
                            )
                          ),
                          br()
                        )
                      )
                    )
                  )
                ),
                br()
              )
            ),
            column(
              width = 4,
              align = "center",
              box(
                solidHeader = TRUE,
                status = "primary",
                width = "100%",
                h3(p("Nodes"), style = "color:white"),
                hr(),
                fluidRow(
                  column(
                    width = 6,
                    column(
                      width = 12,
                      align = "left",
                      h4(p("Label"), style = "color:white;")
                    ),
                    column(
                      width = 12,
                      align = "center",
                      div(
                        class = "label_sel",
                        uiOutput("mst_node_label")
                      ),
                      fluidRow(
                        column(
                          width = 7,
                          colorPickr(
                            inputId = "node_font_color",
                            width = "100%",
                            selected = "#000000",
                            label = "",
                            update = "changestop",
                            interaction = list(clear = FALSE,
                                               save = FALSE),
                            position = "right-start"
                          )
                        ),
                        column(
                          width = 5,
                          dropMenu(
                            actionBttn(
                              "mst_label_menu",
                              label = "",
                              color = "default",
                              size = "sm",
                              style = "material-flat",
                              icon = icon("sliders")
                            ),
                            placement = "top-start",
                            theme = "translucent",
                            numericInput(
                              "node_label_fontsize",
                              label = h5("Size", style = "color:white; margin-bottom: 0px;"),
                              value = 14,
                              min = 8,
                              max = 30,
                              step = 1,
                              width = "80px"
                            )
                          )
                        )
                      )
                    )
                  ),
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Color"), style = "color:white; position: relative; right: -15px"),
                        column(
                          width = 12,
                          align = "center",
                          fluidRow(
                            column(
                              width = 12,
                              align = "left",
                              div(
                                class = "checkbox_bg",
                                checkboxInput(
                                  "mst_color_var",
                                  h5(p("Add variable"), style = "color:white; position: relative; bottom: 6px;"),
                                  value = FALSE
                                )
                              )
                            )
                          ),
                          conditionalPanel(
                            "input.mst_color_var==false",
                            fluidRow(
                              column(
                                width = 7,
                                div(
                                  class = "node_color",
                                  colorPickr(
                                    inputId = "mst_color_node",
                                    width = "100%",
                                    selected = "#B2FACA",
                                    label = "",
                                    update = "changestop",
                                    interaction = list(clear = FALSE,
                                                       save = FALSE),
                                    position = "right-start"
                                  )
                                )
                              ),
                              column(
                                width = 5,
                                dropMenu(
                                  actionBttn(
                                    "mst_node_menu",
                                    label = "",
                                    color = "default",
                                    size = "sm",
                                    style = "material-flat",
                                    icon = icon("sliders")
                                  ),
                                  placement = "top-start",
                                  theme = "translucent",
                                  width = 5,
                                  numericInput(
                                    "node_opacity",
                                    label = h5("Opacity", style = "color:white; margin-bottom: 0px;"),
                                    value = 1,
                                    step = 0.1,
                                    min = 0,
                                    max = 1,
                                    width = "80px"
                                  )
                                )
                              )
                            )
                          ),
                          conditionalPanel(
                            "input.mst_color_var==true",
                            fluidRow(
                              column(
                                width = 12,
                                div(
                                  class = "label_sel",
                                  selectInput(
                                    "mst_col_var",
                                    label = "",
                                    choices = c(
                                      "Host", "Country", "City", "Isolation Date", "Duplicates"
                                    ),
                                    selected = c("Host"),
                                    width = "100%"
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    ), br()
                  )
                ),
                hr(),
                fluidRow(
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Size"), style = "color:white; position: relative; right: -15px"),
                        column(
                          width = 12,
                          align = "center",
                          fluidRow(
                            column(
                              width = 12,
                              align = "left",
                              div(
                                class = "checkbox_bg",
                                checkboxInput(
                                  "scale_nodes",
                                  "Scale by duplicates",
                                  value = TRUE
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    column(
                      width = 12,
                      align = "center",
                      br(),
                      conditionalPanel(
                        "input.scale_nodes==true",
                        div(
                          class = "slider",
                          sliderTextInput(
                            "mst_node_scale",
                            label = NULL,
                            choices = 1:80,
                            selected = c(20, 40),
                            hide_min_max = TRUE
                          )
                        )
                      ),
                      conditionalPanel(
                        "input.scale_nodes==false",
                        div(
                          class = "slider",
                          sliderTextInput(
                            inputId = "mst_node_size",
                            label = NULL,
                            choices = 1:100,
                            selected = c(30),
                            hide_min_max = TRUE
                          ) 
                        )
                      ),
                      br(), br()
                    )
                  ),
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Other Elements"), style = "color:white; position: relative; right: -15px"),
                        column(
                          width = 12,
                          align = "center",
                          fluidRow(
                            column(
                              width = 12,
                              align = "left",
                              div(
                                class = "checkbox_bg",
                                checkboxInput(
                                  "mst_shadow",
                                  "Show shadow",
                                  value = TRUE
                                )
                              ),
                              fluidRow(
                                column(
                                  width = 3,
                                  align = "left",
                                  HTML(
                                    paste(
                                      tags$span(style='color: white; font-size: 14px; position: relative; bottom: -28px; margin-left: 0px ', 'Shape')
                                    )
                                  )
                                ),
                                column(
                                  width = 9,
                                  align = "center",
                                  selectInput(
                                    "mst_node_shape",
                                    "",
                                    choices = list(`Label inside` = c("Circle" = "circle", "Box" = "box", "Text" = "text"),
                                                   `Label outside` = c("Diamond" = "diamond", "Hexagon" = "hexagon","Dot" = "dot", "Square" = "square")),
                                    selected = c("Dot" = "dot"),
                                    width = "85%"
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
              )
            ),
            column(
              width = 4,
              align = "center",
              box(
                solidHeader = TRUE,
                status = "primary",
                width = "100%",
                h3(p("Edges"), style = "color:white"),
                hr(),
                fluidRow(
                  column(
                    width = 6,
                    column(
                      width = 12,
                      align = "left",
                      h4(p("Label"), style = "color:white;")
                    ),
                    column(
                      width = 12,
                      align = "center",
                      div(
                        class = "label_sel",
                        selectInput(
                          "mst_edge_label",
                          label = "",
                          choices = c(
                            `Allelic Distance` = "weight",
                            Index = "index",
                            `Assembly ID` = "assembly_id",
                            `Assembly Name` = "assembly_name",
                            `Isolation Date` = "isolation_date",
                            Host = "host",
                            Country = "country",
                            City = "city"
                          ),
                          selected = c(`Allelic Distance` = "weight"),
                          width = "100%"
                        )
                      ),
                      fluidRow(
                        column(
                          width = 7,
                          colorPickr(
                            inputId = "mst_edge_font_color",
                            width = "100%",
                            selected = "#000000",
                            label = "",
                            update = "changestop",
                            interaction = list(clear = FALSE,
                                               save = FALSE),
                            position = "right-start"
                          )
                        ),
                        column(
                          width = 5,
                          dropMenu(
                            actionBttn(
                              "mst_edgelabel_menu",
                              label = "",
                              color = "default",
                              size = "sm",
                              style = "material-flat",
                              icon = icon("sliders")
                            ),
                            placement = "top-start",
                            theme = "translucent",
                            width = 5,
                            numericInput(
                              "mst_edge_font_size",
                              label = h5("Size", style = "color:white; margin-bottom: 0px;"),
                              value = 18,
                              step = 1,
                              min = 8,
                              max = 30,
                              width = "80px"
                            )
                          )
                        )
                      ),
                      br()
                    )
                  ),
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Color"), style = "color:white; position: relative; right: -15px"),
                        column(
                          width = 12,
                          align = "center",
                          fluidRow(
                            column(
                              width = 7,
                              div(
                                class = "node_color",
                                colorPickr(
                                  inputId = "mst_color_edge",
                                  width = "100%",
                                  selected = "#000000",
                                  label = "",
                                  update = "changestop",
                                  interaction = list(clear = FALSE,
                                                     save = FALSE),
                                  position = "right-start"
                                )
                              )
                            ),
                            column(
                              width = 5,
                              dropMenu(
                                actionBttn(
                                  "mst_edgecolor_menu",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "top-start",
                                theme = "translucent",
                                width = 5,
                                numericInput(
                                  "mst_edge_opacity",
                                  label = h5("Opacity", style = "color:white; margin-bottom: 0px;"),
                                  value = 0.7,
                                  step = 0.1,
                                  min = 0,
                                  max = 1,
                                  width = "80px"
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                hr(style = "margin-top: 3px !important"),
                fluidRow(
                  column(
                    width = 12,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Size multiplier"), style = "color:white; position: relative; right: -15px; margin-bottom: -5px")
                      )
                    ),
                    column(
                      width = 6,
                      align = "left",
                      br(),
                      checkboxInput(
                        "mst_scale_edges",
                        "Scale edge length",
                        value = FALSE
                      ),
                      conditionalPanel(
                        "input.mst_scale_edges==true",
                        div(
                          class = "slider_edge",
                          sliderTextInput(
                            inputId = "mst_edge_length_scale",
                            label = NULL,
                            choices = 1:40,
                            selected = c(15),
                            hide_min_max = TRUE
                          ) 
                        )
                      ),
                      conditionalPanel(
                        "input.mst_scale_edges==false",
                        div(
                          class = "slider_edge",
                          sliderTextInput(
                            inputId = "mst_edge_length",
                            label = NULL,
                            choices = append(seq(0.1, 1, 0.1), 2:100),
                            selected = 35,
                            hide_min_max = TRUE
                          ) 
                        )
                      ),
                      br(), br()
                    )
                  )
                )
              ),
              br(), br(), br(), br(), br(), br(), br()
            )
          )
        ),
        
        ### Control Panels NJ ----
        
        conditionalPanel(
          "input.tree_algo=='Neighbour-Joining'",
          fluidRow(
            column(
              width = 4,
              align = "center",
              box(
                solidHeader = TRUE,
                status = "primary",
                width = "100%",
                h3(p("Layout"), style = "color:white"),
                hr(),
                fluidRow(
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Theme"), style = "color:white; position: relative; right: -15px"),
                        fluidRow(
                          column(
                            width = 12,
                            align = "center",
                            selectInput(
                              inputId = "nj_layout",
                              label = "",
                              choices = list(
                                Linear = list(
                                  "Rectangular" = "rectangular",
                                  "Roundrect" = "roundrect",
                                  "Slanted" = "slanted",
                                  "Ellipse" = "ellipse"
                                ),
                                Circular = list("Circular" = "circular",
                                                "Inward" = "inward")
                              ),
                              selected = "circular",
                              width = "90%"
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 5,
                            align = "left",
                            checkboxInput(
                              "nj_ladder",
                              h5(p("Ladder"), style = "color:white; position: relative; bottom: 3px; right: -15px; margin-top: 22px"),
                              value = TRUE
                            )
                          ),
                          column(
                            width = 3,
                            align = "left",
                            checkboxInput(
                              "rootedge_show",
                              h5(p("Root"), style = "color:white; position: relative; bottom: 3px; right: -5px; margin-top: 23px"),
                              value = FALSE
                            )
                          ),
                          column(
                            width = 2,
                            align = "right",
                            dropMenu(
                              actionBttn(
                                "nj_rootedge_menu",
                                label = "",
                                color = "default",
                                size = "sm",
                                style = "material-flat",
                                icon = icon("sliders")
                              ),
                              placement = "top-start",
                              theme = "translucent",
                              fluidRow(
                                column(
                                  width = 12,
                                  align = "center",
                                  numericInput(
                                    "nj_rootedge_length",
                                    label = h5("Length", style = "color:white; margin-bottom: 0px"),
                                    value = 3,
                                    min = 1,
                                    max = 30,
                                    step = 1,
                                    width = "80px"
                                  ),
                                  br(),
                                  selectInput(
                                    "nj_rootedge_line",
                                    label = h5("Linetype", style = "color:white"),
                                    choices = c(Solid = "solid", Dashed = "dashed", Dotted = "dotted"),
                                    selected = c(Dotted = "dotted"),
                                    width = "100px"
                                  )
                                )
                              )
                            )
                          )
                        ),
                        conditionalPanel(
                          "input.nj_layout=='circular' | input.nj_layout=='inward'",
                          fluidRow(
                            column(
                              width = 3,
                              h5(p("Adjust"), style = "color:white; position: relative; right: -15px; margin-top: 20px")
                            ),
                            column(
                              width = 8,
                              align = "right",
                              conditionalPanel(
                                "input.nj_layout=='circular'",
                                sliderTextInput(
                                  "nj_xlim",
                                  label = NULL,
                                  choices = -50:0,
                                  selected = -10,
                                  hide_min_max = TRUE
                                )
                              ),
                              conditionalPanel(
                                "input.nj_layout=='inward'",
                                sliderTextInput(
                                  "nj_inward_xlim",
                                  label = NULL,
                                  choices = 30:120,
                                  selected = 50,
                                  hide_min_max = TRUE
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Color"), style = "color:white; position: relative; right: -15px"),
                        fluidRow(
                          column(
                            width = 5,
                            h5(p("Lines/Text"), style = "color:white; position: relative; right: -15px; margin-top: 30px")
                          ),
                          column(
                            width = 7,
                            colorPickr(
                              inputId = "nj_color",
                              width = "90%",
                              selected = "#000000",
                              label = "",
                              update = "changestop",
                              interaction = list(clear = FALSE,
                                                 save = FALSE),
                              position = "right-start"
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 5,
                            h5(p("Background"), style = "color:white; position: relative; right: -15px; margin-top: 30px")
                          ),
                          column(
                            width = 7,
                            colorPickr(
                              inputId = "nj_bg",
                              width = "90%",
                              selected = "#ffffff",
                              label = "",
                              update = "changestop",
                              interaction = list(clear = FALSE,
                                                 save = FALSE),
                              position = "right-start"
                            )
                          )
                        )
                      )
                    ), br()
                  )
                ),
                hr(),
                fluidRow(
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Title"), style = "color:white; position: relative; right: -15px"),
                        column(
                          width = 12,
                          align = "center",
                          textInput(
                            "nj_title",
                            label = "",
                            width = "100%",
                            placeholder = "Plot Title"
                          ),
                          fluidRow(
                            column(
                              width = 7,
                              colorPickr(
                                inputId = "nj_title_color",
                                selected = "#000000",
                                label = "",
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 5,
                              align = "right",
                              dropMenu(
                                actionBttn(
                                  "nj_title_menu",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "top-start",
                                theme = "translucent",
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "nj_title_size",
                                      label = h5("Size", style = "color:white; margin-bottom: 0px"),
                                      value = 30,
                                      min = 15,
                                      max = 40,
                                      step = 1,
                                      width = "80px"
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          br()
                        )
                      )
                    )
                  ),
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Subtitle"), style = "color:white; position: relative; right: -15px"),
                        column(
                          width = 12,
                          align = "center",
                          textInput(
                            "nj_subtitle",
                            label = "",
                            width = "100%",
                            placeholder = "Plot Subtitle"
                          ),
                          fluidRow(
                            column(
                              width = 7,
                              colorPickr(
                                inputId = "nj_subtitle_color",
                                selected = "#000000",
                                label = "",
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 5,
                              align = "right",
                              dropMenu(
                                actionBttn(
                                  "nj_subtitle_menu",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "top-start",
                                theme = "translucent",
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "nj_subtitle_size",
                                      label = h5("Size", style = "color:white; margin-bottom: 0px"),
                                      value = 20,
                                      min = 15,
                                      max = 40,
                                      step = 1,
                                      width = "80px"
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          br()
                        )
                      )
                    )
                  )
                ),
                hr(),
                fluidRow(
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Tree scale"), style = "color:white; position: relative; right: -15px"),
                        column(
                          width = 12,
                          fluidRow(
                            column(
                              width = 7,
                              align = "left",
                              checkboxInput(
                                "nj_treescale_show",
                                h5(p("Show "), style = "color:white; position: relative; bottom: -7px; right: 0px"),
                                value = TRUE
                              ),
                              br()
                            ),
                            column(
                              width = 5,
                              align = "right",
                              dropMenu(
                                actionBttn(
                                  "nj_treescale_menu",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "top-start",
                                theme = "translucent",
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "nj_treescale_width",
                                      label = h5("Length", style = "color:white; margin-bottom: 0px"),
                                      value = 3,
                                      min = 1,
                                      max = 20,
                                      step = 1,
                                      width = "80px"
                                    ),
                                    br(),
                                    sliderTextInput(
                                      "nj_treescale_x",
                                      label = h5("X Position", style = "color:white; margin-bottom: 0px"),
                                      choices = 0:40,
                                      selected = 5,
                                      hide_min_max = TRUE,
                                      width = "150px"
                                    ),
                                    br(),
                                    sliderTextInput(
                                      "nj_treescale_y",
                                      label = h5("Y Position", style = "color:white; margin-bottom: 0px"),
                                      choices = 0:65,
                                      selected = 0,
                                      hide_min_max = TRUE
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
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Legend"), style = "color:white; position: relative; right: -15px"),
                        column(
                          width = 12,
                          align = "center",
                          fluidRow(
                            column(
                              width = 7,
                              align = "left",
                              prettyRadioButtons(
                                "nj_legend_orientation",
                                "",
                                choices = c(Horizontal = "horizontal",
                                            Vertical = "vertical"),
                                selected = c(Horizontal = "horizontal"),
                                inline = FALSE
                              )
                            ),
                            column(
                              width = 5,
                              align = "right",
                              dropMenu(
                                actionBttn(
                                  "nj_legend_menu",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "top-start",
                                theme = "translucent",
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "nj_legend_size",
                                      label = h5("Size", style = "color:white; margin-bottom: 0px"),
                                      value = 10,
                                      min = 5,
                                      max = 25,
                                      step = 1,
                                      width = "80px"
                                    ),
                                    numericInput(
                                      "nj_legend_x",
                                      label = h5("X Pos", style = "color:white; margin-bottom: 0px"),
                                      value = 0.1,
                                      min = -0.9,
                                      max = 1.9,
                                      step = 0.2,
                                      width = "80px"
                                    ),
                                    numericInput(
                                      "nj_legend_y",
                                      label = h5("Y Pos", style = "color:white; margin-bottom: 0px"),
                                      value = 1,
                                      min = -1,
                                      max = 1,
                                      step = 0.1,
                                      width = "80px"
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
                )
              )
            ),
            column(
              width = 4,
              align = "center",
              box(
                solidHeader = TRUE,
                status = "primary",
                width = "100%",
                h3(p("Label"), style = "color:white"),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    align = "left",
                    h4(p("Tips"), style = "color:white; position: relative; right: -15px"),
                    fluidRow(
                      column(
                        width = 4,
                        align = "left",
                        checkboxInput(
                          "nj_tiplab_show",
                          h5(p("Show"), style = "color:white; position: relative; right: -17px; bottom: -7px"),
                          value = TRUE
                        )
                      ),
                      column(
                        width = 4,
                        align = "center",
                        uiOutput("nj_tiplab")
                      ),
                      column(
                        width = 3,
                        align = "right",
                        dropMenu(
                          actionBttn(
                            "nj_labeltext_menu",
                            label = "",
                            color = "default",
                            size = "sm",
                            style = "material-flat",
                            icon = icon("sliders")
                          ),
                          placement = "top-start",
                          theme = "translucent",
                          fluidRow(
                            column(
                              width = 6,
                              align = "center",
                              numericInput(
                                "tiplab_size",
                                label = h5("Label size", style = "color:white; margin-bottom: 0px"),
                                min = 1,
                                max = 10,
                                step = 0.5,
                                value = 4,
                                width = "80px"
                              ),
                              br(),
                              numericInput(
                                "nj_tiplab_alpha",
                                label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                min = 0.1,
                                max = 1,
                                value = 1,
                                width = "80px"
                              ),
                              br(),
                              selectInput(
                                "nj_tiplab_fontface",
                                label = h5("Fontface", style = "color:white; margin-bottom: 0px"),
                                width = "250px",
                                choices = c(Plain = "plain", Bold =  "bold", Italic =  "italic", `B & I` = "bold.italic")
                              )
                            ),
                            column(
                              width = 6,
                              align = "center",
                              conditionalPanel(
                                "!(input.nj_layout=='inward'|input.nj_layout=='circular')",
                                numericInput(
                                  inputId = "nj_tiplab_nudge_x",
                                  label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                  min = -3,
                                  max = 3,
                                  step = 0.05,
                                  value = 0,
                                  width = "80px"
                                )
                              ),
                              conditionalPanel(
                                "input.nj_layout=='circular'",
                                numericInput(
                                  inputId = "nj_tiplab_position",
                                  label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                  min = -3,
                                  max = 3,
                                  step = 0.05,
                                  value = -0.1,
                                  width = "80px"
                                )
                              ),
                              conditionalPanel(
                                "input.nj_layout=='inward'",
                                numericInput(
                                  inputId = "nj_tiplab_position_inw",
                                  label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                  min = -3,
                                  max = 3,
                                  step = 0.05,
                                  value = 1.1,
                                  width = "80px"
                                )
                              ),
                              br(),
                              sliderTextInput(
                                inputId = "nj_tiplab_angle",
                                label = h5("Angle", style = "color:white; margin-bottom: 0px"),
                                choices = -180:180,
                                selected = 0,
                                hide_min_max = TRUE
                              )      
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 4,
                    align = "left",
                    checkboxInput(
                      "nj_align",
                      h5(p("Align labels"), style = "color:white; position: relative; bottom: -10px; right: -15px"),
                      value = FALSE
                    )
                  ),
                  column(
                    width = 1,
                    HTML(
                      paste(
                        tags$span(style='color: white; font-size: 14px; position: relative; bottom: -28px; margin-left: 0px ', 'Width')
                      )
                    )
                  ),
                  column(
                    width = 3,
                    align = "left",
                    numericInput(
                      "nj_tiplab_linesize",
                      "",
                      value = 0.5,
                      min = 0.1,
                      max = 3,
                      step = 0.1,
                      width = "80px"
                    )
                  ),
                  column(
                    width = 3,
                    selectInput(
                      "nj_tiplab_linetype",
                      "",
                      choices = c(Solid = "solid", Dashed = "dashed", Dotted = "dotted"),
                      selected = c(Dotted = "dotted")
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 4,
                    align = "left",
                    h5(p("Color"), style = "color:white; position: relative; right: -14px; margin-top: 30px")
                  ),
                  column(
                    width = 4,
                    conditionalPanel(
                      "input.nj_mapping_show==false",
                      colorPickr(
                        inputId = "nj_tiplab_color",
                        width = "100%",
                        selected = "#000000",
                        label = "",
                        update = "changestop",
                        interaction = list(clear = FALSE,
                                           save = FALSE),
                        position = "right-start"
                      )
                    ),
                    conditionalPanel(
                      "input.nj_mapping_show==true",
                      uiOutput("nj_color_mapping")
                    )
                  ),
                  column(
                    width = 4,
                    align = "left",
                    checkboxInput(
                      "nj_mapping_show",
                      label = h5("Add variable", style = "color:white; font-size: 14px; position: relative; bottom: -10px;"),
                      value = FALSE
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 4,
                    align = "left",
                    br(),
                    checkboxInput(
                      "nj_geom",
                      h5(p("Panel"), style = "color:white; position: relative; bottom: 10px; right: -17px"),
                      value = FALSE
                    )
                  ),
                  column(
                    width = 4,
                    colorPickr(
                      inputId = "nj_tiplab_fill",
                      width = "100%",
                      selected = "#84D9A0",
                      label = "",
                      update = "changestop",
                      interaction = list(clear = FALSE,
                                         save = FALSE),
                      position = "right-start"
                    )
                  ),
                  column(
                    width = 3,
                    align = "right",
                    dropMenu(
                      actionBttn(
                        "nj_labelformat_menu",
                        label = "",
                        color = "default",
                        size = "sm",
                        style = "material-flat",
                        icon = icon("sliders")
                      ),
                      placement = "top-start",
                      theme = "translucent",
                      fluidRow(
                        column(
                          width = 12,
                          align = "center",
                          numericInput(
                            inputId = "nj_tiplab_padding",
                            label = h5("Size", style = "color:white; margin-bottom: 0px"),
                            min = 0.1,
                            step = 0.1,
                            max = 1,
                            value = 0.3,
                            width = "80px"
                          ),
                          br(),
                          numericInput(
                            inputId = "nj_tiplab_labelradius",
                            label = h5("Smooth edge", style = "color:white; margin-bottom: 0px"),
                            min = 0,
                            max = 0.5,
                            step = 0.05,
                            value = 0.2,
                            width = "80px"
                          )
                        )
                      )
                    )
                  )
                ),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    align = "left",
                    h4(p("Branches"), style = "color:white; position: relative; right: -15px"),
                    fluidRow(
                      column(
                        width = 4,
                        align = "left",
                        checkboxInput(
                          "nj_show_branch_label",
                          h5(p("Show"), style = "color:white; position: relative; bottom: -7px; right: -17px"),
                          value = FALSE
                        )
                      ),
                      column(
                        width = 4,
                        align = "center",
                        uiOutput("nj_branch_label")
                      ),
                      column(
                        width = 3,
                        align = "right",
                        dropMenu(
                          actionBttn(
                            "nj_branch_label_menu",
                            label = "",
                            color = "default",
                            size = "sm",
                            style = "material-flat",
                            icon = icon("sliders")
                          ),
                          placement = "top-start",
                          theme = "translucent",
                          fluidRow(
                            column(
                              width = 6,
                              align = "center",
                              numericInput(
                                "nj_branch_size",
                                label = h5("Size", style = "color:white; margin-bottom: 0px"),
                                min = 3,
                                max = 10,
                                step = 0.5,
                                value = 4,
                                width = "80px"
                              ),
                              br(),
                              numericInput(
                                "nj_branch_labelradius",
                                label = h5("Smooth edge", style = "color:white; margin-bottom: 0px"),
                                value = 0.5,
                                step = 0.05,
                                min = 0,
                                max = 0.5,
                                width = "80px"
                              ),
                              br(),
                              selectInput(
                                "nj_branchlab_fontface",
                                label = h5("Fontface", style = "color:white"),
                                width = "250px",
                                choices = c(Plain = "plain", Bold =  "bold", Italic =  "italic", `B & I` = "bold.italic")
                              )
                            ),
                            column(
                              width = 6,
                              align = "center",
                              numericInput(
                                "nj_branchlab_alpha",
                                label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                min = 0.1,
                                max = 1,
                                value = 0.7,
                                width = "80px"
                              ),
                              br(),
                              sliderTextInput(
                                inputId = "nj_branch_x",
                                label = h5("X Position", style = "color:white; margin-bottom: 0px"),
                                choices = seq(-3, 3, by = 0.1),
                                selected = 0,
                                width = "250px",
                                hide_min_max = TRUE
                              ),
                              br(),
                              sliderTextInput(
                                inputId = "nj_branch_y",
                                label = h5("Y Position", style = "color:white; margin-bottom: 0px"),
                                choices = seq(-3, 3, by = 0.1),
                                selected = 0,
                                width = "250px",
                                hide_min_max = TRUE
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 4,
                    align = "left",
                    h5(p("Color"), style = "color:white; position: relative; right: -14px; margin-top: 30px")
                  ),
                  column(
                    width = 4,
                    colorPickr(
                      inputId = "nj_branch_label_color",
                      width = "100%",
                      selected = "#FFB7B7",
                      label = "",
                      update = "changestop",
                      interaction = list(clear = FALSE,
                                         save = FALSE),
                      position = "right-start"
                    )
                  )
                ),
                br(), br(), br(), 
                conditionalPanel(
                  "input.nj_layout=='inward'|input.nj_layout=='circular'",
                  br(), br()
                )
              )
            ),
            column(
              width = 4,
              align = "center",
              box(
                solidHeader = TRUE,
                status = "primary",
                width = "100%",
                h3(p("Elements"), style = "color:white"),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    align = "left",
                    h4(p("Tip points"), style = "color:white; position: relative; right: -15px"),
                    fluidRow(
                      column(
                        width = 4,
                        align = "left",
                        checkboxInput(
                          "nj_tippoint_show",
                          h5(p("Show"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                          value = FALSE
                        )
                      ),
                      column(
                        width = 2,
                        align = "left",
                        dropMenu(
                          actionBttn(
                            "nj_tippoint_menu",
                            label = "",
                            color = "default",
                            size = "sm",
                            style = "material-flat",
                            icon = icon("sliders")
                          ),
                          placement = "top-start",
                          theme = "translucent",
                          fluidRow(
                            column(
                              width = 12,
                              align = "center",
                              numericInput(
                                "nj_tippoint_alpha",
                                label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                value = 0.5,
                                min = 0.1,
                                max = 1,
                                step = 0.1,
                                width = "80px"
                              ), 
                              br(),
                              numericInput(
                                inputId = "nj_tippoint_size",
                                label = h5("Size", style = "color:white; margin-bottom: 0px"),
                                min = 1,
                                max = 20,
                                step = 1,
                                value = 5,
                                width = "80px"
                              )
                            )
                          )
                        )  
                      )
                    ),
                    fluidRow(
                      column(
                        width = 4,
                        align = "left",
                        h5(p("Color"), style = "color:white; position: relative; right: -15px; margin-top: 30px")
                      ),
                      column(
                        width = 4,
                        align = "center",
                        conditionalPanel(
                          "input.nj_tipcolor_mapping_show==true & input.nj_mapping_show==false",
                          uiOutput("nj_tipcolor_mapping")
                        ),
                        conditionalPanel(
                          "input.nj_tipcolor_mapping_show==false || input.nj_mapping_show==true",
                          colorPickr(
                            inputId = "nj_tippoint_color",
                            width = "100%",
                            selected = "#3A4657",
                            label = "",
                            update = "changestop",
                            interaction = list(clear = FALSE,
                                               save = FALSE),
                            position = "right-start"
                          )
                        )
                      ),
                      column(
                        width = 4,
                        align = "left",
                        conditionalPanel(
                          "input.nj_mapping_show==false",
                          checkboxInput(
                            "nj_tipcolor_mapping_show",
                            label = h5("Add variable", style = "color:white; font-size: 14px; position: relative; bottom: -10px;"),
                            value = FALSE
                          )
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 4,
                        align = "left",
                        h5(p("Shape"), style = "color:white; position: relative; right: -17px; margin-top: 30px")
                      ),
                      column(
                        width = 4,
                        align = "center",
                        conditionalPanel(
                          "input.nj_tipshape_mapping_show==false",
                          selectInput(
                            "nj_tippoint_shape",
                            "",
                            width = "100%",
                            choices = c(
                              Circle = "circle", 
                              Square = "square", 
                              Diamond = "diamond", 
                              Triangle = "triangle",
                              Cross = "cross", 
                              Asterisk = "asterisk"
                            )
                          )
                        ),
                        conditionalPanel(
                          "input.nj_tipshape_mapping_show==true",
                          uiOutput("nj_tipshape_mapping")
                        )
                      ),
                      column(
                        width = 4,
                        align = "left",
                        checkboxInput(
                          "nj_tipshape_mapping_show",
                          label = h5("Add variable", style = "color:white; font-size: 14px; position: relative; bottom: -10px;"),
                          value = FALSE
                        )
                      )
                    )
                  )
                ),
                hr(),
                fluidRow(
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Node points"), style = "color:white; position: relative; right: -15px"),
                        fluidRow(
                          column(
                            width = 5,
                            align = "left",
                            checkboxInput(
                              "nj_nodepoint_show",
                              h5(p("Show"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                              value = FALSE
                            )
                          ),
                          column(
                            width = 7,
                            align = "right",
                            dropMenu(
                              actionBttn(
                                "nj_nodepoint_menu",
                                label = "",
                                color = "default",
                                size = "sm",
                                style = "material-flat",
                                icon = icon("sliders")
                              ),
                              placement = "top-start",
                              theme = "translucent",
                              fluidRow(
                                column(
                                  width = 12,
                                  align = "center",
                                  numericInput(
                                    "nj_nodepoint_alpha",
                                    label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                    value = 0.5,
                                    min = 0.1,
                                    max = 1,
                                    step = 0.1,
                                    width = "80px"
                                  ), 
                                  br(),
                                  numericInput(
                                    inputId = "nj_nodepoint_size",
                                    label = h5("Size", style = "color:white; margin-bottom: 0px"),
                                    min = 1,
                                    max = 20,
                                    step = 1,
                                    value = 5,
                                    width = "80px"
                                  )
                                )
                              )
                            )  
                          )
                        ),
                        fluidRow(
                          column(
                            width = 5,
                            h5(p("Color"), style = "color:white; position: relative; right: -15px; margin-top: 30px")
                          ),
                          column(
                            width = 7,
                            align = "center",
                            colorPickr(
                              inputId = "nj_nodepoint_color",
                              width = "100%",
                              selected = "#3A4657",
                              label = "",
                              update = "changestop",
                              interaction = list(clear = FALSE,
                                                 save = FALSE),
                              position = "right-start"
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 5,
                            h5(p("Shape"), style = "color:white; position: relative; right: -20px; margin-top: 30px")
                          ),
                          column(
                            width = 7,
                            align = "center",
                            selectInput(
                              "nj_nodepoint_shape",
                              "",
                              choices = c(
                                Circle = "circle", 
                                Square = "square", 
                                Diamond = "diamond", 
                                Triangle = "triangle",
                                Cross = "cross", 
                                Asterisk = "asterisk"
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        fluidRow(
                          column(
                            width = 6,
                            h4(p("Tiles"), style = "color:white; position: relative; right: -15px")
                          ),
                          column(
                            width = 6,
                            div(
                              class = "tile1",
                              selectInput(
                                "tile_num",
                                "",
                                choices = 1:5,
                                width = "70px"
                              )
                            )
                          )
                        ),
                        conditionalPanel(
                          "input.tile_num == 1",
                          fluidRow(
                            column(
                              width = 4,
                              align = "left",
                              checkboxInput(
                                "nj_tiles_show",
                                h5(p("Show"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                                value = FALSE
                              )
                            ),
                            column(
                              width = 7,
                              align = "right",
                              dropMenu(
                                actionBttn(
                                  "nj_tile_menu",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "left-start",
                                theme = "translucent",
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    uiOutput("nj_fruit_width")
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "nj_fruit_offset_circ",
                                      label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                      min = -3,
                                      max = 3,
                                      step = 0.05,
                                      value = 0,
                                      width = "80px"
                                    )
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "nj_fruit_alpha",
                                      label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                      min = 0.1,
                                      max = 1,
                                      step = 0.1,
                                      value = 1,
                                      width = "80px"
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 4,
                              align = "left",
                              HTML(
                                paste(
                                  tags$span(style='color: white; font-size: 14px; margin-left: 16px; position: relative; bottom: -28px', 'Variable')
                                )
                              )
                            ),
                            column(
                              width = 7,
                              align = "center",
                              uiOutput("nj_fruit_variable")
                            )
                          ),
                          fluidRow(
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "nj_tile_color_low",
                                selected = "#F53900",
                                label = h5(p("Low"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "nj_tile_color_mid",
                                selected = "#FFFFFF",
                                label = h5(p("Mid"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "nj_tile_color_high",
                                selected = "#68B127",
                                label = h5(p("High"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(
                              width = 6,
                              align = "left",
                              checkboxInput(
                                "nj_div_tiles",
                                h5(p("Diverging"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                              )
                            ),
                            column(
                              width = 6,
                              div(
                                class = "tile_select",
                                selectInput(
                                  "nj_tile_mid",
                                  h5(p("Midpoint"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                                  choices = c("Zero", "Mean", "Median"),
                                  width = "90px"
                                )
                              )
                            )
                          )
                        ),
                        conditionalPanel(
                          "input.tile_num == 2",
                          fluidRow(
                            column(
                              width = 4,
                              align = "left",
                              checkboxInput(
                                "nj_tiles_show_2",
                                h5(p("Show"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                                value = FALSE
                              )
                            ),
                            column(
                              width = 7,
                              align = "right",
                              dropMenu(
                                actionBttn(
                                  "nj_tile_menu_2",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "left-start",
                                theme = "translucent",
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    uiOutput("nj_fruit_width2")
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "nj_fruit_offset_circ_2",
                                      label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                      min = -3,
                                      max = 3,
                                      step = 0.05,
                                      value = 0,
                                      width = "80px"
                                    )
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "nj_fruit_alpha_2",
                                      label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                      min = 0.1,
                                      max = 1,
                                      step = 0.1,
                                      value = 1,
                                      width = "80px"
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 4,
                              align = "left",
                              HTML(
                                paste(
                                  tags$span(style='color: white; font-size: 14px; margin-left: 16px; position: relative; bottom: -28px', 'Variable')
                                )
                              )
                            ),
                            column(
                              width = 7,
                              align = "center",
                              uiOutput("nj_fruit_variable2")
                            )
                          ),
                          fluidRow(
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "nj_tile_color_low_2",
                                selected = "#F53900",
                                label = h5(p("Low"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "nj_tile_color_mid_2",
                                selected = "#FFFFFF",
                                label = h5(p("Mid"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "nj_tile_color_high_2",
                                selected = "#68B127",
                                label = h5(p("High"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(
                              width = 6,
                              align = "left",
                              checkboxInput(
                                "nj_div_tiles_2",
                                h5(p("Diverging"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                              )
                            ),
                            column(
                              width = 6,
                              div(
                                class = "tile_select",
                                selectInput(
                                  "nj_tile_mid_2",
                                  h5(p("Midpoint"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                                  choices = c("Zero", "Mean", "Median"),
                                  width = "90px"
                                )
                              )
                            )
                          )
                        ),
                        conditionalPanel(
                          "input.tile_num == 3",
                          fluidRow(
                            column(
                              width = 4,
                              align = "left",
                              checkboxInput(
                                "nj_tiles_show_3",
                                h5(p("Show"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                                value = FALSE
                              )
                            ),
                            column(
                              width = 7,
                              align = "right",
                              dropMenu(
                                actionBttn(
                                  "nj_tile_menu_3",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "left-start",
                                theme = "translucent",
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    uiOutput("nj_fruit_width3")
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "nj_fruit_offset_circ_3",
                                      label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                      min = -3,
                                      max = 3,
                                      step = 0.05,
                                      value = 0,
                                      width = "80px"
                                    )
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "nj_fruit_alpha_3",
                                      label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                      min = 0.1,
                                      max = 1,
                                      step = 0.1,
                                      value = 1,
                                      width = "80px"
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 4,
                              align = "left",
                              HTML(
                                paste(
                                  tags$span(style='color: white; font-size: 14px; margin-left: 16px; position: relative; bottom: -28px', 'Variable')
                                )
                              )
                            ),
                            column(
                              width = 7,
                              align = "center",
                              uiOutput("nj_fruit_variable3")
                            )
                          ),
                          fluidRow(
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "nj_tile_color_low_3",
                                selected = "#F53900",
                                label = h5(p("Low"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "nj_tile_color_mid_3",
                                selected = "#FFFFFF",
                                label = h5(p("Mid"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "nj_tile_color_high_3",
                                selected = "#68B127",
                                label = h5(p("High"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(
                              width = 6,
                              align = "left",
                              checkboxInput(
                                "nj_div_tiles_3",
                                h5(p("Diverging"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                              )
                            ),
                            column(
                              width = 6,
                              div(
                                class = "tile_select",
                                selectInput(
                                  "nj_tile_mid_3",
                                  h5(p("Midpoint"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                                  choices = c("Zero", "Mean", "Median"),
                                  width = "90px"
                                )
                              )
                            )
                          )
                        ),
                        conditionalPanel(
                          "input.tile_num == 4",
                          fluidRow(
                            column(
                              width = 4,
                              align = "left",
                              checkboxInput(
                                "nj_tiles_show_4",
                                h5(p("Show"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                                value = FALSE
                              )
                            ),
                            column(
                              width = 7,
                              align = "right",
                              dropMenu(
                                actionBttn(
                                  "nj_tile_menu_4",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "left-start",
                                theme = "translucent",
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    uiOutput("nj_fruit_width4")
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "nj_fruit_offset_circ_4",
                                      label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                      min = -3,
                                      max = 3,
                                      step = 0.05,
                                      value = 0,
                                      width = "80px"
                                    )
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "nj_fruit_alpha_4",
                                      label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                      min = 0.1,
                                      max = 1,
                                      step = 0.1,
                                      value = 1,
                                      width = "80px"
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 4,
                              align = "left",
                              HTML(
                                paste(
                                  tags$span(style='color: white; font-size: 14px; margin-left: 16px; position: relative; bottom: -28px', 'Variable')
                                )
                              )
                            ),
                            column(
                              width = 7,
                              align = "center",
                              uiOutput("nj_fruit_variable4")
                            )
                          ),
                          fluidRow(
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "nj_tile_color_low_4",
                                selected = "#F53900",
                                label = h5(p("Low"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "nj_tile_color_mid_4",
                                selected = "#FFFFFF",
                                label = h5(p("Mid"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "nj_tile_color_high_4",
                                selected = "#68B127",
                                label = h5(p("High"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(
                              width = 6,
                              align = "left",
                              checkboxInput(
                                "nj_div_tiles_4",
                                h5(p("Diverging"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                              )
                            ),
                            column(
                              width = 6,
                              div(
                                class = "tile_select",
                                selectInput(
                                  "nj_tile_mid_4",
                                  h5(p("Midpoint"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                                  choices = c("Zero", "Mean", "Median"),
                                  width = "90px"
                                )
                              )
                            )
                          )
                        ),
                        conditionalPanel(
                          "input.tile_num == 5",
                          fluidRow(
                            column(
                              width = 4,
                              align = "left",
                              checkboxInput(
                                "nj_tiles_show_5",
                                h5(p("Show"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                                value = FALSE
                              )
                            ),
                            column(
                              width = 7,
                              align = "right",
                              dropMenu(
                                actionBttn(
                                  "nj_tile_menu_5",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "left-start",
                                theme = "translucent",
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    uiOutput("nj_fruit_width5")
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "nj_fruit_offset_circ_5",
                                      label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                      min = -3,
                                      max = 3,
                                      step = 0.05,
                                      value = 0,
                                      width = "80px"
                                    )
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "nj_fruit_alpha_5",
                                      label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                      min = 0.1,
                                      max = 1,
                                      step = 0.1,
                                      value = 1,
                                      width = "80px"
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 4,
                              align = "left",
                              HTML(
                                paste(
                                  tags$span(style='color: white; font-size: 14px; margin-left: 16px; position: relative; bottom: -28px', 'Variable')
                                )
                              )
                            ),
                            column(
                              width = 7,
                              align = "center",
                              uiOutput("nj_fruit_variable5")
                            )
                          ),
                          fluidRow(
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "nj_tile_color_low_5",
                                selected = "#F53900",
                                label = h5(p("Low"), style = "color:white; margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "nj_tile_color_mid_5",
                                selected = "#FFFFFF",
                                label = h5(p("Mid"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "nj_tile_color_high_5",
                                selected = "#68B127",
                                label = h5(p("High"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(
                              width = 6,
                              align = "left",
                              checkboxInput(
                                "nj_div_tiles_5",
                                h5(p("Diverging"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                              )
                            ),
                            column(
                              width = 6,
                              div(
                                class = "tile_select",
                                selectInput(
                                  "nj_tile_mid_5",
                                  h5(p("Midpoint"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                                  choices = c("Zero", "Mean", "Median"),
                                  width = "90px"
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                ), 
                hr(),
                fluidRow(
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        fluidRow(
                          column(
                            width = 6,
                            h4(p("Heatmap"), style = "color:white; position: relative; right: -15px")
                          )
                        ),
                        fluidRow(
                          column(
                            width = 4,
                            align = "left",
                            checkboxInput(
                              "nj_heatmap_show",
                              h5(p("Show"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                              value = FALSE
                            )
                          ),
                          column(
                            width = 7,
                            align = "right",
                            dropMenu(
                              actionBttn(
                                "nj_heatmap_menu",
                                label = "",
                                color = "default",
                                size = "sm",
                                style = "material-flat",
                                icon = icon("sliders")
                              ),
                              placement = "left-start",
                              theme = "translucent",
                              fluidRow(
                                column(
                                  width = 12,
                                  align = "center",
                                  uiOutput("nj_heatmap_offs")
                                )
                              ),
                              fluidRow(
                                column(
                                  width = 12,
                                  align = "center",
                                  numericInput(
                                    "nj_heatmap_width",
                                    label = h5("Width", style = "color:white; margin-bottom: 0px"),
                                    min = 0.1,
                                    max = 5,
                                    step = 0.1,
                                    value = 0.5,
                                    width = "80px"
                                  )
                                )
                              ),
                              fluidRow(
                                column(
                                  width = 12,
                                  align = "center",
                                  numericInput(
                                    "nj_colnames_angle",
                                    label = h5("Names angle", style = "color:white; margin-bottom: 0px"),
                                    min = 0,
                                    max = 360,
                                    value = 0,
                                    step = 5,
                                    width = "80px"
                                  )
                                )
                              ),
                              fluidRow(
                                column(
                                  width = 12,
                                  align = "center",
                                  numericInput(
                                    "nj_colnames_x",
                                    label = h5("Names X Pos", style = "color:white; margin-bottom: 0px"),
                                    min = -10,
                                    max = 10,
                                    value = 0,
                                    step = 1,
                                    width = "80px"
                                  )
                                )
                              ),
                              fluidRow(
                                column(
                                  width = 12,
                                  align = "center",
                                  numericInput(
                                    "nj_colnames_y",
                                    label = h5("Names Y Pos", style = "color:white; margin-bottom: 0px"),
                                    min = -10,
                                    max = 10,
                                    value = 0,
                                    step = 1,
                                    width = "80px"
                                  )
                                )
                              )
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 6,
                            uiOutput("nj_heatmap_sel")
                          ),
                          column(
                            width = 6,
                            textInput(
                              "nj_heatmap_title",
                              label = h5("Legend title", style = "color:white; margin-bottom: 0px; position: relative; bottom: -20px"),
                              value = "values",
                              placeholder = "values" 
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                conditionalPanel(
                  "input.nj_layout=='inward'|input.nj_layout=='circular'",
                  br(), br()
                )
              ),
              br(), br(), br(), br(), br(), br(), br()
            )
          )
        ),
        
        ### Control Panels UPGMA ----
        
        conditionalPanel(
          "input.tree_algo=='UPGMA'",
          fluidRow(
            column(
              width = 4,
              align = "center",
              box(
                solidHeader = TRUE,
                status = "primary",
                width = "100%",
                h3(p("Layout"), style = "color:white"),
                hr(),
                fluidRow(
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Theme"), style = "color:white; position: relative; right: -15px"),
                        fluidRow(
                          column(
                            width = 12,
                            align = "center",
                            selectInput(
                              inputId = "upgma_layout",
                              label = "",
                              choices = list(
                                Linear = list(
                                  "Rectangular" = "rectangular",
                                  "Roundrect" = "roundrect",
                                  "Slanted" = "slanted",
                                  "Ellipse" = "ellipse"
                                ),
                                Circular = list("Circular" = "circular",
                                                "Inward" = "inward")
                              ),
                              selected = "circular",
                              width = "90%"
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 5,
                            align = "left",
                            checkboxInput(
                              "upgma_ladder",
                              h5(p("Ladder"), style = "color:white; position: relative; bottom: 3px; right: -15px; margin-top: 22px"),
                              value = TRUE
                            )
                          ),
                          column(
                            width = 3,
                            align = "left",
                            checkboxInput(
                              "upgma_rootedge_show",
                              h5(p("Root"), style = "color:white; position: relative; bottom: 3px; right: -5px; margin-top: 23px"),
                              value = FALSE
                            )
                          ),
                          column(
                            width = 2,
                            align = "right",
                            dropMenu(
                              actionBttn(
                                "upgma_rootedge_menu",
                                label = "",
                                color = "default",
                                size = "sm",
                                style = "material-flat",
                                icon = icon("sliders")
                              ),
                              placement = "top-start",
                              theme = "translucent",
                              fluidRow(
                                column(
                                  width = 12,
                                  align = "center",
                                  numericInput(
                                    "upgma_rootedge_length",
                                    label = h5("Length", style = "color:white; margin-bottom: 0px"),
                                    value = 3,
                                    min = 1,
                                    max = 30,
                                    step = 1,
                                    width = "80px"
                                  ),
                                  br(),
                                  selectInput(
                                    "upgma_rootedge_line",
                                    label = h5("Linetype", style = "color:white"),
                                    choices = c(Solid = "solid", Dashed = "dashed", Dotted = "dotted"),
                                    selected = c(Dotted = "dotted"),
                                    width = "100px"
                                  )
                                )
                              )
                            )
                          )
                        ),
                        conditionalPanel(
                          "input.upgma_layout=='circular' | input.upgma_layout=='inward'",
                          fluidRow(
                            column(
                              width = 3,
                              h5(p("Adjust"), style = "color:white; position: relative; right: -15px; margin-top: 20px")
                            ),
                            column(
                              width = 8,
                              align = "right",
                              conditionalPanel(
                                "input.upgma_layout=='circular'",
                                sliderTextInput(
                                  "upgma_xlim",
                                  label = NULL,
                                  choices = -50:0,
                                  selected = -10,
                                  hide_min_max = TRUE
                                )
                              ),
                              conditionalPanel(
                                "input.upgma_layout=='inward'",
                                sliderTextInput(
                                  "upgma_inward_xlim",
                                  label = NULL,
                                  choices = 30:120,
                                  selected = 50,
                                  hide_min_max = TRUE
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Color"), style = "color:white; position: relative; right: -15px"),
                        fluidRow(
                          column(
                            width = 5,
                            h5(p("Lines/Text"), style = "color:white; position: relative; right: -15px; margin-top: 30px")
                          ),
                          column(
                            width = 7,
                            colorPickr(
                              inputId = "upgma_color",
                              width = "90%",
                              selected = "#000000",
                              label = "",
                              update = "changestop",
                              interaction = list(clear = FALSE,
                                                 save = FALSE),
                              position = "right-start"
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 5,
                            h5(p("Background"), style = "color:white; position: relative; right: -15px; margin-top: 30px")
                          ),
                          column(
                            width = 7,
                            colorPickr(
                              inputId = "upgma_bg",
                              width = "90%",
                              selected = "#ffffff",
                              label = "",
                              update = "changestop",
                              interaction = list(clear = FALSE,
                                                 save = FALSE),
                              position = "right-start"
                            )
                          )
                        )
                      )
                    ), br()
                  )
                ),
                hr(),
                fluidRow(
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Title"), style = "color:white; position: relative; right: -15px"),
                        column(
                          width = 12,
                          align = "center",
                          textInput(
                            "upgma_title",
                            label = "",
                            width = "100%",
                            placeholder = "Plot Title"
                          ),
                          fluidRow(
                            column(
                              width = 7,
                              colorPickr(
                                inputId = "upgma_title_color",
                                selected = "#000000",
                                label = "",
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 5,
                              align = "right",
                              dropMenu(
                                actionBttn(
                                  "upgma_title_menu",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "top-start",
                                theme = "translucent",
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "upgma_title_size",
                                      label = h5("Size", style = "color:white; margin-bottom: 0px"),
                                      value = 30,
                                      min = 15,
                                      max = 40,
                                      step = 1,
                                      width = "80px"
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          br()
                        )
                      )
                    )
                  ),
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Subtitle"), style = "color:white; position: relative; right: -15px"),
                        column(
                          width = 12,
                          align = "center",
                          textInput(
                            "upgma_subtitle",
                            label = "",
                            width = "100%",
                            placeholder = "Plot Subtitle"
                          ),
                          fluidRow(
                            column(
                              width = 7,
                              colorPickr(
                                inputId = "upgma_subtitle_color",
                                selected = "#000000",
                                label = "",
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 5,
                              align = "right",
                              dropMenu(
                                actionBttn(
                                  "upgma_subtitle_menu",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "top-start",
                                theme = "translucent",
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "upgma_subtitle_size",
                                      label = h5("Size", style = "color:white; margin-bottom: 0px"),
                                      value = 20,
                                      min = 15,
                                      max = 40,
                                      step = 1,
                                      width = "80px"
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          br()
                        )
                      )
                    )
                  )
                ),
                hr(),
                fluidRow(
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Tree scale"), style = "color:white; position: relative; right: -15px"),
                        column(
                          width = 12,
                          fluidRow(
                            column(
                              width = 7,
                              align = "left",
                              checkboxInput(
                                "upgma_treescale_show",
                                h5(p("Show "), style = "color:white; position: relative; bottom: -7px; right: 0px"),
                                value = TRUE
                              ),
                              br()
                            ),
                            column(
                              width = 5,
                              align = "right",
                              dropMenu(
                                actionBttn(
                                  "upgma_treescale_menu",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "top-start",
                                theme = "translucent",
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "upgma_treescale_width",
                                      label = h5("Length", style = "color:white; margin-bottom: 0px"),
                                      value = 3,
                                      min = 1,
                                      max = 20,
                                      step = 1,
                                      width = "80px"
                                    ),
                                    br(),
                                    uiOutput("upgma_treescalex"),
                                    br(),
                                    uiOutput("upgma_treescaley")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Legend"), style = "color:white; position: relative; right: -15px"),
                        column(
                          width = 12,
                          align = "center",
                          fluidRow(
                            column(
                              width = 5,
                              align = "left",
                              prettyRadioButtons(
                                "upgma_legend_orientation",
                                "",
                                choices = c(Horizontal = "horizontal",
                                            Vertical = "vertical"),
                                selected = c(Horizontal = "horizontal"),
                                inline = FALSE
                              )
                            ),
                            column(
                              width = 5,
                              align = "right",
                              dropMenu(
                                actionBttn(
                                  "upgma_legend_menu",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "top-start",
                                theme = "translucent",
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "upgma_legend_size",
                                      label = h5("Size", style = "color:white; margin-bottom: 0px"),
                                      value = 10,
                                      min = 5,
                                      max = 25,
                                      step = 1,
                                      width = "80px"
                                    ),
                                    numericInput(
                                      "upgma_legend_x",
                                      label = h5("X Pos", style = "color:white; margin-bottom: 0px"),
                                      value = 0.1,
                                      min = -0.9,
                                      max = 1.9,
                                      step = 0.2,
                                      width = "80px"
                                    ),
                                    numericInput(
                                      "upgma_legend_y",
                                      label = h5("Y Pos", style = "color:white; margin-bottom: 0px"),
                                      value = 1,
                                      min = -1,
                                      max = 1,
                                      step = 0.1,
                                      width = "80px"
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
                )
              )
            ),
            column(
              width = 4,
              align = "center",
              box(
                solidHeader = TRUE,
                status = "primary",
                width = "100%",
                h3(p("Label"), style = "color:white"),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    align = "left",
                    h4(p("Tips"), style = "color:white; position: relative; right: -15px"),
                    fluidRow(
                      column(
                        width = 4,
                        align = "left",
                        checkboxInput(
                          "upgma_tiplab_show",
                          h5(p("Show"), style = "color:white; position: relative; right: -17px; bottom: -7px"),
                          value = TRUE
                        )
                      ),
                      column(
                        width = 4,
                        align = "center",
                        uiOutput("upgma_tiplab")
                      ),
                      column(
                        width = 3,
                        align = "right",
                        dropMenu(
                          actionBttn(
                            "upgma_labeltext_menu",
                            label = "",
                            color = "default",
                            size = "sm",
                            style = "material-flat",
                            icon = icon("sliders")
                          ),
                          placement = "top-start",
                          theme = "translucent",
                          fluidRow(
                            column(
                              width = 6,
                              align = "center",
                              numericInput(
                                "upgma_tiplab_size",
                                label = h5("Label size", style = "color:white; margin-bottom: 0px"),
                                min = 1,
                                max = 10,
                                step = 0.5,
                                value = 4,
                                width = "80px"
                              ),
                              br(),
                              numericInput(
                                "upgma_tiplab_alpha",
                                label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                min = 0.1,
                                max = 1,
                                value = 1,
                                width = "80px"
                              ),
                              br(),
                              selectInput(
                                "upgma_tiplab_fontface",
                                label = h5("Fontface", style = "color:white; margin-bottom: 0px"),
                                width = "250px",
                                choices = c(Plain = "plain", Bold =  "bold", Italic =  "italic", `B & I` = "bold.italic")
                              )
                            ),
                            column(
                              width = 6,
                              align = "center",
                              conditionalPanel(
                                "!(input.upgma_layout=='inward'|input.upgma_layout=='circular')",
                                numericInput(
                                  inputId = "upgma_tiplab_nudge_x",
                                  label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                  min = -3,
                                  max = 3,
                                  step = 0.05,
                                  value = 0,
                                  width = "80px"
                                )
                              ),
                              conditionalPanel(
                                "input.upgma_layout=='circular'",
                                numericInput(
                                  inputId = "upgma_tiplab_position",
                                  label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                  min = -3,
                                  max = 3,
                                  step = 0.05,
                                  value = -0.1,
                                  width = "80px"
                                )
                              ),
                              conditionalPanel(
                                "input.upgma_layout=='inward'",
                                numericInput(
                                  inputId = "upgma_tiplab_position_inw",
                                  label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                  min = -3,
                                  max = 3,
                                  step = 0.05,
                                  value = 1.1,
                                  width = "80px"
                                )
                              ),
                              br(),
                              sliderTextInput(
                                inputId = "upgma_tiplab_angle",
                                label = h5("Angle", style = "color:white; margin-bottom: 0px"),
                                choices = -180:180,
                                selected = 0,
                                hide_min_max = TRUE
                              )      
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 4,
                    align = "left",
                    checkboxInput(
                      "upgma_align",
                      h5(p("Align labels"), style = "color:white; position: relative; bottom: -10px; right: -15px"),
                      value = FALSE
                    )
                  ),
                  column(
                    width = 1,
                    HTML(
                      paste(
                        tags$span(style='color: white; font-size: 14px; position: relative; bottom: -28px; margin-left: 0px ', 'Width')
                      )
                    )
                  ),
                  column(
                    width = 3,
                    align = "left",
                    numericInput(
                      "upgma_tiplab_linesize",
                      "",
                      value = 0.5,
                      min = 0.1,
                      max = 3,
                      step = 0.1,
                      width = "80px"
                    )
                  ),
                  column(
                    width = 3,
                    selectInput(
                      "upgma_tiplab_linetype",
                      "",
                      choices = c(Solid = "solid", Dashed = "dashed", Dotted = "dotted"),
                      selected = c(Dotted = "dotted")
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 4,
                    align = "left",
                    h5(p("Color"), style = "color:white; position: relative; right: -14px; margin-top: 30px")
                  ),
                  column(
                    width = 4,
                    conditionalPanel(
                      "input.upgma_mapping_show==false",
                      colorPickr(
                        inputId = "upgma_tiplab_color",
                        width = "100%",
                        selected = "#000000",
                        label = "",
                        update = "changestop",
                        interaction = list(clear = FALSE,
                                           save = FALSE),
                        position = "right-start"
                      )
                    ),
                    conditionalPanel(
                      "input.upgma_mapping_show==true",
                      uiOutput("upgma_color_mapping")
                    )
                  ),
                  column(
                    width = 4,
                    align = "left",
                    checkboxInput(
                      "upgma_mapping_show",
                      label = h5("Add variable", style = "color:white; font-size: 14px; position: relative; bottom: -10px;"),
                      value = FALSE
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 4,
                    align = "left",
                    br(),
                    checkboxInput(
                      "upgma_geom",
                      h5(p("Panel"), style = "color:white; position: relative; bottom: 10px; right: -17px"),
                      value = FALSE
                    )
                  ),
                  column(
                    width = 4,
                    colorPickr(
                      inputId = "upgma_tiplab_fill",
                      width = "100%",
                      selected = "#84D9A0",
                      label = "",
                      update = "changestop",
                      interaction = list(clear = FALSE,
                                         save = FALSE),
                      position = "right-start"
                    )
                  ),
                  column(
                    width = 3,
                    align = "right",
                    dropMenu(
                      actionBttn(
                        "upgma_labelformat_menu",
                        label = "",
                        color = "default",
                        size = "sm",
                        style = "material-flat",
                        icon = icon("sliders")
                      ),
                      placement = "top-start",
                      theme = "translucent",
                      fluidRow(
                        column(
                          width = 12,
                          align = "center",
                          numericInput(
                            inputId = "upgma_tiplab_padding",
                            label = h5("Size", style = "color:white; margin-bottom: 0px"),
                            min = 0.1,
                            step = 0.1,
                            max = 1,
                            value = 0.3,
                            width = "80px"
                          ),
                          br(),
                          numericInput(
                            inputId = "upgma_tiplab_labelradius",
                            label = h5("Smooth edge", style = "color:white; margin-bottom: 0px"),
                            min = 0,
                            max = 0.5,
                            step = 0.05,
                            value = 0.2,
                            width = "80px"
                          )
                        )
                      )
                    )
                  )
                ),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    align = "left",
                    h4(p("Branches"), style = "color:white; position: relative; right: -15px"),
                    fluidRow(
                      column(
                        width = 4,
                        align = "left",
                        checkboxInput(
                          "upgma_show_branch_label",
                          h5(p("Show"), style = "color:white; position: relative; bottom: -7px; right: -17px"),
                          value = FALSE
                        )
                      ),
                      column(
                        width = 4,
                        align = "center",
                        uiOutput("upgma_branch_label")
                      ),
                      column(
                        width = 3,
                        align = "right",
                        dropMenu(
                          actionBttn(
                            "upgma_branch_label_menu",
                            label = "",
                            color = "default",
                            size = "sm",
                            style = "material-flat",
                            icon = icon("sliders")
                          ),
                          placement = "top-start",
                          theme = "translucent",
                          fluidRow(
                            column(
                              width = 6,
                              align = "center",
                              numericInput(
                                "upgma_branch_size",
                                label = h5("Size", style = "color:white; margin-bottom: 0px"),
                                min = 3,
                                max = 10,
                                step = 0.5,
                                value = 4,
                                width = "80px"
                              ),
                              br(),
                              numericInput(
                                "upgma_branch_labelradius",
                                label = h5("Smooth edge", style = "color:white; margin-bottom: 0px"),
                                value = 0.5,
                                step = 0.05,
                                min = 0,
                                max = 0.5,
                                width = "80px"
                              ),
                              br(),
                              selectInput(
                                "upgma_branchlab_fontface",
                                label = h5("Fontface", style = "color:white"),
                                width = "250px",
                                choices = c(Plain = "plain", Bold =  "bold", Italic =  "italic", `B & I` = "bold.italic")
                              )
                            ),
                            column(
                              width = 6,
                              align = "center",
                              numericInput(
                                "upgma_branchlab_alpha",
                                label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                min = 0.1,
                                max = 1,
                                value = 0.7,
                                width = "80px"
                              ),
                              br(),
                              sliderTextInput(
                                inputId = "upgma_branch_x",
                                label = h5("X Position", style = "color:white; margin-bottom: 0px"),
                                choices = seq(-3, 3, by = 0.1),
                                selected = 0,
                                width = "250px",
                                hide_min_max = TRUE
                              ),
                              br(),
                              sliderTextInput(
                                inputId = "upgma_branch_y",
                                label = h5("Y Position", style = "color:white; margin-bottom: 0px"),
                                choices = seq(-3, 3, by = 0.1),
                                selected = 0,
                                width = "250px",
                                hide_min_max = TRUE
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 4,
                    align = "left",
                    h5(p("Color"), style = "color:white; position: relative; right: -14px; margin-top: 30px")
                  ),
                  column(
                    width = 4,
                    colorPickr(
                      inputId = "upgma_branch_label_color",
                      width = "100%",
                      selected = "#FFB7B7",
                      label = "",
                      update = "changestop",
                      interaction = list(clear = FALSE,
                                         save = FALSE),
                      position = "right-start"
                    )
                  )
                ),
                br(), br(), br(), 
                conditionalPanel(
                  "input.upgma_layout=='inward'|input.upgma_layout=='circular'",
                  br(), br()
                )
              )
            ),
            column(
              width = 4,
              align = "center",
              box(
                solidHeader = TRUE,
                status = "primary",
                width = "100%",
                h3(p("Elements"), style = "color:white"),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    align = "left",
                    h4(p("Tip points"), style = "color:white; position: relative; right: -15px"),
                    fluidRow(
                      column(
                        width = 4,
                        align = "left",
                        checkboxInput(
                          "upgma_tippoint_show",
                          h5(p("Show"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                          value = FALSE
                        )
                      ),
                      column(
                        width = 2,
                        align = "left",
                        dropMenu(
                          actionBttn(
                            "upgma_tippoint_menu",
                            label = "",
                            color = "default",
                            size = "sm",
                            style = "material-flat",
                            icon = icon("sliders")
                          ),
                          placement = "top-start",
                          theme = "translucent",
                          fluidRow(
                            column(
                              width = 12,
                              align = "center",
                              numericInput(
                                "upgma_tippoint_alpha",
                                label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                value = 0.5,
                                min = 0.1,
                                max = 1,
                                step = 0.1,
                                width = "80px"
                              ), 
                              br(),
                              numericInput(
                                inputId = "upgma_tippoint_size",
                                label = h5("Size", style = "color:white; margin-bottom: 0px"),
                                min = 1,
                                max = 20,
                                step = 1,
                                value = 5,
                                width = "80px"
                              )
                            )
                          )
                        )  
                      )
                    ),
                    fluidRow(
                      column(
                        width = 4,
                        align = "left",
                        h5(p("Color"), style = "color:white; position: relative; right: -15px; margin-top: 30px")
                      ),
                      column(
                        width = 4,
                        align = "center",
                        conditionalPanel(
                          "input.upgma_tipcolor_mapping_show==true & input.upgma_mapping_show==false",
                          uiOutput("upgma_tipcolor_mapping")
                        ),
                        conditionalPanel(
                          "input.upgma_tipcolor_mapping_show==false || input.upgma_mapping_show==true",
                          colorPickr(
                            inputId = "upgma_tippoint_color",
                            width = "100%",
                            selected = "#3A4657",
                            label = "",
                            update = "changestop",
                            interaction = list(clear = FALSE,
                                               save = FALSE),
                            position = "right-start"
                          )
                        )
                      ),
                      column(
                        width = 4,
                        align = "left",
                        conditionalPanel(
                          "input.upgma_mapping_show==false",
                          checkboxInput(
                            "upgma_tipcolor_mapping_show",
                            label = h5("Add variable", style = "color:white; font-size: 14px; position: relative; bottom: -10px;"),
                            value = FALSE
                          )
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 4,
                        align = "left",
                        h5(p("Shape"), style = "color:white; position: relative; right: -17px; margin-top: 30px")
                      ),
                      column(
                        width = 4,
                        align = "center",
                        conditionalPanel(
                          "input.upgma_tipshape_mapping_show==false",
                          selectInput(
                            "upgma_tippoint_shape",
                            "",
                            width = "100%",
                            choices = c(
                              Circle = "circle", 
                              Square = "square", 
                              Diamond = "diamond", 
                              Triangle = "triangle",
                              Cross = "cross", 
                              Asterisk = "asterisk"
                            )
                          )
                        ),
                        conditionalPanel(
                          "input.upgma_tipshape_mapping_show==true",
                          uiOutput("upgma_tipshape_mapping")
                        )
                      ),
                      column(
                        width = 4,
                        align = "left",
                        checkboxInput(
                          "upgma_tipshape_mapping_show",
                          label = h5("Add variable", style = "color:white; font-size: 14px; position: relative; bottom: -10px;"),
                          value = FALSE
                        )
                      )
                    )
                  )
                ),
                hr(),
                fluidRow(
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Node points"), style = "color:white; position: relative; right: -15px"),
                        fluidRow(
                          column(
                            width = 5,
                            align = "left",
                            checkboxInput(
                              "upgma_nodepoint_show",
                              h5(p("Show"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                              value = FALSE
                            )
                          ),
                          column(
                            width = 7,
                            align = "right",
                            dropMenu(
                              actionBttn(
                                "upgma_nodepoint_menu",
                                label = "",
                                color = "default",
                                size = "sm",
                                style = "material-flat",
                                icon = icon("sliders")
                              ),
                              placement = "top-start",
                              theme = "translucent",
                              fluidRow(
                                column(
                                  width = 12,
                                  align = "center",
                                  numericInput(
                                    "upgma_nodepoint_alpha",
                                    label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                    value = 0.5,
                                    min = 0.1,
                                    max = 1,
                                    step = 0.1,
                                    width = "80px"
                                  ), 
                                  br(),
                                  numericInput(
                                    inputId = "upgma_nodepoint_size",
                                    label = h5("Size", style = "color:white; margin-bottom: 0px"),
                                    min = 1,
                                    max = 20,
                                    step = 1,
                                    value = 5,
                                    width = "80px"
                                  )
                                )
                              )
                            )  
                          )
                        ),
                        fluidRow(
                          column(
                            width = 5,
                            h5(p("Color"), style = "color:white; position: relative; right: -15px; margin-top: 30px")
                          ),
                          column(
                            width = 7,
                            align = "center",
                            colorPickr(
                              inputId = "upgma_nodepoint_color",
                              width = "100%",
                              selected = "#3A4657",
                              label = "",
                              update = "changestop",
                              interaction = list(clear = FALSE,
                                                 save = FALSE),
                              position = "right-start"
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 5,
                            h5(p("Shape"), style = "color:white; position: relative; right: -20px; margin-top: 30px")
                          ),
                          column(
                            width = 7,
                            align = "center",
                            selectInput(
                              "upgma_nodepoint_shape",
                              "",
                              choices = c(
                                Circle = "circle", 
                                Square = "square", 
                                Diamond = "diamond", 
                                Triangle = "triangle",
                                Cross = "cross", 
                                Asterisk = "asterisk"
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        fluidRow(
                          column(
                            width = 6,
                            h4(p("Tiles"), style = "color:white; position: relative; right: -15px")
                          ),
                          column(
                            width = 6,
                            div(
                              class = "tile1",
                              selectInput(
                                "tile_num",
                                "",
                                choices = 1:5,
                                width = "70px"
                              )
                            )
                          )
                        ),
                        conditionalPanel(
                          "input.tile_num == 1",
                          fluidRow(
                            column(
                              width = 4,
                              align = "left",
                              checkboxInput(
                                "upgma_tiles_show",
                                h5(p("Show"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                                value = FALSE
                              )
                            ),
                            column(
                              width = 7,
                              align = "right",
                              dropMenu(
                                actionBttn(
                                  "upgma_tile_menu",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "left-start",
                                theme = "translucent",
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    uiOutput("upgma_fruit_width")
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "upgma_fruit_offset_circ",
                                      label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                      min = -3,
                                      max = 3,
                                      step = 0.05,
                                      value = 0,
                                      width = "80px"
                                    )
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "upgma_fruit_alpha",
                                      label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                      min = 0.1,
                                      max = 1,
                                      step = 0.1,
                                      value = 1,
                                      width = "80px"
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 4,
                              align = "left",
                              HTML(
                                paste(
                                  tags$span(style='color: white; font-size: 14px; margin-left: 16px; position: relative; bottom: -28px', 'Variable')
                                )
                              )
                            ),
                            column(
                              width = 7,
                              align = "center",
                              uiOutput("upgma_fruit_variable")
                            )
                          ),
                          fluidRow(
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "upgma_tile_color_low",
                                selected = "#F53900",
                                label = h5(p("Low"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "upgma_tile_color_mid",
                                selected = "#FFFFFF",
                                label = h5(p("Mid"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "upgma_tile_color_high",
                                selected = "#68B127",
                                label = h5(p("High"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(
                              width = 6,
                              align = "left",
                              checkboxInput(
                                "upgma_div_tiles",
                                h5(p("Diverging"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                              )
                            ),
                            column(
                              width = 6,
                              div(
                                class = "tile_select",
                                selectInput(
                                  "upgma_tile_mid",
                                  h5(p("Midpoint"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                                  choices = c("Zero", "Mean", "Median"),
                                  width = "90px"
                                )
                              )
                            )
                          )
                        ),
                        conditionalPanel(
                          "input.tile_num == 2",
                          fluidRow(
                            column(
                              width = 4,
                              align = "left",
                              checkboxInput(
                                "upgma_tiles_show_2",
                                h5(p("Show"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                                value = FALSE
                              )
                            ),
                            column(
                              width = 7,
                              align = "right",
                              dropMenu(
                                actionBttn(
                                  "upgma_tile_menu_2",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "left-start",
                                theme = "translucent",
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    uiOutput("upgma_fruit_width2")
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "upgma_fruit_offset_circ_2",
                                      label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                      min = -3,
                                      max = 3,
                                      step = 0.05,
                                      value = 0,
                                      width = "80px"
                                    )
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "upgma_fruit_alpha_2",
                                      label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                      min = 0.1,
                                      max = 1,
                                      step = 0.1,
                                      value = 1,
                                      width = "80px"
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 4,
                              align = "left",
                              HTML(
                                paste(
                                  tags$span(style='color: white; font-size: 14px; margin-left: 16px; position: relative; bottom: -28px', 'Variable')
                                )
                              )
                            ),
                            column(
                              width = 7,
                              align = "center",
                              uiOutput("upgma_fruit_variable2")
                            )
                          ),
                          fluidRow(
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "upgma_tile_color_low_2",
                                selected = "#F53900",
                                label = h5(p("Low"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "upgma_tile_color_mid_2",
                                selected = "#FFFFFF",
                                label = h5(p("Mid"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "upgma_tile_color_high_2",
                                selected = "#68B127",
                                label = h5(p("High"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(
                              width = 6,
                              align = "left",
                              checkboxInput(
                                "upgma_div_tiles_2",
                                h5(p("Diverging"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                              )
                            ),
                            column(
                              width = 6,
                              div(
                                class = "tile_select",
                                selectInput(
                                  "upgma_tile_mid_2",
                                  h5(p("Midpoint"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                                  choices = c("Zero", "Mean", "Median"),
                                  width = "90px"
                                )
                              )
                            )
                          )
                        ),
                        conditionalPanel(
                          "input.tile_num == 3",
                          fluidRow(
                            column(
                              width = 4,
                              align = "left",
                              checkboxInput(
                                "upgma_tiles_show_3",
                                h5(p("Show"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                                value = FALSE
                              )
                            ),
                            column(
                              width = 7,
                              align = "right",
                              dropMenu(
                                actionBttn(
                                  "upgma_tile_menu_3",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "left-start",
                                theme = "translucent",
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    uiOutput("upgma_fruit_width3")
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "upgma_fruit_offset_circ_3",
                                      label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                      min = -3,
                                      max = 3,
                                      step = 0.05,
                                      value = 0,
                                      width = "80px"
                                    )
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "upgma_fruit_alpha_3",
                                      label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                      min = 0.1,
                                      max = 1,
                                      step = 0.1,
                                      value = 1,
                                      width = "80px"
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 4,
                              align = "left",
                              HTML(
                                paste(
                                  tags$span(style='color: white; font-size: 14px; margin-left: 16px; position: relative; bottom: -28px', 'Variable')
                                )
                              )
                            ),
                            column(
                              width = 7,
                              align = "center",
                              uiOutput("upgma_fruit_variable3")
                            )
                          ),
                          fluidRow(
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "upgma_tile_color_low_3",
                                selected = "#F53900",
                                label = h5(p("Low"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "upgma_tile_color_mid_3",
                                selected = "#FFFFFF",
                                label = h5(p("Mid"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "upgma_tile_color_high_3",
                                selected = "#68B127",
                                label = h5(p("High"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(
                              width = 6,
                              align = "left",
                              checkboxInput(
                                "upgma_div_tiles_3",
                                h5(p("Diverging"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                              )
                            ),
                            column(
                              width = 6,
                              div(
                                class = "tile_select",
                                selectInput(
                                  "upgma_tile_mid_3",
                                  h5(p("Midpoint"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                                  choices = c("Zero", "Mean", "Median"),
                                  width = "90px"
                                )
                              )
                            )
                          )
                        ),
                        conditionalPanel(
                          "input.tile_num == 4",
                          fluidRow(
                            column(
                              width = 4,
                              align = "left",
                              checkboxInput(
                                "upgma_tiles_show_4",
                                h5(p("Show"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                                value = FALSE
                              )
                            ),
                            column(
                              width = 7,
                              align = "right",
                              dropMenu(
                                actionBttn(
                                  "upgma_tile_menu_4",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "left-start",
                                theme = "translucent",
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    uiOutput("upgma_fruit_width4")
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "upgma_fruit_offset_circ_4",
                                      label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                      min = -3,
                                      max = 3,
                                      step = 0.05,
                                      value = 0,
                                      width = "80px"
                                    )
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "upgma_fruit_alpha_4",
                                      label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                      min = 0.1,
                                      max = 1,
                                      step = 0.1,
                                      value = 1,
                                      width = "80px"
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 4,
                              align = "left",
                              HTML(
                                paste(
                                  tags$span(style='color: white; font-size: 14px; margin-left: 16px; position: relative; bottom: -28px', 'Variable')
                                )
                              )
                            ),
                            column(
                              width = 7,
                              align = "center",
                              uiOutput("upgma_fruit_variable4")
                            )
                          ),
                          fluidRow(
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "upgma_tile_color_low_4",
                                selected = "#F53900",
                                label = h5(p("Low"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "upgma_tile_color_mid_4",
                                selected = "#FFFFFF",
                                label = h5(p("Mid"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "upgma_tile_color_high_4",
                                selected = "#68B127",
                                label = h5(p("High"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(
                              width = 6,
                              align = "left",
                              checkboxInput(
                                "upgma_div_tiles_4",
                                h5(p("Diverging"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                              )
                            ),
                            column(
                              width = 6,
                              div(
                                class = "tile_select",
                                selectInput(
                                  "upgma_tile_mid_4",
                                  h5(p("Midpoint"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                                  choices = c("Zero", "Mean", "Median"),
                                  width = "90px"
                                )
                              )
                            )
                          )
                        ),
                        conditionalPanel(
                          "input.tile_num == 5",
                          fluidRow(
                            column(
                              width = 4,
                              align = "left",
                              checkboxInput(
                                "upgma_tiles_show_5",
                                h5(p("Show"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                                value = FALSE
                              )
                            ),
                            column(
                              width = 7,
                              align = "right",
                              dropMenu(
                                actionBttn(
                                  "upgma_tile_menu_5",
                                  label = "",
                                  color = "default",
                                  size = "sm",
                                  style = "material-flat",
                                  icon = icon("sliders")
                                ),
                                placement = "left-start",
                                theme = "translucent",
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    uiOutput("upgma_fruit_width5")
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "upgma_fruit_offset_circ_5",
                                      label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                      min = -3,
                                      max = 3,
                                      step = 0.05,
                                      value = 0,
                                      width = "80px"
                                    )
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    numericInput(
                                      "upgma_fruit_alpha_5",
                                      label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                      min = 0.1,
                                      max = 1,
                                      step = 0.1,
                                      value = 1,
                                      width = "80px"
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 4,
                              align = "left",
                              HTML(
                                paste(
                                  tags$span(style='color: white; font-size: 14px; margin-left: 16px; position: relative; bottom: -28px', 'Variable')
                                )
                              )
                            ),
                            column(
                              width = 7,
                              align = "center",
                              uiOutput("upgma_fruit_variable5")
                            )
                          ),
                          fluidRow(
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "upgma_tile_color_low_5",
                                selected = "#F53900",
                                label = h5(p("Low"), style = "color:white; margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "upgma_tile_color_mid_5",
                                selected = "#FFFFFF",
                                label = h5(p("Mid"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            ),
                            column(
                              width = 4,
                              align = "center",
                              colorPickr(
                                inputId = "upgma_tile_color_high_5",
                                selected = "#68B127",
                                label = h5(p("High"), style = "color:white;margin-bottom: -5px"),
                                update = "changestop",
                                interaction = list(clear = FALSE,
                                                   save = FALSE),
                                position = "right-start",
                                width = "100%"
                              )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(
                              width = 6,
                              align = "left",
                              checkboxInput(
                                "upgma_div_tiles_5",
                                h5(p("Diverging"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                              )
                            ),
                            column(
                              width = 6,
                              div(
                                class = "tile_select",
                                selectInput(
                                  "upgma_tile_mid_5",
                                  h5(p("Midpoint"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                                  choices = c("Zero", "Mean", "Median"),
                                  width = "90px"
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                ), 
                hr(),
                fluidRow(
                  column(
                    width = 6,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        fluidRow(
                          column(
                            width = 6,
                            h4(p("Heatmap"), style = "color:white; position: relative; right: -15px")
                          )
                        ),
                        fluidRow(
                          column(
                            width = 4,
                            align = "left",
                            checkboxInput(
                              "upgma_heatmap_show",
                              h5(p("Show"), style = "color:white; position: relative; bottom: -8px; right: -17px"),
                              value = FALSE
                            )
                          ),
                          column(
                            width = 7,
                            align = "right",
                            dropMenu(
                              actionBttn(
                                "upgma_heatmap_menu",
                                label = "",
                                color = "default",
                                size = "sm",
                                style = "material-flat",
                                icon = icon("sliders")
                              ),
                              placement = "left-start",
                              theme = "translucent",
                              fluidRow(
                                column(
                                  width = 12,
                                  align = "center",
                                  uiOutput("upgma_heatmap_offs")
                                )
                              ),
                              fluidRow(
                                column(
                                  width = 12,
                                  align = "center",
                                  numericInput(
                                    "upgma_heatmap_width",
                                    label = h5("Width", style = "color:white; margin-bottom: 0px"),
                                    min = 0.1,
                                    max = 5,
                                    step = 0.1,
                                    value = 0.5,
                                    width = "80px"
                                  )
                                )
                              ),
                              fluidRow(
                                column(
                                  width = 12,
                                  align = "center",
                                  numericInput(
                                    "upgma_colnames_angle",
                                    label = h5("Names angle", style = "color:white; margin-bottom: 0px"),
                                    min = 0,
                                    max = 360,
                                    value = 0,
                                    step = 5,
                                    width = "80px"
                                  )
                                )
                              ),
                              fluidRow(
                                column(
                                  width = 12,
                                  align = "center",
                                  numericInput(
                                    "upgma_colnames_x",
                                    label = h5("Names X Pos", style = "color:white; margin-bottom: 0px"),
                                    min = -10,
                                    max = 10,
                                    value = 0,
                                    step = 1,
                                    width = "80px"
                                  )
                                )
                              ),
                              fluidRow(
                                column(
                                  width = 12,
                                  align = "center",
                                  numericInput(
                                    "upgma_colnames_y",
                                    label = h5("Names Y Pos", style = "color:white; margin-bottom: 0px"),
                                    min = -10,
                                    max = 10,
                                    value = 0,
                                    step = 1,
                                    width = "80px"
                                  )
                                )
                              )
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 6,
                            uiOutput("upgma_heatmap_sel")
                          ),
                          column(
                            width = 6,
                            textInput(
                              "upgma_heatmap_title",
                              label = h5("Legend title", style = "color:white; margin-bottom: 0px; position: relative; bottom: -20px"),
                              value = "values",
                              placeholder = "values" 
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                conditionalPanel(
                  "input.upgma_layout=='inward'|input.upgma_layout=='circular'",
                  br(), br()
                )
              ),
              br(), br(), br(), br(), br(), br(), br()
            )
          )
        ),
        br(), br(), br(), br(), br(), br(), br()
      )
    ) # End tabItems
  ) # End dashboardPage
) # end UI



# Server ----

server <- function(input, output, session) {
  
  phylotraceVersion <- paste("PhyloTrace-1.0.0", Sys.Date())
  
  # Disable MST variable mappings
  shinyjs::disable('mst_edge_label') 
  shinyjs::disable('mst_color_var') 
  
  ## Functions ----
  
  # Get rhandsontable
  get.entry.table.meta <- reactive({
    if(!is.null(hot_to_r(input$db_entries))){
      table <- hot_to_r(input$db_entries)
      select(table, 1:(12 + nrow(DF1$cust_var)))
    }
  })
  
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
  
  # Function to compute Hamming distance between two vectors
  hamming_distance <- function(x, y) {
    sum(x != y)
  }
  
  hamming_distance_ignore <- function(x, y) {
    sum( (x != y) & !is.na(x) & !is.na(y) )
  }
  
  hamming_distance_category <- function(x, y) {
    sum( ( (x != y) | (is.na(x) & !is.na(y)) | (!is.na(x) & is.na(y)) ) & !(is.na(x) & is.na(y))  )
  }
  
  # Function to determine entry table height
  table_height <- reactive({
    if (input$table_height == TRUE) {
      NULL
    } else {900}
  })
  
  # Function to determine distance matrix height
  distancematrix_height <- reactive({
    if(DF1$distancematrix_nrow > 33) {
      800
    } else {NULL}
  })
  
  # Function to missing value table height
  miss.val.height <- reactive({
    if(input$miss_val_height == TRUE) {
      NULL
    } else {800}
  })
  
  #Function to check custom variable classes
  column_classes <- function(df) {
    sapply(df, function(x) {
      if (class(x) == "numeric") {
        return("(cont.)")
      } else if (class(x) == "character") {
        return("(categ.)")
      } else {
        return(class(x))
      }
    })
  }
  
  # Function to check single typing log file
  check_new_entry <- reactive({
    invalidateLater(5000, session)
    Database <-
      readRDS(paste0(
        DF1$database, "/",
        gsub(" ", "_", DF1$scheme),
        "/Typing.rds"
      ))
    
    if(nrow(DF1$data) < nrow(Database[["Typing"]])) {
      TRUE
    } else {
      FALSE
    }
  })
  
  #### Render Entry Table Highlights ----
  
  diff_allele <- reactive({
    if (!is.null(DF1$data) & !is.null(input$compare_select) & !is.null(DF1$cust_var)) {
      var_alleles(select(DF1$data, input$compare_select)) + (12 + nrow(DF1$cust_var))
    }
  })
  
  true_rows <- reactive({
    if (!is.null(DF1$data)) {
      which(DF1$data$Include == TRUE, )
    }
  })
  
  duplicated_rows <- reactive({
    if (!is.null(DF1$meta)) {
      which(duplicated(DF1$meta$`Assembly Name`) | duplicated(DF1$meta$`Assembly Name`, fromLast = TRUE))
    }
  })
  
  ## Startup ----
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  shinyjs::removeClass(selector = "body", class = "sidebar-toggle")
  
  renderstart <- reactiveValues(sidebar = TRUE, header = TRUE)
  
  DF1 <- reactiveValues(data = NULL, block_db = FALSE, load_selected = TRUE)
  
  DF1$no_na_switch <- FALSE
  DF1$first_look <- FALSE
  
  if(paste0(getwd(), "/execute/last_db.rds") %in% dir_ls(paste0(getwd(), "/execute"))) {
    DF1$last_db <- TRUE
  }
  
  # Locate local Database
  observe({
    shinyDirChoose(input,
                   "db_location",
                   roots = c(wd = "/home"),
                   session = session)
    
    if(!is.null(DF1$select_new)) {
      if(DF1$select_new == FALSE) {
        if(DF1$block_db == FALSE) {
          DF1$database <- as.character(
            parseDirPath(
              roots = c(wd = "/home"), 
              input$db_location
            )
          )
        }
        
      } else if (DF1$select_new ==  TRUE) {
        DF1$database <- paste0(DF1$new_database, "/Database")
        
      }
    } else {
      if(!is.null(DF1$last_db) & file.exists(paste0(getwd(), "/execute/last_db.rds")) & dir_exists(readRDS(paste0(getwd(), "/execute/last_db.rds")))){
        DF1$database <- readRDS(paste0(getwd(), "/execute/last_db.rds"))
        
        # Logical any local database present 
        DF1$exist <- (length(dir_ls(DF1$database)) == 0)
        
        # List of local schemes available
        DF1$available <- gsub("_", " ", basename(dir_ls(DF1$database)))
      }
    }
  })
  
  if (!paste0(getwd(), "/execute/script_log.txt") %in% dir_ls(paste0(getwd(), "/execute"))) {
    
    system(paste("chmod +x", paste0(getwd(), "/execute", "/make_log.sh")))
    system(paste0(getwd(), "/execute", "/make_log.sh"), wait = TRUE)
  }
  
  # Typing reactive values
  typing_reactive <- reactiveValues(table = data.frame(), single_path = data.frame(), 
                                    progress = 0, progress_pct = 0, progress_format_start = 0, 
                                    progress_format_end = 0)
  
  typing_reactive$last_success <- "0"
  
  # Typing feedback values
  if(file.exists(paste0(getwd(), "/execute/script_log.txt"))) {
    if(tail(readLines(paste0(getwd(), "/execute/script_log.txt")), 1)!= "0") {
      typing_reactive$failures <- sum(str_detect(readLines(paste0(getwd(), "/execute/script_log.txt"), warn = FALSE), "failed"))
      typing_reactive$successes <- sum(str_detect(readLines(paste0(getwd(), "/execute/script_log.txt"), warn = FALSE), "Successful"))
      typing_reactive$last_scheme <- gsub("_", " ", sub(".*with (.*?) scheme.*", "\\1", readLines(paste0(getwd(), "/execute/script_log.txt"))[1]))
    }
  }
  
  ### Landing page ----
  observe({
    if (renderstart$sidebar == FALSE) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      shinyjs::addClass(selector = "body", class = "sidebar-toggle")
    }
  })
  
  output$start_message <- renderUI({
    if(!is.null(typing_reactive$last_scheme)) {
      column(
        width = 12, 
        align = "center",
        br(), br(), br(), br(), br(), br(),
        div( 
          class = "image",
          imageOutput("imageOutput")
        ),
        br(), br(), br(),
        p(
          tags$span(
            style='color: white; font-size: 16px;',
            HTML(
              paste0(
                'Pending multi typing for ', 
                typing_reactive$last_scheme, 
                "."
              )
            )
          )
        ),
        br(), br(),
        fluidRow(
          column(
            width = 12,
            align = "center",
            uiOutput("load_db"),
            br(), br(), br(), br(), br(), br(), br()
          )
        )
      )
    } else {
      column(
        width = 12, 
        align = "center",
        br(), br(), br(), br(), br(), br(),
        div( 
          class = "image",
          imageOutput("imageOutput")
        ),
        br(), br(), br(),
        p(
          HTML(
            paste(
              tags$span(style='color: white; font-size: 16px;', 'Proceed by loading a compatible local database or create a new one.')
            )
          )
        ),
        br(), 
        fluidRow(
          column(
            width = 6,
            align = "right",
            shinyDirButton(
              "db_location",
              "Browse",
              icon = icon("folder-open"),
              title = "Locate the database folder",
              buttonType = "default"
            )
          ),
          column(
            width = 6,
            align = "left",
            shinyDirButton(
              "create_new_db",
              "Create New",
              icon = icon("plus"),
              title = "Choose location for new PhyloTrace database",
              buttonType = "default"
            )
          )
        ),
        br(), br(),
        fluidRow(
          column(
            width = 12,
            align = "center",
            uiOutput("load_db"),
            br(), br(), br(), br(), br(), br(), br()
          )
        )
      )
    }
  })
  
  # User selection new db or load db
  observeEvent(input$create_new_db, {
    DF1$select_new <- TRUE
  })
  
  observeEvent(input$db_location, {
    DF1$select_new <- FALSE
  })
    
    output$load_db <- renderUI({
      if(!is.null(DF1$select_new)) {
        if(length(DF1$new_database) > 0 & DF1$select_new) {
          column(
            width = 12,
            p(
              tags$span(
                style='color: white; font-size: 15px;',
                HTML(
                  paste(
                    'New database will be created in',
                    DF1$new_database
                  )
                )
              )
            ),
            br(),
            actionButton(
              "load",
              "Create"
            )
          )
        } else if(length(DF1$available) > 0 & !(DF1$select_new)) {
          if(any(!(gsub(" ", "_", DF1$available) %in% schemes))) {
            column(
              width = 12,
              p(
                tags$span(
                  style='color: white; font-size: 15px; font-style: italic;',
                  HTML(
                    paste('Selected:', DF1$database)
                  )
                )
              ),
              uiOutput("scheme_db"),
              br(), 
              p(
                HTML(
                  paste(
                    tags$span(style='color: white; font-size: 13px; font-style: italic;', 
                              'Warning: Folder contains invalid elements.')
                  )
                )
              ),
              br(),
              actionButton(
                "load",
                "Load"
              )
            )
          } else {
            column(
              width = 12,
              p(
                tags$span(
                  style='color: white; font-size: 15px; font-style: italic;',
                  HTML(
                    paste('Selected:', DF1$database)
                  )
                )
              ),
              uiOutput("scheme_db"),
              br(), br(),
              actionButton(
                "load",
                "Load"
              )
            )
          }
        } 
      } else if((!is.null(DF1$last_db)) & (!is.null(DF1$available))) {
        if (DF1$last_db == TRUE & (length(DF1$available) > 0)) {
          if(any(!(gsub(" ", "_", DF1$available) %in% schemes))) {
            column(
              width = 12,
              p(
                tags$span(
                  style='color: white; font-size: 15px; font-style: italic;',
                  HTML(
                    paste('Last used:', DF1$database)
                  )
                )
              ),
              uiOutput("scheme_db"),
              br(), 
              p(
                HTML(
                  paste(
                    tags$span(style='color: white; font-size: 13px; font-style: italic;', 
                              'Warning: Folder contains invalid elements.')
                  )
                )
              ),
              br(),
              actionButton(
                "load",
                "Load"
              )
            )
          } else {
            column(
              width = 12,
              p(
                tags$span(
                  style='color: white; font-size: 15px; font-style: italic;',
                  HTML(
                    paste('Last used:', DF1$database)
                  )
                )
              ),
              uiOutput("scheme_db"),
              br(), br(),
              actionButton(
                "load",
                "Load"
              )
            )
          }
        } else if (DF1$last_db == TRUE & (length(DF1$available) == 0)) {
          column(
            width = 12,
            p(
              tags$span(
                style='color: white; font-size: 15px; font-style: italic;',
                HTML(
                  paste('Last used:', DF1$database)
                )
              )
            ),
            br(),
            actionButton(
              "load",
              "Load"
            )
          )
        }
      }
    })
  
  observeEvent(input$reload_db, {
    if(tail(readLines(paste0(getwd(), "/execute/script_log.txt")), 1)!= "0") {
      show_toast(
        title = "Pending Multi Typing",
        type = "warning",
        position = "top-end",
        timer = 6000,
        width = "500px"
      )
    } else if(readLines(paste0(getwd(), "/execute", "/progress.fifo"))[1] != "0") {
      show_toast(
        title = "Pending Single Typing",
        type = "warning",
        position = "top-end",
        timer = 6000,
        width = "500px"
      )
    } else {
      showModal(
        modalDialog(
          selectInput(
            "scheme_db",
            label = "",
            choices = DF1$available,
            selected = DF1$scheme),
          title = "Select a local database to load.",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("load", "Load", class = "btn btn-default")
          )
        )
      )
    }
  })
  
  output$imageOutput <- renderImage({
    # Path to your PNG image with a transparent background
    image_path <- paste0(getwd(), "/www/PhyloTrace.png")
    
    # Use HTML to display the image with the <img> tag
    list(src = image_path,
         height = 180)
  }, deleteFile = FALSE)
  
  ### Startup event ----
  
  observeEvent(input$load, {
    
    # Load app elements based on database availability and missing value presence
    if(!is.null(DF1$select_new)) {
      if(DF1$select_new & (paste0(DF1$new_database, "/Database") %in% dir_ls(DF1$new_database))) {
        show_toast(
          title = "Directory already contains a database",
          type = "error",
          width = "500px",
          position = "top-end",
          timer = 6000
        )
        DF1$load_selected <- FALSE
        
      } else if(DF1$select_new | (DF1$select_new == FALSE & is.null(input$scheme_db))) {
        
        DF1$check_new_entries <- TRUE
        
        DF1$data <- NULL
        
        DF1$meta <- NULL
        
        DF1$meta_true <- NULL
        
        DF1$allelic_profile <- NULL
        
        DF1$allelic_profile_true <- NULL
        
        DF1$scheme <- input$scheme_db
        
        # null Distance matrix, entry table and plots
        output$db_distancematrix <- NULL 
        output$db_entries_table <- NULL
        output$tree_mst <- NULL
        output$tree_nj <- NULL
        output$tree_upgma <- NULL
        
        # null report values
        reportVAR$report_list_mst <- list()
        reportVAR$report_list_nj <- list()
        reportVAR$report_list_upgma <- list()
        
        # null plots
        plot_loc$nj <- NULL
        plot_loc$upgma <- NULL
        plot_loc$ggraph_1 <- NULL
        
        removeModal()
        
        #### Render Menu Items ----
        
        renderstart$sidebar <- FALSE
        renderstart$header <- FALSE
        
        output$menu_sep1 <- renderUI(hr())
        output$menu_sep2 <- renderUI(hr())
        
        # Hide start message
        output$start_message <- NULL
        output$start_message_no_db <- NULL
        
        DF1$load_selected <- FALSE
        
        
        
        # Declare database path
        DF1$database <- paste0(DF1$new_database, "/Database")
        
        # Set database availability screening variables to present database
        DF1$block_db <- TRUE
        DF1$select_new <- FALSE
        
        # Render menu with Add Scheme as start tab and no Missing values tab
        output$menu <- renderMenu(
          sidebarMenu(
            menuItem(
              text = "Database Browser",
              tabName = "database",
              icon = icon("hard-drive"),
              startExpanded = TRUE,
              menuSubItem(
                text = "Browse Entries",
                tabName = "db_browse_entries"
              ),
              menuSubItem(
                text = "Scheme Info",
                tabName = "db_schemeinfo"
              ),
              menuSubItem(
                text = "Distance Matrix",
                tabName = "db_distmatrix"
              )
            ),
            menuItem(
              text = "Add Scheme",
              tabName = "init",
              icon = icon("plus"),
              selected = TRUE
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
            )
          )
        )
        
        # Dont render these elements
        output$db_no_entries <- NULL
        output$distancematrix_no_entries <- NULL
        output$db_entries <- NULL
        output$edit_index <- NULL
        output$edit_scheme_d <- NULL
        output$edit_entries <- NULL
        output$compare_select <- NULL
        output$delete_select <- NULL
        output$del_bttn <- NULL
        output$compare_allele_box <- NULL
        output$download_entries <- NULL
        output$missing_values <- NULL
        output$delete_box <- NULL
        output$missing_values_sidebar <- NULL
        output$distmatrix_sidebar <- NULL
        output$download_scheme_info <- NULL
        output$download_loci <- NULL
        output$entry_table_controls <- NULL
      }
    }
    
    if(DF1$load_selected == TRUE) {
      
      if(gsub(" ", "_", input$scheme_db) %in% schemes) { #Check if selected scheme valid
        
        # Save database path for next start
        saveRDS(DF1$database, paste0(getwd(), "/execute/last_db.rds"))
        
        DF1$check_new_entries <- TRUE
        
        DF1$data <- NULL
        
        DF1$meta <- NULL
        
        DF1$meta_true <- NULL
        
        DF1$allelic_profile <- NULL
        
        DF1$allelic_profile_true <- NULL
        
        DF1$scheme <- input$scheme_db
        
        # null Distance matrix, entry table and plots
        output$db_distancematrix <- NULL 
        output$db_entries_table <- NULL
        output$tree_mst <- NULL
        output$tree_nj <- NULL
        output$tree_upgma <- NULL
        
        # null report values
        reportVAR$report_list_mst <- list()
        reportVAR$report_list_nj <- list()
        reportVAR$report_list_upgma <- list()
        
        # null plots
        plot_loc$nj <- NULL
        plot_loc$upgma <- NULL
        plot_loc$ggraph_1 <- NULL
        
        removeModal()
        
        #### Render Menu Items ----
        
        renderstart$sidebar <- FALSE
        renderstart$header <- FALSE
        
        output$menu_sep1 <- renderUI(hr())
        output$menu_sep2 <- renderUI(hr())
        
        # Hide start message
        output$start_message <- NULL
        output$start_message_no_db <- NULL
        
        if(any(grepl(gsub(" ", "_", DF1$scheme), dir_ls(DF1$database)))) {
          
          if(!any(grepl("alleles", dir_ls(paste0(
            DF1$database, "/", 
            gsub(" ", "_", DF1$scheme)))))) {
            
            # Show message that loci files are missing
            showModal(
              modalDialog(
                paste0("Whoops! No loci files are present in the local ", 
                       DF1$scheme, 
                       " folder. Download the scheme again (no influence on already typed assemblies)."),
                title = "Local Database Error",
                fade = TRUE,
                easyClose = TRUE,
                footer = tagList(
                  modalButton("Okay")
                )
              )
            )
            
            # Render menu with Add Scheme as start tab
            output$menu <- renderMenu(
              sidebarMenu(
                menuItem(
                  text = "Database Browser",
                  tabName = "database",
                  icon = icon("hard-drive"),
                  startExpanded = TRUE,
                  menuSubItem(
                    text = "Browse Entries",
                    tabName = "db_browse_entries"
                  ),
                  menuSubItem(
                    text = "Scheme Info",
                    tabName = "db_schemeinfo"
                  ),
                  menuSubItem(
                    text = "Distance Matrix",
                    tabName = "db_distmatrix"
                  ),
                  menuSubItem(
                    text = "Missing Values",
                    tabName = "db_missing_values",
                    icon = icon("triangle-exclamation")
                  )
                ),
                menuItem(
                  text = "Add Scheme",
                  tabName = "init",
                  icon = icon("plus"),
                  selected = TRUE
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
                )
              )
            )
          } else if (!any(grepl("scheme_info.html", dir_ls(paste0(
            DF1$database, "/", 
            gsub(" ", "_", DF1$scheme)))))) {
            
            output$download_scheme_info <- NULL
            
            # Show message that scheme info is missing
            showModal(
              modalDialog(
                paste0("Whoops! Scheme info of the local ", 
                       DF1$scheme, 
                       " database is missing. Download the scheme again (no influence on already typed assemblies)."),
                title = "Local Database Error",
                fade = TRUE,
                easyClose = TRUE,
                footer = tagList(
                  modalButton("Okay")
                )
              )
            )
            
            # Render menu with Add Scheme as start tab
            output$menu <- renderMenu(
              sidebarMenu(
                menuItem(
                  text = "Database Browser",
                  tabName = "database",
                  icon = icon("hard-drive"),
                  startExpanded = TRUE,
                  menuSubItem(
                    text = "Browse Entries",
                    tabName = "db_browse_entries"
                  ),
                  menuSubItem(
                    text = "Scheme Info",
                    tabName = "db_schemeinfo"
                  ),
                  menuSubItem(
                    text = "Distance Matrix",
                    tabName = "db_distmatrix"
                  ),
                  menuSubItem(
                    text = "Missing Values",
                    tabName = "db_missing_values",
                    icon = icon("triangle-exclamation")
                  )
                ),
                menuItem(
                  text = "Add Scheme",
                  tabName = "init",
                  icon = icon("plus"),
                  selected = TRUE
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
                )
              )
            )
            
          } else if (!any(grepl("targets.csv", dir_ls(paste0(
            DF1$database, "/", 
            gsub(" ", "_", DF1$scheme)))))) {
            
            # Dont render target download button
            output$download_loci <- NULL
            
            # Show message that scheme info is missing
            showModal(
              modalDialog(
                paste0("Whoops! Loci info of the local ", 
                       DF1$scheme, 
                       " database is missing. Download the scheme again (no influence on already typed assemblies)."),
                title = "Local Database Error",
                fade = TRUE,
                easyClose = TRUE,
                footer = tagList(
                  modalButton("Okay")
                )
              )
            )
            
            # Render menu with Add Scheme as start tab
            output$menu <- renderMenu(
              sidebarMenu(
                menuItem(
                  text = "Database Browser",
                  tabName = "database",
                  icon = icon("hard-drive"),
                  startExpanded = TRUE,
                  menuSubItem(
                    text = "Browse Entries",
                    tabName = "db_browse_entries"
                  ),
                  menuSubItem(
                    text = "Scheme Info",
                    tabName = "db_schemeinfo"
                  ),
                  menuSubItem(
                    text = "Distance Matrix",
                    tabName = "db_distmatrix"
                  ),
                  menuSubItem(
                    text = "Missing Values",
                    tabName = "db_missing_values",
                    icon = icon("triangle-exclamation")
                  )
                ),
                menuItem(
                  text = "Add Scheme",
                  tabName = "init",
                  icon = icon("plus"),
                  selected = TRUE
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
                )
              )
            )
            
          } else {
            # Render scheme selector in sidebar
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
                          tags$span(style='color: white; font-size: 14px;', strong("Selected scheme:"))
                        )
                      )
                    ),
                    p(
                      HTML(
                        paste(
                          tags$span(style='color: white; font-size: 13px; font-style: italic', DF1$scheme)
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
            
            # Produce Scheme Info Table
            schemeinfo <-
              read_html(paste0(
                DF1$database, "/",
                gsub(" ", "_", DF1$scheme),
                "/scheme_info.html"
              )) %>%
              html_table(header = FALSE) %>%
              as.data.frame(stringsAsFactors = FALSE)
            names(schemeinfo) <- NULL
            DF1$schemeinfo <- schemeinfo
            number_loci <- as.vector(DF1$schemeinfo[6, 2])
            number_loci <- as.numeric(gsub(",", "", number_loci))
            
            # Produce Loci Info table
            DF1$loci_info <- read.csv(
              paste0(
                DF1$database, "/",
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
            
            # Check if number of loci/fastq-files of alleles is coherent with number of targets in scheme
            if(number_loci != length(dir_ls(paste0(DF1$database, "/", gsub(" ", "_", DF1$scheme), "/", gsub(" ", "_", DF1$scheme), "_alleles")))) {
              
              # Show message that loci files are missing
              showModal(
                modalDialog(
                  paste0("Whoops! Some loci files are missing in the local ", 
                         DF1$scheme, 
                         " folder. Download the scheme again (no influence on already typed assemblies)."),
                  title = "Local Database Error",
                  fade = TRUE,
                  easyClose = TRUE,
                  footer = tagList(
                    modalButton("Okay")
                  )
                )
              )
              
              # Render menu with Add Scheme as start tab
              output$menu <- renderMenu(
                sidebarMenu(
                  menuItem(
                    text = "Database Browser",
                    tabName = "database",
                    icon = icon("hard-drive"),
                    startExpanded = TRUE,
                    menuSubItem(
                      text = "Browse Entries",
                      tabName = "db_browse_entries"
                    ),
                    menuSubItem(
                      text = "Scheme Info",
                      tabName = "db_schemeinfo"
                    ),
                    menuSubItem(
                      text = "Distance Matrix",
                      tabName = "db_distmatrix"
                    ),
                    menuSubItem(
                      text = "Missing Values",
                      tabName = "db_missing_values",
                      icon = icon("triangle-exclamation")
                    )
                  ),
                  menuItem(
                    text = "Add Scheme",
                    tabName = "init",
                    icon = icon("plus"),
                    selected = TRUE
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
                  )
                )
              )
              
            } else {
              ###### Alle checks bestanden -> Laden der DTB
              # If typed entries present
              if (any(grepl("Typing.rds", dir_ls(paste0(
                DF1$database, "/", gsub(" ", "_", DF1$scheme)
              ))))) {
                
                # Load database from files  
                Database <-
                  readRDS(paste0(
                    DF1$database, "/",
                    gsub(" ", "_", DF1$scheme),
                    "/Typing.rds"
                  ))
                
                DF1$data <- Database[["Typing"]]
                
                if(!is.null(DF1$data)){
                  if ((ncol(DF1$data)-12) != as.numeric(gsub(",", "", as.vector(DF1$schemeinfo[6, 2])))) {
                    cust_var <- select(DF1$data, 13:(ncol(DF1$data) - as.numeric(gsub(",", "", as.vector(DF1$schemeinfo[6, 2])))))
                    DF1$cust_var <- data.frame(Variable = names(cust_var), Type = column_classes(cust_var))
                  } else {
                    DF1$cust_var <- data.frame()
                  }
                }
                
                DF1$change <- FALSE
                
                DF1$meta <- select(DF1$data, 1:(12 + nrow(DF1$cust_var)))
                
                DF1$meta_true <- DF1$meta[which(DF1$data$Include == TRUE),]
                
                DF1$allelic_profile <- select(DF1$data, -(1:(12 + nrow(DF1$cust_var))))
                
                DF1$allelic_profile_true <- DF1$allelic_profile[which(DF1$data$Include == TRUE),]
                
                # Null pipe 
                system(paste("chmod +x", paste0(getwd(), "/execute/zero_pipe.sh")))
                
                system(paste(paste0(getwd(), "/execute/zero_pipe.sh")), wait = TRUE)
                
                # Reset other reactive typing variables
                typing_reactive$progress_format_end <- 0 
                
                typing_reactive$progress_format_start <- 0
                
                typing_reactive$pending_format <- 0
                
                typing_reactive$entry_added <- 0
                
                typing_reactive$progress <- 0
                
                typing_reactive$progress_pct <- 0
                
                typing_reactive$progress_format <- 900000
                
                output$single_typing_progress <- NULL
                
                output$typing_fin <- NULL
                
                output$typing_formatting <- NULL
                
                typing_reactive$single_path <- data.frame()
                
                # Null multi typing feedback variable
                typing_reactive$reset <- TRUE
                
                # Check need for new missing vlaue display
                if(DF1$first_look == TRUE) {
                  if(sum(apply(DF1$data, 1, anyNA)) >= 1) {
                    DF1$no_na_switch <- TRUE
                  } else {
                    DF1$no_na_switch <- FALSE
                  }
                }
                
                DF1$first_look <- TRUE
                
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
                          tags$span(style='color: white; font-size: 15px; margin-bottom: 0px', 'Select Assembly File')
                        )
                      )
                    ),
                    shinyFilesButton(
                      "genome_file",
                      "Browse",
                      icon = icon("folder-open"),
                      title = "Please select the genome in .fasta/.fna/.fa format:",
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
                
                if(!anyNA(DF1$allelic_profile)) {
                  
                  # no NA's -> dont render missing values sidebar elements
                  output$missing_values_sidebar <- NULL
                  
                  # Render menu if no NA's present
                  output$menu <- renderMenu(
                    sidebarMenu(
                      menuItem(
                        text = "Database Browser",
                        tabName = "database",
                        icon = icon("hard-drive"),
                        startExpanded = TRUE,
                        menuSubItem(
                          text = "Browse Entries",
                          tabName = "db_browse_entries"
                        ),
                        menuSubItem(
                          text = "Scheme Info",
                          tabName = "db_schemeinfo"
                        ),
                        menuSubItem(
                          text = "Distance Matrix",
                          tabName = "db_distmatrix"
                        )
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
                      )
                    )
                  )
                } else {
                  output$menu <- renderMenu(
                    sidebarMenu(
                      menuItem(
                        text = "Database Browser",
                        tabName = "database",
                        icon = icon("hard-drive"),
                        startExpanded = TRUE,
                        menuSubItem(
                          text = "Browse Entries",
                          tabName = "db_browse_entries",
                          selected = TRUE
                        ),
                        menuSubItem(
                          text = "Scheme Info",
                          tabName = "db_schemeinfo"
                        ),
                        menuSubItem(
                          text = "Distance Matrix",
                          tabName = "db_distmatrix"
                        ),
                        menuSubItem(
                          text = "Missing Values",
                          tabName = "db_missing_values",
                          icon = icon("triangle-exclamation")
                        )
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
                      )
                    )
                  )
                }
                # Render custom variable display
                
                output$show_cust_var <- renderTable(
                  DF1$cust_var,
                  width = "100%"
                )
                
                # render visualization sidebar elements
                
                output$visualization_sidebar <- renderUI({
                  if(!class(DF1$data) == "NULL") {
                    column(
                      width = 12,
                      br(),
                      fluidRow(
                        column(1),
                        column(
                          width = 11,
                          align = "left",
                          prettyRadioButtons(
                            "tree_algo",
                            choices = c("Minimum-Spanning", "Neighbour-Joining", "UPGMA"),
                            label = ""
                          ),
                        )
                      ),
                      br(),
                      fluidRow(
                        column(
                          width = 12,
                          align = "center",
                          actionButton(
                            "create_tree",
                            "",
                            width = "100%",
                            icon = icon(
                              name = NULL,
                              style = "
                background: url('phylo.png');
                background-size: contain;
                background-position: center;
                background-repeat: no-repeat;
                height: 32px;
                display: block;
              "
                            )
                          )
                        )
                      ), 
                      br(),
                      hr(),
                      conditionalPanel(
                        "input.tree_algo=='Minimum-Spanning'",
                        fluidRow(
                          column(
                            width = 12,
                            align = "left",
                            br(),
                            HTML(
                              paste(
                                tags$span(style='color: white; font-size: 16px; margin-left: 15px', "Sizing")
                              )
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 12,
                            radioGroupButtons(
                              "mst_ratio",
                              "",
                              choiceNames = c("16:10", "16:9", "4:3"),
                              choiceValues = c((16/10), (16/9), (4/3)),
                              width = "100%"
                            ),
                            br(),
                            sliderTextInput(
                              "mst_scale",
                              "",
                              choices = 500:1200,
                              selected = 800,
                              width = "95%"
                            )
                          )
                        ),
                        br(),
                        hr(),
                        fluidRow(
                          column(
                            width = 12,
                            align = "left",
                            br(),
                            HTML(
                              paste(
                                tags$span(style='color: white; font-size: 16px; margin-left: 15px', "Save Plot")
                              )
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 8,
                            div(
                              class = "format",
                              selectInput(
                                inputId = "mst_plot_format",
                                label = "",
                                choices = c("html", 
                                            "jpeg", "png", "bmp")
                              )
                            )
                          ),
                          column(
                            width = 4,
                            align = "left",
                            conditionalPanel(
                              "input.mst_plot_format=='jpeg'",
                              actionBttn(
                                "save_plot_jpeg",
                                style = "simple",
                                label = "",
                                size = "sm",
                                icon = icon("download"),
                                color = "primary"
                              )  
                            ),
                            conditionalPanel(
                              "input.mst_plot_format=='png'",
                              actionBttn(
                                "save_plot_png",
                                style = "simple",
                                label = "",
                                size = "sm",
                                icon = icon("download"),
                                color = "primary"
                              )
                            ),
                            conditionalPanel(
                              "input.mst_plot_format=='bmp'",
                              actionBttn(
                                "save_plot_bmp",
                                style = "simple",
                                label = "",
                                size = "sm",
                                icon = icon("download"),
                                color = "primary"
                              )  
                            ),
                            conditionalPanel(
                              "input.mst_plot_format=='html'",
                              downloadBttn(
                                "save_plot_html",
                                style = "simple",
                                label = "",
                                size = "sm",
                                icon = icon("download"),
                                color = "primary"
                              )
                            )
                          )
                        )
                      ),
                      conditionalPanel(
                        "input.tree_algo=='Neighbour-Joining'",
                        fluidRow(
                          column(
                            width = 12,
                            align = "left",
                            br(),
                            HTML(
                              paste(
                                tags$span(style='color: white; font-size: 16px; margin-left: 15px', "Sizing")
                              )
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 12,
                            radioGroupButtons(
                              "nj_ratio",
                              "",
                              choiceNames = c("16:10", "16:9", "4:3"),
                              choiceValues = c((16/10), (16/9), (4/3)),
                              width = "100%"
                            ),
                            br(),
                            sliderTextInput(
                              "nj_scale",
                              "",
                              choices = 500:1200,
                              selected = 800,
                              width = "95%"
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 3,
                            align = "left",
                            br(),
                            HTML(
                              paste(
                                tags$span(style='color: white; font-size: 14px; position: relative; bottom: -28px; margin-left: 15px ', "Zoom")
                              )
                            )
                          ),
                          column(
                            width = 8,
                            align = "right",
                            br(),
                            sliderTextInput(
                              "nj_zoom",
                              label = NULL,
                              choices = seq(0.5, 1.5, 0.05),
                              selected = 0.95,
                              hide_min_max = TRUE
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 5,
                            align = "left",
                            div(
                              class = "arrow_move",
                              numericInput(
                                "nj_v",
                                label = h5("Y", style = "color:white; margin-bottom: -6px; margin-left: 10px"),
                                min = -0.5,
                                max = 0.5,
                                step = 0.01,
                                value = 0,
                                width = "80px"
                              )
                            )
                          ),
                          column(
                            width = 5,
                            align = "left",
                            div(
                              class = "arrow_move",
                              numericInput(
                                "nj_h",
                                label = h5("X", style = "color:white; margin-bottom: -6px; margin-left: 10px"),
                                min = -0.5,
                                max = 0.5,
                                step = 0.01,
                                value = 0,
                                width = "80px"
                              )
                            )
                          )
                        ),
                        br(),
                        hr(),
                        fluidRow(
                          column(
                            width = 12,
                            align = "left",
                            br(),
                            HTML(
                              paste(
                                tags$span(style='color: white; font-size: 16px; margin-left: 15px', "Save Plot")
                              )
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 8,
                            div(
                              class = "format",
                              selectInput(
                                inputId = "filetype_nj",
                                label = "",
                                choices = c("jpeg", "png", "bmp", "svg")
                              )
                            )
                          ),
                          column(
                            width = 4,
                            align = "left",
                            downloadBttn(
                              "download_nj",
                              style = "simple",
                              label = "",
                              size = "sm",
                              icon = icon("download"),
                              color = "primary"
                            )
                          )
                        )
                      ),
                      conditionalPanel(
                        "input.tree_algo=='UPGMA'",
                        fluidRow(
                          column(
                            width = 12,
                            align = "left",
                            br(),
                            HTML(
                              paste(
                                tags$span(style='color: white; font-size: 16px; margin-left: 15px', "Sizing")
                              )
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 12,
                            radioGroupButtons(
                              "upgma_ratio",
                              "",
                              choiceNames = c("16:10", "16:9", "4:3"),
                              choiceValues = c((16/10), (16/9), (4/3)),
                              width = "100%"
                            ),
                            br(),
                            sliderTextInput(
                              "upgma_scale",
                              "",
                              choices = 500:1200,
                              selected = 800,
                              width = "100%"
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 3,
                            align = "left",
                            br(),
                            HTML(
                              paste(
                                tags$span(style='color: white; font-size: 14px; position: relative; bottom: -28px; margin-left: 15px ', "Zoom")
                              )
                            )
                          ),
                          column(
                            width = 9,
                            align = "right",
                            br(),
                            sliderTextInput(
                              "upgma_zoom",
                              label = NULL,
                              choices = seq(0.5, 1.5, 0.05),
                              selected = 0.95,
                              hide_min_max = TRUE
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 5,
                            align = "left",
                            div(
                              class = "arrow_move",
                              numericInput(
                                "upgma_v",
                                label = h5("Y", style = "color:white; margin-bottom: -6px; margin-left: 10px"),
                                min = -0.5,
                                max = 0.5,
                                step = 0.01,
                                value = 0,
                                width = "80px"
                              )
                            )
                          ),
                          column(
                            width = 5,
                            align = "left",
                            div(
                              class = "arrow_move",
                              numericInput(
                                "upgma_h",
                                label = h5("X", style = "color:white; margin-bottom: -6px; margin-left: 10px"),
                                min = -0.5,
                                max = 0.5,
                                step = 0.01,
                                value = 0,
                                width = "80px"
                              )
                            )
                          )
                        ),
                        br(),
                        hr(),
                        fluidRow(
                          column(
                            width = 12,
                            align = "left",
                            br(),
                            HTML(
                              paste(
                                tags$span(style='color: white; font-size: 16px; margin-left: 15px', "Save Plot")
                              )
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 8,
                            div(
                              class = "format",
                              selectInput(
                                inputId = "filetype_upgma",
                                label = "",
                                choices = c("jpeg", "png", "bmp", "svg")
                              )
                            )
                          ),
                          column(
                            width = 4,
                            align = "left",
                            downloadBttn(
                              "download_upgma",
                              style = "simple",
                              label = "",
                              size = "sm",
                              icon = icon("download"),
                              color = "primary"
                            )
                          )
                        )
                      ),
                      br(),
                      hr(),
                      fluidRow(
                        column(
                          width = 12,
                          align = "left",
                          br(),
                          HTML(
                            paste(
                              tags$span(style='color: white; font-size: 16px; margin-left: 15px', "Download Report")
                            )
                          )
                        )
                      ),
                      fluidRow(
                        column(
                          width = 8,
                          align = "left",
                          checkboxInput(
                            "rep_entrytable",
                            label = h5("Entry table", style = "color:white; position: absolute; top: -6px"),
                            value = TRUE
                          )
                        )
                      ),
                      fluidRow(
                        column(
                          width = 6,
                          align = "left",
                          checkboxInput(
                            "rep_general",
                            label = h5("General", style = "color:white; position: absolute; top: -23px"),
                            value = TRUE
                          )
                        ),
                        column(
                          width = 4,
                          align = "left",
                          dropMenu(
                            actionBttn(
                              "mst_general_menu",
                              label = "",
                              color = "default",
                              size = "sm",
                              style = "material-flat",
                              icon = icon("pen-to-square")
                            ),
                            placement = "top-start",
                            padding = "20px",
                            theme = "translucent",
                            fluidRow(
                              column(
                                width = 3,
                                checkboxInput(
                                  "rep_date_general", 
                                  label = h5("Date", style = "color:white; font-size: 17px; margin-top: 16px;"),
                                  value = TRUE
                                )
                              ),
                              column(
                                width = 7,
                                dateInput(
                                  "mst_date_general_select",
                                  ""
                                )
                              )
                            ),
                            fluidRow(
                              column(
                                width = 3,
                                checkboxInput(
                                  "rep_operator_general", 
                                  label = h5("Operator", style = "color:white; font-size: 17px; margin-top: -1px;"),
                                  value = TRUE
                                )
                              ),
                              column(
                                width = 8,
                                textInput(
                                  "mst_operator_general_select",
                                  ""
                                ) 
                              )
                            ),
                            fluidRow(
                              column(
                                width = 3,
                                checkboxInput(
                                  "rep_institute_general", 
                                  label = h5("Institute", style = "color:white; font-size: 17px; margin-top: -1px;"),
                                  value = TRUE
                                )
                              ),
                              column(
                                width = 8,
                                textInput(
                                  "mst_institute_general_select",
                                  ""
                                ) 
                              )
                            ),
                            fluidRow(
                              column(
                                width = 3,
                                checkboxInput(
                                  "rep_comm_general", 
                                  label = h5("Comment", style = "color:white; font-size: 17px; margin-top: -1px;")
                                )
                              ),
                              column(
                                width = 8,
                                textAreaInput(
                                  inputId = "mst_comm_general_select",
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
                          )
                        )
                      ),
                      fluidRow(
                        column(
                          width = 6,
                          align = "left",
                          checkboxInput(
                            "rep_analysis",
                            label = h5("Analysis", style = "color:white; position: absolute; top: -42px"),
                            value = TRUE
                          )
                        ),
                        column(
                          width = 4,
                          align = "left",
                          dropMenu(
                            actionBttn(
                              "mst_analysis_menu",
                              label = "",
                              color = "default",
                              size = "sm",
                              style = "material-flat",
                              icon = icon("pen-to-square")
                            ),
                            placement = "top-start",
                            padding = "20px",
                            theme = "translucent",
                            fluidRow(
                              column(
                                width = 4,
                                checkboxInput(
                                  "rep_cgmlst_analysis",
                                  label = h5("Scheme", style = "color:white; font-size: 17px; margin-top: 18px"),
                                  value = TRUE
                                )
                              ),
                              column(
                                width = 8,
                                align = "right"
                              )
                            ),
                            fluidRow(
                              column(
                                width = 4,
                                checkboxInput(
                                  "rep_tree_analysis",
                                  label = h5("Tree", style = "color:white; font-size: 17px; margin-top: -1px"),
                                  value = TRUE
                                )
                              ),
                              column(
                                width = 6,
                                align = "right",
                                HTML(
                                  paste(
                                    tags$span(style='color: white; font-size: 15px; font-style: italic; position: relative; top: 21px; right: -23px', 'Tree algorithm')
                                  )
                                )
                              )
                            ),
                            fluidRow(
                              column(
                                width = 4,
                                checkboxInput(
                                  "rep_distance",
                                  label = h5("Distance", style = "color:white; font-size: 17px; margin-top: -1px"),
                                  value = TRUE
                                )
                              ),
                              column(
                                width = 6,
                                align = "right",
                                HTML(
                                  paste(
                                    tags$span(style='color: white; font-size: 15px; font-style: italic; position: relative; top: 21px; right: -23px', 'Distance algorithm')
                                  )
                                )
                              )
                            ),
                            fluidRow(
                              column(
                                width = 7,
                                align = "left",
                                checkboxInput(
                                  "rep_missval",
                                  label = h5("NA handling", style = "color:white; font-size: 17px; margin-top: -1px"),
                                  value = TRUE
                                )
                              ),
                              column(
                                width = 5,
                                align = "right",
                                HTML(
                                  paste(
                                    tags$span(style='color: white; font-size: 15px; font-style: italic; position: relative; top: 21px; right: 31px', 'Missing values')
                                  )
                                )
                              )
                            ),
                            fluidRow(
                              column(
                                width = 4,
                                checkboxInput(
                                  "rep_version",
                                  label = h5("Version", style = "color:white; font-size: 17px; margin-top: -1px"),
                                  value = TRUE
                                )
                              ),
                              column(
                                width = 6,
                                align = "right",
                                HTML(
                                  paste(
                                    tags$span(style='color: white; font-size: 15px; font-style: italic; position: relative; top: 21px; right: -23px', 'Version info')
                                  )
                                )
                              )
                            )
                          )
                        )
                      ),
                      fluidRow(
                        column(
                          width = 8,
                          align = "left",
                          checkboxInput(
                            "rep_plot_report",
                            label = h5("Attach plot", style = "color:white; position: absolute; top: -61px"),
                            value = TRUE
                          )
                        )
                      ),
                      fluidRow(
                        column(
                          width = 12,
                          align = "left",
                          downloadBttn(
                            "download_report",
                            style = "simple",
                            label = "Save",
                            size = "sm",
                            icon = icon("download")
                          )
                        )
                      )
                    )
                  }
                })
                
                # Render entry table sidebar elements
                output$entrytable_sidebar <- renderUI({
                  if(!class(DF1$data) == "NULL") {
                    column(
                      width = 12,
                      align = "center",
                      br(), 
                      fluidRow(
                        column(1),
                        column(
                          width = 10,
                          align = "left",
                          if(nrow(DF1$data) > 40) {
                            checkboxInput(
                              "table_height",
                              "Show full table",
                              value = FALSE
                            )
                          }
                        )
                      ),
                      br(), br(), br(),
                      fluidRow(
                        column(
                          width = 12,
                          HTML(
                            paste(
                              tags$span(style='color: white; font-size: 18px; margin-bottom: 5px', 'Custom Variables')
                            )
                          )
                        )
                      ),
                      br(),
                      fluidRow(
                        column(
                          width = 8,
                          div(
                            class = "textinput_var",
                            textInput(
                              "new_var_name",
                              label = h5("Name", style = "color:white; margin-bottom: 0px"),
                            )
                          )
                        ),
                        column(
                          width = 2,
                          actionButton(
                            "add_new_variable",
                            "",
                            style = "background: green; height: 35px; width: 38px; margin-top: 30px; margin-left: 5px",
                            icon = icon("plus")
                          )
                        )
                      ),
                      fluidRow(
                        column(
                          width = 8,
                          align = "left",
                          div(
                            class = "textinput_var",
                            selectInput(
                              "del_which_var",
                              "",
                              DF1$cust_var$Variable
                            )
                          )
                        ),
                        column(
                          width = 2,
                          align = "left",
                          actionButton(
                            "delete_new_variable",
                            "",
                            style = "background: #FF5964; height: 35px; width: 38px; margin-top: 20px; margin-left: 5px",
                            icon = icon("minus")
                          )
                        )
                      ),
                      br(), br(),
                      fluidRow(
                        column(1),
                        column(
                          width = 11,
                          align = "center",
                          tableOutput("show_cust_var")
                        )
                      )
                    )
                  } 
                })
                
                # Render scheme selector in sidebar
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
                              tags$span(style='color: white; font-size: 14px;', strong("Selected scheme:"))
                            )
                          )
                        ),
                        p(
                          HTML(
                            paste(
                              tags$span(style='color: white; font-size: 13px; font-style: italic', DF1$scheme)
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
                
                # Render missing values sidebar elements
                output$missing_values_sidebar <- renderUI({
                  column(
                    width = 12,
                    fluidRow(
                      column(
                        width = 12,
                        checkboxInput(
                          "miss_val_height",
                          "Show full table",
                          value = FALSE
                        )
                      ),
                      br()
                    ),
                    fluidRow(
                      column(
                        width = 6,
                        HTML(
                          paste(
                            tags$span(style='color: white; font-size: 14px; position: relative; bottom: -23px; right: -15px', 
                                      'Download CSV')
                          )
                        )
                      ),
                      column(
                        width = 4,
                        downloadBttn(
                          "download_na_matrix",
                          style = "simple",
                          label = "",
                          size = "sm",
                          icon = icon("download")
                        )
                      )
                    )
                  )
                })
                
                # Render scheme info download button
                output$download_loci <- renderUI({
                  downloadBttn(
                    "download_loci_info",
                    style = "simple",
                    label = "",
                    size = "sm",
                    icon = icon("download"),
                    color = "primary"
                  )
                })
                
                # Render scheme info download button
                output$download_scheme_info <- renderUI({
                  downloadBttn(
                    "download_schemeinfo",
                    style = "simple",
                    label = "",
                    size = "sm",
                    icon = icon("download"),
                    color = "primary"
                  )
                })
                
                # Render distance matrix sidebar
                output$distmatrix_sidebar <- renderUI({
                  column(
                    width = 12,
                    align = "left",
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        selectInput(
                          "distmatrix_label",
                          label = "",
                          choices = c("Index", "Assembly Name", "Assembly ID"),
                          selected = c("Assembly Name"),
                          width = "100%"
                        ),
                        br()
                      )
                    ),
                    checkboxInput(
                      "distmatrix_true",
                      label = h5("Only included entries", style = "color:white; position: absolute; top: -26px"),
                      value = FALSE
                    ),
                    checkboxInput(
                      "distmatrix_triangle",
                      label = h5("Show upper triangle", style = "color:white; position: absolute; top: -46px"),
                      value = FALSE
                    ),
                    checkboxInput(
                      "distmatrix_diag",
                      label = h5("Show diagonal", style = "color:white; position: absolute; top: -66px"),
                      value = TRUE
                    ),
                    fluidRow(
                      column(
                        width = 6,
                        HTML(
                          paste(
                            tags$span(style='color: white; font-size: 14px; position: relative; bottom: 37px; right: -15px', 
                                      'Download CSV')
                          )
                        )
                      ),
                      column(
                        width = 4,
                        downloadBttn(
                          "download_distmatrix",
                          style = "simple",
                          label = "",
                          size = "sm",
                          icon = icon("download")
                        )
                      )
                    )
                  )
                })
                
                # Render select input to choose displayed loci
                output$compare_select <- renderUI({
                  
                  if(nrow(DF1$data) == 1) {
                    HTML(
                      paste(
                        tags$span(style='color: white; font-size: 15px;', "Type at least two assemblies to compare")
                      )
                    )
                  } else {
                    if(!is.null(input$compare_difference)) {
                      if (input$compare_difference == FALSE) {
                        DF1$allelic_profile
                        pickerInput(
                          inputId = "compare_select",
                          label = "",
                          width = "85%",
                          choices = names(DF1$allelic_profile),
                          selected = names(DF1$allelic_profile)[1:20],
                          options = list(
                            `live-search` = TRUE,
                            `actions-box` = TRUE,
                            size = 10,
                            style = "background-color: white; border-radius: 5px;"
                          ),
                          multiple = TRUE
                        )
                      } else {
                        pickerInput(
                          inputId = "compare_select",
                          label = "",
                          width = "85%",
                          choices = names(DF1$allelic_profile),
                          selected = names(DF1$allelic_profile)[var_alleles(DF1$allelic_profile)],
                          options = list(
                            `live-search` = TRUE,
                            `actions-box` = TRUE,
                            size = 10,
                            style = "background-color: white; border-radius: 5px;"
                          ),
                          multiple = TRUE
                        )
                      }
                    }
                  }
                })
                
                #### Render Entry Data Table ----
                output$db_entries_table <- renderUI({
                  if(!class(DF1$data) == "NULL") {
                    if(between(nrow(DF1$data), 1, 30)) {
                      rHandsontableOutput("db_entries")
                    } else {
                      addSpinner(
                        rHandsontableOutput("db_entries"),
                        spin = "dots",
                        color = "#ffffff"
                      )
                    }
                  }
                })
                
                if (!is.null(DF1$data)) {
                  
                  observe({
                    
                    if (!is.null(DF1$data)) {
                      if (nrow(DF1$data) == 1) {
                        if(!is.null(DF1$data) & !is.null(DF1$cust_var)) {
                          output$db_entries <- renderRHandsontable({
                            rhandsontable(
                              select(DF1$data, 1:(12 + nrow(DF1$cust_var))),
                              rowHeaders = NULL
                            ) %>%
                              hot_col(1, 
                                      readOnly = TRUE,
                                      valign = "htMiddle",
                                      halign = "htCenter") %>%
                              hot_col(3:(12 + nrow(DF1$cust_var)), 
                                      valign = "htMiddle",
                                      halign = "htLeft") %>%
                              hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
                              hot_col(2, type = "checkbox", width = "auto",
                                      valign = "htTop",
                                      halign = "htCenter") %>%
                              hot_context_menu(allowRowEdit = FALSE,
                                               allowColEdit = FALSE,
                                               allowReadOnly = FALSE) %>%
                              hot_rows(fixedRowsTop = 0)
                          })
                        }
                      } else if (between(nrow(DF1$data), 1, 40)) {
                        if (length(input$compare_select) > 0) {
                          if(!is.null(DF1$data) & !is.null(DF1$cust_var) & !is.null(input$compare_select)) {
                            output$db_entries <- renderRHandsontable({
                              row_highlight <- true_rows()-1
                              rhandsontable(
                                select(DF1$data, 1:(12 + nrow(DF1$cust_var)), input$compare_select),
                                col_highlight = diff_allele()-1,
                                rowHeaders = NULL,
                                duplicated_highlight = duplicated_rows()-1,
                                row_highlight = row_highlight
                              ) %>%
                                hot_col((12 + nrow(DF1$cust_var)):((12 + nrow(DF1$cust_var))+length(input$compare_select)), 
                                        valign = "htMiddle",
                                        halign = "htCenter") %>%
                                hot_col(1, 
                                        readOnly = TRUE,
                                        valign = "htMiddle",
                                        halign = "htCenter") %>%
                                hot_col(3:(12 + nrow(DF1$cust_var)), 
                                        valign = "htMiddle",
                                        halign = "htLeft") %>%
                                hot_context_menu(allowRowEdit = FALSE,
                                                 allowColEdit = FALSE,
                                                 allowReadOnly = FALSE) %>%
                                hot_col(2, type = "checkbox", width = "auto",
                                        valign = "htTop",
                                        halign = "htCenter",
                                        strict = TRUE,
                                        allowInvalid = FALSE,
                                        copyable = TRUE) %>%
                                hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
                                hot_rows(fixedRowsTop = 0) %>%
                                hot_col(1, renderer = "
            function (instance, td, row, col, prop, value, cellProperties) {
                     Handsontable.renderers.TextRenderer.apply(this, arguments);

                     if (instance.params) {
                       hrows = instance.params.row_highlight
                       hrows = hrows instanceof Array ? hrows : [hrows]

                       if (hrows.includes(row)) { 
                         td.style.backgroundColor = 'rgba(3, 227, 77, 0.2)' 
                       }

                     }
            }") %>%
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
                                ) %>%
                                hot_col(4, renderer = "
            function (instance, td, row, col, prop, value, cellProperties) {
                     Handsontable.renderers.TextRenderer.apply(this, arguments);

                     if (instance.params) {
                       hrows = instance.params.duplicated_highlight
                       hrows = hrows instanceof Array ? hrows : [hrows]

                       if (hrows.includes(row)) { 
                         td.style.backgroundColor = 'rgb(224, 179, 0)' 
                       }
                     }
            }") 
                            })
                          }
                        } else {
                          if(!is.null(DF1$data) & !is.null(DF1$cust_var)) {
                            output$db_entries <- renderRHandsontable({
                              row_highlight <- true_rows()-1
                              rhandsontable(
                                select(DF1$data, 1:(12 + nrow(DF1$cust_var))),
                                rowHeaders = NULL,
                                row_highlight = row_highlight,
                                duplicated_highlight = duplicated_rows()-1
                              ) %>%
                                hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
                                hot_col(1, 
                                        readOnly = TRUE,
                                        valign = "htMiddle",
                                        halign = "htCenter") %>%
                                hot_col(3:(12 + nrow(DF1$cust_var)), 
                                        valign = "htMiddle",
                                        halign = "htLeft") %>%
                                hot_col(2, type = "checkbox", width = "auto",
                                        valign = "htTop",
                                        halign = "htCenter") %>%
                                hot_context_menu(allowRowEdit = FALSE,
                                                 allowColEdit = FALSE,
                                                 allowReadOnly = FALSE) %>%
                                hot_rows(fixedRowsTop = 0) %>%
                                hot_col(1, renderer = "
            function (instance, td, row, col, prop, value, cellProperties) {
                     Handsontable.renderers.TextRenderer.apply(this, arguments);

                     if (instance.params) {
                       hrows = instance.params.row_highlight
                       hrows = hrows instanceof Array ? hrows : [hrows]

                       if (hrows.includes(row)) { 
                         td.style.backgroundColor = 'rgba(3, 227, 77, 0.2)' 
                       }

                     }
            }") %>%
                                hot_col(4, renderer = "
            function (instance, td, row, col, prop, value, cellProperties) {
                     Handsontable.renderers.TextRenderer.apply(this, arguments);

                     if (instance.params) {
                       hrows = instance.params.duplicated_highlight
                       hrows = hrows instanceof Array ? hrows : [hrows]

                       if (hrows.includes(row)) { 
                         td.style.backgroundColor = 'rgb(224, 179, 0)' 
                       }
                     }
            }")
                            })    
                          }
                        }
                      } else {
                        if (length(input$compare_select) > 0) {
                          if(!is.null(DF1$data) & !is.null(DF1$cust_var) & !is.null(input$table_height) & !is.null(input$compare_select)) {
                            output$db_entries <- renderRHandsontable({
                              rhandsontable(
                                select(DF1$data, 1:(12 + nrow(DF1$cust_var)), input$compare_select),
                                col_highlight = diff_allele()-1,
                                rowHeaders = NULL,
                                height = table_height(),
                                row_highlight = true_rows()-1,
                                duplicated_highlight = duplicated_rows()-1
                              ) %>%
                                hot_col((12 + nrow(DF1$cust_var)):((12 + nrow(DF1$cust_var))+length(input$compare_select)), 
                                        valign = "htMiddle",
                                        halign = "htCenter") %>%
                                hot_col(3:(12 + nrow(DF1$cust_var)), 
                                        valign = "htMiddle",
                                        halign = "htLeft") %>%
                                hot_col(1, 
                                        readOnly = TRUE,
                                        valign = "htMiddle",
                                        halign = "htCenter") %>%
                                hot_context_menu(allowRowEdit = FALSE,
                                                 allowColEdit = FALSE,
                                                 allowReadOnly = FALSE)  %>%
                                hot_col(2, type = "checkbox", width = "auto",
                                        valign = "htTop",
                                        halign = "htCenter",
                                        allowInvalid = FALSE,
                                        copyable = TRUE,
                                ) %>%
                                hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
                                hot_rows(fixedRowsTop = 0) %>%
                                hot_col(1, renderer = "
            function (instance, td, row, col, prop, value, cellProperties) {
                     Handsontable.renderers.TextRenderer.apply(this, arguments);

                     if (instance.params) {
                       hrows = instance.params.row_highlight
                       hrows = hrows instanceof Array ? hrows : [hrows]

                       if (hrows.includes(row)) { 
                         td.style.backgroundColor = 'rgba(3, 227, 77, 0.2)' 
                       }

                     }
            }") %>%
                                hot_col(4, renderer = "
            function (instance, td, row, col, prop, value, cellProperties) {
                     Handsontable.renderers.TextRenderer.apply(this, arguments);

                     if (instance.params) {
                       hrows = instance.params.duplicated_highlight
                       hrows = hrows instanceof Array ? hrows : [hrows]

                       if (hrows.includes(row)) { 
                         td.style.backgroundColor = 'rgb(224, 179, 0)' 
                       }
                     }
            }") %>%
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
              }") 
                            })    
                          }
                        } else {
                          if(!is.null(DF1$data) & !is.null(DF1$cust_var) & !is.null(input$table_height)) {
                            output$db_entries <- renderRHandsontable({
                              rhandsontable(
                                select(DF1$data, 1:(12 + nrow(DF1$cust_var))),
                                rowHeaders = NULL,
                                height = table_height(),
                                duplicated_highlight = duplicated_rows()-1,
                                row_highlight = true_rows()-1
                              ) %>%
                                hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
                                hot_col(1, 
                                        readOnly = TRUE,
                                        valign = "htMiddle",
                                        halign = "htCenter") %>%
                                hot_col(3:(12 + nrow(DF1$cust_var)), 
                                        valign = "htMiddle",
                                        halign = "htLeft") %>%
                                hot_context_menu(allowRowEdit = FALSE,
                                                 allowColEdit = FALSE,
                                                 allowReadOnly = FALSE) %>%
                                hot_rows(fixedRowsTop = 0) %>%
                                hot_col(1, renderer = "
            function (instance, td, row, col, prop, value, cellProperties) {
                     Handsontable.renderers.TextRenderer.apply(this, arguments);

                     if (instance.params) {
                       hrows = instance.params.row_highlight
                       hrows = hrows instanceof Array ? hrows : [hrows]

                       if (hrows.includes(row)) { 
                         td.style.backgroundColor = 'rgba(3, 227, 77, 0.2)' 
                       }
                     }
            }") %>%
                                hot_col(2, type = "checkbox", width = "auto",
                                        valign = "htTop", halign = "htCenter") %>%
                                hot_col(4, renderer = "
            function (instance, td, row, col, prop, value, cellProperties) {
                     Handsontable.renderers.TextRenderer.apply(this, arguments);

                     if (instance.params) {
                       hrows = instance.params.duplicated_highlight
                       hrows = hrows instanceof Array ? hrows : [hrows]

                       if (hrows.includes(row)) { 
                         td.style.backgroundColor = 'rgb(224, 179, 0)' 
                       }
                     }
            }") 
                            })
                          }
                        }
                      }
                    }
                    
                    
                    
                    # Dynamic save button when rhandsontable changes or new entries
                    output$edit_entry_table <- renderUI({
                      if(check_new_entry() & DF1$check_new_entries) {
                        fluidRow(
                          column(
                            width = 8,
                            align = "left",
                            HTML(
                              paste(
                                tags$span(style='color: white; font-size: 14px; position: absolute; bottom: -30px; right: -5px', 'New entries - reload database')
                              )
                            )
                          ),
                          column(
                            width = 4,
                            actionButton(
                              "load",
                              "",
                              icon = icon("rotate"),
                              class = "pulsating-button"
                            )
                          )
                        )
                      } else if(typing_reactive$pending_format == 888888 & !(typing_reactive$entry_added == 999999)) {
                        fluidRow(
                          column(
                            width = 11,
                            align = "left",
                            HTML(
                              paste(
                                tags$span(style='color: white; font-size: 14px; position: absolute; bottom: -30px; right: -5px', 'No database changes possible - pending entry addition')
                              )
                            )
                          ),
                          column(
                            width = 1,
                            HTML(paste('<i class="fa fa-spinner fa-spin" style="font-size:20px; color:white; margin-top: 10px"></i>'))
                          )
                        )
                      } else if((DF1$change == TRUE) | !identical(get.entry.table.meta(), DF1$meta)) {
                        fluidRow(
                          column(
                            width = 5,
                            HTML(
                              paste(
                                tags$span(style='color: white; font-size: 16px; position: absolute; bottom: -30px; right: -5px', 'Confirm changes')
                              )
                            )
                          ),
                          column(
                            width = 3,
                            actionButton(
                              "edit_button",
                              "",
                              icon = icon("bookmark"),
                              class = "pulsating-button"
                            )
                          ),
                          column(
                            width = 4,
                            actionButton(
                              "undo_changes",
                              "Undo",
                              icon = icon("repeat")
                            )
                          )
                        )
                      } else {NULL}
                    })
                    
                  })
                  
                  # Hide no entry message
                  output$db_no_entries <- NULL
                  output$distancematrix_no_entries <- NULL
                  
                } else {
                  
                  # If database loading not successful dont show entry table
                  output$db_entries_table <- NULL
                  output$entry_table_controls <- NULL
                }
                
                # Render Entry table controls
                output$entry_table_controls <- renderUI({
                  fluidRow(
                    column(
                      width = 3,
                      align = "center",
                      fluidRow(
                        column(
                          width = 4,
                          align = "center",
                          actionButton(
                            "sel_all_entries",
                            "Select all",
                            icon = icon("check")
                          )
                        ),
                        column(
                          width = 4,
                          align = "left",
                          actionButton(
                            "desel_all_entries",
                            "Deselect all",
                            icon = icon("xmark")
                          )
                        )
                      )
                    ),
                    column(
                      width = 3,
                      uiOutput("edit_entry_table")
                    )
                  )
                })
                
                #### Render Distance Matrix ----
                observe({
                  if(!class(DF1$data) == "NULL") {
                    
                    if(any(duplicated(DF1$meta$`Assembly Name`))) {
                      output$db_distancematrix <- NULL
                      output$distancematrix_duplicated <- renderUI({
                        column(
                          width = 12,
                          tags$span(style = "font-size: 15; color: white",
                                    "Change duplicated entry names to display distance matrix."),
                          br(), br(), br(),
                          actionButton("change_entries", "Go to Entry Table", class = "btn btn-default"),
                          br(), br(), br(),
                          tags$span(
                            style = "font-size: 15; color: white",
                            HTML(
                              append(
                                "Duplicated:",
                                append(
                                  "<br>",
                                  paste0(
                                    paste(
                                      paste0("# ", which(duplicated(DF1$meta$`Assembly Name`)), " - "),
                                      DF1$meta$`Assembly Name`[which(duplicated(DF1$meta$`Assembly Name`))]
                                    ),
                                    "<br>"
                                  )
                                )
                              )
                            )
                          )
                        )
                      })
                    } else {
                      output$distancematrix_duplicated <- NULL
                      if(!is.null(DF1$data) & !is.null(DF1$allelic_profile) & !is.null(DF1$allelic_profile_true) & !is.null(DF1$cust_var) & !is.null(input$distmatrix_label) & !is.null(input$distmatrix_diag) & !is.null(input$distmatrix_triangle)) {
                        output$db_distancematrix <- renderRHandsontable({
                          rhandsontable(hamming_df(), digits = 1, 
                                        height = distancematrix_height(), rowHeaders = NULL) %>%
                            hot_heatmap(renderer = paste0("function (instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.TextRenderer.apply(this, arguments);
  heatmapScale  = chroma.scale(['#17F556', '#ED6D47']);

  if (instance.heatmap[col]) {
    mn = ", DF1$matrix_min, ";
    mx = ", DF1$matrix_max, ";

    pt = (parseInt(value, 10) - mn) / (mx - mn);    

    td.style.backgroundColor = heatmapScale(pt).hex();
  }
}
")) %>%
                            hot_rows(fixedRowsTop = 0) %>%
                            hot_cols(fixedColumnsLeft = 1) %>%
                            hot_col(1:(dim(DF1$ham_matrix)[1]+1),
                                    halign = "htCenter",
                                    valign = "htMiddle") %>%
                            hot_col(1,
                                    renderer = "
                function(instance, td, row, col, prop, value, cellProperties) {
                  Handsontable.renderers.NumericRenderer.apply(this, arguments);

                    td.style.background = '#F0F0F0'
              }"
                            ) 
                        })  
                      }
                    }
                  
                  # Render Distance Matrix UI
                  
                  output$distmatrix_show <- renderUI({
                    if(!class(DF1$data) == "NULL") {
                      if(nrow(DF1$data) > 1) {
                        column(
                          width = 10,
                          uiOutput("distancematrix_duplicated"),
                          div(
                            class = "distmatrix",
                            rHandsontableOutput("db_distancematrix")
                          )
                        )
                      } else {
                        column(
                          width = 10,
                          align = "left",
                          p(
                            HTML(
                              paste(
                                tags$span(style='color: white; font-size: 15px;', "Type at least two assemblies to display a distance matrix.")
                              )
                            )
                          ),
                          br(),
                          br()
                        )
                      }
                    }
                  })
                  
                  }
                })
                
                
                # Render delete entry box UI
                output$delete_box <- renderUI({
                  box(
                    solidHeader = TRUE,
                    status = "primary",
                    width = "100%",
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        h3(p("Delete Entries"), style = "color:white")
                      )
                    ),
                    hr(),
                    fluidRow(
                      column(width = 1),
                      column(
                        width = 2,
                        align = "right",
                        br(),
                        h5("Index", style = "color:white; margin-bottom: 0px;")
                      ),
                      column(
                        width = 6,
                        align = "center",
                        uiOutput("delete_select")
                      ),
                      column(
                        width = 2,
                        align = "center",
                        br(),
                        uiOutput("del_bttn")
                      )
                    ),
                    br()
                  )
                })
                
                # Render loci comparison box UI
                output$compare_allele_box <- renderUI({
                  box(
                    solidHeader = TRUE,
                    status = "primary",
                    width = "100%",
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        h3(p("Compare Loci"), style = "color:white")
                      )
                    ),
                    hr(),
                    column(
                      width = 12,
                      align = "center",
                      br(),
                      uiOutput("compare_select"),
                      br(),
                      column(3),
                      column(
                        width = 8,
                        align = "left",
                        uiOutput("compare_difference_box")
                      )
                    ),
                    br()
                  )
                })
                
                # Render entry table download box UI
                output$download_entries <- renderUI({
                  box(
                    solidHeader = TRUE,
                    status = "primary",
                    width = "100%",
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        h3(p("Download Table"), style = "color:white")
                      )
                    ),
                    hr(),
                    fluidRow(
                      column(2),
                      column(
                        width = 10,
                        align = "left",
                        br(),
                        checkboxInput(
                          "download_table_include",
                          label = h5("Only included entries (Include = TRUE)", style = "color:white; margin-top: 4px")
                        ),
                        checkboxInput(
                          "download_table_loci",
                          label = h5("Include displayed loci", style = "color:white; margin-top: 4px"),
                          value = FALSE
                        ),
                        br(),
                      )
                    ),
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        downloadBttn(
                          "download_entry_table",
                          style = "simple",
                          label = "",
                          size = "sm",
                          icon = icon("download"),
                          color = "primary"
                        )
                      )
                    ),
                    br()
                  )
                })
                
                # Render entry deletion select input
                output$delete_select <- renderUI({
                  pickerInput("select_delete",
                              label = "",
                              choices = DF1$data[, "Index"],
                              options = list(
                                `live-search` = TRUE,
                                `actions-box` = TRUE,
                                size = 10,
                                style = "background-color: white; border-radius: 5px;"
                              ),
                              multiple = TRUE)
                })
                
                # Render delete entry button
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
                
                #### Missing Values UI ----
                
                # Missing values calculations and table 
                NA_table <- DF1$allelic_profile[, colSums(is.na(DF1$allelic_profile)) != 0]
                
                NA_table <- NA_table[rowSums(is.na(NA_table)) != 0,]
                
                NA_table[is.na(NA_table)] <- "NA"
                
                NA_table <- NA_table %>% 
                  cbind("Assembly Name" = DF1$meta[rownames(NA_table),]$`Assembly Name`) %>%
                  cbind("Errors" = DF1$meta[rownames(NA_table),]$Errors) %>%
                  relocate("Assembly Name", "Errors")
                
                observe({
                  if(!is.null(input$miss_val_height)) {
                    if(nrow(NA_table) < 31) {
                      output$table_missing_values <- renderRHandsontable({
                        rhandsontable(
                          NA_table,
                          rowHeaders = NULL
                        ) %>%
                          hot_context_menu(allowRowEdit = FALSE,
                                           allowColEdit = FALSE,
                                           allowReadOnly = TRUE) %>%
                          hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
                          hot_rows(fixedRowsTop = 0) %>%
                          hot_col(1:ncol(NA_table), valign = "htMiddle", halign = "htCenter")
                      })
                    } else {
                      output$table_missing_values <- renderRHandsontable({
                        rhandsontable(
                          NA_table,
                          rowHeaders = NULL,
                          height = miss.val.height()
                        ) %>%
                          hot_context_menu(allowRowEdit = FALSE,
                                           allowColEdit = FALSE,
                                           allowReadOnly = TRUE) %>%
                          hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
                          hot_rows(fixedRowsTop = 0) %>%
                          hot_col(1:ncol(NA_table), valign = "htMiddle", halign = "htCenter")
                      })
                    }
                  }
                })
                
                # Render missing value informatiojn box UI
                output$missing_values <- renderUI({
                  box(
                    solidHeader = TRUE,
                    status = "primary",
                    width = "100%",
                    fluidRow(
                      div(
                        class = "white",
                        column(
                          width = 12,
                          align = "left",
                          br(), 
                          HTML(
                            paste0("There are ", 
                                   strong(as.character(sum(is.na(DF1$data)))), 
                                   " unsuccessful allele allocations (NA). ",
                                   strong(sum(sapply(DF1$allelic_profile, anyNA))),
                                   " out of ",
                                   strong(ncol(DF1$allelic_profile)),
                                   " total loci in this scheme contain NA's (",
                                   strong(round((sum(sapply(DF1$allelic_profile, anyNA)) / ncol(DF1$allelic_profile) * 100), 1)),
                                   " %). ",
                                   "Decide how these missing values should be treated:")
                            
                          ),
                          br()
                        )
                      )
                    ),
                    fluidRow(
                      column(1),
                      column(
                        width = 11,
                        align = "left",
                        br(),
                        prettyRadioButtons(
                          "na_handling",
                          "",
                          choiceNames = c("Ignore missing values for pairwise comparison",
                                          "Omit loci with missing values for all assemblies",
                                          "Treat missing values as allele variant"),
                          choiceValues = c("ignore_na", "omit", "category"),
                          shape = "curve",
                          selected = c("ignore_na")
                        ),
                        br()
                      )
                    )
                  )
                })  
                
              } else { 
                #if no typed assemblies present
                
                # null underlying database
                
                DF1$data <- NULL
                
                DF1$meta <- NULL
                
                DF1$meta_true <- NULL
                
                DF1$allelic_profile <- NULL
                
                DF1$allelic_profile_true <- NULL
                
                # Render menu without missing values tab
                output$menu <- renderMenu(
                  sidebarMenu(
                    menuItem(
                      text = "Database Browser",
                      tabName = "database",
                      icon = icon("hard-drive"),
                      startExpanded = TRUE,
                      selected = TRUE,
                      menuSubItem(
                        text = "Browse Entries",
                        tabName = "db_browse_entries"
                      ),
                      menuSubItem(
                        text = "Scheme Info",
                        tabName = "db_schemeinfo"
                      ),
                      menuSubItem(
                        text = "Distance Matrix",
                        tabName = "db_distmatrix"
                      )
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
                    )
                  )
                )
                
                observe({
                  if(is.null(DF1$data)) {
                    if(is.null(typing_reactive$entry_added)) {
                      output$db_no_entries <- renderUI(
                        column(
                          width = 12,
                          fluidRow(
                            column(1),
                            column(
                              width = 11,
                              align = "left",
                              HTML(
                                paste(
                                  "<span style='color: white;'>",
                                  "No Entries for this scheme available.",
                                  "Type a genome in the section <strong>Allelic Typing</strong> and add the result to the local database.",
                                  sep = '<br/>'
                                )
                              )
                            )
                          )
                        )
                      )
                    } else {
                      if(typing_reactive$entry_added == 999999) {
                        output$db_no_entries <- renderUI(
                          column(
                            width = 12,
                            fluidRow(
                              column(1),
                              column(
                                width = 3,
                                align = "left",
                                HTML(
                                  paste(
                                    tags$span(style='color: white; font-size: 15px; position: absolute; bottom: -30px; right: -5px', 'New entries - reload database')
                                  )
                                )
                              ),
                              column(
                                width = 4,
                                actionButton(
                                  "load",
                                  "",
                                  icon = icon("rotate"),
                                  class = "pulsating-button"
                                )
                              )
                            )
                          )
                        )
                      } else {
                        output$db_no_entries <- renderUI(
                          column(
                            width = 12,
                            fluidRow(
                              column(1),
                              column(
                                width = 11,
                                align = "left",
                                HTML(paste(
                                  "<span style='color: white;'>",
                                  "No Entries for this scheme available.",
                                  "Type a genome in the section <strong>Allelic Typing</strong> and add the result to the local database.",
                                  sep = '<br/>'
                                ))
                              )
                            )
                          )
                        )
                      }
                    }
                  }
                })
                
                output$distancematrix_no_entries <- renderUI(
                  fluidRow(
                    column(1),
                    column(
                      width = 11,
                      align = "left",
                      HTML(paste(
                        "<span style='color: white;'>",
                        "No Entries for this scheme available.",
                        "Type a genome in the section <strong>Allelic Typing</strong> and add the result to the local database.",
                        sep = '<br/>'
                      ))
                    )
                  )
                )
                
                output$db_entries <- NULL
                output$edit_index <- NULL
                output$edit_scheme_d <- NULL
                output$edit_entries <- NULL
                output$compare_select <- NULL
                output$delete_select <- NULL
                output$del_bttn <- NULL
                output$compare_allele_box <- NULL
                output$download_entries <- NULL
                output$missing_values <- NULL
                output$delete_box <- NULL
                output$entry_table_controls <- NULL
                
              }
            }
          }
        }
      } else {
        show_toast(
          title = "Invalid scheme folder",
          type = "warning",
          position = "top-end",
          width = "500px",
          timer = 4000
        )
      }
    }
    
  })
  
  ## Database ----
  
  ### Conditional UI Elements rendering ----
  
  # Message on Database tabs if no scheme available yet
  observe({
    if(!is.null(DF1$exist)) {
      if(DF1$exist){
        
        # Message for tab Browse Entries
        output$no_scheme_entries <- renderUI({
          fluidRow(
            column(1),
            column(
              width = 4,
              align = "left",
              p(
                HTML(
                  paste(
                    tags$span(style='color: white; font-size: 15px; ', 
                              'No scheme available.')
                  )
                )
              ),
              p(
                HTML(
                  paste(
                    tags$span(style='color: white; font-size: 15px; ', 
                              'Download a scheme first and type assemblies in the section Allelic Typing.')
                  )
                )
              )
            )
          )
        })
        
        # Message for Tab Scheme Info
        output$no_scheme_info <- renderUI({
          fluidRow(
            column(1),
            column(
              width = 10,
              align = "left",
              p(
                HTML(
                  paste(
                    tags$span(style='color: white; font-size: 15px; ', 
                              'No scheme available.')
                  )
                )
              ),
              p(
                HTML(
                  paste(
                    tags$span(style='color: white; font-size: 15px; ', 
                              'Download a scheme first and type assemblies in the section Allelic Typing.')
                  )
                )
              )
            )
          )
        })
        
        # Message for Tab Distance Matrix
        output$no_scheme_distancematrix <- renderUI({
          fluidRow(
            column(1),
            column(
              width = 10,
              align = "left",
              p(
                HTML(
                  paste(
                    tags$span(style='color: white; font-size: 15px; ', 
                              'No scheme available.')
                  )
                )
              ),
              p(
                HTML(
                  paste(
                    tags$span(style='color: white; font-size: 15px; ', 
                              'Download a scheme first and type assemblies in the section Allelic Typing.')
                  )
                )
              )
            )
          )
        })
        
      } else {
        output$no_scheme_entries <- NULL
        output$no_scheme_info <- NULL
        output$no_scheme_distancematrix <- NULL
      }
    }
    
  })
  
  observe({
    # Conditional Missing Values Tab
    if(!is.null(DF1$allelic_profile)) {
      if(anyNA(DF1$allelic_profile)) {
        if(DF1$no_na_switch == FALSE) {
          output$menu <- renderMenu(
            sidebarMenu(
              menuItem(
                text = "Database Browser",
                tabName = "database",
                icon = icon("hard-drive"),
                startExpanded = TRUE,
                menuSubItem(
                  text = "Browse Entries",
                  tabName = "db_browse_entries"
                ),
                menuSubItem(
                  text = "Scheme Info",
                  tabName = "db_schemeinfo"
                ),
                menuSubItem(
                  text = "Distance Matrix",
                  tabName = "db_distmatrix"
                ),
                menuSubItem(
                  text = "Missing Values",
                  tabName = "db_missing_values",
                  selected = TRUE,
                  icon = icon("triangle-exclamation")
                )
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
              )
            )
          )
        }
        
      } else {
        output$menu <- renderMenu(
          sidebarMenu(
            menuItem(
              text = "Database Browser",
              tabName = "database",
              icon = icon("hard-drive"),
              startExpanded = TRUE,
              menuSubItem(
                text = "Browse Entries",
                tabName = "db_browse_entries"
              ),
              menuSubItem(
                text = "Scheme Info",
                tabName = "db_schemeinfo"
              ),
              menuSubItem(
                text = "Distance Matrix",
                tabName = "db_distmatrix"
              )
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
            )
          )
        )
      }
    }
    
  })
  
  observe({
    
    if (!is.null(DF1$available)) {
      output$scheme_db <- renderUI({
        if (length(DF1$available) > 5) {
          selectInput(
            "scheme_db",
            label = "",
            choices = if(!is.null(typing_reactive$last_scheme)) {
              typing_reactive$last_scheme
            } else {DF1$available},
            selected = if(!is.null(typing_reactive$last_scheme)) {
              typing_reactive$last_scheme
            } else {if(!is.null(DF1$scheme)) {DF1$scheme} else {DF1$available[1]}}
          )
        } else {
          prettyRadioButtons(
            "scheme_db",
            label = "",
            choices = if(!is.null(typing_reactive$last_scheme)) {
              typing_reactive$last_scheme
            } else {DF1$available},
            selected = if(!is.null(typing_reactive$last_scheme)) {
              typing_reactive$last_scheme
            } else {if(!is.null(DF1$scheme)) {DF1$scheme} else {DF1$available[1]}}
          )
        }
      })
      
      # Dont Show 'No Database' message
      output$start_message_no_db <- NULL
      
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
    } 
  })
  
  # If only one entry available disable varying loci checkbox
  
  output$compare_difference_box <- renderUI({
    if(!class(DF1$data) == "NULL") {
      if(nrow(DF1$data) > 1) {
        checkboxInput(
          "compare_difference",
          label = h5("Only varying loci", style = "color:white; position: relative; top: -6px"),
          value = FALSE
        )
      }
    }
  })
  
  ### Database Events ----
  
  # Create new database
  observe({
    shinyDirChoose(input,
                   "create_new_db",
                   roots = c(wd = "/home"),
                   session = session)
    
    if(!is.null(input$create_new_db)) {
      DF1$new_database <- as.character(
        parseDirPath(
          roots = c(wd = "/home"), 
          input$create_new_db
        )
      )
    }
  })
  
  #Undo changes
  observeEvent(input$undo_changes, {
    Data <- readRDS(paste0(
      DF1$database, "/",
      gsub(" ", "_", DF1$scheme),
      "/Typing.rds"
    ))
    
    DF1$data <- Data[["Typing"]]
    
    if ((ncol(DF1$data)-12) != as.numeric(gsub(",", "", as.vector(DF1$schemeinfo[6, 2])))) {
      cust_var <- select(DF1$data, 13:(ncol(DF1$data) - as.numeric(gsub(",", "", as.vector(DF1$schemeinfo[6, 2])))))
      DF1$cust_var <- data.frame(Variable = names(cust_var), Type = column_classes(cust_var))
    } else {
      DF1$cust_var <- data.frame()
    }
    
    DF1$change <- FALSE
    
    DF1$count <- 0
    
    DF1$no_na_switch <- TRUE
    
    DF1$meta <- select(DF1$data, 1:(12 + nrow(DF1$cust_var)))
    
    DF1$meta_true <- DF1$meta[which(DF1$data$Include == TRUE),]
    
    DF1$allelic_profile <- select(DF1$data, -(1:(12 + nrow(DF1$cust_var))))
    
    DF1$allelic_profile_true <- DF1$allelic_profile[which(DF1$data$Include == TRUE),]
    
    DF1$deleted_entries <- character(0)
    
    observe({
      if (!is.null(DF1$data)) {
        if (nrow(DF1$data) == 1) {
          output$db_entries <- renderRHandsontable({
            rhandsontable(
              select(DF1$data, 1:(12 + nrow(DF1$cust_var))),
              rowHeaders = NULL
            ) %>%
              hot_col(1, 
                      readOnly = TRUE,
                      valign = "htMiddle",
                      halign = "htCenter") %>%
              hot_col(3:(12 + nrow(DF1$cust_var)), 
                      valign = "htMiddle",
                      halign = "htLeft") %>%
              hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
              hot_col(2, type = "checkbox", width = "auto",
                      valign = "htTop",
                      halign = "htCenter") %>%
              hot_context_menu(allowRowEdit = FALSE,
                               allowColEdit = FALSE,
                               allowReadOnly = FALSE) %>%
              hot_rows(fixedRowsTop = 0)
          })
        } else if (between(nrow(DF1$data), 1, 40)) {
          if (length(input$compare_select) > 0) {
            output$db_entries <- renderRHandsontable({
              row_highlight <- true_rows()-1
              rhandsontable(
                select(DF1$data, 1:(12 + nrow(DF1$cust_var)), input$compare_select),
                col_highlight = diff_allele()-1,
                rowHeaders = NULL,
                row_highlight = row_highlight
              ) %>%
                hot_col((12 + nrow(DF1$cust_var)):((12 + nrow(DF1$cust_var))+length(input$compare_select)), 
                        valign = "htMiddle",
                        halign = "htCenter") %>%
                hot_col(1, 
                        readOnly = TRUE,
                        valign = "htMiddle",
                        halign = "htCenter") %>%
                hot_col(3:(12 + nrow(DF1$cust_var)), 
                        valign = "htMiddle",
                        halign = "htLeft") %>%
                hot_context_menu(allowRowEdit = FALSE,
                                 allowColEdit = FALSE,
                                 allowReadOnly = FALSE) %>%
                hot_col(2, type = "checkbox", width = "auto",
                        valign = "htTop",
                        halign = "htCenter",
                        strict = TRUE,
                        allowInvalid = FALSE,
                        copyable = TRUE) %>%
                hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
                hot_rows(fixedRowsTop = 0) %>%
                hot_col(1, renderer = "
            function (instance, td, row, col, prop, value, cellProperties) {
                     Handsontable.renderers.TextRenderer.apply(this, arguments);

                     if (instance.params) {
                       hrows = instance.params.row_highlight
                       hrows = hrows instanceof Array ? hrows : [hrows]

                       if (hrows.includes(row)) { 
                         td.style.backgroundColor = 'rgba(3, 227, 77, 0.2)' 
                       }

                     }
            }") %>%
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
              row_highlight <- true_rows()-1
              rhandsontable(
                select(DF1$data, 1:(12 + nrow(DF1$cust_var))),
                rowHeaders = NULL,
                row_highlight = row_highlight
              ) %>%
                hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
                hot_col(1, 
                        readOnly = TRUE,
                        valign = "htMiddle",
                        halign = "htCenter") %>%
                hot_col(3:(12 + nrow(DF1$cust_var)), 
                        valign = "htMiddle",
                        halign = "htLeft") %>%
                hot_col(2, type = "checkbox", width = "auto",
                        valign = "htTop",
                        halign = "htCenter") %>%
                hot_context_menu(allowRowEdit = FALSE,
                                 allowColEdit = FALSE,
                                 allowReadOnly = FALSE) %>%
                hot_rows(fixedRowsTop = 0) %>%
                hot_col(1, renderer = "
            function (instance, td, row, col, prop, value, cellProperties) {
                     Handsontable.renderers.TextRenderer.apply(this, arguments);

                     if (instance.params) {
                       hrows = instance.params.row_highlight
                       hrows = hrows instanceof Array ? hrows : [hrows]

                       if (hrows.includes(row)) { 
                         td.style.backgroundColor = 'rgba(3, 227, 77, 0.2)' 
                       }

                     }
            }")
            })
          }
        } else {
          if (length(input$compare_select) > 0) {
            output$db_entries <- renderRHandsontable({
              rhandsontable(
                select(DF1$data, 1:(12 + nrow(DF1$cust_var)), input$compare_select),
                col_highlight = diff_allele()-1,
                rowHeaders = NULL,
                height = table_height(),
                row_highlight = true_rows()-1
              ) %>%
                hot_col((12 + nrow(DF1$cust_var)):((12 + nrow(DF1$cust_var))+length(input$compare_select)), 
                        valign = "htMiddle",
                        halign = "htCenter") %>%
                hot_col(3:(12 + nrow(DF1$cust_var)), 
                        valign = "htMiddle",
                        halign = "htLeft") %>%
                hot_col(1, 
                        readOnly = TRUE,
                        valign = "htMiddle",
                        halign = "htCenter") %>%
                hot_context_menu(allowRowEdit = FALSE,
                                 allowColEdit = FALSE,
                                 allowReadOnly = FALSE)  %>%
                hot_col(2, type = "checkbox", width = "auto",
                        valign = "htTop",
                        halign = "htCenter",
                        allowInvalid = FALSE,
                        copyable = TRUE,
                ) %>%
                hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
                hot_rows(fixedRowsTop = 0) %>%
                hot_col(1, renderer = "
            function (instance, td, row, col, prop, value, cellProperties) {
                     Handsontable.renderers.TextRenderer.apply(this, arguments);

                     if (instance.params) {
                       hrows = instance.params.row_highlight
                       hrows = hrows instanceof Array ? hrows : [hrows]

                       if (hrows.includes(row)) { 
                         td.style.backgroundColor = 'rgba(3, 227, 77, 0.2)' 
                       }

                     }
            }") %>%
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
              }") 
            })
          } else {
            output$db_entries <- renderRHandsontable({
              row_highlight <- true_rows()-1
              rhandsontable(
                select(DF1$data, 1:(12 + nrow(DF1$cust_var))),
                rowHeaders = NULL,
                height = table_height(),
                row_highlight = row_highlight
              ) %>%
                hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
                hot_col(1, 
                        readOnly = TRUE,
                        valign = "htMiddle",
                        halign = "htCenter") %>%
                hot_col(3:(12 + nrow(DF1$cust_var)), 
                        valign = "htMiddle",
                        halign = "htLeft") %>%
                hot_context_menu(allowRowEdit = FALSE,
                                 allowColEdit = FALSE,
                                 allowReadOnly = FALSE) %>%
                hot_rows(fixedRowsTop = 0) %>%
                hot_col(1, renderer = "
            function (instance, td, row, col, prop, value, cellProperties) {
                     Handsontable.renderers.TextRenderer.apply(this, arguments);

                     if (instance.params) {
                       hrows = instance.params.row_highlight
                       hrows = hrows instanceof Array ? hrows : [hrows]

                       if (hrows.includes(row)) { 
                         td.style.backgroundColor = 'rgba(3, 227, 77, 0.2)' 
                       }
                     }
            }") %>%
                hot_col(2, type = "checkbox", width = "auto",
                        valign = "htTop", halign = "htCenter")
            })
          }
        }
      }
    })
    
    
  })
  
  observe({
    if(!is.null(DF1$data)){
      if ((ncol(DF1$data)-12) != as.numeric(gsub(",", "", as.vector(DF1$schemeinfo[6, 2])))) {
        cust_var <- select(DF1$data, 13:(ncol(DF1$data) - as.numeric(gsub(",", "", as.vector(DF1$schemeinfo[6, 2])))))
        DF1$cust_var <- data.frame(Variable = names(cust_var), Type = column_classes(cust_var))
        
      } else {
        DF1$cust_var <- data.frame()
      }
    }
  })
  
  DF1$count <- 0
  
  observeEvent(input$add_new_variable, {
    if(nchar(input$new_var_name) > 12) {
      show_toast(
        title = "Max. 10 characters",
        type = "warning",
        position = "top-end",
        width = "500px",
        timer = 6000
      )
    } else {
      if (input$new_var_name == "") {
        show_toast(
          title = "Min. 1 character",
          type = "error",
          position = "top-end",
          width = "500px",
          timer = 6000
        )
      } else {
        if(trimws(input$new_var_name) %in% names(DF1$meta)) {
          show_toast(
            title = "Variable name already existing",
            type = "warning",
            position = "top-end",
            width = "500px",
            timer = 6000
          )
        } else {
          showModal(
            modalDialog(
              selectInput(
                "new_var_type",
                label = "",
                choices = c("Categorical (character)",
                            "Continous (numeric)")),
              title = paste0("Select the data type for ", input$new_varname),
              easyClose = TRUE,
              footer = tagList(
                modalButton("Cancel"),
                actionButton("conf_new_var", "Confirm", class = "btn btn-default")
              )
            )
          )
        }
      }
    }
  })
  
  observeEvent(input$conf_new_var, {
    
    removeModal()
    
    DF1$count <- DF1$count + 1
    
    DF1$change <- TRUE
    
    name <- trimws(input$new_var_name)
    
    if(input$new_var_type == "Categorical (character)") {
      DF1$data <- DF1$data %>%
        mutate("{name}" := character(nrow(DF1$data)), .after = 12)
      
      DF1$cust_var <- rbind(DF1$cust_var, data.frame(Variable = name, Type = "(categ.)"))
    } else {
      DF1$data <- DF1$data %>%
        mutate("{name}" := numeric(nrow(DF1$data)), .after = 12)
      
      DF1$cust_var <- rbind(DF1$cust_var, data.frame(Variable = name, Type = "(cont.)"))
    }
    
    DF1$meta <- select(DF1$data, 1:(12 + nrow(DF1$cust_var)))
    
    DF1$meta_true <- DF1$meta[which(DF1$data$Include == TRUE),]
    
    DF1$allelic_profile <- select(DF1$data, -(1:(12 + nrow(DF1$cust_var))))
    
    DF1$allelic_profile_true <- DF1$allelic_profile[which(DF1$data$Include == TRUE),]
    
    show_toast(
      title = paste0("Variable ", trimws(input$new_var_name), " added"),
      type = "success",
      position = "top-end",
      width = "500px",
      timer = 6000
    )
    
  })
  
  observeEvent(input$delete_new_variable, {
    if (input$del_which_var == "") {
      show_toast(
        title = "No custom variables",
        type = "error",
        position = "top-end",
        width = "500px",
        timer = 6000
      )
    } else {
      showModal(
        modalDialog(
          paste0(
            "Confirmation will lead to irreversible deletion of the custom ",
            input$del_which_var,
            " variable. Continue?"
          ),
          title = "Delete custom variables",
          fade = TRUE,
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("conf_var_del", "Delete", class = "btn btn-danger")
          )
        )
      )
    }
  })
  
  observeEvent(input$conf_var_del, {
    DF1$change <- TRUE
    
    removeModal()
    
    if(DF1$count >= 1) {
      DF1$count <- DF1$count - 1
    } 
    
    show_toast(
      title = paste0("Variable ", input$del_which_var, " removed"),
      type = "warning",
      position = "top-end",
      width = "500px",
      timer = 6000
    )
    
    DF1$cust_var <- DF1$cust_var[-which(DF1$cust_var$Variable == input$del_which_var),]
    DF1$data <- select(DF1$data, -(input$del_which_var))
    DF1$meta <- select(DF1$data, 1:(12 + nrow(DF1$cust_var)))
    DF1$meta_true <- DF1$meta[which(DF1$data$Include == TRUE),]
    
    DF1$allelic_profile <- select(DF1$data, -(1:(12 + nrow(DF1$cust_var))))
    DF1$allelic_profile_true <- DF1$allelic_profile[which(DF1$data$Include == TRUE),]
    
  })
  
  # Select all button
  
  observeEvent(input$sel_all_entries, {
    DF1$data$Include <- TRUE
  })
  
  observeEvent(input$desel_all_entries, {
    DF1$data$Include <- FALSE
  })
  
  # Switch to entry table
  
  observeEvent(input$change_entries, {
    removeModal()
    updateTabItems(session, "tabs", selected = "db_browse_entries")
  })
  
  #### Download Missing Value Matrix as CSV ----
  
  output$download_na_matrix <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_", gsub(" ", "_", DF1$scheme), "_Missing_Values.csv")
    },
    content = function(file) {
      download_matrix <- hot_to_r(input$table_missing_values)
      write.csv(download_matrix, file, sep = ",", row.names=FALSE, quote=FALSE) 
    }
  )
  
  #### Save scheme info table as CSV ----
  
  output$download_schemeinfo <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", DF1$scheme), "_scheme.csv")
    },
    content = function(file) {
      pub_index <- which(DF1$schemeinfo[,1] == "Publications")
      write.table(
        DF1$schemeinfo[1:(pub_index-1),],
        file, 
        sep = ";",
        row.names = FALSE, 
        quote = FALSE
      ) 
    }
  )
  
  #### Save Loci info table as CSV ----
  
  output$download_loci_info <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", DF1$scheme), "_Loci.csv")
    },
    content = function(file) {
      write.table(
        DF1$loci_info,
        file, 
        sep = ";",
        row.names = FALSE, 
        quote = FALSE
      ) 
    }
  )
  
  #### Save entry table as CSV ----
  
  output$download_entry_table <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_", gsub(" ", "_", DF1$scheme), "_Entries.csv")
    },
    content = function(file) {
      download_matrix <- hot_to_r(input$db_entries)
      
      if (input$download_table_include == TRUE) {
        download_matrix <- download_matrix[which(download_matrix$Include == TRUE),]
      }
      
      if (input$download_table_loci == FALSE) {
        download_matrix <- select(download_matrix, 1:(12 + nrow(DF1$cust_var)))
      } 
      
      write.csv(download_matrix, file, row.names=FALSE, quote=FALSE) 
    }
  )
  
  # Save Edits Button
  
  observeEvent(input$edit_button, {
    showModal(
      modalDialog(
        if(length(DF1$deleted_entries > 0)) {
          paste0(
            "Overwriting previous metadata of local ",
            DF1$scheme,
            " database. Deleted entries will be irreversibly removed. Continue?"
          )
        } else {
          paste0(
            "Overwriting previous metadata of local ",
            DF1$scheme,
            " database. Continue?"
          )
        },
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
      DF1$database, "/",
      gsub(" ", "_", DF1$scheme),
      "/Typing.rds"
    ))
    
    if ((ncol(Data[["Typing"]])-12) != as.numeric(gsub(",", "", as.vector(DF1$schemeinfo[6, 2])))) {
      cust_vars_pre <- select(Data[["Typing"]], 13:(ncol(Data[["Typing"]]) - as.numeric(gsub(",", "", as.vector(DF1$schemeinfo[6, 2])))))
      cust_vars_pre <- names(cust_vars_pre)
    } else {
      cust_vars_pre <- character()
    }
    
    Data[["Typing"]] <- select(Data[["Typing"]], -(1:(12 + length(cust_vars_pre))))
    
    meta_hot <- hot_to_r(input$db_entries)
    
    if(length(DF1$deleted_entries > 0)){
      
      meta_hot <- mutate(meta_hot, Index = as.character(1:nrow(DF1$data)))
      
      Data[["Typing"]] <- mutate(Data[["Typing"]][-as.numeric(DF1$deleted_entries),], meta_hot, .before = 1)
      rownames(Data[["Typing"]]) <- Data[["Typing"]]$Index
    } else {
      Data[["Typing"]] <- mutate(Data[["Typing"]], meta_hot, .before = 1)
      
    }
    
    # Ensure correct logical data type
    Data[["Typing"]][["Include"]] <- as.logical(Data[["Typing"]][["Include"]])
    
    saveRDS(Data, paste0(
      DF1$database, "/",
      gsub(" ", "_", DF1$scheme),
      "/Typing.rds"
    ))
    
    # Load database from files  
    Database <-
      readRDS(paste0(
        DF1$database, "/",
        gsub(" ", "_", DF1$scheme),
        "/Typing.rds"
      ))
    
    DF1$data <- Database[["Typing"]]
    
    if(!is.null(DF1$data)){
      if ((ncol(DF1$data)-12) != as.numeric(gsub(",", "", as.vector(DF1$schemeinfo[6, 2])))) {
        cust_var <- select(DF1$data, 13:(ncol(DF1$data) - as.numeric(gsub(",", "", as.vector(DF1$schemeinfo[6, 2])))))
        DF1$cust_var <- data.frame(Variable = names(cust_var), Type = column_classes(cust_var))
      } else {
        DF1$cust_var <- data.frame()
      }
    }
    
    DF1$change <- FALSE
    
    DF1$count <- 0
    
    DF1$no_na_switch <- TRUE
    
    DF1$meta <- select(DF1$data, 1:(12 + nrow(DF1$cust_var)))
    
    DF1$meta_true <- DF1$meta[which(DF1$data$Include == TRUE),]
    
    DF1$allelic_profile <- select(DF1$data, -(1:(12 + nrow(DF1$cust_var))))
    
    DF1$allelic_profile_true <- DF1$allelic_profile[which(DF1$data$Include == TRUE),]
    
    DF1$deleted_entries <- character(0)
    
    removeModal()
    
    show_toast(
      title = "Database successfully saved",
      type = "success",
      position = "top-end",
      timer = 4000,
      width = "500px"
    )
  })
  
  observeEvent(input$del_button, {
    if (length(input$select_delete) < 1) {
      show_toast(
        title = "No entry selected",
        type = "warning",
        position = "top-end",
        timer = 4000,
        width = "500px"
      )
    } else {
      if( (length(input$select_delete) - nrow(DF1$data) ) == 0) {
        showModal(
          modalDialog(
            paste0("Deleting will lead to removal of all entries from local ", DF1$scheme, " database. The data can not be recovered afterwards. Continue?"),
            easyClose = TRUE,
            title = "Deleting Entries",
            footer = tagList(
              modalButton("Cancel"),
              actionButton("conf_delete_all", "Delete", class = "btn btn-danger")
            )
          )
        )
      } else {
        showModal(
          modalDialog(
            paste0(
              "Confirmation will lead to irreversible removal of selected entries. Continue?"
            ),
            title = "Deleting Entries",
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
      }
    }
  })
  
  observeEvent(input$conf_delete_all, {
    
    delete_typing_path <- paste0(DF1$database, "/", gsub(" ", "_", DF1$scheme), "/Typing.rds")
    
    saveRDS(delete_typing_path, paste0(getwd(), "/execute/del_local.rds"))
    
    system(paste("chmod +x", paste0(getwd(), "/execute", "/delete_typing.sh")))
    system(paste0(getwd(), "/execute", "/delete_typing.sh"), wait = TRUE)
    
    showModal(
      modalDialog(
        selectInput(
          "scheme_db",
          label = "",
          choices = if(!is.null(typing_reactive$last_scheme)) {
            typing_reactive$last_scheme
          } else {DF1$available},
          selected = if(!is.null(typing_reactive$last_scheme)) {
            typing_reactive$last_scheme
          } else {if(!is.null(DF1$scheme)) {DF1$scheme} else {DF1$available[1]}}),
        title = "All entries have been removed. Select a local database to load.",
        footer = tagList(
          actionButton("load", "Load", class = "btn btn-default")
        )
      )
    )
    
  })
  
  DF1$deleted_entries <- character(0)
  
  observeEvent(input$conf_delete, {
    
    DF1$deleted_entries <- append(DF1$deleted_entries, DF1$data$Index[as.numeric(input$select_delete)])
    
    DF1$no_na_switch <- TRUE
    
    DF1$change <- TRUE
    
    DF1$check_new_entries <- FALSE
    
    DF1$data <- DF1$data[!(DF1$data$Index %in% as.numeric(input$select_delete)),]
    
    DF1$meta <- select(DF1$data, 1:(12 + nrow(DF1$cust_var)))
    
    DF1$meta_true <- DF1$meta[which(DF1$data$Include == TRUE),]
    
    DF1$allelic_profile <- select(DF1$data, -(1:(12 + nrow(DF1$cust_var))))
    
    DF1$allelic_profile_true <- DF1$allelic_profile[which(DF1$data$Include == TRUE),]
    
    removeModal()
    if(length(input$select_delete) > 1) {
      show_toast(
        title = "Entries deleted",
        type = "success",
        position = "top-end",
        timer = 4000,
        width = "500px"
      )
    } else {
      show_toast(
        title = "Entry deleted",
        type = "success",
        position = "top-end",
        timer = 4000,
        width = "500px"
      )
    }
  })
  
  
  ### Distance Matrix ---- 
  
  hamming_df <- reactive({
    # Create a custom proxy object for Hamming distance
    if(input$distmatrix_true == TRUE) {
      if(anyNA(DF1$allelic_profile)) {
        if(input$na_handling == "omit") {
          allelic_profile_noNA <- DF1$allelic_profile[, colSums(is.na(DF1$allelic_profile)) == 0]
          
          allelic_profile_noNA_true <- allelic_profile_noNA[which(DF1$data$Include == TRUE),]
          
          DF1$hamming_proxy <- proxy::dist(allelic_profile_noNA_true, method = hamming_distance)
          
        } else if(input$na_handling == "ignore_na"){
          DF1$hamming_proxy <- proxy::dist(DF1$allelic_profile_true, method = hamming_distance_ignore)
          
        } else {
          DF1$hamming_proxy <- proxy::dist(DF1$allelic_profile_true, method = hamming_distance_category)
          
        } 
      } else {
        DF1$hamming_proxy <- proxy::dist(DF1$allelic_profile_true, method = hamming_distance)
      }
    } else {
      if(anyNA(DF1$allelic_profile)) {
        if(input$na_handling == "omit") {
          allelic_profile_noNA <- DF1$allelic_profile[, colSums(is.na(DF1$allelic_profile)) == 0]
          DF1$hamming_proxy <- proxy::dist(allelic_profile_noNA, method = hamming_distance)
        } else if(input$na_handling == "ignore_na"){
          DF1$hamming_proxy <- proxy::dist(DF1$allelic_profile, method = hamming_distance_ignore)
        } else {
          DF1$hamming_proxy <- proxy::dist(DF1$allelic_profile, method = hamming_distance_category)
        }  
      } else {
        DF1$hamming_proxy <- proxy::dist(DF1$allelic_profile, method = hamming_distance)
      }
    }
    
    hamming_matrix <- as.matrix(DF1$hamming_proxy)
    
    DF1$matrix_min <- min(hamming_matrix, na.rm = TRUE)
    DF1$matrix_max <- max(hamming_matrix, na.rm = TRUE)
    
    if(input$distmatrix_triangle == FALSE) {
      hamming_matrix[upper.tri(hamming_matrix, diag = !input$distmatrix_diag)] <- NA
    } 
    
    # Rownames change
    rownames(hamming_matrix) <- select(DF1$data, 1:(12 + nrow(DF1$cust_var)))[rownames(select(DF1$data, 1:(12 + nrow(DF1$cust_var)))) %in% rownames(hamming_matrix), 
                                                                              input$distmatrix_label]
    colnames(hamming_matrix) <- rownames(hamming_matrix)
    
    mode(hamming_matrix) <- "integer"
    
    DF1$ham_matrix <- hamming_matrix %>%
      as.data.frame() %>%
      mutate(Index = colnames(hamming_matrix)) %>%
      relocate(Index)
    DF1$distancematrix_nrow <- nrow(DF1$ham_matrix)
    DF1$ham_matrix
  })
  
  output$download_distmatrix <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_", gsub(" ", "_", DF1$scheme), "_Distance_Matrix.csv")
    },
    content = function(file) {
      download_matrix <- hot_to_r(input$db_distancematrix)
      download_matrix[is.na(download_matrix)] <- ""
      write.csv(download_matrix, file, row.names=FALSE, quote=FALSE) 
    }
  )
  
  
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
    
    if(length(DF1$available) == 0) {
      saveRDS(DF1$new_database, paste0(getwd(), "/execute/new_db.rds"))
      system(paste("chmod +x",  paste0(getwd(), "/execute/make_db.sh")))
      system(paste0(getwd(), "/execute/make_db.sh"), wait = TRUE)
    }
    
    DF1$load_selected <- TRUE
    
    myReactives$target_table <- NULL
    
    # Download Loci Fasta Files
    download(link_cgmlst, dest = "dataset.zip", mode = "wb")
    
    unzip(
      zipfile = "dataset.zip",
      exdir = paste0(
        DF1$database, "/",
        folder_name,
        paste0("/", folder_name, "_alleles")
      )
    )
    
    unlink("dataset.zip")
    
    # Download Scheme Info
    download(
      myReactives$link_scheme,
      dest = paste0(DF1$database, "/", folder_name, "/scheme_info.html"),
      mode = "wb"
    )
    
    # Download Loci Info
    download(
      link_targets,
      dest = paste0(DF1$database, "/", folder_name, "/targets.csv"),
      mode = "wb"
    )
    
    # Send downloaded scheme to database browser overview
    DF1$available <- gsub("_", " ", basename(dir_ls(DF1$database)))
    
    myReactives$target_table <-
      read.csv(
        paste0(DF1$database, "/", folder_name, "/targets.csv"),
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
    
    DF1$exist <-
      (length(dir_ls(DF1$database)) == 0)
    
    show_toast(
      title = "Download successful",
      type = "success",
      position = "top-end",
      timer = 5000,
      width = "500px"
    )
    
    showModal(
      modalDialog(
        selectInput(
          "scheme_db",
          label = "",
          choices = if(!is.null(typing_reactive$last_scheme)) {
            typing_reactive$last_scheme
          } else {DF1$available},
          selected = if(!is.null(typing_reactive$last_scheme)) {
            typing_reactive$last_scheme
          } else {if(!is.null(DF1$scheme)) {DF1$scheme} else {DF1$available[1]}}),
        title = "Select a local database to load.",
        footer = tagList(
          actionButton("load", "Load", class = "btn btn-default")
        )
      )
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
  
  plot_loc <- reactiveValues(cluster = NULL, metadata = list())
  
  ### Reactive Render of Visualization Controls ----
  
  # Heatmap offset
  output$nj_heatmap_offs <- renderUI({
    numericInput(
      "nj_heatmap_offset",
      label = h5("Position", style = "color:white; margin-bottom: 0px"),
      min = -ceiling(max(plot_loc$xrange_nj)),
      max = ceiling(max(plot_loc$xrange_nj)),
      step = 1,
      value = 0,
      width = "80px"
    )
  })
  
  output$upgma_heatmap_offs <- renderUI({
    numericInput(
      "upgma_heatmap_offset",
      label = h5("Position", style = "color:white; margin-bottom: 0px"),
      min = -ceiling(max(plot_loc$xrange_upgma)),
      max = ceiling(max(plot_loc$xrange_upgma)),
      step = 1,
      value = 0,
      width = "80px"
    )
  })
  
  # Treescale Positioning
  
  output$upgma_treescalex <- renderUI({
    if(!is.null(plot_loc$xrange_upgma)) {
      sliderTextInput(
        "upgma_treescale_x",
        label = h5("X Position", style = "color:white; margin-bottom: 0px"),
        choices = -1:(ceiling(max(plot_loc$xrange_upgma)) * 1.1),
        selected = ceiling(min(plot_loc$xrange_upgma)),
        hide_min_max = TRUE,
        width = "150px"
      )
    }
  })
  
  output$upgma_treescaley <- renderUI({
    if(!is.null(plot_loc$yrange_upgma)) {
      sliderTextInput(
        "upgma_treescale_y",
        label = h5("Y Position", style = "color:white; margin-bottom: 0px"),
        choices = -1:(ceiling(max(plot_loc$yrange_upgma)) * 1.1),
        selected = 0,
        hide_min_max = TRUE
      )
    }
  })
  
  output$nj_treescalex <- renderUI({
    if(!is.null(plot_loc$xrange_nj)) {
      sliderTextInput(
        "nj_treescale_x",
        label = h5("X Position", style = "color:white; margin-bottom: 0px"),
        choices = -1:(ceiling(max(plot_loc$xrange_nj)) * 1.1),
        selected = ceiling(min(plot_loc$xrange_nj)),
        hide_min_max = TRUE,
        width = "150px"
      )
    }
  })
  
  output$nj_treescaley <- renderUI({
    if(!is.null(plot_loc$yrange_nj)) {
      sliderTextInput(
        "nj_treescale_y",
        label = h5("Y Position", style = "color:white; margin-bottom: 0px"),
        choices = -1:(ceiling(max(plot_loc$yrange_nj)) * 1.1),
        selected = 0,
        hide_min_max = TRUE
      )
    }
  })
  
  # heatmap picker
  output$upgma_heatmap_sel <- renderUI({
    div(
      class = "heatmap_picker",
      pickerInput(
        inputId = "upgma_heatmap_select",
        label = h5("Variables", style = "color:white; margin-bottom: 0px"),
        width = "100%",
        choices = if(ncol(DF1$meta) == 12) {
          c(
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          )
        } else {
          append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
                 names(DF1$meta)[13:ncol(DF1$meta)])
        },
        options = list(
          size = 10,
          style = "background-color: white; border-radius: 5px;"
        ),
        multiple = TRUE
      )
    )
  })
  
  output$nj_heatmap_sel <- renderUI({
    div(
      class = "heatmap_picker",
      pickerInput(
        inputId = "nj_heatmap_select",
        label = h5("Variables", style = "color:white; margin-bottom: 0px"),
        width = "100%",
        choices = if(ncol(DF1$meta) == 12) {
          c(
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          )
        } else {
          append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
                 names(DF1$meta)[13:ncol(DF1$meta)])
        },
        options = list(
          size = 10,
          style = "background-color: white; border-radius: 5px;"
        ),
        multiple = TRUE
      )
    )
  })
  
  # Geom Fruit Width
  output$upgma_fruit_width <- renderUI({
    numericInput(
      "upgma_fruit_width_circ",
      label = h5("Width", style = "color:white; margin-bottom: 0px"),
      min = ceiling(min(plot_loc$xrange_upgma)),
      max = ceiling(max(plot_loc$xrange_upgma)),
      value = ceiling(max(plot_loc$xrange_upgma) * 0.1),
      step = 0.5,
      width = "80px"
    )
  })
  
  output$upgma_fruit_width2 <- renderUI({
    numericInput(
      "upgma_fruit_width_circ_2",
      label = h5("Width", style = "color:white; margin-bottom: 0px"),
      min = ceiling(min(plot_loc$xrange_upgma)),
      max = ceiling(max(plot_loc$xrange_upgma)),
      value = ceiling(max(plot_loc$xrange_upgma) * 0.1),
      step = 0.5,
      width = "80px"
    )
  })
  
  output$upgma_fruit_width3 <- renderUI({
    numericInput(
      "upgma_fruit_width_circ_3",
      label = h5("Width", style = "color:white; margin-bottom: 0px"),
      min = ceiling(min(plot_loc$xrange_upgma)),
      max = ceiling(max(plot_loc$xrange_upgma)),
      value = ceiling(max(plot_loc$xrange_upgma) * 0.1),
      step = 0.5,
      width = "80px"
    )
  })
  
  output$upgma_fruit_width4 <- renderUI({
    numericInput(
      "upgma_fruit_width_circ_4",
      label = h5("Width", style = "color:white; margin-bottom: 0px"),
      min = ceiling(min(plot_loc$xrange_upgma)),
      max = ceiling(max(plot_loc$xrange_upgma)),
      value = ceiling(max(plot_loc$xrange_upgma) * 0.1),
      step = 0.5,
      width = "80px"
    )
  })
  
  output$upgma_fruit_width5 <- renderUI({
    numericInput(
      "upgma_fruit_width_circ_5",
      label = h5("Width", style = "color:white; margin-bottom: 0px"),
      min = ceiling(min(plot_loc$xrange_upgma)),
      max = ceiling(max(plot_loc$xrange_upgma)),
      value = ceiling(max(plot_loc$xrange_upgma) * 0.1),
      step = 0.5,
      width = "80px"
    )
  })
  
  output$nj_fruit_width <- renderUI({
    numericInput(
      "nj_fruit_width_circ",
      label = h5("Width", style = "color:white; margin-bottom: 0px"),
      min = ceiling(min(plot_loc$xrange_nj)),
      max = ceiling(max(plot_loc$xrange_nj)),
      value = ceiling(max(plot_loc$xrange_nj) * 0.1),
      step = 0.5,
      width = "80px"
    )
  })
  
  output$nj_fruit_width2 <- renderUI({
    numericInput(
      "nj_fruit_width_circ_2",
      label = h5("Width", style = "color:white; margin-bottom: 0px"),
      min = ceiling(min(plot_loc$xrange_nj)),
      max = ceiling(max(plot_loc$xrange_nj)),
      value = ceiling(max(plot_loc$xrange_nj) * 0.1),
      step = 0.5,
      width = "80px"
    )
  })
  
  output$nj_fruit_width3 <- renderUI({
    numericInput(
      "nj_fruit_width_circ_3",
      label = h5("Width", style = "color:white; margin-bottom: 0px"),
      min = ceiling(min(plot_loc$xrange_nj)),
      max = ceiling(max(plot_loc$xrange_nj)),
      value = ceiling(max(plot_loc$xrange_nj) * 0.1),
      step = 0.5,
      width = "80px"
    )
  })
  
  output$nj_fruit_width4 <- renderUI({
    numericInput(
      "nj_fruit_width_circ_4",
      label = h5("Width", style = "color:white; margin-bottom: 0px"),
      min = ceiling(min(plot_loc$xrange_nj)),
      max = ceiling(max(plot_loc$xrange_nj)),
      value = ceiling(max(plot_loc$xrange_nj) * 0.1),
      step = 0.5,
      width = "80px"
    )
  })
  
  output$nj_fruit_width5 <- renderUI({
    numericInput(
      "nj_fruit_width_circ_5",
      label = h5("Width", style = "color:white; margin-bottom: 0px"),
      min = ceiling(min(plot_loc$xrange_nj)),
      max = ceiling(max(plot_loc$xrange_nj)),
      value = ceiling(max(plot_loc$xrange_nj) * 0.1),
      step = 0.5,
      width = "80px"
    )
  })
  
  output$nj_tipcolor_mapping <- renderUI({
    selectInput(
      "nj_tipcolor_mapping",
      "",
      choices = if(ncol(DF1$meta) == 12) {
        c(
          `Assembly Name` = "Assembly Name",
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      } else {
        append(c(`Assembly Name` = "Assembly Name", `Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
               names(DF1$meta)[13:ncol(DF1$meta)])
      },
      selected = c("Host" = "Host"),
      width = "100%"
    )
  })
  
  output$upgma_tipcolor_mapping <- renderUI({
    selectInput(
      "upgma_tipcolor_mapping",
      "",
      choices = if(ncol(DF1$meta) == 12) {
        c(
          `Assembly Name` = "Assembly Name",
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      } else {
        append(c(`Assembly Name` = "Assembly Name", `Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
               names(DF1$meta)[13:ncol(DF1$meta)])
      },
      selected = c("Host" = "Host"),
      width = "100%"
    )
  })
  
  # Geom Fruit select Variable
  output$upgma_fruit_variable <- renderUI({
    selectInput(
      "upgma_fruit_variable",
      "",
      choices = if(ncol(DF1$meta) == 12) {
        c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City",
          Errors = "Errors"
        )
      } else {
        append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City", Errors = "Errors"),
               names(DF1$meta)[13:ncol(DF1$meta)])
      },
      selected = c("Country" = "Country"),
      width = "100%"
    )
  })
  
  output$upgma_fruit_variable2 <- renderUI({
    selectInput(
      "upgma_fruit_variable_2",
      "",
      choices = if(ncol(DF1$meta) == 12) {
        c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      } else {
        append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
               names(DF1$meta)[13:ncol(DF1$meta)])
      },
      selected = c("Country" = "Country"),
      width = "100%"
    )
  })
  
  output$upgma_fruit_variable3 <- renderUI({
    selectInput(
      "upgma_fruit_variable_3",
      "",
      choices = if(ncol(DF1$meta) == 12) {
        c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      } else {
        append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
               names(DF1$meta)[13:ncol(DF1$meta)])
      },
      selected = c("Country" = "Country"),
      width = "100%"
    )
  })
  
  output$upgma_fruit_variable4 <- renderUI({
    selectInput(
      "upgma_fruit_variable_4",
      "",
      choices = if(ncol(DF1$meta) == 12) {
        c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      } else {
        append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
               names(DF1$meta)[13:ncol(DF1$meta)])
      },
      selected = c("Country" = "Country"),
      width = "100%"
    )
  })
  
  output$upgma_fruit_variable5 <- renderUI({
    selectInput(
      "upgma_fruit_variable_5",
      "",
      choices = if(ncol(DF1$meta) == 12) {
        c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      } else {
        append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
               names(DF1$meta)[13:ncol(DF1$meta)])
      },
      selected = c("Country" = "Country"),
      width = "100%"
    )
  })
  
  output$nj_fruit_variable <- renderUI({
    selectInput(
      "nj_fruit_variable",
      "",
      choices = if(ncol(DF1$meta) == 12) {
        c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      } else {
        append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
               names(DF1$meta)[13:ncol(DF1$meta)])
      },
      selected = c("Country" = "Country"),
      width = "100%"
    )
  })
  
  output$nj_fruit_variable2 <- renderUI({
    selectInput(
      "nj_fruit_variable_2",
      "",
      choices = if(ncol(DF1$meta) == 12) {
        c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      } else {
        append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
               names(DF1$meta)[13:ncol(DF1$meta)])
      },
      selected = c("Country" = "Country"),
      width = "100%"
    )
  })
  
  output$nj_fruit_variable3 <- renderUI({
    selectInput(
      "nj_fruit_variable_3",
      "",
      choices = if(ncol(DF1$meta) == 12) {
        c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      } else {
        append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
               names(DF1$meta)[13:ncol(DF1$meta)])
      },
      selected = c("Country" = "Country"),
      width = "100%"
    )
  })
  
  output$nj_fruit_variable4 <- renderUI({
    selectInput(
      "nj_fruit_variable_4",
      "",
      choices = if(ncol(DF1$meta) == 12) {
        c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      } else {
        append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
               names(DF1$meta)[13:ncol(DF1$meta)])
      },
      selected = c("Country" = "Country"),
      width = "100%"
    )
  })
  
  output$nj_fruit_variable5 <- renderUI({
    selectInput(
      "nj_fruit_variable_5",
      "",
      choices = if(ncol(DF1$meta) == 12) {
        c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      } else {
        append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
               names(DF1$meta)[13:ncol(DF1$meta)])
      },
      selected = c("Country" = "Country"),
      width = "100%"
    )
  })
  
  # Tipshape Mapping
  output$nj_tipshape_mapping <- renderUI({
    selectInput(
      "nj_tipshape_mapping",
      "",
      choices = if(ncol(DF1$meta) == 12) {
        c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      } else {
        append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
               names(DF1$meta)[13:ncol(DF1$meta)])
      },
      selected = c("Host" = "Host"),
      width = "100%"
    )
  })
  
  output$upgma_tipshape_mapping <- renderUI({
    selectInput(
      "upgma_tipshape_mapping",
      "",
      choices = if(ncol(DF1$meta) == 12) {
        c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      } else {
        append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
               names(DF1$meta)[13:ncol(DF1$meta)])
      },
      selected = c("Host" = "Host"),
      width = "100%"
    )
  })
  
  output$upgma_branch_label <- renderUI({
    selectInput(
      "upgma_branch_label",
      "",
      choices = if(ncol(DF1$meta) == 12) {
        c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      } else {
        append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
               names(DF1$meta)[13:ncol(DF1$meta)])
      },
      selected = c("Host" = "Host"),
      width = "100%"
    )
  })
  
  output$nj_branch_label <- renderUI({
    selectInput(
      "nj_branch_label",
      "",
      choices = if(ncol(DF1$meta) == 12) {
        c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      } else {
        append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
               names(DF1$meta)[13:ncol(DF1$meta)])
      },
      selected = c("Host" = "Host"),
      width = "100%"
    )
  })
  
  output$nj_color_mapping <- renderUI({
    selectInput(
      "nj_color_mapping",
      "",
      choices = if(ncol(DF1$meta) == 12) {
        c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      } else {
        append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
               names(DF1$meta)[13:ncol(DF1$meta)])
      },
      selected = c("Host" = "Host"),
      width = "100%"
    )
  })
  
  output$upgma_color_mapping <- renderUI({
    selectInput(
      "upgma_color_mapping",
      "",
      choices = if(ncol(DF1$meta) == 12) {
        c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      } else {
        append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
               names(DF1$meta)[13:ncol(DF1$meta)])
      },
      selected = c("Host" = "Host"),
      width = "100%"
    )
  })
  
  output$mst_node_label <- renderUI({
    selectInput(
      "mst_node_label",
      label = "",
      choices = names(DF1$meta)[-c(2, 5, 10, 11, 12)],
      selected = "Assembly Name",
      width = "100%"
    )
  })
  
  output$nj_tiplab <- renderUI({
    selectInput(
      "nj_tiplab",
      label = "",
      choices = if(ncol(DF1$meta) == 12) {
        c(
          Index = "Index",
          `Assembly ID` = "Assembly ID",
          `Assembly Name` = "Assembly Name",
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      } else {
        append(c(Index = "Index", `Assembly ID` = "Assembly ID", `Assembly Name` = "Assembly Name",
                 `Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
               names(DF1$meta)[13:ncol(DF1$meta)])
      },
      selected = c(`Assembly Name` = "Assembly Name"),
      width = "100%"
    )
  })
  
  output$upgma_tiplab <- renderUI({
    selectInput(
      "upgma_tiplab",
      label = "",
      choices = if(ncol(DF1$meta) == 12) {
        c(
          Index = "Index",
          `Assembly ID` = "Assembly ID",
          `Assembly Name` = "Assembly Name",
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      } else {
        append(c(Index = "Index", `Assembly ID` = "Assembly ID", `Assembly Name` = "Assembly Name",
                 `Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
               names(DF1$meta)[13:ncol(DF1$meta)])
      },
      selected = c(`Assembly Name` = "Assembly Name"),
      width = "100%"
    )
  })
  
  ### Render Plot field ----
  
  output$mst_field <- renderUI({
    visNetworkOutput("tree_mst", width = paste0(as.character(as.numeric(input$mst_scale) * as.numeric(input$mst_ratio)), "px"), height = paste0(as.character(input$mst_scale), "px")) 
  })
  
  output$nj_field <- renderUI({
    plotOutput("tree_nj", width = paste0(as.character(as.numeric(input$nj_scale) * as.numeric(input$nj_ratio)), "px"), height = paste0(as.character(input$nj_scale), "px")) 
  })
  
  output$upgma_field <- renderUI({
    plotOutput("tree_upgma", width = paste0(as.character(as.numeric(input$upgma_scale) * as.numeric(input$upgma_ratio)), "px"), height = paste0(as.character(input$upgma_scale), "px")) 
  })
  
  ### Plot Reactives ----
  
  #### MST ----
  
  mst_tree <- reactive({
    data <- toVisNetworkData(plot_loc$ggraph_1)
    data$nodes <- mutate(data$nodes, 
                         label = label_mst(),
                         value = mst_node_scaling(),
                         opacity = node_opacity())
    data$edges <- mutate(data$edges,
                         length = if(input$mst_scale_edges == FALSE) {
                           input$mst_edge_length
                         } else {
                           data$edges$weight * input$mst_edge_length_scale
                         },
                         label = as.character(weight),
                         opacity = mst_edge_opacity())
    
    visNetwork(data$nodes, data$edges, 
               main = mst_title(),
               background = mst_background_color(),
               submain = mst_subtitle(),
               footer = mst_footer()) %>%
      visNodes(size = mst_node_size(), 
               shape = mst_node_shape(),
               shadow = input$mst_shadow,
               color = mst_color_node(),
               scaling = list(min = mst_node_size_min(), 
                              max = mst_node_size_max()),
               font = list(color = node_font_color(),
                           size = input$node_label_fontsize)) %>%
      visEdges(color = mst_color_edge(), 
               font = list(color = mst_edge_font_color(),
                           size = mst_edge_font_size(),
                           strokeWidth = 4)) %>%
      visOptions(collapse = TRUE) %>%
      visInteraction(hover = TRUE) %>%
      visLayout(randomSeed = 1) %>%
      visLegend()
  })
  
  # Set MST node shape
  mst_node_shape <- reactive({
    if(input$mst_node_shape %in% c("circle", "database", "box", "text")) {
      shinyjs::disable('scale_nodes') 
      updateCheckboxInput(session, "scale_nodes", value = FALSE)
      shinyjs::disable('mst_node_size') 
      shinyjs::disable('mst_node_scale')
      input$mst_node_shape
    } else {
      shinyjs::enable('scale_nodes') 
      shinyjs::enable('mst_node_size') 
      shinyjs::enable('mst_node_scale')
      input$mst_node_shape
    }
  })
  
  # Set MST label
  label_mst <- reactive({
    plot_loc$unique_meta[, colnames(plot_loc$unique_meta) %in% input$mst_node_label]
  })
  
  # Set node color
  mst_color_node <- reactive({
    input$mst_color_node
  })
  
  # Node Label Color
  node_font_color <- reactive({
    input$node_font_color
  })
  
  
  # Node Size Scaling
  mst_node_scaling <- reactive({
    if(input$scale_nodes == TRUE){
      plot_loc$unique_meta$size
    } else {NULL}
  })
  
  # Node Size Min/May
  mst_node_size_min <- reactive({
    input$mst_node_scale[1]
  })
  
  mst_node_size_max <- reactive({
    input$mst_node_scale[2]
  })
  
  # Node Size
  mst_node_size <- reactive({
    input$mst_node_size
  })
  
  # Node Alpha/Opacity
  node_opacity <- reactive({
    input$node_opacity
  })
  
  # Set Title
  mst_title <- reactive({
    list(text = input$mst_title,
         style = paste0(
           "font-family:Georgia, Times New Roman, Times, serif;",
           "text-align:center;",
           "font-size: ", as.character(input$mst_title_size), "px", 
           "; color: ", as.character(input$mst_title_color))
    )
  })
  
  # Set Subtitle
  mst_subtitle <- reactive({
    list(text = input$mst_subtitle,
         style = paste0(
           "font-family:Georgia, Times New Roman, Times, serif;",
           "text-align:center;",
           "font-size: ", as.character(input$mst_subtitle_size), "px", 
           "; color: ", as.character(input$mst_subtitle_color))
    )
  })
  
  # Set Footer
  mst_footer <- reactive({
    list(text = input$mst_footer,
         style = paste0(
           "font-family:Georgia, Times New Roman, Times, serif;",
           "text-align:center;",
           "font-size: ", as.character(input$mst_footer_size), "px", 
           "; color: ", as.character(input$mst_footer_color))
    )
  })
  
  # Background color
  
  mst_background_color <- reactive({
    if(input$mst_background_transparent == TRUE) {
      'rgba(0, 0, 0, 0)'
    } else{
      input$mst_background_color
    }
  })
  
  # Edge Opacity
  mst_edge_opacity <- reactive({
    input$mst_edge_opacity
  })
  
  # Edge font color
  mst_edge_font_color <- reactive({
    input$mst_edge_font_color
  })
  
  # Edge color
  mst_color_edge <- reactive({
    input$mst_color_edge
  })
  
  # Edge font size
  mst_edge_font_size <- reactive({
    input$mst_edge_font_size
  })
  
  #### NJ ----
  
  nj_tree <- reactive({
    tree <-
      ggtree(plot_loc$nj, 
             color = input$nj_color,
             layout = layout_nj(),
             ladderize = input$nj_ladder) %<+% plot_loc$meta_nj +
      nj_tiplab() +
      limit() +
      inward() +
      label_branch() +
      treescale() +
      nodepoint() +
      tippoint() +
      clip_label() +
      rootedge() +
      ggtitle(label = input$nj_title,
              subtitle = input$nj_subtitle) +
      theme_tree(bgcolor = input$nj_bg) +
      theme(plot.title = element_text(colour = input$nj_title_color,
                                      size = input$nj_title_size),
            plot.subtitle = element_text(colour = input$nj_subtitle_color,
                                         size = input$nj_subtitle_size),
            legend.background = element_rect(fill = input$nj_bg),
            legend.direction = input$nj_legend_orientation,
            legend.title = element_text(color = input$nj_color,
                                        size = input$nj_legend_size*1.2),
            legend.title.align = 0.5,
            legend.position = nj_legend_pos(),
            legend.text = element_text(color = input$nj_color, 
                                       size = input$nj_legend_size),
            legend.key = element_rect(fill = input$nj_bg),
            legend.box.spacing = unit(1.5, "cm"),
            legend.key.size = unit(0.05*input$nj_legend_size, 'cm'),
            plot.background = element_rect(fill = input$nj_bg))  +
      new_scale_fill() +
      nj_fruit() +
      nj_gradient() +
      new_scale_fill() +
      nj_fruit2() +
      nj_gradient2() +
      new_scale_fill() +
      nj_fruit3() +
      nj_gradient3() +
      new_scale_fill() +
      nj_fruit4() +
      nj_gradient4() +
      new_scale_fill() +
      nj_fruit5() +
      nj_gradient5() 
    
    
    plot_loc$xrange_nj <- tree$data$x
    plot_loc$yrange_nj <- tree$data$y
    
    if(input$nj_heatmap_show == TRUE & length(input$nj_heatmap_select) > 0) {
      tree <- gheatmap(tree, 
                       data = select(plot_loc$meta_nj, input$nj_heatmap_select),
                       offset = nj_heatmap_offset(),
                       width = input$nj_heatmap_width,
                       legend_title = input$nj_heatmap_title,
                       colnames_angle = -(input$nj_colnames_angle),
                       colnames_offset_x = input$nj_colnames_x,
                       colnames_offset_y = input$nj_colnames_y
      )
    } 
    
    plot_loc$nj_plot <- ggplotify::as.ggplot(tree, 
                                             scale = input$nj_zoom,
                                             hjust = input$nj_h,
                                             vjust = input$nj_v)  
    plot_loc$nj_plot
  })
  
  # Legend Position
  nj_legend_pos <- reactive({
    if(!is.null(input$nj_legend_x) & !is.null(input$nj_legend_y)) {
      c(input$nj_legend_x, input$nj_legend_y)
    } else {
      c(0.1, 1)
    }
  })
  
  # Heatmap offset
  nj_heatmap_offset <- reactive({
    if(is.null(input$nj_heatmap_offset)) {
      0
    } else {input$nj_heatmap_offset}
  })
  
  # Tiles fill color gradient
  nj_gradient <- reactive({
    if(input$nj_tiles_show == TRUE) {
      if(class(DF1$meta_true[[input$nj_fruit_variable]]) == "numeric") {
        if(input$nj_div_tiles == TRUE) {
          if(input$nj_tile_mid == "Median"){
            scale_fill_gradient2(low = input$nj_tile_color_low,
                                 mid = input$nj_tile_color_mid,
                                 midpoint = median(DF1$meta_true[[input$nj_fruit_variable]], na.rm = TRUE),
                                 high = input$nj_tile_color_high)
          } else if(input$nj_tile_mid == "Mean") {
            scale_fill_gradient2(low = input$nj_tile_color_low,
                                 mid = input$nj_tile_color_mid,
                                 midpoint = mean(DF1$meta_true[[input$nj_fruit_variable]], na.rm = TRUE),
                                 high = input$nj_tile_color_high)
          } else {
            scale_fill_gradient2(low = input$nj_tile_color_low,
                                 mid = input$nj_tile_color_mid,
                                 high = input$nj_tile_color_high)
          }
        } else {
          scale_fill_gradient(low = input$nj_tile_color_low,
                              high = input$nj_tile_color_high)
        }
      } else {NULL}
    } else {NULL}
  })
  
  nj_gradient2 <- reactive({
    if(input$nj_tiles_show_2 == TRUE) {
      if(class(DF1$meta_true[[input$nj_fruit_variable_2]]) == "numeric") {
        if(input$nj_div_tiles == TRUE) {
          if(input$nj_tile_mid == "Median"){
            scale_fill_gradient2(low = input$nj_tile_color_low_2,
                                 mid = input$nj_tile_color_mid_2,
                                 midpoint = median(DF1$meta_true[[input$nj_fruit_variable_2]], na.rm = TRUE),
                                 high = input$nj_tile_color_high_2)
          } else if(input$nj_tile_mid == "Mean") {
            scale_fill_gradient2(low = input$nj_tile_color_low_2,
                                 mid = input$nj_tile_color_mid_2,
                                 midpoint = mean(DF1$meta_true[[input$nj_fruit_variable_2]], na.rm = TRUE),
                                 high = input$nj_tile_color_high_2)
          } else {
            scale_fill_gradient2(low = input$nj_tile_color_low_2,
                                 mid = input$nj_tile_color_mid_2,
                                 high = input$nj_tile_color_high_2)
          }
        } else {
          scale_fill_gradient(low = input$nj_tile_color_low_2,
                              high = input$nj_tile_color_high_2)
        }
      } else {NULL}
    } else {NULL}
    
  })
  
  nj_gradient3 <- reactive({
    if(input$nj_tiles_show_3 == TRUE) {
      if(class(DF1$meta_true[[input$nj_fruit_variable_2]]) == "numeric") {
        if(input$nj_div_tiles == TRUE) {
          if(input$nj_tile_mid == "Median"){
            scale_fill_gradient2(low = input$nj_tile_color_low_2,
                                 mid = input$nj_tile_color_mid_2,
                                 midpoint = median(DF1$meta_true[[input$nj_fruit_variable_2]], na.rm = TRUE),
                                 high = input$nj_tile_color_high_2)
          } else if(input$nj_tile_mid == "Mean") {
            scale_fill_gradient2(low = input$nj_tile_color_low_2,
                                 mid = input$nj_tile_color_mid_2,
                                 midpoint = mean(DF1$meta_true[[input$nj_fruit_variable_2]], na.rm = TRUE),
                                 high = input$nj_tile_color_high_2)
          } else {
            scale_fill_gradient2(low = input$nj_tile_color_low_2,
                                 mid = input$nj_tile_color_mid_2,
                                 high = input$nj_tile_color_high_2)
          }
        } else {
          scale_fill_gradient(low = input$nj_tile_color_low_2,
                              high = input$nj_tile_color_high_2)
        }
      } else {NULL}
    }else {NULL}
    
  })
  
  nj_gradient4 <- reactive({
    if(input$nj_tiles_show_4 == TRUE) {
      if(class(DF1$meta_true[[input$nj_fruit_variable_4]]) == "numeric") {
        if(input$nj_div_tiles == TRUE) {
          if(input$nj_tile_mid == "Median"){
            scale_fill_gradient2(low = input$nj_tile_color_low_4,
                                 mid = input$nj_tile_color_mid_4,
                                 midpoint = median(DF1$meta_true[[input$nj_fruit_variable_4]], na.rm = TRUE),
                                 high = input$nj_tile_color_high_4)
          } else if(input$nj_tile_mid == "Mean") {
            scale_fill_gradient2(low = input$nj_tile_color_low_4,
                                 mid = input$nj_tile_color_mid_4,
                                 midpoint = mean(DF1$meta_true[[input$nj_fruit_variable_4]], na.rm = TRUE),
                                 high = input$nj_tile_color_high_4)
          } else {
            scale_fill_gradient2(low = input$nj_tile_color_low_4,
                                 mid = input$nj_tile_color_mid_4,
                                 high = input$nj_tile_color_high_4)
          }
        } else {
          scale_fill_gradient(low = input$nj_tile_color_low_4,
                              high = input$nj_tile_color_high_4)
        }
      } else {NULL}
    } else {NULL}
    
  })
  
  nj_gradient5 <- reactive({
    if(input$nj_tiles_show_5 == TRUE) {
      if(class(DF1$meta_true[[input$nj_fruit_variable_5]]) == "numeric") {
        if(input$nj_div_tiles == TRUE) {
          if(input$nj_tile_mid == "Median"){
            scale_fill_gradient2(low = input$nj_tile_color_low_5,
                                 mid = input$nj_tile_color_mid_5,
                                 midpoint = median(DF1$meta_true[[input$nj_fruit_variable_5]], na.rm = TRUE),
                                 high = input$nj_tile_color_high_5)
          } else if(input$nj_tile_mid == "Mean") {
            scale_fill_gradient2(low = input$nj_tile_color_low_5,
                                 mid = input$nj_tile_color_mid_5,
                                 midpoint = mean(DF1$meta_true[[input$nj_fruit_variable_5]], na.rm = TRUE),
                                 high = input$nj_tile_color_high_5)
          } else {
            scale_fill_gradient2(low = input$nj_tile_color_low_5,
                                 mid = input$nj_tile_color_mid_5,
                                 high = input$nj_tile_color_high_5)
          }
        } else {
          scale_fill_gradient(low = input$nj_tile_color_low_5,
                              high = input$nj_tile_color_high_5)
        }
      } else {NULL}
    } else {NULL}
    
  })
  
  # No label clip off for linear NJ tree
  
  clip_label <- reactive({
    if(!(input$nj_layout == "circular" | input$nj_layout == "inward")) {
      coord_cartesian(clip = "off")
    } else {NULL}
  })
  
  # Geom Fruit
  nj_fruit <- reactive({
    if(input$nj_tiles_show == TRUE) {
      if(input$nj_layout == "circular" | input$nj_layout == "inward") {
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill= !!sym(input$nj_fruit_variable)),
          offset = input$nj_fruit_offset_circ,
          pwidth = input$nj_fruit_width_circ,
          alpha = input$nj_fruit_alpha
        )
      } else {
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill= !!sym(input$nj_fruit_variable)),
          offset = input$nj_fruit_offset_circ,
          pwidth = input$nj_fruit_width_circ,
          alpha = input$nj_fruit_alpha
        )
      }
    } else {NULL}
  })
  
  # Geom Fruit
  nj_fruit2 <- reactive({
    if(input$nj_tiles_show_2 == TRUE) {
      if(input$nj_layout == "circular" | input$nj_layout == "inward") {
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill = !!sym(input$nj_fruit_variable_2)),
          offset = input$nj_fruit_offset_circ_2,
          pwidth = input$nj_fruit_width_circ_2,
          alpha = input$nj_fruit_alpha_2
        )
      } else {
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill = !!sym(input$nj_fruit_variable_2)),
          offset = input$nj_fruit_offset_circ_2,
          pwidth = input$nj_fruit_width_circ_2,
          alpha = input$nj_fruit_alpha_2
        )
      }
    } else {NULL}
  })
  
  nj_fruit3 <- reactive({
    if(input$nj_tiles_show_3 == TRUE) {
      if(input$nj_layout == "circular" | input$nj_layout == "inward") {
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill = !!sym(input$nj_fruit_variable_3)),
          offset = input$nj_fruit_offset_circ_3,
          pwidth = input$nj_fruit_width_circ_3,
          alpha = input$nj_fruit_alpha_3
        )
      } else {
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill = !!sym(input$nj_fruit_variable_3)),
          offset = input$nj_fruit_offset_circ_3,
          pwidth = input$nj_fruit_width_circ_3,
          alpha = input$nj_fruit_alpha_3
        )
      }
    } else {NULL}
    
    
  })
  
  nj_fruit4 <- reactive({
    if(input$nj_tiles_show_4 == TRUE) {
      if(input$nj_layout == "circular" | input$nj_layout == "inward") {
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill = !!sym(input$nj_fruit_variable_4)),
          offset = input$nj_fruit_offset_circ_4,
          pwidth = input$nj_fruit_width_circ_4,
          alpha = input$nj_fruit_alpha_4
        )
      } else {
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill = !!sym(input$nj_fruit_variable_4)),
          offset = input$nj_fruit_offset_circ_4,
          pwidth = input$nj_fruit_width_circ_4,
          alpha = input$nj_fruit_alpha_4
        )
      }
    } else {NULL}
    
  })
  
  nj_fruit5 <- reactive({
    if(input$nj_tiles_show_5 == TRUE) {
      if(input$nj_layout == "circular" | input$nj_layout == "inward") {
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill = !!sym(input$nj_fruit_variable_5)),
          offset = input$nj_fruit_offset_circ_5,
          pwidth = input$nj_fruit_width_circ_5,
          alpha = input$nj_fruit_alpha_5
        )
      } else {
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill = !!sym(input$nj_fruit_variable_5)),
          offset = input$nj_fruit_offset_circ_5,
          pwidth = input$nj_fruit_width_circ_5,
          alpha = input$nj_fruit_alpha_5
        )
      }
    } else {NULL}
    
  })
  
  # Xlim
  limit <- reactive({
    if(input$nj_layout == "circular") {
      xlim(input$nj_xlim, NA)
    } else {NULL}
  })
  
  # Treescale
  treescale <- reactive({
    if(!input$nj_layout == "circular") {
      if(input$nj_treescale_show == TRUE) {
        geom_treescale(x = input$nj_treescale_x,
                       y = input$nj_treescale_y,
                       width = input$nj_treescale_width,
                       color = input$nj_color)
      } else {NULL}
    } else {NULL}
  }) 
  
  # Label branches
  label_branch <- reactive({
    if(!input$nj_layout == "circular" | !input$nj_layout == "inward") {
      if(input$nj_show_branch_label == TRUE) {
        geom_label(
          aes(
            x=!!sym("branch"), 
            label= !sym(input$nj_branch_label)),
          fill = input$nj_branch_label_color,
          size = input$nj_branch_size,
          label.r = unit(input$nj_branch_labelradius, "lines"),
          nudge_x = input$nj_branch_x,
          nudge_y = input$nj_branch_y,
          fontface = input$nj_branchlab_fontface,
          alpha = input$nj_branchlab_alpha
        )
      } else {NULL}
    } else {NULL}
  })
  
  # Rootedge
  rootedge <- reactive({
    if(input$rootedge_show == TRUE) {
      geom_rootedge(rootedge = input$nj_rootedge_length,
                    linetype = input$nj_rootedge_line)
    } else {NULL}
  })
  
  # Tippoints
  tippoint <- reactive({
    if(input$nj_tippoint_show == TRUE) {
      if(input$nj_tipcolor_mapping_show == TRUE & input$nj_tipshape_mapping_show == FALSE) {
        if(input$nj_mapping_show == TRUE) {
          geom_tippoint(
            aes(shape = !!sym(input$nj_tipcolor_mapping)),
            alpha = input$nj_tippoint_alpha,
            color = input$nj_tippoint_color,
            size = input$nj_tippoint_size
          )
        } else {
          geom_tippoint(
            aes(color = !!sym(input$nj_tipcolor_mapping)),
            alpha = input$nj_tippoint_alpha,
            shape = input$nj_tippoint_shape,
            size = input$nj_tippoint_size
          )
        }
      } else if (input$nj_tipcolor_mapping_show == FALSE & input$nj_tipshape_mapping_show == TRUE) {
        geom_tippoint(
          aes(shape = !!sym(input$nj_tipshape_mapping)),
          alpha = input$nj_tippoint_alpha,
          color = input$nj_tippoint_color,
          size = input$nj_tippoint_size
        )
      } else if (input$nj_tipcolor_mapping_show == TRUE & input$nj_tipshape_mapping_show == TRUE) {
        if(input$nj_mapping_show == TRUE) {
          geom_tippoint(
            aes(shape = !!sym(input$nj_tipshape_mapping)),
            color = input$nj_tippoint_color,
            alpha = input$nj_tippoint_alpha,
            size = input$nj_tippoint_size
          )
        } else {
          geom_tippoint(
            aes(shape = !!sym(input$nj_tipshape_mapping),
                color = !!sym(input$nj_tipcolor_mapping)),
            alpha = input$nj_tippoint_alpha,
            size = input$nj_tippoint_size
          )
        }
      } else {
        geom_tippoint(
          alpha = input$nj_tippoint_alpha,
          colour = input$nj_tippoint_color,
          fill = input$nj_tippoint_color,
          shape = input$nj_tippoint_shape,
          size = input$nj_tippoint_size
        )
      } 
    } else {NULL
      
    }
  })
  
  # Nodepoints
  nodepoint <- reactive({
    if(input$nj_nodepoint_show == TRUE) {
      geom_nodepoint(
        alpha = input$nj_nodepoint_alpha,
        color = input$nj_nodepoint_color,
        shape = input$nj_nodepoint_shape,
        size = input$nj_nodepoint_size
      )
    } else {NULL}
  })
  
  # NJ circular or not
  nj_tiplab <- reactive({
    if(input$nj_tiplab_show == TRUE) {
      if(input$nj_layout == "circular") {
        if(input$nj_mapping_show == TRUE) {
          geom_tiplab(
            mapping_tiplab(), 
            geom = "text",
            size = input$tiplab_size,
            linesize = input$nj_tiplab_linesize,
            linetype = input$nj_tiplab_linetype,
            alpha = input$nj_tiplab_alpha,
            fontface = input$nj_tiplab_fontface,
            align = as.logical(input$nj_align),
            hjust = input$nj_tiplab_position,
            check.overlap = input$nj_tiplab_overlap
          )
        } else {
          geom_tiplab(
            mapping_tiplab(),
            color = input$nj_tiplab_color,
            geom = "text",
            size = input$tiplab_size,
            linesize = input$nj_tiplab_linesize,
            linetype = input$nj_tiplab_linetype,
            alpha = input$nj_tiplab_alpha,
            fontface = input$nj_tiplab_fontface,
            align = as.logical(input$nj_align),
            hjust = input$nj_tiplab_position,
            check.overlap = input$nj_tiplab_overlap
          )
        }
      } else if (input$nj_layout == "inward") {
        if(input$nj_mapping_show == TRUE) {
          geom_tiplab(
            mapping_tiplab(), 
            geom = "text",
            size = input$tiplab_size,
            linesize = input$nj_tiplab_linesize,
            linetype = input$nj_tiplab_linetype,
            alpha = input$nj_tiplab_alpha,
            fontface = input$nj_tiplab_fontface,
            align = as.logical(input$nj_align),
            hjust = input$nj_tiplab_position_inw,
            check.overlap = input$nj_tiplab_overlap
          )
        } else {
          geom_tiplab(
            mapping_tiplab(),
            color = input$nj_tiplab_color,
            geom = "text",
            size = input$tiplab_size,
            linesize = input$nj_tiplab_linesize,
            linetype = input$nj_tiplab_linetype,
            alpha = input$nj_tiplab_alpha,
            fontface = input$nj_tiplab_fontface,
            align = as.logical(input$nj_align),
            hjust = input$nj_tiplab_position_inw,
            check.overlap = input$nj_tiplab_overlap
          )
        }
      } else {
        if(input$nj_mapping_show == TRUE) {
          geom_tiplab(
            mapping_tiplab(), 
            geom = nj_geom(),
            angle = input$nj_tiplab_angle,
            size = input$tiplab_size,
            linesize = input$nj_tiplab_linesize,
            linetype = input$nj_tiplab_linetype,
            alpha = input$nj_tiplab_alpha,
            fontface = input$nj_tiplab_fontface,
            align = as.logical(input$nj_align),
            nudge_x = input$nj_tiplab_nudge_x,
            check.overlap = input$nj_tiplab_overlap,
            label.padding = unit(input$nj_tiplab_padding, "lines"),
            label.r = unit(input$nj_tiplab_labelradius, "lines"), 
            fill = input$nj_tiplab_fill
          )
        } else {
          geom_tiplab(
            mapping_tiplab(), 
            geom = nj_geom(),
            color = input$nj_tiplab_color,
            angle = input$nj_tiplab_angle,
            size = input$tiplab_size,
            linesize = input$nj_tiplab_linesize,
            linetype = input$nj_tiplab_linetype,
            alpha = input$nj_tiplab_alpha,
            fontface = input$nj_tiplab_fontface,
            align = as.logical(input$nj_align),
            nudge_x = input$nj_tiplab_nudge_x,
            check.overlap = input$nj_tiplab_overlap,
            label.padding = unit(input$nj_tiplab_padding, "lines"),
            label.r = unit(input$nj_tiplab_labelradius, "lines"), 
            fill = input$nj_tiplab_fill
          )
        }
      }
    } else {NULL}
    
    
  })
  
  # Show Label Panels?
  
  nj_geom <- reactive({
    if(input$nj_geom == TRUE) {
      "label"
    } else {"text"}
  })
  
  # NJ Tiplab color
  mapping_tiplab <- reactive({
    if(input$nj_mapping_show == TRUE) {
      aes(label = !!sym(input$nj_tiplab),
          colour = !!sym(input$nj_color_mapping))
    } else {
      aes(label = !!sym(input$nj_tiplab))
    }
  })
  
  # NJ Tree Layout
  layout_nj <- reactive({
    if(input$nj_layout == "inward") {
      "circular"
    } else {input$nj_layout}
  })
  
  # NJ inward circular
  inward <- reactive({
    if (input$nj_layout == "inward") {
      layout_inward_circular(xlim = input$nj_inward_xlim)
    } else {
      NULL
    }
  })
  
  #### UPGMA ----
  
  upgma_tree <- reactive({
    plot_loc$meta_upgma$Errors <- as.numeric(plot_loc$meta_upgma$Errors)
    tree <-
      ggtree(plot_loc$upgma, 
             color = input$upgma_color,
             layout = layout_upgma(),
             ladderize = input$upgma_ladder) %<+% plot_loc$meta_upgma +
      upgma_tiplab() +
      upgma_limit() +
      upgma_inward() +
      upgma_label_branch() +
      upgma_treescale() +
      upgma_nodepoint() +
      upgma_tippoint() +
      upgma_clip_label() +
      upgma_rootedge() +
      ggtitle(label = input$upgma_title,
              subtitle = input$upgma_subtitle) +
      theme_tree(bgcolor = input$upgma_bg) +
      theme(plot.title = element_text(colour = input$upgma_title_color,
                                      size = input$upgma_title_size),
            plot.subtitle = element_text(colour = input$upgma_subtitle_color,
                                         size = input$upgma_subtitle_size),
            legend.background = element_rect(fill = input$upgma_bg),
            legend.direction = input$upgma_legend_orientation,
            legend.position = upgma_legend_pos(),
            legend.title = element_text(color = input$upgma_color,
                                        size = input$upgma_legend_size*1.2),
            legend.title.align = 0.5,
            legend.text = element_text(color = input$upgma_color, 
                                       size = input$upgma_legend_size),
            legend.key = element_rect(fill = input$upgma_bg),
            legend.box.spacing = unit(1.5, "cm"),
            legend.key.size = unit(0.05*input$upgma_legend_size, 'cm'),
            plot.background = element_rect(fill = input$upgma_bg)) +
      new_scale_fill() +
      upgma_fruit() +
      upgma_gradient() +
      new_scale_fill() +
      upgma_fruit2() +
      upgma_gradient2() +
      new_scale_fill() +
      upgma_fruit3() +
      upgma_gradient3() +
      new_scale_fill() +
      upgma_fruit4() +
      upgma_gradient4() +
      new_scale_fill() +
      upgma_fruit5() +
      upgma_gradient5() 
    
    plot_loc$xrange_upgma <- tree$data$x
    plot_loc$yrange_upgma <- tree$data$y
    
    if(input$upgma_heatmap_show == TRUE & length(input$upgma_heatmap_select) > 0) {
      tree <- gheatmap(tree, 
                       data = select(plot_loc$meta_upgma, input$upgma_heatmap_select),
                       offset = upgma_heatmap_offset(),
                       width = input$upgma_heatmap_width,
                       legend_title = input$upgma_heatmap_title,
                       colnames_angle = -(input$upgma_colnames_angle),
                       colnames_offset_x = input$upgma_colnames_x,
                       colnames_offset_y = input$upgma_colnames_y
      )
    } 
    
    plot_loc$upgma_plot <- ggplotify::as.ggplot(tree, 
                                                scale = input$upgma_zoom,
                                                hjust = input$upgma_h,
                                                vjust = input$upgma_v)  
    
    plot_loc$upgma_plot
  })
  
  # Legend Position
  upgma_legend_pos <- reactive({
    if(!is.null(input$upgma_legend_x) & !is.null(input$upgma_legend_y)) {
      c(input$upgma_legend_x, input$upgma_legend_y)
    } else {
      c(0.1, 1)
    }
  })
  
  # Heatmap offset
  upgma_heatmap_offset <- reactive({
    if(is.null(input$upgma_heatmap_offset)) {
      0
    } else {input$upgma_heatmap_offset}
  })
  
  # Tiles fill color gradient
  upgma_gradient <- reactive({
    if(input$upgma_tiles_show == TRUE) {
      if(class(DF1$meta_true[[input$upgma_fruit_variable]]) == "numeric") {
        if(input$upgma_div_tiles == TRUE) {
          if(input$upgma_tile_mid == "Median"){
            scale_fill_gradient2(low = input$upgma_tile_color_low,
                                 mid = input$upgma_tile_color_mid,
                                 midpoint = median(DF1$meta_true[[input$upgma_fruit_variable]], na.rm = TRUE),
                                 high = input$upgma_tile_color_high)
          } else if(input$upgma_tile_mid == "Mean") {
            scale_fill_gradient2(low = input$upgma_tile_color_low,
                                 mid = input$upgma_tile_color_mid,
                                 midpoint = mean(DF1$meta_true[[input$upgma_fruit_variable]], na.rm = TRUE),
                                 high = input$upgma_tile_color_high)
          } else {
            scale_fill_gradient2(low = input$upgma_tile_color_low,
                                 mid = input$upgma_tile_color_mid,
                                 high = input$upgma_tile_color_high)
          }
        } else {
          scale_fill_gradient(low = input$upgma_tile_color_low,
                              high = input$upgma_tile_color_high)
        }
      } else {NULL}
    } else {NULL}
  })
  
  upgma_gradient2 <- reactive({
    if(input$upgma_tiles_show_2 == TRUE) {
      if(class(DF1$meta_true[[input$upgma_fruit_variable_2]]) == "numeric") {
        if(input$upgma_div_tiles == TRUE) {
          if(input$upgma_tile_mid == "Median"){
            scale_fill_gradient2(low = input$upgma_tile_color_low_2,
                                 mid = input$upgma_tile_color_mid_2,
                                 midpoint = median(DF1$meta_true[[input$upgma_fruit_variable_2]], na.rm = TRUE),
                                 high = input$upgma_tile_color_high_2)
          } else if(input$upgma_tile_mid == "Mean") {
            scale_fill_gradient2(low = input$upgma_tile_color_low_2,
                                 mid = input$upgma_tile_color_mid_2,
                                 midpoint = mean(DF1$meta_true[[input$upgma_fruit_variable_2]], na.rm = TRUE),
                                 high = input$upgma_tile_color_high_2)
          } else {
            scale_fill_gradient2(low = input$upgma_tile_color_low_2,
                                 mid = input$upgma_tile_color_mid_2,
                                 high = input$upgma_tile_color_high_2)
          }
        } else {
          scale_fill_gradient(low = input$upgma_tile_color_low_2,
                              high = input$upgma_tile_color_high_2)
        }
      } else {NULL}
    } else {NULL}
    
  })
  
  upgma_gradient3 <- reactive({
    if(input$upgma_tiles_show_3 == TRUE) {
      if(class(DF1$meta_true[[input$upgma_fruit_variable_2]]) == "numeric") {
        if(input$upgma_div_tiles == TRUE) {
          if(input$upgma_tile_mid == "Median"){
            scale_fill_gradient2(low = input$upgma_tile_color_low_2,
                                 mid = input$upgma_tile_color_mid_2,
                                 midpoint = median(DF1$meta_true[[input$upgma_fruit_variable_2]], na.rm = TRUE),
                                 high = input$upgma_tile_color_high_2)
          } else if(input$upgma_tile_mid == "Mean") {
            scale_fill_gradient2(low = input$upgma_tile_color_low_2,
                                 mid = input$upgma_tile_color_mid_2,
                                 midpoint = mean(DF1$meta_true[[input$upgma_fruit_variable_2]], na.rm = TRUE),
                                 high = input$upgma_tile_color_high_2)
          } else {
            scale_fill_gradient2(low = input$upgma_tile_color_low_2,
                                 mid = input$upgma_tile_color_mid_2,
                                 high = input$upgma_tile_color_high_2)
          }
        } else {
          scale_fill_gradient(low = input$upgma_tile_color_low_2,
                              high = input$upgma_tile_color_high_2)
        }
      } else {NULL}
    }else {NULL}
    
  })
  
  upgma_gradient4 <- reactive({
    if(input$upgma_tiles_show_4 == TRUE) {
      if(class(DF1$meta_true[[input$upgma_fruit_variable_4]]) == "numeric") {
        if(input$upgma_div_tiles == TRUE) {
          if(input$upgma_tile_mid == "Median"){
            scale_fill_gradient2(low = input$upgma_tile_color_low_4,
                                 mid = input$upgma_tile_color_mid_4,
                                 midpoint = median(DF1$meta_true[[input$upgma_fruit_variable_4]], na.rm = TRUE),
                                 high = input$upgma_tile_color_high_4)
          } else if(input$upgma_tile_mid == "Mean") {
            scale_fill_gradient2(low = input$upgma_tile_color_low_4,
                                 mid = input$upgma_tile_color_mid_4,
                                 midpoint = mean(DF1$meta_true[[input$upgma_fruit_variable_4]], na.rm = TRUE),
                                 high = input$upgma_tile_color_high_4)
          } else {
            scale_fill_gradient2(low = input$upgma_tile_color_low_4,
                                 mid = input$upgma_tile_color_mid_4,
                                 high = input$upgma_tile_color_high_4)
          }
        } else {
          scale_fill_gradient(low = input$upgma_tile_color_low_4,
                              high = input$upgma_tile_color_high_4)
        }
      } else {NULL}
    } else {NULL}
    
  })
  
  upgma_gradient5 <- reactive({
    if(input$upgma_tiles_show_5 == TRUE) {
      if(class(DF1$meta_true[[input$upgma_fruit_variable_5]]) == "numeric") {
        if(input$upgma_div_tiles == TRUE) {
          if(input$upgma_tile_mid == "Median"){
            scale_fill_gradient2(low = input$upgma_tile_color_low_5,
                                 mid = input$upgma_tile_color_mid_5,
                                 midpoint = median(DF1$meta_true[[input$upgma_fruit_variable_5]], na.rm = TRUE),
                                 high = input$upgma_tile_color_high_5)
          } else if(input$upgma_tile_mid == "Mean") {
            scale_fill_gradient2(low = input$upgma_tile_color_low_5,
                                 mid = input$upgma_tile_color_mid_5,
                                 midpoint = mean(DF1$meta_true[[input$upgma_fruit_variable_5]], na.rm = TRUE),
                                 high = input$upgma_tile_color_high_5)
          } else {
            scale_fill_gradient2(low = input$upgma_tile_color_low_5,
                                 mid = input$upgma_tile_color_mid_5,
                                 high = input$upgma_tile_color_high_5)
          }
        } else {
          scale_fill_gradient(low = input$upgma_tile_color_low_5,
                              high = input$upgma_tile_color_high_5)
        }
      } else {NULL}
    } else {NULL}
    
  })
  
  
  # No label clip off for linear UPGMA tree
  
  upgma_clip_label <- reactive({
    if(!(input$upgma_layout == "circular" | input$upgma_layout == "inward")) {
      coord_cartesian(clip = "off")
    } else {NULL}
  })
  
  # Geom Fruit
  upgma_fruit <- reactive({
    if(input$upgma_tiles_show == TRUE) {
      if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill = !!sym(input$upgma_fruit_variable)),
          offset = input$upgma_fruit_offset_circ,
          pwidth = input$upgma_fruit_width_circ,
          alpha = input$upgma_fruit_alpha
        )
      } else {
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill = !!sym(input$upgma_fruit_variable)),
          offset = input$upgma_fruit_offset_circ,
          pwidth = input$upgma_fruit_width_circ,
          alpha = input$upgma_fruit_alpha
        )
      }
    } else {NULL}
  })
  
  # Geom Fruit
  upgma_fruit2 <- reactive({
    if(input$upgma_tiles_show_2 == TRUE) {
      if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill = !!sym(input$upgma_fruit_variable_2)),
          offset = input$upgma_fruit_offset_circ_2,
          pwidth = input$upgma_fruit_width_circ_2,
          alpha = input$upgma_fruit_alpha_2
        )
      } else {
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill = !!sym(input$upgma_fruit_variable_2)),
          offset = input$upgma_fruit_offset_circ_2,
          pwidth = input$upgma_fruit_width_circ_2,
          alpha = input$upgma_fruit_alpha_2
        )
      }
    } else {NULL}
  })
  
  upgma_fruit3 <- reactive({
    if(input$upgma_tiles_show_3 == TRUE) {
      if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill = !!sym(input$upgma_fruit_variable_3)),
          offset = input$upgma_fruit_offset_circ_3,
          pwidth = input$upgma_fruit_width_circ_3,
          alpha = input$upgma_fruit_alpha_3
        )
      } else {
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill = !!sym(input$upgma_fruit_variable_3)),
          offset = input$upgma_fruit_offset_circ_3,
          pwidth = input$upgma_fruit_width_circ_3,
          alpha = input$upgma_fruit_alpha_3
        )
      }
    } else {NULL}
    
    
  })
  
  upgma_fruit4 <- reactive({
    if(input$upgma_tiles_show_4 == TRUE) {
      if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill = !!sym(input$upgma_fruit_variable_4)),
          offset = input$upgma_fruit_offset_circ_4,
          pwidth = input$upgma_fruit_width_circ_4,
          alpha = input$upgma_fruit_alpha_4
        )
      } else {
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill = !!sym(input$upgma_fruit_variable_4)),
          offset = input$upgma_fruit_offset_circ_4,
          pwidth = input$upgma_fruit_width_circ_4,
          alpha = input$upgma_fruit_alpha_4
        )
      }
    } else {NULL}
    
  })
  
  upgma_fruit5 <- reactive({
    if(input$upgma_tiles_show_5 == TRUE) {
      if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill = !!sym(input$upgma_fruit_variable_5)),
          offset = input$upgma_fruit_offset_circ_5,
          pwidth = input$upgma_fruit_width_circ_5,
          alpha = input$upgma_fruit_alpha_5
        )
      } else {
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill = !!sym(input$upgma_fruit_variable_5)),
          offset = input$upgma_fruit_offset_circ_5,
          pwidth = input$upgma_fruit_width_circ_5,
          alpha = input$upgma_fruit_alpha_5
        )
      }
    } else {NULL}
    
  })
  
  # Xlim
  upgma_limit <- reactive({
    if(input$upgma_layout == "circular") {
      xlim(input$upgma_xlim, NA)
    } else {NULL}
  })
  
  # Treescale
  upgma_treescale <- reactive({
    if(!input$upgma_layout == "circular") {
      if(input$upgma_treescale_show == TRUE) {
        geom_treescale(x = input$upgma_treescale_x,
                       y = input$upgma_treescale_y,
                       width = input$upgma_treescale_width,
                       color = input$upgma_color)
      } else {NULL}
    } else {NULL}
  }) 
  
  # Label branches
  upgma_label_branch <- reactive({
    if(!input$upgma_layout == "circular" | !input$upgma_layout == "inward") {
      if(input$upgma_show_branch_label == TRUE) {
        geom_label(
          aes(
            x = !!sym("branch"), 
            label = !!sym(input$upgma_branch_label)),
          fill = input$upgma_branch_label_color,
          size = input$upgma_branch_size,
          label.r = unit(input$upgma_branch_labelradius, "lines"),
          nudge_x = input$upgma_branch_x,
          nudge_y = input$upgma_branch_y,
          fontface = input$upgma_branchlab_fontface,
          alpha = input$upgma_branchlab_alpha
        )
      } else {NULL}
    } else {NULL}
  })
  
  # Rootedge
  upgma_rootedge <- reactive({
    if(input$upgma_rootedge_show == TRUE) {
      geom_rootedge(rootedge = input$upgma_rootedge_length,
                    linetype = input$upgma_rootedge_line)
    } else {NULL}
  })
  
  # Tippoints
  upgma_tippoint <- reactive({
    if(input$upgma_tippoint_show == TRUE) {
      if(input$upgma_tipcolor_mapping_show == TRUE & input$upgma_tipshape_mapping_show == FALSE) {
        if(input$upgma_mapping_show == TRUE) {
          geom_tippoint(
            aes(shape = !!sym(input$upgma_tipcolor_mapping)),
            alpha = input$upgma_tippoint_alpha,
            color = input$upgma_tippoint_color,
            size = input$upgma_tippoint_size
          )
        } else {
          geom_tippoint(
            aes(color = !!sym(input$upgma_tipcolor_mapping)),
            alpha = input$upgma_tippoint_alpha,
            shape = input$upgma_tippoint_shape,
            size = input$upgma_tippoint_size
          )
        }
      } else if (input$upgma_tipcolor_mapping_show == FALSE & input$upgma_tipshape_mapping_show == TRUE) {
        geom_tippoint(
          aes(shape = !!sym(input$upgma_tipshape_mapping)),
          alpha = input$upgma_tippoint_alpha,
          color = input$upgma_tippoint_color,
          size = input$upgma_tippoint_size
        )
      } else if (input$upgma_tipcolor_mapping_show == TRUE & input$upgma_tipshape_mapping_show == TRUE) {
        if(input$upgma_mapping_show == TRUE) {
          geom_tippoint(
            aes(shape = !!sym(input$upgma_tipshape_mapping)),
            color = input$upgma_tippoint_color,
            alpha = input$upgma_tippoint_alpha,
            size = input$upgma_tippoint_size
          )
        } else {
          geom_tippoint(
            aes(shape = !!sym(input$upgma_tipshape_mapping),
                color = !!sym(input$upgma_tipcolor_mapping)),
            alpha = input$upgma_tippoint_alpha,
            size = input$upgma_tippoint_size
          )
        }
      } else {
        geom_tippoint(
          alpha = input$upgma_tippoint_alpha,
          colour = input$upgma_tippoint_color,
          fill = input$upgma_tippoint_color,
          shape = input$upgma_tippoint_shape,
          size = input$upgma_tippoint_size
        )
      } 
    } else {NULL
      
    }
  })
  
  # Nodepoints
  upgma_nodepoint <- reactive({
    if(input$upgma_nodepoint_show == TRUE) {
      geom_nodepoint(
        alpha = input$upgma_nodepoint_alpha,
        color = input$upgma_nodepoint_color,
        shape = input$upgma_nodepoint_shape,
        size = input$upgma_nodepoint_size
      )
    } else {NULL}
  })
  
  # upgma circular or not
  upgma_tiplab <- reactive({
    if(input$upgma_tiplab_show == TRUE) {
      if(input$upgma_layout == "circular") {
        if(input$upgma_mapping_show == TRUE) {
          geom_tiplab(
            upgma_mapping_tiplab(), 
            geom = "text",
            size = input$upgma_tiplab_size,
            linesize = input$upgma_tiplab_linesize,
            linetype = input$upgma_tiplab_linetype,
            alpha = input$upgma_tiplab_alpha,
            fontface = input$upgma_tiplab_fontface,
            align = as.logical(input$upgma_align),
            hjust = input$upgma_tiplab_position,
            check.overlap = input$upgma_tiplab_overlap
          )
        } else {
          geom_tiplab(
            upgma_mapping_tiplab(),
            color = input$upgma_tiplab_color,
            geom = "text",
            size = input$upgma_tiplab_size,
            linesize = input$upgma_tiplab_linesize,
            linetype = input$upgma_tiplab_linetype,
            alpha = input$upgma_tiplab_alpha,
            fontface = input$upgma_tiplab_fontface,
            align = as.logical(input$upgma_align),
            hjust = input$upgma_tiplab_position,
            check.overlap = input$upgma_tiplab_overlap
          )
        }
      } else if (input$upgma_layout == "inward") {
        if(input$upgma_mapping_show == TRUE) {
          geom_tiplab(
            upgma_mapping_tiplab(), 
            geom = "text",
            size = input$upgma_tiplab_size,
            linesize = input$upgma_tiplab_linesize,
            linetype = input$upgma_tiplab_linetype,
            alpha = input$upgma_tiplab_alpha,
            fontface = input$upgma_tiplab_fontface,
            align = as.logical(input$upgma_align),
            hjust = input$upgma_tiplab_position_inw,
            check.overlap = input$upgma_tiplab_overlap
          )
        } else {
          geom_tiplab(
            upgma_mapping_tiplab(),
            color = input$upgma_tiplab_color,
            geom = "text",
            size = input$upgma_tiplab_size,
            linesize = input$upgma_tiplab_linesize,
            linetype = input$upgma_tiplab_linetype,
            alpha = input$upgma_tiplab_alpha,
            fontface = input$upgma_tiplab_fontface,
            align = as.logical(input$upgma_align),
            hjust = input$upgma_tiplab_position_inw,
            check.overlap = input$upgma_tiplab_overlap
          )
        }
      } else {
        if(input$upgma_mapping_show == TRUE) {
          geom_tiplab(
            upgma_mapping_tiplab(), 
            geom = upgma_geom(),
            angle = input$upgma_tiplab_angle,
            size = input$upgma_tiplab_size,
            linesize = input$upgma_tiplab_linesize,
            linetype = input$upgma_tiplab_linetype,
            alpha = input$upgma_tiplab_alpha,
            fontface = input$upgma_tiplab_fontface,
            align = as.logical(input$upgma_align),
            nudge_x = input$upgma_tiplab_nudge_x,
            check.overlap = input$upgma_tiplab_overlap,
            label.padding = unit(input$upgma_tiplab_padding, "lines"),
            label.r = unit(input$upgma_tiplab_labelradius, "lines"), 
            fill = input$upgma_tiplab_fill
          )
        } else {
          geom_tiplab(
            upgma_mapping_tiplab(), 
            geom = upgma_geom(),
            color = input$upgma_tiplab_color,
            angle = input$upgma_tiplab_angle,
            size = input$upgma_tiplab_size,
            linesize = input$upgma_tiplab_linesize,
            linetype = input$upgma_tiplab_linetype,
            alpha = input$upgma_tiplab_alpha,
            fontface = input$upgma_tiplab_fontface,
            align = as.logical(input$upgma_align),
            nudge_x = input$upgma_tiplab_nudge_x,
            check.overlap = input$upgma_tiplab_overlap,
            label.padding = unit(input$upgma_tiplab_padding, "lines"),
            label.r = unit(input$upgma_tiplab_labelradius, "lines"), 
            fill = input$upgma_tiplab_fill
          )
        }
      }
    } else {NULL}
    
    
  })
  
  # Show Label Panels?
  
  upgma_geom <- reactive({
    if(input$upgma_geom == TRUE) {
      "label"
    } else {"text"}
  })
  
  # upgma Tiplab color
  upgma_mapping_tiplab <- reactive({
    if(input$upgma_mapping_show == TRUE) {
      aes(label = !!sym(input$upgma_tiplab),
          colour = !!sym(input$upgma_color_mapping))
    } else {
      aes(label = !!sym(input$upgma_tiplab))
    }
  })
  
  # upgma Tree Layout
  layout_upgma <- reactive({
    if(input$upgma_layout == "inward") {
      "circular"
    } else {input$upgma_layout}
  })
  
  # upgma inward circular
  upgma_inward <- reactive({
    if (input$upgma_layout == "inward") {
      layout_inward_circular(xlim = input$upgma_inward_xlim)
    } else {
      NULL
    }
  })
  
  ### Save MST Plot ----
  output$save_plot_html <- downloadHandler(
    filename = function() {
      paste0("MST_", Sys.Date(), ".html")
    },
    content = function(file) {
      mst_tree() %>% visSave(file = file, background = mst_background_color())
    }
  )
  
  ### Save NJ Plot ----
  
  # Define download handler to save the plot
  
  output$download_nj <- downloadHandler(
    filename = function() {
      paste0("NJ_", Sys.Date(), ".", input$filetype_nj)
    },
    content = function(file) {
      if (input$filetype_nj == "png") {
        png(file, width = (as.numeric(input$nj_scale) * as.numeric(input$nj_ratio)), height = as.numeric(input$nj_scale))
        print(nj_tree())
        dev.off()
      } else if (input$filetype_nj == "jpeg") {
        jpeg(file, width = (as.numeric(input$nj_scale) * as.numeric(input$nj_ratio)), height = as.numeric(input$nj_scale), quality = 100)
        print(nj_tree())
        dev.off()
      } else if (input$filetype_nj == "svg") {
        plot <- print(nj_tree())
        ggsave(file=file, plot=plot, device = svg(width = (as.numeric(input$nj_scale) * as.numeric(input$nj_ratio))/96,
                                                  height = as.numeric(input$nj_scale)/96))
      } else if (input$filetype_nj == "bmp") {
        bmp(file, width = (as.numeric(input$nj_scale) * as.numeric(input$nj_ratio)), height = as.numeric(input$nj_scale))
        print(nj_tree())
        dev.off()
      }
    }
  )
  
  ### Save UPGMA Plot ----
  
  # Define download handler to save the plot
  
  output$download_upgma <- downloadHandler(
    filename = function() {
      paste0("UPGMA_", Sys.Date(), ".", input$filetype_upgma)
    },
    content = function(file) {
      if (input$filetype_upgma == "png") {
        png(file, width = (as.numeric(input$upgma_scale) * as.numeric(input$upgma_ratio)), height = as.numeric(input$upgma_scale))
        print(upgma_tree())
        dev.off()
      } else if (input$filetype_upgma == "jpeg") {
        jpeg(file, width = (as.numeric(input$upgma_scale) * as.numeric(input$upgma_ratio)), height = as.numeric(input$upgma_scale), quality = 100)
        print(upgma_tree())
        dev.off()
      } else if (input$filetype_upgma == "svg") {
        plot <- print(upgma_tree())
        ggsave(file=file, plot=plot, device = svg(width = (as.numeric(input$upgma_scale) * as.numeric(input$upgma_ratio))/96,
                                                  height = as.numeric(input$upgma_scale)/96))
      } else if (input$filetype_upgma == "bmp") {
        bmp(file, width = (as.numeric(input$upgma_scale) * as.numeric(input$upgma_ratio)), height = as.numeric(input$upgma_scale))
        print(upgma_tree())
        dev.off()
      }
    }
  )
  
  
  ### Reactive Events ----
  
  # Shut off "Align Labels" control for UPGMA trees
  shinyjs::disable('upgma_align')
  shinyjs::disable('upgma_tiplab_linesize')
  shinyjs::disable('upgma_tiplab_linetype')
  
  # Shut off Tiles (geom_fruit()) when inward layout
  observe({
    if(input$nj_layout == "inward") {
      shinyjs::disable('nj_tiles_show') 
      shinyjs::disable('nj_fruit_variable')
      shinyjs::disable('nj_fruit_width')
      shinyjs::disable('nj_fruit_offset')
    } else {
      shinyjs::enable('nj_tiles_show')
      shinyjs::enable('nj_fruit_variable')
      shinyjs::enable('nj_fruit_width')
      shinyjs::enable('nj_fruit_offset')
    }
  })
  
  observe({
    if(input$upgma_layout == "inward") {
      shinyjs::disable('upgma_tiles_show') 
      shinyjs::disable('upgma_tiles_show_2')
      shinyjs::disable('upgma_tiles_show_3') 
      shinyjs::disable('upgma_tiles_show_4')
      shinyjs::disable('upgma_tiles_show_5') 
      shinyjs::disable('upgma_fruit_variable')
      shinyjs::disable('upgma_fruit_width')
      shinyjs::disable('upgma_fruit_offset')
    } else {
      shinyjs::enable('upgma_tiles_show') 
      shinyjs::enable('upgma_tiles_show_2')
      shinyjs::enable('upgma_tiles_show_3') 
      shinyjs::enable('upgma_tiles_show_4')
      shinyjs::enable('upgma_tiles_show_5')
      shinyjs::enable('upgma_fruit_variable')
      shinyjs::enable('upgma_fruit_width')
      shinyjs::enable('upgma_fruit_offset')
    }
  })
  
  #### Generate Plot ----
  
  hamming_nj <- reactive({
    if(anyNA(DF1$allelic_profile)) {
      if(input$na_handling == "omit") {
        allelic_profile_noNA <- DF1$allelic_profile[, colSums(is.na(DF1$allelic_profile)) == 0]
        
        allelic_profile_noNA_true <- allelic_profile_noNA[which(DF1$data$Include == TRUE),]
        
        proxy::dist(allelic_profile_noNA_true, method = hamming_distance)
        
      } else if(input$na_handling == "ignore_na"){
        proxy::dist(DF1$allelic_profile_true, method = hamming_distance_ignore)
        
      } else {
        proxy::dist(DF1$allelic_profile_true, method = hamming_distance_category)
      } 
      
    } else {proxy::dist(DF1$allelic_profile_true, method = hamming_distance)}
  })
  
  hamming_mst <- reactive({
    if(anyNA(DF1$allelic_profile)) {
      if(input$na_handling == "omit") {
        allelic_profile_noNA <- DF1$allelic_profile[, colSums(is.na(DF1$allelic_profile)) == 0]
        
        allelic_profile_noNA_true <- allelic_profile_noNA[which(DF1$data$Include == TRUE),]
        
        dist <- proxy::dist(allelic_profile_noNA_true, method = hamming_distance)
        
      } else if (input$na_handling == "ignore_na") {
        dist <- proxy::dist(DF1$allelic_profile_true, method = hamming_distance_ignore)
      } else {
        dist <- proxy::dist(DF1$allelic_profile_true, method = hamming_distance_category)
      }
    } else {
      dist <- proxy::dist(DF1$allelic_profile_true, method = hamming_distance)
    }
    
    # Find indices of pairs with a distance of 0
    zero_distance_pairs <- as.data.frame(which(as.matrix(dist) == 0, arr.ind = TRUE))
    
    zero_distance_pairs <- zero_distance_pairs[zero_distance_pairs$row != zero_distance_pairs$col, ]
    
    if(nrow(zero_distance_pairs) > 0) {
      
      # Sort each row so that x <= y
      df_sorted <- t(apply(zero_distance_pairs, 1, function(row) sort(row)))
      
      # Remove duplicate rows
      df_unique <- as.data.frame(unique(df_sorted))
      
      colnames(df_unique) <- c("col", "row")
      
      # get metadata in df
      vector_col <- character(0)
      count <- 1
      for (i in df_unique$col) {
        vector_col[count] <- DF1$meta_true$`Assembly Name`[i]
        count <- count + 1
      }
      
      vector_row <- character(0)
      count <- 1
      for (i in df_unique$row) {
        vector_row[count] <- DF1$meta_true$`Assembly Name`[i]
        count <- count + 1
      }
      
      col_id <- character(0)
      count <- 1
      for (i in df_unique$col) {
        col_id[count] <- DF1$meta_true$`Assembly ID`[i]
        count <- count + 1
      }
      
      row_id <- character(0)
      count <- 1
      for (i in df_unique$row) {
        row_id[count] <- DF1$meta_true$`Assembly ID`[i]
        count <- count + 1
      }
      
      col_index <- character(0)
      count <- 1
      for (i in df_unique$col) {
        col_index[count] <- DF1$meta_true$Index[i]
        count <- count + 1
      }
      
      row_index <- character(0)
      count <- 1
      for (i in df_unique$row) {
        row_index[count] <- DF1$meta_true$Index[i]
        count <- count + 1
      }
      
      col_date <- character(0)
      count <- 1
      for (i in df_unique$col) {
        col_date[count] <- DF1$meta_true$`Isolation Date`[i]
        count <- count + 1
      }
      
      row_date <- character(0)
      count <- 1
      for (i in df_unique$row) {
        row_date[count] <- DF1$meta_true$`Isolation Date`[i]
        count <- count + 1
      }
      
      col_host <- character(0)
      count <- 1
      for (i in df_unique$col) {
        col_host[count] <- DF1$meta_true$Host[i]
        count <- count + 1
      }
      
      row_host <- character(0)
      count <- 1
      for (i in df_unique$row) {
        row_host[count] <- DF1$meta_true$Host[i]
        count <- count + 1
      }
      
      col_country <- character(0)
      count <- 1
      for (i in df_unique$col) {
        col_country[count] <- DF1$meta_true$Country[i]
        count <- count + 1
      }
      
      row_country <- character(0)
      count <- 1
      for (i in df_unique$row) {
        row_country[count] <- DF1$meta_true$Country[i]
        count <- count + 1
      }
      
      col_city <- character(0)
      count <- 1
      for (i in df_unique$col) {
        col_city[count] <- DF1$meta_true$City[i]
        count <- count + 1
      }
      
      row_city <- character(0)
      count <- 1
      for (i in df_unique$row) {
        row_city[count] <- DF1$meta_true$City[i]
        count <- count + 1
      }
      
      df_unique <- cbind(df_unique, col_name = vector_col, row_name = vector_row, 
                         col_index = col_index, row_index = row_index, col_id = col_id,
                         row_id = row_id, col_date = col_date, row_date = row_date,
                         col_host = col_host, row_host = row_host, col_country = col_country,
                         row_country = row_country, col_city = col_city, row_city = row_city)
      
      # Add groups
      grouped_df <- df_unique %>%
        group_by(col) %>%
        mutate(group_id = cur_group_id())
      
      # Merge groups
      name <- character(0)
      index <- character(0)
      id <- character(0)
      count <- 1
      for (i in grouped_df$group_id) {
        name[count] <- paste(unique(append(grouped_df$col_name[which(grouped_df$group_id == i)], 
                                           grouped_df$row_name[which(grouped_df$group_id == i)])), 
                             collapse = "\n")
        
        id[count] <- paste(unique(append(grouped_df$col_id[which(grouped_df$group_id == i)], 
                                         grouped_df$row_id[which(grouped_df$group_id == i)])), 
                           collapse = "\n")
        
        index[count] <- paste(unique(append(grouped_df$col_index[which(grouped_df$group_id == i)], 
                                            grouped_df$row_index[which(grouped_df$group_id == i)])), 
                              collapse = "\n")
        
        count <- count + 1
      }
      
      merged_names <- cbind(grouped_df, "Index" = index, "Assembly Name" = name, "Assembly ID" = id)
      
      # remove duplicate groups
      
      final <- merged_names[!duplicated(merged_names$group_id), ]
      
      final_cleaned <- final[!(final$col_name %in% final$row_name),]
      
      final_cleaned <- select(final_cleaned, 3, 17:20)
      
      # adapt metadata
      Date_merged <- character(0)
      for(j in 1:length(final_cleaned$Index)) {
        Date <- character(0)
        for(i in strsplit(final_cleaned$Index, "\n")[[j]]) {
          Date <- append(Date, DF1$meta_true$`Isolation Date`[which(DF1$meta_true$Index == i)])
        }
        Date_merged <- append(Date_merged, paste(Date, collapse = "\n"))
      }
      
      Host_merged <- character(0)
      for(j in 1:length(final_cleaned$Index)) {
        Host <- character(0)
        for(i in strsplit(final_cleaned$Index, "\n")[[j]]) {
          Host <- append(Host, DF1$meta_true$Host[which(DF1$meta_true$Index == i)])
        }
        Host_merged <- append(Host_merged, paste(Host, collapse = "\n"))
      }
      
      Country_merged <- character(0)
      for(j in 1:length(final_cleaned$Index)) {
        Country <- character(0)
        for(i in strsplit(final_cleaned$Index, "\n")[[j]]) {
          Country <- append(Country, DF1$meta_true$Country[which(DF1$meta_true$Index == i)])
        }
        Country_merged <- append(Country_merged, paste(Country, collapse = "\n"))
      }
      
      City_merged <- character(0)
      for(j in 1:length(final_cleaned$Index)) {
        City <- character(0)
        for(i in strsplit(final_cleaned$Index, "\n")[[j]]) {
          City <- append(City, DF1$meta_true$City[which(DF1$meta_true$Index == i)])
        }
        City_merged <- append(City_merged, paste(City, collapse = "\n"))
      }
      
      final_meta <- cbind(final_cleaned, "Isolation Date" = Date_merged, 
                          "Host" = Host_merged, "Country" = Country_merged, "City" = City_merged)
      
      
      # Merging with original data frame / allelic profile
      
      allelic_profile_true <- DF1$allelic_profile_true
      meta_true <- DF1$meta_true
      
      rownames(allelic_profile_true) <- DF1$meta_true$`Assembly Name`
      rownames(meta_true) <- DF1$meta_true$`Assembly Name`
      
      omit <- unique(append(df_unique$col_name, df_unique$row_name)) %in% final_cleaned$col_name
      
      omit_id <- unique(append(df_unique$col_name, df_unique$row_name))[!omit]
      
      remove <- !(rownames(allelic_profile_true) %in% omit_id)
      
      allelic_profile_clean <- allelic_profile_true[remove, ]
      
      meta_clean <- meta_true[remove, ]
      
      # substitute meta assembly names with group names
      
      count <- 1
      for(i in which(rownames(meta_clean) %in% final_meta$col_name)) {
        meta_clean$Index[i] <- final_meta$Index[count]
        meta_clean$`Assembly Name`[i] <- final_meta$`Assembly Name`[count]
        meta_clean$`Assembly ID`[i] <- final_meta$`Assembly ID`[count]
        meta_clean$`Isolation Date`[i] <- final_meta$`Isolation Date`[count]
        meta_clean$Host[i] <- final_meta$Host[count]
        meta_clean$Country[i] <- final_meta$Country[count]
        meta_clean$City[i] <- final_meta$City[count]
        count <- count + 1
      }
      
      # Metadata completion
      # get group size
      
      size_vector <- numeric(0)
      for(i in 1:nrow(meta_clean)) {
        if (str_count(meta_clean$`Assembly Name`[i], "\n") == 0) {
          size_vector[i] <- 1
        } else {
          size_vector[i] <- str_count(meta_clean$`Assembly Name`[i], "\n") +1
        }
      }
      
      meta_clean <- mutate(meta_clean, size = size_vector)
      
      # get font size dependent on group size
      
      font_size <- numeric(nrow(meta_clean))
      
      for (i in 1:length(font_size)) {
        if(meta_clean$size[i] < 3) {
          font_size[i] <- 12
        } else {
          font_size[i] <- 11
        }
      }
      
      # get v-align dependent on group size
      valign <- numeric(nrow(meta_clean))
      
      for (i in 1:length(valign)) {
        if(meta_clean$size[i] == 1) {
          valign[i] <- -30
        } else if(meta_clean$size[i] == 2) {
          valign[i] <- -38
        } else if(meta_clean$size[i] == 3) {
          valign[i] <- -46
        } else if(meta_clean$size[i] == 4) {
          valign[i] <- -54
        } else if(meta_clean$size[i] == 5) {
          valign[i] <- -62
        } else if(meta_clean$size[i] > 5) {
          valign[i] <- -70
        }
      }
      
      plot_loc$unique_meta <- meta_clean %>%
        cbind(font_size = font_size, valign = valign)
      
      # final dist calculation
      
      if(anyNA(DF1$allelic_profile)){
        if(input$na_handling == "omit") {
          allelic_profile_clean_noNA_names <- allelic_profile_clean[, colSums(is.na(allelic_profile_clean)) == 0]
          proxy::dist(allelic_profile_clean_noNA_names, method = hamming_distance)
        } else if (input$na_handling == "ignore_na") {
          proxy::dist(allelic_profile_clean, method = hamming_distance_ignore)
        } else {
          proxy::dist(allelic_profile_clean, method = hamming_distance_category)
        }
      } else {proxy::dist(allelic_profile_clean, method = hamming_distance)}
      
      
    } else {
      font_size <- rep(12, nrow(DF1$meta_true))
      valign <- rep(-30, nrow(DF1$meta_true))
      size <- rep(1, nrow(DF1$meta_true))
      plot_loc$unique_meta <- DF1$meta_true %>%
        cbind(size , font_size, valign)
      
      dist
    }
    
  })
  
  observeEvent(input$create_tree, {
    if(is.null(DF1$data)) {
      show_toast(
        title = "Missing data",
        type = "error",
        position = "top-end",
        timer = 6000,
        width = "500px"
      )
    } else if(nrow(DF1$allelic_profile_true) < 3) {
      show_toast(
        title = "Min. of 3 entries required for visualization",
        type = "error",
        position = "top-end",
        timer = 6000,
        width = "500px"
      )
    } else {
      
      if(any(duplicated(DF1$meta$`Assembly Name`))) {
        showModal(
          modalDialog(
            if(sum(duplicated(DF1$meta$`Assembly Name`)) == 1) {
              HTML(paste0("Entry #", which(duplicated(DF1$meta$`Assembly Name`)), 
                          " contains a duplicated assembly name:", "<br><br>",
                          DF1$meta$`Assembly Name`[which(duplicated(DF1$meta$`Assembly Name`))]))
            } else {
              HTML(append("Entries contain duplicated assembly names: <br><br>", 
                          paste0(unique(DF1$meta$`Assembly Name`[which(duplicated(DF1$meta$`Assembly Name`))]), "<br>")))
            },
            title = "Duplicate entries",
            fade = TRUE,
            easyClose = TRUE,
            footer = tagList(
              modalButton("Cancel"),
              actionButton("change_entries", "Go to Entry Table", class = "btn btn-default")
            )
          )
        )
      } else {
        set.seed(1)
        
        if (input$tree_algo == "Neighbour-Joining") {
          
          plot_loc$meta_nj <- select(DF1$meta_true, -2)
          
          if(length(unique(gsub(" ", "_", colnames(plot_loc$meta_nj)))) < length(gsub(" ", "_", colnames(plot_loc$meta_nj)))) {
            show_toast(
              title = "Conflicting Custom Variable Names",
              type = "warning",
              position = "top-end",
              width = "500px",
              timer = 6000
            )
          } else {
            
            plot_loc$meta_nj <- mutate(plot_loc$meta_nj, taxa = Index) %>%
              relocate(taxa)
            
            # Create phylogenetic tree
            plot_loc$nj <- ape::nj(hamming_nj())
            
            output$tree_nj <- renderPlot({
              nj_tree()
            })
          }
        } else if (input$tree_algo == "UPGMA") {
          
          plot_loc$meta_upgma <- select(DF1$meta_true, -2)
          
          if(length(unique(gsub(" ", "_", colnames(plot_loc$meta_upgma)))) < length(gsub(" ", "_", colnames(plot_loc$meta_upgma)))) {
            show_toast(
              title = "Conflicting Custom Variable Names",
              type = "warning",
              position = "top-end",
              width = "500px",
              timer = 6000
            )
          } else {
            
            plot_loc$meta_upgma <- mutate(plot_loc$meta_upgma, taxa = Index) %>%
              relocate(taxa)
            
            # Create phylogenetic tree
            plot_loc$upgma <- phangorn::upgma(hamming_nj())
            
            output$tree_upgma <- renderPlot({
              upgma_tree()
            })
          }
        } else {
          
          if(nrow(DF1$meta_true) > 100) {
            show_toast(
              title = "Computation might take a while",
              type = "warning",
              position = "top-end",
              width = "500px",
              timer = 10000
            )
          }
          
          # prepare igraph object
          plot_loc$ggraph_1 <- hamming_mst() |>
            as.matrix() |>
            graph.adjacency(weighted = TRUE) |>
            igraph::mst() 
          
          output$tree_mst <- renderVisNetwork({
            mst_tree()
          })
        }
      }
    }
  })
  
  
  ## Report ----
  
  observe({
    if(!is.null(DF1$data)) {
      if(!is.null(input$tree_algo)) {
        if(input$tree_algo == "Minimum-Spanning") {
          shinyjs::disable("rep_plot_report")
          updateCheckboxInput(session, "rep_plot_report", value = FALSE)
        } else {
          shinyjs::enable("rep_plot_report")
        }
      }
    }
  })
  
  ### Save Report ----
  
  #### Get Report elements ----
  reportVAR <- reactiveValues()
  
  observe({
    if(!is.null(DF1$data)){
      if(!is.null(input$tree_algo)) {
        reportVAR$report_df <- data.frame(Element = c("entry_table", "general_show",
                                                      "general_date", "operator",
                                                      "institute", "comment",
                                                      "analysis_show", "scheme",
                                                      "tree", "distance", "na_handling", "version",
                                                      "plot"), 
                                          Include = c(input$rep_entrytable, input$rep_general,
                                                      input$rep_date_general, input$rep_operator_general,
                                                      input$rep_institute_general, input$rep_comm_general,
                                                      input$rep_analysis, input$rep_cgmlst_analysis,
                                                      input$rep_tree_analysis, input$rep_distance,
                                                      input$rep_missval, input$rep_version,
                                                      input$rep_plot_report))
      }
    }
  })
  
  #### Get Report values ----
  
  observeEvent(input$create_tree, {
    if(input$tree_algo == "Minimum-Spanning") {
      reportVAR$report_list_mst <- list(entry_table = DF1$meta_true,
                                        scheme = DF1$schemeinfo, 
                                        tree = input$tree_algo,
                                        na_handling = if(anyNA(DF1$allelic_profile_true)){input$na_handling} else {NULL},
                                        distance = "Hamming Distances",
                                        version = c(phylotraceVersion, "KMA-1.3.23"),
                                        plot = "MST")
    } else if(input$tree_algo == "Neighbour-Joining") {
      reportVAR$report_list_nj <- list(entry_table = DF1$meta_true,
                                       scheme = DF1$schemeinfo, 
                                       tree = input$tree_algo,
                                       na_handling = input$na_handling,
                                       distance = "Hamming Distances",
                                       version = c(phylotraceVersion, "KMA-1.3.23"),
                                       plot = "NJ")
    } else {
      reportVAR$report_list_upgma <- list(entry_table = DF1$meta_true,
                                          scheme = DF1$schemeinfo, 
                                          tree = input$tree_algo,
                                          na_handling = input$na_handling,
                                          distance = "Hamming Distances",
                                          version = c(phylotraceVersion, "KMA-1.3.23"),
                                          plot = "UPGMA")
    }
  })
  
  # Save plot for Report
  plot.report <- reactive({
    if(input$tree_algo == "Neighbour-Joining") {
      jpeg(paste0(getwd(), "/Report/NJ.jpeg"), width = (as.numeric(input$nj_scale) * as.numeric(input$nj_ratio)), height = as.numeric(input$nj_scale), quality = 100)
      print(nj_tree())
      dev.off()
    } else {
      jpeg(paste0(getwd(), "/Report/UPGMA.jpeg"), width = (as.numeric(input$upgma_scale) * as.numeric(input$upgma_ratio)), height = as.numeric(input$upgma_scale), quality = 100)
      print(upgma_tree())
      dev.off()
    }
  })
  
  #### Event Save Report ----
  output$download_report <- downloadHandler(
    filename = function() {
      if(input$tree_algo == "Minimum-Spanning") {
        paste0("MST_Report_", Sys.Date(), ".html")
      } else if(input$tree_algo == "Neighbour-Joining") {
        paste0("NJ_Report_", Sys.Date(), ".html")
      } else {paste0("UPGMA_Report_", Sys.Date(), ".html")}
    },
    content = function(file) {
      if(input$tree_algo == "Minimum-Spanning") {
        report <- c(reportVAR$report_list_mst, 
                    "general_date" = as.character(input$mst_date_general_select),
                    "operator" = input$mst_operator_general_select,
                    "institute" = input$mst_institute_general_select,
                    "comment" = input$mst_comm_general_select,
                    "report_df" = reportVAR$report_df)
        
        # Save data to an RDS file if any elements were selected
        if (!class(report) == "NULL") {
          saveRDS(report, file = paste0(getwd(), "/Report/selected_elements.rds"))
        }
        
        rmarkdown::render(paste0(getwd(), "/Report/Report.Rmd"))
        
        file.copy(paste0(getwd(), "/Report/Report.html"), file)
        
      } else if(input$tree_algo == "Neighbour-Joining") {
        plot.report()
        report <- c(reportVAR$report_list_nj, 
                    "general_date" = as.character(input$mst_date_general_select),
                    "operator" = input$mst_operator_general_select,
                    "institute" = input$mst_institute_general_select,
                    "comment" = input$mst_comm_general_select,
                    "report_df" = reportVAR$report_df)
        
        # Save data to an RDS file if any elements were selected
        if (!class(report) == "NULL") {
          saveRDS(report, file = paste0(getwd(), "/Report/selected_elements.rds"))
        }
        
        rmarkdown::render(paste0(getwd(), "/Report/Report.Rmd"))
        
        file.copy(paste0(getwd(), "/Report/Report.html"), file)
        
      } else {
        plot.report()
        report <- c(reportVAR$report_list_upgma, 
                    "general_date" = as.character(input$mst_date_general_select),
                    "operator" = input$mst_operator_general_select,
                    "institute" = input$mst_institute_general_select,
                    "comment" = input$mst_comm_general_select,
                    "report_df" = reportVAR$report_df)
        
        # Save data to an RDS file if any elements were selected
        if (!class(report) == "NULL") {
          saveRDS(report, file = paste0(getwd(), "/Report/selected_elements.rds"))
        }
        
        rmarkdown::render(paste0(getwd(), "/Report/Report.Rmd"))
        
        file.copy(paste0(getwd(), "/Report/Report.html"), file)
        
      }
    }
  )
  
  ## Typing  ----
  
  # Render Single/Multi Switch
  
  readLogFile <- reactive({
    invalidateLater(5000, session)
    readLines(paste0(getwd(), "/execute/script_log.txt"))
  })
  
  # Render sidebar dependent on data presence
  # No sidebar
  output$typing_sidebar <- renderUI({
    if(!is.null(DF1$exist)) {
      if(DF1$exist) {
        NULL
      } else {
        column(
          width = 12,
          align = "center",
          br(), br(),
          p(
            HTML(
              paste(
                tags$span(style='color: white; font-size: 18px; margin-bottom: 0px', 'Typing Mode')
              )
            )
          ),
          radioGroupButtons(
            inputId = "typing_mode",
            choices = c("Single", "Multi"),
            selected = "Single",
            checkIcon = list(
              yes = icon("square-check"),
              no = icon("square")
            )
          ),
          br()
        )
      }
    }
    
  })
  
  # No db typing message
  output$typing_no_db <- renderUI({
    if(DF1$exist) {
      column(
        width = 4,
        align = "left",
        br(),
        br(),
        br(),
        br(),
        p(
          HTML(
            paste0(
              tags$span(style='color: white; font-size: 15px; margin-bottom: 0px; margin-left: 50px', 'To initiate allelic typing, a cgMLST scheme must be downloaded first.'
              )
            )
          )
        )
      )
    } else {NULL}
  })
  
  ### Single Typing ----
  
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
            tags$span(style='color: white; font-size: 15px; margin-bottom: 0px', 'Select Assembly File (FASTA)')
          )
        )
      ),
      shinyFilesButton(
        "genome_file",
        "Browse" ,
        icon = icon("file"),
        title = "Select the assembly in .fasta/.fna/.fa format:",
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
      
      if (str_detect(str_sub(typing_reactive$single_path$name, start = -6), ".fasta") | 
          str_detect(str_sub(typing_reactive$single_path$name, start = -6), ".fna") | 
          str_detect(str_sub(typing_reactive$single_path$name, start = -6), ".fa")) {
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
      } else {
        show_toast(
          title = "Wrong file type (only fasta/fna/fa)",
          type = "error",
          position = "top-end",
          width = "500px",
          timer = 6000
        )
      }
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
    
    if(tail(readLogFile(), 1)!= "0") {
      show_toast(
        title = "Pending Multi Typing",
        type = "warning",
        position = "top-end",
        timer = 6000,
        width = "500px"
      )
    } else {
      if(!is.null(DF1$data)) {
        if(sum(apply(DF1$data, 1, anyNA)) >= 1) {
          DF1$no_na_switch <- TRUE
        } else {
          DF1$no_na_switch <- FALSE
        }
      }
      
      typing_reactive$single_end <- FALSE
      
      typing_reactive$progress_format_start <- 0
      typing_reactive$progress_format_end <- 0
      
      # Remove UI 
      output$initiate_typing_ui <- NULL
      output$metadata_single_box <- NULL
      output$start_typing_ui <- NULL
      
      # Locate folder containing cgMLST scheme
      search_string <-
        paste0(gsub(" ", "_", DF1$scheme), "_alleles")
      
      scheme_folders <-
        dir_ls(paste0(DF1$database, "/", gsub(" ", "_", DF1$scheme)))
      
      if (any(grepl(search_string, scheme_folders))) {
        # KMA initiate index
        
        scheme_select <-
          as.character(scheme_folders[which(grepl(search_string, scheme_folders))])
        
        show_toast(
          title = "Typing Initiated",
          type = "success",
          position = "top-end",
          timer = 12000,
          width = "500px"
        )
        
        ### Run KMA Typing
        
        single_typing_df <- data.frame(
          db_path = DF1$database,
          wd = getwd(),
          scheme = paste0(gsub(" ", "_", DF1$scheme)),
          genome = typing_reactive$single_path$datapath,
          alleles = paste0(DF1$database, "/", gsub(" ", "_", DF1$scheme), "/", search_string)
        )
        
        saveRDS(single_typing_df, "execute/single_typing_df.rds")
        
        # Execute singlye typing script
        system(paste("chmod +x", paste0(getwd(), "/execute/kma_run.sh")))
        system(paste0(getwd(), "/execute/kma_run.sh"), wait = FALSE)
        
        scheme_loci <-
          list.files(path = scheme_select, full.names = TRUE)
        
        # Filter the files that have the ".fasta" extension
        typing_reactive$scheme_loci_f <-
          scheme_loci[grep("\\.(fasta|fa|fna)$", scheme_loci, ignore.case = TRUE)]
        
        output$single_typing_progress <- renderUI({
          fluidRow(
            br(), br(), 
            column(width = 1),
            column(
              width = 3,
              h3(p("Pending Single Typing ..."), style = "color:white")
            ),
            br(), br(), br(), 
            fluidRow(
              column(width = 1),
              column(
                width = 4,
                br(), br(), br(),
                uiOutput("reset_single_typing"),
                HTML(
                  paste(
                    "<span style='color: white; font-weight: bolder'>",
                    as.character(typing_reactive$single_path$name)
                  )
                ),
                br(), br(),
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
    }
    
    
  })
  
  # Function to update Progress Bar
  update <- reactive({
    invalidateLater(3000, session)
    progress <- readLines(paste0(getwd(), "/execute", "/progress.fifo"))[1]
    if(!is.na(readLines(paste0(getwd(), "/execute", "/progress.fifo"))[2])) {
      typing_reactive$progress_format_start <- as.numeric(readLines(paste0(getwd(), "/execute", "/progress.fifo"))[2])
      typing_reactive$pending_format <- as.numeric(readLines(paste0(getwd(), "/execute", "/progress.fifo"))[2])
    }
    if(!is.na(readLines(paste0(getwd(), "/execute", "/progress.fifo"))[3])) {
      typing_reactive$progress_format_end <- as.numeric(readLines(paste0(getwd(), "/execute", "/progress.fifo"))[3])
      typing_reactive$entry_added <- as.numeric(readLines(paste0(getwd(), "/execute", "/progress.fifo"))[3])
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
          width = 3,
          align = "center",
          br(),
          fluidRow(
            column(
              width = 6,
              HTML(paste("<span style='color: white;'>", "Transforming data ..."))
            ),
            column(
              width = 3,
              HTML(paste('<i class="fa fa-spinner fa-spin" style="font-size:20px;color:white"></i>'))
            )
          )
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
          width = 4,
          align = "center",
          br(), br(),
          if(str_detect(tail(readLines(paste0(getwd(),"/execute/single_typing_log.txt")), 1), "Successful")) {
            HTML(paste("<span style='color: white;'>", 
                       sub(".*Successful", "Successful", tail(readLines(paste0(getwd(),"/execute/single_typing_log.txt")), 1)),
                       "Reset to start another typing process.", sep = '<br/>'))
          } else {
            HTML(paste("<span style='color: white;'>", 
                       sub(".*typing", "Typing", tail(readLines(paste0(getwd(),"/execute/single_typing_log.txt")), 1)),
                       "Reset to start another typing process.", sep = '<br/>'))
          },
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
    
    meta_info <- data.frame(assembly_id = trimws(input$assembly_id),
                            assembly_name = trimws(input$assembly_name),
                            cgmlst_typing = DF1$scheme,
                            append_isodate = input$append_isodate,
                            append_host = trimws(input$append_host),
                            append_country = trimws(input$append_country),
                            append_city = trimws(input$append_city),
                            append_analysisdate = input$append_analysisdate,
                            db_directory = getwd()) 
    
    saveRDS(meta_info, paste0(
      getwd(),
      "/execute/meta_info_single.rds"
    ))
    
    show_toast(
      title = "Metadata declared",
      type = "success",
      position = "top-end",
      timer = 3000,
      width = "500px"
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
    system(paste("chmod +x", paste0(getwd(), "/execute/reset_kma.sh")))
    system(paste0(getwd(), "/execute/reset_kma.sh"), wait = TRUE)
    
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
              tags$span(style='color: white; font-size: 15px; margin-bottom: 0px', 'Select Assembly File (FASTA)')
            )
          )
        ),
        shinyFilesButton(
          "genome_file",
          "Browse",
          icon = icon("folder-open"),
          title = "Please select the genome in .fasta/.fna/.fa format:",
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
  
  # Notification for finalized Single typing
  typing_reactive$single_end <- TRUE
  typing_reactive$progress_format_end <- 0
  
  observe({
    if(typing_reactive$single_end == FALSE) {
      if (typing_reactive$progress_format_end == 999999) {
        show_toast(
          title = "Single Typing finalized",
          type = "success",
          position = "top-end",
          timer = 8000,
          width = "500px"
        )
        typing_reactive$single_end <- TRUE
      }
    }
  })
  
  ### Multi Typing ----
  
  #### Render Multi Typing UI Elements ----
  output$initiate_multi_typing_ui <- renderUI({
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
            tags$span(style='color: white; font-size: 15px; margin-bottom: 0px', 'Select Assembly Folder')
          )
        )
      ),
      column(
        width = 12,
        align = "center",
        shinyDirButton(
          "genome_file_multi",
          "Browse",
          icon = icon("folder-open"),
          title = "Select the folder containing the genome assemblies (FASTA)",
          buttonType = "default"
        ),
        br(),
        br()
      ),
      column(
        width = 12,
        align = "left",
        rHandsontableOutput("multi_select_table")
      )
    )
  })
  
  # Render Metadata Select Box after Folder selection
  observe({
    if (nrow(typing_reactive$table) > 0) {
      
      typing_reactive$genome_selected <- hot_to_r(input$multi_select_table)
      
      output$metadata_multi_box <- renderUI({
        column(
          width = 3,
          align = "center",
          br(),
          br(),
          h3(p("Declare Metadata"), style = "color:white"),
          br(), br(),
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
        )
      }) 
    } else {
      output$metadata_multi_box <- NULL
    }
  })
  
  
  
  
  # Check if ongoing Multi Typing - Render accordingly
  
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
    
    if (between(nrow(typing_reactive$table), 1, 15)) {
      output$multi_select_table <- renderRHandsontable({
        rhandsontable(typing_reactive$table, rowHeaders = NULL, stretchH = "all") %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          hot_cols(columnSorting = TRUE) %>%
          hot_rows(rowHeights = 25) %>%
          hot_col(2,
                  readOnly = TRUE,
                  valign = "htBottom") %>%
          hot_col(1,
                  halign = "htCenter",
                  valign = "htTop",
                  width = "auto")
      })
    } else if(nrow(typing_reactive$table) > 15) {
      output$multi_select_table <- renderRHandsontable({
        rhandsontable(typing_reactive$table, rowHeaders = NULL, stretchH = "all", height = 500) %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          hot_cols(columnSorting = TRUE) %>%
          hot_rows(rowHeights = 25) %>%
          hot_col(2,
                  readOnly = TRUE,
                  valign = "htBottom") %>%
          hot_col(1,
                  halign = "htCenter",
                  valign = "htTop",
                  width = "auto")
      })
    } else {
      output$multi_select_table <- NULL
    }
    
  })
  
  
  observeEvent(input$conf_meta_multi, {
    
    meta_info <- data.frame(cgmlst_typing = DF1$scheme,
                            append_isodate = trimws(input$append_isodate_multi),
                            append_host = trimws(input$append_host_multi),
                            append_country = trimws(input$append_country_multi),
                            append_city = trimws(input$append_city_multi),
                            append_analysisdate = input$append_analysisdate_multi,
                            db_directory = getwd())
    
    saveRDS(meta_info, paste0(
      getwd(),
      "/execute/meta_info.rds"
    ))
    
    show_toast(
      title = "Metadata declared",
      type = "success",
      position = "top-end",
      timer = 3000,
      width = "500px"
    )
    
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
  
  # Print Log
  output$print_log <- downloadHandler(
    filename = function() {
      paste("Multi_Typing_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      writeLines(readLines(paste0(getwd(), "/execute/script_log.txt")), file)
    }
  )
  
  # Reset Multi Typing
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
    } else {
      
      # Null logfile
      system(paste("chmod +x", paste0(getwd(), "/execute/reset_multi.sh")))
      system(paste0(getwd(), "/execute/reset_multi.sh"), wait = TRUE)
      
      # Reset User Feedback variable
      typing_reactive$pending_format <- 0
      
      output$initiate_multi_typing_ui <- renderUI({
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
                tags$span(style='color: white; font-size: 15px; margin-bottom: 0px', 'Select Assembly Folder')
              )
            )
          ),
          column(
            width = 12,
            align = "center",
            shinyDirButton(
              "genome_file_multi",
              "Browse",
              icon = icon("folder-open"),
              title = "Select the folder containing the genome assemblies (FASTA)",
              buttonType = "default"
            ),
            br(),
            br()
          ),
          column(
            width = 12,
            align = "left",
            rHandsontableOutput("multi_select_table")
          )
        )
      })
      
      typing_reactive$table <- data.frame()
      
      output$test_yes_pending <- NULL
    }
  })
  
  # Confirm Reset after 
  observeEvent(input$conf_multi_kill, {
    removeModal()
    
    show_toast(
      title = "Execution cancelled",
      type = "warning",
      position = "top-end",
      timer = 6000,
      width = "500px"
    )
    
    # Kill multi typing and reset logfile  
    system(paste("chmod +x", paste0(getwd(), "/execute/kill_multi.sh")))
    system(paste0(getwd(), "/execute/kill_multi.sh"), wait = TRUE)
    
    # Reset User Feedback variable
    typing_reactive$pending_format <- 0
    output$test_yes_pending <- NULL
    typing_reactive$failures <- 0
    typing_reactive$successes <- 0
    
    output$initiate_multi_typing_ui <- renderUI({
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
              tags$span(style='color: white; font-size: 15px; margin-bottom: 0px', 'Select Assembly Folder')
            )
          )
        ),
        column(
          width = 12,
          align = "center",
          shinyDirButton(
            "genome_file_multi",
            "Browse",
            icon = icon("folder-open"),
            title = "Please select the folder containing the genome assemblies (FASTA)",
            buttonType = "default"
          ),
          br(),
          br()
        ),
        column(
          width = 12,
          align = "left",
          rHandsontableOutput("multi_select_table")
        )
      )
    })
    
  })
  
  observeEvent(input$start_typ_multi, {
    if(readLines(paste0(getwd(), "/execute", "/progress.fifo"))[1] != "0") {
      show_toast(
        title = "Pending Single Typing",
        type = "warning",
        position = "top-end",
        timer = 6000,
        width = "500px"
      )
    } else {
      if (any(!grepl("\\.fasta|\\.fna|\\.fa", str_sub(typing_reactive$genome_selected$Files[which(typing_reactive$genome_selected$Include == TRUE)], start = -6)))) {
        
        show_toast(
          title = "Wrong file type (include only fasta/fna/fa)",
          type = "error",
          position = "top-end",
          timer = 6000,
          width = "500px"
        )
      } else {
        showModal(
          modalDialog(
            paste0(
              "Typing multiple assemblies will take a while. Continue?"
            ),
            title = "Start multi typing",
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
      }
    }
    
  })
  
  observeEvent(input$conf_start_multi, {
    
    removeModal()
    
    show_toast(
      title = "Multi Typing started",
      type = "success",
      position = "top-end",
      timer = 6000,
      width = "500px"
    )
    
    typing_reactive$final <- FALSE
    
    # Remove Allelic Typing Controls
    output$initiate_multi_typing_ui <- NULL
    
    output$metadata_multi_box <- NULL
    
    output$start_multi_typing_ui <- NULL
    
    # Initiate Feedback variables
    typing_reactive$failures <- 0
    
    typing_reactive$successes <- 0
    
    # Start Multi Typing Script
    multi_typing_df <- data.frame(
      db_path = DF1$database,
      wd = getwd(),
      scheme = paste0(gsub(" ", "_", DF1$scheme)),
      genome_folder = as.character(parseDirPath(roots = c(wd = "/home"), input$genome_file_multi)),
      genome_names = paste(typing_reactive$genome_selected$Files[which(typing_reactive$genome_selected$Include == TRUE)], collapse= " "),
      alleles = paste0(DF1$database, "/", gsub(" ", "_", DF1$scheme), "/", gsub(" ", "_", DF1$scheme), "_alleles")
    )
    
    saveRDS(multi_typing_df, "execute/multi_typing_df.rds")
    
    # Execute multi kma script  
    system(paste("chmod +x", paste0(getwd(), "/execute/kma_multi.sh")))
    system(paste("nohup", paste0(getwd(), "/execute/kma_multi.sh"), "> script.log 2>&1"), wait = FALSE)
  })
  
  #### User Feedback ----
  
  # Database messages
  observe({
    if(tail(readLogFile(), 1) != "0") {
      if(!is.null(typing_reactive$reset)){
        if(typing_reactive$reset == TRUE) {
          if(str_detect(tail(readLogFile(), 1), "Attaching")) {
            typing_reactive$pending_format <- 888888
          } else if(str_detect(tail(readLogFile(), 2)[1], "Successful typing")) {
            if(!identical(typing_reactive$last_success, tail(readLogFile(), 2)[1])) {
              typing_reactive$entry_added <- 999999
              typing_reactive$last_success <- tail(readLogFile(), 2)[1]
              typing_reactive$reset <- FALSE  
            }
          } 
        }
      }
    }
  })
  
  checkFile <- reactive({
    invalidateLater(10000, session)  # Check every 10 seconds
    
    # Path to your file
    file_path <- paste0(getwd(), "/execute/script_log.txt")
    
    # Check if the file exists to avoid readLines() error
    if(file.exists(file_path)) {
      
      # Count failures
      if(sum(str_detect(readLines(file_path, warn = FALSE), "failed")) > typing_reactive$failures) {
        
        typing_reactive$failures <- sum(str_detect(readLines(file_path, warn = FALSE), "failed"))
        
        msg_string <- sub(".*typing", "Typing", readLines(file_path, warn = FALSE)[max(which(str_detect(readLines(file_path, warn = FALSE), "failed") == TRUE))])
        
        show_toast(
          title = msg_string,
          type = "error",
          width = "500px",
          position = "top-end",
          timer = 8000
        )
      }
      
      # Count successes
      if(sum(str_detect(readLines(file_path, warn = FALSE), "Successful")) > typing_reactive$successes) {
        
        typing_reactive$successes <- sum(str_detect(readLines(file_path, warn = FALSE), "Successful"))
        
        msg_string <- sub(".*Successful", "Successful", readLines(file_path, warn = FALSE)[max(which(str_detect(readLines(file_path, warn = FALSE), "Successful") == TRUE))])
        
        show_toast(
          title = msg_string,
          type = "success",
          width = "500px",
          position = "top-end",
          timer = 8000
        )
      }
    }
  })
  
  typing_reactive$final <- TRUE
  
  # Typing Notifications
  observe({
    
    if(tail(readLogFile(), 1)!= "0" & typing_reactive$final == FALSE) {
      checkFile()
    }
    
    if(typing_reactive$final == FALSE){
      if(any(str_detect(readLines(paste0(getwd(), "/execute/script_log.txt")), "finalized"))) {
        show_toast(
          title = "Multi Typing finalized",
          type = "success",
          position = "top-end",
          timer = 8000,
          width = "500px"
        )
        typing_reactive$final <- TRUE
      }
    }
  })
  
  observe({
    
    # Render log content
    output$logText <- renderPrint({
      cat(rev(paste0(tail(readLogFile(), 50), "\n")))
    })
    
    output$logTextFull <- renderPrint({
      cat(rev(paste0(readLines(paste0(getwd(), "/execute/script_log.txt")), "\n")))
    })
    
    # Render Pending UI
    if(!grepl("Multi Typing", tail(readLogFile(), n = 1)) & grepl("Start Multi Typing", head(readLogFile(), n = 1))) {
      
      output$initiate_multi_typing_ui <- NULL
      
      output$test_yes_pending <- renderUI({
        fluidRow(
          fluidRow(
            br(), br(),
            column(width = 1),
            column(
              width = 2,
              h3(p("Pending Multi Typing ..."), style = "color:white"),
              br(), br(),
              fluidRow(
                column(
                  width = 5,
                  HTML(paste('<i class="fa fa-spinner fa-spin" style="font-size:24px;color:white;margin-top:5px"></i>'))
                ),
                column(
                  width = 6,
                  align = "left",
                  actionButton(
                    "reset_multi",
                    "Terminate",
                    icon = icon("ban")
                  )
                )
              ),
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
      })
    } else if(grepl("Multi Typing finalized", tail(readLogFile(), n = 1))) {
      
      typing_reactive$last_scheme <- NULL
      
      output$initiate_multi_typing_ui <- NULL
      
      output$test_yes_pending <- renderUI({
        fluidRow(
          fluidRow(
            br(), br(),
            column(width = 1),
            column(
              width = 2,
              h3(p("Pending Multi Typing ..."), style = "color:white"),
              br(), br(),
              HTML(paste("<span style='color: white;'>", 
                         paste("Typing of", sum(str_detect(readLines(paste0(getwd(), "/execute/script_log.txt")), "Processing")), "assemblies finalized."),
                         paste(sum(str_detect(readLines(paste0(getwd(), "/execute/script_log.txt")), "Successful")), "successes."),
                         paste(sum(str_detect(readLines(paste0(getwd(), "/execute/script_log.txt")), "failed")), "failures."),
                         "Reset to start another typing process.", 
                         sep = '<br/>')),
              br(), br(),
              fluidRow(
                column(
                  width = 5,
                  actionButton(
                    "reset_multi",
                    "Reset",
                    icon = icon("arrows-rotate")
                  )
                ),
                column(
                  width = 5,
                  downloadButton(
                    "print_log",
                    "Logfile",
                    icon = icon("floppy-disk")
                  )
                )
              )
            )
          ),
          br(), br(),
          fluidRow(
           column(width = 1),
            column(
              width = 6,
              verbatimTextOutput("logTextFull"),
            )  
          )
        )
      })
      
    } else if (!grepl("Start Multi Typing", head(readLogFile(), n = 1))){
      output$test_yes_pending <- NULL
    }
  })
  
} # end server

# Shiny ----

shinyApp(ui = ui, server = server)
