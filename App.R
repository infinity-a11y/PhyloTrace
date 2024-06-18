######## PhyloTrace #########

# _______________________ ####
# CRAN Packages
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
library(cowplot)
library(viridis)
library(RColorBrewer)
# Bioconductor Packages
library(treeio)
library(ggtree)
library(ggtreeExtra)

schemes <- c("Acinetobacter_baumanii", "Bacillus_anthracis", "Bordetella_pertussis", 
             "Brucella_melitensis", "Brucella_spp", "Burkholderia_mallei_FLI", 
             "Burkholderia_mallei_RKI", "Burkholderia_pseudomallei", "Campylobacter_jejuni_coli", 
             "Clostridioides_difficile", "Clostridium_perfringens", "Corynebacterium_diphtheriae",
             "Cronobacter_sakazakii_malonaticus", "Enterococcus_faecalis", "Enterococcus_faecium", 
             "Escherichia_coli", "Francisella_tularensis", "Klebsiella_oxytoca_sensu_lato", "Klebsiella_pneumoniae_sensu_lato", 
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
  "Democratic Republic of the Congo",
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
  
  title = "PhyloTrace 1.3.1",
  
  # Title
  dashboardHeader(
    
    title = span(
    div(
      class = "img_logo",
      img(
        src = "PhyloTrace.jpg", width = 190
      )
    )
    ),
    tags$li(class = "dropdown", 
            tags$span(id = "currentTime", style = "color:white; font-weight:bold;")),
    disable = FALSE
  ),
  
  ## Sidebar ----
  dashboardSidebar(
    tags$head(includeCSS("www/head.css")),
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
        uiOutput("missing_values_sidebar")
      ),
      conditionalPanel(
        "input.tabs==='typing'",
        uiOutput("typing_sidebar")
      ),
      conditionalPanel(
        "input.tabs==='visualization'",
        uiOutput("visualization_sidebar")
      )
    )
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    
    shinyDashboardThemeDIY(
      ### general
      appFontFamily = "Liberation Sans",
      appFontColor = "#000000",
      primaryFontColor = "#ffffff",
      infoFontColor = "rgb(0,0,0)",
      successFontColor = "rgb(0,0,0)",
      warningFontColor = "rgb(0,0,0)",
      dangerFontColor = "rgb(0,0,0)",
      bodyBackColor = cssGradientThreeColors(
        direction = "down",
        colorStart = "#282f38",
        colorMiddle = "#384454",
        colorEnd = "#495d78",
        colorStartPos = 0,
        colorMiddlePos = 50,
        colorEndPos = 100
      ),
      
      ### header
      logoBackColor = "#282f38",
      headerButtonBackColor = "#282f38",
      headerButtonIconColor = "#18ece1",
      headerButtonBackColorHover = "#282f38",
      headerButtonIconColorHover = "#ffffff",
      headerBackColor = "#282f38",
      headerBoxShadowColor = "#aaaaaa",
      headerBoxShadowSize = "0px 0px 0px",
      
      ### sidebar
      sidebarBackColor = cssGradientThreeColors(
        direction = "down",
        colorStart = "#282f38",
        colorMiddle = "#384454",
        colorEnd = "#495d78",
        colorStartPos = 0,
        colorMiddlePos = 50,
        colorEndPos = 100),
      
      sidebarPadding = 0,
      sidebarMenuBackColor = "transparent",
      sidebarMenuPadding = 0,
      sidebarMenuBorderRadius = 0,
      sidebarShadowRadius = "5px 5px 5px",
      sidebarShadowColor = "#282f38",
      sidebarUserTextColor = "#ffffff",
      sidebarSearchBackColor = "rgb(55,72,80)",
      sidebarSearchIconColor = "rgb(153,153,153)",
      sidebarSearchBorderColor = "rgb(55,72,80)",
      sidebarTabTextColor = "rgb(255,255,255)",
      sidebarTabTextSize = 15,
      sidebarTabBorderStyle = "none none solid none",
      sidebarTabBorderColor = "rgb(35,106,135)",
      sidebarTabBorderWidth = 0,
      sidebarTabBackColorSelected = cssGradientThreeColors(
        direction = "right",
        colorStart = "rgba(44,222,235,1)",
        colorMiddle = "rgba(44,222,235,1)",
        colorEnd = "rgba(0,255,213,1)",
        colorStartPos = 0,
        colorMiddlePos = 30,
        colorEndPos = 100
      ),
      sidebarTabTextColorSelected = "rgb(0,0,0)",
      sidebarTabRadiusSelected = "0px 0px 0px 0px",
      sidebarTabBackColorHover = cssGradientThreeColors(
        direction = "right",
        colorStart = "rgba(44,222,235,1)",
        colorMiddle = "rgba(44,222,235,1)",
        colorEnd = "rgba(0,255,213,1)",
        colorStartPos = 0,
        colorMiddlePos = 30,
        colorEndPos = 100
      ),
      sidebarTabTextColorHover = "rgb(50,50,50)",
      sidebarTabBorderStyleHover = "none none solid none",
      sidebarTabBorderColorHover = "rgb(75,126,151)",
      sidebarTabBorderWidthHover = 0,
      sidebarTabRadiusHover = "0px 0px 0px 0px",
      
      ### boxes
      boxBackColor = "#ffffff",
      boxBorderRadius = 7,
      boxShadowSize = "0px 0px 0px",
      boxShadowColor = "#ffffff",
      boxTitleSize = 20,
      boxDefaultColor = "#00a65a",
      boxPrimaryColor = "#ffffff",
      boxInfoColor = "#00a65a",
      boxSuccessColor = "#00a65a",
      boxWarningColor = "#ffffff",
      boxDangerColor = "#ffffff",
      tabBoxTabColor = "#ffffff",
      tabBoxTabTextSize = 14,
      tabBoxTabTextColor = "rgb(0,0,0)",
      tabBoxTabTextColorSelected = "rgb(0,0,0)",
      tabBoxBackColor = "#ffffff",
      tabBoxHighlightColor = "#ffffff",
      tabBoxBorderRadius = 5,
      
      ### inputs
      buttonBackColor = "#282F38",
      buttonTextColor = "#ffffff",
      buttonBorderColor = "#282F38",
      buttonBorderRadius = 5,
      buttonBackColorHover = cssGradientThreeColors(
        direction = "right",
        colorStart = "rgba(44,222,235,1)",
        colorMiddle = "rgba(44,222,235,1)",
        colorEnd = "rgba(0,255,213,1)",
        colorStartPos = 0,
        colorMiddlePos = 30,
        colorEndPos = 100
      ),
      buttonTextColorHover = "#000000",
      buttonBorderColorHover = "transparent",
      textboxBackColor = "#ffffff",
      textboxBorderColor = "#ffffff",
      textboxBorderRadius = 5,
      textboxBackColorSelect = "#ffffff",
      textboxBorderColorSelect = "#000000",
      
      ### tables
      tableBackColor = "rgb(255,255,255)",
      tableBorderColor = "rgb(240,240,240)",
      tableBorderTopSize = 1,
      tableBorderRowSize = 1
    ),
    
    uiOutput("start_message"),
    
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
          column(1),
          column(
            width = 8,
            uiOutput("db_entries_table")
          ),
          column(
            width = 3,
            align = "left",
            uiOutput("delete_box"),
            uiOutput("compare_allele_box"),
            uiOutput("download_entries"),
            br(), br(), br(), br(), br(), br(), br(), br(),
            br(), br(), br(), br(), br(), br(), br()
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
            div(class = "loci_table",
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
          column(1),
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
                "Klebsiella oxytoca sensu lato",
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
            width = 6,
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
          column(1),
          column(
            width = 5,
            align = "center",
            br(),
            br(),
            br(),
            addSpinner(
              tableOutput("cgmlst_scheme"),
              spin = "dots",
              color = "#ffffff"
            )
          ),
          column(
            width = 6,
            align = "center",
            br(),
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
            uiOutput("multi_stop"),
            column(1),
            uiOutput("metadata_multi_box"),
            column(1),
            uiOutput("start_multi_typing_ui")
          ),
          fluidRow(
            column(
              width = 6,
              uiOutput("test_yes_pending")
            ),
            column(
              width = 6,
              uiOutput("multi_typing_results")
            )
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
                              width = 12,
                              align = "left",
                              div(
                                class = "mat-switch-mst-nodes",
                                materialSwitch(
                                  "mst_background_transparent",
                                  h5(p("Transparent"), style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"),
                                  value = FALSE,
                                  right = TRUE
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
                                class = "mat-switch-mst-nodes",
                                materialSwitch(
                                  "scale_nodes",
                                  h5(p("Scale by Duplicates"), style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"),
                                  value = TRUE,
                                  right = TRUE
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
                          sliderInput(
                            "mst_node_scale",
                            label = NULL,
                            min = 1,
                            max = 80,
                            value = c(20, 40),
                            ticks = FALSE
                          )
                        )
                      ),
                      conditionalPanel(
                        "input.scale_nodes==false",
                        div(
                          class = "slider",
                          sliderInput(
                            inputId = "mst_node_size",
                            label = NULL,
                            min = 1,
                            max = 100,
                            value = 30,
                            ticks = FALSE
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
                                class = "mat-switch-mst-nodes",
                                materialSwitch(
                                  "mst_shadow",
                                  h5(p("Show Shadow"), style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"),
                                  value = FALSE,
                                  right = TRUE
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
                                sliderInput(
                                  "mst_edge_opacity",
                                  label = h5("Opacity", style = "color:white; margin-bottom: 0px;"),
                                  value = 0.7,
                                  step = 0.1,
                                  min = 0,
                                  max = 1,
                                  ticks = FALSE,
                                  width = "150px"
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
                      div(
                        class = "switch-mst-edges",
                        materialSwitch(
                          "mst_scale_edges",
                          h5(p("Scale Edge Length"), style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"),
                          value = FALSE,
                          right = TRUE
                        )
                      ),
                      conditionalPanel(
                        "input.mst_scale_edges==true",
                        div(
                          class = "slider_edge",
                          sliderInput(
                            inputId = "mst_edge_length_scale",
                            label = NULL,
                            min = 1,
                            max = 40,
                            value = 15,
                            ticks = FALSE
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
              width = 1,
              radioGroupButtons(
                inputId = "nj_controls",
                label = "",
                choices = c("Layout", "Label", "Elements", "Variables"),
                direction = "vertical"
              )
            ),
            conditionalPanel(
              "input.nj_controls=='Layout'",
              column(
                width = 2,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
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
                          selected = "rectangular",
                          width = "90%"
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 8,
                        align = "left",
                        div(
                          class = "mat-switch-layout",
                          materialSwitch(
                            "nj_rootedge_show",
                            h5(p("Rootedge"), style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"),
                            value = FALSE,
                            right = TRUE
                          )
                        )
                      ),
                      column(
                        width = 4,
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
                              uiOutput("nj_rootedge_length"),
                              br(),
                              selectInput(
                                "nj_rootedge_line",
                                label = h5("Rootedge Line", style = "color:white"),
                                choices = c(Solid = "solid", Dashed = "dashed", Dotted = "dotted"),
                                selected = c(Dotted = "solid"),
                                width = "100px"
                              ),
                              br(),
                              conditionalPanel(
                                "input.nj_layout=='circular'",
                                sliderInput(
                                  "nj_xlim",
                                  label = h5("Adjust Circular", style = "color:white"),
                                  min = -50,
                                  max = 0,
                                  value = -10,
                                  width = "150px",
                                  ticks = FALSE
                                )
                              ),
                              conditionalPanel(
                                "input.nj_layout=='inward'",
                                sliderInput(
                                  "nj_inward_xlim",
                                  label = h5("Adjust Circular", style = "color:white"),
                                  min = 30,
                                  max = 120,
                                  value = 50,
                                  ticks = FALSE,
                                  width = "150px",
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
                        div(
                          class = "mat-switch-re",
                          materialSwitch(
                            "nj_ladder",
                            h5(p("Ladderize"), style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"),
                            value = TRUE,
                            right = TRUE
                          )
                        )
                      )
                    )
                  )
                )
              ),
              column(
                width = 2,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
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
                    br(),
                    fluidRow(
                      column(
                        width = 5,
                        h5(p("Background"), style = "color:white; position: relative; right: -15px; margin-top: 30px; margin-bottom: 38px")
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
                    ), 
                    br()
                  )
                )
              ),
              column(
                width = 2,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
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
                      textInput(
                        "nj_subtitle",
                        label = "",
                        width = "100%",
                        placeholder = "Plot Subtitle"
                      ),
                      br(),
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
                                  label = h5("Title Size", style = "color:white; margin-bottom: 0px"),
                                  value = 30,
                                  min = 15,
                                  max = 40,
                                  step = 1,
                                  width = "80px"
                                ),
                                br(),
                                numericInput(
                                  "nj_subtitle_size",
                                  label = h5("Subtitle Size", style = "color:white; margin-bottom: 0px"),
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
              ),
              column(
                width = 2,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
                  column(
                    width = 12,
                    align = "left",
                    h4(p("Sizing"), style = "color:white; position: relative; right: -15px"),
                    column(
                      width = 12,
                      align = "center",
                      br(),
                      fluidRow(
                        column(
                          width = 3,
                          h5("Ratio", style = "color: white; font-size: 14px;")
                        ),
                        column(
                          width = 6,
                          align = "left",
                          div(
                            class = "ratio-sel",
                            selectInput(
                              "nj_ratio",
                              "",
                              choices = c("16:10" = (16/10), "16:9" = (16/9), "4:3" = (4/3))
                            )
                          )
                        ),
                        column(
                          width = 3,
                          dropMenu(
                            actionBttn(
                              "nj_size_menu",
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
                                sliderInput(
                                  "nj_v",
                                  label = h5("Vertical Position", style = "color:white; margin-bottom: 0px"),
                                  min = -0.5,
                                  max = 0.5,
                                  step = 0.01,
                                  value = 0,
                                  width = "150px",
                                  ticks = FALSE
                                ),
                                br(),
                                sliderInput(
                                  "nj_h",
                                  label = h5("Horizontal Position", style = "color:white; margin-bottom: 0px"),
                                  min = -0.5,
                                  max = 0.5,
                                  step = 0.01,
                                  value = 0,
                                  width = "150px",
                                  ticks = FALSE
                                )
                              )
                            )
                          )
                        )
                      ),
                      fluidRow(
                        column(
                          width = 3,
                          h5("Size", style = "color: white; font-size: 14px; margin-top: 30px")
                        ),
                        column(
                          width = 9,
                          sliderInput(
                            "nj_scale",
                            "",
                            min = 500,
                            max = 1200,
                            value = 800,
                            width = "95%",
                            ticks = FALSE
                          )
                        )
                      ),
                      fluidRow(
                        column(
                          width = 3,
                          h5("Zoom", style = "color: white; font-size: 14px; margin-top: 30px")
                        ),
                        column(
                          width = 9,
                          div(
                            class = "zoom-slider",
                            sliderInput(
                              "nj_zoom",
                              label = NULL,
                              min = 0.5,
                              max = 1.5,
                              step = 0.05,
                              value = 0.95,
                              ticks = FALSE
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              column(
                width = 2,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
                  column(
                    width = 12,
                    align = "left",
                    h4(p("Tree Scale"), style = "color:white; position: relative; right: -15px"),
                    column(
                      width = 12,
                      fluidRow(
                        column(
                          width = 8,
                          align = "left",
                          div(
                            class = "mat-switch-layout",
                            materialSwitch(
                              "nj_treescale_show",
                              h5(p("Show"), style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"),
                              value = TRUE,
                              right = TRUE
                            )
                          ),
                          br()
                        ),
                        column(
                          width = 4,
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
                                uiOutput("nj_treescale_width"),
                                br(),
                                uiOutput("nj_treescale_x"),
                                br(),
                                uiOutput("nj_treescale_y")
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  column(
                    width = 12,
                    align = "left",
                    h4(p("Legend"), style = "color:white; position: relative; right: -15px; margin-top: 10px; margin-bottom: -2px"),
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
                            selected = c(Vertical = "vertical"),
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
                                br(),
                                sliderInput(
                                  "nj_legend_x",
                                  label = h5("Horizontal Position", style = "color:white; margin-bottom: 0px"),
                                  value = 0.9,
                                  min = -0.9,
                                  max = 1.9,
                                  step = 0.2,
                                  width = "150px",
                                  ticks = FALSE
                                ),
                                br(),
                                sliderInput(
                                  "nj_legend_y",
                                  label = h5("Vertical Position", style = "color:white; margin-bottom: 0px"),
                                  value = 0.2,
                                  min = -1.5,
                                  max = 1.5,
                                  step = 0.1,
                                  width = "150px",
                                  ticks = FALSE
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
            conditionalPanel(
              "input.nj_controls=='Label'",
              column(
                width = 4,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
                  column(
                    width = 12,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Tips"), style = "color:white; position: relative; right: -15px"),
                        fluidRow(
                          column(
                            width = 4,
                            align = "left",
                            div(
                              class = "mat-switch-lab",
                              materialSwitch(
                                "nj_tiplab_show",
                                h5(p("Show"), style = "color:white; padding-left: 5px; position: relative; top: -4px; right: -5px;"),
                                value = TRUE,
                                right = TRUE
                              )
                            )
                          ),
                          column(
                            width = 4,
                            align = "center",
                            uiOutput("nj_tiplab")
                          ),
                          column(
                            width = 3,
                            div(
                              class = "mat-switch-align",
                              materialSwitch(
                                "nj_align",
                                h5(p("Align"), style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"),
                                value = FALSE,
                                right = TRUE
                              )
                            )
                          ),
                          column(
                            width = 1,
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
                                  sliderInput(
                                    "nj_tiplab_alpha",
                                    label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                    min = 0.1,
                                    max = 1,
                                    value = 1,
                                    width = "150px",
                                    ticks = FALSE
                                  ),
                                  br(),
                                  conditionalPanel(
                                    "!(input.nj_layout=='inward'|input.nj_layout=='circular')",
                                    sliderInput(
                                      inputId = "nj_tiplab_nudge_x",
                                      label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                      min = -3,
                                      max = 3,
                                      step = 0.05,
                                      value = 0,
                                      width = "150px",
                                      ticks = FALSE
                                    )
                                  ),
                                  conditionalPanel(
                                    "input.nj_layout=='circular'",
                                    sliderInput(
                                      inputId = "nj_tiplab_position",
                                      label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                      min = -3,
                                      max = 3,
                                      step = 0.05,
                                      value = -0.05,
                                      width = "150px",
                                      ticks = FALSE
                                    )
                                  ),
                                  conditionalPanel(
                                    "input.nj_layout=='inward'",
                                    sliderInput(
                                      inputId = "nj_tiplab_position_inw",
                                      label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                      min = -3,
                                      max = 3,
                                      step = 0.05,
                                      value = 1.1,
                                      width = "150px",
                                      ticks = FALSE
                                    )
                                  ),
                                  br(),
                                  sliderInput(
                                    inputId = "nj_tiplab_angle",
                                    label = h5("Angle", style = "color:white; margin-bottom: 0px"),
                                    min = -90,
                                    max = 90,
                                    value = 0,
                                    ticks = FALSE,
                                    width = "150px",
                                  )      
                                ),
                                column(
                                  width = 6,
                                  align = "center",
                                  uiOutput("nj_tiplab_size"),
                                  br(),
                                  selectInput(
                                    "nj_tiplab_fontface",
                                    label = h5("Fontface", style = "color:white; margin-bottom: 5px; margin-top: 16px"),
                                    width = "250px",
                                    choices = c(Plain = "plain", Bold =  "bold", Italic =  "italic", `B & I` = "bold.italic")
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
                        h5(p("Color"), style = "color:white; position: relative; right: -14px; margin-top: 23px")
                      ),
                      column(
                        width = 4,
                        align = "center",
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
                      )
                    ),
                    fluidRow(
                      column(
                        width = 4,
                        align = "left",
                        br(),
                        div(
                          class = "mat-switch-geom",
                          materialSwitch(
                            "nj_geom",
                            h5(p("Panels"), style = "color:white; padding-left: 5px; position: relative; top: -4px; right: -5px;"),
                            value = FALSE,
                            right = TRUE
                          )
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
                        align = "left",
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
                              uiOutput("nj_tiplab_padding"),
                              br(),
                              sliderInput(
                                inputId = "nj_tiplab_labelradius",
                                label = h5("Smooth edge", style = "color:white; margin-bottom: 0px"),
                                min = 0,
                                max = 0.5,
                                value = 0.2,
                                width = "150px",
                                ticks = FALSE
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
                width = 3,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
                  column(
                    width = 12,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Branches"), style = "color:white; position: relative; right: -15px"),
                        fluidRow(
                          column(
                            width = 5,
                            align = "left",
                            div(
                              class = "mat-switch-lab",
                              materialSwitch(
                                "nj_show_branch_label",
                                h5(p("Show"), style = "color:white; padding-left: 5px; position: relative; top: -4px; right: -5px;"),
                                value = FALSE,
                                right = TRUE
                              )
                            )
                          ),
                          column(
                            width = 5,
                            align = "center",
                            uiOutput("nj_branch_label")
                          ),
                          column(
                            width = 2,
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
                                  sliderInput(
                                    "nj_branchlab_alpha",
                                    label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                    min = 0.1,
                                    max = 1,
                                    value = 0.65,
                                    width = "250px",
                                    ticks = FALSE
                                  ),
                                  br(),
                                  sliderInput(
                                    inputId = "nj_branch_x",
                                    label = h5("X Position", style = "color:white; margin-bottom: 0px"),
                                    min = -3,
                                    max = 3,
                                    value = 0,
                                    width = "250px",
                                    ticks = FALSE
                                  ),
                                  br(),
                                  sliderInput(
                                    inputId = "nj_branch_y",
                                    label = h5("Y Position", style = "color:white; margin-bottom: 0px"),
                                    min = -3,
                                    max = 3,
                                    value = 0,
                                    width = "250px",
                                    ticks = FALSE
                                  )
                                ),
                                column(
                                  width = 6,
                                  align = "center",
                                  uiOutput("nj_branch_size"),
                                  selectInput(
                                    "nj_branchlab_fontface",
                                    label = h5("Fontface", style = "color:white; margin-bottom: 0px;"),
                                    width = "250px",
                                    choices = c(Plain = "plain", Bold =  "bold", Italic =  "italic", `B & I` = "bold.italic")
                                  ),
                                  br(),
                                  sliderInput(
                                    "nj_branch_labelradius",
                                    label = h5("Smooth edge", style = "color:white; margin-bottom: 0px"),
                                    min = 0,
                                    max = 0.5,
                                    value = 0.5,
                                    width = "250px",
                                    ticks = FALSE
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
                        width = 5,
                        align = "left",
                        h5(p("Color"), style = "color:white; position: relative; right: -14px; margin-top: 23px; margin-bottom: 109px")
                      ),
                      column(
                        width = 5,
                        colorPickr(
                          inputId = "nj_branch_label_color",
                          width = "100%",
                          selected = "#FFB7B7",
                          label = "",
                          update = "changestop",
                          interaction = list(clear = FALSE,
                                             save = FALSE),
                          position = "right-start"
                        ),
                        br(), br()
                      )
                    )
                  )
                )
              ),
              column(
                width = 3,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
                  column(
                    width = 12,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Custom Labels"), style = "color:white; position: relative; right: -15px"),
                        fluidRow(
                          column(
                            width = 6,
                            textInput(
                              "nj_new_label_name",
                              "",
                              placeholder = "New Label"
                            )
                          ),
                          column(
                            width = 3,
                            actionButton(
                              "nj_add_new_label",
                              "",
                              icon = icon("plus")
                            )
                          ),
                          column(
                            width = 2,
                            align = "right",
                            dropMenu(
                              actionBttn(
                                "nj_custom_label_menu",
                                label = "",
                                color = "default",
                                size = "sm",
                                style = "material-flat",
                                icon = icon("sliders")
                              ),
                              placement = "top-end",
                              theme = "translucent",
                              fluidRow(
                                column(
                                  width = 12,
                                  align = "center",
                                  uiOutput("nj_custom_labelsize"),
                                  br(),
                                  uiOutput("nj_sliderInput_y"),
                                  br(),
                                  uiOutput("nj_sliderInput_x")
                                )
                              )
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 6,
                            uiOutput("nj_custom_label_select")
                          ),
                          column(
                            width = 4,
                            uiOutput("nj_del_label"),
                          )
                        ),
                        fluidRow(
                          column(
                            width = 12,
                            align = "center",
                            uiOutput("nj_cust_label_save")
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),
            conditionalPanel(
              "input.nj_controls=='Elements'",
              column(
                width = 2,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
                  column(
                    width = 12,
                    align = "left",
                    h4(p("Tip Points"), style = "color:white; position: relative; right: -15px"),
                    fluidRow(
                      column(
                        width = 8,
                        align = "left",
                        div(
                          class = "mat-switch",
                          materialSwitch(
                            "nj_tippoint_show",
                            h5(p("Show"), style = "color:white; padding-left: 5px; position: relative; top: -4px; right: -5px;"),
                            value = FALSE,
                            right = TRUE
                          )
                        )
                      ),
                      column(
                        width = 4,
                        align = "right",
                        dropMenu(
                          actionBttn(
                            "nj_tippoint_menu",
                            label = "",
                            color = "default",
                            size = "sm",
                            style = "material-flat",
                            icon = icon("sliders")
                          ),
                          placement = "top-end",
                          theme = "translucent",
                          fluidRow(
                            column(
                              width = 12,
                              align = "center",
                              sliderInput(
                                "nj_tippoint_alpha",
                                label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                value = 0.5,
                                min = 0.1,
                                max = 1,
                                width = "150px",
                                ticks = FALSE
                              ), 
                              br(),
                              uiOutput("nj_tippoint_size")
                            )
                          )
                        )  
                      )
                    ),
                    fluidRow(
                      column(
                        width = 5,
                        align = "left",
                        h5(p("Color"), style = "color:white; position: relative; right: -15px; margin-top: 36px")
                      ),
                      column(
                        width = 7,
                        align = "center",
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
                    fluidRow(
                      column(
                        width = 5,
                        align = "left",
                        h5(p("Shape"), style = "color:white; position: relative; right: -15px; margin-top: 30px; margin-bottom: 48px")
                      ),
                      column(
                        width = 7,
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
                          h5(p("Variable assigned"), style = "color:white; position: relative; right: -15px; margin-top: 30px; font-style: italic")
                        ),
                        br()
                      )
                    )
                  )
                )
              ),
              column(
                width = 2,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
                  column(
                    width = 12,
                    align = "left",
                    h4(p("Node Points"), style = "color:white; position: relative; right: -15px"),
                    fluidRow(
                      column(
                        width = 8,
                        align = "left",
                        div(
                          class = "mat-switch",
                          materialSwitch(
                            "nj_nodepoint_show",
                            h5(p("Show"), style = "color:white; padding-left: 5px; position: relative; top: -4px; right: -5px;"),
                            value = FALSE,
                            right = TRUE
                          )
                        )
                      ),
                      column(
                        width = 4,
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
                          placement = "top-end",
                          theme = "translucent",
                          fluidRow(
                            column(
                              width = 12,
                              align = "center",
                              sliderInput(
                                "nj_nodepoint_alpha",
                                label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                value = 1,
                                min = 0.1,
                                max = 1,
                                width = "150px",
                                ticks = FALSE
                              ), 
                              br(),
                              uiOutput("nj_nodepoint_size")
                            )
                          )
                        )  
                      )
                    ),
                    fluidRow(
                      column(
                        width = 5,
                        h5(p("Color"), style = "color:white; position: relative; right: -15px; margin-top: 36px")
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
                        h5(p("Shape"), style = "color:white; position: relative; right: -15px; margin-top: 30px; margin-bottom: 48px")
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
                        ),
                        br()
                      )
                    )
                  )
                )
              ),
              column(
                width = 2,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
                  column(
                    width = 12,
                    align = "left",
                    fluidRow(
                      column(
                        width = 6,
                        h4(p("Tiles"), style = "color:white; position: relative; right: -15px")
                      )
                    ),
                    fluidRow(
                      column(
                        width = 5,
                        div(
                          class = "sel-tile-number",
                          selectInput(
                            "nj_tile_number",
                            "",
                            choices = 1:5,
                            width = "70px"
                          )
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
                          placement = "top-start",
                          theme = "translucent",
                          fluidRow(
                            column(
                              width = 12,
                              align = "center",
                              conditionalPanel(
                                "input.nj_tile_num == 1",
                                sliderInput(
                                  "nj_fruit_alpha",
                                  label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                  min = 0.1,
                                  max = 1,
                                  value = 1,
                                  step = 0.05,
                                  width = "150px",
                                  ticks = FALSE
                                )
                              ),
                              conditionalPanel(
                                "input.nj_tile_num == 2",
                                sliderInput(
                                  "nj_fruit_alpha_2",
                                  label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                  min = 0.1,
                                  max = 1,
                                  value = 1,
                                  step = 0.05,
                                  width = "150px",
                                  ticks = FALSE
                                )
                              ),
                              conditionalPanel(
                                "input.nj_tile_num == 3",
                                sliderInput(
                                  "nj_fruit_alpha_3",
                                  label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                  min = 0.1,
                                  max = 1,
                                  value = 1,
                                  step = 0.05,
                                  width = "150px",
                                  ticks = FALSE
                                )
                              ),
                              conditionalPanel(
                                "input.nj_tile_num == 4",
                                sliderInput(
                                  "nj_fruit_alpha_4",
                                  label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                  min = 0.1,
                                  max = 1,
                                  value = 1,
                                  step = 0.05,
                                  width = "150px",
                                  ticks = FALSE
                                )
                              ),
                              conditionalPanel(
                                "input.nj_tile_num == 5",
                                sliderInput(
                                  "nj_fruit_alpha_5",
                                  label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                  min = 0.1,
                                  max = 1,
                                  value = 1,
                                  step = 0.05,
                                  width = "150px",
                                  ticks = FALSE
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    conditionalPanel(
                      "input.nj_tile_num == 1",
                      fluidRow(
                        column(
                          width = 5,
                          h5("Width", style = "color:white; margin-left: 15px; margin-top: 27px;")
                        ),
                        column(
                          width = 7,
                          align = "center",
                          uiOutput("nj_fruit_width"),
                          br()
                        )
                      ),
                      fluidRow(
                        column(
                          width = 5,
                          h5("Position", style = "color:white; margin-left: 15px; margin-top: 32px; margin-bottom: 54px")
                        ),
                        column(
                          width = 7,
                          align = "center",
                          uiOutput("nj_fruit_offset_circ"),
                          br()
                        )
                      )
                    ),
                    conditionalPanel(
                      "input.nj_tile_num == 2",
                      fluidRow(
                        column(
                          width = 5,
                          h5("Width", style = "color:white; margin-left: 15px; margin-top: 27px;")
                        ),
                        column(
                          width = 7,
                          align = "center",
                          uiOutput("nj_fruit_width2"),
                          br()
                        )
                      ),
                      fluidRow(
                        column(
                          width = 5,
                          h5("Position", style = "color:white; margin-left: 15px; margin-top: 32px; margin-bottom: 54px")
                        ),
                        column(
                          width = 7,
                          align = "center",
                          uiOutput("nj_fruit_offset_circ_2"),
                          br()
                        )
                      )
                    ),
                    conditionalPanel(
                      "input.nj_tile_num == 3",
                      fluidRow(
                        column(
                          width = 5,
                          h5("Width", style = "color:white; margin-left: 15px; margin-top: 27px;")
                        ),
                        column(
                          width = 7,
                          align = "center",
                          uiOutput("nj_fruit_width3"),
                          br()
                        )
                      ),
                      fluidRow(
                        column(
                          width = 5,
                          h5("Position", style = "color:white; margin-left: 15px; margin-top: 32px; margin-bottom: 54px")
                        ),
                        column(
                          width = 7,
                          align = "center",
                          uiOutput("nj_fruit_offset_circ_3"),
                          br()
                        )
                      )
                    ),
                    conditionalPanel(
                      "input.nj_tile_num == 4",
                      fluidRow(
                        column(
                          width = 5,
                          h5("Width", style = "color:white; margin-left: 15px; margin-top: 27px;")
                        ),
                        column(
                          width = 7,
                          align = "center",
                          uiOutput("nj_fruit_width4"),
                          br()
                        )
                      ),
                      fluidRow(
                        column(
                          width = 5,
                          h5("Position", style = "color:white; margin-left: 15px; margin-top: 32px; margin-bottom: 54px")
                        ),
                        column(
                          width = 7,
                          align = "center",
                          uiOutput("nj_fruit_offset_circ_4"),
                          br()
                        )
                      )
                    ),
                    conditionalPanel(
                      "input.nj_tile_num == 5",
                      fluidRow(
                        column(
                          width = 5,
                          h5("Width", style = "color:white; margin-left: 15px; margin-top: 27px;")
                        ),
                        column(
                          width = 7,
                          align = "center",
                          uiOutput("nj_fruit_width5"),
                          br()
                        )
                      ),
                      fluidRow(
                        column(
                          width = 5,
                          h5("Position", style = "color:white; margin-left: 15px; margin-top: 32px; margin-bottom: 54px")
                        ),
                        column(
                          width = 7,
                          align = "center",
                          uiOutput("nj_fruit_offset_circ_5"),
                          br()
                        )
                      )
                    )
                  )
                )
              ),
              column(
                width = 2,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
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
                        width = 3,
                        h5("Title", style = "color:white; margin-left: 15px; margin-top: 32px;")
                      ),
                      column(
                        width = 6,
                        align = "center",
                        textInput(
                          "nj_heatmap_title",
                          label = "",
                          value = "Heatmap",
                          placeholder = "Heatmap" 
                        )
                      ),
                      column(
                        width = 3,
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
                          placement = "top-end",
                          theme = "translucent",
                          fluidRow(
                            column(
                              width = 12,
                              align = "center",
                              uiOutput("nj_colnames_angle"),
                              br(),
                              uiOutput("nj_colnames_y")
                            )
                          )
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 5,
                        h5("Width", style = "color: white; margin-left: 15px; margin-top: 40px;")
                      ),
                      column(
                        width = 7,
                        uiOutput("nj_heatmap_width")
                      )
                    ),
                    fluidRow(
                      column(
                        width = 5,
                        h5("Position", style = "color:white; margin-left: 15px; margin-top: 36px;")
                      ),
                      column(
                        width = 7,
                        uiOutput("nj_heatmap_offset")
                      )
                    ),
                    br(), br()
                  )
                )
              ),
              column(
                width = 2,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
                  column(
                    width = 12,
                    align = "left",
                    h4(p("Clade Highlight"), style = "color:white; position: relative; right: -15px"),
                    fluidRow(
                      column(
                        width = 12,
                        div(
                          class = "mat-switch",
                          materialSwitch(
                            "nj_nodelabel_show",
                            h5(p("Toggle Node View"), style = "color:white; padding-left: 5px; position: relative; top: -4px; right: -5px;"),
                            value = FALSE,
                            right = TRUE
                          )
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 3,
                        h5(p("Nodes"), style = "color:white; position: relative; right: -15px; margin-top: 20px")
                      ),
                      column(
                        width = 9,
                        uiOutput("nj_parentnode")
                      )
                    ),
                    uiOutput("nj_clade_scale"),
                    fluidRow(
                      column(
                        width = 8,
                        align = "center",
                        div(
                          class = "sel-clade",
                          selectInput(
                            "nj_clade_type",
                            "",
                            choices = c("Rect" = "rect",
                                        "Round" = "roundrect"),
                            selected = c("Round" = "roundrect")
                          ) 
                        )
                      ),
                      column(
                        width = 4,
                        align = "right",
                        dropMenu(
                          actionBttn(
                            "nj_clade_menu",
                            label = "",
                            color = "default",
                            size = "sm",
                            style = "material-flat",
                            icon = icon("sliders")
                          ),
                          placement = "top-end",
                          theme = "translucent",
                          fluidRow(
                            column(
                              width = 12,
                              align = "center",
                              selectInput(
                                "nj_clade_align",
                                label = h5("Align", style = "color:white; font-size: 14px;"),
                                choices = c("None" = "none",
                                            "Left" = "left",
                                            "Right" = "right",
                                            "Both" = "both"),
                                width = "100px"
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
            conditionalPanel(
              "input.nj_controls=='Variables'",
              column(
                width = 7,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
                  column(
                    width = 12,
                    align = "left",
                    fluidRow(
                      column(
                        width = 3,
                        align = "center",
                        h4(p("Element"), style = "color:white; margin-bottom: 20px")
                      ),
                      column(
                        width = 3,
                        align = "center",
                        h4(p("Variable"), style = "color:white; margin-bottom: 20px; margin-right: 30px;")
                      ),
                      column(
                        width = 6,
                        align = "center",
                        h4(p("Color Scale"), style = "color:white; margin-bottom: 20px")
                      )
                    ),
                    fluidRow(
                      column(
                        width = 3,
                        div(
                          class = "mat-switch-v",
                          materialSwitch(
                            "nj_mapping_show",
                            h5(p("Tip Label Color"), style = "color:white; position: relative; top: -4px; right: -5px;"),
                            value = FALSE,
                            right = TRUE
                          )
                        )
                      ),
                      column(
                        width = 3,
                        align = "center",
                        uiOutput("nj_color_mapping")
                      ),
                      column(
                        width = 3,
                        align = "center",
                        uiOutput("nj_tiplab_scale")
                      ),
                      uiOutput("nj_tiplab_mapping_info"),
                    ),
                    fluidRow(
                      column(
                        width = 3,
                        div(
                          class = "mat-switch-v",
                          materialSwitch(
                            "nj_tipcolor_mapping_show",
                            h5(p("Tip Point Color"), style = "color:white; position: relative; top: -4px; right: -5px;"),
                            value = FALSE,
                            right = TRUE
                          )
                        )
                      ),
                      column(
                        width = 3,
                        align = "center",
                        uiOutput("nj_tipcolor_mapping")
                      ),
                      column(
                        width = 3,
                        align = "center",
                        uiOutput("nj_tippoint_scale")
                      ),
                      uiOutput("nj_tipcolor_mapping_info")
                    ),
                    fluidRow(
                      column(
                        width = 3,
                        div(
                          class = "mat-switch-v",
                          materialSwitch(
                            "nj_tipshape_mapping_show",
                            h5(p("Tip Point Shape"), style = "color:white; position: relative; top: -4px; right: -5px;"),
                            value = FALSE,
                            right = TRUE
                          )
                        )
                      ),
                      column(
                        width = 3,
                        align = "center",
                        uiOutput("nj_tipshape_mapping")
                      ),
                      column(
                        width = 3,
                        HTML(
                          paste(
                            tags$span(style='color: white; font-size: 14px; font-style: italic; position: relative; bottom: -16px; right: -40px;', 'No scale for shapes')
                          )
                        )
                      ),
                      uiOutput("nj_tipshape_mapping_info")
                    ),
                    fluidRow(
                      column(
                        width = 3,
                        fluidRow(
                          column(
                            width = 8,
                            conditionalPanel(
                              "input.nj_tile_num == 1",
                              div(
                                class = "mat-switch-v",
                                materialSwitch(
                                  "nj_tiles_show_1",
                                  h5(p("Tile"), style = "color:white; position: relative; top: -4px; right: -5px; margin-right: 10px"),
                                  value = FALSE,
                                  right = TRUE
                                )
                              )
                            ),
                            conditionalPanel(
                              "input.nj_tile_num == 2",
                              div(
                                class = "mat-switch-v",
                                materialSwitch(
                                  "nj_tiles_show_2",
                                  h5(p("Tile"), style = "color:white; position: relative; top: -4px; right: -5px;"),
                                  value = FALSE,
                                  right = TRUE
                                )
                              )
                            ),
                            conditionalPanel(
                              "input.nj_tile_num == 3",
                              div(
                                class = "mat-switch-v",
                                materialSwitch(
                                  "nj_tiles_show_3",
                                  h5(p("Tile"), style = "color:white; position: relative; top: -4px; right: -5px;"),
                                  value = FALSE,
                                  right = TRUE
                                )
                              )
                            ),
                            conditionalPanel(
                              "input.nj_tile_num == 4",
                              div(
                                class = "mat-switch-v",
                                materialSwitch(
                                  "nj_tiles_show_4",
                                  h5(p("Tile"), style = "color:white; position: relative; top: -4px; right: -5px;"),
                                  value = FALSE,
                                  right = TRUE
                                )
                              )
                            ),
                            conditionalPanel(
                              "input.nj_tile_num == 5",
                              div(
                                class = "mat-switch-v",
                                materialSwitch(
                                  "nj_tiles_show_5",
                                  h5(p("Tile"), style = "color:white; position: relative; top: -4px; right: -5px;"),
                                  value = FALSE,
                                  right = TRUE
                                )
                              )
                            )
                          ), 
                          column(
                            width = 4,
                            align = "left",
                            div(
                              class = "tile-sel",
                              selectInput(
                                "nj_tile_num",
                                "",
                                choices = 1:5,
                                width = "50px"
                              )
                            )
                          )
                        )
                      ),
                      column(
                        width = 3,
                        align = "center",
                        conditionalPanel(
                          "input.nj_tile_num == 1",
                          div(
                            class = "heatmap-scale",
                            uiOutput("nj_fruit_variable")
                          )
                        ),
                        conditionalPanel(
                          "input.nj_tile_num == 2",
                          div(
                            class = "heatmap-scale",
                            uiOutput("nj_fruit_variable2")
                          )
                        ),
                        conditionalPanel(
                          "input.nj_tile_num == 3",
                          div(
                            class = "heatmap-scale",
                            uiOutput("nj_fruit_variable3")
                          )
                        ),
                        conditionalPanel(
                          "input.nj_tile_num == 4",
                          div(
                            class = "heatmap-scale",
                            uiOutput("nj_fruit_variable4")
                          )
                        ),
                        conditionalPanel(
                          "input.nj_tile_num == 5",
                          div(
                            class = "heatmap-scale",
                            uiOutput("nj_fruit_variable5")
                          )
                        )
                      ),
                      conditionalPanel(
                        "input.nj_tile_num == 1",
                        column(
                          width = 3,
                          align = "center",
                          div(
                            class = "heatmap-scale",
                            uiOutput("nj_tiles_scale_1")
                          )
                        )
                      ),
                      conditionalPanel(
                        "input.nj_tile_num == 2",
                        column(
                          width = 3,
                          align = "center",
                          div(
                            class = "heatmap-scale",
                            uiOutput("nj_tiles_scale_2")
                          )
                        )
                      ),
                      conditionalPanel(
                        "input.nj_tile_num == 3",
                        column(
                          width = 3,
                          align = "center",
                          div(
                            class = "heatmap-scale",
                            uiOutput("nj_tiles_scale_3")
                          )
                        )
                      ),
                      conditionalPanel(
                        "input.nj_tile_num == 4",
                        column(
                          width = 3,
                          align = "center",
                          div(
                            class = "heatmap-scale",
                            uiOutput("nj_tiles_scale_4")
                          )
                        )
                      ),
                      conditionalPanel(
                        "input.nj_tile_num == 5",
                        column(
                          width = 3,
                          align = "center",
                          div(
                            class = "heatmap-scale",
                            uiOutput("nj_tiles_scale_5")
                          )
                        )
                      ),
                      uiOutput("nj_fruit_mapping_info")
                    ),
                    fluidRow(
                      column(
                        width = 3,
                        div(
                          class = "mat-switch-v",
                          materialSwitch(
                            "nj_heatmap_show",
                            h5(p("Heatmap"), style = "color:white; position: relative; top: -4px; right: -5px;"),
                            value = FALSE,
                            right = TRUE
                          )
                        )
                      ),
                      column(
                        width = 3,
                        align = "center",
                        uiOutput("nj_heatmap_sel")
                      ),
                      column(
                        width = 3,
                        align = "center",
                        div(
                          class = "heatmap-scale",
                          uiOutput("nj_heatmap_scale")
                        )
                      ),
                      uiOutput("nj_heatmap_mapping_info")
                    )
                  )
                )
              )
            )
          ),
          br(), br(), br(), br(), br(), br()
        ),
        
        ### Control Panels UPGMA ----
        
        conditionalPanel(
          "input.tree_algo=='UPGMA'",
          fluidRow(
            column(
              width = 1,
              radioGroupButtons(
                inputId = "upgma_controls",
                label = "",
                choices = c("Layout", "Label", "Elements", "Variables"),
                direction = "vertical"
              )
            ),
            conditionalPanel(
              "input.upgma_controls=='Layout'",
              column(
                width = 2,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
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
                          selected = "rectangular",
                          width = "90%"
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 8,
                        align = "left",
                        div(
                          class = "mat-switch-layout",
                          materialSwitch(
                            "upgma_rootedge_show",
                            h5(p("Rootedge"), style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"),
                            value = FALSE,
                            right = TRUE
                          )
                        )
                      ),
                      column(
                        width = 4,
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
                              uiOutput("upgma_rootedge_length"),
                              br(),
                              selectInput(
                                "upgma_rootedge_line",
                                label = h5("Rootedge Line", style = "color:white"),
                                choices = c(Solid = "solid", Dashed = "dashed", Dotted = "dotted"),
                                selected = c(Dotted = "solid"),
                                width = "100px"
                              ),
                              br(),
                              conditionalPanel(
                                "input.upgma_layout=='circular'",
                                sliderInput(
                                  "upgma_xlim",
                                  label = h5("Adjust Circular", style = "color:white"),
                                  min = -50,
                                  max = 0,
                                  value = -10,
                                  width = "150px",
                                  ticks = FALSE
                                )
                              ),
                              conditionalPanel(
                                "input.upgma_layout=='inward'",
                                sliderInput(
                                  "upgma_inward_xlim",
                                  label = h5("Adjust Circular", style = "color:white"),
                                  min = 30,
                                  max = 120,
                                  value = 50,
                                  ticks = FALSE,
                                  width = "150px",
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
                        div(
                          class = "mat-switch-re",
                          materialSwitch(
                            "upgma_ladder",
                            h5(p("Ladderize"), style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"),
                            value = TRUE,
                            right = TRUE
                          )
                        )
                      )
                    )
                  )
                )
              ),
              column(
                width = 2,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
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
                    br(),
                    fluidRow(
                      column(
                        width = 5,
                        h5(p("Background"), style = "color:white; position: relative; right: -15px; margin-top: 30px; margin-bottom: 38px")
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
                    ), 
                    br()
                  )
                )
              ),
              column(
                width = 2,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
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
                      textInput(
                        "upgma_subtitle",
                        label = "",
                        width = "100%",
                        placeholder = "Plot Subtitle"
                      ),
                      br(),
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
                                  label = h5("Title Size", style = "color:white; margin-bottom: 0px"),
                                  value = 30,
                                  min = 15,
                                  max = 40,
                                  step = 1,
                                  width = "80px"
                                ),
                                br(),
                                numericInput(
                                  "upgma_subtitle_size",
                                  label = h5("Subtitle Size", style = "color:white; margin-bottom: 0px"),
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
              ),
              column(
                width = 2,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
                  column(
                    width = 12,
                    align = "left",
                    h4(p("Sizing"), style = "color:white; position: relative; right: -15px"),
                    column(
                      width = 12,
                      align = "center",
                      br(),
                      fluidRow(
                        column(
                          width = 3,
                          h5("Ratio", style = "color: white; font-size: 14px;")
                        ),
                        column(
                          width = 6,
                          align = "left",
                          div(
                            class = "ratio-sel",
                            selectInput(
                              "upgma_ratio",
                              "",
                              choices = c("16:10" = (16/10), "16:9" = (16/9), "4:3" = (4/3))
                            )
                          )
                        ),
                        column(
                          width = 3,
                          dropMenu(
                            actionBttn(
                              "upgma_size_menu",
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
                                sliderInput(
                                  "upgma_v",
                                  label = "Vertical Position",
                                  min = -0.5,
                                  max = 0.5,
                                  step = 0.01,
                                  value = 0,
                                  width = "150px",
                                  ticks = FALSE
                                ),
                                br(),
                                sliderInput(
                                  "upgma_h",
                                  label = "Horizontal Position",
                                  min = -0.5,
                                  max = 0.5,
                                  step = 0.01,
                                  value = 0,
                                  width = "150px",
                                  ticks = FALSE
                                )
                              )
                            )
                          )
                        )
                      ),
                      fluidRow(
                        column(
                          width = 3,
                          h5("Size", style = "color: white; font-size: 14px; margin-top: 30px")
                        ),
                        column(
                          width = 9,
                          sliderInput(
                            "upgma_scale",
                            "",
                            min = 500,
                            max = 1200,
                            value = 800,
                            width = "95%",
                            ticks = FALSE
                          )
                        )
                      ),
                      fluidRow(
                        column(
                          width = 3,
                          h5("Zoom", style = "color: white; font-size: 14px; margin-top: 30px")
                        ),
                        column(
                          width = 9,
                          div(
                            class = "zoom-slider",
                            sliderInput(
                              "upgma_zoom",
                              label = NULL,
                              min = 0.5,
                              max = 1.5,
                              step = 0.05,
                              value = 0.95,
                              ticks = FALSE
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              column(
                width = 2,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
                  column(
                    width = 12,
                    align = "left",
                    h4(p("Tree Scale"), style = "color:white; position: relative; right: -15px"),
                    column(
                      width = 12,
                      fluidRow(
                        column(
                          width = 8,
                          align = "left",
                          div(
                            class = "mat-switch-layout",
                            materialSwitch(
                              "upgma_treescale_show",
                              h5(p("Show"), style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"),
                              value = TRUE,
                              right = TRUE
                            )
                          ),
                          br()
                        ),
                        column(
                          width = 4,
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
                                uiOutput("upgma_treescale_width"),
                                br(),
                                uiOutput("upgma_treescale_x"),
                                br(),
                                uiOutput("upgma_treescale_y")
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  column(
                    width = 12,
                    align = "left",
                    h4(p("Legend"), style = "color:white; position: relative; right: -15px; margin-top: 10px; margin-bottom: -2px"),
                    column(
                      width = 12,
                      align = "center",
                      fluidRow(
                        column(
                          width = 7,
                          align = "left",
                          prettyRadioButtons(
                            "upgma_legend_orientation",
                            "",
                            choices = c(Horizontal = "horizontal",
                                        Vertical = "vertical"),
                            selected = c(Vertical = "vertical"),
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
                                br(),
                                sliderInput(
                                  "upgma_legend_x",
                                  label = h5("X Position", style = "color:white; margin-bottom: 0px"),
                                  value = 0.9,
                                  min = -0.9,
                                  max = 1.9,
                                  step = 0.2,
                                  width = "150px",
                                  ticks = FALSE
                                ),
                                br(),
                                sliderInput(
                                  "upgma_legend_y",
                                  label = h5("Y Position", style = "color:white; margin-bottom: 0px"),
                                  value = 0.2,
                                  min = -1.5,
                                  max = 1.5,
                                  step = 0.1,
                                  width = "150px",
                                  ticks = FALSE
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
            conditionalPanel(
              "input.upgma_controls=='Label'",
              column(
                width = 4,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
                  column(
                    width = 12,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Tips"), style = "color:white; position: relative; right: -15px"),
                        fluidRow(
                          column(
                            width = 4,
                            align = "left",
                            div(
                              class = "mat-switch-lab",
                              materialSwitch(
                                "upgma_tiplab_show",
                                h5(p("Show"), style = "color:white; padding-left: 5px; position: relative; top: -4px; right: -5px;"),
                                value = TRUE,
                                right = TRUE
                              )
                            )
                          ),
                          column(
                            width = 4,
                            align = "center",
                            uiOutput("upgma_tiplab")
                          ),
                          column(
                            width = 3,
                            div(
                              class = "mat-switch-align",
                              materialSwitch(
                                "upgma_align",
                                h5(p("Align"), style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"),
                                value = FALSE,
                                right = TRUE
                              )
                            )
                          ),
                          column(
                            width = 1,
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
                                  sliderInput(
                                    "upgma_tiplab_alpha",
                                    label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                    min = 0.1,
                                    max = 1,
                                    value = 1,
                                    width = "150px",
                                    ticks = FALSE
                                  ),
                                  br(),
                                  conditionalPanel(
                                    "!(input.upgma_layout=='inward'|input.upgma_layout=='circular')",
                                    sliderInput(
                                      inputId = "upgma_tiplab_nudge_x",
                                      label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                      min = -3,
                                      max = 3,
                                      step = 0.05,
                                      value = 0,
                                      width = "150px",
                                      ticks = FALSE
                                    )
                                  ),
                                  conditionalPanel(
                                    "input.upgma_layout=='circular'",
                                    sliderInput(
                                      inputId = "upgma_tiplab_position",
                                      label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                      min = -3,
                                      max = 3,
                                      step = 0.05,
                                      value = -0.05,
                                      width = "150px",
                                      ticks = FALSE
                                    )
                                  ),
                                  conditionalPanel(
                                    "input.upgma_layout=='inward'",
                                    sliderInput(
                                      inputId = "upgma_tiplab_position_inw",
                                      label = h5("Position", style = "color:white; margin-bottom: 0px"),
                                      min = -3,
                                      max = 3,
                                      step = 0.05,
                                      value = 1.1,
                                      width = "150px",
                                      ticks = FALSE
                                    )
                                  ),
                                  br(),
                                  sliderInput(
                                    inputId = "upgma_tiplab_angle",
                                    label = h5("Angle", style = "color:white; margin-bottom: 0px"),
                                    min = -90,
                                    max = 90,
                                    value = 0,
                                    ticks = FALSE,
                                    width = "150px",
                                  )      
                                ),
                                column(
                                  width = 6,
                                  align = "center",
                                  uiOutput("upgma_tiplab_size"),
                                  br(),
                                  selectInput(
                                    "upgma_tiplab_fontface",
                                    label = h5("Fontface", style = "color:white; margin-bottom: 5px; margin-top: 16px"),
                                    width = "250px",
                                    choices = c(Plain = "plain", Bold =  "bold", Italic =  "italic", `B & I` = "bold.italic")
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
                        h5(p("Color"), style = "color:white; position: relative; right: -14px; margin-top: 23px")
                      ),
                      column(
                        width = 4,
                        align = "center",
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
                      )
                    ),
                    fluidRow(
                      column(
                        width = 4,
                        align = "left",
                        br(),
                        div(
                          class = "mat-switch-geom",
                          materialSwitch(
                            "upgma_geom",
                            h5(p("Panels"), style = "color:white; padding-left: 5px; position: relative; top: -4px; right: -5px;"),
                            value = FALSE,
                            right = TRUE
                          )
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
                        align = "left",
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
                              uiOutput("upgma_tiplab_padding"),
                              br(),
                              sliderInput(
                                inputId = "upgma_tiplab_labelradius",
                                label = h5("Smooth edge", style = "color:white; margin-bottom: 0px"),
                                min = 0,
                                max = 0.5,
                                value = 0.2,
                                width = "150px",
                                ticks = FALSE
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
                width = 3,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
                  column(
                    width = 12,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Branches"), style = "color:white; position: relative; right: -15px"),
                        fluidRow(
                          column(
                            width = 5,
                            align = "left",
                            div(
                              class = "mat-switch-lab",
                              materialSwitch(
                                "upgma_show_branch_label",
                                h5(p("Show"), style = "color:white; padding-left: 5px; position: relative; top: -4px; right: -5px;"),
                                value = FALSE,
                                right = TRUE
                              )
                            )
                          ),
                          column(
                            width = 5,
                            align = "center",
                            uiOutput("upgma_branch_label")
                          ),
                          column(
                            width = 2,
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
                                  sliderInput(
                                    "upgma_branchlab_alpha",
                                    label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                    min = 0.1,
                                    max = 1,
                                    value = 0.65,
                                    width = "250px",
                                    ticks = FALSE
                                  ),
                                  br(),
                                  sliderInput(
                                    inputId = "upgma_branch_x",
                                    label = h5("X Position", style = "color:white; margin-bottom: 0px"),
                                    min = -3,
                                    max = 3,
                                    value = 0,
                                    width = "250px",
                                    ticks = FALSE
                                  ),
                                  br(),
                                  sliderInput(
                                    inputId = "upgma_branch_y",
                                    label = h5("Y Position", style = "color:white; margin-bottom: 0px"),
                                    min = -3,
                                    max = 3,
                                    value = 0,
                                    width = "250px",
                                    ticks = FALSE
                                  )
                                ),
                                column(
                                  width = 6,
                                  align = "center",
                                  uiOutput("upgma_branch_size"),
                                  selectInput(
                                    "upgma_branchlab_fontface",
                                    label = h5("Fontface", style = "color:white; margin-bottom: 0px;"),
                                    width = "250px",
                                    choices = c(Plain = "plain", Bold =  "bold", Italic =  "italic", `B & I` = "bold.italic")
                                  ),
                                  br(),
                                  sliderInput(
                                    "upgma_branch_labelradius",
                                    label = h5("Smooth edge", style = "color:white; margin-bottom: 0px"),
                                    min = 0,
                                    max = 0.5,
                                    value = 0.5,
                                    width = "250px",
                                    ticks = FALSE
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
                        width = 5,
                        align = "left",
                        h5(p("Color"), style = "color:white; position: relative; right: -14px; margin-top: 23px; margin-bottom: 109px")
                      ),
                      column(
                        width = 5,
                        colorPickr(
                          inputId = "upgma_branch_label_color",
                          width = "100%",
                          selected = "#FFB7B7",
                          label = "",
                          update = "changestop",
                          interaction = list(clear = FALSE,
                                             save = FALSE),
                          position = "right-start"
                        ),
                        br(), br()
                      )
                    )
                  )
                )
              ),
              column(
                width = 3,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
                  column(
                    width = 12,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Custom Labels"), style = "color:white; position: relative; right: -15px"),
                        fluidRow(
                          column(
                            width = 6,
                            textInput(
                              "upgma_new_label_name",
                              "",
                              placeholder = "New Label"
                            )
                          ),
                          column(
                            width = 3,
                            actionButton(
                              "upgma_add_new_label",
                              "",
                              icon = icon("plus")
                            )
                          ),
                          column(
                            width = 2,
                            align = "right",
                            dropMenu(
                              actionBttn(
                                "upgma_custom_label_menu",
                                label = "",
                                color = "default",
                                size = "sm",
                                style = "material-flat",
                                icon = icon("sliders")
                              ),
                              placement = "top-end",
                              theme = "translucent",
                              fluidRow(
                                column(
                                  width = 12,
                                  align = "center",
                                  uiOutput("upgma_custom_labelsize"),
                                  br(),
                                  uiOutput("upgma_sliderInput_y"),
                                  br(),
                                  uiOutput("upgma_sliderInput_x")
                                )
                              )
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 6,
                            uiOutput("upgma_custom_label_select")
                          ),
                          column(
                            width = 4,
                            uiOutput("upgma_del_label"),
                          )
                        ),
                        fluidRow(
                          column(
                            width = 12,
                            align = "center",
                            uiOutput("upgma_cust_label_save")
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),
            conditionalPanel(
              "input.upgma_controls=='Elements'",
              column(
                width = 2,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
                  column(
                    width = 12,
                    align = "left",
                    h4(p("Tip Points"), style = "color:white; position: relative; right: -15px"),
                    fluidRow(
                      column(
                        width = 8,
                        align = "left",
                        div(
                          class = "mat-switch",
                          materialSwitch(
                            "upgma_tippoint_show",
                            h5(p("Show"), style = "color:white; padding-left: 5px; position: relative; top: -4px; right: -5px;"),
                            value = FALSE,
                            right = TRUE
                          )
                        )
                      ),
                      column(
                        width = 4,
                        align = "right",
                        dropMenu(
                          actionBttn(
                            "upgma_tippoint_menu",
                            label = "",
                            color = "default",
                            size = "sm",
                            style = "material-flat",
                            icon = icon("sliders")
                          ),
                          placement = "top-end",
                          theme = "translucent",
                          fluidRow(
                            column(
                              width = 12,
                              align = "center",
                              sliderInput(
                                "upgma_tippoint_alpha",
                                label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                value = 0.5,
                                min = 0.1,
                                max = 1,
                                width = "150px",
                                ticks = FALSE
                              ), 
                              br(),
                              uiOutput("upgma_tippoint_size")
                            )
                          )
                        )  
                      )
                    ),
                    fluidRow(
                      column(
                        width = 5,
                        align = "left",
                        h5(p("Color"), style = "color:white; position: relative; right: -15px; margin-top: 36px")
                      ),
                      column(
                        width = 7,
                        align = "center",
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
                    fluidRow(
                      column(
                        width = 5,
                        align = "left",
                        h5(p("Shape"), style = "color:white; position: relative; right: -15px; margin-top: 30px; margin-bottom: 48px")
                      ),
                      column(
                        width = 7,
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
                          h5(p("Variable assigned"), style = "color:white; position: relative; right: -15px; margin-top: 30px; font-style: italic")
                        ),
                        br()
                      )
                    )
                  )
                )
              ),
              column(
                width = 2,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
                  column(
                    width = 12,
                    align = "left",
                    h4(p("Node Points"), style = "color:white; position: relative; right: -15px"),
                    fluidRow(
                      column(
                        width = 8,
                        align = "left",
                        div(
                          class = "mat-switch",
                          materialSwitch(
                            "upgma_nodepoint_show",
                            h5(p("Show"), style = "color:white; padding-left: 5px; position: relative; top: -4px; right: -5px;"),
                            value = FALSE,
                            right = TRUE
                          )
                        )
                      ),
                      column(
                        width = 4,
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
                          placement = "top-end",
                          theme = "translucent",
                          fluidRow(
                            column(
                              width = 12,
                              align = "center",
                              sliderInput(
                                "upgma_nodepoint_alpha",
                                label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                value = 1,
                                min = 0.1,
                                max = 1,
                                width = "150px",
                                ticks = FALSE
                              ), 
                              br(),
                              uiOutput("upgma_nodepoint_size")
                            )
                          )
                        )  
                      )
                    ),
                    fluidRow(
                      column(
                        width = 5,
                        h5(p("Color"), style = "color:white; position: relative; right: -15px; margin-top: 36px")
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
                        h5(p("Shape"), style = "color:white; position: relative; right: -15px; margin-top: 30px; margin-bottom: 48px")
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
                        ),
                        br()
                      )
                    )
                  )
                )
              ),
              column(
                width = 2,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
                  column(
                    width = 12,
                    align = "left",
                    fluidRow(
                      column(
                        width = 6,
                        h4(p("Tiles"), style = "color:white; position: relative; right: -15px")
                      )
                    ),
                    fluidRow(
                      column(
                        width = 5,
                        div(
                          class = "sel-tile-number",
                          selectInput(
                            "upgma_tile_number",
                            "",
                            choices = 1:5,
                            width = "70px"
                          )
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
                          placement = "top-start",
                          theme = "translucent",
                          fluidRow(
                            column(
                              width = 12,
                              align = "center",
                              conditionalPanel(
                                "input.upgma_tile_num == 1",
                                sliderInput(
                                  "upgma_fruit_alpha",
                                  label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                  min = 0.1,
                                  max = 1,
                                  value = 1,
                                  step = 0.05,
                                  width = "150px",
                                  ticks = FALSE
                                )
                              ),
                              conditionalPanel(
                                "input.upgma_tile_num == 2",
                                sliderInput(
                                  "upgma_fruit_alpha_2",
                                  label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                  min = 0.1,
                                  max = 1,
                                  value = 1,
                                  step = 0.05,
                                  width = "150px",
                                  ticks = FALSE
                                )
                              ),
                              conditionalPanel(
                                "input.upgma_tile_num == 3",
                                sliderInput(
                                  "upgma_fruit_alpha_3",
                                  label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                  min = 0.1,
                                  max = 1,
                                  value = 1,
                                  step = 0.05,
                                  width = "150px",
                                  ticks = FALSE
                                )
                              ),
                              conditionalPanel(
                                "input.upgma_tile_num == 4",
                                sliderInput(
                                  "upgma_fruit_alpha_4",
                                  label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                  min = 0.1,
                                  max = 1,
                                  value = 1,
                                  step = 0.05,
                                  width = "150px",
                                  ticks = FALSE
                                )
                              ),
                              conditionalPanel(
                                "input.upgma_tile_num == 5",
                                sliderInput(
                                  "upgma_fruit_alpha_5",
                                  label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                  min = 0.1,
                                  max = 1,
                                  value = 1,
                                  step = 0.05,
                                  width = "150px",
                                  ticks = FALSE
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    conditionalPanel(
                      "input.upgma_tile_num == 1",
                      fluidRow(
                        column(
                          width = 5,
                          h5("Width", style = "color:white; margin-left: 15px; margin-top: 27px;")
                        ),
                        column(
                          width = 7,
                          align = "center",
                          uiOutput("upgma_fruit_width"),
                          br()
                        )
                      ),
                      fluidRow(
                        column(
                          width = 5,
                          h5("Position", style = "color:white; margin-left: 15px; margin-top: 32px; margin-bottom: 54px")
                        ),
                        column(
                          width = 7,
                          align = "center",
                          uiOutput("upgma_fruit_offset_circ"),
                          br()
                        )
                      )
                    ),
                    conditionalPanel(
                      "input.upgma_tile_num == 2",
                      fluidRow(
                        column(
                          width = 5,
                          h5("Width", style = "color:white; margin-left: 15px; margin-top: 27px;")
                        ),
                        column(
                          width = 7,
                          align = "center",
                          uiOutput("upgma_fruit_width2"),
                          br()
                        )
                      ),
                      fluidRow(
                        column(
                          width = 5,
                          h5("Position", style = "color:white; margin-left: 15px; margin-top: 32px; margin-bottom: 54px")
                        ),
                        column(
                          width = 7,
                          align = "center",
                          uiOutput("upgma_fruit_offset_circ_2"),
                          br()
                        )
                      )
                    ),
                    conditionalPanel(
                      "input.upgma_tile_num == 3",
                      fluidRow(
                        column(
                          width = 5,
                          h5("Width", style = "color:white; margin-left: 15px; margin-top: 27px;")
                        ),
                        column(
                          width = 7,
                          align = "center",
                          uiOutput("upgma_fruit_width3"),
                          br()
                        )
                      ),
                      fluidRow(
                        column(
                          width = 5,
                          h5("Position", style = "color:white; margin-left: 15px; margin-top: 32px; margin-bottom: 54px")
                        ),
                        column(
                          width = 7,
                          align = "center",
                          uiOutput("upgma_fruit_offset_circ_3"),
                          br()
                        )
                      )
                    ),
                    conditionalPanel(
                      "input.upgma_tile_num == 4",
                      fluidRow(
                        column(
                          width = 5,
                          h5("Width", style = "color:white; margin-left: 15px; margin-top: 27px;")
                        ),
                        column(
                          width = 7,
                          align = "center",
                          uiOutput("upgma_fruit_width4"),
                          br()
                        )
                      ),
                      fluidRow(
                        column(
                          width = 5,
                          h5("Position", style = "color:white; margin-left: 15px; margin-top: 32px; margin-bottom: 54px")
                        ),
                        column(
                          width = 7,
                          align = "center",
                          uiOutput("upgma_fruit_offset_circ_4"),
                          br()
                        )
                      )
                    ),
                    conditionalPanel(
                      "input.upgma_tile_num == 5",
                      fluidRow(
                        column(
                          width = 5,
                          h5("Width", style = "color:white; margin-left: 15px; margin-top: 27px;")
                        ),
                        column(
                          width = 7,
                          align = "center",
                          uiOutput("upgma_fruit_width5"),
                          br()
                        )
                      ),
                      fluidRow(
                        column(
                          width = 5,
                          h5("Position", style = "color:white; margin-left: 15px; margin-top: 32px; margin-bottom: 54px")
                        ),
                        column(
                          width = 7,
                          align = "center",
                          uiOutput("upgma_fruit_offset_circ_5"),
                          br()
                        )
                      )
                    )
                  )
                )
              ),
              column(
                width = 2,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
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
                        width = 3,
                        h5("Title", style = "color:white; margin-left: 15px; margin-top: 32px;")
                      ),
                      column(
                        width = 6,
                        align = "center",
                        textInput(
                          "upgma_heatmap_title",
                          label = "",
                          value = "Heatmap",
                          placeholder = "Heatmap" 
                        )
                      ),
                      column(
                        width = 3,
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
                          placement = "top-end",
                          theme = "translucent",
                          fluidRow(
                            column(
                              width = 12,
                              align = "center",
                              uiOutput("upgma_colnames_angle"),
                              br(),
                              uiOutput("upgma_colnames_y")
                            )
                          )
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 5,
                        h5("Width", style = "color: white; margin-left: 15px; margin-top: 40px;")
                      ),
                      column(
                        width = 7,
                        uiOutput("upgma_heatmap_width")
                      )
                    ),
                    fluidRow(
                      column(
                        width = 5,
                        h5("Position", style = "color:white; margin-left: 15px; margin-top: 36px;")
                      ),
                      column(
                        width = 7,
                        uiOutput("upgma_heatmap_offset")
                      )
                    ),
                    br(), br()
                  )
                )
              ),
              column(
                width = 2,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
                  column(
                    width = 12,
                    align = "left",
                    h4(p("Clade Highlight"), style = "color:white; position: relative; right: -15px"),
                    fluidRow(
                      column(
                        width = 12,
                        div(
                          class = "mat-switch",
                          materialSwitch(
                            "upgma_nodelabel_show",
                            h5(p("Toggle Node View"), style = "color:white; padding-left: 5px; position: relative; top: -4px; right: -5px;"),
                            value = FALSE,
                            right = TRUE
                          )
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 3,
                        h5(p("Nodes"), style = "color:white; position: relative; right: -15px; margin-top: 20px")
                      ),
                      column(
                        width = 9,
                        uiOutput("upgma_parentnode")
                      )
                    ),
                    uiOutput("upgma_clade_scale"),
                    fluidRow(
                      column(
                        width = 8,
                        align = "center",
                        div(
                          class = "sel-clade",
                          selectInput(
                            "upgma_clade_type",
                            "",
                            choices = c("Rect" = "rect",
                                        "Round" = "roundrect"),
                            selected = c("Round" = "roundrect")
                          ) 
                        )
                      ),
                      column(
                        width = 4,
                        align = "right",
                        dropMenu(
                          actionBttn(
                            "upgma_clade_menu",
                            label = "",
                            color = "default",
                            size = "sm",
                            style = "material-flat",
                            icon = icon("sliders")
                          ),
                          placement = "top-end",
                          theme = "translucent",
                          fluidRow(
                            column(
                              width = 12,
                              align = "center",
                              selectInput(
                                "upgma_clade_align",
                                label = h5("Align", style = "color:white; font-size: 14px;"),
                                choices = c("None" = "none",
                                            "Left" = "left",
                                            "Right" = "right",
                                            "Both" = "both"),
                                width = "100px"
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
            conditionalPanel(
              "input.upgma_controls=='Variables'",
              column(
                width = 7,
                box(
                  solidHeader = TRUE,
                  status = "info",
                  width = "100%",
                  column(
                    width = 12,
                    align = "left",
                    fluidRow(
                      column(
                        width = 3,
                        align = "center",
                        h4(p("Element"), style = "color:white; margin-bottom: 20px")
                      ),
                      column(
                        width = 3,
                        align = "center",
                        h4(p("Variable"), style = "color:white; margin-bottom: 20px; margin-right: 30px;")
                      ),
                      column(
                        width = 6,
                        align = "center",
                        h4(p("Color Scale"), style = "color:white; margin-bottom: 20px")
                      )
                    ),
                    fluidRow(
                      column(
                        width = 3,
                        div(
                          class = "mat-switch-v",
                          materialSwitch(
                            "upgma_mapping_show",
                            h5(p("Tip Label Color"), style = "color:white; position: relative; top: -4px; right: -5px;"),
                            value = FALSE,
                            right = TRUE
                          )
                        )
                      ),
                      column(
                        width = 3,
                        align = "center",
                        uiOutput("upgma_color_mapping")
                      ),
                      column(
                        width = 3,
                        align = "center",
                        uiOutput("upgma_tiplab_scale")
                      ),
                      uiOutput("upgma_tiplab_mapping_info"),
                    ),
                    fluidRow(
                      column(
                        width = 3,
                        div(
                          class = "mat-switch-v",
                          materialSwitch(
                            "upgma_tipcolor_mapping_show",
                            h5(p("Tip Point Color"), style = "color:white; position: relative; top: -4px; right: -5px;"),
                            value = FALSE,
                            right = TRUE
                          )
                        )
                      ),
                      column(
                        width = 3,
                        align = "center",
                        uiOutput("upgma_tipcolor_mapping")
                      ),
                      column(
                        width = 3,
                        align = "center",
                        uiOutput("upgma_tippoint_scale")
                      ),
                      uiOutput("upgma_tipcolor_mapping_info")
                    ),
                    fluidRow(
                      column(
                        width = 3,
                        div(
                          class = "mat-switch-v",
                          materialSwitch(
                            "upgma_tipshape_mapping_show",
                            h5(p("Tip Point Shape"), style = "color:white; position: relative; top: -4px; right: -5px;"),
                            value = FALSE,
                            right = TRUE
                          )
                        )
                      ),
                      column(
                        width = 3,
                        align = "center",
                        uiOutput("upgma_tipshape_mapping")
                      ),
                      column(
                        width = 3,
                        HTML(
                          paste(
                            tags$span(style='color: white; font-size: 14px; font-style: italic; position: relative; bottom: -16px; right: -40px;', 'No scale for shapes')
                          )
                        )
                      ),
                      uiOutput("upgma_tipshape_mapping_info")
                    ),
                    fluidRow(
                      column(
                        width = 3,
                        fluidRow(
                          column(
                            width = 8,
                            conditionalPanel(
                              "input.upgma_tile_num == 1",
                              div(
                                class = "mat-switch-v",
                                materialSwitch(
                                  "upgma_tiles_show_1",
                                  h5(p("Tile"), style = "color:white; position: relative; top: -4px; right: -5px; margin-right: 10px"),
                                  value = FALSE,
                                  right = TRUE
                                )
                              )
                            ),
                            conditionalPanel(
                              "input.upgma_tile_num == 2",
                              div(
                                class = "mat-switch-v",
                                materialSwitch(
                                  "upgma_tiles_show_2",
                                  h5(p("Tile"), style = "color:white; position: relative; top: -4px; right: -5px;"),
                                  value = FALSE,
                                  right = TRUE
                                )
                              )
                            ),
                            conditionalPanel(
                              "input.upgma_tile_num == 3",
                              div(
                                class = "mat-switch-v",
                                materialSwitch(
                                  "upgma_tiles_show_3",
                                  h5(p("Tile"), style = "color:white; position: relative; top: -4px; right: -5px;"),
                                  value = FALSE,
                                  right = TRUE
                                )
                              )
                            ),
                            conditionalPanel(
                              "input.upgma_tile_num == 4",
                              div(
                                class = "mat-switch-v",
                                materialSwitch(
                                  "upgma_tiles_show_4",
                                  h5(p("Tile"), style = "color:white; position: relative; top: -4px; right: -5px;"),
                                  value = FALSE,
                                  right = TRUE
                                )
                              )
                            ),
                            conditionalPanel(
                              "input.upgma_tile_num == 5",
                              div(
                                class = "mat-switch-v",
                                materialSwitch(
                                  "upgma_tiles_show_5",
                                  h5(p("Tile"), style = "color:white; position: relative; top: -4px; right: -5px;"),
                                  value = FALSE,
                                  right = TRUE
                                )
                              )
                            )
                          ), 
                          column(
                            width = 4,
                            align = "left",
                            div(
                              class = "tile-sel",
                              selectInput(
                                "upgma_tile_num",
                                "",
                                choices = 1:5,
                                width = "50px"
                              )
                            )
                          )
                        )
                      ),
                      column(
                        width = 3,
                        align = "center",
                        conditionalPanel(
                          "input.upgma_tile_num == 1",
                          div(
                            class = "heatmap-scale",
                            uiOutput("upgma_fruit_variable")
                          )
                        ),
                        conditionalPanel(
                          "input.upgma_tile_num == 2",
                          div(
                            class = "heatmap-scale",
                            uiOutput("upgma_fruit_variable2")
                          )
                        ),
                        conditionalPanel(
                          "input.upgma_tile_num == 3",
                          div(
                            class = "heatmap-scale",
                            uiOutput("upgma_fruit_variable3")
                          )
                        ),
                        conditionalPanel(
                          "input.upgma_tile_num == 4",
                          div(
                            class = "heatmap-scale",
                            uiOutput("upgma_fruit_variable4")
                          )
                        ),
                        conditionalPanel(
                          "input.upgma_tile_num == 5",
                          div(
                            class = "heatmap-scale",
                            uiOutput("upgma_fruit_variable5")
                          )
                        )
                      ),
                      conditionalPanel(
                        "input.upgma_tile_num == 1",
                        column(
                          width = 3,
                          align = "center",
                          div(
                            class = "heatmap-scale",
                            uiOutput("upgma_tiles_scale_1")
                          )
                        )
                      ),
                      conditionalPanel(
                        "input.upgma_tile_num == 2",
                        column(
                          width = 3,
                          align = "center",
                          div(
                            class = "heatmap-scale",
                            uiOutput("upgma_tiles_scale_2")
                          )
                        )
                      ),
                      conditionalPanel(
                        "input.upgma_tile_num == 3",
                        column(
                          width = 3,
                          align = "center",
                          div(
                            class = "heatmap-scale",
                            uiOutput("upgma_tiles_scale_3")
                          )
                        )
                      ),
                      conditionalPanel(
                        "input.upgma_tile_num == 4",
                        column(
                          width = 3,
                          align = "center",
                          div(
                            class = "heatmap-scale",
                            uiOutput("upgma_tiles_scale_4")
                          )
                        )
                      ),
                      conditionalPanel(
                        "input.upgma_tile_num == 5",
                        column(
                          width = 3,
                          align = "center",
                          div(
                            class = "heatmap-scale",
                            uiOutput("upgma_tiles_scale_5")
                          )
                        )
                      ),
                      uiOutput("upgma_fruit_mapping_info")
                    ),
                    fluidRow(
                      column(
                        width = 3,
                        div(
                          class = "mat-switch-v",
                          materialSwitch(
                            "upgma_heatmap_show",
                            h5(p("Heatmap"), style = "color:white; position: relative; top: -4px; right: -5px;"),
                            value = FALSE,
                            right = TRUE
                          )
                        )
                      ),
                      column(
                        width = 3,
                        align = "center",
                        uiOutput("upgma_heatmap_sel")
                      ),
                      column(
                        width = 3,
                        align = "center",
                        div(
                          class = "heatmap-scale",
                          uiOutput("upgma_heatmap_scale")
                        )
                      ),
                      uiOutput("upgma_heatmap_mapping_info")
                    )
                  )
                )
              )
            )
          ),
          br(), br(), br(), br(), br(), br()
        )
      )
    ) # End tabItems
  ) # End dashboardPage
) # end UI

# _______________________ ####

# Server ----

server <- function(input, output, session) {
  
  phylotraceVersion <- paste("PhyloTrace-1.1.1", Sys.Date())
  
  # Kill server on session end
  session$onSessionEnded( function() {
    stopApp()
  })
  
  # Disable MST variable mappings
  shinyjs::disable('mst_edge_label') 
  shinyjs::disable('mst_color_var') 
  
  ## Functions ----
  
  # Modified gheatmap function
  gheatmap.mod <- function(p, data, offset=0, width=1, low="green", high="red", color="white",
                           colnames=TRUE, colnames_position="bottom", colnames_angle=0, colnames_level=NULL,
                           colnames_offset_x = 0, colnames_offset_y = 0, font.size=4, family="", hjust=0.5, legend_title = "value",
                           colnames_color = "black") {
    
    colnames_position %<>% match.arg(c("bottom", "top"))
    variable <- value <- lab <- y <- NULL
    
    ## if (is.null(width)) {
    ##     width <- (p$data$x %>% range %>% diff)/30
    ## }
    
    ## convert width to width of each cell
    width <- width * (p$data$x %>% range(na.rm=TRUE) %>% diff) / ncol(data)
    
    isTip <- x <- y <- variable <- value <- from <- to <- NULL
    
    ## handle the display of heatmap on collapsed nodes
    ## https://github.com/GuangchuangYu/ggtree/issues/242
    ## extract data on leaves (& on collapsed internal nodes) 
    ## (the latter is extracted only when the input data has data on collapsed
    ## internal nodes)
    df <- p$data
    nodeCo <- intersect(df %>% filter(is.na(x)) %>% 
                          select(.data$parent, .data$node) %>% unlist(), 
                        df %>% filter(!is.na(x)) %>% 
                          select(.data$parent, .data$node) %>% unlist())
    labCo <- df %>% filter(.data$node %in% nodeCo) %>% 
      select(.data$label) %>% unlist()
    selCo <- intersect(labCo, rownames(data))
    isSel <- df$label %in% selCo
    
    df <- df[df$isTip | isSel, ]
    start <- max(df$x, na.rm=TRUE) + offset
    
    dd <- as.data.frame(data)
    ## dd$lab <- rownames(dd)
    i <- order(df$y)
    
    ## handle collapsed tree
    ## https://github.com/GuangchuangYu/ggtree/issues/137
    i <- i[!is.na(df$y[i])]
    
    lab <- df$label[i]
    ## dd <- dd[lab, , drop=FALSE]
    ## https://github.com/GuangchuangYu/ggtree/issues/182
    dd <- dd[match(lab, rownames(dd)), , drop = FALSE]
    
    
    dd$y <- sort(df$y)
    dd$lab <- lab
    ## dd <- melt(dd, id=c("lab", "y"))
    dd <- gather(dd, variable, value, -c(lab, y))
    
    i <- which(dd$value == "")
    if (length(i) > 0) {
      dd$value[i] <- NA
    }
    if (is.null(colnames_level)) {
      dd$variable <- factor(dd$variable, levels=colnames(data))
    } else {
      dd$variable <- factor(dd$variable, levels=colnames_level)
    }
    V2 <- start + as.numeric(dd$variable) * width
    mapping <- data.frame(from=dd$variable, to=V2)
    mapping <- unique(mapping)
    
    dd$x <- V2
    dd$width <- width
    dd[[".panel"]] <- factor("Tree")
    if (is.null(color)) {
      p2 <- p + geom_tile(data=dd, aes(x, y, fill=value), width=width, inherit.aes=FALSE)
    } else {
      p2 <- p + geom_tile(data=dd, aes(x, y, fill=value), width=width, color=color, inherit.aes=FALSE)
    }
    if (is(dd$value,"numeric")) {
      p2 <- p2 + scale_fill_gradient(low=low, high=high, na.value=NA, name = legend_title) # "white")
    } else {
      p2 <- p2 + scale_fill_discrete(na.value=NA, name = legend_title) #"white")
    }
    
    if (colnames) {
      if (colnames_position == "bottom") {
        y <- 0
      } else {
        y <- max(p$data$y) + 1
      }
      mapping$y <- y
      mapping[[".panel"]] <- factor("Tree")
      p2 <- p2 + geom_text(data=mapping, aes(x=to, y = y, label=from), color = colnames_color, size=font.size, family=family, inherit.aes = FALSE,
                           angle=colnames_angle, nudge_x=colnames_offset_x, nudge_y = colnames_offset_y, hjust=hjust)
    }
    
    p2 <- p2 + theme(legend.position="right")
    ## p2 <- p2 + guides(fill = guide_legend(override.aes = list(colour = NULL)))
    
    if (!colnames) {
      ## https://github.com/GuangchuangYu/ggtree/issues/204
      p2 <- p2 + scale_y_continuous(expand = c(0,0))
    }
    
    attr(p2, "mapping") <- mapping
    return(p2)
  }
  
  # Get rhandsontable
  get.entry.table.meta <- reactive({
    if(!is.null(hot_to_r(input$db_entries))){
      table <- hot_to_r(input$db_entries)
      select(table, 1:(12 + nrow(DB$cust_var)))
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
  
  # Functions to compute Hamming distance between two vectors
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
    if(DB$distancematrix_nrow > 33) {
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
        return("cont")
      } else if (class(x) == "character") {
        return("categ")
      } else {
        return(class(x))
      }
    })
  }
  
  # Function to check single typing log file
  check_new_entry <- reactive({
    
    invalidateLater(5000, session)
    
    if(!is.null(DB$database)) {
      if(file_exists(paste0(
        DB$database, "/",
        gsub(" ", "_", DB$scheme),
        "/Typing.rds"
      ))) {
        
        Database <- readRDS(paste0(DB$database, "/",gsub(" ", "_", DB$scheme),"/Typing.rds"))
        
        if(is.null(DB$data)) {
          if(nrow(Database[["Typing"]]) == 1) {
            TRUE
          } else {FALSE}
        } else {
          if(nrow(DB$data) < nrow(Database[["Typing"]])) {
            TRUE
          } else {
            FALSE
          }
        }
      } else {FALSE}
    }
  })
  
  # Render Entry Table Highlights 
  
  diff_allele <- reactive({
    if (!is.null(DB$data) & !is.null(input$compare_select) & !is.null(DB$cust_var)) {
      var_alleles(select(DB$data, input$compare_select)) + (12 + nrow(DB$cust_var))
    }
  })
  
  true_rows <- reactive({
    if (!is.null(DB$data)) {
      which(DB$data$Include == TRUE, )
    }
  })
  
  duplicated_rows <- reactive({
    if (!is.null(DB$meta)) {
      which(duplicated(DB$meta$`Assembly Name`) | duplicated(DB$meta$`Assembly Name`, fromLast = TRUE))
    }
  })
  
  # _______________________ ####
  
  ## Startup ----
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  shinyjs::removeClass(selector = "body", class = "sidebar-toggle")
  
  output$messageMenu <- renderText({
    HTML(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  })
  
  # Declare reactive variables
  Startup <- reactiveValues(sidebar = TRUE, 
                            header = TRUE) # reactive variables related to startup process
  
  DB <- reactiveValues(data = NULL, 
                       block_db = FALSE, 
                       load_selected = TRUE,
                       no_na_switch = FALSE,
                       first_look = FALSE) # reactiive variables related to local database
  
  Typing <- reactiveValues(table = data.frame(), 
                           single_path = data.frame(),
                           progress = 0, 
                           progress_format_start = 0, 
                           progress_format_end = 0,
                           result_list = NULL) # reactive variables related to typing process
  
  Vis <- reactiveValues(cluster = NULL, 
                        metadata = list(),
                        custom_label_nj = data.frame(),
                        nj_label_pos_y = list(),
                        nj_label_pos_x = list(),
                        nj_label_size = list(),
                        custom_label_upgma = data.frame(),
                        upgma_label_pos_y = list(),
                        upgma_label_pos_x = list(),
                        upgma_label_size = list()) # reactive variables related to visualization
  
  Report <- reactiveValues() # reactive variables related to report functions
  
  Scheme <- reactiveValues() # reactive variables related to scheme  functions
  
  # Load last used database if possible
  if(paste0(getwd(), "/execute/last_db.rds") %in% dir_ls(paste0(getwd(), "/execute"))) {
    DB$last_db <- TRUE
  }
  
  # Locate local Database
  observe({
    shinyDirChoose(input,
                   "db_location",
                   roots = c(home = path_home()),
                   session = session)
    
    if(!is.null(DB$select_new)) {
      if(DB$select_new == FALSE) {
        if(DB$block_db == FALSE) {
          DB$database <- as.character(
            parseDirPath(
              roots = c(home = path_home()),
              input$db_location
            )
          )
          
          DB$exist <- (length(dir_ls(DB$database)) == 0)  # Logical any local database present
          
          DB$available <- gsub("_", " ", basename(dir_ls(DB$database))) # List of local schemes available
        }
        
      } else if (DB$select_new ==  TRUE) {
        DB$database <- paste0(DB$new_database, "/Database")
        
      }
    } else {
      if(!is.null(DB$last_db) & file.exists(paste0(getwd(), "/execute/last_db.rds"))) {
        
        DB$database <- readRDS(paste0(getwd(), "/execute/last_db.rds")) 
        
        if(dir_exists(DB$database)) {
          DB$exist <- (length(dir_ls(DB$database)) == 0)  # Logical any local database present
          
          DB$available <- gsub("_", " ", basename(dir_ls(DB$database))) # List of local schemes available
        }
      }
    }
  })
  
  ### Set up typing environment ----
  
  # Null typing progress trackers
  writeLines("0", paste0(getwd(), "/execute/script_log.txt"))
  
  if(dir_exists(paste0(getwd(), "/execute/blat_single/results"))) {
    unlink(list.files(paste0(getwd(), "/execute/blat_single/results"), full.names = TRUE), recursive = TRUE)
    # Resetting single typing progress logfile bar 
    con <- file(paste0(getwd(), "/execute/progress.txt"), open = "w")
    
    cat("0\n", file = con)   
    close(con)
  }
  
  # Reset typing feedback values
  Typing$pending <- FALSE
  Typing$multi_started <- FALSE
  Typing$multi_help <- FALSE
  saveRDS(list(), paste0(getwd(), "/execute/event_list.rds"))
  Typing$last_success <- "0" # Null last multi typing success name
  Typing$last_failure <- "0" # Null last multi typing failure name
  
  ### Landing page UI ----
  observe({
    if (Startup$sidebar == FALSE) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      shinyjs::addClass(selector = "body", class = "sidebar-toggle")
    }
  })
  
  output$start_message <- renderUI({
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
            buttonType = "default",
            root = path_home()
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
            buttonType = "default",
            root = path_home()
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
  })
  
  # User selection new db or load db
  observeEvent(input$create_new_db, {
    DB$select_new <- TRUE
  })
  
  observeEvent(input$db_location, {
    DB$select_new <- FALSE
  })
  
  # Load db & scheme selection UI
  output$load_db <- renderUI({
    if(!is.null(DB$select_new)) {
      if(length(DB$new_database) > 0 & DB$select_new) {
        column(
          width = 12,
          p(
            tags$span(
              style='color: white; font-size: 15px;',
              HTML(
                paste(
                  'New database will be created in',
                  DB$new_database
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
      } else if(length(DB$available) > 0 & !(DB$select_new)) {
        if(any(!(gsub(" ", "_", DB$available) %in% schemes))) {
          column(
            width = 12,
            p(
              tags$span(
                style='color: white; font-size: 15px; font-style: italic;',
                HTML(
                  paste('Selected:', DB$database)
                )
              )
            ),
            uiOutput("scheme_db"),
            br(), 
            p(
              HTML(
                paste(
                  tags$span(style='color: #E18B00; font-size: 13px; font-style: italic;', 
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
                  paste('Selected:', DB$database)
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
    } else if((!is.null(DB$last_db)) & (!is.null(DB$available))) {
      if (DB$last_db == TRUE & (length(DB$available) > 0)) {
        if(any(!(gsub(" ", "_", DB$available) %in% schemes))) {
          column(
            width = 12,
            p(
              tags$span(
                style='color: white; font-size: 15px; font-style: italic;',
                HTML(
                  paste('Last used:', DB$database)
                )
              )
            ),
            uiOutput("scheme_db"),
            br(), 
            p(
              HTML(
                paste(
                  tags$span(style='color: #E18B00; font-size: 13px; font-style: italic;', 
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
                  paste('Last used:', DB$database)
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
      } else if (DB$last_db == TRUE & (length(DB$available) == 0)) {
        column(
          width = 12,
          p(
            tags$span(
              style='color: white; font-size: 15px; font-style: italic;',
              HTML(
                paste('Last used:', DB$database)
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
  
  output$imageOutput <- renderImage({
    # Path to your PNG image with a transparent background
    image_path <- paste0(getwd(), "/www/PhyloTrace.png")
    
    # Use HTML to display the image with the <img> tag
    list(src = image_path,
         height = 180)
  }, deleteFile = FALSE)
  
  ### Load app event ----
  
  observeEvent(input$load, {
    
    # Load app elements based on database availability and missing value presence
    if(!is.null(DB$select_new)) {
      if(DB$select_new & (paste0(DB$new_database, "/Database") %in% dir_ls(DB$new_database))) {
        show_toast(
          title = "Directory already contains a database",
          type = "error",
          width = "500px",
          position = "top-end",
          timer = 6000
        )
        DB$load_selected <- FALSE
        
      } else if(DB$select_new | (DB$select_new == FALSE & is.null(input$scheme_db))) {
        
        DB$check_new_entries <- TRUE
        
        DB$data <- NULL
        
        DB$meta <- NULL
        
        DB$meta_true <- NULL
        
        DB$allelic_profile <- NULL
        
        DB$allelic_profile_true <- NULL
        
        DB$scheme <- input$scheme_db
        
        # null Distance matrix, entry table and plots
        output$db_distancematrix <- NULL 
        output$db_entries_table <- NULL
        output$tree_mst <- NULL
        output$tree_nj <- NULL
        output$tree_upgma <- NULL
        
        # null report values
        Report$report_list_mst <- list()
        Report$report_list_nj <- list()
        Report$report_list_upgma <- list()
        
        # null plots
        Vis$nj <- NULL
        Vis$upgma <- NULL
        Vis$ggraph_1 <- NULL
        
        removeModal()
        
        #### Render Menu Items ----
        
        Startup$sidebar <- FALSE
        Startup$header <- FALSE
        
        output$menu_sep1 <- renderUI(hr())
        output$menu_sep2 <- renderUI(hr())
        
        # Hide start message
        output$start_message <- NULL
        
        DB$load_selected <- FALSE
        
        # Declare database path
        DB$database <- paste0(DB$new_database, "/Database")
        
        # Set database availability screening variables to present database
        DB$block_db <- TRUE
        DB$select_new <- FALSE
        
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
    
    if(DB$load_selected == TRUE) {
      
      if(gsub(" ", "_", input$scheme_db) %in% schemes) { #Check if selected scheme valid
        
        # Save database path for next start
        saveRDS(DB$database, paste0(getwd(), "/execute/last_db.rds"))
        
        DB$check_new_entries <- TRUE
        
        DB$data <- NULL
        
        DB$meta <- NULL
        
        DB$meta_true <- NULL
        
        DB$allelic_profile <- NULL
        
        DB$allelic_profile_true <- NULL
        
        DB$scheme <- input$scheme_db
        
        # null Distance matrix, entry table and plots
        output$db_distancematrix <- NULL 
        output$db_entries_table <- NULL
        output$tree_mst <- NULL
        output$tree_nj <- NULL
        output$tree_upgma <- NULL
        
        # null report values
        Report$report_list_mst <- list()
        Report$report_list_nj <- list()
        Report$report_list_upgma <- list()
        
        # null plots
        Vis$nj <- NULL
        Vis$upgma <- NULL
        Vis$ggraph_1 <- NULL
        
        removeModal()
        
        #### Render Menu Items ----
        
        Startup$sidebar <- FALSE
        Startup$header <- FALSE
        
        output$menu_sep1 <- renderUI(hr())
        output$menu_sep2 <- renderUI(hr())
        
        # Hide start message
        output$start_message <- NULL
        
        if(any(grepl(gsub(" ", "_", DB$scheme), dir_ls(DB$database)))) {
          
          if(!any(grepl("alleles", dir_ls(paste0(
            DB$database, "/", 
            gsub(" ", "_", DB$scheme)))))) {
            
            # Show message that loci files are missing
            showModal(
              modalDialog(
                paste0("Whoops! No loci files are present in the local ", 
                       DB$scheme, 
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
            DB$database, "/", 
            gsub(" ", "_", DB$scheme)))))) {
            
            output$download_scheme_info <- NULL
            
            # Show message that scheme info is missing
            showModal(
              modalDialog(
                paste0("Whoops! Scheme info of the local ", 
                       DB$scheme, 
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
            DB$database, "/", 
            gsub(" ", "_", DB$scheme)))))) {
            
            # Dont render target download button
            output$download_loci <- NULL
            
            # Show message that scheme info is missing
            showModal(
              modalDialog(
                paste0("Whoops! Loci info of the local ", 
                       DB$scheme, 
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
                          tags$span(style='color: white; font-size: 13px; font-style: italic', DB$scheme)
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
                DB$database, "/",
                gsub(" ", "_", DB$scheme),
                "/scheme_info.html"
              )) %>%
              html_table(header = FALSE) %>%
              as.data.frame(stringsAsFactors = FALSE)
            names(schemeinfo) <- NULL
            DB$schemeinfo <- schemeinfo
            number_loci <- as.vector(DB$schemeinfo[6, 2])
            number_loci <- as.numeric(gsub(",", "", number_loci))
            
            # Produce Loci Info table
            DB$loci_info <- read.csv(
              paste0(
                DB$database, "/",
                gsub(" ", "_", DB$scheme),
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
            if(number_loci != length(dir_ls(paste0(DB$database, "/", gsub(" ", "_", DB$scheme), "/", gsub(" ", "_", DB$scheme), "_alleles")))) {
              
              # Show message that loci files are missing
              showModal(
                modalDialog(
                  paste0("Whoops! Some loci files are missing in the local ", 
                         DB$scheme, 
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
                DB$database, "/", gsub(" ", "_", DB$scheme)
              ))))) {
                
                # Load database from files  
                Database <-
                  readRDS(paste0(
                    DB$database, "/",
                    gsub(" ", "_", DB$scheme),
                    "/Typing.rds"
                  ))
                
                DB$data <- Database[["Typing"]]
                
                if(!is.null(DB$data)){
                  if ((ncol(DB$data)-12) != as.numeric(gsub(",", "", as.vector(DB$schemeinfo[6, 2])))) {
                    cust_var <- select(DB$data, 13:(ncol(DB$data) - as.numeric(gsub(",", "", as.vector(DB$schemeinfo[6, 2])))))
                    DB$cust_var <- data.frame(Variable = names(cust_var), Type = column_classes(cust_var))
                  } else {
                    DB$cust_var <- data.frame()
                  }
                }
                
                DB$change <- FALSE
                
                DB$meta <- select(DB$data, 1:(12 + nrow(DB$cust_var)))
                
                DB$meta_true <- DB$meta[which(DB$data$Include == TRUE),]
                
                DB$allelic_profile <- select(DB$data, -(1:(12 + nrow(DB$cust_var))))
                
                DB$allelic_profile_true <- DB$allelic_profile[which(DB$data$Include == TRUE),]
                
                # Null pipe 
                con <- file(paste0(getwd(), "/execute/progress.txt"), open = "w")
                
                cat("0\n", file = con)
                
                # Close the file connection
                close(con)
                
                # Reset other reactive typing variables
                Typing$progress_format_end <- 0 
                
                Typing$progress_format_start <- 0
                
                Typing$pending_format <- 0
                
                Typing$entry_added <- 0
                
                Typing$progress <- 0
                
                Typing$progress_format <- 900000
                
                output$single_typing_progress <- NULL
                
                output$typing_fin <- NULL
                
                output$single_typing_results <- NULL
                
                output$typing_formatting <- NULL
                
                Typing$single_path <- data.frame()
                
                # Null multi typing feedback variable
                Typing$reset <- TRUE
                
                # Check need for new missing vlaue display
                if(DB$first_look == TRUE) {
                  if(sum(apply(DB$data, 1, anyNA)) >= 1) {
                    DB$no_na_switch <- TRUE
                  } else {
                    DB$no_na_switch <- FALSE
                  }
                }
                
                DB$first_look <- TRUE
                
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
                      class = NULL,
                      filetypes = c('fasta', 'fna', 'fa'),
                      root = path_home()
                    ),
                    br(),
                    br(),
                    br(),
                    uiOutput("genome_path")
                  )
                })
                
                if(!anyNA(DB$allelic_profile)) {
                  
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
                  width = "100%", 
                  {
                    if((!is.null(DB$cust_var)) & (!is.null(input$cust_var_select))) {
                      if(nrow(DB$cust_var) > 5) {
                        low <- -4
                        high <- 0
                        for (i in 1:input$cust_var_select) {
                          low <- low + 5
                          if((nrow(DB$cust_var) %% 5) != 0) {
                            if(i == ceiling(nrow(DB$cust_var) / 5 )) {
                              high <- high + nrow(DB$cust_var) %% 5
                            } else {
                              high <- high + 5
                            }
                          } else {
                            high <- high + 5
                          }
                        }
                        DB$cust_var[low:high,]
                      } else {
                        DB$cust_var
                      }
                    }
                  })
                
                # render visualization sidebar elements
                observe({
                  Vis$tree_algo <- input$tree_algo
                })
                output$visualization_sidebar <- renderUI({
                  if(!is.null(DB$data)) {
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
                            label = "",
                            selected = if(!is.null(Vis$tree_algo)){Vis$tree_algo} else {"Minimum-Spanning"}
                          ),
                        )
                      ),
                      br(),
                      fluidRow(
                        column(
                          width = 12,
                          align = "center",
                          tags$div(
                            id = "button-wrapper",
                            actionButton(
                              "create_tree",
                              h5("Create Tree", style = "position: relative; left: 15px; color: white; font-size: 15px;"),
                              width = "100%"
                            ),
                            tags$img(
                              src = "phylo.png",
                              alt = "icon",
                              class = "icon"
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
                            sliderInput(
                              "mst_scale",
                              "",
                              min = 500,
                              max = 1200,
                              value = 800,
                              width = "95%",
                              ticks = FALSE
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
                                choices = c("png", "jpeg", "bmp", "svg")
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
                                choices = c("png", "jpeg", "bmp", "svg")
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
                  if(!is.null(DB$data)) {
                    column(
                      width = 12,
                      align = "center",
                      br(), 
                      fluidRow(
                        column(1),
                        column(
                          width = 10,
                          align = "left",
                          if(nrow(DB$data) > 40) {
                            div(
                              class = "mat-switch-db-tab",
                              materialSwitch(
                                "table_height",
                                h5(p("Show Full Table"), style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"),
                                value = FALSE,
                                right = TRUE
                              )
                            )
                          }
                        )
                      ),
                      br(), br(), 
                      fluidRow(
                        column(
                          width = 12,
                          HTML(
                            paste(
                              tags$span(style='color: white; font-size: 18px; margin-bottom: 0px', 'Custom Variables')
                            )
                          )
                        )
                      ),
                      fluidRow(
                        column(
                          width = 8,
                          textInput(
                            "new_var_name",
                            label = "",
                            placeholder = "New Variable"
                          )
                        ),
                        column(
                          width = 2,
                          actionButton(
                            "add_new_variable",
                            "",
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
                              DB$cust_var$Variable
                            )
                          )
                        ),
                        column(
                          width = 2,
                          align = "left",
                          actionButton(
                            "delete_new_variable",
                            "",
                            icon = icon("minus")
                          )
                        )
                      ),
                      br(), 
                      fluidRow(
                        column(1),
                        column(
                          width = 4,
                          uiOutput("cust_var_info")
                        )
                      ),
                      fluidRow(
                        column(1),
                        column(
                          width = 11,
                          align = "center",
                          tableOutput("show_cust_var")
                        )
                      ),
                      fluidRow(
                        column(4),
                        column(
                          width = 7,
                          align = "center",
                          uiOutput("cust_var_select")
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
                              tags$span(style='color: white; font-size: 13px; font-style: italic', DB$scheme)
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
                        br(),
                        materialSwitch(
                          "miss_val_height",
                          h5(p("Show Full Table"), style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"),
                          value = FALSE,
                          right = TRUE
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
                    div(
                      class = "mat-switch-dmatrix",
                      materialSwitch(
                        "distmatrix_true",
                        h5(p("Only Included Entries"), style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"),
                        value = FALSE,
                        right = TRUE
                      )
                    ),
                    div(
                      class = "mat-switch-dmatrix",
                      materialSwitch(
                        "distmatrix_triangle",
                        h5(p("Show Upper Triangle"), style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"),
                        value = FALSE,
                        right = TRUE
                      )
                    ),
                    div(
                      class = "mat-switch-dmatrix-last",
                      materialSwitch(
                        "distmatrix_diag",
                        h5(p("Show Diagonal"), style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"),
                        value = TRUE,
                        right = TRUE
                      )
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
                  
                  if(nrow(DB$data) == 1) {
                    HTML(
                      paste(
                        tags$span(style='color: white; font-size: 15px;', "Type at least two assemblies to compare")
                      )
                    )
                  } else {
                    if(!is.null(input$compare_difference)) {
                      if (input$compare_difference == FALSE) {
                        DB$allelic_profile
                        pickerInput(
                          inputId = "compare_select",
                          label = "",
                          width = "85%",
                          choices = names(DB$allelic_profile),
                          selected = names(DB$allelic_profile)[1:20],
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
                          choices = names(DB$allelic_profile),
                          selected = names(DB$allelic_profile)[var_alleles(DB$allelic_profile)],
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
                  if(!is.null(DB$data)) {
                    if(between(nrow(DB$data), 1, 30)) {
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
                
                if (!is.null(DB$data)) {
                  
                  observe({
                    
                    if (!is.null(DB$data)) {
                      if (nrow(DB$data) == 1) {
                        if(!is.null(DB$data) & !is.null(DB$cust_var)) {
                          output$db_entries <- renderRHandsontable({
                            rhandsontable(
                              select(DB$data, 1:(12 + nrow(DB$cust_var))),
                              rowHeaders = NULL
                            ) %>%
                              hot_col(1, 
                                      readOnly = TRUE,
                                      valign = "htMiddle",
                                      halign = "htCenter") %>%
                              hot_col(3:(12 + nrow(DB$cust_var)), 
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
                      } else if (between(nrow(DB$data), 1, 40)) {
                        if (length(input$compare_select) > 0) {
                          if(!is.null(DB$data) & !is.null(DB$cust_var) & !is.null(input$compare_select)) {
                            output$db_entries <- renderRHandsontable({
                              row_highlight <- true_rows()-1
                              rhandsontable(
                                select(DB$data, 1:(12 + nrow(DB$cust_var)), input$compare_select),
                                col_highlight = diff_allele()-1,
                                rowHeaders = NULL,
                                duplicated_highlight = duplicated_rows()-1,
                                row_highlight = row_highlight
                              ) %>%
                                hot_col((12 + nrow(DB$cust_var)):((12 + nrow(DB$cust_var))+length(input$compare_select)), 
                                        valign = "htMiddle",
                                        halign = "htCenter") %>%
                                hot_col(1, 
                                        readOnly = TRUE,
                                        valign = "htMiddle",
                                        halign = "htCenter") %>%
                                hot_col(3:(12 + nrow(DB$cust_var)), 
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
                          if(!is.null(DB$data) & !is.null(DB$cust_var)) {
                            output$db_entries <- renderRHandsontable({
                              row_highlight <- true_rows()-1
                              rhandsontable(
                                select(DB$data, 1:(12 + nrow(DB$cust_var))),
                                rowHeaders = NULL,
                                row_highlight = row_highlight,
                                duplicated_highlight = duplicated_rows()-1
                              ) %>%
                                hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
                                hot_col(1, 
                                        readOnly = TRUE,
                                        valign = "htMiddle",
                                        halign = "htCenter") %>%
                                hot_col(3:(12 + nrow(DB$cust_var)), 
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
                          if(!is.null(DB$data) & !is.null(DB$cust_var) & !is.null(input$table_height) & !is.null(input$compare_select)) {
                            output$db_entries <- renderRHandsontable({
                              rhandsontable(
                                select(DB$data, 1:(12 + nrow(DB$cust_var)), input$compare_select),
                                col_highlight = diff_allele()-1,
                                rowHeaders = NULL,
                                height = table_height(),
                                row_highlight = true_rows()-1,
                                duplicated_highlight = duplicated_rows()-1
                              ) %>%
                                hot_col((12 + nrow(DB$cust_var)):((12 + nrow(DB$cust_var))+length(input$compare_select)), 
                                        valign = "htMiddle",
                                        halign = "htCenter") %>%
                                hot_col(3:(12 + nrow(DB$cust_var)), 
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
                          if(!is.null(DB$data) & !is.null(DB$cust_var) & !is.null(input$table_height)) {
                            output$db_entries <- renderRHandsontable({
                              rhandsontable(
                                select(DB$data, 1:(12 + nrow(DB$cust_var))),
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
                                hot_col(3:(12 + nrow(DB$cust_var)), 
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
                      if(check_new_entry() & DB$check_new_entries) {
                        fluidRow(
                          column(
                            width = 8,
                            align = "left",
                            HTML(
                              paste(
                                tags$span(style='color: white; font-size: 14px; position: absolute; bottom: -30px; right: -5px', 
                                          'New entries - reload database')
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
                      } else if(Typing$status == "Attaching") {
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
                      } else if((DB$change == TRUE) | !identical(get.entry.table.meta(), DB$meta)) {
                        if(!is.null(input$db_entries)) {
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
                        }
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
                    column(1),
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
                  if(!is.null(DB$data)) {
                    
                    if(any(duplicated(DB$meta$`Assembly Name`))) {
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
                                      paste0("# ", which(duplicated(DB$meta$`Assembly Name`)), " - "),
                                      DB$meta$`Assembly Name`[which(duplicated(DB$meta$`Assembly Name`))]
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
                      if(!is.null(DB$data) & !is.null(DB$allelic_profile) & !is.null(DB$allelic_profile_true) & !is.null(DB$cust_var) & !is.null(input$distmatrix_label) & !is.null(input$distmatrix_diag) & !is.null(input$distmatrix_triangle)) {
                        output$db_distancematrix <- renderRHandsontable({
                          rhandsontable(hamming_df(), digits = 1, 
                                        height = distancematrix_height(), rowHeaders = NULL) %>%
                            hot_heatmap(renderer = paste0("function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    heatmapScale  = chroma.scale(['#17F556', '#ED6D47']);
  
    if (instance.heatmap[col]) {
      mn = ", DB$matrix_min, ";
      mx = ", DB$matrix_max, ";
  
      pt = (parseInt(value, 10) - mn) / (mx - mn);    
  
      td.style.backgroundColor = heatmapScale(pt).hex();
    }
  }
  ")) %>%
                            hot_rows(fixedRowsTop = 0) %>%
                            hot_cols(fixedColumnsLeft = 1) %>%
                            hot_col(1:(dim(DB$ham_matrix)[1]+1),
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
                      if(!is.null(DB$data)) {
                        if(nrow(DB$data) > 1) {
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
                      column(2),
                      column(
                        width = 10,
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
                        div(
                          class = "mat-switch-db",
                          materialSwitch(
                            "download_table_include",
                            h5(p("Only Included Entries"), style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"),
                            value = FALSE,
                            right = TRUE
                          )
                        ),
                        div(
                          class = "mat-switch-db",
                          materialSwitch(
                            "download_table_loci",
                            h5(p("Include Displayed Loci"), style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"),
                            value = FALSE,
                            right = TRUE
                          )
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
                              choices = DB$data[, "Index"],
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
                NA_table <- DB$allelic_profile[, colSums(is.na(DB$allelic_profile)) != 0]
                
                NA_table <- NA_table[rowSums(is.na(NA_table)) != 0,]
                
                NA_table[is.na(NA_table)] <- "NA"
                
                NA_table <- NA_table %>% 
                  cbind("Assembly Name" = DB$meta[rownames(NA_table),]$`Assembly Name`) %>%
                  cbind("Errors" = DB$meta[rownames(NA_table),]$Errors) %>%
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
                                   strong(as.character(sum(is.na(DB$data)))), 
                                   " unsuccessful allele allocations (NA). ",
                                   strong(sum(sapply(DB$allelic_profile, anyNA))),
                                   " out of ",
                                   strong(ncol(DB$allelic_profile)),
                                   " total loci in this scheme contain NA's (",
                                   strong(round((sum(sapply(DB$allelic_profile, anyNA)) / ncol(DB$allelic_profile) * 100), 1)),
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
                
                DB$data <- NULL
                
                DB$meta <- NULL
                
                DB$meta_true <- NULL
                
                DB$allelic_profile <- NULL
                
                DB$allelic_profile_true <- NULL
                
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
                  if(is.null(DB$data)) {
                    if(check_new_entry()) {
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
                      if(is.null(Typing$entry_added)) {
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
                        if(Typing$entry_added == 999999) {
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
  
  # _______________________ ####
  
  ## Database ----
  
  ### Conditional UI Elements rendering ----
  
  # Contro custom variables table
  output$cust_var_select <- renderUI({
    if(nrow(DB$cust_var) > 5) {
      selectInput(
        "cust_var_select",
        "",
        choices = 1:ceiling(nrow(DB$cust_var) / 5 )
      )
    }
  })
  
  output$cust_var_info <- renderUI({
    if((!is.null(DB$cust_var)) & (!is.null(input$cust_var_select))) {
      if(nrow(DB$cust_var) > 5) {
        low <- -4
        high <- 0
        for (i in 1:input$cust_var_select) {
          low <- low + 5
          if((nrow(DB$cust_var) %% 5) != 0) {
            if(i == ceiling(nrow(DB$cust_var) / 5 )) {
              high <- high + nrow(DB$cust_var) %% 5
            } else {
              high <- high + 5
            }
          } else {
            high <- high + 5
          }
        }
        h5(paste0("Showing ", low, " to ", high," of ", nrow(DB$cust_var), " variables"), style = "color: white; font-size: 10px;")
      }
    }
  })
  
  # Message on Database tabs if no scheme available yet
  observe({
    if(!is.null(DB$exist)) {
      if(DB$exist){
        
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
    if(!is.null(DB$allelic_profile)) {
      if(anyNA(DB$allelic_profile)) {
        if(DB$no_na_switch == FALSE) {
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
    
    if (!is.null(DB$available)) {
      output$scheme_db <- renderUI({
        if (length(DB$available) > 5) {
          selectInput(
            "scheme_db",
            label = "",
            choices = if(!is.null(Typing$last_scheme)) {
              Typing$last_scheme
            } else {DB$available},
            selected = if(!is.null(Typing$last_scheme)) {
              Typing$last_scheme
            } else {if(!is.null(DB$scheme)) {DB$scheme} else {DB$available[1]}}
          )
        } else {
          prettyRadioButtons(
            "scheme_db",
            label = "",
            choices = if(!is.null(Typing$last_scheme)) {
              Typing$last_scheme
            } else {DB$available},
            selected = if(!is.null(Typing$last_scheme)) {
              Typing$last_scheme
            } else {if(!is.null(DB$scheme)) {DB$scheme} else {DB$available[1]}}
          )
        }
      })
      
      if (!is.null(DB$schemeinfo)) {
        
        output$scheme_info <- renderTable({
          DB$schemeinfo
        })
        
        output$scheme_header <- renderUI(h3(p("cgMLST Scheme"), style = "color:white"))
        
      } else {
        
        output$scheme_info <- NULL
        output$scheme_header <- NULL
        
      }
      
      if (!is.null(DB$loci_info)) {
        output$db_loci <- renderDataTable(
          DB$loci_info,
          options = list(pageLength = 10,
                         columnDefs = list(list(searchable = FALSE,
                                                targets = "_all")
                         )))
        
        output$loci_header <- renderUI(h3(p("Loci"), style = "color:white"))
        
      } else {
        output$db_loci <- NULL
        output$loci_header <- NULL
      }
    } 
  })
  
  # If only one entry available disable varying loci checkbox
  
  output$compare_difference_box <- renderUI({
    if(!is.null(DB$data)) {
      if(nrow(DB$data) > 1) {
        div(
          class = "mat-switch-db",
          materialSwitch(
            "compare_difference",
            h5(p("Only Varying Loci"), style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"),
            value = FALSE,
            right = TRUE
          )
        )
      }
    }
  })
  
  ### Database Events ----
  
  # Change scheme
  observeEvent(input$reload_db, {
    
    if(tail(readLines(paste0(getwd(), "/execute/script_log.txt")), 1)!= "0") {
      show_toast(
        title = "Pending Multi Typing",
        type = "warning",
        position = "top-end",
        timer = 6000,
        width = "500px"
      )
    } else if(readLines(paste0(getwd(), "/execute", "/progress.txt"))[1] != "0") {
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
            choices = DB$available,
            selected = DB$scheme),
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
  
  # Create new database
  observe({
    shinyDirChoose(input,
                   "create_new_db",
                   roots = c(home = path_home()),
                   session = session)
    
    if(!is.null(input$create_new_db)) {
      DB$new_database <- as.character(
        parseDirPath(
          roots = c(home = path_home()), 
          input$create_new_db
        )
      )
    }
  })
  
  # Undo db changes
  observeEvent(input$undo_changes, {
    Data <- readRDS(paste0(
      DB$database, "/",
      gsub(" ", "_", DB$scheme),
      "/Typing.rds"
    ))
    
    DB$data <- Data[["Typing"]]
    
    if ((ncol(DB$data)-12) != as.numeric(gsub(",", "", as.vector(DB$schemeinfo[6, 2])))) {
      cust_var <- select(DB$data, 13:(ncol(DB$data) - as.numeric(gsub(",", "", as.vector(DB$schemeinfo[6, 2])))))
      DB$cust_var <- data.frame(Variable = names(cust_var), Type = column_classes(cust_var))
    } else {
      DB$cust_var <- data.frame()
    }
    
    DB$change <- FALSE
    
    DB$count <- 0
    
    DB$no_na_switch <- TRUE
    
    DB$meta <- select(DB$data, 1:(12 + nrow(DB$cust_var)))
    
    DB$meta_true <- DB$meta[which(DB$data$Include == TRUE),]
    
    DB$allelic_profile <- select(DB$data, -(1:(12 + nrow(DB$cust_var))))
    
    DB$allelic_profile_true <- DB$allelic_profile[which(DB$data$Include == TRUE),]
    
    DB$deleted_entries <- character(0)
    
    observe({
      if (!is.null(DB$data)) {
        if (nrow(DB$data) == 1) {
          output$db_entries <- renderRHandsontable({
            rhandsontable(
              select(DB$data, 1:(12 + nrow(DB$cust_var))),
              rowHeaders = NULL
            ) %>%
              hot_col(1, 
                      readOnly = TRUE,
                      valign = "htMiddle",
                      halign = "htCenter") %>%
              hot_col(3:(12 + nrow(DB$cust_var)), 
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
        } else if (between(nrow(DB$data), 1, 40)) {
          
          if (length(input$compare_select) > 0) {
            
            output$db_entries <- renderRHandsontable({
              row_highlight <- true_rows()-1
              rhandsontable(
                select(DB$data, 1:(12 + nrow(DB$cust_var)), input$compare_select),
                col_highlight = diff_allele()-1,
                rowHeaders = NULL,
                row_highlight = row_highlight
              ) %>%
                hot_col((12 + nrow(DB$cust_var)):((12 + nrow(DB$cust_var))+length(input$compare_select)), 
                        valign = "htMiddle",
                        halign = "htCenter") %>%
                hot_col(1, 
                        readOnly = TRUE,
                        valign = "htMiddle",
                        halign = "htCenter") %>%
                hot_col(3:(12 + nrow(DB$cust_var)), 
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
                    }")
            })
          } else {
            output$db_entries <- renderRHandsontable({
              row_highlight <- true_rows()-1
              rhandsontable(
                select(DB$data, 1:(12 + nrow(DB$cust_var))),
                rowHeaders = NULL,
                row_highlight = row_highlight
              ) %>%
                hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
                hot_col(1, 
                        readOnly = TRUE,
                        valign = "htMiddle",
                        halign = "htCenter") %>%
                hot_col(3:(12 + nrow(DB$cust_var)), 
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
                select(DB$data, 1:(12 + nrow(DB$cust_var)), input$compare_select),
                col_highlight = diff_allele()-1,
                rowHeaders = NULL,
                height = table_height(),
                row_highlight = true_rows()-1
              ) %>%
                hot_col((12 + nrow(DB$cust_var)):((12 + nrow(DB$cust_var))+length(input$compare_select)), 
                        valign = "htMiddle",
                        halign = "htCenter") %>%
                hot_col(3:(12 + nrow(DB$cust_var)), 
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
                select(DB$data, 1:(12 + nrow(DB$cust_var))),
                rowHeaders = NULL,
                height = table_height(),
                row_highlight = row_highlight
              ) %>%
                hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
                hot_col(1, 
                        readOnly = TRUE,
                        valign = "htMiddle",
                        halign = "htCenter") %>%
                hot_col(3:(12 + nrow(DB$cust_var)), 
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
    if(!is.null(DB$data)){
      if ((ncol(DB$data)-12) != as.numeric(gsub(",", "", as.vector(DB$schemeinfo[6, 2])))) {
        cust_var <- select(DB$data, 13:(ncol(DB$data) - as.numeric(gsub(",", "", as.vector(DB$schemeinfo[6, 2])))))
        DB$cust_var <- data.frame(Variable = names(cust_var), Type = column_classes(cust_var))
        
      } else {
        DB$cust_var <- data.frame()
      }
    }
  })
  
  DB$count <- 0
  
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
        if(trimws(input$new_var_name) %in% names(DB$meta)) {
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
              title = paste0("Select Data Type"),
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
    
    DB$count <- DB$count + 1
    
    DB$change <- TRUE
    
    name <- trimws(input$new_var_name)
    
    if(input$new_var_type == "Categorical (character)") {
      DB$data <- DB$data %>%
        mutate("{name}" := character(nrow(DB$data)), .after = 12)
      
      DB$cust_var <- rbind(DB$cust_var, data.frame(Variable = name, Type = "categ"))
    } else {
      DB$data <- DB$data %>%
        mutate("{name}" := numeric(nrow(DB$data)), .after = 12)
      
      DB$cust_var <- rbind(DB$cust_var, data.frame(Variable = name, Type = "cont"))
    }
    
    DB$meta <- select(DB$data, 1:(12 + nrow(DB$cust_var)))
    
    DB$meta_true <- DB$meta[which(DB$data$Include == TRUE),]
    
    DB$allelic_profile <- select(DB$data, -(1:(12 + nrow(DB$cust_var))))
    
    DB$allelic_profile_true <- DB$allelic_profile[which(DB$data$Include == TRUE),]
    
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
    DB$change <- TRUE
    
    removeModal()
    
    if(DB$count >= 1) {
      DB$count <- DB$count - 1
    } 
    
    show_toast(
      title = paste0("Variable ", input$del_which_var, " removed"),
      type = "warning",
      position = "top-end",
      width = "500px",
      timer = 6000
    )
    
    DB$cust_var <- DB$cust_var[-which(DB$cust_var$Variable == input$del_which_var),]
    DB$data <- select(DB$data, -(input$del_which_var))
    DB$meta <- select(DB$data, 1:(12 + nrow(DB$cust_var)))
    DB$meta_true <- DB$meta[which(DB$data$Include == TRUE),]
    
    DB$allelic_profile <- select(DB$data, -(1:(12 + nrow(DB$cust_var))))
    DB$allelic_profile_true <- DB$allelic_profile[which(DB$data$Include == TRUE),]
    
  })
  
  # Select all button
  
  observeEvent(input$sel_all_entries, {
    DB$data$Include <- TRUE
  })
  
  observeEvent(input$desel_all_entries, {
    DB$data$Include <- FALSE
  })
  
  # Switch to entry table
  
  observeEvent(input$change_entries, {
    removeModal()
    updateTabItems(session, "tabs", selected = "db_browse_entries")
  })
  
  #### Save Missing Value as CSV ----
  
  output$download_na_matrix <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_", gsub(" ", "_", DB$scheme), "_Missing_Values.csv")
    },
    content = function(file) {
      download_matrix <- hot_to_r(input$table_missing_values)
      write.csv(download_matrix, file, sep = ",", row.names=FALSE, quote=FALSE) 
    }
  )
  
  #### Save scheme info table as CSV ----
  
  output$download_schemeinfo <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", DB$scheme), "_scheme.csv")
    },
    content = function(file) {
      pub_index <- which(DB$schemeinfo[,1] == "Publications")
      write.table(
        DB$schemeinfo[1:(pub_index-1),],
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
      paste0(gsub(" ", "_", DB$scheme), "_Loci.csv")
    },
    content = function(file) {
      write.table(
        DB$loci_info,
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
      paste0(Sys.Date(), "_", gsub(" ", "_", DB$scheme), "_Entries.csv")
    },
    content = function(file) {
      download_matrix <- hot_to_r(input$db_entries)
      
      if (input$download_table_include == TRUE) {
        download_matrix <- download_matrix[which(download_matrix$Include == TRUE),]
      }
      
      if (input$download_table_loci == FALSE) {
        download_matrix <- select(download_matrix, 1:(12 + nrow(DB$cust_var)))
      } 
      
      write.csv(download_matrix, file, row.names=FALSE, quote=FALSE) 
    }
  )
  
  # Save Edits Button
  
  observeEvent(input$edit_button, {
    showModal(
      modalDialog(
        if(length(DB$deleted_entries > 0)) {
          paste0(
            "Overwriting previous metadata of local ",
            DB$scheme,
            " database. Deleted entries will be irreversibly removed. Continue?"
          )
        } else {
          paste0(
            "Overwriting previous metadata of local ",
            DB$scheme,
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
      DB$database, "/",
      gsub(" ", "_", DB$scheme),
      "/Typing.rds"
    ))
    
    if ((ncol(Data[["Typing"]])-12) != as.numeric(gsub(",", "", as.vector(DB$schemeinfo[6, 2])))) {
      cust_vars_pre <- select(Data[["Typing"]], 13:(ncol(Data[["Typing"]]) - as.numeric(gsub(",", "", as.vector(DB$schemeinfo[6, 2])))))
      cust_vars_pre <- names(cust_vars_pre)
    } else {
      cust_vars_pre <- character()
    }
    
    Data[["Typing"]] <- select(Data[["Typing"]], -(1:(12 + length(cust_vars_pre))))
    
    meta_hot <- hot_to_r(input$db_entries)
    
    if(length(DB$deleted_entries > 0)){
      
      meta_hot <- mutate(meta_hot, Index = as.character(1:nrow(DB$data)))
      
      Data[["Typing"]] <- mutate(Data[["Typing"]][-as.numeric(DB$deleted_entries),], meta_hot, .before = 1)
      rownames(Data[["Typing"]]) <- Data[["Typing"]]$Index
    } else {
      Data[["Typing"]] <- mutate(Data[["Typing"]], meta_hot, .before = 1)
      
    }
    
    # Ensure correct logical data type
    Data[["Typing"]][["Include"]] <- as.logical(Data[["Typing"]][["Include"]])
    
    saveRDS(Data, paste0(
      DB$database, "/",
      gsub(" ", "_", DB$scheme),
      "/Typing.rds"
    ))
    
    # Load database from files  
    Database <-
      readRDS(paste0(
        DB$database, "/",
        gsub(" ", "_", DB$scheme),
        "/Typing.rds"
      ))
    
    DB$data <- Database[["Typing"]]
    
    if(!is.null(DB$data)){
      if ((ncol(DB$data)-12) != as.numeric(gsub(",", "", as.vector(DB$schemeinfo[6, 2])))) {
        cust_var <- select(DB$data, 13:(ncol(DB$data) - as.numeric(gsub(",", "", as.vector(DB$schemeinfo[6, 2])))))
        DB$cust_var <- data.frame(Variable = names(cust_var), Type = column_classes(cust_var))
      } else {
        DB$cust_var <- data.frame()
      }
    }
    
    DB$change <- FALSE
    
    DB$count <- 0
    
    DB$no_na_switch <- TRUE
    
    DB$meta <- select(DB$data, 1:(12 + nrow(DB$cust_var)))
    
    DB$meta_true <- DB$meta[which(DB$data$Include == TRUE),]
    
    DB$allelic_profile <- select(DB$data, -(1:(12 + nrow(DB$cust_var))))
    
    DB$allelic_profile_true <- DB$allelic_profile[which(DB$data$Include == TRUE),]
    
    DB$deleted_entries <- character(0)
    
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
    } else if((readLines(paste0(getwd(), "/execute", "/progress.txt"))[1] != "0") |
              (tail(readLogFile(), 1) != "0")) {
      show_toast(
        title = "Pending Typing",
        type = "warning",
        position = "top-end",
        timer = 4000,
        width = "500px"
      )
    } else {
      if( (length(input$select_delete) - nrow(DB$data) ) == 0) {
        showModal(
          modalDialog(
            paste0("Deleting will lead to removal of all entries from local ", DB$scheme, " database. The data can not be recovered afterwards. Continue?"),
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
    
    # remove file with typing data
    file.remove(paste0(DB$database, "/", gsub(" ", "_", DB$scheme), "/Typing.rds"))
    
    showModal(
      modalDialog(
        selectInput(
          "scheme_db",
          label = "",
          choices = if(!is.null(Typing$last_scheme)) {
            Typing$last_scheme
          } else {DB$available},
          selected = if(!is.null(Typing$last_scheme)) {
            Typing$last_scheme
          } else {if(!is.null(DB$scheme)) {DB$scheme} else {DB$available[1]}}),
        title = "All entries have been removed. Select a local database to load.",
        footer = tagList(
          actionButton("load", "Load", class = "btn btn-default")
        )
      )
    )
    
  })
  
  DB$deleted_entries <- character(0)
  
  observeEvent(input$conf_delete, {
    
    DB$deleted_entries <- append(DB$deleted_entries, DB$data$Index[as.numeric(input$select_delete)])
    
    DB$no_na_switch <- TRUE
    
    DB$change <- TRUE
    
    DB$check_new_entries <- FALSE
    
    DB$data <- DB$data[!(DB$data$Index %in% as.numeric(input$select_delete)),]
    
    DB$meta <- select(DB$data, 1:(12 + nrow(DB$cust_var)))
    
    DB$meta_true <- DB$meta[which(DB$data$Include == TRUE),]
    
    DB$allelic_profile <- select(DB$data, -(1:(12 + nrow(DB$cust_var))))
    
    DB$allelic_profile_true <- DB$allelic_profile[which(DB$data$Include == TRUE),]
    
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
      if(anyNA(DB$allelic_profile)) {
        if(input$na_handling == "omit") {
          allelic_profile_noNA <- DB$allelic_profile[, colSums(is.na(DB$allelic_profile)) == 0]
          
          allelic_profile_noNA_true <- allelic_profile_noNA[which(DB$data$Include == TRUE),]
          
          DB$hamming_proxy <- proxy::dist(allelic_profile_noNA_true, method = hamming_distance)
          
        } else if(input$na_handling == "ignore_na"){
          DB$hamming_proxy <- proxy::dist(DB$allelic_profile_true, method = hamming_distance_ignore)
          
        } else {
          DB$hamming_proxy <- proxy::dist(DB$allelic_profile_true, method = hamming_distance_category)
          
        } 
      } else {
        DB$hamming_proxy <- proxy::dist(DB$allelic_profile_true, method = hamming_distance)
      }
    } else {
      if(anyNA(DB$allelic_profile)) {
        if(input$na_handling == "omit") {
          allelic_profile_noNA <- DB$allelic_profile[, colSums(is.na(DB$allelic_profile)) == 0]
          DB$hamming_proxy <- proxy::dist(allelic_profile_noNA, method = hamming_distance)
        } else if(input$na_handling == "ignore_na"){
          DB$hamming_proxy <- proxy::dist(DB$allelic_profile, method = hamming_distance_ignore)
        } else {
          DB$hamming_proxy <- proxy::dist(DB$allelic_profile, method = hamming_distance_category)
        }  
      } else {
        DB$hamming_proxy <- proxy::dist(DB$allelic_profile, method = hamming_distance)
      }
    }
    
    hamming_matrix <- as.matrix(DB$hamming_proxy)
    
    DB$matrix_min <- min(hamming_matrix, na.rm = TRUE)
    DB$matrix_max <- max(hamming_matrix, na.rm = TRUE)
    
    if(input$distmatrix_triangle == FALSE) {
      hamming_matrix[upper.tri(hamming_matrix, diag = !input$distmatrix_diag)] <- NA
    } 
    
    # Rownames change
    rownames(hamming_matrix) <- select(DB$data, 1:(12 + nrow(DB$cust_var)))[rownames(select(DB$data, 1:(12 + nrow(DB$cust_var)))) %in% rownames(hamming_matrix), 
                                                                            input$distmatrix_label]
    colnames(hamming_matrix) <- rownames(hamming_matrix)
    
    mode(hamming_matrix) <- "integer"
    
    DB$ham_matrix <- hamming_matrix %>%
      as.data.frame() %>%
      mutate(Index = colnames(hamming_matrix)) %>%
      relocate(Index)
    DB$distancematrix_nrow <- nrow(DB$ham_matrix)
    DB$ham_matrix
  })
  
  output$download_distmatrix <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_", gsub(" ", "_", DB$scheme), "_Distance_Matrix.csv")
    },
    content = function(file) {
      download_matrix <- hot_to_r(input$db_distancematrix)
      download_matrix[is.na(download_matrix)] <- ""
      write.csv(download_matrix, file, row.names=FALSE, quote=FALSE) 
    }
  )
  
  # _______________________ ####
  
  ## Download cgMLST ----
  
  observe({
    if (input$select_cgmlst == "Acinetobacter baumanii") {
      species <- "Abaumannii1907"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- Scheme$folder_name <- "Acinetobacter_baumanii"
    } else if (input$select_cgmlst == "Bacillus anthracis") {
      species <- "Banthracis1917"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Bacillus_anthracis"
    } else if (input$select_cgmlst == "Bordetella pertussis") {
      species <- "Bpertussis1917"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Bordetella_pertussis"
    } else if (input$select_cgmlst == "Brucella melitensis") {
      species <- "Bmelitensis1912"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Brucella_melitensis"
    } else if (input$select_cgmlst == "Brucella spp.") {
      species <- "Brucella1914"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Brucella_spp"
    } else if (input$select_cgmlst == "Burkholderia mallei (FLI)") {
      species <- "Bmallei_fli1911"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Burkholderia_mallei_FLI"
    } else if (input$select_cgmlst == "Burkholderia mallei (RKI)") {
      species <- "Bmallei_rki1909"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Burkholderia_mallei_RKI"
    } else if (input$select_cgmlst == "Burkholderia pseudomallei") {
      species <- "Bpseudomallei1906"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Burkholderia_pseudomallei"
    } else if (input$select_cgmlst == "Campylobacter jejuni/coli") {
      species <- "Cjejuni1911"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Campylobacter_jejuni_coli"
    } else if (input$select_cgmlst == "Clostridioides difficile") {
      species <- "Cdifficile1905"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Clostridioides_difficile"
    } else if (input$select_cgmlst == "Clostridium perfringens") {
      species <- "Cperfringens1907"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Clostridium_perfringens"
    } else if (input$select_cgmlst == "Corynebacterium diphtheriae") {
      species <- "Cdiphtheriae1907"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Corynebacterium_diphtheriae"
    } else if (input$select_cgmlst == "Cronobacter sakazakii/malonaticus") {
      species <- "Csakazakii1910"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Cronobacter_sakazakii_malonaticus"
    } else if (input$select_cgmlst == "Enterococcus faecalis") {
      species <- "Efaecalis1912"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Enterococcus_faecalis"
    } else if (input$select_cgmlst == "Enterococcus faecium") {
      species <- "Efaecium1911"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Enterococcus_faecium"
    } else if (input$select_cgmlst == "Escherichia coli") {
      species <- "Ecoli1911"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Escherichia_coli"
    } else if (input$select_cgmlst == "Francisella tularensis") {
      species <- "Ftularensis1913"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Francisella_tularensis"
    } else if (input$select_cgmlst == "Klebsiella oxytoca sensu lato") {
      species <- "Koxytoca717"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Klebsiella_oxytoca_sensu_lato"
    } else if (input$select_cgmlst == "Klebsiella pneumoniae sensu lato") {
      species <- "Kpneumoniae1909"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Klebsiella_pneumoniae_sensu_lato"
    } else if (input$select_cgmlst == "Legionella pneumophila") {
      species <- "Lpneumophila1911"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Legionella_pneumophila"
    } else if (input$select_cgmlst == "Listeria monocytogenes") {
      species <- "Lmonocytogenes1910"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Listeria_monocytogenes"
    } else if (input$select_cgmlst == "Mycobacterium tuberculosis complex") {
      species <- "Mtuberculosis1909"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Mycobacterium_tuberculosis_complex"
    } else if (input$select_cgmlst == "Mycobacteroides abscessus") {
      species <- "Mabscessus1911"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Mycobacteroides_abscessus"
    } else if (input$select_cgmlst == "Mycoplasma gallisepticum") {
      species <- "Mgallisepticum1911"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Mycoplasma_gallisepticum"
    } else if (input$select_cgmlst == "Paenibacillus larvae") {
      species <- "Plarvae1902"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Paenibacillus_larvae"
    } else if (input$select_cgmlst == "Pseudomonas aeruginosa") {
      species <- "Paeruginosa1911"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Pseudomonas_aeruginosa"
    } else if (input$select_cgmlst == "Salmonella enterica") {
      species <- "Senterica1913"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Salmonella_enterica"
    } else if (input$select_cgmlst == "Serratia marcescens") {
      species <- "Smarcescens1912"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Serratia_marcescens"
    } else if (input$select_cgmlst == "Staphylococcus aureus") {
      species <- "Saureus1908"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Staphylococcus_aureus"
    } else if (input$select_cgmlst == "Staphylococcus capitis") {
      species <- "Scapitis1905"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Staphylococcus_capitis"
    } else if (input$select_cgmlst == "Streptococcus pyogenes") {
      species <- "Spyogenes1904"
      Scheme$link_scheme <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/")
      Scheme$link_cgmlst <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/alleles/")
      Scheme$link_targets <- paste0("https://www.cgmlst.org/ncs/schema/", species, "/locus/?content-type=csv")
      Scheme$folder_name <- "Streptococcus_pyogenes"
    }
  })
  
  observeEvent(input$download_cgMLST, {
    
    show_toast(
      title = "Download started",
      type = "success",
      position = "top-end",
      timer = 5000,
      width = "400px"
    )
    
    if(length(DB$available) == 0) {
      saveRDS(DB$new_database, paste0(getwd(), "/execute/new_db.rds"))
      dir.create(file.path(readRDS(paste0(getwd(), "/execute/new_db.rds")), "Database"), recursive = TRUE)
    }
    
    DB$load_selected <- TRUE
    Scheme$target_table <- NULL
    
    # Download Loci Fasta Files
    
    options(timeout = 600)
    
    tryCatch({
      download.file(Scheme$link_cgmlst, "dataset.zip")
      "Download successful!"
    }, error = function(e) {
      paste("Error: ", e$message)
    })
    
    unzip(
      zipfile = "dataset.zip",
      exdir = paste0(
        DB$database, "/",
        Scheme$folder_name,
        paste0("/", Scheme$folder_name, "_alleles")
      )
    )
    
    unlink("dataset.zip")
    
    # Download Scheme Info
    download(
      Scheme$link_scheme,
      dest = paste0(DB$database, "/", Scheme$folder_name, "/scheme_info.html"),
      mode = "wb"
    )
    
    # Download Loci Info
    download(
      Scheme$link_targets,
      dest = paste0(DB$database, "/", Scheme$folder_name, "/targets.csv"),
      mode = "wb"
    )
    
    # Send downloaded scheme to database browser overview
    DB$available <- gsub("_", " ", basename(dir_ls(DB$database)))
    
    Scheme$target_table <-
      read.csv(
        paste0(DB$database, "/", Scheme$folder_name, "/targets.csv"),
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
    
    DB$exist <-
      (length(dir_ls(DB$database)) == 0)
    
    show_toast(
      title = "Download successful",
      type = "success",
      position = "top-end",
      timer = 5000,
      width = "400px"
    )
    
    showModal(
      modalDialog(
        selectInput(
          "scheme_db",
          label = "",
          choices = if(!is.null(Typing$last_scheme)) {
            Typing$last_scheme
          } else {DB$available},
          selected = if(!is.null(Typing$last_scheme)) {
            Typing$last_scheme
          } else {if(!is.null(DB$scheme)) {input$select_cgmlst} else {DB$available[1]}}),
        title = "Select a local database to load.",
        footer = tagList(
          actionButton("load", "Load", class = "btn btn-default")
        )
      )
    )
  })
  
  
  
  # Download Target Info (CSV Table)
  
  
  output$cgmlst_scheme <- renderTable({
    scheme_overview <- read_html(Scheme$link_scheme) %>%
      html_table(header = FALSE) %>%
      as.data.frame(stringsAsFactors = FALSE)
    names(scheme_overview) <- NULL
    scheme_overview
  })
  
  ### Display Target Table ----
  
  output$cgmlst_targets <- renderDataTable({
    targets_overview <- Scheme$target_table
  },
  options = list(pageLength = 10,
                 columnDefs = list(
                   list(searchable = FALSE, targets = "_all")
                 )))
  
  # _______________________ ####
  
  ## Visualization ----
  
  # Render placeholder image
  
  output$placeholder <- renderImage({
    # Path to your PNG image with a transparent background
    image_path <- paste0(getwd(), "/www/PhyloTrace.png")
    
    # Use HTML to display the image with the <img> tag
    list(src = image_path,
         height = 180)
  }, deleteFile = FALSE)
  
  # Render tree plot fields
  
  output$nj_field <- renderUI(
    fluidRow(
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br()
    )
  )
  
  output$mst_field <- renderUI(
    fluidRow(
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br()
    )
  )
  
  output$upgma_field <- renderUI(
    fluidRow(
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br()
    )
  )
  
  ### Render Visualization Controls ----
  
  #### NJ and UPGMA controls ----
  
  # Custom Labels
  
  # Add custom label
  observeEvent(input$nj_add_new_label, {
    if(nchar(input$nj_new_label_name) > 0) {
      if(!(input$nj_new_label_name %in% Vis$custom_label_nj)) {
        Vis$custom_label_nj <- rbind(Vis$custom_label_nj, input$nj_new_label_name) 
        if(!(nrow(Vis$custom_label_nj) == 1)) {
          updateSelectInput(session, "nj_custom_label_sel", selected = input$nj_new_label_name)
        }
      } else {
        show_toast(
          title = "Label already exists",
          type = "error",
          position = "top-end",
          timer = 6000,
          width = "500px"
        )
      }
    } else {
      show_toast(
        title = "Min. 1 character",
        type = "error",
        position = "top-end",
        timer = 6000,
        width = "500px"
      )
    }
  })
  
  observeEvent(input$upgma_add_new_label, {
    if(nchar(input$upgma_new_label_name) > 0) {
      if(!(input$upgma_new_label_name %in% Vis$custom_label_upgma)) {
        Vis$custom_label_upgma <- rbind(Vis$custom_label_upgma, input$upgma_new_label_name) 
        if(!(nrow(Vis$custom_label_upgma) == 1)) {
          updateSelectInput(session, "upgma_custom_label_sel", selected = input$upgma_new_label_name)
        }
      } else {
        show_toast(
          title = "Label already exists",
          type = "error",
          position = "top-end",
          timer = 6000,
          width = "500px"
        )
      }
    } else {
      show_toast(
        title = "Min. 1 character",
        type = "error",
        position = "top-end",
        timer = 6000,
        width = "500px"
      )
    }
  })
  
  # Delete custom label
  observeEvent(input$nj_del_label, {
    if(nrow(Vis$custom_label_nj) > 1) {
      Vis$custom_label_nj <- Vis$custom_label_nj[-which(Vis$custom_label_nj[,1] == input$nj_custom_label_sel), , drop = FALSE]
    } else if (nrow(Vis$custom_label_nj) == 1) {
      Vis$nj_label_pos_x <- list()
      Vis$nj_label_pos_y <- list()
      Vis$nj_label_size <- list()
      Vis$custom_label_nj <- data.frame()
    }
  })
  
  observeEvent(input$upgma_del_label, {
    if(nrow(Vis$custom_label_upgma) > 1) {
      Vis$custom_label_upgma <- Vis$custom_label_upgma[-which(Vis$custom_label_upgma[,1] == input$upgma_custom_label_sel), , drop = FALSE]
    } else if (nrow(Vis$custom_label_upgma) == 1) {
      Vis$upgma_label_pos_x <- list()
      Vis$upgma_label_pos_y <- list()
      Vis$upgma_label_size <- list()
      Vis$custom_label_upgma <- data.frame()
    }
  })
  
  # Select custom labels
  output$nj_custom_label_select <- renderUI({
    if(nrow(Vis$custom_label_nj) > 0) {
      selectInput(
        "nj_custom_label_sel",
        "",
        choices = Vis$custom_label_nj[,1]
      )
    }
  })
  
  output$upgma_custom_label_select <- renderUI({
    if(nrow(Vis$custom_label_upgma) > 0) {
      selectInput(
        "upgma_custom_label_sel",
        "",
        choices = Vis$custom_label_upgma[,1]
      )
    }
  })
  
  # Select custom labels
  output$nj_cust_label_save <- renderUI({
    if(nrow(Vis$custom_label_nj) > 0) {
      actionButton(
        "nj_cust_label_save",
        "Apply"
      )
    } else {
      column(
        width = 12,
        br(), br(), br(), br(), br(), br(),
        h5("test", style = "color: transparent; margin-bottom: 3px")
      )
    }
  })
  
  output$upgma_cust_label_save <- renderUI({
    if(nrow(Vis$custom_label_upgma) > 0) {
      actionButton(
        "upgma_cust_label_save",
        "Apply"
      )
    } else {
      column(
        width = 12,
        br(), br(), br(), br(), br(), br(),
        h5("test", style = "color: transparent; margin-bottom: 3px")
      )
    }
  })
  
  # Custom Label Size
  output$nj_custom_labelsize <- renderUI({
    if(length(Vis$custom_label_nj) > 0) {
      if(!is.null(Vis$nj_label_size[[input$nj_custom_label_sel]])) {
        sliderInput(inputId = paste0("nj_slider_", input$nj_custom_label_sel, "_size"),
                    label = h5("Size", style = "color: white; margin-bottom: 0px;"),
                    min = 0, max = 10, step = 0.5, ticks = F,
                    value = Vis$nj_label_size[[input$nj_custom_label_sel]],
                    width = "150px")
      } else {
        sliderInput(inputId = paste0("nj_slider_", input$nj_custom_label_sel, "_size"),
                    label = h5("Size", style = "color: white; margin-bottom: 0px;"),
                    min = 0, max = 10, step = 0.5, ticks = F, value = 5,
                    width = "150px")
      }
    } 
  })
  
  output$upgma_custom_labelsize <- renderUI({
    if(length(Vis$custom_label_upgma) > 0) {
      if(!is.null(Vis$upgma_label_size[[input$upgma_custom_label_sel]])) {
        sliderInput(inputId = paste0("upgma_slider_", input$upgma_custom_label_sel, "_size"),
                    label = h5("Size", style = "color: white; margin-bottom: 0px;"),
                    min = 0, max = 10, step = 0.5, ticks = F,
                    value = Vis$upgma_label_size[[input$upgma_custom_label_sel]],
                    width = "150px")
      } else {
        sliderInput(inputId = paste0("upgma_slider_", input$upgma_custom_label_sel, "_size"),
                    label = h5("Size", style = "color: white; margin-bottom: 0px;"),
                    min = 0, max = 10, step = 0.5, ticks = F, value = 5,
                    width = "150px")
      }
    } 
  })
  
  # Render slider input based on selected label
  output$nj_sliderInput_y <- renderUI({
    if(length(Vis$custom_label_nj) > 0) {
      if(!is.null(Vis$nj_label_pos_y[[input$nj_custom_label_sel]])) {
        sliderInput(inputId = paste0("nj_slider_", input$nj_custom_label_sel, "_y"),
                    label = h5("Vertical", style = "color: white; margin-bottom: 5px;"),
                    min = 0, max = 50, step = 1, ticks = F,
                    value = Vis$nj_label_pos_y[[input$nj_custom_label_sel]],
                    width = "150px")
      } else {
        sliderInput(inputId = paste0("nj_slider_", input$nj_custom_label_sel, "_y"),
                    label = h5("Vertical", style = "color: white; margin-bottom: 5px;"),
                    min = 0, max = sum(DB$data$Include), step = 1, ticks = F, 
                    value = sum(DB$data$Include) / 2,
                    width = "150px")
      }
    } 
  })
  
  output$upgma_sliderInput_y <- renderUI({
    if(length(Vis$custom_label_upgma) > 0) {
      if(!is.null(Vis$upgma_label_pos_y[[input$upgma_custom_label_sel]])) {
        sliderInput(inputId = paste0("upgma_slider_", input$upgma_custom_label_sel, "_y"),
                    label = h5("Vertical", style = "color: white; margin-bottom: 5px;"),
                    min = 0, max = 50, step = 1, ticks = F,
                    value = Vis$upgma_label_pos_y[[input$upgma_custom_label_sel]],
                    width = "150px")
      } else {
        sliderInput(inputId = paste0("upgma_slider_", input$upgma_custom_label_sel, "_y"),
                    label = h5("Vertical", style = "color: white; margin-bottom: 5px;"),
                    min = 0, max = sum(DB$data$Include), step = 1, ticks = F, 
                    value = sum(DB$data$Include) / 2,
                    width = "150px")
      }
    } 
  })
  
  output$nj_sliderInput_x <- renderUI({
    if(length(Vis$custom_label_nj) > 0) {
      if(!is.null(Vis$nj_label_pos_x[[input$nj_custom_label_sel]])) {
        sliderInput(inputId = paste0("nj_slider_", input$nj_custom_label_sel, "_x"),
                    label = h5("Horizontal", style = "color: white; margin-bottom: 5px;"),
                    min = 0, max = 50, step = 1, ticks = F,
                    value = Vis$nj_label_pos_x[[input$nj_custom_label_sel]],
                    width = "150px")
      } else {
        sliderInput(inputId = paste0("nj_slider_", input$nj_custom_label_sel, "_x"),
                    label = h5("Horizontal", style = "color: white; margin-bottom: 5px;"),
                    min = 0, max = round(Vis$nj_max_x, 0), step = 1, ticks = F, 
                    value = round(Vis$nj_max_x / 2, 0),
                    width = "150px")
      }
    }
  })
  
  output$upgma_sliderInput_x <- renderUI({
    if(length(Vis$custom_label_upgma) > 0) {
      if(!is.null(Vis$upgma_label_pos_x[[input$upgma_custom_label_sel]])) {
        sliderInput(inputId = paste0("upgma_slider_", input$upgma_custom_label_sel, "_x"),
                    label = h5("Horizontal", style = "color: white; margin-bottom: 5px;"),
                    min = 0, max = 50, step = 1, ticks = F,
                    value = Vis$upgma_label_pos_x[[input$upgma_custom_label_sel]],
                    width = "150px")
      } else {
        sliderInput(inputId = paste0("upgma_slider_", input$upgma_custom_label_sel, "_x"),
                    label = h5("Horizontal", style = "color: white; margin-bottom: 5px;"),
                    min = 0, max = round(Vis$upgma_max_x, 0), step = 1, ticks = F, 
                    value = round(Vis$upgma_max_x / 2, 0),
                    width = "150px")
      }
    }
  })
  
  # Apply custom label changes
  observeEvent(input$nj_cust_label_save, {
    if(!is.null(Vis$nj_label_pos_y) &
       !is.null(Vis$nj_label_pos_x) &
       !is.null(Vis$nj_label_size) &
       !is.null(input$nj_custom_label_sel)) {
      Vis$nj_label_pos_y[[input$nj_custom_label_sel]] <- input[[paste0("nj_slider_", input$nj_custom_label_sel, "_y")]]
      Vis$nj_label_pos_x[[input$nj_custom_label_sel]] <- input[[paste0("nj_slider_", input$nj_custom_label_sel, "_x")]]
      Vis$nj_label_size[[input$nj_custom_label_sel]] <- input[[paste0("nj_slider_", input$nj_custom_label_sel, "_size")]]
    }
  })
  
  observeEvent(input$upgma_cust_label_save, {
    if(!is.null(Vis$upgma_label_pos_y) &
       !is.null(Vis$upgma_label_pos_x) &
       !is.null(Vis$upgma_label_size) &
       !is.null(input$upgma_custom_label_sel)) {
      Vis$upgma_label_pos_y[[input$upgma_custom_label_sel]] <- input[[paste0("upgma_slider_", input$upgma_custom_label_sel, "_y")]]
      Vis$upgma_label_pos_x[[input$upgma_custom_label_sel]] <- input[[paste0("upgma_slider_", input$upgma_custom_label_sel, "_x")]]
      Vis$upgma_label_size[[input$upgma_custom_label_sel]] <- input[[paste0("upgma_slider_", input$upgma_custom_label_sel, "_size")]]
    }
  })
  
  # Show delete custom label button if custam label added
  output$nj_del_label <- renderUI({
    if(nrow(Vis$custom_label_nj) > 0) {
      actionButton(
        "nj_del_label",
        "",
        icon = icon("minus")
      )
    } else {NULL}
  })
  
  output$upgma_del_label <- renderUI({
    if(nrow(Vis$custom_label_upgma) > 0) {
      actionButton(
        "upgma_del_label",
        "",
        icon = icon("minus")
      )
    } else {NULL}
  })
  
  # Mapping value number information
  output$nj_tiplab_mapping_info <- renderUI({
    if(!is.null(input$nj_color_mapping) & (!is.null(Vis$meta_nj))) {
      if(is.numeric(unlist(Vis$meta_nj[input$nj_color_mapping]))) {
        if(input$nj_tiplab_scale %in% c('Spectral', 'RdYlGn', 'RdYlBu', 'RdGy', 'RdBu', 'PuOr', 'PRGn', 'PiYG', 'BrBG')) {
          column(
            width = 3,
            fluidRow(
              column(
                width = 4,
                h5("Midpoint", style = "color: white; margin-top: 22px;")
              ),
              column(
                width = 8,
                div(
                  class = "divmid-sel1",
                  selectInput(
                    "nj_color_mapping_div_mid",
                    label = "",
                    choices = c("Zero", "Mean", "Median"),
                    selected = "Mean"
                  )
                )
              )
            )
          )
        } else {
          column(
            width = 3,
            h5("Continous values", style = "color: white; font-size: 14px; margin-top: 23px; margin-left: 40px")
          ) 
        }
      } else {
        if(length(unique(unlist(Vis$meta_nj[input$nj_color_mapping]))) > 7) {
          column(
            width = 3,
            h5(paste0("> 7 (", length(unique(unlist(Vis$meta_nj[input$nj_color_mapping]))), ") categorical values"), style = "color: #E18B00; font-size: 12px; margin-top: 23px;  margin-left: 40px")        
          )
        } else {
          column(
            width = 3,
            h5(paste0(length(unique(unlist(Vis$meta_nj[input$nj_color_mapping]))), paste0(" categorical values")), style = "color: white; font-size: 14px; margin-top: 20px;  margin-left: 40px")        
          )
        }
      }
    } else {NULL}
  })
  
  output$upgma_tiplab_mapping_info <- renderUI({
    if(!is.null(input$upgma_color_mapping) & (!is.null(Vis$meta_upgma))) {
      if(is.numeric(unlist(Vis$meta_upgma[input$upgma_color_mapping]))) {
        if(input$upgma_tiplab_scale %in% c('Spectral', 'RdYlGn', 'RdYlBu', 'RdGy', 'RdBu', 'PuOr', 'PRGn', 'PiYG', 'BrBG')) {
          column(
            width = 3,
            fluidRow(
              column(
                width = 4,
                h5("Midpoint", style = "color: white; margin-top: 22px;")
              ),
              column(
                width = 8,
                div(
                  class = "divmid-sel1",
                  selectInput(
                    "upgma_color_mapping_div_mid",
                    label = "",
                    choices = c("Zero", "Mean", "Median"),
                    selected = "Mean"
                  )
                )
              )
            )
          )
        } else {
          column(
            width = 3,
            h5("Continous values", style = "color: white; font-size: 14px; margin-top: 23px; margin-left: 40px")
          ) 
        }
      } else {
        if(length(unique(unlist(Vis$meta_upgma[input$upgma_color_mapping]))) > 7) {
          column(
            width = 3,
            h5(paste0("> 7 (", length(unique(unlist(Vis$meta_upgma[input$upgma_color_mapping]))), ") categorical values"), style = "color: #E18B00; font-size: 12px; margin-top: 23px;  margin-left: 40px")        
          )
        } else {
          column(
            width = 3,
            h5(paste0(length(unique(unlist(Vis$meta_upgma[input$upgma_color_mapping]))), paste0(" categorical values")), style = "color: white; font-size: 14px; margin-top: 20px;  margin-left: 40px")        
          )
        }
      }
    } else {NULL}
  })
  
  output$nj_tipcolor_mapping_info <- renderUI({
    if(!is.null(input$nj_tipcolor_mapping) & (!is.null(Vis$meta_nj))) {
      if(is.numeric(unlist(Vis$meta_nj[input$nj_tipcolor_mapping]))) {
        if(input$nj_tippoint_scale %in% c('Spectral', 'RdYlGn', 'RdYlBu', 'RdGy', 'RdBu', 'PuOr', 'PRGn', 'PiYG', 'BrBG')) {
          column(
            width = 3,
            fluidRow(
              column(
                width = 4,
                h5("Midpoint", style = "color: white; margin-top: 22px;")
              ),
              column(
                width = 8,
                div(
                  class = "divmid-sel1",
                  selectInput(
                    "nj_tipcolor_mapping_div_mid",
                    label = "",
                    choices = c("Zero", "Mean", "Median"),
                    selected = "Mean"
                  )
                )
              )
            )
          )
        } else {
          column(
            width = 3,
            h5("Continous values", style = "color: white; font-size: 14px; margin-top: 20px; margin-left: 40px")
          ) 
        }
      } else {
        if(length(unique(unlist(Vis$meta_nj[input$nj_tipcolor_mapping]))) > 7) {
          column(
            width = 3,
            h5(paste0("> 7 (", length(unique(unlist(Vis$meta_nj[input$nj_tipcolor_mapping]))), ") categorical values"), style = "color: #E18B00; font-size: 12px; margin-top: 23px;  margin-left: 40px")        
          )
        } else {
          column(
            width = 3,
            h5(paste0(length(unique(unlist(Vis$meta_nj[input$nj_tipcolor_mapping]))), paste0(" categorical values")), style = "color: white; font-size: 14px; margin-top: 20px;  margin-left: 40px")        
          )
        }
      }
    } else {NULL}
  })
  
  output$upgma_tipcolor_mapping_info <- renderUI({
    if(!is.null(input$upgma_tipcolor_mapping) & (!is.null(Vis$meta_upgma))) {
      if(is.numeric(unlist(Vis$meta_upgma[input$upgma_tipcolor_mapping]))) {
        if(input$upgma_tippoint_scale %in% c('Spectral', 'RdYlGn', 'RdYlBu', 'RdGy', 'RdBu', 'PuOr', 'PRGn', 'PiYG', 'BrBG')) {
          column(
            width = 3,
            fluidRow(
              column(
                width = 4,
                h5("Midpoint", style = "color: white; margin-top: 22px;")
              ),
              column(
                width = 8,
                div(
                  class = "divmid-sel1",
                  selectInput(
                    "upgma_tipcolor_mapping_div_mid",
                    label = "",
                    choices = c("Zero", "Mean", "Median"),
                    selected = "Mean"
                  )
                )
              )
            )
          )
        } else {
          column(
            width = 3,
            h5("Continous values", style = "color: white; font-size: 14px; margin-top: 20px; margin-left: 40px")
          ) 
        }
      } else {
        if(length(unique(unlist(Vis$meta_upgma[input$upgma_tipcolor_mapping]))) > 7) {
          column(
            width = 3,
            h5(paste0("> 7 (", length(unique(unlist(Vis$meta_upgma[input$upgma_tipcolor_mapping]))), ") categorical values"), style = "color: #E18B00; font-size: 12px; margin-top: 23px;  margin-left: 40px")        
          )
        } else {
          column(
            width = 3,
            h5(paste0(length(unique(unlist(Vis$meta_upgma[input$upgma_tipcolor_mapping]))), paste0(" categorical values")), style = "color: white; font-size: 14px; margin-top: 20px;  margin-left: 40px")        
          )
        }
      }
    } else {NULL}
  })
  
  output$nj_tipshape_mapping_info <- renderUI({
    if(!is.null(input$nj_tipshape_mapping) & (!is.null(Vis$meta_nj))) {
      if(is.numeric(unlist(Vis$meta_nj[input$nj_tipshape_mapping]))) {
        column(
          width = 3,
          h5("Mapping continous variables to shape not possible", style = "color: #E18B00; font-style: italic; font-size: 12px; margin-top: 15px;  margin-left: 40px")        
        )
      } else {
        if(length(unique(unlist(Vis$meta_nj[input$nj_tipshape_mapping]))) > 6) {
          column(
            width = 3,
            h5("Mapping > 6 variables to shape not possible", style = "color: #E18B00; font-style: italic; font-size: 12px; margin-top: 15px;  margin-left: 40px")        
          )
        } else {
          column(
            width = 3,
            h5(paste0(length(unique(unlist(Vis$meta_nj[input$nj_tipshape_mapping]))), paste0(" categorical values")), style = "color: white; font-size: 14px; margin-top: 20px;  margin-left: 40px")        
          )    
        }
      }
    } else {NULL}
  })
  
  output$upgma_tipshape_mapping_info <- renderUI({
    if(!is.null(input$upgma_tipshape_mapping) & (!is.null(Vis$meta_upgma))) {
      if(is.numeric(unlist(Vis$meta_upgma[input$upgma_tipshape_mapping]))) {
        column(
          width = 3,
          h5("Mapping continous variables to shape not possible", style = "color: #E18B00; font-style: italic; font-size: 12px; margin-top: 15px;  margin-left: 40px")        
        )
      } else {
        if(length(unique(unlist(Vis$meta_upgma[input$upgma_tipshape_mapping]))) > 6) {
          column(
            width = 3,
            h5("Mapping > 6 variables to shape not possible", style = "color: #E18B00; font-style: italic; font-size: 12px; margin-top: 15px;  margin-left: 40px")        
          )
        } else {
          column(
            width = 3,
            h5(paste0(length(unique(unlist(Vis$meta_upgma[input$upgma_tipshape_mapping]))), paste0(" categorical values")), style = "color: white; font-size: 14px; margin-top: 20px;  margin-left: 40px")        
          )    
        }
      }
    } else {NULL}
  })
  
  output$nj_fruit_mapping_info <- renderUI({
    if(input$nj_tile_num == 1) {
      if(!is.null(input$nj_fruit_variable) & (!is.null(Vis$meta_nj))) {
        if(is.numeric(unlist(Vis$meta_nj[input$nj_fruit_variable]))) {
          if(input$nj_tiles_scale_1 %in% c('Spectral', 'RdYlGn', 'RdYlBu', 'RdGy', 'RdBu', 'PuOr', 'PRGn', 'PiYG', 'BrBG')) {
            column(
              width = 3,
              fluidRow(
                column(
                  width = 4,
                  h5("Midpoint", style = "color: white; margin-top: 22px;")
                ),
                column(
                  width = 8,
                  div(
                    class = "divmid-sel1",
                    selectInput(
                      "nj_tiles_mapping_div_mid_1",
                      label = "",
                      choices = c("Zero", "Mean", "Median"),
                      selected = "Mean"
                    )
                  )
                )
              )
            )
          } else {
            column(
              width = 3,
              h5("Continous values", style = "color: white; font-size: 14px; margin-top: 20px; margin-left: 40px")
            ) 
          }
        } else {
          if(length(unique(unlist(Vis$meta_nj[input$nj_fruit_variable]))) > 7) {
            column(
              width = 3,
              h5(paste0("> 7 (", length(unique(unlist(Vis$meta_nj[input$nj_fruit_variable]))), ") categorical values"), style = "color: #E18B00; font-size: 12px; margin-top: 23px;  margin-left: 40px")        
            )
          } else {
            column(
              width = 3,
              h5(paste0(length(unique(unlist(Vis$meta_nj[input$nj_fruit_variable]))), paste0(" categorical values")), style = "color: white; font-size: 14px; margin-top: 20px;  margin-left: 40px")        
            )
          }
        }
      } else {NULL}
    } else if (input$nj_tile_num == 2) {
      if(!is.null(input$nj_fruit_variable_2) & (!is.null(Vis$meta_nj))) {
        if(is.numeric(unlist(Vis$meta_nj[input$nj_fruit_variable_2]))) {
          if(input$nj_tiles_scale_2 %in% c('Spectral', 'RdYlGn', 'RdYlBu', 'RdGy', 'RdBu', 'PuOr', 'PRGn', 'PiYG', 'BrBG')) {
            column(
              width = 3,
              fluidRow(
                column(
                  width = 4,
                  h5("Midpoint", style = "color: white; margin-top: 22px;")
                ),
                column(
                  width = 8,
                  div(
                    class = "divmid-sel1",
                    selectInput(
                      "nj_tiles_mapping_div_mid_2",
                      label = "",
                      choices = c("Zero", "Mean", "Median"),
                      selected = "Mean"
                    )
                  )
                )
              )
            )
          } else {
            column(
              width = 3,
              h5("Continous values", style = "color: white; font-size: 14px; margin-top: 20px; margin-left: 40px")
            ) 
          }
        } else {
          if(length(unique(unlist(Vis$meta_nj[input$nj_fruit_variable_2]))) > 7) {
            column(
              width = 3,
              h5(paste0("> 7 (", length(unique(unlist(Vis$meta_nj[input$nj_fruit_variable_2]))), ") categorical values"), style = "color: #E18B00; font-size: 12px; margin-top: 23px;  margin-left: 40px")        
            )
          } else {
            column(
              width = 3,
              h5(paste0(length(unique(unlist(Vis$meta_nj[input$nj_fruit_variable_2]))), paste0(" categorical values")), style = "color: white; font-size: 14px; margin-top: 20px;  margin-left: 40px")        
            )
          }
        }
      } else {NULL}
    } else if (input$nj_tile_num == 3) {
      if(!is.null(input$nj_fruit_variable_3) & (!is.null(Vis$meta_nj))) {
        if(is.numeric(unlist(Vis$meta_nj[input$nj_fruit_variable_3]))) {
          if(input$nj_tiles_scale_3 %in% c('Spectral', 'RdYlGn', 'RdYlBu', 'RdGy', 'RdBu', 'PuOr', 'PRGn', 'PiYG', 'BrBG')) {
            column(
              width = 3,
              fluidRow(
                column(
                  width = 4,
                  h5("Midpoint", style = "color: white; margin-top: 22px;")
                ),
                column(
                  width = 8,
                  div(
                    class = "divmid-sel1",
                    selectInput(
                      "nj_tiles_mapping_div_mid_3",
                      label = "",
                      choices = c("Zero", "Mean", "Median"),
                      selected = "Mean"
                    )
                  )
                )
              )
            )
          } else {
            column(
              width = 3,
              h5("Continous values", style = "color: white; font-size: 14px; margin-top: 20px; margin-left: 40px")
            ) 
          }
        } else {
          if(length(unique(unlist(Vis$meta_nj[input$nj_fruit_variable_3]))) > 7) {
            column(
              width = 3,
              h5(paste0("> 7 (", length(unique(unlist(Vis$meta_nj[input$nj_fruit_variable_3]))), ") categorical values"), style = "color: #E18B00; font-size: 12px; margin-top: 23px;  margin-left: 40px")        
            )
          } else {
            column(
              width = 3,
              h5(paste0(length(unique(unlist(Vis$meta_nj[input$nj_fruit_variable_3]))), paste0(" categorical values")), style = "color: white; font-size: 14px; margin-top: 20px;  margin-left: 40px")        
            )
          }
        }
      } else {NULL}
    } else if (input$nj_tile_num == 4) {
      if(!is.null(input$nj_fruit_variable_4) & (!is.null(Vis$meta_nj))) {
        if(is.numeric(unlist(Vis$meta_nj[input$nj_fruit_variable_4]))) {
          if(input$nj_tiles_scale_4 %in% c('Spectral', 'RdYlGn', 'RdYlBu', 'RdGy', 'RdBu', 'PuOr', 'PRGn', 'PiYG', 'BrBG')) {
            column(
              width = 3,
              fluidRow(
                column(
                  width = 4,
                  h5("Midpoint", style = "color: white; margin-top: 22px;")
                ),
                column(
                  width = 8,
                  div(
                    class = "divmid-sel1",
                    selectInput(
                      "nj_tiles_mapping_div_mid_4",
                      label = "",
                      choices = c("Zero", "Mean", "Median"),
                      selected = "Mean"
                    )
                  )
                )
              )
            )
          } else {
            column(
              width = 3,
              h5("Continous values", style = "color: white; font-size: 14px; margin-top: 20px; margin-left: 40px")
            ) 
          }
        } else {
          if(length(unique(unlist(Vis$meta_nj[input$nj_fruit_variable_4]))) > 7) {
            column(
              width = 3,
              h5(paste0("> 7 (", length(unique(unlist(Vis$meta_nj[input$nj_fruit_variable_4]))), ") categorical values"), style = "color: #E18B00; font-size: 12px; margin-top: 23px;  margin-left: 40px")        
            )
          } else {
            column(
              width = 3,
              h5(paste0(length(unique(unlist(Vis$meta_nj[input$nj_fruit_variable_4]))), paste0(" categorical values")), style = "color: white; font-size: 14px; margin-top: 20px;  margin-left: 40px")        
            )
          }
        }
      } else {NULL}
    } else if (input$nj_tile_num == 5) {
      if(!is.null(input$nj_fruit_variable_5) & (!is.null(Vis$meta_nj))) {
        if(is.numeric(unlist(Vis$meta_nj[input$nj_fruit_variable_5]))) {
          if(input$nj_tiles_scale_5 %in% c('Spectral', 'RdYlGn', 'RdYlBu', 'RdGy', 'RdBu', 'PuOr', 'PRGn', 'PiYG', 'BrBG')) {
            column(
              width = 3,
              fluidRow(
                column(
                  width = 4,
                  h5("Midpoint", style = "color: white; margin-top: 22px;")
                ),
                column(
                  width = 8,
                  div(
                    class = "divmid-sel1",
                    selectInput(
                      "nj_tiles_mapping_div_mid_5",
                      label = "",
                      choices = c("Zero", "Mean", "Median"),
                      selected = "Mean"
                    )
                  )
                )
              )
            )
          } else {
            column(
              width = 3,
              h5("Continous values", style = "color: white; font-size: 14px; margin-top: 20px; margin-left: 40px")
            ) 
          }
        } else {
          if(length(unique(unlist(Vis$meta_nj[input$nj_fruit_variable_5]))) > 7) {
            column(
              width = 3,
              h5(paste0("> 7 (", length(unique(unlist(Vis$meta_nj[input$nj_fruit_variable_5]))), ") categorical values"), style = "color: #E18B00; font-size: 12px; margin-top: 23px;  margin-left: 40px")        
            )
          } else {
            column(
              width = 3,
              h5(paste0(length(unique(unlist(Vis$meta_nj[input$nj_fruit_variable_5]))), paste0(" categorical values")), style = "color: white; font-size: 14px; margin-top: 20px;  margin-left: 40px")        
            )
          }
        }
      } else {NULL}
    }
  })
  
  output$upgma_fruit_mapping_info <- renderUI({
    if(input$upgma_tile_num == 1) {
      if(!is.null(input$upgma_fruit_variable) & (!is.null(Vis$meta_upgma))) {
        if(is.numeric(unlist(Vis$meta_upgma[input$upgma_fruit_variable]))) {
          if(input$upgma_tiles_scale_1 %in% c('Spectral', 'RdYlGn', 'RdYlBu', 'RdGy', 'RdBu', 'PuOr', 'PRGn', 'PiYG', 'BrBG')) {
            column(
              width = 3,
              fluidRow(
                column(
                  width = 4,
                  h5("Midpoint", style = "color: white; margin-top: 22px;")
                ),
                column(
                  width = 8,
                  div(
                    class = "divmid-sel1",
                    selectInput(
                      "upgma_tiles_mapping_div_mid_1",
                      label = "",
                      choices = c("Zero", "Mean", "Median"),
                      selected = "Mean"
                    )
                  )
                )
              )
            )
          } else {
            column(
              width = 3,
              h5("Continous values", style = "color: white; font-size: 14px; margin-top: 20px; margin-left: 40px")
            ) 
          }
        } else {
          if(length(unique(unlist(Vis$meta_upgma[input$upgma_fruit_variable]))) > 7) {
            column(
              width = 3,
              h5(paste0("> 7 (", length(unique(unlist(Vis$meta_upgma[input$upgma_fruit_variable]))), ") categorical values"), style = "color: #E18B00; font-size: 12px; margin-top: 23px;  margin-left: 40px")        
            )
          } else {
            column(
              width = 3,
              h5(paste0(length(unique(unlist(Vis$meta_upgma[input$upgma_fruit_variable]))), paste0(" categorical values")), style = "color: white; font-size: 14px; margin-top: 20px;  margin-left: 40px")        
            )
          }
        }
      } else {NULL}
    } else if (input$upgma_tile_num == 2) {
      if(!is.null(input$upgma_fruit_variable_2) & (!is.null(Vis$meta_upgma))) {
        if(is.numeric(unlist(Vis$meta_upgma[input$upgma_fruit_variable_2]))) {
          if(input$upgma_tiles_scale_2 %in% c('Spectral', 'RdYlGn', 'RdYlBu', 'RdGy', 'RdBu', 'PuOr', 'PRGn', 'PiYG', 'BrBG')) {
            column(
              width = 3,
              fluidRow(
                column(
                  width = 4,
                  h5("Midpoint", style = "color: white; margin-top: 22px;")
                ),
                column(
                  width = 8,
                  div(
                    class = "divmid-sel1",
                    selectInput(
                      "upgma_tiles_mapping_div_mid_2",
                      label = "",
                      choices = c("Zero", "Mean", "Median"),
                      selected = "Mean"
                    )
                  )
                )
              )
            )
          } else {
            column(
              width = 3,
              h5("Continous values", style = "color: white; font-size: 14px; margin-top: 20px; margin-left: 40px")
            ) 
          }
        } else {
          if(length(unique(unlist(Vis$meta_upgma[input$upgma_fruit_variable_2]))) > 7) {
            column(
              width = 3,
              h5(paste0("> 7 (", length(unique(unlist(Vis$meta_upgma[input$upgma_fruit_variable_2]))), ") categorical values"), style = "color: #E18B00; font-size: 12px; margin-top: 23px;  margin-left: 40px")        
            )
          } else {
            column(
              width = 3,
              h5(paste0(length(unique(unlist(Vis$meta_upgma[input$upgma_fruit_variable_2]))), paste0(" categorical values")), style = "color: white; font-size: 14px; margin-top: 20px;  margin-left: 40px")        
            )
          }
        }
      } else {NULL}
    } else if (input$upgma_tile_num == 3) {
      if(!is.null(input$upgma_fruit_variable_3) & (!is.null(Vis$meta_upgma))) {
        if(is.numeric(unlist(Vis$meta_upgma[input$upgma_fruit_variable_3]))) {
          if(input$upgma_tiles_scale_3 %in% c('Spectral', 'RdYlGn', 'RdYlBu', 'RdGy', 'RdBu', 'PuOr', 'PRGn', 'PiYG', 'BrBG')) {
            column(
              width = 3,
              fluidRow(
                column(
                  width = 4,
                  h5("Midpoint", style = "color: white; margin-top: 22px;")
                ),
                column(
                  width = 8,
                  div(
                    class = "divmid-sel1",
                    selectInput(
                      "upgma_tiles_mapping_div_mid_3",
                      label = "",
                      choices = c("Zero", "Mean", "Median"),
                      selected = "Mean"
                    )
                  )
                )
              )
            )
          } else {
            column(
              width = 3,
              h5("Continous values", style = "color: white; font-size: 14px; margin-top: 20px; margin-left: 40px")
            ) 
          }
        } else {
          if(length(unique(unlist(Vis$meta_upgma[input$upgma_fruit_variable_3]))) > 7) {
            column(
              width = 3,
              h5(paste0("> 7 (", length(unique(unlist(Vis$meta_upgma[input$upgma_fruit_variable_3]))), ") categorical values"), style = "color: #E18B00; font-size: 12px; margin-top: 23px;  margin-left: 40px")        
            )
          } else {
            column(
              width = 3,
              h5(paste0(length(unique(unlist(Vis$meta_upgma[input$upgma_fruit_variable_3]))), paste0(" categorical values")), style = "color: white; font-size: 14px; margin-top: 20px;  margin-left: 40px")        
            )
          }
        }
      } else {NULL}
    } else if (input$upgma_tile_num == 4) {
      if(!is.null(input$upgma_fruit_variable_4) & (!is.null(Vis$meta_upgma))) {
        if(is.numeric(unlist(Vis$meta_upgma[input$upgma_fruit_variable_4]))) {
          if(input$upgma_tiles_scale_4 %in% c('Spectral', 'RdYlGn', 'RdYlBu', 'RdGy', 'RdBu', 'PuOr', 'PRGn', 'PiYG', 'BrBG')) {
            column(
              width = 3,
              fluidRow(
                column(
                  width = 4,
                  h5("Midpoint", style = "color: white; margin-top: 22px;")
                ),
                column(
                  width = 8,
                  div(
                    class = "divmid-sel1",
                    selectInput(
                      "upgma_tiles_mapping_div_mid_4",
                      label = "",
                      choices = c("Zero", "Mean", "Median"),
                      selected = "Mean"
                    )
                  )
                )
              )
            )
          } else {
            column(
              width = 3,
              h5("Continous values", style = "color: white; font-size: 14px; margin-top: 20px; margin-left: 40px")
            ) 
          }
        } else {
          if(length(unique(unlist(Vis$meta_upgma[input$upgma_fruit_variable_4]))) > 7) {
            column(
              width = 3,
              h5(paste0("> 7 (", length(unique(unlist(Vis$meta_upgma[input$upgma_fruit_variable_4]))), ") categorical values"), style = "color: #E18B00; font-size: 12px; margin-top: 23px;  margin-left: 40px")        
            )
          } else {
            column(
              width = 3,
              h5(paste0(length(unique(unlist(Vis$meta_upgma[input$upgma_fruit_variable_4]))), paste0(" categorical values")), style = "color: white; font-size: 14px; margin-top: 20px;  margin-left: 40px")        
            )
          }
        }
      } else {NULL}
    } else if (input$upgma_tile_num == 5) {
      if(!is.null(input$upgma_fruit_variable_5) & (!is.null(Vis$meta_upgma))) {
        if(is.numeric(unlist(Vis$meta_upgma[input$upgma_fruit_variable_5]))) {
          if(input$upgma_tiles_scale_5 %in% c('Spectral', 'RdYlGn', 'RdYlBu', 'RdGy', 'RdBu', 'PuOr', 'PRGn', 'PiYG', 'BrBG')) {
            column(
              width = 3,
              fluidRow(
                column(
                  width = 4,
                  h5("Midpoint", style = "color: white; margin-top: 22px;")
                ),
                column(
                  width = 8,
                  div(
                    class = "divmid-sel1",
                    selectInput(
                      "upgma_tiles_mapping_div_mid_5",
                      label = "",
                      choices = c("Zero", "Mean", "Median"),
                      selected = "Mean"
                    )
                  )
                )
              )
            )
          } else {
            column(
              width = 3,
              h5("Continous values", style = "color: white; font-size: 14px; margin-top: 20px; margin-left: 40px")
            ) 
          }
        } else {
          if(length(unique(unlist(Vis$meta_upgma[input$upgma_fruit_variable_5]))) > 7) {
            column(
              width = 3,
              h5(paste0("> 7 (", length(unique(unlist(Vis$meta_upgma[input$upgma_fruit_variable_5]))), ") categorical values"), style = "color: #E18B00; font-size: 12px; margin-top: 23px;  margin-left: 40px")        
            )
          } else {
            column(
              width = 3,
              h5(paste0(length(unique(unlist(Vis$meta_upgma[input$upgma_fruit_variable_5]))), paste0(" categorical values")), style = "color: white; font-size: 14px; margin-top: 20px;  margin-left: 40px")        
            )
          }
        }
      } else {NULL}
    }
  })
  
  output$nj_heatmap_mapping_info <- renderUI({
    if(!is.null(input$nj_heatmap_select) & (!is.null(Vis$meta_nj))) {
      if (any(sapply(Vis$meta_nj[input$nj_heatmap_select], is.numeric)) & 
          any(!sapply(Vis$meta_nj[input$nj_heatmap_select], is.numeric))) {
        column(
          width = 3,
          h5("Heatmap with categorical and continous values not possible", 
             style = "color: #E18B00; font-size: 12px; font-style: italic; margin-top: 15px;  margin-left: 40px")
        ) 
      } else {
        if(any(sapply(Vis$meta_nj[input$nj_heatmap_select], is.numeric))) {
          if(input$nj_heatmap_scale %in% c('Spectral', 'RdYlGn', 'RdYlBu', 'RdGy', 'RdBu', 'PuOr', 'PRGn', 'PiYG', 'BrBG')) {
            column(
              width = 3,
              fluidRow(
                column(
                  width = 4,
                  h5("Midpoint", style = "color: white; margin-top: 22px;")
                ),
                column(
                  width = 8,
                  div(
                    class = "divmid-sel1",
                    selectInput(
                      "nj_heatmap_div_mid",
                      label = "",
                      choices = c("Zero", "Mean", "Median"),
                      selected = "Mean"
                    )
                  )
                )
              )
            )
          } else {
            column(
              width = 3,
              h5("Continous values", style = "color: white; font-size: 14px; margin-top: 23px; margin-left: 40px")
            ) 
          }
        } else {
          if(length(unique(unlist(Vis$meta_nj[input$nj_heatmap_select]))) > 7) {
            column(
              width = 3,
              h5(paste0("> 7 categorical values"), style = "color: #E18B00; font-size: 12px; margin-top: 23px;  margin-left: 40px")        
            )
          } else {
            column(
              width = 3,
              h5("Categorical values", style = "color: white; font-size: 14px; margin-top: 20px;  margin-left: 40px")        
            )
          }
        }
      }
    } else {NULL}
  })
  
  output$upgma_heatmap_mapping_info <- renderUI({
    if(!is.null(input$upgma_heatmap_select) & (!is.null(Vis$meta_upgma))) {
      if (any(sapply(Vis$meta_upgma[input$upgma_heatmap_select], is.numeric)) & 
          any(!sapply(Vis$meta_upgma[input$upgma_heatmap_select], is.numeric))) {
        column(
          width = 3,
          h5("Heatmap with categorical and continous values not possible", 
             style = "color: #E18B00; font-size: 12px; font-style: italic; margin-top: 15px;  margin-left: 40px")
        ) 
      } else {
        if(any(sapply(Vis$meta_upgma[input$upgma_heatmap_select], is.numeric))) {
          if(input$upgma_heatmap_scale %in% c('Spectral', 'RdYlGn', 'RdYlBu', 'RdGy', 'RdBu', 'PuOr', 'PRGn', 'PiYG', 'BrBG')) {
            column(
              width = 3,
              fluidRow(
                column(
                  width = 4,
                  h5("Midpoint", style = "color: white; margin-top: 22px;")
                ),
                column(
                  width = 8,
                  div(
                    class = "divmid-sel1",
                    selectInput(
                      "upgma_heatmap_div_mid",
                      label = "",
                      choices = c("Zero", "Mean", "Median"),
                      selected = "Mean"
                    )
                  )
                )
              )
            )
          } else {
            column(
              width = 3,
              h5("Continous values", style = "color: white; font-size: 14px; margin-top: 23px; margin-left: 40px")
            ) 
          }
        } else {
          if(length(unique(unlist(Vis$meta_upgma[input$upgma_heatmap_select]))) > 7) {
            column(
              width = 3,
              h5(paste0("> 7 categorical values"), style = "color: #E18B00; font-size: 12px; margin-top: 23px;  margin-left: 40px")        
            )
          } else {
            column(
              width = 3,
              h5("Categorical values", style = "color: white; font-size: 14px; margin-top: 20px;  margin-left: 40px")        
            )
          }
        }
      }
    } else {NULL}
  })
  
  # Tiles offset
  output$nj_fruit_offset_circ <- renderUI({
    if(!is.null(input$nj_layout)) {
      if(input$nj_layout == "circular" | input$nj_layout == "inward") {
        offset <- 0.15
        step <- 0.03
        min <- -0.6
        max <- 0.6
      } else {
        offset <- 0.05
        step <- 0.01
        min <- -0.2
        max <- 0.2
      }
      
      sliderInput(
        "nj_fruit_offset_circ",
        label = "",
        min = min,
        max = max,
        step= step,
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "nj_fruit_offset_circ",
        label = "",
        min = -0.2,
        max = 0.2,
        step= 0.01,
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  output$upgma_fruit_offset_circ <- renderUI({
    if(!is.null(input$upgma_layout)) {
      if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
        offset <- 0.15
        step <- 0.1
        min <- -0.6
        max <- 0.6
      } else {
        offset <- 0.05
        step <- 0.05
        min <- -0.2
        max <- 0.2
      }
      
      sliderInput(
        "upgma_fruit_offset_circ",
        label = "",
        min = min,
        max = max,
        step= step,
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "upgma_fruit_offset_circ",
        label = "",
        min = -0.2,
        max = 0.2,
        step= 0.05,
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  output$nj_fruit_offset_circ_2 <- renderUI({
    if(!is.null(input$nj_layout)) {
      if(input$nj_layout == "circular" | input$nj_layout == "inward") {
        offset <- 0.15
        step <- 0.03
        min <- -0.6
        max <- 0.6
      } else {
        offset <- 0.05
        step <- 0.01
        min <- -0.2
        max <- 0.2
      }
      
      sliderInput(
        "nj_fruit_offset_circ_2",
        label = "",
        min = min,
        max = max,
        step= step,
        value = offset,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "nj_fruit_offset_circ_2",
        label = "",
        min = -0.2,
        max = 0.2,
        step= 0.01,
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  output$upgma_fruit_offset_circ_2 <- renderUI({
    if(!is.null(input$upgma_layout)) {
      if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
        offset <- 0.15
        step <- 0.03
        min <- -0.6
        max <- 0.6
      } else {
        offset <- 0.05
        step <- 0.01
        min <- -0.2
        max <- 0.2
      }
      
      sliderInput(
        "upgma_fruit_offset_circ_2",
        label = "",
        min = min,
        max = max,
        step= step,
        value = offset,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "upgma_fruit_offset_circ_2",
        label = "",
        min = -0.2,
        max = 0.2,
        step= 0.01,
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  output$nj_fruit_offset_circ_3 <- renderUI({
    if(!is.null(input$nj_layout)) {
      if(input$nj_layout == "circular" | input$nj_layout == "inward") {
        offset <- 0.15
        step <- 0.03
        min <- -0.6
        max <- 0.6
      } else {
        offset <- 0.05
        step <- 0.01
        min <- -0.2
        max <- 0.2
      }
      
      sliderInput(
        "nj_fruit_offset_circ_3",
        label = "",
        min = min,
        max = max,
        step= step,
        value = offset,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "nj_fruit_offset_circ_3",
        label = "",
        min = -0.2,
        max = 0.2,
        step= 0.01,
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  output$upgma_fruit_offset_circ_3 <- renderUI({
    if(!is.null(input$upgma_layout)) {
      if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
        offset <- 0.15
        step <- 0.03
        min <- -0.6
        max <- 0.6
      } else {
        offset <- 0.05
        step <- 0.01
        min <- -0.2
        max <- 0.2
      }
      
      sliderInput(
        "upgma_fruit_offset_circ_3",
        label = "",
        min = min,
        max = max,
        step= step,
        value = offset,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "upgma_fruit_offset_circ_3",
        label = "",
        min = -0.2,
        max = 0.2,
        step= 0.01,
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  output$nj_fruit_offset_circ_4 <- renderUI({
    if(!is.null(input$nj_layout)) {
      if(input$nj_layout == "circular" | input$nj_layout == "inward") {
        offset <- 0.15
        step <- 0.03
        min <- -0.6
        max <- 0.6
      } else {
        offset <- 0.05
        step <- 0.01
        min <- -0.2
        max <- 0.2
      }
      
      sliderInput(
        "nj_fruit_offset_circ_4",
        label = "",
        min = min,
        max = max,
        step= step,
        value = offset,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "nj_fruit_offset_circ_4",
        label = "",
        min = -0.2,
        max = 0.2,
        step= 0.01,
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  output$upgma_fruit_offset_circ_4 <- renderUI({
    if(!is.null(input$upgma_layout)) {
      if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
        offset <- 0.15
        step <- 0.03
        min <- -0.6
        max <- 0.6
      } else {
        offset <- 0.05
        step <- 0.01
        min <- -0.2
        max <- 0.2
      }
      
      sliderInput(
        "upgma_fruit_offset_circ_4",
        label = "",
        min = min,
        max = max,
        step= step,
        value = offset,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "upgma_fruit_offset_circ_4",
        label = "",
        min = -0.2,
        max = 0.2,
        step= 0.01,
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  output$nj_fruit_offset_circ_5 <- renderUI({
    if(!is.null(input$nj_layout)) {
      if(input$nj_layout == "circular" | input$nj_layout == "inward") {
        offset <- 0.15
        step <- 0.03
        min <- -0.6
        max <- 0.6
      } else {
        offset <- 0.05
        step <- 0.01
        min <- -0.2
        max <- 0.2
      }
      
      sliderInput(
        "nj_fruit_offset_circ_5",
        label = "",
        min = min,
        max = max,
        step= step,
        value = offset,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "nj_fruit_offset_circ_5",
        label = "",
        min = -0.2,
        max = 0.2,
        step= 0.01,
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  output$upgma_fruit_offset_circ_5 <- renderUI({
    if(!is.null(input$upgma_layout)) {
      if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
        offset <- 0.15
        step <- 0.03
        min <- -0.6
        max <- 0.6
      } else {
        offset <- 0.05
        step <- 0.01
        min <- -0.2
        max <- 0.2
      }
      
      sliderInput(
        "upgma_fruit_offset_circ_5",
        label = "",
        min = min,
        max = max,
        step= step,
        value = offset,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "upgma_fruit_offset_circ_5",
        label = "",
        min = -0.2,
        max = 0.2,
        step= 0.01,
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  # For Layout change update tiles offset position
  observeEvent(input$nj_layout, {
    if(input$nj_layout == "circular" | input$nj_layout == "inward") {
      offset <- 0.05
      step <- 0.01
      min <- -0.2
      max <- 0.2
    } else {
      offset <- 0.15
      step <- 0.03
      min <- -0.6
      max <- 0.6
    }
    
    updateSliderInput(session, "nj_fruit_offset_circ", min = min, step = step, max = max)
    updateSliderInput(session, "nj_fruit_offset_circ_2", min = min, step = step, max = max, value = offset)
    updateSliderInput(session, "nj_fruit_offset_circ_3", min = min, step = step, max = max, value = offset)
    updateSliderInput(session, "nj_fruit_offset_circ_4", min = min, step = step, max = max, value = offset)
    updateSliderInput(session, "nj_fruit_offset_circ_5", min = min, step = step, max = max, value = offset) 
  })
  
  observeEvent(input$upgma_layout, {
    if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
      offset <- 0.05
      step <- 0.01
      min <- -0.2
      max <- 0.2
    } else {
      offset <- 0.15
      step <- 0.03
      min <- -0.6
      max <- 0.6
    }
    
    updateSliderInput(session, "upgma_fruit_offset_circ", min = min, step = step, max = max)
    updateSliderInput(session, "upgma_fruit_offset_circ_2", min = min, step = step, max = max, value = offset)
    updateSliderInput(session, "upgma_fruit_offset_circ_3", min = min, step = step, max = max, value = offset)
    updateSliderInput(session, "upgma_fruit_offset_circ_4", min = min, step = step, max = max, value = offset)
    updateSliderInput(session, "upgma_fruit_offset_circ_5", min = min, step = step, max = max, value = offset) 
  })
  
  # Heatmap width
  output$nj_heatmap_width <- renderUI({
    if(!is.null(input$nj_heatmap_select)) {
      length_input <- length(input$nj_heatmap_select)
      if((!(input$nj_layout == "circular")) & (!(input$nj_layout == "inward"))) {
        if(length_input < 3) {
          width <- 0.1
        } else {
          if (length_input >= 3 && length_input <= 50) {
            width <- min(0.15 + 0.05 * floor((length_input - 3) / 2), 1.5)
          } else {
            width <- 1.5
          }   
        }
      } else {
        if(length_input < 3) {
          width <- 0.3
        } else if (length_input >= 3 && length_input <= 27) {
          width <- min(0.6 + 0.2 * floor((length_input - 3) / 2), 1.5)
        } else {
          width <- 3
        }
      }
      
      sliderInput(
        "nj_heatmap_width",
        label = "",
        min = 0.05,
        max = 1.5,
        value = width,
        step = 0.05,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "nj_heatmap_width",
        label = "",
        min = 0.05,
        max = 1.5,
        value = 0.1,
        step = 0.05,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  output$upgma_heatmap_width <- renderUI({
    if(!is.null(input$upgma_heatmap_select)) {
      length_input <- length(input$upgma_heatmap_select)
      if((!(input$upgma_layout == "circular")) & (!(input$upgma_layout == "inward"))) {
        if(length_input < 3) {
          width <- 0.1
        } else {
          if (length_input >= 3 && length_input <= 50) {
            width <- min(0.15 + 0.05 * floor((length_input - 3) / 2), 1.5)
          } else {
            width <- 1.5
          }   
        }
      } else {
        if(length_input < 3) {
          width <- 0.3
        } else if (length_input >= 3 && length_input <= 27) {
          width <- min(0.6 + 0.2 * floor((length_input - 3) / 2), 1.5)
        } else {
          width <- 3
        }
      }
      
      sliderInput(
        "upgma_heatmap_width",
        label = "",
        min = 0.05,
        max = 1.5,
        value = width,
        step = 0.05,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "upgma_heatmap_width",
        label = "",
        min = 0.05,
        max = 1.5,
        value = 0.1,
        step = 0.05,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  # Update value if new variables added
  observeEvent(input$nj_heatmap_select, {
    length_input <- length(input$nj_heatmap_select)
    if((!(input$nj_layout == "circular")) & (!(input$nj_layout == "inward"))) {
      if(length_input < 3) {
        width <- 0.1
      } else {
        if (length_input >= 3 && length_input <= 50) {
          width <- min(0.15 + 0.05 * floor((length_input - 3) / 2), 1.5)
        } else {
          width <- 1.5
        }   
      }
    } else {
      if(length_input < 3) {
        width <- 0.3
      } else if (length_input >= 3 && length_input <= 27) {
        width <- min(0.6 + 0.2 * floor((length_input - 3) / 2), 1.5)
      } else {
        width <- 3
      }
    }
    updateSliderInput(session, "nj_heatmap_width", value = width)
  })
  
  observeEvent(input$upgma_heatmap_select, {
    length_input <- length(input$upgma_heatmap_select)
    if((!(input$upgma_layout == "circular")) & (!(input$upgma_layout == "inward"))) {
      if(length_input < 3) {
        width <- 0.1
      } else {
        if (length_input >= 3 && length_input <= 50) {
          width <- min(0.15 + 0.05 * floor((length_input - 3) / 2), 1.5)
        } else {
          width <- 1.5
        }   
      }
    } else {
      if(length_input < 3) {
        width <- 0.3
      } else if (length_input >= 3 && length_input <= 27) {
        width <- min(0.6 + 0.2 * floor((length_input - 3) / 2), 1.5)
      } else {
        width <- 3
      }
    }
    updateSliderInput(session, "upgma_heatmap_width", value = width)
  })
  
  # Update value if layout changed
  observeEvent(input$nj_layout, {
    length_input <- length(input$nj_heatmap_select)
    if((!(input$nj_layout == "circular")) & (!(input$nj_layout == "inward"))) {
      if(length_input < 3) {
        width <- 0.1
      } else {
        if (length_input >= 3 && length_input <= 50) {
          width <- min(0.15 + 0.05 * floor((length_input - 3) / 2), 1.5)
        } else {
          width <- 1.5
        }   
      }
    } else {
      if(length_input < 3) {
        width <- 0.3
      } else if (length_input >= 3 && length_input <= 27) {
        width <- min(0.6 + 0.2 * floor((length_input - 3) / 2), 1.5)
      } else {
        width <- 3
      }
    }
    updateSliderInput(session, "nj_heatmap_width", value = width)
  })
  
  observeEvent(input$upgma_layout, {
    length_input <- length(input$upgma_heatmap_select)
    if((!(input$upgma_layout == "circular")) & (!(input$upgma_layout == "inward"))) {
      if(length_input < 3) {
        width <- 0.1
      } else {
        if (length_input >= 3 && length_input <= 50) {
          width <- min(0.15 + 0.05 * floor((length_input - 3) / 2), 1.5)
        } else {
          width <- 1.5
        }   
      }
    } else {
      if(length_input < 3) {
        width <- 0.3
      } else if (length_input >= 3 && length_input <= 27) {
        width <- min(0.6 + 0.2 * floor((length_input - 3) / 2), 1.5)
      } else {
        width <- 3
      }
    }
    updateSliderInput(session, "upgma_heatmap_width", value = width)
  })
  
  # Heatmap column titles position
  observeEvent(input$nj_layout, {
    if(!(input$nj_layout == "inward" | input$nj_layout == "circular")) {
      updateSliderInput(session, "nj_colnames_y", value = -1)
    } else {
      updateSliderInput(session, "nj_colnames_y", value = 0)
    }
  })
  
  observeEvent(input$upgma_layout, {
    if(!(input$upgma_layout == "inward" | input$upgma_layout == "circular")) {
      updateSliderInput(session, "upgma_colnames_y", value = -1)
    } else {
      updateSliderInput(session, "upgma_colnames_y", value = 0)
    }
  })
  
  output$nj_colnames_y <- renderUI({
    if(!is.null(sum(DB$data$Include))) {
      if(input$nj_layout == "inward" | input$nj_layout == "circular") {
        min <- 0
        val <- 0
      } else {
        val <- -1
        if((sum(DB$data$Include) * -0.1) > -2) {
          min <- -2
        } else {
          min <- round(sum(DB$data$Include) * -0.1, 0)
        }
      }
      sliderInput(
        "nj_colnames_y",
        label = h5("Names Y-Position", style = "color:white; margin-bottom: 0px"),
        min = min,
        max = sum(DB$data$Include),
        value = val,
        step = 1,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "nj_colnames_y",
        label = h5("Names Y-Position", style = "color:white; margin-bottom: 0px"),
        min = -10,
        max = 10,
        value = 0,
        step = 1,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  output$upgma_colnames_y <- renderUI({
    if(!is.null(sum(DB$data$Include))) {
      if(input$upgma_layout == "inward" | input$upgma_layout == "circular") {
        min <- 0
        val <- 0
      } else {
        val <- -1
        if((sum(DB$data$Include) * -0.1) > -2) {
          min <- -2
        } else {
          min <- round(sum(DB$data$Include) * -0.1, 0)
        }
      }
      sliderInput(
        "upgma_colnames_y",
        label = h5("Names Y-Position", style = "color:white; margin-bottom: 0px"),
        min = min,
        max = sum(DB$data$Include),
        value = val,
        step = 1,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "upgma_colnames_y",
        label = h5("Names Y-Position", style = "color:white; margin-bottom: 0px"),
        min = -10,
        max = 10,
        value = 0,
        step = 1,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  # Heatmap column titles angle
  output$nj_colnames_angle <- renderUI({
    if(!is.null(input$nj_layout)) {
      if(input$nj_layout == "circular" | input$nj_layout == "inward") {
        angle <- 90
      } else {angle <- -90}
      sliderInput(
        "nj_colnames_angle",
        label = h5("Names Angle", style = "color:white; margin-bottom: 0px"),
        min = -90,
        max = 90,
        value = angle,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "nj_colnames_angle",
        label = h5("Names Angle", style = "color:white; margin-bottom: 0px"),
        min = -90,
        max = 90,
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  output$upgma_colnames_angle <- renderUI({
    if(!is.null(input$upgma_layout)) {
      if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
        angle <- 90
      } else {angle <- -90}
      sliderInput(
        "upgma_colnames_angle",
        label = h5("Names Angle", style = "color:white; margin-bottom: 0px"),
        min = -90,
        max = 90,
        value = angle,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "upgma_colnames_angle",
        label = h5("Names Angle", style = "color:white; margin-bottom: 0px"),
        min = -90,
        max = 90,
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  # Change heatmap column titles angle and label align when switching layout
  observeEvent(input$nj_layout, {
    if(input$nj_layout == "circular" | input$nj_layout == "inward"){
      angle <- 90
      val <- TRUE
    } else {
      angle <- -90
      val <- FALSE
    }
    updateSwitchInput(session, "nj_align", value = val)
    updateSliderInput(session, "nj_colnames_angle", value = angle)
  })
  
  observeEvent(input$upgma_layout, {
    if(input$upgma_layout == "circular" | input$upgma_layout == "inward"){
      angle <- 90
      val <- TRUE
    } else {
      angle <- -90
      val <- FALSE
    }
    updateSwitchInput(session, "upgma_align", value = val)
    updateSliderInput(session, "upgma_colnames_angle", value = angle)
  })
  
  # Tile number selector update each other
  observeEvent(input$nj_tile_num, {
    updateSelectInput(session, "nj_tile_number", selected = input$nj_tile_num)
  })
  
  observeEvent(input$nj_tile_number, {
    updateSelectInput(session, "nj_tile_num", selected = input$nj_tile_number)
  })
  
  observeEvent(input$nj_tipcolor_mapping_show, {
    updateCheckboxInput(session, "nj_tippoint_show", value = input$nj_tipcolor_mapping_show)
  })
  
  observeEvent(input$nj_tipshape_mapping_show, {
    updateCheckboxInput(session, "nj_tippoint_show", value = input$nj_tipshape_mapping_show)
  })
  
  observeEvent(input$nj_tippoint_show, {
    if(input$nj_tippoint_show == FALSE) {
      updateCheckboxInput(session, "nj_tipcolor_mapping_show", value = FALSE)
      updateCheckboxInput(session, "nj_tipshape_mapping_show", value = FALSE)
    }
  })
  
  observeEvent(input$upgma_tile_num, {
    updateSelectInput(session, "upgma_tile_number", selected = input$upgma_tile_num)
  })
  
  observeEvent(input$upgma_tile_number, {
    updateSelectInput(session, "upgma_tile_num", selected = input$upgma_tile_number)
  })
  
  observeEvent(input$upgma_tipcolor_mapping_show, {
    updateCheckboxInput(session, "upgma_tippoint_show", value = input$upgma_tipcolor_mapping_show)
  })
  
  observeEvent(input$upgma_tipshape_mapping_show, {
    updateCheckboxInput(session, "upgma_tippoint_show", value = input$upgma_tipshape_mapping_show)
  })
  
  observeEvent(input$upgma_tippoint_show, {
    if(input$upgma_tippoint_show == FALSE) {
      updateCheckboxInput(session, "upgma_tipcolor_mapping_show", value = FALSE)
      updateCheckboxInput(session, "upgma_tipshape_mapping_show", value = FALSE)
    }
  })
  
  # Clade coloring
  output$nj_clade_scale <- renderUI({
    if(length(input$nj_parentnode) <= 1) {
      fluidRow(
        column(
          width = 5,
          h5("Color", style = "color:white; position: relative; right: -15px; margin-top: 30px")
        ),
        column(
          width = 7,
          align = "center",
          colorPickr(
            inputId = "nj_clade_scale",
            selected = "#D0F221",
            label = "",
            update = "changestop",
            interaction = list(clear = FALSE,
                               save = FALSE),
            position = "right-start",
            width = "100%"
          )
        )
      )
    } else {
      fluidRow(
        column(
          width = 5,
          h5("Scale", style = "color:white; position: relative; right: -15px; margin-top: 30px")
        ),
        column(
          width = 7,
          align = "center",
          div(
            class = "sel-clade-scale",
            selectInput(
              "nj_clade_scale",
              "",
              choices = list(
                Qualitative = list(
                  "Set1",
                  "Set2",
                  "Set3",
                  "Pastel1",
                  "Pastel2",
                  "Paired",
                  "Dark2",
                  "Accent"
                ),
                Sequential = list(
                  "YlOrRd",
                  "YlOrBr",
                  "YlGnBu",
                  "YlGn",
                  "Reds",
                  "RdPu",
                  "Purples",
                  "PuRd",
                  "PuBuGn",
                  "PuBu",
                  "OrRd",
                  "Oranges",
                  "Greys",
                  "Greens",
                  "GnBu",
                  "BuPu",
                  "BuGn",
                  "Blues"
                )
              )
            )
          )
        )
      )
    }
  })
  
  output$upgma_clade_scale <- renderUI({
    if(length(input$upgma_parentnode) <= 1) {
      fluidRow(
        column(
          width = 5,
          h5("Color", style = "color:white; position: relative; right: -15px; margin-top: 30px")
        ),
        column(
          width = 7,
          align = "center",
          colorPickr(
            inputId = "upgma_clade_scale",
            selected = "#D0F221",
            label = "",
            update = "changestop",
            interaction = list(clear = FALSE,
                               save = FALSE),
            position = "right-start",
            width = "100%"
          )
        )
      )
    } else {
      fluidRow(
        column(
          width = 5,
          h5("Scale", style = "color:white; position: relative; right: -15px; margin-top: 30px")
        ),
        column(
          width = 7,
          align = "center",
          div(
            class = "sel-clade-scale",
            selectInput(
              "upgma_clade_scale",
              "",
              choices = list(
                Qualitative = list(
                  "Set1",
                  "Set2",
                  "Set3",
                  "Pastel1",
                  "Pastel2",
                  "Paired",
                  "Dark2",
                  "Accent"
                ),
                Sequential = list(
                  "YlOrRd",
                  "YlOrBr",
                  "YlGnBu",
                  "YlGn",
                  "Reds",
                  "RdPu",
                  "Purples",
                  "PuRd",
                  "PuBuGn",
                  "PuBu",
                  "OrRd",
                  "Oranges",
                  "Greys",
                  "Greens",
                  "GnBu",
                  "BuPu",
                  "BuGn",
                  "Blues"
                )
              )
            )
          )
        )
      )
    }
  })
  
  # Heatmap variable color scale
  output$nj_heatmap_scale <- renderUI({
    if(class(unlist(Vis$meta_nj[input$nj_heatmap_select])) == "numeric") {
      selectInput(
        "nj_heatmap_scale",
        "",
        choices = list(
          Continous = list(
            "Magma" = "magma",
            "Inferno" = "inferno",
            "Plasma" = "plasma",
            "Viridis" = "viridis",
            "Cividis" = "cividis",
            "Rocket" = "rocket",
            "Mako" = "mako",
            "Turbo" = "turbo"
          ),
          Diverging = list(
            "Spectral",
            "RdYlGn",
            "RdYlBu",
            "RdGy",
            "RdBu",
            "PuOr",
            "PRGn",
            "PiYG",
            "BrBG"
          )
        )
      )
    } else {
      if(length(unique(unlist(Vis$meta_nj[input$nj_heatmap_select]))) > 7) {
        selectInput(
          "nj_heatmap_scale",
          "",
          choices = list(
            Gradient = list(
              "Magma" = "magma",
              "Inferno" = "inferno",
              "Plasma" = "plasma",
              "Viridis" = "viridis",
              "Cividis" = "cividis",
              "Rocket" = "rocket",
              "Mako" = "mako",
              "Turbo" = "turbo"
            )
          ),
          selected = "turbo"
        )
      } else {
        selectInput(
          "nj_heatmap_scale",
          "",
          choices = list(
            Qualitative = list(
              "Set1",
              "Set2",
              "Set3",
              "Pastel1",
              "Pastel2",
              "Paired",
              "Dark2",
              "Accent"
            ),
            Sequential = list(
              "YlOrRd",
              "YlOrBr",
              "YlGnBu",
              "YlGn",
              "Reds",
              "RdPu",
              "Purples",
              "PuRd",
              "PuBuGn",
              "PuBu",
              "OrRd",
              "Oranges",
              "Greys",
              "Greens",
              "GnBu",
              "BuPu",
              "BuGn",
              "Blues"
            )
          ),
          selected = "Paired"
        )
      }
    }
  })
  
  output$upgma_heatmap_scale <- renderUI({
    if(class(unlist(Vis$meta_upgma[input$upgma_heatmap_select])) == "numeric") {
      selectInput(
        "upgma_heatmap_scale",
        "",
        choices = list(
          Continous = list(
            "Magma" = "magma",
            "Inferno" = "inferno",
            "Plasma" = "plasma",
            "Viridis" = "viridis",
            "Cividis" = "cividis",
            "Rocket" = "rocket",
            "Mako" = "mako",
            "Turbo" = "turbo"
          ),
          Diverging = list(
            "Spectral",
            "RdYlGn",
            "RdYlBu",
            "RdGy",
            "RdBu",
            "PuOr",
            "PRGn",
            "PiYG",
            "BrBG"
          )
        )
      )
    } else {
      if(length(unique(unlist(Vis$meta_upgma[input$upgma_heatmap_select]))) > 7) {
        selectInput(
          "upgma_heatmap_scale",
          "",
          choices = list(
            Gradient = list(
              "Magma" = "magma",
              "Inferno" = "inferno",
              "Plasma" = "plasma",
              "Viridis" = "viridis",
              "Cividis" = "cividis",
              "Rocket" = "rocket",
              "Mako" = "mako",
              "Turbo" = "turbo"
            )
          ),
          selected = "turbo"
        )
      } else {
        selectInput(
          "upgma_heatmap_scale",
          "",
          choices = list(
            Qualitative = list(
              "Set1",
              "Set2",
              "Set3",
              "Pastel1",
              "Pastel2",
              "Paired",
              "Dark2",
              "Accent"
            ),
            Sequential = list(
              "YlOrRd",
              "YlOrBr",
              "YlGnBu",
              "YlGn",
              "Reds",
              "RdPu",
              "Purples",
              "PuRd",
              "PuBuGn",
              "PuBu",
              "OrRd",
              "Oranges",
              "Greys",
              "Greens",
              "GnBu",
              "BuPu",
              "BuGn",
              "Blues"
            )
          ),
          selected = "Paired"
        )
      }
    }
  })
  
  # Tiles variable color scale
  output$nj_tiles_scale_1 <- renderUI({
    if(class(unlist(Vis$meta_nj[input$nj_fruit_variable])) == "numeric") {
      selectInput(
        "nj_tiles_scale_1",
        "",
        choices = list(
          Continous = list(
            "Magma" = "magma",
            "Inferno" = "inferno",
            "Plasma" = "plasma",
            "Viridis" = "viridis",
            "Cividis" = "cividis",
            "Rocket" = "rocket",
            "Mako" = "mako",
            "Turbo" = "turbo"
          ),
          Diverging = list(
            "Spectral",
            "RdYlGn",
            "RdYlBu",
            "RdGy",
            "RdBu",
            "PuOr",
            "PRGn",
            "PiYG",
            "BrBG"
          )
        )
      )
    } else {
      if(length(unique(unlist(Vis$meta_nj[input$nj_fruit_variable]))) > 7) {
        selectInput(
          "nj_tiles_scale_1",
          "",
          choices = list(
            Gradient = list(
              "Magma" = "magma",
              "Inferno" = "inferno",
              "Plasma" = "plasma",
              "Viridis" = "viridis",
              "Cividis" = "cividis",
              "Rocket" = "rocket",
              "Mako" = "mako",
              "Turbo" = "turbo"
            )
          ),
          selected = "turbo"
        )
      } else {
        selectInput(
          "nj_tiles_scale_1",
          "",
          choices = list(
            Qualitative = list(
              "Set1",
              "Set2",
              "Set3",
              "Pastel1",
              "Pastel2",
              "Paired",
              "Dark2",
              "Accent"
            ),
            Sequential = list(
              "YlOrRd",
              "YlOrBr",
              "YlGnBu",
              "YlGn",
              "Reds",
              "RdPu",
              "Purples",
              "PuRd",
              "PuBuGn",
              "PuBu",
              "OrRd",
              "Oranges",
              "Greys",
              "Greens",
              "GnBu",
              "BuPu",
              "BuGn",
              "Blues"
            )
          ),
          selected = "Accent"
        )
      }
    }
  })
  
  output$upgma_tiles_scale_1 <- renderUI({
    if(class(unlist(Vis$meta_upgma[input$upgma_fruit_variable])) == "numeric") {
      selectInput(
        "upgma_tiles_scale_1",
        "",
        choices = list(
          Continous = list(
            "Magma" = "magma",
            "Inferno" = "inferno",
            "Plasma" = "plasma",
            "Viridis" = "viridis",
            "Cividis" = "cividis",
            "Rocket" = "rocket",
            "Mako" = "mako",
            "Turbo" = "turbo"
          ),
          Diverging = list(
            "Spectral",
            "RdYlGn",
            "RdYlBu",
            "RdGy",
            "RdBu",
            "PuOr",
            "PRGn",
            "PiYG",
            "BrBG"
          )
        )
      )
    } else {
      if(length(unique(unlist(Vis$meta_upgma[input$upgma_fruit_variable]))) > 7) {
        selectInput(
          "upgma_tiles_scale_1",
          "",
          choices = list(
            Gradient = list(
              "Magma" = "magma",
              "Inferno" = "inferno",
              "Plasma" = "plasma",
              "Viridis" = "viridis",
              "Cividis" = "cividis",
              "Rocket" = "rocket",
              "Mako" = "mako",
              "Turbo" = "turbo"
            )
          ),
          selected = "turbo"
        )
      } else {
        selectInput(
          "upgma_tiles_scale_1",
          "",
          choices = list(
            Qualitative = list(
              "Set1",
              "Set2",
              "Set3",
              "Pastel1",
              "Pastel2",
              "Paired",
              "Dark2",
              "Accent"
            ),
            Sequential = list(
              "YlOrRd",
              "YlOrBr",
              "YlGnBu",
              "YlGn",
              "Reds",
              "RdPu",
              "Purples",
              "PuRd",
              "PuBuGn",
              "PuBu",
              "OrRd",
              "Oranges",
              "Greys",
              "Greens",
              "GnBu",
              "BuPu",
              "BuGn",
              "Blues"
            )
          ),
          selected = "Accent"
        )
      }
    }
  })
  
  output$nj_tiles_scale_2 <- renderUI({
    if(class(unlist(Vis$meta_nj[input$nj_fruit_variable_2])) == "numeric") {
      selectInput(
        "nj_tiles_scale_2",
        "",
        choices = list(
          Continous = list(
            "Magma" = "magma",
            "Inferno" = "inferno",
            "Plasma" = "plasma",
            "Viridis" = "viridis",
            "Cividis" = "cividis",
            "Rocket" = "rocket",
            "Mako" = "mako",
            "Turbo" = "turbo"
          ),
          Diverging = list(
            "Spectral",
            "RdYlGn",
            "RdYlBu",
            "RdGy",
            "RdBu",
            "PuOr",
            "PRGn",
            "PiYG",
            "BrBG"
          )
        )
      )
    } else {
      if(length(unique(unlist(Vis$meta_nj[input$nj_fruit_variable_2]))) > 7) {
        selectInput(
          "nj_tiles_scale_2",
          "",
          choices = list(
            Gradient = list(
              "Magma" = "magma",
              "Inferno" = "inferno",
              "Plasma" = "plasma",
              "Viridis" = "viridis",
              "Cividis" = "cividis",
              "Rocket" = "rocket",
              "Mako" = "mako",
              "Turbo" = "turbo"
            )
          ),
          selected = "turbo"
        )
      } else {
        selectInput(
          "nj_tiles_scale_2",
          "",
          choices = list(
            Qualitative = list(
              "Set1",
              "Set2",
              "Set3",
              "Pastel1",
              "Pastel2",
              "Paired",
              "Dark2",
              "Accent"
            ),
            Sequential = list(
              "YlOrRd",
              "YlOrBr",
              "YlGnBu",
              "YlGn",
              "Reds",
              "RdPu",
              "Purples",
              "PuRd",
              "PuBuGn",
              "PuBu",
              "OrRd",
              "Oranges",
              "Greys",
              "Greens",
              "GnBu",
              "BuPu",
              "BuGn",
              "Blues"
            )
          ),
          selected = "Accent"
        )
      }
    }
  })
  
  output$upgma_tiles_scale_2 <- renderUI({
    if(class(unlist(Vis$meta_upgma[input$upgma_fruit_variable_2])) == "numeric") {
      selectInput(
        "upgma_tiles_scale_2",
        "",
        choices = list(
          Continous = list(
            "Magma" = "magma",
            "Inferno" = "inferno",
            "Plasma" = "plasma",
            "Viridis" = "viridis",
            "Cividis" = "cividis",
            "Rocket" = "rocket",
            "Mako" = "mako",
            "Turbo" = "turbo"
          ),
          Diverging = list(
            "Spectral",
            "RdYlGn",
            "RdYlBu",
            "RdGy",
            "RdBu",
            "PuOr",
            "PRGn",
            "PiYG",
            "BrBG"
          )
        )
      )
    } else {
      if(length(unique(unlist(Vis$meta_upgma[input$upgma_fruit_variable_2]))) > 7) {
        selectInput(
          "upgma_tiles_scale_2",
          "",
          choices = list(
            Gradient = list(
              "Magma" = "magma",
              "Inferno" = "inferno",
              "Plasma" = "plasma",
              "Viridis" = "viridis",
              "Cividis" = "cividis",
              "Rocket" = "rocket",
              "Mako" = "mako",
              "Turbo" = "turbo"
            )
          ),
          selected = "turbo"
        )
      } else {
        selectInput(
          "upgma_tiles_scale_2",
          "",
          choices = list(
            Qualitative = list(
              "Set1",
              "Set2",
              "Set3",
              "Pastel1",
              "Pastel2",
              "Paired",
              "Dark2",
              "Accent"
            ),
            Sequential = list(
              "YlOrRd",
              "YlOrBr",
              "YlGnBu",
              "YlGn",
              "Reds",
              "RdPu",
              "Purples",
              "PuRd",
              "PuBuGn",
              "PuBu",
              "OrRd",
              "Oranges",
              "Greys",
              "Greens",
              "GnBu",
              "BuPu",
              "BuGn",
              "Blues"
            )
          ),
          selected = "Accent"
        )
      }
    }
  })
  
  output$nj_tiles_scale_3 <- renderUI({
    if(class(unlist(Vis$meta_nj[input$nj_fruit_variable_3])) == "numeric") {
      selectInput(
        "nj_tiles_scale_3",
        "",
        choices = list(
          Continous = list(
            "Magma" = "magma",
            "Inferno" = "inferno",
            "Plasma" = "plasma",
            "Viridis" = "viridis",
            "Cividis" = "cividis",
            "Rocket" = "rocket",
            "Mako" = "mako",
            "Turbo" = "turbo"
          ),
          Diverging = list(
            "Spectral",
            "RdYlGn",
            "RdYlBu",
            "RdGy",
            "RdBu",
            "PuOr",
            "PRGn",
            "PiYG",
            "BrBG"
          )
        )
      )
    } else {
      if(length(unique(unlist(Vis$meta_nj[input$nj_fruit_variable_3]))) > 7) {
        selectInput(
          "nj_tiles_scale_3",
          "",
          choices = list(
            Gradient = list(
              "Magma" = "magma",
              "Inferno" = "inferno",
              "Plasma" = "plasma",
              "Viridis" = "viridis",
              "Cividis" = "cividis",
              "Rocket" = "rocket",
              "Mako" = "mako",
              "Turbo" = "turbo"
            )
          ),
          selected = "turbo"
        )
      } else {
        selectInput(
          "nj_tiles_scale_3",
          "",
          choices = list(
            Qualitative = list(
              "Set1",
              "Set2",
              "Set3",
              "Pastel1",
              "Pastel2",
              "Paired",
              "Dark2",
              "Accent"
            ),
            Sequential = list(
              "YlOrRd",
              "YlOrBr",
              "YlGnBu",
              "YlGn",
              "Reds",
              "RdPu",
              "Purples",
              "PuRd",
              "PuBuGn",
              "PuBu",
              "OrRd",
              "Oranges",
              "Greys",
              "Greens",
              "GnBu",
              "BuPu",
              "BuGn",
              "Blues"
            )
          ),
          selected = "Accent"
        )
      }
    }
  })
  
  output$upgma_tiles_scale_3 <- renderUI({
    if(class(unlist(Vis$meta_upgma[input$upgma_fruit_variable_3])) == "numeric") {
      selectInput(
        "upgma_tiles_scale_3",
        "",
        choices = list(
          Continous = list(
            "Magma" = "magma",
            "Inferno" = "inferno",
            "Plasma" = "plasma",
            "Viridis" = "viridis",
            "Cividis" = "cividis",
            "Rocket" = "rocket",
            "Mako" = "mako",
            "Turbo" = "turbo"
          ),
          Diverging = list(
            "Spectral",
            "RdYlGn",
            "RdYlBu",
            "RdGy",
            "RdBu",
            "PuOr",
            "PRGn",
            "PiYG",
            "BrBG"
          )
        )
      )
    } else {
      if(length(unique(unlist(Vis$meta_upgma[input$upgma_fruit_variable_3]))) > 7) {
        selectInput(
          "upgma_tiles_scale_3",
          "",
          choices = list(
            Gradient = list(
              "Magma" = "magma",
              "Inferno" = "inferno",
              "Plasma" = "plasma",
              "Viridis" = "viridis",
              "Cividis" = "cividis",
              "Rocket" = "rocket",
              "Mako" = "mako",
              "Turbo" = "turbo"
            )
          ),
          selected = "turbo"
        )
      } else {
        selectInput(
          "upgma_tiles_scale_3",
          "",
          choices = list(
            Qualitative = list(
              "Set1",
              "Set2",
              "Set3",
              "Pastel1",
              "Pastel2",
              "Paired",
              "Dark2",
              "Accent"
            ),
            Sequential = list(
              "YlOrRd",
              "YlOrBr",
              "YlGnBu",
              "YlGn",
              "Reds",
              "RdPu",
              "Purples",
              "PuRd",
              "PuBuGn",
              "PuBu",
              "OrRd",
              "Oranges",
              "Greys",
              "Greens",
              "GnBu",
              "BuPu",
              "BuGn",
              "Blues"
            )
          ),
          selected = "Accent"
        )
      }
    }
  })
  
  output$nj_tiles_scale_4 <- renderUI({
    if(class(unlist(Vis$meta_nj[input$nj_fruit_variable_4])) == "numeric") {
      selectInput(
        "nj_tiles_scale_4",
        "",
        choices = list(
          Continous = list(
            "Magma" = "magma",
            "Inferno" = "inferno",
            "Plasma" = "plasma",
            "Viridis" = "viridis",
            "Cividis" = "cividis",
            "Rocket" = "rocket",
            "Mako" = "mako",
            "Turbo" = "turbo"
          ),
          Diverging = list(
            "Spectral",
            "RdYlGn",
            "RdYlBu",
            "RdGy",
            "RdBu",
            "PuOr",
            "PRGn",
            "PiYG",
            "BrBG"
          )
        )
      )
    } else {
      if(length(unique(unlist(Vis$meta_nj[input$nj_fruit_variable_4]))) > 7) {
        selectInput(
          "nj_tiles_scale_4",
          "",
          choices = list(
            Gradient = list(
              "Magma" = "magma",
              "Inferno" = "inferno",
              "Plasma" = "plasma",
              "Viridis" = "viridis",
              "Cividis" = "cividis",
              "Rocket" = "rocket",
              "Mako" = "mako",
              "Turbo" = "turbo"
            )
          ),
          selected = "turbo"
        )
      } else {
        selectInput(
          "nj_tiles_scale_4",
          "",
          choices = list(
            Qualitative = list(
              "Set1",
              "Set2",
              "Set3",
              "Pastel1",
              "Pastel2",
              "Paired",
              "Dark2",
              "Accent"
            ),
            Sequential = list(
              "YlOrRd",
              "YlOrBr",
              "YlGnBu",
              "YlGn",
              "Reds",
              "RdPu",
              "Purples",
              "PuRd",
              "PuBuGn",
              "PuBu",
              "OrRd",
              "Oranges",
              "Greys",
              "Greens",
              "GnBu",
              "BuPu",
              "BuGn",
              "Blues"
            )
          ),
          selected = "Accent"
        )
      }
    }
  })
  
  output$upgma_tiles_scale_4 <- renderUI({
    if(class(unlist(Vis$meta_upgma[input$upgma_fruit_variable_4])) == "numeric") {
      selectInput(
        "upgma_tiles_scale_4",
        "",
        choices = list(
          Continous = list(
            "Magma" = "magma",
            "Inferno" = "inferno",
            "Plasma" = "plasma",
            "Viridis" = "viridis",
            "Cividis" = "cividis",
            "Rocket" = "rocket",
            "Mako" = "mako",
            "Turbo" = "turbo"
          ),
          Diverging = list(
            "Spectral",
            "RdYlGn",
            "RdYlBu",
            "RdGy",
            "RdBu",
            "PuOr",
            "PRGn",
            "PiYG",
            "BrBG"
          )
        )
      )
    } else {
      if(length(unique(unlist(Vis$meta_upgma[input$upgma_fruit_variable_4]))) > 7) {
        selectInput(
          "upgma_tiles_scale_4",
          "",
          choices = list(
            Gradient = list(
              "Magma" = "magma",
              "Inferno" = "inferno",
              "Plasma" = "plasma",
              "Viridis" = "viridis",
              "Cividis" = "cividis",
              "Rocket" = "rocket",
              "Mako" = "mako",
              "Turbo" = "turbo"
            )
          ),
          selected = "turbo"
        )
      } else {
        selectInput(
          "upgma_tiles_scale_4",
          "",
          choices = list(
            Qualitative = list(
              "Set1",
              "Set2",
              "Set3",
              "Pastel1",
              "Pastel2",
              "Paired",
              "Dark2",
              "Accent"
            ),
            Sequential = list(
              "YlOrRd",
              "YlOrBr",
              "YlGnBu",
              "YlGn",
              "Reds",
              "RdPu",
              "Purples",
              "PuRd",
              "PuBuGn",
              "PuBu",
              "OrRd",
              "Oranges",
              "Greys",
              "Greens",
              "GnBu",
              "BuPu",
              "BuGn",
              "Blues"
            )
          ),
          selected = "Accent"
        )
      }
    }
  })
  
  output$nj_tiles_scale_5 <- renderUI({
    if(class(unlist(Vis$meta_nj[input$nj_fruit_variable_5])) == "numeric") {
      selectInput(
        "nj_tiles_scale_5",
        "",
        choices = list(
          Continous = list(
            "Magma" = "magma",
            "Inferno" = "inferno",
            "Plasma" = "plasma",
            "Viridis" = "viridis",
            "Cividis" = "cividis",
            "Rocket" = "rocket",
            "Mako" = "mako",
            "Turbo" = "turbo"
          ),
          Diverging = list(
            "Spectral",
            "RdYlGn",
            "RdYlBu",
            "RdGy",
            "RdBu",
            "PuOr",
            "PRGn",
            "PiYG",
            "BrBG"
          )
        )
      )
    } else {
      if(length(unique(unlist(Vis$meta_nj[input$nj_fruit_variable_5]))) > 7) {
        selectInput(
          "nj_tiles_scale_5",
          "",
          choices = list(
            Gradient = list(
              "Magma" = "magma",
              "Inferno" = "inferno",
              "Plasma" = "plasma",
              "Viridis" = "viridis",
              "Cividis" = "cividis",
              "Rocket" = "rocket",
              "Mako" = "mako",
              "Turbo" = "turbo"
            )
          ),
          selected = "turbo"
        )
      } else {
        selectInput(
          "nj_tiles_scale_5",
          "",
          choices = list(
            Qualitative = list(
              "Set1",
              "Set2",
              "Set3",
              "Pastel1",
              "Pastel2",
              "Paired",
              "Dark2",
              "Accent"
            ),
            Sequential = list(
              "YlOrRd",
              "YlOrBr",
              "YlGnBu",
              "YlGn",
              "Reds",
              "RdPu",
              "Purples",
              "PuRd",
              "PuBuGn",
              "PuBu",
              "OrRd",
              "Oranges",
              "Greys",
              "Greens",
              "GnBu",
              "BuPu",
              "BuGn",
              "Blues"
            )
          ),
          selected = "Accent"
        )
      }
    }
  })
  
  output$upgma_tiles_scale_5 <- renderUI({
    if(class(unlist(Vis$meta_upgma[input$upgma_fruit_variable_5])) == "numeric") {
      selectInput(
        "upgma_tiles_scale_5",
        "",
        choices = list(
          Continous = list(
            "Magma" = "magma",
            "Inferno" = "inferno",
            "Plasma" = "plasma",
            "Viridis" = "viridis",
            "Cividis" = "cividis",
            "Rocket" = "rocket",
            "Mako" = "mako",
            "Turbo" = "turbo"
          ),
          Diverging = list(
            "Spectral",
            "RdYlGn",
            "RdYlBu",
            "RdGy",
            "RdBu",
            "PuOr",
            "PRGn",
            "PiYG",
            "BrBG"
          )
        )
      )
    } else {
      if(length(unique(unlist(Vis$meta_upgma[input$upgma_fruit_variable_5]))) > 7) {
        selectInput(
          "upgma_tiles_scale_5",
          "",
          choices = list(
            Gradient = list(
              "Magma" = "magma",
              "Inferno" = "inferno",
              "Plasma" = "plasma",
              "Viridis" = "viridis",
              "Cividis" = "cividis",
              "Rocket" = "rocket",
              "Mako" = "mako",
              "Turbo" = "turbo"
            )
          ),
          selected = "turbo"
        )
      } else {
        selectInput(
          "upgma_tiles_scale_5",
          "",
          choices = list(
            Qualitative = list(
              "Set1",
              "Set2",
              "Set3",
              "Pastel1",
              "Pastel2",
              "Paired",
              "Dark2",
              "Accent"
            ),
            Sequential = list(
              "YlOrRd",
              "YlOrBr",
              "YlGnBu",
              "YlGn",
              "Reds",
              "RdPu",
              "Purples",
              "PuRd",
              "PuBuGn",
              "PuBu",
              "OrRd",
              "Oranges",
              "Greys",
              "Greens",
              "GnBu",
              "BuPu",
              "BuGn",
              "Blues"
            )
          ),
          selected = "Accent"
        )
      }
    }
  })
  
  # Tip Labels Variable Color Scale
  output$nj_tiplab_scale <- renderUI({
    if(class(unlist(Vis$meta_nj[input$nj_color_mapping])) == "numeric") {
      selectInput(
        "nj_tiplab_scale",
        "",
        choices = list(
          Continous = list(
            "Magma" = "magma",
            "Inferno" = "inferno",
            "Plasma" = "plasma",
            "Viridis" = "viridis",
            "Cividis" = "cividis",
            "Rocket" = "rocket",
            "Mako" = "mako",
            "Turbo" = "turbo"
          ),
          Diverging = list(
            "Spectral",
            "RdYlGn",
            "RdYlBu",
            "RdGy",
            "RdBu",
            "PuOr",
            "PRGn",
            "PiYG",
            "BrBG"
          )
        )
      )
    } else {
      if(length(unique(unlist(Vis$meta_nj[input$nj_color_mapping]))) > 7) {
        selectInput(
          "nj_tiplab_scale",
          "",
          choices = list(
            Gradient = list(
              "Magma" = "magma",
              "Inferno" = "inferno",
              "Plasma" = "plasma",
              "Viridis" = "viridis",
              "Cividis" = "cividis",
              "Rocket" = "rocket",
              "Mako" = "mako",
              "Turbo" = "turbo"
            )
          ),
          selected = "turbo"
        )
      } else {
        selectInput(
          "nj_tiplab_scale",
          "",
          choices = list(
            Qualitative = list(
              "Set1",
              "Set2",
              "Set3",
              "Pastel1",
              "Pastel2",
              "Paired",
              "Dark2",
              "Accent"
            ),
            Sequential = list(
              "YlOrRd",
              "YlOrBr",
              "YlGnBu",
              "YlGn",
              "Reds",
              "RdPu",
              "Purples",
              "PuRd",
              "PuBuGn",
              "PuBu",
              "OrRd",
              "Oranges",
              "Greys",
              "Greens",
              "GnBu",
              "BuPu",
              "BuGn",
              "Blues"
            )
          )
        )
      }
    }
  })
  
  output$upgma_tiplab_scale <- renderUI({
    if(class(unlist(Vis$meta_upgma[input$upgma_color_mapping])) == "numeric") {
      selectInput(
        "upgma_tiplab_scale",
        "",
        choices = list(
          Continous = list(
            "Magma" = "magma",
            "Inferno" = "inferno",
            "Plasma" = "plasma",
            "Viridis" = "viridis",
            "Cividis" = "cividis",
            "Rocket" = "rocket",
            "Mako" = "mako",
            "Turbo" = "turbo"
          ),
          Diverging = list(
            "Spectral",
            "RdYlGn",
            "RdYlBu",
            "RdGy",
            "RdBu",
            "PuOr",
            "PRGn",
            "PiYG",
            "BrBG"
          )
        )
      )
    } else {
      if(length(unique(unlist(Vis$meta_upgma[input$upgma_color_mapping]))) > 7) {
        selectInput(
          "upgma_tiplab_scale",
          "",
          choices = list(
            Gradient = list(
              "Magma" = "magma",
              "Inferno" = "inferno",
              "Plasma" = "plasma",
              "Viridis" = "viridis",
              "Cividis" = "cividis",
              "Rocket" = "rocket",
              "Mako" = "mako",
              "Turbo" = "turbo"
            )
          ),
          selected = "turbo"
        )
      } else {
        selectInput(
          "upgma_tiplab_scale",
          "",
          choices = list(
            Qualitative = list(
              "Set1",
              "Set2",
              "Set3",
              "Pastel1",
              "Pastel2",
              "Paired",
              "Dark2",
              "Accent"
            ),
            Sequential = list(
              "YlOrRd",
              "YlOrBr",
              "YlGnBu",
              "YlGn",
              "Reds",
              "RdPu",
              "Purples",
              "PuRd",
              "PuBuGn",
              "PuBu",
              "OrRd",
              "Oranges",
              "Greys",
              "Greens",
              "GnBu",
              "BuPu",
              "BuGn",
              "Blues"
            )
          )
        )
      }
    }
  })
  
  # Tippoint Scale
  output$nj_tippoint_scale <- renderUI({
    if(!is.null(Vis$meta_nj)) {
      if(class(unlist(Vis$meta_nj[input$nj_tipcolor_mapping])) == "numeric") {
        selectInput(
          "nj_tippoint_scale",
          "",
          choices = list(
            Continous = list(
              "Magma" = "magma",
              "Inferno" = "inferno",
              "Plasma" = "plasma",
              "Viridis" = "viridis",
              "Cividis" = "cividis",
              "Rocket" = "rocket",
              "Mako" = "mako",
              "Turbo" = "turbo"
            ),
            Diverging = list(
              "Spectral",
              "RdYlGn",
              "RdYlBu",
              "RdGy",
              "RdBu",
              "PuOr",
              "PRGn",
              "PiYG",
              "BrBG"
            )
          )
        )
      } else {
        if(length(unique(unlist(Vis$meta_nj[input$nj_tipcolor_mapping]))) > 7) {
          selectInput(
            "nj_tippoint_scale",
            "",
            choices = list(
              Gradient = list(
                "Magma" = "magma",
                "Inferno" = "inferno",
                "Plasma" = "plasma",
                "Viridis" = "viridis",
                "Cividis" = "cividis",
                "Rocket" = "rocket",
                "Mako" = "mako",
                "Turbo" = "turbo"
              )
            ),
            selected = "turbo"
          )
        } else {
          selectInput(
            "nj_tippoint_scale",
            "",
            choices = list(
              Qualitative = list(
                "Set1",
                "Set2",
                "Set3",
                "Pastel1",
                "Pastel2",
                "Paired",
                "Dark2",
                "Accent"
              ),
              Sequential = list(
                "YlOrRd",
                "YlOrBr",
                "YlGnBu",
                "YlGn",
                "Reds",
                "RdPu",
                "Purples",
                "PuRd",
                "PuBuGn",
                "PuBu",
                "OrRd",
                "Oranges",
                "Greys",
                "Greens",
                "GnBu",
                "BuPu",
                "BuGn",
                "Blues"
              )
            ),
            selected = "Set2"
          )
        }
      }
    } else {
      selectInput(
        "nj_tippoint_scale",
        "",
        choices = list(
          Qualitative = list(
            "Set1",
            "Set2",
            "Set3",
            "Pastel1",
            "Pastel2",
            "Paired",
            "Dark2",
            "Accent"
          ),
          Sequential = list(
            "YlOrRd",
            "YlOrBr",
            "YlGnBu",
            "YlGn",
            "Reds",
            "RdPu",
            "Purples",
            "PuRd",
            "PuBuGn",
            "PuBu",
            "OrRd",
            "Oranges",
            "Greys",
            "Greens",
            "GnBu",
            "BuPu",
            "BuGn",
            "Blues"
          )
        ),
        selected = "Set2"
      )
    }
  })
  
  output$upgma_tippoint_scale <- renderUI({
    if(!is.null(Vis$meta_upgma)) {
      if(class(unlist(Vis$meta_upgma[input$upgma_tipcolor_mapping])) == "numeric") {
        selectInput(
          "upgma_tippoint_scale",
          "",
          choices = list(
            Continous = list(
              "Magma" = "magma",
              "Inferno" = "inferno",
              "Plasma" = "plasma",
              "Viridis" = "viridis",
              "Cividis" = "cividis",
              "Rocket" = "rocket",
              "Mako" = "mako",
              "Turbo" = "turbo"
            ),
            Diverging = list(
              "Spectral",
              "RdYlGn",
              "RdYlBu",
              "RdGy",
              "RdBu",
              "PuOr",
              "PRGn",
              "PiYG",
              "BrBG"
            )
          ),
          selected = c("Viridis" = "viridis")
        )
      } else {
        if(length(unique(unlist(Vis$meta_upgma[input$upgma_tipcolor_mapping]))) > 7) {
          selectInput(
            "upgma_tippoint_scale",
            "",
            choices = list(
              Gradient = list(
                "Magma" = "magma",
                "Inferno" = "inferno",
                "Plasma" = "plasma",
                "Viridis" = "viridis",
                "Cividis" = "cividis",
                "Rocket" = "rocket",
                "Mako" = "mako",
                "Turbo" = "turbo"
              )
            ),
            selected = "turbo"
          )
        } else {
          selectInput(
            "upgma_tippoint_scale",
            "",
            choices = list(
              Qualitative = list(
                "Set1",
                "Set2",
                "Set3",
                "Pastel1",
                "Pastel2",
                "Paired",
                "Dark2",
                "Accent"
              ),
              Sequential = list(
                "YlOrRd",
                "YlOrBr",
                "YlGnBu",
                "YlGn",
                "Reds",
                "RdPu",
                "Purples",
                "PuRd",
                "PuBuGn",
                "PuBu",
                "OrRd",
                "Oranges",
                "Greys",
                "Greens",
                "GnBu",
                "BuPu",
                "BuGn",
                "Blues"
              )
            ),
            selected = "Set2"
          )
        }
      }
    } else {
      selectInput(
        "upgma_tippoint_scale",
        "",
        choices = list(
          Qualitative = list(
            "Set1",
            "Set2",
            "Set3",
            "Pastel1",
            "Pastel2",
            "Paired",
            "Dark2",
            "Accent"
          ),
          Sequential = list(
            "YlOrRd",
            "YlOrBr",
            "YlGnBu",
            "YlGn",
            "Reds",
            "RdPu",
            "Purples",
            "PuRd",
            "PuBuGn",
            "PuBu",
            "OrRd",
            "Oranges",
            "Greys",
            "Greens",
            "GnBu",
            "BuPu",
            "BuGn",
            "Blues"
          )
        ),
        selected = "Set2"
      )
    }
  })
  
  # Clade Highlights
  output$nj_parentnode <- renderUI({
    if(!is.null(Vis$nj_parentnodes)) {
      pickerInput(
        "nj_parentnode",
        label = "",
        choices = sort(unique(as.numeric(Vis$nj_parentnodes))),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `noneSelectedText` = "Test",
          size = 10,
          style = "background-color: white; border-radius: 5px;"
        ),
        width = "99%"
      )
    } else {
      pickerInput(
        "nj_parentnode",
        label = "",
        choices = c(),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          noneSelectedText = "Test",
          size = 10,
          style = "background-color: white; border-radius: 5px;"
        ),
        width = "99%"
      )
    }
  })
  
  output$upgma_parentnode <- renderUI({
    if(!is.null(Vis$upgma_parentnodes)) {
      pickerInput(
        "upgma_parentnode",
        label = "",
        choices = sort(unique(as.numeric(Vis$upgma_parentnodes))),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `noneSelectedText` = "Test",
          size = 10,
          style = "background-color: white; border-radius: 5px;"
        ),
        width = "99%"
      )
    } else {
      pickerInput(
        "upgma_parentnode",
        label = "",
        choices = c(),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          noneSelectedText = "Test",
          size = 10,
          style = "background-color: white; border-radius: 5px;"
        ),
        width = "99%"
      )
    }
  })
  
  # Branch label size
  output$nj_branch_size <- renderUI(
    numericInput(
      "nj_branch_size",
      label = h5("Size", style = "color:white; margin-bottom: 0px"),
      min = 2,
      max = 10,
      step = 0.5,
      value = Vis$branch_size_nj,
      width = "80px"
    )
  )
  
  output$upgma_branch_size <- renderUI(
    numericInput(
      "upgma_branch_size",
      label = h5("Size", style = "color:white; margin-bottom: 0px"),
      min = 2,
      max = 10,
      step = 0.5,
      value = Vis$branch_size_upgma,
      width = "80px"
    )
  )
  
  # Tippanel size
  output$nj_tiplab_padding <- renderUI(
    if(!is.null(Vis$tiplab_padding_nj)) {
      sliderInput(
        inputId = "nj_tiplab_padding",
        label = h5("Size", style = "color:white; margin-bottom: 0px"),
        min = 0.05,
        max = 1,
        value = Vis$tiplab_padding_nj,
        step = 0.05,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        inputId = "nj_tiplab_padding",
        label = h5("Size", style = "color:white; margin-bottom: 0px"),
        min = 0.05,
        max = 1,
        value = 0.2,
        step = 0.05,
        width = "150px",
        ticks = FALSE
      )
    }
  )
  
  output$upgma_tiplab_padding <- renderUI(
    if(!is.null(Vis$tiplab_padding_upgma)) {
      sliderInput(
        inputId = "upgma_tiplab_padding",
        label = h5("Size", style = "color:white; margin-bottom: 0px"),
        min = 0.05,
        max = 1,
        value = Vis$tiplab_padding_upgma,
        step = 0.05,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        inputId = "upgma_tiplab_padding",
        label = h5("Size", style = "color:white; margin-bottom: 0px"),
        min = 0.05,
        max = 1,
        value = 0.2,
        step = 0.05,
        width = "150px",
        ticks = FALSE
      )
    }
  )
  
  # Nodepoint size
  output$nj_nodepoint_size <- renderUI(
    if(!is.null(Vis$nodepointsize_nj)) {
      sliderInput(
        inputId = "nj_nodepoint_size",
        label = h5("Size", style = "color:white; margin-bottom: 0px"),
        min = 1,
        max = 20,
        value = Vis$nodepointsize_nj,
        step = 0.5,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        inputId = "nj_nodepoint_size",
        label = h5("Size", style = "color:white; margin-bottom: 0px"),
        min = 1,
        max = 20,
        value = 2.5,
        step = 0.5,
        width = "150px",
        ticks = FALSE
      )
    }
  )
  
  output$upgma_nodepoint_size <- renderUI(
    if(!is.null(Vis$nodepointsize_upgma)) {
      sliderInput(
        inputId = "upgma_nodepoint_size",
        label = h5("Size", style = "color:white; margin-bottom: 0px"),
        min = 1,
        max = 20,
        value = Vis$nodepointsize_upgma,
        step = 0.5,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        inputId = "upgma_nodepoint_size",
        label = h5("Size", style = "color:white; margin-bottom: 0px"),
        min = 1,
        max = 20,
        value = 2.5,
        step = 0.5,
        width = "150px",
        ticks = FALSE
      )
    }
  )
  
  # Tippoint size 
  output$nj_tippoint_size <- renderUI(
    if(!is.null(Vis$tippointsize_nj)) {
      sliderInput(
        inputId = "nj_tippoint_size",
        label = h5("Size", style = "color:white; margin-bottom: 0px"),
        min = 1,
        max = 20,
        step = 0.5,
        value = Vis$tippointsize_nj,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        inputId = "nj_tippoint_size",
        label = h5("Size", style = "color:white; margin-bottom: 0px"),
        min = 1,
        max = 20,
        step = 0.5,
        value = 4,
        width = "150px",
        ticks = FALSE
      )
    }
  )
  
  output$upgma_tippoint_size <- renderUI(
    if(!is.null(Vis$tippointsize_upgma)) {
      sliderInput(
        inputId = "upgma_tippoint_size",
        label = h5("Size", style = "color:white; margin-bottom: 0px"),
        min = 1,
        max = 20,
        step = 0.5,
        value = Vis$tippointsize_upgma,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        inputId = "upgma_tippoint_size",
        label = h5("Size", style = "color:white; margin-bottom: 0px"),
        min = 1,
        max = 20,
        step = 0.5,
        value = 4,
        width = "150px",
        ticks = FALSE
      )
    }
  )
  
  # Tiplabel size
  output$nj_tiplab_size <- renderUI(
    if(!is.null(Vis$labelsize_nj)) {
      numericInput(
        "nj_tiplab_size",
        label = h5("Label size", style = "color:white; margin-bottom: 0px"),
        min = 1,
        max = 10,
        step = 0.5,
        value = Vis$labelsize_nj,
        width = "80px"
      )
    } else {
      numericInput(
        "nj_tiplab_size",
        label = h5("Label size", style = "color:white; margin-bottom: 0px"),
        min = 1,
        max = 10,
        step = 0.5,
        value = 4,
        width = "80px"
      )
    }
  )
  
  output$upgma_tiplab_size <- renderUI(
    if(!is.null(Vis$labelsize_upgma)) {
      numericInput(
        "upgma_tiplab_size",
        label = h5("Label size", style = "color:white; margin-bottom: 0px"),
        min = 1,
        max = 10,
        step = 0.5,
        value = Vis$labelsize_upgma,
        width = "80px"
      )
    } else {
      numericInput(
        "upgma_tiplab_size",
        label = h5("Label size", style = "color:white; margin-bottom: 0px"),
        min = 1,
        max = 10,
        step = 0.5,
        value = 4,
        width = "80px"
      )
    }
  )
  
  # Rootedge length 
  output$nj_rootedge_length <- renderUI({
    if(!is.null(Vis$nj_max_x)) {
      if(round(ceiling(Vis$nj_max_x) * 0.02, 0) < 1) {
        min <- 1
      } else {
        min <- round(ceiling(Vis$nj_max_x) * 0.02, 0)  
      }
      max <- round(ceiling(Vis$nj_max_x) * 0.2, 0)
      sliderInput(
        "nj_rootedge_length",
        label = h5("Rootedge Length", style = "color:white; margin-bottom: 0px"),
        min = min,
        max = max,
        value = round(ceiling(Vis$nj_max_x) * 0.05, 0),
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "nj_rootedge_length",
        label = h5("Length", style = "color:white; margin-bottom: 0px"),
        min = 1,
        max = 10,
        value = 2,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  output$upgma_rootedge_length <- renderUI({
    if(!is.null(Vis$upgma_max_x)) {
      if(round(ceiling(Vis$upgma_max_x) * 0.02, 0) < 1) {
        min <- 1
      } else {
        min <- round(ceiling(Vis$upgma_max_x) * 0.02, 0)  
      }
      max <- round(ceiling(Vis$upgma_max_x) * 0.2, 0)
      sliderInput(
        "upgma_rootedge_length",
        label = h5("Rootedge Length", style = "color:white; margin-bottom: 0px"),
        min = min,
        max = max,
        value = round(ceiling(Vis$upgma_max_x) * 0.05, 0),
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "upgma_rootedge_length",
        label = h5("Length", style = "color:white; margin-bottom: 0px"),
        min = 1,
        max = 10,
        value = 2,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  # Treescale 
  output$nj_treescale_width <- renderUI({
    if(!is.null(Vis$nj_max_x)) {
      numericInput(
        "nj_treescale_width",
        label = h5("Length", style = "color:white; margin-bottom: 0px"),
        value = round(ceiling(Vis$nj_max_x) * 0.1, 0),
        min = 1,
        max = round(floor(Vis$nj_max_x) * 0.5, 0),
        step = 1,
        width = "80px"
      )
    } else {
      numericInput(
        "nj_treescale_width",
        label = h5("Length", style = "color:white; margin-bottom: 0px"),
        value = 2,
        min = 1,
        max = 10,
        step = 1,
        width = "80px"
      )
    }
  })
  
  output$upgma_treescale_width <- renderUI({
    if(!is.null(Vis$upgma_max_x)) {
      numericInput(
        "upgma_treescale_width",
        label = h5("Length", style = "color:white; margin-bottom: 0px"),
        value = round(ceiling(Vis$upgma_max_x) * 0.1, 0),
        min = 1,
        max = round(floor(Vis$upgma_max_x) * 0.5, 0),
        step = 1,
        width = "80px"
      )
    } else {
      numericInput(
        "upgma_treescale_width",
        label = h5("Length", style = "color:white; margin-bottom: 0px"),
        value = 2,
        min = 1,
        max = 10,
        step = 1,
        width = "80px"
      )
    }
  })
  
  output$nj_treescale_x <- renderUI({
    if((!is.null(Vis$nj_min_x)) & (!is.null(Vis$nj_max_x))) {
      if(ceiling(Vis$nj_min_x) < 1) {
        floor <- 1
      } else {
        floor <- ceiling(Vis$nj_min_x)
      }
      sliderInput(
        "nj_treescale_x",
        label = h5("X Position", style = "color:white; margin-bottom: 0px"),
        min = floor,
        max = round(floor(Vis$nj_max_x)),
        value = round(ceiling(Vis$nj_max_x) * 0.2, 0),
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "nj_treescale_x",
        label = h5("X Position", style = "color:white; margin-bottom: 0px"),
        min = 1,
        max = 10,
        value = 2,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  output$upgma_treescale_x <- renderUI({
    if((!is.null(Vis$upgma_min_x)) & (!is.null(Vis$upgma_max_x))) {
      if(ceiling(Vis$upgma_min_x) < 1) {
        floor <- 1
      } else {
        floor <- ceiling(Vis$upgma_min_x)
      }
      sliderInput(
        "upgma_treescale_x",
        label = h5("X Position", style = "color:white; margin-bottom: 0px"),
        min = floor,
        max = round(floor(Vis$upgma_max_x)),
        value = round(ceiling(Vis$upgma_max_x) * 0.2, 0),
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "upgma_treescale_x",
        label = h5("X Position", style = "color:white; margin-bottom: 0px"),
        min = 1,
        max = 10,
        value = 2,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  output$nj_treescale_y <- renderUI({
    if(!is.null(sum(DB$data$Include))) {
      sliderInput(
        "nj_treescale_y",
        label = h5("Y Position", style = "color:white; margin-bottom: 0px"),
        min = 0,
        max = sum(DB$data$Include),
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "nj_treescale_y",
        label = h5("Y Position", style = "color:white; margin-bottom: 0px"),
        min = 0,
        max = 10,
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  output$upgma_treescale_y <- renderUI({
    if(!is.null(sum(DB$data$Include))) {
      sliderInput(
        "upgma_treescale_y",
        label = h5("Y Position", style = "color:white; margin-bottom: 0px"),
        min = 0,
        max = sum(DB$data$Include),
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "upgma_treescale_y",
        label = h5("Y Position", style = "color:white; margin-bottom: 0px"),
        min = 0,
        max = 10,
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  ### Heatmap 
  # Heatmap picker
  output$nj_heatmap_sel <- renderUI({
    if(!is.null(Vis$meta_nj)) {
      meta <- select(Vis$meta_nj, -c(taxa, Index, `Assembly ID`, `Assembly Name`,
                                     Scheme, `Typing Date`, Successes, Errors))
      
      # Identify numeric columns
      numeric_columns <- sapply(meta, is.numeric)
      
      numeric_column_names <- names(meta[numeric_columns])
      
      non_numeric_column_names <- names(meta)[!numeric_columns]
      
      choices <- list()
      
      # Add Continuous list only if there are numeric columns
      if (length(numeric_column_names) > 0) {
        choices$Continuous <- as.list(setNames(numeric_column_names, numeric_column_names))
      }
      
      # Add Diverging list
      choices$Categorical <- as.list(setNames(non_numeric_column_names, non_numeric_column_names))
      
      div(
        class = "heatmap-picker",
        pickerInput(
          inputId = "nj_heatmap_select",
          label = "",
          width = "100%",
          choices = if(ncol(Vis$meta_nj) == 11) {
            c(
              `Isolation Date` = "Isolation Date",
              Host = "Host",
              Country = "Country",
              City = "City"
            )
          } else {choices},
          options = list(
            `dropdown-align-center` = TRUE,
            size = 10,
            style = "background-color: white; border-radius: 5px;"
          ),
          multiple = TRUE
        )
      )
    } else {
      div(
        class = "heatmap-picker",
        pickerInput(
          inputId = "nj_heatmap_select",
          label = "",
          width = "100%",
          choices = c(
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          ),
          multiple = TRUE
        )
      )
    }
  })
  
  output$upgma_heatmap_sel <- renderUI({
    if(!is.null(Vis$meta_upgma)) {
      meta <- select(Vis$meta_upgma, -c(taxa, Index, `Assembly ID`, `Assembly Name`,
                                        Scheme, `Typing Date`, Successes, Errors))
      
      # Identify numeric columns
      numeric_columns <- sapply(meta, is.numeric)
      
      numeric_column_names <- names(meta[numeric_columns])
      
      non_numeric_column_names <- names(meta)[!numeric_columns]
      
      choices <- list()
      
      # Add Continuous list only if there are numeric columns
      if (length(numeric_column_names) > 0) {
        choices$Continuous <- as.list(setNames(numeric_column_names, numeric_column_names))
      }
      
      # Add Diverging list
      choices$Categorical <- as.list(setNames(non_numeric_column_names, non_numeric_column_names))
      
      div(
        class = "heatmap-picker",
        pickerInput(
          inputId = "upgma_heatmap_select",
          label = "",
          width = "100%",
          choices = if(ncol(Vis$meta_upgma) == 11) {
            c(
              `Isolation Date` = "Isolation Date",
              Host = "Host",
              Country = "Country",
              City = "City"
            )
          } else {choices},
          options = list(
            `dropdown-align-center` = TRUE,
            size = 10,
            style = "background-color: white; border-radius: 5px;"
          ),
          multiple = TRUE
        )
      )
    } else {
      div(
        class = "heatmap-picker",
        pickerInput(
          inputId = "upgma_heatmap_select",
          label = "",
          width = "100%",
          choices = c(
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          ),
          multiple = TRUE
        )
      )
    }
  })
  
  # Heatmap offset
  output$nj_heatmap_offset <- renderUI({
    if(!is.null(Vis$nj_max_x)) {
      sliderInput(
        "nj_heatmap_offset",
        label = "",
        min = 0,
        max = round(ceiling(Vis$nj_max_x)*1.5, 0),
        step = 1,
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "nj_heatmap_offset",
        label = "",
        min = 0,
        max = 10,
        step = 1,
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  output$upgma_heatmap_offset <- renderUI({
    if(!is.null(Vis$upgma_max_x)) {
      sliderInput(
        "upgma_heatmap_offset",
        label = "",
        min = 0,
        max = round(ceiling(Vis$upgma_max_x)*1.5, 0),
        step = 1,
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    } else {
      sliderInput(
        "upgma_heatmap_offset",
        label = "",
        min = 0,
        max = 10,
        step = 1,
        value = 0,
        width = "150px",
        ticks = FALSE
      )
    }
  })
  
  ### Tiling 
  # Geom Fruit select Variable
  output$nj_fruit_variable <- renderUI({
    if(!is.null(Vis$meta_nj)) {
      selectInput(
        "nj_fruit_variable",
        "",
        choices = if(ncol(Vis$meta_nj) == 11) {
          c(
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          )
        } else {
          append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
                 names(Vis$meta_nj)[13:ncol(Vis$meta_nj)])
        },
        selected = c(`Isolation Date` = "Isolation Date"),
        width = "100%"
      )
    } else {
      selectInput(
        "nj_fruit_variable",
        "",
        choices = c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      )
    }
  })
  
  output$nj_fruit_variable2 <- renderUI({
    if(!is.null(Vis$meta_nj)) {
      selectInput(
        "nj_fruit_variable_2",
        "",
        choices = if(ncol(Vis$meta_nj) == 11) {
          c(
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          )
        } else {
          append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
                 names(Vis$meta_nj)[13:ncol(Vis$meta_nj)])
        },
        selected = c(`Isolation Date` = "Isolation Date"),
        width = "100%"
      )
    } else {
      selectInput(
        "nj_fruit_variable_2",
        "",
        choices = c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      )
    }
  })
  
  output$nj_fruit_variable3 <- renderUI({
    if(!is.null(Vis$meta_nj)) {
      selectInput(
        "nj_fruit_variable_3",
        "",
        choices = if(ncol(Vis$meta_nj) == 11) {
          c(
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          )
        } else {
          append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
                 names(Vis$meta_nj)[13:ncol(Vis$meta_nj)])
        },
        selected = c(`Isolation Date` = "Isolation Date"),
        width = "100%"
      )
    } else {
      selectInput(
        "nj_fruit_variable_3",
        "",
        choices = c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      )
    }
  })
  
  output$nj_fruit_variable4 <- renderUI({
    if(!is.null(Vis$meta_nj)) {
      selectInput(
        "nj_fruit_variable_4",
        "",
        choices = if(ncol(Vis$meta_nj) == 11) {
          c(
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          )
        } else {
          append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
                 names(Vis$meta_nj)[13:ncol(Vis$meta_nj)])
        },
        selected = c(`Isolation Date` = "Isolation Date"),
        width = "100%"
      )
    } else {
      selectInput(
        "nj_fruit_variable_4",
        "",
        choices = c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      )
    }
  })
  
  output$nj_fruit_variable5 <- renderUI({
    if(!is.null(Vis$meta_nj)) {
      selectInput(
        "nj_fruit_variable_5",
        "",
        choices = if(ncol(Vis$meta_nj) == 11) {
          c(
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          )
        } else {
          append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
                 names(Vis$meta_nj)[13:ncol(Vis$meta_nj)])
        },
        selected = c(`Isolation Date` = "Isolation Date"),
        width = "100%"
      )
    } else {
      selectInput(
        "nj_fruit_variable_5",
        "",
        choices = c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      )
    }
  })
  
  output$upgma_fruit_variable <- renderUI({
    if(!is.null(Vis$meta_upgma)) {
      selectInput(
        "upgma_fruit_variable",
        "",
        choices = if(ncol(Vis$meta_upgma) == 11) {
          c(
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          )
        } else {
          append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
                 names(Vis$meta_upgma)[13:ncol(Vis$meta_upgma)])
        },
        selected = c(`Isolation Date` = "Isolation Date"),
        width = "100%"
      )
    } else {
      selectInput(
        "upgma_fruit_variable",
        "",
        choices = c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      )
    }
  })
  
  output$upgma_fruit_variable2 <- renderUI({
    if(!is.null(Vis$meta_upgma)) {
      selectInput(
        "upgma_fruit_variable_2",
        "",
        choices = if(ncol(Vis$meta_upgma) == 11) {
          c(
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          )
        } else {
          append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
                 names(Vis$meta_upgma)[13:ncol(Vis$meta_upgma)])
        },
        selected = c(`Isolation Date` = "Isolation Date"),
        width = "100%"
      )
    } else {
      selectInput(
        "upgma_fruit_variable_2",
        "",
        choices = c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      )
    }
  })
  
  output$upgma_fruit_variable3 <- renderUI({
    if(!is.null(Vis$meta_upgma)) {
      selectInput(
        "upgma_fruit_variable_3",
        "",
        choices = if(ncol(Vis$meta_upgma) == 11) {
          c(
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          )
        } else {
          append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
                 names(Vis$meta_upgma)[13:ncol(Vis$meta_upgma)])
        },
        selected = c(`Isolation Date` = "Isolation Date"),
        width = "100%"
      )
    } else {
      selectInput(
        "upgma_fruit_variable_3",
        "",
        choices = c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      )
    }
  })
  
  output$upgma_fruit_variable4 <- renderUI({
    if(!is.null(Vis$meta_upgma)) {
      selectInput(
        "upgma_fruit_variable_4",
        "",
        choices = if(ncol(Vis$meta_upgma) == 11) {
          c(
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          )
        } else {
          append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
                 names(Vis$meta_upgma)[13:ncol(Vis$meta_upgma)])
        },
        selected = c(`Isolation Date` = "Isolation Date"),
        width = "100%"
      )
    } else {
      selectInput(
        "upgma_fruit_variable_4",
        "",
        choices = c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      )
    }
  })
  
  output$upgma_fruit_variable5 <- renderUI({
    if(!is.null(Vis$meta_upgma)) {
      selectInput(
        "upgma_fruit_variable_5",
        "",
        choices = if(ncol(Vis$meta_upgma) == 11) {
          c(
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          )
        } else {
          append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
                 names(Vis$meta_upgma)[13:ncol(Vis$meta_upgma)])
        },
        selected = c(`Isolation Date` = "Isolation Date"),
        width = "100%"
      )
    } else {
      selectInput(
        "upgma_fruit_variable_5",
        "",
        choices = c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        )
      )
    }
  })
  
  # Geom Fruit Width
  output$nj_fruit_width <- renderUI({
    if((!is.null(Vis$nj_min_x)) & (!is.null(Vis$nj_max_x))) {
      if(round(ceiling(Vis$nj_max_x) * 0.1, 0) < 1) {
        if(input$nj_layout == "circular" | input$nj_layout == "inward") {
          width <- 3
        } else {
          width <- 1
        }
      } else {
        if(input$nj_layout == "circular" | input$nj_layout == "inward") {
          width <- round(ceiling(Vis$nj_max_x) * 0.033, 0) * 3
        } else {
          width <- round(ceiling(Vis$nj_max_x) * 0.033, 0)
        }
      }
      sliderInput(
        "nj_fruit_width_circ",
        label = "",
        min = 1,
        max = round(ceiling(Vis$nj_max_x) * 0.5, 0),
        value = width,
        width = "150px",
        ticks = FALSE
      )
    } else {
      if(input$nj_layout == "circular" | input$nj_layout == "inward") {
        sliderInput(
          "nj_fruit_width_circ",
          label = "",
          min = 1,
          max = 10,
          value = 3,
          width = "150px",
          ticks = FALSE
        )
      } else {
        sliderInput(
          "nj_fruit_width_circ",
          label = "",
          min = 1,
          max = 10,
          value = 1,
          width = "150px",
          ticks = FALSE
        )
      }
    }
  })
  
  output$nj_fruit_width2 <- renderUI({
    if((!is.null(Vis$nj_min_x)) & (!is.null(Vis$nj_max_x))) {
      if(round(ceiling(Vis$nj_max_x) * 0.1, 0) < 1) {
        if(input$nj_layout == "circular" | input$nj_layout == "inward") {
          width <- 3
        } else {
          width <- 1
        }
      } else {
        if(input$nj_layout == "circular" | input$nj_layout == "inward") {
          width <- round(ceiling(Vis$nj_max_x) * 0.033, 0) * 3
        } else {
          width <- round(ceiling(Vis$nj_max_x) * 0.033, 0)
        }
      }
      sliderInput(
        "nj_fruit_width_circ_2",
        label = "",
        min = 1,
        max = round(ceiling(Vis$nj_max_x) * 0.5, 0),
        value = width,
        width = "150px",
        ticks = FALSE
      )
    } else {
      if(input$nj_layout == "circular" | input$nj_layout == "inward") {
        sliderInput(
          "nj_fruit_width_circ_2",
          label = "",
          min = 1,
          max = 10,
          value = 3,
          width = "150px",
          ticks = FALSE
        )
      } else {
        sliderInput(
          "nj_fruit_width_circ_2",
          label = "",
          min = 1,
          max = 10,
          value = 1,
          width = "150px",
          ticks = FALSE
        )
      }
    }
  })
  
  output$nj_fruit_width3 <- renderUI({
    if((!is.null(Vis$nj_min_x)) & (!is.null(Vis$nj_max_x))) {
      if(round(ceiling(Vis$nj_max_x) * 0.1, 0) < 1) {
        if(input$nj_layout == "circular" | input$nj_layout == "inward") {
          width <- 3
        } else {
          width <- 1
        }
      } else {
        if(input$nj_layout == "circular" | input$nj_layout == "inward") {
          width <- round(ceiling(Vis$nj_max_x) * 0.033, 0) * 3
        } else {
          width <- round(ceiling(Vis$nj_max_x) * 0.033, 0)
        }
      }
      sliderInput(
        "nj_fruit_width_circ_3",
        label = "",
        min = 1,
        max = round(ceiling(Vis$nj_max_x) * 0.5, 0),
        value = width,
        width = "150px",
        ticks = FALSE
      )
    } else {
      if(input$nj_layout == "circular" | input$nj_layout == "inward") {
        sliderInput(
          "nj_fruit_width_circ_3",
          label = "",
          min = 1,
          max = 10,
          value = 3,
          width = "150px",
          ticks = FALSE
        )
      } else {
        sliderInput(
          "nj_fruit_width_circ_3",
          label = "",
          min = 1,
          max = 10,
          value = 1,
          width = "150px",
          ticks = FALSE
        )
      }
    }
  })
  
  output$nj_fruit_width4 <- renderUI({
    if((!is.null(Vis$nj_min_x)) & (!is.null(Vis$nj_max_x))) {
      if(round(ceiling(Vis$nj_max_x) * 0.1, 0) < 1) {
        if(input$nj_layout == "circular" | input$nj_layout == "inward") {
          width <- 3
        } else {
          width <- 1
        }
      } else {
        if(input$nj_layout == "circular" | input$nj_layout == "inward") {
          width <- round(ceiling(Vis$nj_max_x) * 0.033, 0) * 3
        } else {
          width <- round(ceiling(Vis$nj_max_x) * 0.033, 0)
        }
      }
      sliderInput(
        "nj_fruit_width_circ_4",
        label = "",
        min = 1,
        max = round(ceiling(Vis$nj_max_x) * 0.5, 0),
        value = width,
        width = "150px",
        ticks = FALSE
      )
    } else {
      if(input$nj_layout == "circular" | input$nj_layout == "inward") {
        sliderInput(
          "nj_fruit_width_circ_4",
          label = "",
          min = 1,
          max = 10,
          value = 3,
          width = "150px",
          ticks = FALSE
        )
      } else {
        sliderInput(
          "nj_fruit_width_circ_4",
          label = "",
          min = 1,
          max = 10,
          value = 1,
          width = "150px",
          ticks = FALSE
        )
      }
    }
  })
  
  output$nj_fruit_width5 <- renderUI({
    if((!is.null(Vis$nj_min_x)) & (!is.null(Vis$nj_max_x))) {
      if(round(ceiling(Vis$nj_max_x) * 0.1, 0) < 1) {
        if(input$nj_layout == "circular" | input$nj_layout == "inward") {
          width <- 3
        } else {
          width <- 1
        }
      } else {
        if(input$nj_layout == "circular" | input$nj_layout == "inward") {
          width <- round(ceiling(Vis$nj_max_x) * 0.033, 0) * 3
        } else {
          width <- round(ceiling(Vis$nj_max_x) * 0.033, 0)
        }
      }
      sliderInput(
        "nj_fruit_width_circ_5",
        label = "",
        min = 1,
        max = round(ceiling(Vis$nj_max_x) * 0.5, 0),
        value = width,
        width = "150px",
        ticks = FALSE
      )
    } else {
      if(input$nj_layout == "circular" | input$nj_layout == "inward") {
        sliderInput(
          "nj_fruit_width_circ_5",
          label = "",
          min = 1,
          max = 10,
          value = 3,
          width = "150px",
          ticks = FALSE
        )
      } else {
        sliderInput(
          "nj_fruit_width_circ_5",
          label = "",
          min = 1,
          max = 10,
          value = 1,
          width = "150px",
          ticks = FALSE
        )
      }
    }
  })
  
  output$upgma_fruit_width <- renderUI({
    if((!is.null(Vis$upgma_min_x)) & (!is.null(Vis$upgma_max_x))) {
      if(round(ceiling(Vis$upgma_max_x) * 0.1, 0) < 1) {
        if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
          width <- 3
        } else {
          width <- 1
        }
      } else {
        if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
          width <- round(ceiling(Vis$upgma_max_x) * 0.033, 0) * 3
        } else {
          width <- round(ceiling(Vis$upgma_max_x) * 0.033, 0)
        }
      }
      sliderInput(
        "upgma_fruit_width_circ",
        label = "",
        min = 1,
        max = round(ceiling(Vis$upgma_max_x) * 0.5, 0),
        value = width,
        width = "150px",
        ticks = FALSE
      )
    } else {
      if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
        sliderInput(
          "upgma_fruit_width_circ",
          label = "",
          min = 1,
          max = 10,
          value = 3,
          width = "150px",
          ticks = FALSE
        )
      } else {
        sliderInput(
          "upgma_fruit_width_circ",
          label = "",
          min = 1,
          max = 10,
          value = 1,
          width = "150px",
          ticks = FALSE
        )
      }
    }
  })
  
  output$upgma_fruit_width2 <- renderUI({
    if((!is.null(Vis$upgma_min_x)) & (!is.null(Vis$upgma_max_x))) {
      if(round(ceiling(Vis$upgma_max_x) * 0.1, 0) < 1) {
        if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
          width <- 3
        } else {
          width <- 1
        }
      } else {
        if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
          width <- round(ceiling(Vis$upgma_max_x) * 0.033, 0) * 3
        } else {
          width <- round(ceiling(Vis$upgma_max_x) * 0.033, 0)
        }
      }
      sliderInput(
        "upgma_fruit_width_circ_2",
        label = "",
        min = 1,
        max = round(ceiling(Vis$upgma_max_x) * 0.5, 0),
        value = width,
        width = "150px",
        ticks = FALSE
      )
    } else {
      if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
        sliderInput(
          "upgma_fruit_width_circ_2",
          label = "",
          min = 1,
          max = 10,
          value = 3,
          width = "150px",
          ticks = FALSE
        )
      } else {
        sliderInput(
          "upgma_fruit_width_circ_2",
          label = "",
          min = 1,
          max = 10,
          value = 1,
          width = "150px",
          ticks = FALSE
        )
      }
    }
  })
  
  output$upgma_fruit_width3 <- renderUI({
    if((!is.null(Vis$upgma_min_x)) & (!is.null(Vis$upgma_max_x))) {
      if(round(ceiling(Vis$upgma_max_x) * 0.1, 0) < 1) {
        if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
          width <- 3
        } else {
          width <- 1
        }
      } else {
        if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
          width <- round(ceiling(Vis$upgma_max_x) * 0.033, 0) * 3
        } else {
          width <- round(ceiling(Vis$upgma_max_x) * 0.033, 0)
        }
      }
      sliderInput(
        "upgma_fruit_width_circ_3",
        label = "",
        min = 1,
        max = round(ceiling(Vis$upgma_max_x) * 0.5, 0),
        value = width,
        width = "150px",
        ticks = FALSE
      )
    } else {
      if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
        sliderInput(
          "upgma_fruit_width_circ_3",
          label = "",
          min = 1,
          max = 10,
          value = 3,
          width = "150px",
          ticks = FALSE
        )
      } else {
        sliderInput(
          "upgma_fruit_width_circ_3",
          label = "",
          min = 1,
          max = 10,
          value = 1,
          width = "150px",
          ticks = FALSE
        )
      }
    }
  })
  
  output$upgma_fruit_width4 <- renderUI({
    if((!is.null(Vis$upgma_min_x)) & (!is.null(Vis$upgma_max_x))) {
      if(round(ceiling(Vis$upgma_max_x) * 0.1, 0) < 1) {
        if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
          width <- 3
        } else {
          width <- 1
        }
      } else {
        if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
          width <- round(ceiling(Vis$upgma_max_x) * 0.033, 0) * 3
        } else {
          width <- round(ceiling(Vis$upgma_max_x) * 0.033, 0)
        }
      }
      sliderInput(
        "upgma_fruit_width_circ_4",
        label = "",
        min = 1,
        max = round(ceiling(Vis$upgma_max_x) * 0.5, 0),
        value = width,
        width = "150px",
        ticks = FALSE
      )
    } else {
      if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
        sliderInput(
          "upgma_fruit_width_circ_4",
          label = "",
          min = 1,
          max = 10,
          value = 3,
          width = "150px",
          ticks = FALSE
        )
      } else {
        sliderInput(
          "upgma_fruit_width_circ_4",
          label = "",
          min = 1,
          max = 10,
          value = 1,
          width = "150px",
          ticks = FALSE
        )
      }
    }
  })
  
  output$upgma_fruit_width5 <- renderUI({
    if((!is.null(Vis$upgma_min_x)) & (!is.null(Vis$upgma_max_x))) {
      if(round(ceiling(Vis$upgma_max_x) * 0.1, 0) < 1) {
        if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
          width <- 3
        } else {
          width <- 1
        }
      } else {
        if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
          width <- round(ceiling(Vis$upgma_max_x) * 0.033, 0) * 3
        } else {
          width <- round(ceiling(Vis$upgma_max_x) * 0.033, 0)
        }
      }
      sliderInput(
        "upgma_fruit_width_circ_5",
        label = "",
        min = 1,
        max = round(ceiling(Vis$upgma_max_x) * 0.5, 0),
        value = width,
        width = "150px",
        ticks = FALSE
      )
    } else {
      if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
        sliderInput(
          "upgma_fruit_width_circ_5",
          label = "",
          min = 1,
          max = 10,
          value = 3,
          width = "150px",
          ticks = FALSE
        )
      } else {
        sliderInput(
          "upgma_fruit_width_circ_5",
          label = "",
          min = 1,
          max = 10,
          value = 1,
          width = "150px",
          ticks = FALSE
        )
      }
    }
  })
  
  # For Layout change update tiles 
  observeEvent(input$nj_layout, {
    if((!is.null(Vis$nj_min_x)) & (!is.null(Vis$nj_max_x))) {
      if(round(ceiling(Vis$nj_max_x) * 0.1, 0) < 1) {
        if(input$nj_layout == "circular" | input$nj_layout == "inward") {
          width <- 3
        } else {
          width <- 1
        }
      } else {
        if(input$nj_layout == "circular" | input$nj_layout == "inward") {
          width <- round(ceiling(Vis$nj_max_x) * 0.033, 0) * 3
        } else {
          width <- round(ceiling(Vis$nj_max_x) * 0.033, 0)
        }
      }
      
      updateSliderInput(session, "nj_fruit_width_circ", value = width)
      updateSliderInput(session, "nj_fruit_width_circ_2", value = width)
      updateSliderInput(session, "nj_fruit_width_circ_3", value = width)
      updateSliderInput(session, "nj_fruit_width_circ_4", value = width)
      updateSliderInput(session, "nj_fruit_width_circ_5", value = width) 
    }
  })
  
  observeEvent(input$upgma_layout, {
    if((!is.null(Vis$upgma_min_x)) & (!is.null(Vis$upgma_max_x))) {
      if(round(ceiling(Vis$upgma_max_x) * 0.1, 0) < 1) {
        if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
          width <- 3
        } else {
          width <- 1
        }
      } else {
        if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
          width <- round(ceiling(Vis$upgma_max_x) * 0.033, 0) * 3
        } else {
          width <- round(ceiling(Vis$upgma_max_x) * 0.033, 0)
        }
      }
      
      updateSliderInput(session, "upgma_fruit_width_circ", value = width)
      updateSliderInput(session, "upgma_fruit_width_circ_2", value = width)
      updateSliderInput(session, "upgma_fruit_width_circ_3", value = width)
      updateSliderInput(session, "upgma_fruit_width_circ_4", value = width)
      updateSliderInput(session, "upgma_fruit_width_circ_5", value = width) 
    }
  })
  
  # Tip color mapping 
  output$nj_tipcolor_mapping <- renderUI({
    if(!is.null(Vis$meta_nj)) {
      selectInput(
        "nj_tipcolor_mapping",
        "",
        choices = if(ncol(Vis$meta_nj) == 11) {
          c(
            `Assembly Name` = "Assembly Name",
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          )
        } else {
          append(c(`Assembly Name` = "Assembly Name", `Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
                 names(Vis$meta_nj)[13:ncol(Vis$meta_nj)])
        },
        selected = c(City = "City"),
        width = "100%"
      )
    } else {
      selectInput(
        "nj_tipcolor_mapping",
        "",
        choices = c(
          `Assembly Name` = "Assembly Name",
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        ),
        selected = c(City = "City")
      )
    }
  })
  
  output$upgma_tipcolor_mapping <- renderUI({
    if(!is.null(Vis$meta_upgma)) {
      selectInput(
        "upgma_tipcolor_mapping",
        "",
        choices = if(ncol(Vis$meta_upgma) == 11) {
          c(
            `Assembly Name` = "Assembly Name",
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          )
        } else {
          append(c(`Assembly Name` = "Assembly Name", `Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
                 names(Vis$meta_upgma)[13:ncol(Vis$meta_upgma)])
        },
        selected = c(City = "City"),
        width = "100%"
      )
    } else {
      selectInput(
        "upgma_tipcolor_mapping",
        "",
        choices = c(
          `Assembly Name` = "Assembly Name",
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        ),
        selected = c(City = "City")
      )
    }
  })
  
  # Tip shape Mapping 
  output$nj_tipshape_mapping <- renderUI({
    if(!is.null(Vis$meta_nj)) {
      selectInput(
        "nj_tipshape_mapping",
        "",
        choices = if(ncol(Vis$meta_nj) == 11) {
          c(
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          )
        } else {
          append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
                 names(Vis$meta_nj)[13:ncol(Vis$meta_nj)])
        },
        selected = c("Host" = "Host"),
        width = "100%"
      )
    } else {
      selectInput(
        "nj_tipshape_mapping",
        "",
        choices = c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        ),
        selected = c("Host" = "Host"),
        width = "100%"
      )
    }
  })
  
  output$upgma_tipshape_mapping <- renderUI({
    if(!is.null(Vis$meta_upgma)) {
      selectInput(
        "upgma_tipshape_mapping",
        "",
        choices = if(ncol(Vis$meta_upgma) == 11) {
          c(
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          )
        } else {
          append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
                 names(Vis$meta_upgma)[13:ncol(Vis$meta_upgma)])
        },
        selected = c("Host" = "Host"),
        width = "100%"
      )
    } else {
      selectInput(
        "upgma_tipshape_mapping",
        "",
        choices = c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        ),
        selected = c("Host" = "Host"),
        width = "100%"
      )
    }
  })
  
  # Branch label 
  output$nj_branch_label <- renderUI({
    if(!is.null(Vis$meta_nj)) {
      selectInput(
        "nj_branch_label",
        "",
        choices = if(ncol(Vis$meta_nj) == 11) {
          c(
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          )
        } else {
          append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
                 names(Vis$meta_nj)[13:ncol(Vis$meta_nj)])
        },
        selected = c("Host" = "Host"),
        width = "100%"
      )
    } else {
      selectInput(
        "nj_branch_label",
        "",
        choices = c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        ),
        selected = c("Host" = "Host"),
        width = "100%"
      )
    }
  })
  
  output$upgma_branch_label <- renderUI({
    if(!is.null(Vis$meta_upgma)) {
      selectInput(
        "upgma_branch_label",
        "",
        choices = if(ncol(Vis$meta_upgma) == 11) {
          c(
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          )
        } else {
          append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
                 names(Vis$meta_upgma)[13:ncol(Vis$meta_upgma)])
        },
        selected = c("Host" = "Host"),
        width = "100%"
      )
    } else {
      selectInput(
        "upgma_branch_label",
        "",
        choices = c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        ),
        selected = c("Host" = "Host"),
        width = "100%"
      )
    }
  })
  
  # Color mapping 
  output$nj_color_mapping <- renderUI({
    if(!is.null(Vis$meta_nj)) {
      selectInput(
        "nj_color_mapping",
        "",
        choices = if(ncol(Vis$meta_nj) == 11) {
          c(
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          )
        } else {
          append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
                 names(Vis$meta_nj)[13:ncol(Vis$meta_nj)])
        },
        selected = c(Country = "Country"),
        width = "100%"
      )
    } else {
      selectInput(
        "nj_color_mapping",
        "",
        choices = c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        ),
        selected = c(Country = "Country"),
        width = "100%"
      )
    }
  })
  
  output$upgma_color_mapping <- renderUI({
    if(!is.null(Vis$meta_upgma)) {
      selectInput(
        "upgma_color_mapping",
        "",
        choices = if(ncol(Vis$meta_upgma) == 11) {
          c(
            `Isolation Date` = "Isolation Date",
            Host = "Host",
            Country = "Country",
            City = "City"
          )
        } else {
          append(c(`Isolation Date` = "Isolation Date", Host = "Host", Country = "Country", City = "City"),
                 names(Vis$meta_upgma)[13:ncol(Vis$meta_upgma)])
        },
        selected = c(Country = "Country"),
        width = "100%"
      )
    } else {
      selectInput(
        "upgma_color_mapping",
        "",
        choices = c(
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        ),
        selected = c(Country = "Country"),
        width = "100%"
      )
    }
  })
  
  # Tip labels 
  output$nj_tiplab <- renderUI({
    if(!is.null(Vis$meta_nj)) {
      selectInput(
        "nj_tiplab",
        label = "",
        choices = if(ncol(Vis$meta_nj) == 11) {
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
                 names(Vis$meta_nj)[13:ncol(Vis$meta_nj)])
        },
        selected = c(`Assembly Name` = "Assembly Name"),
        width = "100%"
      )
    } else {
      selectInput(
        "nj_tiplab",
        label = "",
        choices = c(
          Index = "Index",
          `Assembly ID` = "Assembly ID",
          `Assembly Name` = "Assembly Name",
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        ),
        selected = c(`Assembly Name` = "Assembly Name"),
        width = "100%"
      )
    }
  })
  
  output$upgma_tiplab <- renderUI({
    if(!is.null(Vis$meta_upgma)) {
      selectInput(
        "upgma_tiplab",
        label = "",
        choices = if(ncol(Vis$meta_upgma) == 11) {
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
                 names(Vis$meta_upgma)[13:ncol(Vis$meta_upgma)])
        },
        selected = c(`Assembly Name` = "Assembly Name"),
        width = "100%"
      )
    } else {
      selectInput(
        "upgma_tiplab",
        label = "",
        choices = c(
          Index = "Index",
          `Assembly ID` = "Assembly ID",
          `Assembly Name` = "Assembly Name",
          `Isolation Date` = "Isolation Date",
          Host = "Host",
          Country = "Country",
          City = "City"
        ),
        selected = c(`Assembly Name` = "Assembly Name"),
        width = "100%"
      )
    }
  })
  
  #### MST controls ----
  
  # MST node labels 
  output$mst_node_label <- renderUI({
    selectInput(
      "mst_node_label",
      label = "",
      choices = names(DB$meta)[-c(2, 5, 10, 11, 12)],
      selected = "Assembly Name",
      width = "100%"
    )
  })
  
  ### Plot Reactives ----
  
  #### MST ----
  
  mst_tree <- reactive({
    data <- toVisNetworkData(Vis$ggraph_1)
    data$nodes <- mutate(data$nodes, 
                         label = label_mst(),
                         value = mst_node_scaling(),
                         opacity = node_opacity())
    test <<- data$nodes
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
    Vis$unique_meta[, colnames(Vis$unique_meta) %in% input$mst_node_label]
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
      Vis$unique_meta$size
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
    if(!is.null(input$mst_title)) {
      if(nchar(input$mst_title) < 1) {
        list(text = "title",
             style = paste0(
               "font-family:Georgia, Times New Roman, Times, serif;",
               "text-align:center;",
               "font-size: ", as.character(input$mst_title_size), "px", 
               "; color: ", as.character(mst_background_color()))
        )
      } else {
        list(text = input$mst_title,
             style = paste0(
               "font-family:Georgia, Times New Roman, Times, serif;",
               "text-align:center;",
               "font-size: ", as.character(input$mst_title_size), "px", 
               "; color: ", as.character(input$mst_title_color))
        )
      }
    } else {
      list(text = "title",
           style = paste0(
             "font-family:Georgia, Times New Roman, Times, serif;",
             "text-align:center;",
             "font-size: ", as.character(input$mst_title_size), "px", 
             "; color: ", as.character(mst_background_color()))
      )
    }
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
    if(!is.null(input$mst_footer)) {
      if(nchar(input$mst_footer) < 1) {
        list(text = "footer",
             style = paste0(
               "font-family:Georgia, Times New Roman, Times, serif;",
               "text-align:center;",
               "font-size: ", as.character(input$mst_footer_size), "px", 
               "; color: ", as.character(mst_background_color()))
        )
      } else {
        list(text = input$mst_footer,
             style = paste0(
               "font-family:Georgia, Times New Roman, Times, serif;",
               "text-align:center;",
               "font-size: ", as.character(input$mst_footer_size), "px", 
               "; color: ", as.character(input$mst_footer_color))
        )
      }
    } else {
      list(text = "footer",
           style = paste0(
             "font-family:Georgia, Times New Roman, Times, serif;",
             "text-align:center;",
             "font-size: ", as.character(input$mst_footer_size), "px", 
             "; color: ", as.character(mst_background_color()))
      )
    }
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
    if(input$nj_nodelabel_show == TRUE) {
      ggtree(Vis$nj, alpha = 0.2) + 
        geom_nodelab(aes(label = node), color = "#29303A", size = nj_tiplab_size() + 1, hjust = 0.7) +
        nj_limit() +
        nj_inward() 
    } else {
      tree <-
        ggtree(Vis$nj, 
               color = input$nj_color,
               layout = layout_nj(),
               ladderize = input$nj_ladder) %<+% Vis$meta_nj +
        nj_clades() +
        nj_tiplab() +
        nj_tiplab_scale() + 
        new_scale_color() +
        nj_limit() +
        nj_inward() +
        nj_label_branch() +
        nj_treescale() +
        nj_nodepoint() +
        nj_tippoint() +
        nj_tippoint_scale() + 
        new_scale_color() +
        nj_clip_label() +
        nj_rootedge() +
        ggtitle(label = input$nj_title,
                subtitle = input$nj_subtitle) +
        theme_tree(bgcolor = input$nj_bg) +
        theme(plot.title = element_text(colour = input$nj_title_color,
                                        size = input$nj_title_size),
              plot.subtitle = element_text(colour = input$nj_title_color,
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
              plot.background = element_rect(fill = input$nj_bg, color = input$nj_bg)) +
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
        nj_gradient5() +
        new_scale_fill() 
      
      # Add custom labels
      if(length(Vis$custom_label_nj) > 0) {
        
        for(i in Vis$custom_label_nj[,1]) {
          
          if(!is.null(Vis$nj_label_pos_x[[i]])) {
            x_pos <- Vis$nj_label_pos_x[[i]]
          } else {
            x_pos <- round(Vis$nj_max_x / 2, 0)
          }
          
          if(!is.null(Vis$nj_label_pos_y[[i]])) {
            y_pos <- Vis$nj_label_pos_y[[i]]
          } else {
            y_pos <- sum(DB$data$Include) / 2
          }
          
          if(!is.null(Vis$nj_label_size[[i]])) {
            size <- Vis$nj_label_size[[i]]
          } else {
            size <- 5
          }
          
          tree <- tree + annotate("text",
                                  x = x_pos,
                                  y = y_pos, 
                                  label = i,
                                  size = size)
        }
      }
      
      # Add heatmap
      if(input$nj_heatmap_show == TRUE & length(input$nj_heatmap_select) > 0) {
        if (!(any(sapply(Vis$meta_nj[input$nj_heatmap_select], is.numeric)) & 
              any(!sapply(Vis$meta_nj[input$nj_heatmap_select], is.numeric)))) {
          tree <- gheatmap.mod(tree, 
                               data = select(Vis$meta_nj, input$nj_heatmap_select),
                               offset = nj_heatmap_offset(),
                               width = nj_heatmap_width(),
                               legend_title = input$nj_heatmap_title,
                               colnames_angle = -nj_colnames_angle(),
                               colnames_offset_y = nj_colnames_y(),
                               colnames_color = input$nj_color) +
            nj_heatmap_scale()
        }
      } 
      
      # Sizing control
      Vis$nj_plot <- ggplotify::as.ggplot(tree, 
                                          scale = input$nj_zoom,
                                          hjust = input$nj_h,
                                          vjust = input$nj_v)  
      
      # Correct background color if zoomed out
      cowplot::ggdraw(Vis$nj_plot) + 
        theme(plot.background = element_rect(fill = input$nj_bg, color = input$nj_bg))
    }
  })
  
  # Heatmap width
  nj_heatmap_width <- reactive({
    if(!is.null(input$nj_heatmap_width)) {
      input$nj_heatmap_width
    } else {
      length_input <- length(input$nj_heatmap_select)
      if((!(input$nj_layout == "circular")) & (!(input$nj_layout == "inward"))) {
        if(length_input < 3) {
          0.1
        } else {
          if (length_input >= 3 && length_input <= 50) {
            min(0.15 + 0.05 * floor((length_input - 3) / 2), 1.5)
          } else {
            1.5
          }   
        }
      } else {
        if(length_input < 3) {
          0.3
        } else if (length_input >= 3 && length_input <= 27) {
          min(0.6 + 0.2 * floor((length_input - 3) / 2), 1.5)
        } else {
          3
        }
      }
    }
  })
  
  # Heatmap column titles position
  nj_colnames_y <- reactive({
    if(!is.null(input$nj_colnames_y)) {
      input$nj_colnames_y
    } else {
      if(input$nj_layout == "inward" | input$nj_layout == "circular") {
        0
      } else {-1}
    }
  })
  
  # Heatmap column titles angle
  nj_colnames_angle <- reactive({
    if(!is.null(input$nj_colnames_angle)) {
      input$nj_colnames_angle
    } else {
      if(!is.null(input$nj_layout)) {
        if(input$nj_layout == "inward" | input$nj_layout == "circular") {
          90
        } else {-90}
      } else {-90}
    }
  })
  
  # Heatmap scale
  nj_heatmap_scale <- reactive({
    if(!is.null(input$nj_heatmap_scale) & !is.null(input$nj_heatmap_div_mid)) {
      if(input$nj_heatmap_scale %in% c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")) {
        if(input$nj_heatmap_div_mid == "Zero") {
          midpoint <- 0
        } else if(input$nj_heatmap_div_mid == "Mean") {
          midpoint <- mean(as.matrix(Vis$meta_nj[input$nj_heatmap_select]), na.rm = TRUE)
        } else {
          midpoint <- median(as.matrix(Vis$meta_nj[input$nj_heatmap_select]), na.rm = TRUE)
        }
        scale_fill_gradient2(low = brewer.pal(3, input$nj_heatmap_scale)[1],
                             mid = brewer.pal(3, input$nj_heatmap_scale)[2],
                             high = brewer.pal(3, input$nj_heatmap_scale)[3],
                             midpoint = midpoint,
                             name = input$nj_heatmap_title)
      } else {
        if(input$nj_heatmap_scale %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")) {
          if(class(unlist(Vis$meta_nj[input$nj_heatmap_select])) == "numeric") {
            if(input$nj_heatmap_scale == "magma") {
              scale_fill_viridis(option = "A",
                                 name = input$nj_heatmap_title)
            } else if(input$nj_heatmap_scale == "inferno") {
              scale_fill_viridis(option = "B",
                                 name = input$nj_heatmap_title)
            } else if(input$nj_heatmap_scale == "plasma") {
              scale_fill_viridis(option = "C",
                                 name = input$nj_heatmap_title)
            } else if(input$nj_heatmap_scale == "viridis") {
              scale_fill_viridis(option = "D",
                                 name = input$nj_heatmap_title)
            } else if(input$nj_heatmap_scale == "cividis") {
              scale_fill_viridis(option = "E",
                                 name = input$nj_heatmap_title)
            } else if(input$nj_heatmap_scale == "rocket") {
              scale_fill_viridis(option = "F",
                                 name = input$nj_heatmap_title)
            } else if(input$nj_heatmap_scale == "mako") {
              scale_fill_viridis(option = "G",
                                 name = input$nj_heatmap_title)
            } else if(input$nj_heatmap_scale == "turbo") {
              scale_fill_viridis(option = "H",
                                 name = input$nj_heatmap_title)
            } 
          } else {
            if(input$nj_heatmap_scale == "magma") {
              scale_fill_viridis(discrete = TRUE, option = "A",
                                 name = input$nj_heatmap_title)
            } else if(input$nj_heatmap_scale == "inferno") {
              scale_fill_viridis(discrete = TRUE, option = "B",
                                 name = input$nj_heatmap_title)
            } else if(input$nj_heatmap_scale == "plasma") {
              scale_fill_viridis(discrete = TRUE, option = "C",
                                 name = input$nj_heatmap_title)
            } else if(input$nj_heatmap_scale == "viridis") {
              scale_fill_viridis(discrete = TRUE, option = "D",
                                 name = input$nj_heatmap_title)
            } else if(input$nj_heatmap_scale == "cividis") {
              scale_fill_viridis(discrete = TRUE, option = "E",
                                 name = input$nj_heatmap_title)
            } else if(input$nj_heatmap_scale == "rocket") {
              scale_fill_viridis(discrete = TRUE, option = "F",
                                 name = input$nj_heatmap_title)
            } else if(input$nj_heatmap_scale == "mako") {
              scale_fill_viridis(discrete = TRUE, option = "G",
                                 name = input$nj_heatmap_title)
            } else if(input$nj_heatmap_scale == "turbo") {
              scale_fill_viridis(discrete = TRUE, option = "H",
                                 name = input$nj_heatmap_title)
            } 
          }
        } else {
          scale_fill_brewer(palette = input$nj_heatmap_scale,
                            name = input$nj_heatmap_title)
        }
      }
    }
  })
  
  # Tippoint Scale
  nj_tippoint_scale <- reactive({
    if(!is.null(input$nj_tippoint_scale) & !is.null(input$nj_tipcolor_mapping_div_mid)) {
      if(input$nj_tippoint_scale %in% c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")) {
        if(input$nj_tipcolor_mapping_div_mid == "Zero") {
          midpoint <- 0
        } else if(input$nj_tipcolor_mapping_div_mid == "Mean") {
          midpoint <- mean(as.matrix(Vis$meta_nj[input$nj_tipcolor_mapping]), na.rm = TRUE)
        } else {
          midpoint <- median(as.matrix(Vis$meta_nj[input$nj_tipcolor_mapping]), na.rm = TRUE)
        }
        scale_color_gradient2(low = brewer.pal(3, input$nj_tippoint_scale)[1],
                              mid = brewer.pal(3, input$nj_tippoint_scale)[2],
                              high = brewer.pal(3, input$nj_tippoint_scale)[3],
                              midpoint = midpoint)
      } else {
        if(input$nj_tippoint_scale %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")) {
          if(class(unlist(Vis$meta_nj[input$nj_tipcolor_mapping])) == "numeric") {
            if(input$nj_tippoint_scale == "magma") {
              scale_color_viridis(option = "A")
            } else if(input$nj_tippoint_scale == "inferno") {
              scale_color_viridis(option = "B")
            } else if(input$nj_tippoint_scale == "plasma") {
              scale_color_viridis(option = "C")
            } else if(input$nj_tippoint_scale == "viridis") {
              scale_color_viridis(option = "D")
            } else if(input$nj_tippoint_scale == "cividis") {
              scale_color_viridis(option = "E")
            } else if(input$nj_tippoint_scale == "rocket") {
              scale_color_viridis(option = "F")
            } else if(input$nj_tippoint_scale == "mako") {
              scale_color_viridis(option = "G")
            } else if(input$nj_tippoint_scale == "turbo") {
              scale_color_viridis(option = "H")
            } 
          } else {
            if(input$nj_tippoint_scale == "magma") {
              scale_color_viridis(discrete = TRUE, option = "A")
            } else if(input$nj_tippoint_scale == "inferno") {
              scale_color_viridis(discrete = TRUE, option = "B")
            } else if(input$nj_tippoint_scale == "plasma") {
              scale_color_viridis(discrete = TRUE, option = "C")
            } else if(input$nj_tippoint_scale == "viridis") {
              scale_color_viridis(discrete = TRUE, option = "D")
            } else if(input$nj_tippoint_scale == "cividis") {
              scale_color_viridis(discrete = TRUE, option = "E")
            } else if(input$nj_tippoint_scale == "rocket") {
              scale_color_viridis(discrete = TRUE, option = "F")
            } else if(input$nj_tippoint_scale == "mako") {
              scale_color_viridis(discrete = TRUE, option = "G")
            } else if(input$nj_tippoint_scale == "turbo") {
              scale_color_viridis(discrete = TRUE, option = "H")
            } 
          }
        } else {
          scale_color_brewer(palette = input$nj_tippoint_scale)
        }
      }
    }
  })
  
  # Tiplab Scale
  nj_tiplab_scale <- reactive({
    if(!is.null(input$nj_tiplab_scale) & !is.null(input$nj_color_mapping_div_mid)) {
      if(input$nj_tiplab_scale %in% c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")) {
        if(input$nj_color_mapping_div_mid == "Zero") {
          midpoint <- 0
        } else if(input$nj_color_mapping_div_mid == "Mean") {
          midpoint <- mean(as.matrix(Vis$meta_nj[input$nj_color_mapping]), na.rm = TRUE)
        } else {
          midpoint <- median(as.matrix(Vis$meta_nj[input$nj_color_mapping]), na.rm = TRUE)
        }
        scale_color_gradient2(low = brewer.pal(3, input$nj_tiplab_scale)[1],
                              mid = brewer.pal(3, input$nj_tiplab_scale)[2],
                              high = brewer.pal(3, input$nj_tiplab_scale)[3],
                              midpoint = midpoint)
      } else {
        if(input$nj_tiplab_scale %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")) {
          if(class(unlist(Vis$meta_nj[input$nj_color_mapping])) == "numeric") {
            if(input$nj_tiplab_scale == "magma") {
              scale_color_viridis(option = "A")
            } else if(input$nj_tiplab_scale == "inferno") {
              scale_color_viridis(option = "B")
            } else if(input$nj_tiplab_scale == "plasma") {
              scale_color_viridis(option = "C")
            } else if(input$nj_tiplab_scale == "viridis") {
              scale_color_viridis(option = "D")
            } else if(input$nj_tiplab_scale == "cividis") {
              scale_color_viridis(option = "E")
            } else if(input$nj_tiplab_scale == "rocket") {
              scale_color_viridis(option = "F")
            } else if(input$nj_tiplab_scale == "mako") {
              scale_color_viridis(option = "G")
            } else if(input$nj_tiplab_scale == "turbo") {
              scale_color_viridis(option = "H")
            } 
          } else {
            if(input$nj_tiplab_scale == "magma") {
              scale_color_viridis(discrete = TRUE, option = "A")
            } else if(input$nj_tiplab_scale == "inferno") {
              scale_color_viridis(discrete = TRUE, option = "B")
            } else if(input$nj_tiplab_scale == "plasma") {
              scale_color_viridis(discrete = TRUE, option = "C")
            } else if(input$nj_tiplab_scale == "viridis") {
              scale_color_viridis(discrete = TRUE, option = "D")
            } else if(input$nj_tiplab_scale == "cividis") {
              scale_color_viridis(discrete = TRUE, option = "E")
            } else if(input$nj_tiplab_scale == "rocket") {
              scale_color_viridis(discrete = TRUE, option = "F")
            } else if(input$nj_tiplab_scale == "mako") {
              scale_color_viridis(discrete = TRUE, option = "G")
            } else if(input$nj_tiplab_scale == "turbo") {
              scale_color_viridis(discrete = TRUE, option = "H")
            } 
          }
        } else {
          scale_color_brewer(palette = input$nj_tiplab_scale)
        }
      }
    }
  })
  
  # Clade Highlight
  nj_clades <- reactive({
    if(!is.null(input$nj_parentnode)) {
      if(!length(input$nj_parentnode) == 0) {
        if(length(input$nj_parentnode) == 1) {
          fill <- input$nj_clade_scale
        } else if (length(input$nj_parentnode) == 2) {
          fill <- brewer.pal(3, input$nj_clade_scale)[1:2]
        } else {
          fill <- brewer.pal(length(input$nj_parentnode), input$nj_clade_scale)
        }
        geom_hilight(node = as.numeric(input$nj_parentnode),
                     fill = fill,
                     align = nj_align_clade(),
                     type = input$nj_clade_type,
                     gradient.direction = nj_clade_grad_dir(),
                     gradient.length.out = nj_clade_grad_len())
      } else {NULL}
    }
  })
  
  # Clade highlight align 
  
  nj_align_clade <- reactive({
    if(is.null(input$nj_clade_align)) {
      input$nj_clade_align
    } else {input$nj_clade_align}
  })
  
  # Clade highlight gradient direction
  nj_clade_grad_dir <- reactive({
    if(input$nj_clade_type == "gradient") {
      input$nj_clade_grad_dir
    } else {"rt"}
  })
  
  # Clade hightlight gradient length
  nj_clade_grad_len <- reactive({
    if(input$nj_clade_type == "gradient") {
      nj_clade_grad_len
    } else {2}
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
    if(!is.null(input$nj_tiles_show_1) & 
       !is.null(input$nj_fruit_variable) & 
       !is.null(input$nj_tiles_scale_1) & 
       !is.null(input$nj_tiles_mapping_div_mid_1)) {
      if(input$nj_tiles_show_1 == TRUE) {
        if(input$nj_tiles_scale_1 %in% c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")) {
          if(input$nj_tiles_mapping_div_mid_1 == "Zero") {
            midpoint <- 0
          } else if(input$nj_tiles_mapping_div_mid_1 == "Mean") {
            midpoint <- mean(as.matrix(Vis$meta_nj[input$nj_fruit_variable]), na.rm = TRUE)
          } else {
            midpoint <- median(as.matrix(Vis$meta_nj[input$nj_fruit_variable]), na.rm = TRUE)
          }
          scale_fill_gradient2(low = brewer.pal(3, input$nj_tiles_scale_1)[1],
                               mid = brewer.pal(3, input$nj_tiles_scale_1)[2],
                               high = brewer.pal(3, input$nj_tiles_scale_1)[3],
                               midpoint = midpoint)
        } else {
          if(input$nj_tiles_scale_1 %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")) {
            if(class(unlist(DB$meta[input$nj_fruit_variable])) == "numeric") {
              if(input$nj_tiles_scale_1 == "magma") {
                scale_fill_viridis(option = "A")
              } else if(input$nj_tiles_scale_1 == "inferno") {
                scale_fill_viridis(option = "B")
              } else if(input$nj_tiles_scale_1 == "plasma") {
                scale_fill_viridis(option = "C")
              } else if(input$nj_tiles_scale_1 == "viridis") {
                scale_fill_viridis(option = "D")
              } else if(input$nj_tiles_scale_1 == "cividis") {
                scale_fill_viridis(option = "E")
              } else if(input$nj_tiles_scale_1 == "rocket") {
                scale_fill_viridis(option = "F")
              } else if(input$nj_tiles_scale_1 == "mako") {
                scale_fill_viridis(option = "G")
              } else if(input$nj_tiles_scale_1 == "turbo") {
                scale_fill_viridis(option = "H")
              } 
            } else {
              if(input$nj_tiles_scale_1 == "magma") {
                scale_fill_viridis(discrete = TRUE, option = "A")
              } else if(input$nj_tiles_scale_1 == "inferno") {
                scale_fill_viridis(discrete = TRUE, option = "B")
              } else if(input$nj_tiles_scale_1 == "plasma") {
                scale_fill_viridis(discrete = TRUE, option = "C")
              } else if(input$nj_tiles_scale_1 == "viridis") {
                scale_fill_viridis(discrete = TRUE, option = "D")
              } else if(input$nj_tiles_scale_1 == "cividis") {
                scale_fill_viridis(discrete = TRUE, option = "E")
              } else if(input$nj_tiles_scale_1 == "rocket") {
                scale_fill_viridis(discrete = TRUE, option = "F")
              } else if(input$nj_tiles_scale_1 == "mako") {
                scale_fill_viridis(discrete = TRUE, option = "G")
              } else if(input$nj_tiles_scale_1 == "turbo") {
                scale_fill_viridis(discrete = TRUE, option = "H")
              } 
            }
          } else {
            scale_fill_brewer(palette = input$nj_tiles_scale_1)
          }
        }
      } else {NULL}
    }
  })
  
  nj_gradient2 <- reactive({
    if(!is.null(input$nj_tiles_show_2) & 
       !is.null(input$nj_fruit_variable_2) & 
       !is.null(input$nj_tiles_scale_2) & 
       !is.null(input$nj_tiles_mapping_div_mid_2)) {
      if(input$nj_tiles_show_2 == TRUE) {
        if(input$nj_tiles_scale_2 %in% c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")) {
          if(input$nj_tiles_mapping_div_mid_2 == "Zero") {
            midpoint <- 0
          } else if(input$nj_tiles_mapping_div_mid_2 == "Mean") {
            midpoint <- mean(as.matrix(Vis$meta_nj[input$nj_fruit_variable_2]), na.rm = TRUE)
          } else {
            midpoint <- median(as.matrix(Vis$meta_nj[input$nj_fruit_variable_2]), na.rm = TRUE)
          }
          scale_fill_gradient2(low = brewer.pal(3, input$nj_tiles_scale_2)[1],
                               mid = brewer.pal(3, input$nj_tiles_scale_2)[2],
                               high = brewer.pal(3, input$nj_tiles_scale_2)[3],
                               midpoint = midpoint)
        } else {
          if(input$nj_tiles_scale_2 %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")) {
            if(class(unlist(DB$meta[input$nj_fruit_variable_2])) == "numeric") {
              if(input$nj_tiles_scale_2 == "magma") {
                scale_fill_viridis(option = "A")
              } else if(input$nj_tiles_scale_2 == "inferno") {
                scale_fill_viridis(option = "B")
              } else if(input$nj_tiles_scale_2 == "plasma") {
                scale_fill_viridis(option = "C")
              } else if(input$nj_tiles_scale_2 == "viridis") {
                scale_fill_viridis(option = "D")
              } else if(input$nj_tiles_scale_2 == "cividis") {
                scale_fill_viridis(option = "E")
              } else if(input$nj_tiles_scale_2 == "rocket") {
                scale_fill_viridis(option = "F")
              } else if(input$nj_tiles_scale_2 == "mako") {
                scale_fill_viridis(option = "G")
              } else if(input$nj_tiles_scale_2 == "turbo") {
                scale_fill_viridis(option = "H")
              } 
            } else {
              if(input$nj_tiles_scale_2 == "magma") {
                scale_fill_viridis(discrete = TRUE, option = "A")
              } else if(input$nj_tiles_scale_2 == "inferno") {
                scale_fill_viridis(discrete = TRUE, option = "B")
              } else if(input$nj_tiles_scale_2 == "plasma") {
                scale_fill_viridis(discrete = TRUE, option = "C")
              } else if(input$nj_tiles_scale_2 == "viridis") {
                scale_fill_viridis(discrete = TRUE, option = "D")
              } else if(input$nj_tiles_scale_2 == "cividis") {
                scale_fill_viridis(discrete = TRUE, option = "E")
              } else if(input$nj_tiles_scale_2 == "rocket") {
                scale_fill_viridis(discrete = TRUE, option = "F")
              } else if(input$nj_tiles_scale_2 == "mako") {
                scale_fill_viridis(discrete = TRUE, option = "G")
              } else if(input$nj_tiles_scale_2 == "turbo") {
                scale_fill_viridis(discrete = TRUE, option = "H")
              } 
            }
          } else {
            scale_fill_brewer(palette = input$nj_tiles_scale_2)
          }
        }
      } else {NULL}
    }
  })
  
  nj_gradient3 <- reactive({
    if(!is.null(input$nj_tiles_show_3) & 
       !is.null(input$nj_fruit_variable_3) & 
       !is.null(input$nj_tiles_scale_3 & 
       !is.null(input$nj_tiles_mapping_div_mid_3))) {
      if(input$nj_tiles_show_3 == TRUE) {
        if(input$nj_tiles_scale_3 %in% c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")) {
          if(input$nj_tiles_mapping_div_mid_3 == "Zero") {
            midpoint <- 0
          } else if(input$nj_tiles_mapping_div_mid_3 == "Mean") {
            midpoint <- mean(as.matrix(Vis$meta_nj[input$nj_fruit_variable_3]), na.rm = TRUE)
          } else {
            midpoint <- median(as.matrix(Vis$meta_nj[input$nj_fruit_variable_3]), na.rm = TRUE)
          }
          scale_fill_gradient3(low = brewer.pal(3, input$nj_tiles_scale_3)[1],
                               mid = brewer.pal(3, input$nj_tiles_scale_3)[2],
                               high = brewer.pal(3, input$nj_tiles_scale_3)[3],
                               midpoint = midpoint)
        } else {
          if(input$nj_tiles_scale_3 %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")) {
            if(class(unlist(DB$meta[input$nj_fruit_variable_3])) == "numeric") {
              if(input$nj_tiles_scale_3 == "magma") {
                scale_fill_viridis(option = "A")
              } else if(input$nj_tiles_scale_3 == "inferno") {
                scale_fill_viridis(option = "B")
              } else if(input$nj_tiles_scale_3 == "plasma") {
                scale_fill_viridis(option = "C")
              } else if(input$nj_tiles_scale_3 == "viridis") {
                scale_fill_viridis(option = "D")
              } else if(input$nj_tiles_scale_3 == "cividis") {
                scale_fill_viridis(option = "E")
              } else if(input$nj_tiles_scale_3 == "rocket") {
                scale_fill_viridis(option = "F")
              } else if(input$nj_tiles_scale_3 == "mako") {
                scale_fill_viridis(option = "G")
              } else if(input$nj_tiles_scale_3 == "turbo") {
                scale_fill_viridis(option = "H")
              } 
            } else {
              if(input$nj_tiles_scale_3 == "magma") {
                scale_fill_viridis(discrete = TRUE, option = "A")
              } else if(input$nj_tiles_scale_3 == "inferno") {
                scale_fill_viridis(discrete = TRUE, option = "B")
              } else if(input$nj_tiles_scale_3 == "plasma") {
                scale_fill_viridis(discrete = TRUE, option = "C")
              } else if(input$nj_tiles_scale_3 == "viridis") {
                scale_fill_viridis(discrete = TRUE, option = "D")
              } else if(input$nj_tiles_scale_3 == "cividis") {
                scale_fill_viridis(discrete = TRUE, option = "E")
              } else if(input$nj_tiles_scale_3 == "rocket") {
                scale_fill_viridis(discrete = TRUE, option = "F")
              } else if(input$nj_tiles_scale_3 == "mako") {
                scale_fill_viridis(discrete = TRUE, option = "G")
              } else if(input$nj_tiles_scale_3 == "turbo") {
                scale_fill_viridis(discrete = TRUE, option = "H")
              } 
            }
          } else {
            scale_fill_brewer(palette = input$nj_tiles_scale_3)
          }
        }
      } else {NULL}
    }
  })
  
  nj_gradient4 <- reactive({
    if(!is.null(input$nj_tiles_show_4) & 
       !is.null(input$nj_fruit_variable_4) & 
       !is.null(input$nj_tiles_scale_4) & 
       !is.null(input$nj_tiles_mapping_div_mid_4)) {
      if(input$nj_tiles_show_4 == TRUE) {
        if(input$nj_tiles_scale_4 %in% c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")) {
          if(input$nj_tiles_mapping_div_mid_4 == "Zero") {
            midpoint <- 0
          } else if(input$nj_tiles_mapping_div_mid_4 == "Mean") {
            midpoint <- mean(as.matrix(Vis$meta_nj[input$nj_fruit_variable_4]), na.rm = TRUE)
          } else {
            midpoint <- median(as.matrix(Vis$meta_nj[input$nj_fruit_variable_4]), na.rm = TRUE)
          }
          scale_fill_gradient4(low = brewer.pal(3, input$nj_tiles_scale_4)[1],
                               mid = brewer.pal(3, input$nj_tiles_scale_4)[2],
                               high = brewer.pal(3, input$nj_tiles_scale_4)[3],
                               midpoint = midpoint)
        } else {
          if(input$nj_tiles_scale_4 %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")) {
            if(class(unlist(DB$meta[input$nj_fruit_variable])) == "numeric") {
              if(input$nj_tiles_scale_4 == "magma") {
                scale_fill_viridis(option = "A")
              } else if(input$nj_tiles_scale_4 == "inferno") {
                scale_fill_viridis(option = "B")
              } else if(input$nj_tiles_scale_4 == "plasma") {
                scale_fill_viridis(option = "C")
              } else if(input$nj_tiles_scale_4 == "viridis") {
                scale_fill_viridis(option = "D")
              } else if(input$nj_tiles_scale_4 == "cividis") {
                scale_fill_viridis(option = "E")
              } else if(input$nj_tiles_scale_4 == "rocket") {
                scale_fill_viridis(option = "F")
              } else if(input$nj_tiles_scale_4 == "mako") {
                scale_fill_viridis(option = "G")
              } else if(input$nj_tiles_scale_4 == "turbo") {
                scale_fill_viridis(option = "H")
              } 
            } else {
              if(input$nj_tiles_scale_4 == "magma") {
                scale_fill_viridis(discrete = TRUE, option = "A")
              } else if(input$nj_tiles_scale_4 == "inferno") {
                scale_fill_viridis(discrete = TRUE, option = "B")
              } else if(input$nj_tiles_scale_4 == "plasma") {
                scale_fill_viridis(discrete = TRUE, option = "C")
              } else if(input$nj_tiles_scale_4 == "viridis") {
                scale_fill_viridis(discrete = TRUE, option = "D")
              } else if(input$nj_tiles_scale_4 == "cividis") {
                scale_fill_viridis(discrete = TRUE, option = "E")
              } else if(input$nj_tiles_scale_4 == "rocket") {
                scale_fill_viridis(discrete = TRUE, option = "F")
              } else if(input$nj_tiles_scale_4 == "mako") {
                scale_fill_viridis(discrete = TRUE, option = "G")
              } else if(input$nj_tiles_scale_4 == "turbo") {
                scale_fill_viridis(discrete = TRUE, option = "H")
              } 
            }
          } else {
            scale_fill_brewer(palette = input$nj_tiles_scale_4)
          }
        }
      } else {NULL}
    }
  })
  
  nj_gradient5 <- reactive({
    if(!is.null(input$nj_tiles_show_5) & 
       !is.null(input$nj_fruit_variable_5) & 
       !is.null(input$nj_tiles_scale_5) & 
       !is.null(input$nj_tiles_mapping_div_mid_5)) {
      if(input$nj_tiles_show_5 == TRUE) {
        if(input$nj_tiles_scale_5 %in% c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")) {
          if(input$nj_tiles_mapping_div_mid_5 == "Zero") {
            midpoint <- 0
          } else if(input$nj_tiles_mapping_div_mid_5 == "Mean") {
            midpoint <- mean(as.matrix(Vis$meta_nj[input$nj_fruit_variable_5]), na.rm = TRUE)
          } else {
            midpoint <- median(as.matrix(Vis$meta_nj[input$nj_fruit_variable_5]), na.rm = TRUE)
          }
          scale_fill_gradient5(low = brewer.pal(3, input$nj_tiles_scale_5)[1],
                               mid = brewer.pal(3, input$nj_tiles_scale_5)[2],
                               high = brewer.pal(3, input$nj_tiles_scale_5)[3],
                               midpoint = midpoint)
        } else {
          if(input$nj_tiles_scale_5 %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")) {
            if(class(unlist(DB$meta[input$nj_fruit_variable_5])) == "numeric") {
              if(input$nj_tiles_scale_5 == "magma") {
                scale_fill_viridis(option = "A")
              } else if(input$nj_tiles_scale_5 == "inferno") {
                scale_fill_viridis(option = "B")
              } else if(input$nj_tiles_scale_5 == "plasma") {
                scale_fill_viridis(option = "C")
              } else if(input$nj_tiles_scale_5 == "viridis") {
                scale_fill_viridis(option = "D")
              } else if(input$nj_tiles_scale_5 == "cividis") {
                scale_fill_viridis(option = "E")
              } else if(input$nj_tiles_scale_5 == "rocket") {
                scale_fill_viridis(option = "F")
              } else if(input$nj_tiles_scale_5 == "mako") {
                scale_fill_viridis(option = "G")
              } else if(input$nj_tiles_scale_5 == "turbo") {
                scale_fill_viridis(option = "H")
              } 
            } else {
              if(input$nj_tiles_scale_5 == "magma") {
                scale_fill_viridis(discrete = TRUE, option = "A")
              } else if(input$nj_tiles_scale_5 == "inferno") {
                scale_fill_viridis(discrete = TRUE, option = "B")
              } else if(input$nj_tiles_scale_5 == "plasma") {
                scale_fill_viridis(discrete = TRUE, option = "C")
              } else if(input$nj_tiles_scale_5 == "viridis") {
                scale_fill_viridis(discrete = TRUE, option = "D")
              } else if(input$nj_tiles_scale_5 == "cividis") {
                scale_fill_viridis(discrete = TRUE, option = "E")
              } else if(input$nj_tiles_scale_5 == "rocket") {
                scale_fill_viridis(discrete = TRUE, option = "F")
              } else if(input$nj_tiles_scale_5 == "mako") {
                scale_fill_viridis(discrete = TRUE, option = "G")
              } else if(input$nj_tiles_scale_5 == "turbo") {
                scale_fill_viridis(discrete = TRUE, option = "H")
              } 
            }
          } else {
            scale_fill_brewer(palette = input$nj_tiles_scale_5)
          }
        }
      } else {NULL}
    }
  })
  
  # No label clip off for linear NJ tree
  nj_clip_label <- reactive({
    if(!(input$nj_layout == "circular" | input$nj_layout == "inward")) {
      coord_cartesian(clip = "off")
    } else {NULL}
  })
  
  # Geom Fruit
  nj_fruit <- reactive({
    if((!is.null(input$nj_tiles_show_1)) & 
       (!is.null(input$nj_fruit_variable)) & 
       (!is.null(input$nj_layout)) & 
       (!is.null(input$nj_fruit_offset_circ)) & 
       (!is.null(input$nj_fruit_width_circ)) & 
       (!is.null(input$nj_fruit_alpha))) {
      if(input$nj_tiles_show_1 == TRUE) {
        if(input$nj_layout == "circular" | input$nj_layout == "inward") {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill= !!sym(input$nj_fruit_variable)),
            offset = input$nj_fruit_offset_circ,
            width = input$nj_fruit_width_circ,
            alpha = input$nj_fruit_alpha
          )
        } else {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill= !!sym(input$nj_fruit_variable)),
            offset = input$nj_fruit_offset_circ,
            width = input$nj_fruit_width_circ,
            alpha = input$nj_fruit_alpha
          )
        }
      } else {NULL}
    } else {
      if(input$nj_tiles_show_1 == TRUE) {
        if(!is.null(Vis$nj_max_x)) {
          if(round(ceiling(Vis$nj_max_x) * 0.1, 0) < 1) {
            width <- 1
          } else {
            width <- round(ceiling(Vis$nj_max_x) * 0.033, 0)
          }
        } else {
          width <- 2
        }
        if(input$nj_layout == "circular" | input$nj_layout == "inward") {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill= !!sym(input$nj_fruit_variable)),
            offset = 0,
            width = width * 3,
            alpha = 1
          )
        } else {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill= !!sym(input$nj_fruit_variable)),
            offset = 0,
            width = width,
            alpha = 1
          )
        }
      } else {NULL}
    }
  })
  
  # Geom Fruit
  nj_fruit2 <- reactive({
    if((!is.null(input$nj_tiles_show_2)) & 
       (!is.null(input$nj_fruit_variable_2)) & 
       (!is.null(input$nj_layout)) & 
       (!is.null(input$nj_fruit_offset_circ_2)) & 
       (!is.null(input$nj_fruit_width_circ_2)) & 
       (!is.null(input$nj_fruit_alpha_2))) {
      if(input$nj_tiles_show_2 == TRUE) {
        if(input$nj_layout == "circular" | input$nj_layout == "inward") {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill = !!sym(input$nj_fruit_variable_2)),
            offset = input$nj_fruit_offset_circ_2,
            width = input$nj_fruit_width_circ_2,
            alpha = input$nj_fruit_alpha_2
          )
        } else {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill = !!sym(input$nj_fruit_variable_2)),
            offset = input$nj_fruit_offset_circ_2,
            width = input$nj_fruit_width_circ_2,
            alpha = input$nj_fruit_alpha_2
          )
        }
      } else {NULL}
    } else {
      if(input$nj_tiles_show_2 == TRUE) {
        if(!is.null(Vis$nj_max_x)) {
          if(round(ceiling(Vis$nj_max_x) * 0.1, 0) < 1) {
            width <- 1
          } else {
            width <- round(ceiling(Vis$nj_max_x) * 0.033, 0)
          }
        } else {
          width <- 2
        }
        if(input$nj_layout == "circular" | input$nj_layout == "inward") {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill= !!sym(input$nj_fruit_variable_2)),
            offset = 0.15,
            width = width * 3,
            alpha = 1
          )
        } else {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill= !!sym(input$nj_fruit_variable_2)),
            offset = 0.05,
            width = width,
            alpha = 1
          )
        }
      } else {NULL}
    }
  })
  
  nj_fruit3 <- reactive({
    if((!is.null(input$nj_tiles_show_3)) & 
       (!is.null(input$nj_fruit_variable_3)) & 
       (!is.null(input$nj_layout)) & 
       (!is.null(input$nj_fruit_offset_circ_3)) & 
       (!is.null(input$nj_fruit_width_circ_3)) & 
       (!is.null(input$nj_fruit_alpha_3))) {
      if(input$nj_tiles_show_3 == TRUE) {
        if(input$nj_layout == "circular" | input$nj_layout == "inward") {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill = !!sym(input$nj_fruit_variable_3)),
            offset = input$nj_fruit_offset_circ_3,
            width = input$nj_fruit_width_circ_3,
            alpha = input$nj_fruit_alpha_3
          )
        } else {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill = !!sym(input$nj_fruit_variable_3)),
            offset = input$nj_fruit_offset_circ_3,
            width = input$nj_fruit_width_circ_3,
            alpha = input$nj_fruit_alpha_3
          )
        }
      } else {NULL}
    } else {
      if(input$nj_tiles_show_3 == TRUE) {
        if(!is.null(Vis$nj_max_x)) {
          if(round(ceiling(Vis$nj_max_x) * 0.1, 0) < 1) {
            width <- 1
          } else {
            width <- round(ceiling(Vis$nj_max_x) * 0.033, 0)
          }
        } else {
          width <- 2
        }
        if(input$nj_layout == "circular" | input$nj_layout == "inward") {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill= !!sym(input$nj_fruit_variable_3)),
            offset = 0.15,
            width = width * 3,
            alpha = 1
          )
        } else {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill= !!sym(input$nj_fruit_variable_3)),
            offset = 0.05,
            width = width,
            alpha = 1
          )
        }
      } else {NULL}
    }
  })
  
  nj_fruit4 <- reactive({
    if((!is.null(input$nj_tiles_show_4)) & 
       (!is.null(input$nj_fruit_variable_4)) & 
       (!is.null(input$nj_layout)) & 
       (!is.null(input$nj_fruit_offset_circ_4)) & 
       (!is.null(input$nj_fruit_width_circ_4)) & 
       (!is.null(input$nj_fruit_alpha_4))) {
      if(input$nj_tiles_show_4 == TRUE) {
        if(input$nj_layout == "circular" | input$nj_layout == "inward") {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill = !!sym(input$nj_fruit_variable_4)),
            offset = input$nj_fruit_offset_circ_4,
            width = input$nj_fruit_width_circ_4,
            alpha = input$nj_fruit_alpha_4
          )
        } else {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill = !!sym(input$nj_fruit_variable_4)),
            offset = input$nj_fruit_offset_circ_4,
            width = input$nj_fruit_width_circ_4,
            alpha = input$nj_fruit_alpha_4
          )
        }
      } else {
        if(input$nj_tiles_show_4 == TRUE) {
          if(!is.null(Vis$nj_max_x)) {
            if(round(ceiling(Vis$nj_max_x) * 0.1, 0) < 1) {
              width <- 1
            } else {
              width <- round(ceiling(Vis$nj_max_x) * 0.033, 0)
            }
          } else {
            width <- 2
          }
          if(input$nj_layout == "circular" | input$nj_layout == "inward") {
            geom_fruit(
              geom = geom_tile,
              mapping = aes(fill= !!sym(input$nj_fruit_variable_4)),
              offset = 0.15,
              width = width * 3,
              alpha = 1
            )
          } else {
            geom_fruit(
              geom = geom_tile,
              mapping = aes(fill= !!sym(input$nj_fruit_variable_4)),
              offset = 0.05,
              width = width,
              alpha = 1
            )
          }
        } else {NULL}
      }
    }
  })
  
  nj_fruit5 <- reactive({
    if((!is.null(input$nj_tiles_show_5)) & 
       (!is.null(input$nj_fruit_variable_5)) & 
       (!is.null(input$nj_layout)) & 
       (!is.null(input$nj_fruit_offset_circ_5)) & 
       (!is.null(input$nj_fruit_width_circ_5)) & 
       (!is.null(input$nj_fruit_alpha_5))) {
      if(input$nj_tiles_show_5 == TRUE) {
        if(input$nj_layout == "circular" | input$nj_layout == "inward") {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill = !!sym(input$nj_fruit_variable_5)),
            offset = input$nj_fruit_offset_circ_5,
            width = input$nj_fruit_width_circ_5,
            alpha = input$nj_fruit_alpha_5
          )
        } else {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill = !!sym(input$nj_fruit_variable_5)),
            offset = input$nj_fruit_offset_circ_5,
            width = input$nj_fruit_width_circ_5,
            alpha = input$nj_fruit_alpha_5
          )
        }
      } else {NULL}
    } else {
      if(input$nj_tiles_show_5 == TRUE) {
        if(!is.null(Vis$nj_max_x)) {
          if(round(ceiling(Vis$nj_max_x) * 0.1, 0) < 1) {
            width <- 1
          } else {
            width <- round(ceiling(Vis$nj_max_x) * 0.033, 0)
          }
        } else {
          width <- 2
        }
        if(input$nj_layout == "circular" | input$nj_layout == "inward") {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill= !!sym(input$nj_fruit_variable_5)),
            offset = 0.15,
            width = width * 3,
            alpha = 1
          )
        } else {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill= !!sym(input$nj_fruit_variable_5)),
            offset = 0.05,
            width = width,
            alpha = 1
          )
        }
      } else {NULL}
    }
  })
  
  # Xlim
  nj_limit <- reactive({
    if(input$nj_layout == "circular") {
      xlim(input$nj_xlim, NA)
    } else {NULL}
  })
  
  # Treescale
  nj_treescale <- reactive({
    if(!input$nj_layout == "circular") {
      if(input$nj_treescale_show == TRUE) {
        geom_treescale(x = nj_treescale_x(),
                       y = nj_treescale_y(),
                       width = nj_treescale_width(),
                       color = input$nj_color,
                       fontsize = 4)
      } else {NULL}
    } else {NULL}
  }) 
  
  # Treescale Y Position
  nj_treescale_y <- reactive({
    if(is.null(input$nj_treescale_y)) {
      0
    } else {input$nj_treescale_y}
  })
  
  # Treescale X Position
  nj_treescale_x <- reactive({
    if(is.null(input$nj_treescale_x)) {
      round(ceiling(Vis$nj_max_x) * 0.2, 0)
    } else {input$nj_treescale_x}
  })
  
  # Treescale width
  nj_treescale_width <- reactive({
    if(!is.null(input$nj_treescale_width)) {
      input$nj_treescale_width
    } else {
      round(ceiling(Vis$nj_max_x) * 0.1, 0)
    }
  })
  
  # Label branches
  nj_label_branch <- reactive({
    if(!input$nj_layout == "circular" | !input$nj_layout == "inward") {
      if(input$nj_show_branch_label == TRUE) {
        geom_label(
          aes(
            x=!!sym("branch"), 
            label= !!sym(input$nj_branch_label)),
          fill = input$nj_branch_label_color,
          size = nj_branch_size(),
          label.r = unit(input$nj_branch_labelradius, "lines"),
          nudge_x = input$nj_branch_x,
          nudge_y = input$nj_branch_y,
          fontface = input$nj_branchlab_fontface,
          alpha = input$nj_branchlab_alpha
        )
      } else {NULL}
    } else {NULL}
  })
  
  # Branch label size
  nj_branch_size <- reactive({
    if(!is.null(input$nj_branch_size)) {
      input$nj_branch_size
    } else {
      Vis$branch_size_nj
    }
  })
  
  # Rootedge
  nj_rootedge <- reactive({
    if(input$nj_rootedge_show == TRUE) {
      if(is.null(input$nj_rootedge_length)) {
        geom_rootedge(rootedge = round(ceiling(Vis$nj_max_x) * 0.05, 0),
                      linetype = input$nj_rootedge_line)
      } else {
        geom_rootedge(rootedge = input$nj_rootedge_length,
                      linetype = input$nj_rootedge_line)
      }
    } else {NULL}
  })
  
  # Tippoints
  nj_tippoint <- reactive({
    if(input$nj_tippoint_show == TRUE | input$nj_tipcolor_mapping_show == TRUE | input$nj_tipshape_mapping_show == TRUE) {
      if(input$nj_tipcolor_mapping_show == TRUE & input$nj_tipshape_mapping_show == FALSE) {
        geom_tippoint(
          aes(color = !!sym(input$nj_tipcolor_mapping)),
          alpha = input$nj_tippoint_alpha,
          shape = input$nj_tippoint_shape,
          size = nj_tippoint_size()
        )
      } else if (input$nj_tipcolor_mapping_show == FALSE & input$nj_tipshape_mapping_show == TRUE) {
        geom_tippoint(
          aes(shape = !!sym(input$nj_tipshape_mapping)),
          alpha = input$nj_tippoint_alpha,
          color = input$nj_tippoint_color,
          size = nj_tippoint_size()
        )
      } else if (input$nj_tipcolor_mapping_show == TRUE & input$nj_tipshape_mapping_show == TRUE) {
        geom_tippoint(
          aes(shape = !!sym(input$nj_tipshape_mapping),
              color = !!sym(input$nj_tipcolor_mapping)),
          alpha = input$nj_tippoint_alpha,
          size = nj_tippoint_size()
        )
      } else {
        geom_tippoint(
          alpha = input$nj_tippoint_alpha,
          colour = input$nj_tippoint_color,
          fill = input$nj_tippoint_color,
          shape = input$nj_tippoint_shape,
          size = nj_tippoint_size()
        )
      } 
    } else {NULL}
  })
  
  # Nodepoints
  nj_nodepoint <- reactive({
    if(input$nj_nodepoint_show == TRUE) {
      geom_nodepoint(
        alpha = input$nj_nodepoint_alpha,
        color = input$nj_nodepoint_color,
        shape = input$nj_nodepoint_shape,
        size = nj_nodepoint_size()
      )
    } else {NULL}
  })
  
  # Nodepoint size
  nj_nodepoint_size <- reactive({
    if(!is.null(input$nj_nodepoint_size)) {
      input$nj_nodepoint_size
    } else {
      Vis$nodepointsize_nj
    }
  })
  
  # NJ circular or not
  nj_tiplab <- reactive({
    if(input$nj_tiplab_show == TRUE) {
      if(input$nj_layout == "circular") {
        if(input$nj_mapping_show == TRUE) {
          geom_tiplab(
            nj_mapping_tiplab(), 
            geom = "text",
            size = nj_tiplab_size(),
            alpha = input$nj_tiplab_alpha,
            fontface = input$nj_tiplab_fontface,
            align = as.logical(input$nj_align),
            hjust = as.numeric(input$nj_tiplab_position),
            check.overlap = input$nj_tiplab_overlap
          )
        } else {
          geom_tiplab(
            nj_mapping_tiplab(),
            color = input$nj_tiplab_color,
            geom = "text",
            size = nj_tiplab_size(),
            alpha = input$nj_tiplab_alpha,
            fontface = input$nj_tiplab_fontface,
            align = as.logical(input$nj_align),
            hjust = as.numeric(input$nj_tiplab_position),
            check.overlap = input$nj_tiplab_overlap
          )
        }
      } else if (input$nj_layout == "inward") {
        if(input$nj_mapping_show == TRUE) {
          geom_tiplab(
            nj_mapping_tiplab(), 
            geom = "text",
            size = nj_tiplab_size(),
            alpha = input$nj_tiplab_alpha,
            fontface = input$nj_tiplab_fontface,
            align = as.logical(input$nj_align),
            hjust = as.numeric(input$nj_tiplab_position_inw),
            check.overlap = input$nj_tiplab_overlap
          )
        } else {
          geom_tiplab(
            nj_mapping_tiplab(),
            color = input$nj_tiplab_color,
            geom = "text",
            size = nj_tiplab_size(),
            alpha = input$nj_tiplab_alpha,
            fontface = input$nj_tiplab_fontface,
            align = as.logical(input$nj_align),
            hjust = as.numeric(input$nj_tiplab_position_inw),
            check.overlap = input$nj_tiplab_overlap
          )
        }
      } else {
        if(input$nj_mapping_show == TRUE) {
          if(input$nj_geom == TRUE) {
            geom_tiplab(
              nj_mapping_tiplab(), 
              geom = nj_geom(),
              angle = input$nj_tiplab_angle,
              size = nj_tiplab_size(),
              alpha = input$nj_tiplab_alpha,
              fontface = input$nj_tiplab_fontface,
              align = as.logical(input$nj_align),
              nudge_x = input$nj_tiplab_nudge_x,
              check.overlap = input$nj_tiplab_overlap,
              label.padding = unit(nj_tiplab_padding(), "lines"),
              label.r = unit(input$nj_tiplab_labelradius, "lines"), 
              fill = input$nj_tiplab_fill
            )
          } else {
            geom_tiplab(
              nj_mapping_tiplab(), 
              geom = nj_geom(),
              angle = input$nj_tiplab_angle,
              size = nj_tiplab_size(),
              alpha = input$nj_tiplab_alpha,
              fontface = input$nj_tiplab_fontface,
              align = as.logical(input$nj_align),
              nudge_x = input$nj_tiplab_nudge_x,
              check.overlap = input$nj_tiplab_overlap
            )
          }
        } else {
          if(input$nj_geom == TRUE) {
            geom_tiplab(
              nj_mapping_tiplab(), 
              geom = nj_geom(),
              color = input$nj_tiplab_color,
              angle = input$nj_tiplab_angle,
              size = nj_tiplab_size(),
              alpha = input$nj_tiplab_alpha,
              fontface = input$nj_tiplab_fontface,
              align = as.logical(input$nj_align),
              nudge_x = input$nj_tiplab_nudge_x,
              check.overlap = input$nj_tiplab_overlap,
              label.padding = unit(nj_tiplab_padding(), "lines"),
              label.r = unit(input$nj_tiplab_labelradius, "lines"), 
              fill = input$nj_tiplab_fill
            )
          } else {
            geom_tiplab(
              nj_mapping_tiplab(), 
              geom = nj_geom(),
              color = input$nj_tiplab_color,
              angle = input$nj_tiplab_angle,
              size = nj_tiplab_size(),
              alpha = input$nj_tiplab_alpha,
              fontface = input$nj_tiplab_fontface,
              align = as.logical(input$nj_align),
              nudge_x = input$nj_tiplab_nudge_x,
              check.overlap = input$nj_tiplab_overlap
            )
          }
        }
      }
    } else {NULL}
  })
  
  # Tip panel size
  nj_tiplab_padding <- reactive({
    if(!is.null(input$nj_tiplab_padding)) {
      input$nj_tiplab_padding
    } else {
      Vis$tiplab_padding_nj
    }
  })
  
  # Tiplab size
  nj_tiplab_size <- reactive({
    if(!is.null(input$nj_tiplab_size)) {
      input$nj_tiplab_size
    } else {
      Vis$labelsize_nj
    }
  })
  
  # Tippoint size
  nj_tippoint_size <- reactive({
    if(!is.null(input$nj_tippoint_size)) {
      input$nj_tippoint_size
    } else {
      Vis$tippointsize_nj
    }
  })
  
  # Show Label Panels?
  nj_geom <- reactive({
    if(input$nj_geom == TRUE) {
      "label"
    } else {"text"}
  })
  
  # NJ Tiplab color
  nj_mapping_tiplab <- reactive({
    if(input$nj_mapping_show == TRUE) {
      if(!is.null(input$nj_tiplab)) {
        aes(label = !!sym(input$nj_tiplab),
            color = !!sym(input$nj_color_mapping))
      } else {
        aes(label = !!sym("Assembly Name"),
            color = !!sym(input$nj_color_mapping))
      }
    } else {
      if(!is.null(input$nj_tiplab)) {
        aes(label = !!sym(input$nj_tiplab))
      } else {
        aes(label = !!sym("Assembly Name"))
      }
    }
  })
  
  # NJ Tree Layout
  layout_nj <- reactive({
    if(input$nj_layout == "inward") {
      "circular"
    } else {input$nj_layout}
  })
  
  # NJ inward circular
  nj_inward <- reactive({
    if (input$nj_layout == "inward") {
      layout_inward_circular(xlim = input$nj_inward_xlim)
    } else {
      NULL
    }
  })
  
  #### UPGMA ----
  
  upgma_tree <- reactive({
    if(input$upgma_nodelabel_show == TRUE) {
      ggtree(Vis$upgma, alpha = 0.2) + 
        geom_nodelab(aes(label = node), color = "#29303A", size = upgma_tiplab_size() + 1, hjust = 0.7) +
        upgma_limit() +
        upgma_inward() 
    } else {
      tree <-
        ggtree(Vis$upgma, 
               color = input$upgma_color,
               layout = layout_upgma(),
               ladderize = input$upgma_ladder) %<+% Vis$meta_upgma +
        upgma_tiplab() +
        upgma_tiplab_scale() + 
        new_scale_color() +
        upgma_limit() +
        upgma_inward() +
        upgma_label_branch() +
        upgma_treescale() +
        upgma_nodepoint() +
        upgma_tippoint() +
        upgma_tippoint_scale() + 
        new_scale_color() +
        upgma_clip_label() +
        upgma_rootedge() +
        upgma_clades() +
        ggtitle(label = input$upgma_title,
                subtitle = input$upgma_subtitle) +
        theme_tree(bgcolor = input$upgma_bg) +
        theme(plot.title = element_text(colour = input$upgma_title_color,
                                        size = input$upgma_title_size),
              plot.subtitle = element_text(colour = input$upgma_title_color,
                                           size = input$upgma_subtitle_size),
              legend.background = element_rect(fill = input$upgma_bg),
              legend.direction = input$upgma_legend_orientation,
              legend.title = element_text(color = input$upgma_color,
                                          size = input$upgma_legend_size*1.2),
              legend.title.align = 0.5,
              legend.position = upgma_legend_pos(),
              legend.text = element_text(color = input$upgma_color, 
                                         size = input$upgma_legend_size),
              legend.key = element_rect(fill = input$upgma_bg),
              legend.box.spacing = unit(1.5, "cm"),
              legend.key.size = unit(0.05*input$upgma_legend_size, 'cm'),
              plot.background = element_rect(fill = input$upgma_bg, color = input$upgma_bg)) +
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
        upgma_gradient5() +
        new_scale_fill() 
      
      # Add custom labels
      if(length(Vis$custom_label_upgma) > 0) {
        
        for(i in Vis$custom_label_upgma[,1]) {
          
          if(!is.null(Vis$upgma_label_pos_x[[i]])) {
            x_pos <- Vis$upgma_label_pos_x[[i]]
          } else {
            x_pos <- round(Vis$upgma_max_x / 2, 0)
          }
          
          if(!is.null(Vis$upgma_label_pos_y[[i]])) {
            y_pos <- Vis$upgma_label_pos_y[[i]]
          } else {
            y_pos <- sum(DB$data$Include) / 2
          }
          
          if(!is.null(Vis$upgma_label_size[[i]])) {
            size <- Vis$upgma_label_size[[i]]
          } else {
            size <- 5
          }
          
          tree <- tree + annotate("text",
                                  x = x_pos,
                                  y = y_pos, 
                                  label = i,
                                  size = size)
        }
      }
      
      # Add heatmap
      if(input$upgma_heatmap_show == TRUE & length(input$upgma_heatmap_select) > 0) {
        if (!(any(sapply(DB$meta[input$upgma_heatmap_select], is.numeric)) & 
              any(!sapply(DB$meta[input$upgma_heatmap_select], is.numeric)))) {
          tree <- gheatmap.mod(tree, 
                               data = select(Vis$meta_upgma, input$upgma_heatmap_select),
                               offset = upgma_heatmap_offset(),
                               width = upgma_heatmap_width(),
                               legend_title = input$upgma_heatmap_title,
                               colnames_angle = -upgma_colnames_angle(),
                               colnames_offset_y = upgma_colnames_y(),
                               colnames_color = input$upgma_color) +
            upgma_heatmap_scale()
        }
      } 
      
      # Sizing control
      Vis$upgma_plot <- ggplotify::as.ggplot(tree, 
                                             scale = input$upgma_zoom,
                                             hjust = input$upgma_h,
                                             vjust = input$upgma_v)  
      
      # Correct background color if zoomed out
      cowplot::ggdraw(Vis$upgma_plot) + 
        theme(plot.background = element_rect(fill = input$upgma_bg, color = input$upgma_bg))
    }
  })
  
  # Heatmap width
  upgma_heatmap_width <- reactive({
    if(!is.null(input$upgma_heatmap_width)) {
      input$upgma_heatmap_width
    } else {
      length_input <- length(input$upgma_heatmap_select)
      if((!(input$upgma_layout == "circular")) & (!(input$upgma_layout == "inward"))) {
        if(length_input < 3) {
          0.1
        } else {
          if (length_input >= 3 && length_input <= 50) {
            min(0.15 + 0.05 * floor((length_input - 3) / 2), 1.5)
          } else {
            1.5
          }   
        }
      } else {
        if(length_input < 3) {
          0.3
        } else if (length_input >= 3 && length_input <= 27) {
          min(0.6 + 0.2 * floor((length_input - 3) / 2), 1.5)
        } else {
          3
        }
      }
    }
  })
  
  # Heatmap column titles position
  upgma_colnames_y <- reactive({
    if(!is.null(input$upgma_colnames_y)) {
      input$upgma_colnames_y
    } else {
      if(input$upgma_layout == "inward" | input$upgma_layout == "circular") {
        0
      } else {-1}
    }
  })
  
  # Heatmap column titles angle
  upgma_colnames_angle <- reactive({
    if(!is.null(input$upgma_colnames_angle)) {
      input$upgma_colnames_angle
    } else {
      if(!is.null(input$upgma_layout)) {
        if(input$upgma_layout == "inward" | input$upgma_layout == "circular") {
          90
        } else {-90}
      } else {-90}
    }
  })
  
  # Heatmap scale
  upgma_heatmap_scale <- reactive({
    if(!is.null(input$upgma_heatmap_scale) & !is.null(input$upgma_heatmap_div_mid)) {
      if(input$upgma_heatmap_scale %in% c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")) {
        if(input$upgma_heatmap_div_mid == "Zero") {
          midpoint <- 0
        } else if(input$upgma_heatmap_div_mid == "Mean") {
          midpoint <- mean(as.matrix(Vis$meta_upgma[input$upgma_heatmap_select]), na.rm = TRUE)
        } else {
          midpoint <- median(as.matrix(Vis$meta_upgma[input$upgma_heatmap_select]), na.rm = TRUE)
        }
        scale_fill_gradient2(low = brewer.pal(3, input$upgma_heatmap_scale)[1],
                             mid = brewer.pal(3, input$upgma_heatmap_scale)[2],
                             high = brewer.pal(3, input$upgma_heatmap_scale)[3],
                             midpoint = midpoint,
                             name = input$upgma_heatmap_title)
      } else {
        if(input$upgma_heatmap_scale %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")) {
          if(class(unlist(DB$meta[input$upgma_heatmap_select])) == "numeric") {
            if(input$upgma_heatmap_scale == "magma") {
              scale_fill_viridis(option = "A",
                                 name = input$upgma_heatmap_title)
            } else if(input$upgma_heatmap_scale == "inferno") {
              scale_fill_viridis(option = "B",
                                 name = input$upgma_heatmap_title)
            } else if(input$upgma_heatmap_scale == "plasma") {
              scale_fill_viridis(option = "C",
                                 name = input$upgma_heatmap_title)
            } else if(input$upgma_heatmap_scale == "viridis") {
              scale_fill_viridis(option = "D",
                                 name = input$upgma_heatmap_title)
            } else if(input$upgma_heatmap_scale == "cividis") {
              scale_fill_viridis(option = "E",
                                 name = input$upgma_heatmap_title)
            } else if(input$upgma_heatmap_scale == "rocket") {
              scale_fill_viridis(option = "F",
                                 name = input$upgma_heatmap_title)
            } else if(input$upgma_heatmap_scale == "mako") {
              scale_fill_viridis(option = "G",
                                 name = input$upgma_heatmap_title)
            } else if(input$upgma_heatmap_scale == "turbo") {
              scale_fill_viridis(option = "H",
                                 name = input$upgma_heatmap_title)
            } 
          } else {
            if(input$upgma_heatmap_scale == "magma") {
              scale_fill_viridis(discrete = TRUE, option = "A",
                                 name = input$upgma_heatmap_title)
            } else if(input$upgma_heatmap_scale == "inferno") {
              scale_fill_viridis(discrete = TRUE, option = "B",
                                 name = input$upgma_heatmap_title)
            } else if(input$upgma_heatmap_scale == "plasma") {
              scale_fill_viridis(discrete = TRUE, option = "C",
                                 name = input$upgma_heatmap_title)
            } else if(input$upgma_heatmap_scale == "viridis") {
              scale_fill_viridis(discrete = TRUE, option = "D",
                                 name = input$upgma_heatmap_title)
            } else if(input$upgma_heatmap_scale == "cividis") {
              scale_fill_viridis(discrete = TRUE, option = "E",
                                 name = input$upgma_heatmap_title)
            } else if(input$upgma_heatmap_scale == "rocket") {
              scale_fill_viridis(discrete = TRUE, option = "F",
                                 name = input$upgma_heatmap_title)
            } else if(input$upgma_heatmap_scale == "mako") {
              scale_fill_viridis(discrete = TRUE, option = "G",
                                 name = input$upgma_heatmap_title)
            } else if(input$upgma_heatmap_scale == "turbo") {
              scale_fill_viridis(discrete = TRUE, option = "H",
                                 name = input$upgma_heatmap_title)
            } 
          }
        } else {
          scale_fill_brewer(palette = input$upgma_heatmap_scale,
                            name = input$upgma_heatmap_title)
        }
      }
    }
  })
  
  # Tippoint Scale
  upgma_tippoint_scale <- reactive({
    if(!is.null(input$upgma_tippoint_scale) & !is.null(input$upgma_tipcolor_mapping_div_mid)) {
      if(input$upgma_tippoint_scale %in% c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")) {
        if(input$upgma_tipcolor_mapping_div_mid == "Zero") {
          midpoint <- 0
        } else if(input$upgma_tipcolor_mapping_div_mid == "Mean") {
          midpoint <- mean(as.matrix(Vis$meta_upgma[input$upgma_tipcolor_mapping]), na.rm = TRUE)
        } else {
          midpoint <- median(as.matrix(Vis$meta_upgma[input$upgma_tipcolor_mapping]), na.rm = TRUE)
        }
        scale_color_gradient2(low = brewer.pal(3, input$upgma_tippoint_scale)[1],
                              mid = brewer.pal(3, input$upgma_tippoint_scale)[2],
                              high = brewer.pal(3, input$upgma_tippoint_scale)[3],
                              midpoint = midpoint)
      } else {
        if(input$upgma_tippoint_scale %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")) {
          if(class(unlist(DB$meta[input$upgma_tipcolor_mapping])) == "numeric") {
            if(input$upgma_tippoint_scale == "magma") {
              scale_color_viridis(option = "A")
            } else if(input$upgma_tippoint_scale == "inferno") {
              scale_color_viridis(option = "B")
            } else if(input$upgma_tippoint_scale == "plasma") {
              scale_color_viridis(option = "C")
            } else if(input$upgma_tippoint_scale == "viridis") {
              scale_color_viridis(option = "D")
            } else if(input$upgma_tippoint_scale == "cividis") {
              scale_color_viridis(option = "E")
            } else if(input$upgma_tippoint_scale == "rocket") {
              scale_color_viridis(option = "F")
            } else if(input$upgma_tippoint_scale == "mako") {
              scale_color_viridis(option = "G")
            } else if(input$upgma_tippoint_scale == "turbo") {
              scale_color_viridis(option = "H")
            } 
          } else {
            if(input$upgma_tippoint_scale == "magma") {
              scale_color_viridis(discrete = TRUE, option = "A")
            } else if(input$upgma_tippoint_scale == "inferno") {
              scale_color_viridis(discrete = TRUE, option = "B")
            } else if(input$upgma_tippoint_scale == "plasma") {
              scale_color_viridis(discrete = TRUE, option = "C")
            } else if(input$upgma_tippoint_scale == "viridis") {
              scale_color_viridis(discrete = TRUE, option = "D")
            } else if(input$upgma_tippoint_scale == "cividis") {
              scale_color_viridis(discrete = TRUE, option = "E")
            } else if(input$upgma_tippoint_scale == "rocket") {
              scale_color_viridis(discrete = TRUE, option = "F")
            } else if(input$upgma_tippoint_scale == "mako") {
              scale_color_viridis(discrete = TRUE, option = "G")
            } else if(input$upgma_tippoint_scale == "turbo") {
              scale_color_viridis(discrete = TRUE, option = "H")
            } 
          }
        } else {
          scale_color_brewer(palette = input$upgma_tippoint_scale)
        }
      }
    }
  })
  
  # Tiplab Scale
  upgma_tiplab_scale <- reactive({
    if(!is.null(input$upgma_tiplab_scale) & !is.null(input$upgma_color_mapping_div_mid)) {
      if(input$upgma_tiplab_scale %in% c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")) {
        if(input$upgma_color_mapping_div_mid == "Zero") {
          midpoint <- 0
        } else if(input$upgma_color_mapping_div_mid == "Mean") {
          midpoint <- mean(as.matrix(Vis$meta_upgma[input$upgma_color_mapping]), na.rm = TRUE)
        } else {
          midpoint <- median(as.matrix(Vis$meta_upgma[input$upgma_color_mapping]), na.rm = TRUE)
        }
        scale_color_gradient2(low = brewer.pal(3, input$upgma_tiplab_scale)[1],
                              mid = brewer.pal(3, input$upgma_tiplab_scale)[2],
                              high = brewer.pal(3, input$upgma_tiplab_scale)[3],
                              midpoint = midpoint)
      } else {
        if(input$upgma_tiplab_scale %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")) {
          if(class(unlist(DB$meta[input$upgma_color_mapping])) == "numeric") {
            if(input$upgma_tiplab_scale == "magma") {
              scale_color_viridis(option = "A")
            } else if(input$upgma_tiplab_scale == "inferno") {
              scale_color_viridis(option = "B")
            } else if(input$upgma_tiplab_scale == "plasma") {
              scale_color_viridis(option = "C")
            } else if(input$upgma_tiplab_scale == "viridis") {
              scale_color_viridis(option = "D")
            } else if(input$upgma_tiplab_scale == "cividis") {
              scale_color_viridis(option = "E")
            } else if(input$upgma_tiplab_scale == "rocket") {
              scale_color_viridis(option = "F")
            } else if(input$upgma_tiplab_scale == "mako") {
              scale_color_viridis(option = "G")
            } else if(input$upgma_tiplab_scale == "turbo") {
              scale_color_viridis(option = "H")
            } 
          } else {
            if(input$upgma_tiplab_scale == "magma") {
              scale_color_viridis(discrete = TRUE, option = "A")
            } else if(input$upgma_tiplab_scale == "inferno") {
              scale_color_viridis(discrete = TRUE, option = "B")
            } else if(input$upgma_tiplab_scale == "plasma") {
              scale_color_viridis(discrete = TRUE, option = "C")
            } else if(input$upgma_tiplab_scale == "viridis") {
              scale_color_viridis(discrete = TRUE, option = "D")
            } else if(input$upgma_tiplab_scale == "cividis") {
              scale_color_viridis(discrete = TRUE, option = "E")
            } else if(input$upgma_tiplab_scale == "rocket") {
              scale_color_viridis(discrete = TRUE, option = "F")
            } else if(input$upgma_tiplab_scale == "mako") {
              scale_color_viridis(discrete = TRUE, option = "G")
            } else if(input$upgma_tiplab_scale == "turbo") {
              scale_color_viridis(discrete = TRUE, option = "H")
            } 
          }
        } else {
          scale_color_brewer(palette = input$upgma_tiplab_scale)
        }
      }
    }
  })
  
  # Clade Highlight
  upgma_clades <- reactive({
    if(!is.null(input$upgma_parentnode)) {
      if(!length(input$upgma_parentnode) == 0) {
        if(length(input$upgma_parentnode) == 1) {
          fill <- input$upgma_clade_scale
        } else if (length(input$upgma_parentnode) == 2) {
          fill <- brewer.pal(3, input$upgma_clade_scale)[1:2]
        } else {
          fill <- brewer.pal(length(input$upgma_parentnode), input$upgma_clade_scale)
        }
        geom_hilight(node = as.numeric(input$upgma_parentnode),
                     fill = fill,
                     align = input$upgma_clade_align,
                     type = input$upgma_clade_type,
                     gradient.direction = upgma_clade_grad_dir(),
                     gradient.length.out = upgma_clade_grad_len())
      } else {NULL}
    }
  })
  
  # Clade highlight gradient direction
  upgma_clade_grad_dir <- reactive({
    if(input$upgma_clade_type == "gradient") {
      input$upgma_clade_grad_dir
    } else {"rt"}
  })
  
  # Clade hightlight gradient length
  upgma_clade_grad_len <- reactive({
    if(input$upgma_clade_type == "gradient") {
      upgma_clade_grad_len
    } else {2}
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
    if(!is.null(input$upgma_tiles_show_1) & 
       !is.null(input$upgma_fruit_variable) & 
       !is.null(input$upgma_tiles_scale_1) &
       !is.null(input$upgma_tiles_mapping_div_mid_1)) {
      if(input$upgma_tiles_show_1 == TRUE) {
        if(input$upgma_tiles_scale_1 %in% c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")) {
          if(input$upgma_tiles_mapping_div_mid_1 == "Zero") {
            midpoint <- 0
          } else if(input$upgma_tiles_mapping_div_mid_1 == "Mean") {
            midpoint <- mean(as.matrix(Vis$meta_upgma[input$upgma_fruit_variable]), na.rm = TRUE)
          } else {
            midpoint <- median(as.matrix(Vis$meta_upgma[input$upgma_fruit_variable]), na.rm = TRUE)
          }
          scale_fill_gradient2(low = brewer.pal(3, input$upgma_tiles_scale_1)[1],
                               mid = brewer.pal(3, input$upgma_tiles_scale_1)[2],
                               high = brewer.pal(3, input$upgma_tiles_scale_1)[3],
                               midpoint = midpoint)
        } else {
          if(input$upgma_tiles_scale_1 %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")) {
            if(class(unlist(DB$meta[input$upgma_fruit_variable])) == "numeric") {
              if(input$upgma_tiles_scale_1 == "magma") {
                scale_fill_viridis(option = "A")
              } else if(input$upgma_tiles_scale_1 == "inferno") {
                scale_fill_viridis(option = "B")
              } else if(input$upgma_tiles_scale_1 == "plasma") {
                scale_fill_viridis(option = "C")
              } else if(input$upgma_tiles_scale_1 == "viridis") {
                scale_fill_viridis(option = "D")
              } else if(input$upgma_tiles_scale_1 == "cividis") {
                scale_fill_viridis(option = "E")
              } else if(input$upgma_tiles_scale_1 == "rocket") {
                scale_fill_viridis(option = "F")
              } else if(input$upgma_tiles_scale_1 == "mako") {
                scale_fill_viridis(option = "G")
              } else if(input$upgma_tiles_scale_1 == "turbo") {
                scale_fill_viridis(option = "H")
              } 
            } else {
              if(input$upgma_tiles_scale_1 == "magma") {
                scale_fill_viridis(discrete = TRUE, option = "A")
              } else if(input$upgma_tiles_scale_1 == "inferno") {
                scale_fill_viridis(discrete = TRUE, option = "B")
              } else if(input$upgma_tiles_scale_1 == "plasma") {
                scale_fill_viridis(discrete = TRUE, option = "C")
              } else if(input$upgma_tiles_scale_1 == "viridis") {
                scale_fill_viridis(discrete = TRUE, option = "D")
              } else if(input$upgma_tiles_scale_1 == "cividis") {
                scale_fill_viridis(discrete = TRUE, option = "E")
              } else if(input$upgma_tiles_scale_1 == "rocket") {
                scale_fill_viridis(discrete = TRUE, option = "F")
              } else if(input$upgma_tiles_scale_1 == "mako") {
                scale_fill_viridis(discrete = TRUE, option = "G")
              } else if(input$upgma_tiles_scale_1 == "turbo") {
                scale_fill_viridis(discrete = TRUE, option = "H")
              } 
            }
          } else {
            scale_fill_brewer(palette = input$upgma_tiles_scale_1)
          }
        }
      } else {NULL}
    }
  })
  
  upgma_gradient2 <- reactive({
    if(!is.null(input$upgma_tiles_show_2) & 
       !is.null(input$upgma_fruit_variable_2) & 
       !is.null(input$upgma_tiles_scale_2) &
       !is.null(input$upgma_tiles_mapping_div_mid_2)) {
      if(input$upgma_tiles_show_2 == TRUE) {
        if(input$upgma_tiles_scale_2 %in% c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")) {
          if(input$upgma_tiles_mapping_div_mid_2 == "Zero") {
            midpoint <- 0
          } else if(input$upgma_tiles_mapping_div_mid_2 == "Mean") {
            midpoint <- mean(as.matrix(Vis$meta_upgma[input$upgma_fruit_variable_2]), na.rm = TRUE)
          } else {
            midpoint <- median(as.matrix(Vis$meta_upgma[input$upgma_fruit_variable_2]), na.rm = TRUE)
          }
          scale_fill_gradient2(low = brewer.pal(3, input$upgma_tiles_scale_2)[1],
                               mid = brewer.pal(3, input$upgma_tiles_scale_2)[2],
                               high = brewer.pal(3, input$upgma_tiles_scale_2)[3],
                               midpoint = midpoint)
        } else {
          if(input$upgma_tiles_scale_2 %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")) {
            if(class(unlist(DB$meta[input$upgma_fruit_variable_2])) == "numeric") {
              if(input$upgma_tiles_scale_2 == "magma") {
                scale_fill_viridis(option = "A")
              } else if(input$upgma_tiles_scale_2 == "inferno") {
                scale_fill_viridis(option = "B")
              } else if(input$upgma_tiles_scale_2 == "plasma") {
                scale_fill_viridis(option = "C")
              } else if(input$upgma_tiles_scale_2 == "viridis") {
                scale_fill_viridis(option = "D")
              } else if(input$upgma_tiles_scale_2 == "cividis") {
                scale_fill_viridis(option = "E")
              } else if(input$upgma_tiles_scale_2 == "rocket") {
                scale_fill_viridis(option = "F")
              } else if(input$upgma_tiles_scale_2 == "mako") {
                scale_fill_viridis(option = "G")
              } else if(input$upgma_tiles_scale_2 == "turbo") {
                scale_fill_viridis(option = "H")
              } 
            } else {
              if(input$upgma_tiles_scale_2 == "magma") {
                scale_fill_viridis(discrete = TRUE, option = "A")
              } else if(input$upgma_tiles_scale_2 == "inferno") {
                scale_fill_viridis(discrete = TRUE, option = "B")
              } else if(input$upgma_tiles_scale_2 == "plasma") {
                scale_fill_viridis(discrete = TRUE, option = "C")
              } else if(input$upgma_tiles_scale_2 == "viridis") {
                scale_fill_viridis(discrete = TRUE, option = "D")
              } else if(input$upgma_tiles_scale_2 == "cividis") {
                scale_fill_viridis(discrete = TRUE, option = "E")
              } else if(input$upgma_tiles_scale_2 == "rocket") {
                scale_fill_viridis(discrete = TRUE, option = "F")
              } else if(input$upgma_tiles_scale_2 == "mako") {
                scale_fill_viridis(discrete = TRUE, option = "G")
              } else if(input$upgma_tiles_scale_2 == "turbo") {
                scale_fill_viridis(discrete = TRUE, option = "H")
              } 
            }
          } else {
            scale_fill_brewer(palette = input$upgma_tiles_scale_2)
          }
        }
      } else {NULL}
    }
  })
  
  upgma_gradient3 <- reactive({
    if(!is.null(input$upgma_tiles_show_3) & 
       !is.null(input$upgma_fruit_variable_3) & 
       !is.null(input$upgma_tiles_scale_3) &
       !is.null(input$upgma_tiles_mapping_div_mid_3)) {
      if(input$upgma_tiles_show_3 == TRUE) {
        if(input$upgma_tiles_scale_3 %in% c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")) {
          if(input$upgma_tiles_mapping_div_mid_3 == "Zero") {
            midpoint <- 0
          } else if(input$upgma_tiles_mapping_div_mid_3 == "Mean") {
            midpoint <- mean(as.matrix(Vis$meta_upgma[input$upgma_fruit_variable_3]), na.rm = TRUE)
          } else {
            midpoint <- median(as.matrix(Vis$meta_upgma[input$upgma_fruit_variable_3]), na.rm = TRUE)
          }
          scale_fill_gradient3(low = brewer.pal(3, input$upgma_tiles_scale_3)[1],
                               mid = brewer.pal(3, input$upgma_tiles_scale_3)[2],
                               high = brewer.pal(3, input$upgma_tiles_scale_3)[3],
                               midpoint = midpoint)
        } else {
          if(input$upgma_tiles_scale_3 %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")) {
            if(class(unlist(DB$meta[input$upgma_fruit_variable_3])) == "numeric") {
              if(input$upgma_tiles_scale_3 == "magma") {
                scale_fill_viridis(option = "A")
              } else if(input$upgma_tiles_scale_3 == "inferno") {
                scale_fill_viridis(option = "B")
              } else if(input$upgma_tiles_scale_3 == "plasma") {
                scale_fill_viridis(option = "C")
              } else if(input$upgma_tiles_scale_3 == "viridis") {
                scale_fill_viridis(option = "D")
              } else if(input$upgma_tiles_scale_3 == "cividis") {
                scale_fill_viridis(option = "E")
              } else if(input$upgma_tiles_scale_3 == "rocket") {
                scale_fill_viridis(option = "F")
              } else if(input$upgma_tiles_scale_3 == "mako") {
                scale_fill_viridis(option = "G")
              } else if(input$upgma_tiles_scale_3 == "turbo") {
                scale_fill_viridis(option = "H")
              } 
            } else {
              if(input$upgma_tiles_scale_3 == "magma") {
                scale_fill_viridis(discrete = TRUE, option = "A")
              } else if(input$upgma_tiles_scale_3 == "inferno") {
                scale_fill_viridis(discrete = TRUE, option = "B")
              } else if(input$upgma_tiles_scale_3 == "plasma") {
                scale_fill_viridis(discrete = TRUE, option = "C")
              } else if(input$upgma_tiles_scale_3 == "viridis") {
                scale_fill_viridis(discrete = TRUE, option = "D")
              } else if(input$upgma_tiles_scale_3 == "cividis") {
                scale_fill_viridis(discrete = TRUE, option = "E")
              } else if(input$upgma_tiles_scale_3 == "rocket") {
                scale_fill_viridis(discrete = TRUE, option = "F")
              } else if(input$upgma_tiles_scale_3 == "mako") {
                scale_fill_viridis(discrete = TRUE, option = "G")
              } else if(input$upgma_tiles_scale_3 == "turbo") {
                scale_fill_viridis(discrete = TRUE, option = "H")
              } 
            }
          } else {
            scale_fill_brewer(palette = input$upgma_tiles_scale_3)
          }
        }
      } else {NULL}
    }
  })
  
  upgma_gradient4 <- reactive({
    if(!is.null(input$upgma_tiles_show_4) & 
       !is.null(input$upgma_fruit_variable_4) & 
       !is.null(input$upgma_tiles_scale_4) &
       !is.null(input$upgma_tiles_mapping_div_mid_4)) {
      if(input$upgma_tiles_show_4 == TRUE) {
        if(input$upgma_tiles_scale_4 %in% c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")) {
          if(input$upgma_tiles_mapping_div_mid_4 == "Zero") {
            midpoint <- 0
          } else if(input$upgma_tiles_mapping_div_mid_4 == "Mean") {
            midpoint <- mean(as.matrix(Vis$meta_upgma[input$upgma_fruit_variable_4]), na.rm = TRUE)
          } else {
            midpoint <- median(as.matrix(Vis$meta_upgma[input$upgma_fruit_variable_4]), na.rm = TRUE)
          }
          scale_fill_gradient4(low = brewer.pal(3, input$upgma_tiles_scale_4)[1],
                               mid = brewer.pal(3, input$upgma_tiles_scale_4)[2],
                               high = brewer.pal(3, input$upgma_tiles_scale_4)[3],
                               midpoint = midpoint)
        } else {
          if(input$upgma_tiles_scale_4 %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")) {
            if(class(unlist(DB$meta[input$upgma_fruit_variable])) == "numeric") {
              if(input$upgma_tiles_scale_4 == "magma") {
                scale_fill_viridis(option = "A")
              } else if(input$upgma_tiles_scale_4 == "inferno") {
                scale_fill_viridis(option = "B")
              } else if(input$upgma_tiles_scale_4 == "plasma") {
                scale_fill_viridis(option = "C")
              } else if(input$upgma_tiles_scale_4 == "viridis") {
                scale_fill_viridis(option = "D")
              } else if(input$upgma_tiles_scale_4 == "cividis") {
                scale_fill_viridis(option = "E")
              } else if(input$upgma_tiles_scale_4 == "rocket") {
                scale_fill_viridis(option = "F")
              } else if(input$upgma_tiles_scale_4 == "mako") {
                scale_fill_viridis(option = "G")
              } else if(input$upgma_tiles_scale_4 == "turbo") {
                scale_fill_viridis(option = "H")
              } 
            } else {
              if(input$upgma_tiles_scale_4 == "magma") {
                scale_fill_viridis(discrete = TRUE, option = "A")
              } else if(input$upgma_tiles_scale_4 == "inferno") {
                scale_fill_viridis(discrete = TRUE, option = "B")
              } else if(input$upgma_tiles_scale_4 == "plasma") {
                scale_fill_viridis(discrete = TRUE, option = "C")
              } else if(input$upgma_tiles_scale_4 == "viridis") {
                scale_fill_viridis(discrete = TRUE, option = "D")
              } else if(input$upgma_tiles_scale_4 == "cividis") {
                scale_fill_viridis(discrete = TRUE, option = "E")
              } else if(input$upgma_tiles_scale_4 == "rocket") {
                scale_fill_viridis(discrete = TRUE, option = "F")
              } else if(input$upgma_tiles_scale_4 == "mako") {
                scale_fill_viridis(discrete = TRUE, option = "G")
              } else if(input$upgma_tiles_scale_4 == "turbo") {
                scale_fill_viridis(discrete = TRUE, option = "H")
              } 
            }
          } else {
            scale_fill_brewer(palette = input$upgma_tiles_scale_4)
          }
        }
      } else {NULL}
    }
  })
  
  upgma_gradient5 <- reactive({
    if(!is.null(input$upgma_tiles_show_5) & 
       !is.null(input$upgma_fruit_variable_5) & 
       !is.null(input$upgma_tiles_scale_5) &
       !is.null(input$upgma_tiles_mapping_div_mid_5)) {
      if(input$upgma_tiles_show_5 == TRUE) {
        if(input$upgma_tiles_scale_5 %in% c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")) {
          if(input$upgma_tiles_mapping_div_mid_5 == "Zero") {
            midpoint <- 0
          } else if(input$upgma_tiles_mapping_div_mid_5 == "Mean") {
            midpoint <- mean(as.matrix(Vis$meta_upgma[input$upgma_fruit_variable_5]), na.rm = TRUE)
          } else {
            midpoint <- median(as.matrix(Vis$meta_upgma[input$upgma_fruit_variable_5]), na.rm = TRUE)
          }
          scale_fill_gradient5(low = brewer.pal(3, input$upgma_tiles_scale_5)[1],
                               mid = brewer.pal(3, input$upgma_tiles_scale_5)[2],
                               high = brewer.pal(3, input$upgma_tiles_scale_5)[3],
                               midpoint = midpoint)
        } else {
          if(input$upgma_tiles_scale_5 %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")) {
            if(class(unlist(DB$meta[input$upgma_fruit_variable_5])) == "numeric") {
              if(input$upgma_tiles_scale_5 == "magma") {
                scale_fill_viridis(option = "A")
              } else if(input$upgma_tiles_scale_5 == "inferno") {
                scale_fill_viridis(option = "B")
              } else if(input$upgma_tiles_scale_5 == "plasma") {
                scale_fill_viridis(option = "C")
              } else if(input$upgma_tiles_scale_5 == "viridis") {
                scale_fill_viridis(option = "D")
              } else if(input$upgma_tiles_scale_5 == "cividis") {
                scale_fill_viridis(option = "E")
              } else if(input$upgma_tiles_scale_5 == "rocket") {
                scale_fill_viridis(option = "F")
              } else if(input$upgma_tiles_scale_5 == "mako") {
                scale_fill_viridis(option = "G")
              } else if(input$upgma_tiles_scale_5 == "turbo") {
                scale_fill_viridis(option = "H")
              } 
            } else {
              if(input$upgma_tiles_scale_5 == "magma") {
                scale_fill_viridis(discrete = TRUE, option = "A")
              } else if(input$upgma_tiles_scale_5 == "inferno") {
                scale_fill_viridis(discrete = TRUE, option = "B")
              } else if(input$upgma_tiles_scale_5 == "plasma") {
                scale_fill_viridis(discrete = TRUE, option = "C")
              } else if(input$upgma_tiles_scale_5 == "viridis") {
                scale_fill_viridis(discrete = TRUE, option = "D")
              } else if(input$upgma_tiles_scale_5 == "cividis") {
                scale_fill_viridis(discrete = TRUE, option = "E")
              } else if(input$upgma_tiles_scale_5 == "rocket") {
                scale_fill_viridis(discrete = TRUE, option = "F")
              } else if(input$upgma_tiles_scale_5 == "mako") {
                scale_fill_viridis(discrete = TRUE, option = "G")
              } else if(input$upgma_tiles_scale_5 == "turbo") {
                scale_fill_viridis(discrete = TRUE, option = "H")
              } 
            }
          } else {
            scale_fill_brewer(palette = input$upgma_tiles_scale_5)
          }
        }
      } else {NULL}
    }
  })
  
  # No label clip off for linear upgma tree
  upgma_clip_label <- reactive({
    if(!(input$upgma_layout == "circular" | input$upgma_layout == "inward")) {
      coord_cartesian(clip = "off")
    } else {NULL}
  })
  
  # Geom Fruit
  upgma_fruit <- reactive({
    if((!is.null(input$upgma_tiles_show_1)) & 
       (!is.null(input$upgma_fruit_variable)) & 
       (!is.null(input$upgma_layout)) & 
       (!is.null(input$upgma_fruit_offset_circ)) & 
       (!is.null(input$upgma_fruit_width_circ)) & 
       (!is.null(input$upgma_fruit_alpha))) {
      if(input$upgma_tiles_show_1 == TRUE) {
        if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill= !!sym(input$upgma_fruit_variable)),
            offset = input$upgma_fruit_offset_circ,
            width = input$upgma_fruit_width_circ,
            alpha = input$upgma_fruit_alpha
          )
        } else {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill= !!sym(input$upgma_fruit_variable)),
            offset = input$upgma_fruit_offset_circ,
            width = input$upgma_fruit_width_circ,
            alpha = input$upgma_fruit_alpha
          )
        }
      } else {NULL}
    } else {
      if(input$upgma_tiles_show_1 == TRUE) {
        if(!is.null(Vis$upgma_max_x)) {
          if(round(ceiling(Vis$upgma_max_x) * 0.1, 0) < 1) {
            width <- 1
          } else {
            width <- round(ceiling(Vis$upgma_max_x) * 0.033, 0)
          }
        } else {
          width <- 2
        }
        if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill= !!sym(input$upgma_fruit_variable)),
            offset = 0,
            width = width * 3,
            alpha = 1
          )
        } else {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill= !!sym(input$upgma_fruit_variable)),
            offset = 0,
            width = width,
            alpha = 1
          )
        }
      } else {NULL}
    }
  })
  
  # Geom Fruit
  upgma_fruit2 <- reactive({
    if((!is.null(input$upgma_tiles_show_2)) & 
       (!is.null(input$upgma_fruit_variable_2)) & 
       (!is.null(input$upgma_layout)) & 
       (!is.null(input$upgma_fruit_offset_circ_2)) & 
       (!is.null(input$upgma_fruit_width_circ_2)) & 
       (!is.null(input$upgma_fruit_alpha_2))) {
      if(input$upgma_tiles_show_2 == TRUE) {
        if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill = !!sym(input$upgma_fruit_variable_2)),
            offset = input$upgma_fruit_offset_circ_2,
            width = input$upgma_fruit_width_circ_2,
            alpha = input$upgma_fruit_alpha_2
          )
        } else {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill = !!sym(input$upgma_fruit_variable_2)),
            offset = input$upgma_fruit_offset_circ_2,
            width = input$upgma_fruit_width_circ_2,
            alpha = input$upgma_fruit_alpha_2
          )
        }
      } else {NULL}
    } else {
      if(input$upgma_tiles_show_2 == TRUE) {
        if(!is.null(Vis$upgma_max_x)) {
          if(round(ceiling(Vis$upgma_max_x) * 0.1, 0) < 1) {
            width <- 1
          } else {
            width <- round(ceiling(Vis$upgma_max_x) * 0.033, 0)
          }
        } else {
          width <- 2
        }
        if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill= !!sym(input$upgma_fruit_variable_2)),
            offset = 0.15,
            width = width * 3,
            alpha = 1
          )
        } else {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill= !!sym(input$upgma_fruit_variable_2)),
            offset = 0.05,
            width = width,
            alpha = 1
          )
        }
      } else {NULL}
    }
  })
  
  upgma_fruit3 <- reactive({
    if((!is.null(input$upgma_tiles_show_3)) & 
       (!is.null(input$upgma_fruit_variable_3)) & 
       (!is.null(input$upgma_layout)) & 
       (!is.null(input$upgma_fruit_offset_circ_3)) & 
       (!is.null(input$upgma_fruit_width_circ_3)) & 
       (!is.null(input$upgma_fruit_alpha_3))) {
      if(input$upgma_tiles_show_3 == TRUE) {
        if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill = !!sym(input$upgma_fruit_variable_3)),
            offset = input$upgma_fruit_offset_circ_3,
            width = input$upgma_fruit_width_circ_3,
            alpha = input$upgma_fruit_alpha_3
          )
        } else {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill = !!sym(input$upgma_fruit_variable_3)),
            offset = input$upgma_fruit_offset_circ_3,
            width = input$upgma_fruit_width_circ_3,
            alpha = input$upgma_fruit_alpha_3
          )
        }
      } else {NULL}
    } else {
      if(input$upgma_tiles_show_3 == TRUE) {
        if(!is.null(Vis$upgma_max_x)) {
          if(round(ceiling(Vis$upgma_max_x) * 0.1, 0) < 1) {
            width <- 1
          } else {
            width <- round(ceiling(Vis$upgma_max_x) * 0.033, 0)
          }
        } else {
          width <- 2
        }
        if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill= !!sym(input$upgma_fruit_variable_3)),
            offset = 0.15,
            width = width * 3,
            alpha = 1
          )
        } else {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill= !!sym(input$upgma_fruit_variable_3)),
            offset = 0.05,
            width = width,
            alpha = 1
          )
        }
      } else {NULL}
    }
  })
  
  upgma_fruit4 <- reactive({
    if((!is.null(input$upgma_tiles_show_4)) & 
       (!is.null(input$upgma_fruit_variable_4)) & 
       (!is.null(input$upgma_layout)) & 
       (!is.null(input$upgma_fruit_offset_circ_4)) & 
       (!is.null(input$upgma_fruit_width_circ_4)) & 
       (!is.null(input$upgma_fruit_alpha_4))) {
      if(input$upgma_tiles_show_4 == TRUE) {
        if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill = !!sym(input$upgma_fruit_variable_4)),
            offset = input$upgma_fruit_offset_circ_4,
            width = input$upgma_fruit_width_circ_4,
            alpha = input$upgma_fruit_alpha_4
          )
        } else {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill = !!sym(input$upgma_fruit_variable_4)),
            offset = input$upgma_fruit_offset_circ_4,
            width = input$upgma_fruit_width_circ_4,
            alpha = input$upgma_fruit_alpha_4
          )
        }
      } else {
        if(input$upgma_tiles_show_4 == TRUE) {
          if(!is.null(Vis$upgma_max_x)) {
            if(round(ceiling(Vis$upgma_max_x) * 0.1, 0) < 1) {
              width <- 1
            } else {
              width <- round(ceiling(Vis$upgma_max_x) * 0.033, 0)
            }
          } else {
            width <- 2
          }
          if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
            geom_fruit(
              geom = geom_tile,
              mapping = aes(fill= !!sym(input$upgma_fruit_variable_4)),
              offset = 0.15,
              width = width * 3,
              alpha = 1
            )
          } else {
            geom_fruit(
              geom = geom_tile,
              mapping = aes(fill= !!sym(input$upgma_fruit_variable_4)),
              offset = 0.05,
              width = width,
              alpha = 1
            )
          }
        } else {NULL}
      }
    }
  })
  
  upgma_fruit5 <- reactive({
    if((!is.null(input$upgma_tiles_show_5)) & 
       (!is.null(input$upgma_fruit_variable_5)) & 
       (!is.null(input$upgma_layout)) & 
       (!is.null(input$upgma_fruit_offset_circ_5)) & 
       (!is.null(input$upgma_fruit_width_circ_5)) & 
       (!is.null(input$upgma_fruit_alpha_5))) {
      if(input$upgma_tiles_show_5 == TRUE) {
        if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill = !!sym(input$upgma_fruit_variable_5)),
            offset = input$upgma_fruit_offset_circ_5,
            width = input$upgma_fruit_width_circ_5,
            alpha = input$upgma_fruit_alpha_5
          )
        } else {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill = !!sym(input$upgma_fruit_variable_5)),
            offset = input$upgma_fruit_offset_circ_5,
            width = input$upgma_fruit_width_circ_5,
            alpha = input$upgma_fruit_alpha_5
          )
        }
      } else {NULL}
    } else {
      if(input$upgma_tiles_show_5 == TRUE) {
        if(!is.null(Vis$upgma_max_x)) {
          if(round(ceiling(Vis$upgma_max_x) * 0.1, 0) < 1) {
            width <- 1
          } else {
            width <- round(ceiling(Vis$upgma_max_x) * 0.033, 0)
          }
        } else {
          width <- 2
        }
        if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill= !!sym(input$upgma_fruit_variable_5)),
            offset = 0.15,
            width = width * 3,
            alpha = 1
          )
        } else {
          geom_fruit(
            geom = geom_tile,
            mapping = aes(fill= !!sym(input$upgma_fruit_variable_5)),
            offset = 0.05,
            width = width,
            alpha = 1
          )
        }
      } else {NULL}
    }
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
        geom_treescale(x = upgma_treescale_x(),
                       y = upgma_treescale_y(),
                       width = upgma_treescale_width(),
                       color = input$upgma_color,
                       fontsize = 4)
      } else {NULL}
    } else {NULL}
  }) 
  
  # Treescale Y Position
  upgma_treescale_y <- reactive({
    if(is.null(input$upgma_treescale_y)) {
      0
    } else {input$upgma_treescale_y}
  })
  
  # Treescale X Position
  upgma_treescale_x <- reactive({
    if(is.null(input$upgma_treescale_x)) {
      round(ceiling(Vis$upgma_max_x) * 0.2, 0)
    } else {input$upgma_treescale_x}
  })
  
  # Treescale width
  upgma_treescale_width <- reactive({
    if(!is.null(input$upgma_treescale_width)) {
      input$upgma_treescale_width
    } else {
      round(ceiling(Vis$upgma_max_x) * 0.1, 0)
    }
  })
  
  # Label branches
  upgma_label_branch <- reactive({
    if(!input$upgma_layout == "circular" | !input$upgma_layout == "inward") {
      if(input$upgma_show_branch_label == TRUE) {
        geom_label(
          aes(
            x=!!sym("branch"), 
            label= !!sym(input$upgma_branch_label)),
          fill = input$upgma_branch_label_color,
          size = upgma_branch_size(),
          label.r = unit(input$upgma_branch_labelradius, "lines"),
          nudge_x = input$upgma_branch_x,
          nudge_y = input$upgma_branch_y,
          fontface = input$upgma_branchlab_fontface,
          alpha = input$upgma_branchlab_alpha
        )
      } else {NULL}
    } else {NULL}
  })
  
  # Branch label size
  upgma_branch_size <- reactive({
    if(!is.null(input$upgma_branch_size)) {
      input$upgma_branch_size
    } else {
      Vis$branch_size_upgma
    }
  })
  
  # Rootedge
  upgma_rootedge <- reactive({
    if(input$upgma_rootedge_show == TRUE) {
      if(is.null(input$upgma_rootedge_length)) {
        geom_rootedge(rootedge = round(ceiling(Vis$upgma_max_x) * 0.05, 0),
                      linetype = input$upgma_rootedge_line)
      } else {
        geom_rootedge(rootedge = input$upgma_rootedge_length,
                      linetype = input$upgma_rootedge_line)
      }
    } else {NULL}
  })
  
  # Tippoints
  upgma_tippoint <- reactive({
    if(input$upgma_tippoint_show == TRUE | input$upgma_tipcolor_mapping_show == TRUE | input$upgma_tipshape_mapping_show == TRUE) {
      if(input$upgma_tipcolor_mapping_show == TRUE & input$upgma_tipshape_mapping_show == FALSE) {
        geom_tippoint(
          aes(color = !!sym(input$upgma_tipcolor_mapping)),
          alpha = input$upgma_tippoint_alpha,
          shape = input$upgma_tippoint_shape,
          size = upgma_tippoint_size()
        )
      } else if (input$upgma_tipcolor_mapping_show == FALSE & input$upgma_tipshape_mapping_show == TRUE) {
        geom_tippoint(
          aes(shape = !!sym(input$upgma_tipshape_mapping)),
          alpha = input$upgma_tippoint_alpha,
          color = input$upgma_tippoint_color,
          size = upgma_tippoint_size()
        )
      } else if (input$upgma_tipcolor_mapping_show == TRUE & input$upgma_tipshape_mapping_show == TRUE) {
        geom_tippoint(
          aes(shape = !!sym(input$upgma_tipshape_mapping),
              color = !!sym(input$upgma_tipcolor_mapping)),
          alpha = input$upgma_tippoint_alpha,
          size = upgma_tippoint_size()
        )
      } else {
        geom_tippoint(
          alpha = input$upgma_tippoint_alpha,
          colour = input$upgma_tippoint_color,
          fill = input$upgma_tippoint_color,
          shape = input$upgma_tippoint_shape,
          size = upgma_tippoint_size()
        )
      } 
    } else {NULL}
  })
  
  # Nodepoints
  upgma_nodepoint <- reactive({
    if(input$upgma_nodepoint_show == TRUE) {
      geom_nodepoint(
        alpha = input$upgma_nodepoint_alpha,
        color = input$upgma_nodepoint_color,
        shape = input$upgma_nodepoint_shape,
        size = upgma_nodepoint_size()
      )
    } else {NULL}
  })
  
  # Nodepoint size
  upgma_nodepoint_size <- reactive({
    if(!is.null(input$upgma_nodepoint_size)) {
      input$upgma_nodepoint_size
    } else {
      Vis$nodepointsize_upgma
    }
  })
  
  # upgma circular or not
  upgma_tiplab <- reactive({
    if(input$upgma_tiplab_show == TRUE) {
      if(input$upgma_layout == "circular") {
        if(input$upgma_mapping_show == TRUE) {
          geom_tiplab(
            upgma_mapping_tiplab(), 
            geom = "text",
            size = upgma_tiplab_size(),
            alpha = input$upgma_tiplab_alpha,
            fontface = input$upgma_tiplab_fontface,
            align = as.logical(input$upgma_align),
            hjust = as.numeric(input$upgma_tiplab_position),
            check.overlap = input$upgma_tiplab_overlap
          )
        } else {
          geom_tiplab(
            upgma_mapping_tiplab(),
            color = input$upgma_tiplab_color,
            geom = "text",
            size = upgma_tiplab_size(),
            alpha = input$upgma_tiplab_alpha,
            fontface = input$upgma_tiplab_fontface,
            align = as.logical(input$upgma_align),
            hjust = as.numeric(input$upgma_tiplab_position),
            check.overlap = input$upgma_tiplab_overlap
          )
        }
      } else if (input$upgma_layout == "inward") {
        if(input$upgma_mapping_show == TRUE) {
          geom_tiplab(
            upgma_mapping_tiplab(), 
            geom = "text",
            size = upgma_tiplab_size(),
            alpha = input$upgma_tiplab_alpha,
            fontface = input$upgma_tiplab_fontface,
            align = as.logical(input$upgma_align),
            hjust = as.numeric(input$upgma_tiplab_position_inw),
            check.overlap = input$upgma_tiplab_overlap
          )
        } else {
          geom_tiplab(
            upgma_mapping_tiplab(),
            color = input$upgma_tiplab_color,
            geom = "text",
            size = upgma_tiplab_size(),
            alpha = input$upgma_tiplab_alpha,
            fontface = input$upgma_tiplab_fontface,
            align = as.logical(input$upgma_align),
            hjust = as.numeric(input$upgma_tiplab_position_inw),
            check.overlap = input$upgma_tiplab_overlap
          )
        }
      } else {
        if(input$upgma_mapping_show == TRUE) {
          if(input$upgma_geom == TRUE) {
            geom_tiplab(
              upgma_mapping_tiplab(), 
              geom = upgma_geom(),
              angle = input$upgma_tiplab_angle,
              size = upgma_tiplab_size(),
              alpha = input$upgma_tiplab_alpha,
              fontface = input$upgma_tiplab_fontface,
              align = as.logical(input$upgma_align),
              nudge_x = input$upgma_tiplab_nudge_x,
              check.overlap = input$upgma_tiplab_overlap,
              label.padding = unit(upgma_tiplab_padding(), "lines"),
              label.r = unit(input$upgma_tiplab_labelradius, "lines"), 
              fill = input$upgma_tiplab_fill
            )
          } else {
            geom_tiplab(
              upgma_mapping_tiplab(), 
              geom = upgma_geom(),
              angle = input$upgma_tiplab_angle,
              size = upgma_tiplab_size(),
              alpha = input$upgma_tiplab_alpha,
              fontface = input$upgma_tiplab_fontface,
              align = as.logical(input$upgma_align),
              nudge_x = input$upgma_tiplab_nudge_x,
              check.overlap = input$upgma_tiplab_overlap
            )
          }
        } else {
          if(input$upgma_geom == TRUE) {
            geom_tiplab(
              upgma_mapping_tiplab(), 
              geom = upgma_geom(),
              color = input$upgma_tiplab_color,
              angle = input$upgma_tiplab_angle,
              size = upgma_tiplab_size(),
              alpha = input$upgma_tiplab_alpha,
              fontface = input$upgma_tiplab_fontface,
              align = as.logical(input$upgma_align),
              nudge_x = input$upgma_tiplab_nudge_x,
              check.overlap = input$upgma_tiplab_overlap,
              label.padding = unit(upgma_tiplab_padding(), "lines"),
              label.r = unit(input$upgma_tiplab_labelradius, "lines"), 
              fill = input$upgma_tiplab_fill
            )
          } else {
            geom_tiplab(
              upgma_mapping_tiplab(), 
              geom = upgma_geom(),
              color = input$upgma_tiplab_color,
              angle = input$upgma_tiplab_angle,
              size = upgma_tiplab_size(),
              alpha = input$upgma_tiplab_alpha,
              fontface = input$upgma_tiplab_fontface,
              align = as.logical(input$upgma_align),
              nudge_x = input$upgma_tiplab_nudge_x,
              check.overlap = input$upgma_tiplab_overlap
            )
          }
        }
      }
    } else {NULL}
  })
  
  # Tip panel size
  upgma_tiplab_padding <- reactive({
    if(!is.null(input$upgma_tiplab_padding)) {
      input$upgma_tiplab_padding
    } else {
      Vis$tiplab_padding_upgma
    }
  })
  
  # Tiplab size
  upgma_tiplab_size <- reactive({
    if(!is.null(input$upgma_tiplab_size)) {
      input$upgma_tiplab_size
    } else {
      Vis$labelsize_upgma
    }
  })
  
  # Tippoint size
  upgma_tippoint_size <- reactive({
    if(!is.null(input$upgma_tippoint_size)) {
      input$upgma_tippoint_size
    } else {
      Vis$tippointsize_upgma
    }
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
      if(!is.null(input$upgma_tiplab)) {
        aes(label = !!sym(input$upgma_tiplab),
            color = !!sym(input$upgma_color_mapping))
      } else {
        aes(label = !!sym("Assembly Name"),
            color = !!sym(input$upgma_color_mapping))
      }
    } else {
      if(!is.null(input$upgma_tiplab)) {
        aes(label = !!sym(input$upgma_tiplab))
      } else {
        aes(label = !!sym("Assembly Name"))
      }
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
  
  # Conditional disabling of control elemenmts
  observe({
    
    # Tiles for inward layout
    if(input$nj_layout == "inward") {
      shinyjs::disable('nj_tiles_show') 
      shinyjs::disable('nj_tiles_show_2')
      shinyjs::disable('nj_tiles_show_3') 
      shinyjs::disable('nj_tiles_show_4')
      shinyjs::disable('nj_tiles_show_5') 
      shinyjs::disable('nj_fruit_variable')
      shinyjs::disable('nj_fruit_variable_2')
      shinyjs::disable('nj_fruit_variable_3')
      shinyjs::disable('nj_fruit_variable_4')
      shinyjs::disable('nj_fruit_variable_5')
      shinyjs::disable('nj_fruit_width')
      shinyjs::disable('nj_fruit_width_2')
      shinyjs::disable('nj_fruit_width_3')
      shinyjs::disable('nj_fruit_width_4')
      shinyjs::disable('nj_fruit_width_5')
      shinyjs::disable('nj_fruit_offset')
      shinyjs::disable('nj_fruit_offset_2')
      shinyjs::disable('nj_fruit_offset_3')
      shinyjs::disable('nj_fruit_offset_4')
      shinyjs::disable('nj_fruit_offset_5')
    } else {
      shinyjs::enable('nj_tiles_show') 
      shinyjs::enable('nj_tiles_show_2')
      shinyjs::enable('nj_tiles_show_3') 
      shinyjs::enable('nj_tiles_show_4')
      shinyjs::enable('nj_tiles_show_5') 
      shinyjs::enable('nj_fruit_variable')
      shinyjs::enable('nj_fruit_variable_2')
      shinyjs::enable('nj_fruit_variable_3')
      shinyjs::enable('nj_fruit_variable_4')
      shinyjs::enable('nj_fruit_variable_5')
      shinyjs::enable('nj_fruit_width')
      shinyjs::enable('nj_fruit_width_2')
      shinyjs::enable('nj_fruit_width_3')
      shinyjs::enable('nj_fruit_width_4')
      shinyjs::enable('nj_fruit_width_5')
      shinyjs::enable('nj_fruit_offset')
      shinyjs::enable('nj_fruit_offset_2')
      shinyjs::enable('nj_fruit_offset_3')
      shinyjs::enable('nj_fruit_offset_4')
      shinyjs::enable('nj_fruit_offset_5')
    }
    
    if(input$upgma_layout == "inward") {
      shinyjs::disable('upgma_tiles_show') 
      shinyjs::disable('upgma_tiles_show_2')
      shinyjs::disable('upgma_tiles_show_3') 
      shinyjs::disable('upgma_tiles_show_4')
      shinyjs::disable('upgma_tiles_show_5') 
      shinyjs::disable('upgma_fruit_variable')
      shinyjs::disable('upgma_fruit_variable_2')
      shinyjs::disable('upgma_fruit_variable_3')
      shinyjs::disable('upgma_fruit_variable_4')
      shinyjs::disable('upgma_fruit_variable_5')
      shinyjs::disable('upgma_fruit_width')
      shinyjs::disable('upgma_fruit_width_2')
      shinyjs::disable('upgma_fruit_width_3')
      shinyjs::disable('upgma_fruit_width_4')
      shinyjs::disable('upgma_fruit_width_5')
      shinyjs::disable('upgma_fruit_offset')
      shinyjs::disable('upgma_fruit_offset_2')
      shinyjs::disable('upgma_fruit_offset_3')
      shinyjs::disable('upgma_fruit_offset_4')
      shinyjs::disable('upgma_fruit_offset_5')
    } else {
      shinyjs::enable('upgma_tiles_show') 
      shinyjs::enable('upgma_tiles_show_2')
      shinyjs::enable('upgma_tiles_show_3') 
      shinyjs::enable('upgma_tiles_show_4')
      shinyjs::enable('upgma_tiles_show_5') 
      shinyjs::enable('upgma_fruit_variable')
      shinyjs::enable('upgma_fruit_variable_2')
      shinyjs::enable('upgma_fruit_variable_3')
      shinyjs::enable('upgma_fruit_variable_4')
      shinyjs::enable('upgma_fruit_variable_5')
      shinyjs::enable('upgma_fruit_width')
      shinyjs::enable('upgma_fruit_width_2')
      shinyjs::enable('upgma_fruit_width_3')
      shinyjs::enable('upgma_fruit_width_4')
      shinyjs::enable('upgma_fruit_width_5')
      shinyjs::enable('upgma_fruit_offset')
      shinyjs::enable('upgma_fruit_offset_2')
      shinyjs::enable('upgma_fruit_offset_3')
      shinyjs::enable('upgma_fruit_offset_4')
      shinyjs::enable('upgma_fruit_offset_5')
    }
    
    # Shut off branch labels for NJ and UPGMA plots for circular layout
    if(input$nj_layout == "circular" | input$nj_layout == "inward") {
      shinyjs::disable('nj_show_branch_label')
    } else {
      shinyjs::enable('nj_show_branch_label')
    }
    
    if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
      shinyjs::disable('upgma_show_branch_label')
    } else {
      shinyjs::enable('upgma_show_branch_label')
    }
  })
  
  #### Generate Plot ----
  
  hamming_nj <- reactive({
    if(anyNA(DB$allelic_profile)) {
      if(input$na_handling == "omit") {
        allelic_profile_noNA <- DB$allelic_profile[, colSums(is.na(DB$allelic_profile)) == 0]
        
        allelic_profile_noNA_true <- allelic_profile_noNA[which(DB$data$Include == TRUE),]
        
        proxy::dist(allelic_profile_noNA_true, method = hamming_distance)
        
      } else if(input$na_handling == "ignore_na"){
        proxy::dist(DB$allelic_profile_true, method = hamming_distance_ignore)
        
      } else {
        proxy::dist(DB$allelic_profile_true, method = hamming_distance_category)
      } 
      
    } else {proxy::dist(DB$allelic_profile_true, method = hamming_distance)}
  })
  
  hamming_mst <- reactive({
    if(anyNA(DB$allelic_profile)) {
      if(input$na_handling == "omit") {
        allelic_profile_noNA <- DB$allelic_profile[, colSums(is.na(DB$allelic_profile)) == 0]
        
        allelic_profile_noNA_true <- allelic_profile_noNA[which(DB$data$Include == TRUE),]
        
        dist <- proxy::dist(allelic_profile_noNA_true, method = hamming_distance)
        
      } else if (input$na_handling == "ignore_na") {
        dist <- proxy::dist(DB$allelic_profile_true, method = hamming_distance_ignore)
      } else {
        dist <- proxy::dist(DB$allelic_profile_true, method = hamming_distance_category)
      }
    } else {
      dist <- proxy::dist(DB$allelic_profile_true, method = hamming_distance)
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
        vector_col[count] <- DB$meta_true$`Assembly Name`[i]
        count <- count + 1
      }
      
      vector_row <- character(0)
      count <- 1
      for (i in df_unique$row) {
        vector_row[count] <- DB$meta_true$`Assembly Name`[i]
        count <- count + 1
      }
      
      col_id <- character(0)
      count <- 1
      for (i in df_unique$col) {
        col_id[count] <- DB$meta_true$`Assembly ID`[i]
        count <- count + 1
      }
      
      row_id <- character(0)
      count <- 1
      for (i in df_unique$row) {
        row_id[count] <- DB$meta_true$`Assembly ID`[i]
        count <- count + 1
      }
      
      col_index <- character(0)
      count <- 1
      for (i in df_unique$col) {
        col_index[count] <- DB$meta_true$Index[i]
        count <- count + 1
      }
      
      row_index <- character(0)
      count <- 1
      for (i in df_unique$row) {
        row_index[count] <- DB$meta_true$Index[i]
        count <- count + 1
      }
      
      col_date <- character(0)
      count <- 1
      for (i in df_unique$col) {
        col_date[count] <- DB$meta_true$`Isolation Date`[i]
        count <- count + 1
      }
      
      row_date <- character(0)
      count <- 1
      for (i in df_unique$row) {
        row_date[count] <- DB$meta_true$`Isolation Date`[i]
        count <- count + 1
      }
      
      col_host <- character(0)
      count <- 1
      for (i in df_unique$col) {
        col_host[count] <- DB$meta_true$Host[i]
        count <- count + 1
      }
      
      row_host <- character(0)
      count <- 1
      for (i in df_unique$row) {
        row_host[count] <- DB$meta_true$Host[i]
        count <- count + 1
      }
      
      col_country <- character(0)
      count <- 1
      for (i in df_unique$col) {
        col_country[count] <- DB$meta_true$Country[i]
        count <- count + 1
      }
      
      row_country <- character(0)
      count <- 1
      for (i in df_unique$row) {
        row_country[count] <- DB$meta_true$Country[i]
        count <- count + 1
      }
      
      col_city <- character(0)
      count <- 1
      for (i in df_unique$col) {
        col_city[count] <- DB$meta_true$City[i]
        count <- count + 1
      }
      
      row_city <- character(0)
      count <- 1
      for (i in df_unique$row) {
        row_city[count] <- DB$meta_true$City[i]
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
          Date <- append(Date, DB$meta_true$`Isolation Date`[which(DB$meta_true$Index == i)])
        }
        Date_merged <- append(Date_merged, paste(Date, collapse = "\n"))
      }
      
      Host_merged <- character(0)
      for(j in 1:length(final_cleaned$Index)) {
        Host <- character(0)
        for(i in strsplit(final_cleaned$Index, "\n")[[j]]) {
          Host <- append(Host, DB$meta_true$Host[which(DB$meta_true$Index == i)])
        }
        Host_merged <- append(Host_merged, paste(Host, collapse = "\n"))
      }
      
      Country_merged <- character(0)
      for(j in 1:length(final_cleaned$Index)) {
        Country <- character(0)
        for(i in strsplit(final_cleaned$Index, "\n")[[j]]) {
          Country <- append(Country, DB$meta_true$Country[which(DB$meta_true$Index == i)])
        }
        Country_merged <- append(Country_merged, paste(Country, collapse = "\n"))
      }
      
      City_merged <- character(0)
      for(j in 1:length(final_cleaned$Index)) {
        City <- character(0)
        for(i in strsplit(final_cleaned$Index, "\n")[[j]]) {
          City <- append(City, DB$meta_true$City[which(DB$meta_true$Index == i)])
        }
        City_merged <- append(City_merged, paste(City, collapse = "\n"))
      }
      
      final_meta <- cbind(final_cleaned, "Isolation Date" = Date_merged, 
                          "Host" = Host_merged, "Country" = Country_merged, "City" = City_merged)
      
      
      # Merging with original data frame / allelic profile
      
      allelic_profile_true <- DB$allelic_profile_true
      meta_true <- DB$meta_true
      
      rownames(allelic_profile_true) <- DB$meta_true$`Assembly Name`
      rownames(meta_true) <- DB$meta_true$`Assembly Name`
      
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
      
      Vis$unique_meta <- meta_clean %>%
        cbind(font_size = font_size, valign = valign)
      
      # final dist calculation
      
      if(anyNA(DB$allelic_profile)){
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
      font_size <- rep(12, nrow(DB$meta_true))
      valign <- rep(-30, nrow(DB$meta_true))
      size <- rep(1, nrow(DB$meta_true))
      Vis$unique_meta <- DB$meta_true %>%
        cbind(size , font_size, valign)
      
      dist
    }
    
  })
  
  observeEvent(input$create_tree, {
    if(is.null(DB$data)) {
      show_toast(
        title = "Missing data",
        type = "error",
        position = "top-end",
        timer = 6000,
        width = "500px"
      )
    } else if(nrow(DB$allelic_profile_true) < 3) {
      show_toast(
        title = "Min. of 3 entries required for visualization",
        type = "error",
        position = "top-end",
        timer = 6000,
        width = "500px"
      )
    } else {
      
      if(any(duplicated(DB$meta$`Assembly Name`))) {
        showModal(
          modalDialog(
            if(sum(duplicated(DB$meta$`Assembly Name`)) == 1) {
              HTML(paste0("Entry #", which(duplicated(DB$meta$`Assembly Name`)), 
                          " contains a duplicated assembly name:", "<br><br>",
                          DB$meta$`Assembly Name`[which(duplicated(DB$meta$`Assembly Name`))]))
            } else {
              HTML(append("Entries contain duplicated assembly names: <br><br>", 
                          paste0(unique(DB$meta$`Assembly Name`[which(duplicated(DB$meta$`Assembly Name`))]), "<br>")))
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
          
          output$nj_field <- renderUI({
            addSpinner(
              plotOutput("tree_nj", width = paste0(as.character(as.numeric(input$nj_scale) * as.numeric(input$nj_ratio)), "px"), height = paste0(as.character(input$nj_scale), "px")),
              spin = "dots",
              color = "#ffffff"
            )
          })
          
          Vis$meta_nj <- select(DB$meta_true, -2)
          
          if(length(unique(gsub(" ", "_", colnames(Vis$meta_nj)))) < length(gsub(" ", "_", colnames(Vis$meta_nj)))) {
            show_toast(
              title = "Conflicting Custom Variable Names",
              type = "warning",
              position = "top-end",
              width = "500px",
              timer = 6000
            )
          } else {
            
            # Create phylogenetic tree data
            Vis$nj <- ape::nj(hamming_nj())
            
            # Create phylogenetic tree meta data
            Vis$meta_nj <- mutate(Vis$meta_nj, taxa = Index) %>%
              relocate(taxa)
            
            # Get number of included entries calculate start values for tree 
            if(!is.null(input$nj_layout)) {
              if(input$nj_layout == "circular" | input$nj_layout == "inward") {
                if(sum(DB$data$Include) < 21) {
                  Vis$labelsize_nj <- 5.5
                  Vis$tippointsize_nj <- 5.5
                  Vis$nodepointsize_nj <- 4
                  Vis$tiplab_padding_nj <- 0.25
                  Vis$branch_size_nj <- 4.5
                } else if (between(sum(DB$data$Include), 21, 40)) {
                  Vis$labelsize_nj <- 5
                  Vis$tippointsize_nj <- 5
                  Vis$nodepointsize_nj <- 3.5
                  Vis$tiplab_padding_nj <- 0.2
                  Vis$branch_size_nj <- 4
                } else if (between(sum(DB$data$Include), 41, 60)) {
                  Vis$labelsize_nj <- 4.5
                  Vis$tippointsize_nj <- 4.5
                  Vis$nodepointsize_nj <- 3
                  Vis$tiplab_padding_nj <- 0.15
                  Vis$branch_size_nj <- 3.5
                } else if (between(sum(DB$data$Include), 61, 80)) {
                  Vis$labelsize_nj <- 4
                  Vis$tippointsize_nj <- 4
                  Vis$nodepointsize_nj <- 2.5
                  Vis$tiplab_padding_nj <- 0.1
                  Vis$branch_size_nj <- 3
                } else if (between(sum(DB$data$Include), 81, 100)) {
                  Vis$labelsize_nj <- 3.5
                  Vis$tippointsize_nj <- 3.5
                  Vis$nodepointsize_nj <- 2
                  Vis$tiplab_padding_nj <- 0.1
                  Vis$branch_size_nj <- 2.5
                } else {
                  Vis$labelsize_nj <- 3
                  Vis$tippointsize_nj <- 3
                  Vis$nodepointsize_nj <- 1.5
                  Vis$tiplab_padding_nj <- 0.05
                  Vis$branch_size_nj <- 2
                }
              } else {
                if(sum(DB$data$Include) < 21) {
                  Vis$labelsize_nj <- 5
                  Vis$tippointsize_nj <- 5
                  Vis$nodepointsize_nj <- 4
                  Vis$tiplab_padding_nj <- 0.25
                  Vis$branch_size_nj <- 4.5
                } else if (between(sum(DB$data$Include), 21, 40)) {
                  Vis$labelsize_nj <- 4.5
                  Vis$tippointsize_nj <- 4.5
                  Vis$nodepointsize_nj <- 3.5
                  Vis$tiplab_padding_nj <- 0.2
                  Vis$branch_size_nj <- 4
                } else if (between(sum(DB$data$Include), 41, 60)) {
                  Vis$labelsize_nj <- 4
                  Vis$tippointsize_nj <- 4
                  Vis$nodepointsize_nj <- 3
                  Vis$tiplab_padding_nj <- 0.15
                  Vis$branch_size_nj <- 3.5
                } else if (between(sum(DB$data$Include), 61, 80)) {
                  Vis$labelsize_nj <- 3.5
                  Vis$tippointsize_nj <- 3.5
                  Vis$nodepointsize_nj <- 2.5
                  Vis$tiplab_padding_nj <- 0.1
                  Vis$branch_size_nj <- 3
                } else if (between(sum(DB$data$Include), 81, 100)) {
                  Vis$labelsize_nj <- 3
                  Vis$tippointsize_nj <- 3
                  Vis$nodepointsize_nj <- 2
                  Vis$tiplab_padding_nj <- 0.1
                  Vis$branch_size_nj <- 2.5
                } else {
                  Vis$labelsize_nj <- 2.5
                  Vis$tippointsize_nj <- 2.5
                  Vis$nodepointsize_nj <- 1.5
                  Vis$tiplab_padding_nj <- 0.05
                  Vis$branch_size_nj <- 2
                }
              }
            } else {
              Vis$labelsize_nj <- 4
              Vis$tippointsize_nj <- 4
              Vis$nodepointsize_nj <- 2.5
              Vis$tiplab_padding_nj <- 0.2
              Vis$branch_size_nj <- 3.5
            }
            
            Vis$nj_tree <- ggtree(Vis$nj)
            
            # Get upper and lower end of x range
            Vis$nj_max_x <- max(Vis$nj_tree$data$x)
            Vis$nj_min_x <- min(Vis$nj_tree$data$x)
            
            # Get parent node numbers
            Vis$nj_parentnodes <- Vis$nj_tree$data$parent
            
            # Update visualization control inputs
            if(!is.null(input$nj_tiplab_size)) {
              updateNumericInput(session, "nj_tiplab_size", value = Vis$labelsize_nj)
            }
            if(!is.null(input$nj_tippoint_size)) {
              updateSliderInput(session, "nj_tippoint_size", value = Vis$tippointsize_nj)
            }
            if(!is.null(input$nj_nodepoint_size)) {
              updateSliderInput(session, "nj_nodepoint_size", value = Vis$nodepointsize_nj)
            }
            if(!is.null(input$nj_tiplab_padding)) {
              updateSliderInput(session, "nj_tiplab_padding", value = Vis$tiplab_padding_nj)
            }
            if(!is.null(input$nj_branch_size)) {
              updateNumericInput(session, "nj_branch_size", value = Vis$branch_size_nj)
            }
            if(!is.null(input$nj_treescale_width)) {
              updateNumericInput(session, "nj_treescale_width", value = round(ceiling(Vis$nj_max_x) * 0.1, 0))
            }
            if(!is.null(input$nj_rootedge_length)) {
              updateSliderInput(session, "nj_rootedge_length", value = round(ceiling(Vis$nj_max_x) * 0.05, 0))
            }
            
            output$tree_nj <- renderPlot({
              nj_tree()
            })
          }
        } else if (input$tree_algo == "UPGMA") {
          
          output$upgma_field <- renderUI({
            addSpinner(
              plotOutput("tree_upgma", width = paste0(as.character(as.numeric(input$upgma_scale) * as.numeric(input$upgma_ratio)), "px"), height = paste0(as.character(input$upgma_scale), "px")),
              spin = "dots",
              color = "#ffffff"
            )
          })
          
          Vis$meta_upgma <- select(DB$meta_true, -2)
          
          if(length(unique(gsub(" ", "_", colnames(Vis$meta_upgma)))) < length(gsub(" ", "_", colnames(Vis$meta_upgma)))) {
            show_toast(
              title = "Conflicting Custom Variable Names",
              type = "warning",
              position = "top-end",
              width = "500px",
              timer = 6000
            )
          } else {
            
            # Create phylogenetic tree data
            Vis$upgma <- phangorn::upgma(hamming_nj())
            
            # Create phylogenetic tree meta data
            Vis$meta_upgma <- mutate(Vis$meta_upgma, taxa = Index) %>%
              relocate(taxa)
            
            # Get number of included entries calculate start values for tree 
            if(!is.null(input$upgma_layout)) {
              if(input$upgma_layout == "circular" | input$upgma_layout == "inward") {
                if(sum(DB$data$Include) < 21) {
                  Vis$labelsize_upgma <- 5.5
                  Vis$tippointsize_upgma <- 5.5
                  Vis$nodepointsize_upgma <- 4
                  Vis$tiplab_padding_upgma <- 0.25
                  Vis$branch_size_upgma <- 4.5
                } else if (between(sum(DB$data$Include), 21, 40)) {
                  Vis$labelsize_upgma <- 5
                  Vis$tippointsize_upgma <- 5
                  Vis$nodepointsize_upgma <- 3.5
                  Vis$tiplab_padding_upgma <- 0.2
                  Vis$branch_size_upgma <- 4
                } else if (between(sum(DB$data$Include), 41, 60)) {
                  Vis$labelsize_upgma <- 4.5
                  Vis$tippointsize_upgma <- 4.5
                  Vis$nodepointsize_upgma <- 3
                  Vis$tiplab_padding_upgma <- 0.15
                  Vis$branch_size_upgma <- 3.5
                } else if (between(sum(DB$data$Include), 61, 80)) {
                  Vis$labelsize_upgma <- 4
                  Vis$tippointsize_upgma <- 4
                  Vis$nodepointsize_upgma <- 2.5
                  Vis$tiplab_padding_upgma <- 0.1
                  Vis$branch_size_upgma <- 3
                } else if (between(sum(DB$data$Include), 81, 100)) {
                  Vis$labelsize_upgma <- 3.5
                  Vis$tippointsize_upgma <- 3.5
                  Vis$nodepointsize_upgma <- 2
                  Vis$tiplab_padding_upgma <- 0.1
                  Vis$branch_size_upgma <- 2.5
                } else {
                  Vis$labelsize_upgma <- 3
                  Vis$tippointsize_upgma <- 3
                  Vis$nodepointsize_upgma <- 1.5
                  Vis$tiplab_padding_upgma <- 0.05
                  Vis$branch_size_upgma <- 2
                }
              } else {
                if(sum(DB$data$Include) < 21) {
                  Vis$labelsize_upgma <- 5
                  Vis$tippointsize_upgma <- 5
                  Vis$nodepointsize_upgma <- 4
                  Vis$tiplab_padding_upgma <- 0.25
                  Vis$branch_size_upgma <- 4.5
                } else if (between(sum(DB$data$Include), 21, 40)) {
                  Vis$labelsize_upgma <- 4.5
                  Vis$tippointsize_upgma <- 4.5
                  Vis$nodepointsize_upgma <- 3.5
                  Vis$tiplab_padding_upgma <- 0.2
                  Vis$branch_size_upgma <- 4
                } else if (between(sum(DB$data$Include), 41, 60)) {
                  Vis$labelsize_upgma <- 4
                  Vis$tippointsize_upgma <- 4
                  Vis$nodepointsize_upgma <- 3
                  Vis$tiplab_padding_upgma <- 0.15
                  Vis$branch_size_upgma <- 3.5
                } else if (between(sum(DB$data$Include), 61, 80)) {
                  Vis$labelsize_upgma <- 3.5
                  Vis$tippointsize_upgma <- 3.5
                  Vis$nodepointsize_upgma <- 2.5
                  Vis$tiplab_padding_upgma <- 0.1
                  Vis$branch_size_upgma <- 3
                } else if (between(sum(DB$data$Include), 81, 100)) {
                  Vis$labelsize_upgma <- 3
                  Vis$tippointsize_upgma <- 3
                  Vis$nodepointsize_upgma <- 2
                  Vis$tiplab_padding_upgma <- 0.1
                  Vis$branch_size_upgma <- 2.5
                } else {
                  Vis$labelsize_upgma <- 2.5
                  Vis$tippointsize_upgma <- 2.5
                  Vis$nodepointsize_upgma <- 1.5
                  Vis$tiplab_padding_upgma <- 0.05
                  Vis$branch_size_upgma <- 2
                }
              }
            } else {
              Vis$labelsize_upgma <- 4
              Vis$tippointsize_upgma <- 4
              Vis$nodepointsize_upgma <- 2.5
              Vis$tiplab_padding_upgma <- 0.2
              Vis$branch_size_upgma <- 3.5
            }
            
            Vis$upgma_tree <- ggtree(Vis$upgma)
            
            # Get upper and lower end of x range
            Vis$upgma_max_x <- max(Vis$upgma_tree$data$x)
            Vis$upgma_min_x <- min(Vis$upgma_tree$data$x)
            
            # Get parent node numbers
            Vis$upgma_parentnodes <- Vis$upgma_tree$data$parent
            
            # Update visualization control inputs
            if(!is.null(input$upgma_tiplab_size)) {
              updateNumericInput(session, "upgma_tiplab_size", value = Vis$labelsize_upgma)
            }
            if(!is.null(input$upgma_tippoint_size)) {
              updateSliderInput(session, "upgma_tippoint_size", value = Vis$tippointsize_upgma)
            }
            if(!is.null(input$upgma_nodepoint_size)) {
              updateSliderInput(session, "upgma_nodepoint_size", value = Vis$nodepointsize_upgma)
            }
            if(!is.null(input$upgma_tiplab_padding)) {
              updateSliderInput(session, "upgma_tiplab_padding", value = Vis$tiplab_padding_upgma)
            }
            if(!is.null(input$upgma_branch_size)) {
              updateNumericInput(session, "upgma_branch_size", value = Vis$branch_size_upgma)
            }
            if(!is.null(input$upgma_treescale_width)) {
              updateNumericInput(session, "upgma_treescale_width", value = round(ceiling(Vis$upgma_max_x) * 0.1, 0))
            }
            if(!is.null(input$upgma_rootedge_length)) {
              updateSliderInput(session, "upgma_rootedge_length", value = round(ceiling(Vis$upgma_max_x) * 0.05, 0))
            }
            
            output$tree_upgma <- renderPlot({
              upgma_tree()
            })
          }
        } else {
          
          output$mst_field <- renderUI({
            addSpinner(
              visNetworkOutput("tree_mst", width = paste0(as.character(as.numeric(input$mst_scale) * as.numeric(input$mst_ratio)), "px"), height = paste0(as.character(input$mst_scale), "px")),
              spin = "dots",
              color = "#ffffff"
            )
          })
          
          if(nrow(DB$meta_true) > 100) {
            show_toast(
              title = "Computation might take a while",
              type = "warning",
              position = "top-end",
              width = "500px",
              timer = 10000
            )
          }
          
          # prepare igraph object
          Vis$ggraph_1 <- hamming_mst() |>
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
  
  # _______________________ ####
  
  ## Report ----
  
  observe({
    if(!is.null(DB$data)) {
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
  
  observe({
    if(!is.null(DB$data)){
      if(!is.null(input$tree_algo)) {
        Report$report_df <- data.frame(Element = c("entry_table", "general_show",
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
      Report$report_list_mst <- list(entry_table = DB$meta_true[,1:12],
                                     scheme = DB$schemeinfo, 
                                     tree = input$tree_algo,
                                     na_handling = if(anyNA(DB$allelic_profile_true)){input$na_handling} else {NULL},
                                     distance = "Hamming Distances",
                                     version = c(phylotraceVersion, "blat-1.3.23"),
                                     plot = "MST")
    } else if(input$tree_algo == "Neighbour-Joining") {
      Report$report_list_nj <- list(entry_table = DB$meta_true[,1:12],
                                    scheme = DB$schemeinfo, 
                                    tree = input$tree_algo,
                                    na_handling = input$na_handling,
                                    distance = "Hamming Distances",
                                    version = c(phylotraceVersion, "blat-1.3.23"),
                                    plot = "NJ")
    } else {
      Report$report_list_upgma <- list(entry_table = DB$meta_true[,1:12],
                                       scheme = DB$schemeinfo, 
                                       tree = input$tree_algo,
                                       na_handling = input$na_handling,
                                       distance = "Hamming Distances",
                                       version = c(phylotraceVersion, "blat-1.3.23"),
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
        report <- c(Report$report_list_mst, 
                    "general_date" = as.character(input$mst_date_general_select),
                    "operator" = input$mst_operator_general_select,
                    "institute" = input$mst_institute_general_select,
                    "comment" = input$mst_comm_general_select,
                    "report_df" = Report$report_df)
        
        # Save data to an RDS file if any elements were selected
        if (!is.null(report)) {
          saveRDS(report, file = paste0(getwd(), "/Report/selected_elements.rds"))
        }
        
        rmarkdown::render(paste0(getwd(), "/Report/Report.Rmd"))
        
        file.copy(paste0(getwd(), "/Report/Report.html"), file)
        
      } else if(input$tree_algo == "Neighbour-Joining") {
        plot.report()
        report <- c(Report$report_list_nj, 
                    "general_date" = as.character(input$mst_date_general_select),
                    "operator" = input$mst_operator_general_select,
                    "institute" = input$mst_institute_general_select,
                    "comment" = input$mst_comm_general_select,
                    "report_df" = Report$report_df)
        
        # Save data to an RDS file if any elements were selected
        if (!is.null(report)) {
          saveRDS(report, file = paste0(getwd(), "/Report/selected_elements.rds"))
        }
        
        rmarkdown::render(paste0(getwd(), "/Report/Report.Rmd"))
        
        file.copy(paste0(getwd(), "/Report/Report.html"), file)
        
      } else {
        plot.report()
        report <- c(Report$report_list_upgma, 
                    "general_date" = as.character(input$mst_date_general_select),
                    "operator" = input$mst_operator_general_select,
                    "institute" = input$mst_institute_general_select,
                    "comment" = input$mst_comm_general_select,
                    "report_df" = Report$report_df)
        
        # Save data to an RDS file if any elements were selected
        if (!is.null(report)) {
          saveRDS(report, file = paste0(getwd(), "/Report/selected_elements.rds"))
        }
        
        rmarkdown::render(paste0(getwd(), "/Report/Report.Rmd"))
        
        file.copy(paste0(getwd(), "/Report/Report.html"), file)
        
      }
    }
  )
  
  # _______________________ ####
  
  ## Typing  ----
  
  # Render Single/Multi Switch
  
  readLogFile <- reactive({
    invalidateLater(5000, session)
    readLines(paste0(getwd(), "/execute/script_log.txt"))
  })
  
  # Render sidebar dependent on data presence
  # No sidebar
  output$typing_sidebar <- renderUI({
    if(!is.null(DB$exist)) {
      if(DB$exist) {
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
    if(!is.null(DB$exist)) {
      if(DB$exist) {
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
    } else {NULL}
  })
  
  ### Single Typing ----
  
  #### Render UI Elements ----
  
  # Render Typing Results if finished
  observe({
    if(Typing$progress_format_end == 999999) {
      if(file.exists(paste0(getwd(),"/execute/single_typing_log.txt"))) {
        if(str_detect(tail(readLines(paste0(getwd(),"/execute/single_typing_log.txt")), 1), "Successful")) {
          output$typing_result_table <- renderRHandsontable({
            typing_result_table <- readRDS(paste0(getwd(), "/execute/event_df.rds"))
            if(nrow(typing_result_table) > 0) {
              if(nrow(typing_result_table) > 15) {
                rhandsontable(typing_result_table, rowHeaders = NULL, 
                              stretchH = "all", height = 500) %>%
                  hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
                  hot_cols(columnSorting = TRUE) %>%
                  hot_rows(rowHeights = 25) %>%
                  hot_col(1:ncol(typing_result_table), valign = "htMiddle", halign = "htCenter")
              } else {
                rhandsontable(typing_result_table, rowHeaders = NULL, 
                              stretchH = "all") %>%
                  hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
                  hot_cols(columnSorting = TRUE) %>%
                  hot_rows(rowHeights = 25) %>%
                  hot_col(1:ncol(typing_result_table), valign = "htMiddle", halign = "htCenter")
              }
            }
          })
          
          output$single_typing_results <- renderUI({
            result_table <- readRDS(paste0(getwd(), "/execute/event_df.rds"))
            number_events <- nrow(result_table)
            
            n_new <- length(grep("New Variant", result_table$Event))
            
            n_missing <- number_events - n_new
            
            # Show results table only if successful typing 
            if(file.exists(paste0(getwd(),"/execute/single_typing_log.txt"))) {
              if(str_detect(tail(readLines(paste0(getwd(),"/execute/single_typing_log.txt")), 1), "Successful")) {
                if(number_events > 0) {
                  column(
                    width = 12,
                    HTML(paste("<span style='color: white;'>", 
                               length(Typing$scheme_loci_f) - number_events,
                               "loci were assigned a variant from local scheme.")),
                    br(), 
                    HTML(paste("<span style='color: white;'>", 
                               n_missing,
                               if(n_missing == 1) " locus not assigned (<i>NA</i>)." else " loci not assigned (<i>NA</i>).")),
                    br(),
                    HTML(paste("<span style='color: white;'>", 
                               n_new,
                               if(n_new == 1) " locus with new variant."  else " loci with new variants.")),
                    br(), br(),
                    rHandsontableOutput("typing_result_table")
                  )
                } else {
                  column(
                    width = 12,
                    HTML(paste("<span style='color: white;'>", 
                               length(Typing$scheme_loci_f),
                               "successfully assigned from local scheme."))
                  )
                }
              }
            }
          })
          
        } else {
          
          output$single_typing_results <- NULL
          
        }
      } else {
        output$single_typing_results <- NULL
      }
    }
    
  })
  
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
        class = NULL,
        root = path_home()
      ),
      br(),
      br(),
      br(),
      uiOutput("genome_path")
    )
  })
  
  # Render Declare Metadata UI
  
  observe({
    if (nrow(Typing$single_path) < 1) {
      output$genome_path <- renderUI(HTML(
        paste("<span style='color: white;'>", "No file selected.")
      ))
      
      # dont show subsequent metadata declaration and typing start UI
      output$metadata_single_box <- NULL
      output$start_typing_ui <- NULL
      
    } else if (nrow(Typing$single_path) > 0) {
      
      if (str_detect(str_sub(Typing$single_path$name, start = -6), ".fasta") | 
          str_detect(str_sub(Typing$single_path$name, start = -6), ".fna") | 
          str_detect(str_sub(Typing$single_path$name, start = -6), ".fa")) {
        
        # Render selected assembly path
        output$genome_path <- renderUI({
          HTML(
            paste(
              "<span style='color: white; font-weight: bolder'>",
              as.character(Typing$single_path$name)
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
                              width = "80%")
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
                      width = "80%"
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
                    roots = c(home = path_home()),
                    session = session,
                    filetypes = c('', 'fasta', 'fna', 'fa'))
    Typing$single_path <- parseFilePaths(roots = c(home = path_home()), input$genome_file)
    
  })
  
  #### Run blat ----
  
  observeEvent(input$typing_start, {
    
    if(tail(readLogFile(), 1) != "0") {
      show_toast(
        title = "Pending Multi Typing",
        type = "warning",
        position = "top-end",
        timer = 6000,
        width = "500px"
      )
    } else {
      
      if(!is.null(DB$data)) {
        if(sum(apply(DB$data, 1, anyNA)) >= 1) {
          DB$no_na_switch <- TRUE
        } else {
          DB$no_na_switch <- FALSE
        }
      }
      
      Typing$single_end <- FALSE
      
      Typing$progress_format_start <- 0
      Typing$progress_format_end <- 0
      
      # Remove Initiate Typing UI 
      output$initiate_typing_ui <- NULL
      output$metadata_single_box <- NULL
      output$start_typing_ui <- NULL
      
      # Locate folder containing cgMLST scheme
      search_string <- paste0(gsub(" ", "_", DB$scheme), "_alleles")
      
      scheme_folders <- dir_ls(paste0(DB$database, "/", gsub(" ", "_", DB$scheme)))
      
      if (any(grepl(search_string, scheme_folders))) {
        
        # reset results file 
        if(dir_exists(paste0(getwd(), "/execute/blat_single/results"))) {
          unlink(list.files(paste0(getwd(), "/execute/blat_single/results"), full.names = TRUE), recursive = TRUE)
        }
        
        # blat initiate index
        scheme_select <- as.character(scheme_folders[which(grepl(search_string, scheme_folders))])
        
        show_toast(
          title = "Typing Initiated",
          type = "success",
          position = "top-end",
          timer = 12000,
          width = "500px"
        )
        
        ### Run blat Typing
        
        single_typing_df <- data.frame(
          db_path = DB$database,
          wd = getwd(),
          scheme = paste0(gsub(" ", "_", DB$scheme)),
          genome = Typing$single_path$datapath,
          alleles = paste0(DB$database, "/", gsub(" ", "_", DB$scheme), "/", search_string)
        )
        
        saveRDS(single_typing_df, "execute/single_typing_df.rds")
        
        # Execute single typing script
        system(paste("chmod +x", paste0(getwd(), "/execute/blat_run.sh")))
        system(paste0(getwd(), "/execute/blat_run.sh"), wait = FALSE)
        
        scheme_loci <- list.files(path = scheme_select, full.names = TRUE)
        
        # Filter the files that have FASTA extensions
        Typing$scheme_loci_f <-
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
                fluidRow(
                  column(
                    width = 12,
                    uiOutput("reset_single_typing"),
                    HTML(
                      paste(
                        "<span style='color: white; font-weight: bolder'>",
                        as.character(Typing$single_path$name)
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
                  column(
                    width = 12,
                    uiOutput("typing_formatting"),
                    uiOutput("typing_fin")
                  )
                )
              ),
              column(1),
              column(
                width = 3,
                br(), br(), br(),
                uiOutput("single_typing_results")
              )
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
    
    # write progress in process tracker
    cat(
      c(length(list.files(paste0(getwd(), "/execute/blat_single/results"))),
        readLines(paste0(getwd(), "/execute/progress.txt"))[-1]), 
      file = paste0(getwd(), "/execute/progress.txt"),
      sep = "\n"
      )
    
    progress <- readLines(paste0(getwd(), "/execute/progress.txt"))
    
    # if typing with blat is finished -> "attaching" phase started
    if(!is.na(progress[1])) {
      if(!is.na(progress[2])) {
        if(progress[2] == "888888") {
          Typing$progress_format_start <- progress[2]
          Typing$pending_format <- progress[2]
          Typing$status <- "Attaching"
        }
      }
      # "attaching" phase completed
      if(!is.na(progress[3])) {
        if(progress[3] == "999999") {
          Typing$progress_format_end <- progress[3]
          Typing$entry_added <- progress[3]
          Typing$status <- "Finalized"
        }
      }
      Typing$progress <- as.numeric(progress[1])
      floor((Typing$progress / length(Typing$scheme_loci_f)) * 100)
    } else {
      floor((Typing$progress / length(Typing$scheme_loci_f)) * 100)
    }
  })
  
  # Observe Typing Progress
  observe({
    
    if(readLogFile()[1] == "0") {
      # Update Progress Bar
      updateProgressBar(
        session = session,
        id = "progress_bar",
        value = update(),
        total = 100,
        title = paste0(as.character(Typing$progress), "/", length(Typing$scheme_loci_f), " loci screened")
      )
    }
    
    if (Typing$progress_format_start == 888888) {
      output$typing_formatting <- renderUI({
        column(
          width = 12,
          align = "center",
          br(),
          fluidRow(
            column(
              width = 6,
              HTML(paste("<span style='color: white;'>", "Transforming data ..."))
            ),
            column(
              width = 3,
              align = "left",
              HTML(paste('<i class="fa fa-spinner fa-spin" style="font-size:20px;color:white"></i>'))
            )
          )
        )
      })
    } else {
      output$typing_formatting <- NULL
    }
    
    # Render when finalized  
    if (Typing$progress_format_end == 999999) {
      
      output$typing_formatting <- NULL
      
      output$typing_fin <- renderUI({
        fluidRow(
          column(
            width = 12,
            align = "center",
            br(), br(),
            if(file.exists(paste0(getwd(),"/execute/single_typing_log.txt"))) {
              if(str_detect(tail(readLines(paste0(getwd(),"/execute/single_typing_log.txt")), 1), "Successful")) {
                HTML(paste("<span style='color: white;'>", 
                           sub(".*Successful", "Successful", tail(readLines(paste0(getwd(),"/execute/single_typing_log.txt")), 1)),
                           "Reset to start another typing process.", sep = '<br/>'))
              } else {
                HTML(paste("<span style='color: white;'>", 
                           sub(".*typing", "Typing", tail(readLines(paste0(getwd(),"/execute/single_typing_log.txt")), 1)),
                           "Reset to start another typing process.", sep = '<br/>'))
              }
            },
            br(), br(),
            actionButton(
              "reset_single_typing",
              "Reset",
              icon = icon("arrows-rotate")
            )
          )
        )
      })
    } else {
      output$typing_fin <- NULL
      output$single_typing_results <- NULL
    }
    
  })
  
  #### Declare Metadata  ----
  
  observeEvent(input$conf_meta_single, {
    
    meta_info <- data.frame(assembly_id = trimws(input$assembly_id),
                            assembly_name = trimws(input$assembly_name),
                            cgmlst_typing = DB$scheme,
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
            DB$scheme,
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
    
    Typing$progress <- 0
    
    Typing$progress_format <- 900000
    
    output$single_typing_progress <- NULL
    
    output$typing_fin <- NULL
    
    output$single_typing_results <- NULL
    
    output$typing_formatting <- NULL
    
    Typing$single_path <- data.frame()
    
    # reset results file 
    if(dir_exists(paste0(getwd(), "/execute/blat_single/results"))) {
      unlink(list.files(paste0(getwd(), "/execute/blat_single/results"), full.names = TRUE), recursive = TRUE)
      # Resetting single typing progress logfile bar 
      con <- file(paste0(getwd(), "/execute/progress.txt"), open = "w")
      
      cat("0\n", file = con)   
      
      close(con)
    }
    
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
          class = NULL,
          root = path_home()
        ),
        br(),
        br(),
        br(),
        uiOutput("genome_path")
      )
    })
  })
  
  # Notification for finalized Single typing
  Typing$single_end <- TRUE
  Typing$progress_format_end <- 0
  
  observe({
    if(Typing$single_end == FALSE) {
      if (Typing$progress_format_end == 999999) {
        show_toast(
          title = "Single Typing finalized",
          type = "success",
          position = "top-end",
          timer = 8000,
          width = "500px"
        )
        Typing$single_end <- TRUE
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
          buttonType = "default",
          root = path_home()
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
    if (nrow(Typing$table) > 0) {
      
      Typing$genome_selected <- hot_to_r(input$multi_select_table)
      
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
                            width = "80%")
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
                    width = "80%"
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
                   roots = c(home = path_home()),
                   session = session,
                   filetypes = c('', 'fasta', 'fna', 'fa'))
    
    Typing$table <-
      data.frame(Include = rep(TRUE, length(list.files(
        as.character(parseDirPath(
          roots = c(home = path_home()), input$genome_file_multi
        ))
      ))),
      Files = list.files(as.character(
        parseDirPath(roots = c(home = path_home()), input$genome_file_multi)
      )))
    
    if (between(nrow(Typing$table), 1, 15)) {
      output$multi_select_table <- renderRHandsontable({
        rhandsontable(Typing$table, rowHeaders = NULL, stretchH = "all") %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          hot_cols(columnSorting = TRUE) %>%
          hot_rows(rowHeights = 25) %>%
          hot_col(2,
                  readOnly = TRUE,
                  valign = "htBottom") %>%
          hot_col(1,
                  halign = "htCenter",
                  valign = "htTop", 
                  colWidths = 60)
      })
    } else if(nrow(Typing$table) > 15) {
      output$multi_select_table <- renderRHandsontable({
        rhandsontable(Typing$table, rowHeaders = NULL, stretchH = "all", height = 500) %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          hot_cols(columnSorting = TRUE) %>%
          hot_rows(rowHeights = 25) %>%
          hot_col(2,
                  readOnly = TRUE,
                  valign = "htBottom") %>%
          hot_col(1,
                  halign = "htCenter",
                  valign = "htTop", 
                  colWidths = 60)
      })
    } else {
      output$multi_select_table <- NULL
    }
    
  })
  
  
  observeEvent(input$conf_meta_multi, {
    
    meta_info <- data.frame(cgmlst_typing = DB$scheme,
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
            DB$scheme,
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
      
      # Reset multi typing result list
      saveRDS(list(), paste0(getwd(), "/execute/event_list.rds"))
      multi_help <- FALSE
      Typing$result_list <- NULL
      
      # Null logfile
      writeLines("0", paste0(getwd(), "/execute/script_log.txt"))
      
      # Reset User Feedback variable
      Typing$pending_format <- 0
      Typing$multi_started <- FALSE
      
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
              buttonType = "default",
              root = path_home()
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
      
      Typing$table <- data.frame()
      
      output$test_yes_pending <- NULL
      output$multi_typing_results <- NULL
    }
  })
  
  # Confirm Reset after 
  observeEvent(input$conf_multi_kill, {
    removeModal()
    
    # Kill multi typing and reset logfile  
    system(paste("chmod +x", paste0(getwd(), "/execute/kill_multi.sh")))
    system(paste0(getwd(), "/execute/kill_multi.sh"), wait = TRUE)
    
    show_toast(
      title = "Execution cancelled",
      type = "warning",
      position = "top-end",
      timer = 6000,
      width = "500px"
    )
    
    # Kill multi typing and reset logfile  
    writeLines("0", paste0(getwd(), "/execute/script_log.txt"))
    
    #Reset multi typing result list
    saveRDS(list(), paste0(getwd(), "/execute/event_list.rds"))
    multi_help <- FALSE
    Typing$result_list <- NULL
    
    # Reset User Feedback variable
    Typing$pending_format <- 0
    output$test_yes_pending <- NULL
    output$multi_typing_results <- NULL
    Typing$failures <- 0
    Typing$successes <- 0
    Typing$multi_started <- FALSE
    
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
            buttonType = "default",
            root = path_home()
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
    if(readLines(paste0(getwd(), "/execute", "/progress.txt"))[1] != "0") {
      show_toast(
        title = "Pending Single Typing",
        type = "warning",
        position = "top-end",
        timer = 6000,
        width = "500px"
      )
    } else {
      if (any(!grepl("\\.fasta|\\.fna|\\.fa", str_sub(Typing$genome_selected$Files[which(Typing$genome_selected$Include == TRUE)], start = -6)))) {
        
        show_toast(
          title = "Wrong file type (include only fasta/fna/fa)",
          type = "error",
          position = "top-end",
          timer = 6000,
          width = "500px"
        )
      } else {
        
        removeModal()
        
        show_toast(
          title = "Multi Typing started",
          type = "success",
          position = "top-end",
          timer = 10000,
          width = "500px"
        )
        
        # Remove Allelic Typing Controls
        output$initiate_multi_typing_ui <- NULL
        
        output$metadata_multi_box <- NULL
        
        output$start_multi_typing_ui <- NULL
        
        # Initiate Feedback variables
        Typing$multi_started <- TRUE
        Typing$pending <- TRUE
        Typing$failures <- 0
        Typing$successes <- 0
        
        # Start Multi Typing Script
        multi_typing_df <- data.frame(
          db_path = DB$database,
          wd = getwd(),
          scheme = paste0(gsub(" ", "_", DB$scheme)),
          genome_folder = as.character(parseDirPath(roots = c(home = path_home()), input$genome_file_multi)),
          genome_names = paste(Typing$genome_selected$Files[which(Typing$genome_selected$Include == TRUE)], collapse= " "),
          alleles = paste0(DB$database, "/", gsub(" ", "_", DB$scheme), "/", gsub(" ", "_", DB$scheme), "_alleles")
        )
        
        saveRDS(multi_typing_df, "execute/multi_typing_df.rds")
        
        # Execute multi blat script  
        system(paste("chmod +x", paste0(getwd(), "/execute/blat_multi.sh")))
        system(paste0(getwd(), "/execute/blat_multi.sh"), wait = FALSE)
      }
    }
    
  })
  
  
  #### User Feedback ----
  
  observe({
    if(file.exists(paste0(getwd(), "/execute/script_log.txt"))) {
      if(Typing$multi_started == TRUE) {
        check_multi_status()
      } else {
        Typing$status <- "Inactive"
      }
    }
  })
  
  check_multi_status <- reactive({
    
    invalidateLater(3000, session)
    
    log <- readLines(paste0(getwd(), "/execute/script_log.txt"))
    
    # Determine if Single or Multi Typing
    if(str_detect(log[1], "Multi")) {
      Typing$pending_mode <- "Multi"
    } else {
      Typing$pending_mode <- "Single"
    }
    
    # Check typing status
    if(str_detect(tail(log, 1), "Attaching")) {
      Typing$status <- "Attaching"
    } else if(str_detect(tail(log, 1), "Successful")) {
      Typing$multi_help <- TRUE
      Typing$status <- "Successful"
      show_toast(
        title = paste0("Successful", sub(".*Successful", "", tail(log, 1))),
        type = "success",
        width = "500px",
        position = "top-end",
        timer = 8000
      )
    } else if(str_detect(tail(log, 1), "failed")) {
      Typing$status <- "Failed"
      show_toast(
        title = paste0("Failed typing of ", sub(".*failed for ", "", tail(log, 1))),
        type = "error",
        width = "500px",
        position = "top-end",
        timer = 8000
      )
    } else if(str_detect(tail(log, 1), "Processing")) {
      Typing$status <- "Processing"
      
      if(any(str_detect(tail(log, 2), "Successful"))) {
        
        if(!identical(Typing$last_success, tail(log, 2)[1])) {
          Typing$multi_help <- TRUE
          show_toast(
            title = paste0("Successful", sub(".*Successful", "", tail(log, 2)[1])),
            type = "success",
            width = "500px",
            position = "top-end",
            timer = 8000
          )
          
          Typing$last_success <- tail(log, 2)[1]
        }
      } else if(any(str_detect(tail(log, 2), "failed for"))) {
        
        if(!identical(Typing$last_failure, tail(log, 2)[1])) {
          show_toast(
            title = paste0("Failed typing of ", sub(".*failed for ", "", tail(log, 2)[1])),
            type = "error",
            width = "500px",
            position = "top-end",
            timer = 8000
          )
          
          Typing$last_failure <- tail(log, 2)[1]
        }
      }
    } else if(str_detect(tail(log, 1), "finalized")) {
      Typing$multi_help <- TRUE
      Typing$status <- "Finalized"
      
      if(Typing$pending == TRUE) {
        show_toast(
          title = "Typing finalized",
          type = "success",
          width = "500px",
          position = "top-end",
          timer = 8000
        )
        
        Typing$pending <- FALSE
      }
    }
  })
  
  ##### Render Multi Typing UI Feedback ----
  
  observe({
    if(!is.null(input$multi_results_picker)) {
      Typing$multi_table_length <- nrow(Typing$result_list[[input$multi_results_picker]])
    } else {
      Typing$multi_table_length <- NULL
    }
  })
  
  observe({
    if(!is.null(Typing$result_list)) {
      if(length(Typing$result_list) > 0) {
        if(is.null(Typing$multi_table_length)) {
          output$multi_typing_result_table <- renderRHandsontable({
            rhandsontable(Typing$result_list[[input$multi_results_picker]], rowHeaders = NULL, 
                          stretchH = "all") %>%
              hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
              hot_cols(columnSorting = TRUE) %>%
              hot_rows(rowHeights = 25) %>%
              hot_col(1:3, valign = "htMiddle", halign = "htCenter")})
          
        } else {
          if(Typing$multi_table_length > 15) {
            output$multi_typing_result_table <- renderRHandsontable({
              rhandsontable(Typing$result_list[[input$multi_results_picker]], rowHeaders = NULL, 
                            stretchH = "all", height = 500) %>%
                hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
                hot_cols(columnSorting = TRUE) %>%
                hot_rows(rowHeights = 25) %>%
                hot_col(1:3, valign = "htMiddle", halign = "htCenter")})
          } else {
            output$multi_typing_result_table <- renderRHandsontable({
              rhandsontable(Typing$result_list[[input$multi_results_picker]], rowHeaders = NULL, 
                            stretchH = "all") %>%
                hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
                hot_cols(columnSorting = TRUE) %>%
                hot_rows(rowHeights = 25) %>%
                hot_col(1:3, valign = "htMiddle", halign = "htCenter")})
            
          }
        }
      } else {
        output$multi_typing_result_table <- NULL
      }
    } else {
      output$multi_typing_result_table <- NULL
      }
  })
  
  observe({
    if(!is.null(Typing$multi_result_status)) {
      if(Typing$multi_result_status == "start" | Typing$multi_result_status == "finalized"){
        
        if(Typing$multi_help == TRUE) {
          Typing$result_list <- readRDS(paste0(getwd(), "/execute/event_list.rds"))
          Typing$multi_help <- FALSE
        }
      } 
    }
  })
  
  
  observe({
    #Render multi typing result feedback table
    
      if(!is.null(Typing$result_list)) {
        if(length(Typing$result_list) > 0) {
          output$multi_typing_results <- renderUI({
            column(
              width = 12,
              fluidRow(
                column(1),
                column(
                  width = 8,
                  br(), br(),
                  br(), br(),
                  br(), br(),
                  selectInput(
                    "multi_results_picker",
                    label = h5("Select Typing Results", style = "color:white"),
                    choices = names(Typing$result_list),
                    selected = names(Typing$result_list)[length(names(Typing$result_list))],
                  ),
                  br(),
                  rHandsontableOutput("multi_typing_result_table")
                )
              )
            )
          })
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
      
      Typing$multi_result_status <- "start"
      
      output$initiate_multi_typing_ui <- NULL
      
      output$test_yes_pending <- renderUI({
        fluidRow(
          fluidRow(
            br(), br(),
            column(width = 2),
            column(
              width = 4,
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
            column(width = 2),
            column(
              width = 10,
              verbatimTextOutput("logText")
            )  
          )
        )
      })
    } else if(grepl("Multi Typing finalized", tail(readLogFile(), n = 1))) {
      
      Typing$multi_result_status <- "finalized"
      
      Typing$last_scheme <- NULL
      
      output$initiate_multi_typing_ui <- NULL
      
      output$test_yes_pending <- renderUI({
        
        fluidRow(
          fluidRow(
            br(), br(),
            column(width = 2),
            column(
              width = 4,
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
            column(width = 2),
            column(
              width = 10,
              verbatimTextOutput("logTextFull"),
            )
          )
        )
      })
    } else if (!grepl("Start Multi Typing", head(readLogFile(), n = 1))){
      output$test_yes_pending <- NULL
      Typing$multi_result_status <- "idle"
    }
  })
  
} # end server

# _______________________ ####

# Shiny ----

shinyApp(ui = ui, server = server)
