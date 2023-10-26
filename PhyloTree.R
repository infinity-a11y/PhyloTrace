# PhyloTree 
# Version 1.0.0
# Phylogenetic Visualization in a Shiny App
# Author: Marian Freisleben
# Date: 18.09.2023

if (!require(shiny)) install.packages('shiny')
library(shiny)

if (!require(BiocManager)) install.packages('BiocManager')
library(BiocManager)

if (!require(shinyWidgets)) install.packages('shinyWidgets')
library(shinyWidgets)

if (!require(shinydashboard)) install.packages('shinydashboard')
library(shinydashboard)

if (!require(dashboardthemes)) install.packages('dashboardthemes')
library(dashboardthemes)

if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

if (!require(ggplotify)) install.packages('ggplotify')
library(ggplotify)

if (!require(ape)) install.packages('ape')
library(ape)

if (!require(treeio)) install('treeio')
library(treeio)

if (!require(ggtree)) install('ggtree')
library(ggtree)

if (!require(ggtreeExtra)) install('ggtreeExtra')
library(ggtreeExtra)

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(rlang)) install.packages('rlang')
library(rlang)

if (!require(tidytree)) install.packages('tidytree')
library(tidytree)

if (!require(shinyFiles)) install.packages('shinyFiles')
library(shinyFiles)

if (!require(dplyr)) install.packages('dplyr')
library(dplyr)

if (!require(downloader)) install.packages('downloader')
library(downloader)

if (!require(rvest)) install.packages('rvest')
library(rvest)

if (!require(rmarkdown)) install.packages('rmarkdown')
library(rmarkdown)

if (!require(knitr)) install.packages('knitr')
library(knitr)

if (!require(kableExtra)) install.packages('kableExtra')
library(kableExtra)

if (!require(fs)) install.packages('fs')
library(fs)

if (!require(data.table)) install.packages('data.table')
library(data.table)

if (!require(zoo)) install.packages('zoo')
library(zoo)


country_names <- c(
  "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", "Argentina", "Armenia", 
  "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", 
  "Belgium", "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", 
  "Brunei", "Bulgaria", "Burkina Faso", "Burundi", "Côte d'Ivoire", "Cabo Verde", "Cambodia", "Cameroon", 
  "Canada", "Central African Republic", "Chad", "Chile", "China", "Colombia", "Comoros", "Congo (Congo-Brazzaville)", 
  "Costa Rica", "Croatia", "Cuba", "Cyprus", "Czechia (Czech Republic)", "Democratic Republic of the Congo (Congo-Kinshasa)", 
  "Denmark", "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", 
  "Eritrea", "Estonia", 'Eswatini (fmr. "Swaziland")', "Ethiopia", "Fiji", "Finland", "France", "Gabon", 
  "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", 
  "Guyana", "Haiti", "Holy See", "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", 
  "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Kuwait", 
  "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", 
  "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", 
  "Mauritania", "Mauritius", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", 
  "Morocco", "Mozambique", "Myanmar (formerly Burma)", "Namibia", "Nauru", "Nepal", "Netherlands", 
  "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Korea", "North Macedonia (formerly Macedonia)", 
  "Norway", "Oman", "Pakistan", "Palau", "Palestine State", "Panama", "Papua New Guinea", "Paraguay", 
  "Peru", "Philippines", "Poland", "Portugal", "Qatar", "Romania", "Russia", "Rwanda", "Saint Kitts and Nevis", 
  "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe", 
  "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", 
  "Solomon Islands", "Somalia", "South Africa", "South Korea", "South Sudan", "Spain", "Sri Lanka", 
  "Sudan", "Suriname", "Sweden", "Switzerland", "Syria", "Tajikistan", "Tanzania", "Thailand", "Timor-Leste", 
  "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", 
  "United Arab Emirates", "United Kingdom", "United States of America", "Uruguay", "Uzbekistan", "Vanuatu", 
  "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe"
)

sel_countries <- c("Austria", "Germany", "Switzerland", "United Kingdom", "United States of America")

################ User Interface ################

ui <- dashboardPage(
  
  # Title 
  dashboardHeader(title = span(img(src="PhyloTree.jpg", width = 190))),
  
  # Sidebar ----   
  dashboardSidebar(
    tags$style("label{color: white;}"),
    tags$style("file.select{background-color: white;}"),
    br(), br(),
    sidebarMenu(
      id = "tabs",
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
      ),
      hr(), br(),  
      conditionalPanel(
        "input.tabs==='database'",
        column(
          width = 12,
          align = "left",
          uiOutput("scheme_db"),
          br(),
          actionButton(
            "load",
            "Load Database"
          )
          )
      ),
      conditionalPanel(
        "input.tabs==='typing'",
        column(
          width = 12,
          align = "left",
          uiOutput("cgmlst_typing")
        )
      ),
      conditionalPanel(
        "input.tabs==='report'",
        column(
          width = 12,
          align = "left",
          h3(p("Save Report"), style = "color:white"),
          br(),
          textInput(
            inputId = "report_dir",
            label = "Select Directory",
            placeholder = paste0(getwd())
          ),
          br(),
          textInput(
            inputId = "rep_name",
            label = "Name",
            placeholder = paste0("Report_", Sys.Date())
          ),
          br(),
          awesomeRadio(
            inputId = "report_filetype",
            label = "Select Output Type",
            choices = c("PDF", "HTML")
          ),
          br(),
          actionButton(
            inputId = "save_report",
            label = "Download",
            icon = icon("download"),
            width = "auto")
        )
      ),
      conditionalPanel(
        "input.tabs==='visualization'",
        column(
          width = 12,
          awesomeRadio(
            inputId = "generate_tree",
            label = "Source",
            choices = c("Random", "Local")
          ),
          conditionalPanel(
            "input.generate_tree=='Random'",
            numericInput(
              "ntree",
              label = h5("# branches", style = "color:white"), 
              value = 30,
              max = 500,
              width = "110px"
            ),
            br(),
            actionButton(
              "random_tree",
              "Create Tree"
            )
          ),
          conditionalPanel(
            "input.generate_tree=='Local'",
            br(),
            uiOutput("scheme_vis"),
            br(),
            selectInput(
              "tree_algo",
              label = "Tree-building Algorithm",
              choices = c("Neighbour-Joining", "Minimum-Spanning"),
              selected = "Neighbour-Joining"
            ),
            br(),
            actionButton(
              "create_tree",
              "Create Tree"
            )
          )
        )
      )
    )
  ),
  
  dashboardBody(
    tags$style(HTML('
                .shiny-input-container input[type="text"] {
                border-radius: 5px; 
                } 
      
                .box.box-solid.box-primary>.box-header {
                background:#282F38
                }

                .box.box-solid.box-primary{
                background:#282F38
                }

                ')
               ),
    shinyDashboardThemeDIY(
      ### general
      appFontFamily = "Tahoma"
      ,appFontColor = "#000000"
      ,primaryFontColor = "#ffffff"
      ,infoFontColor = "rgb(0,0,0)"
      ,successFontColor = "rgb(0,0,0)"
      ,warningFontColor = "rgb(0,0,0)"
      ,dangerFontColor = "rgb(0,0,0)"
      ,bodyBackColor = cssGradientThreeColors(
        direction = "down"
        ,colorStart = "#282f38"
        ,colorMiddle = "#384454"
        ,colorEnd = "#495d78"
        ,colorStartPos = 0
        ,colorMiddlePos = 50
        ,colorEndPos = 100
      )
      
      ### header
      ,logoBackColor = "#282f38"
      
      ,headerButtonBackColor = "#282f38"
      ,headerButtonIconColor = "#18ece1"
      ,headerButtonBackColorHover = "#282f38"
      ,headerButtonIconColorHover = "#ffffff"
      
      ,headerBackColor = "#282f38"
      ,headerBoxShadowColor = "#aaaaaa"
      ,headerBoxShadowSize = "0px 0px 0px"
      
      ### sidebar
      ,sidebarBackColor = cssGradientThreeColors(
        direction = "down"
        ,colorStart = "#282f38"
        ,colorMiddle = "#384454"
        ,colorEnd = "#495d78"
        ,colorStartPos = 0
        ,colorMiddlePos = 50
        ,colorEndPos = 100
      )
      ,sidebarPadding = 0
      
      ,sidebarMenuBackColor = "transparent"
      ,sidebarMenuPadding = 0
      ,sidebarMenuBorderRadius = 0
      
      ,sidebarShadowRadius = "5px 5px 5px"
      ,sidebarShadowColor = "#282f38"
      
      ,sidebarUserTextColor = "#ffffff"
      
      ,sidebarSearchBackColor = "rgb(55,72,80)"
      ,sidebarSearchIconColor = "rgb(153,153,153)"
      ,sidebarSearchBorderColor = "rgb(55,72,80)"
      
      ,sidebarTabTextColor = "rgb(255,255,255)"
      ,sidebarTabTextSize = 15
      ,sidebarTabBorderStyle = "none none solid none"
      ,sidebarTabBorderColor = "rgb(35,106,135)"
      ,sidebarTabBorderWidth = 0
      
      ,sidebarTabBackColorSelected = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgba(44,222,235,1)"
        ,colorMiddle = "rgba(44,222,235,1)"
        ,colorEnd = "rgba(0,255,213,1)"
        ,colorStartPos = 0
        ,colorMiddlePos = 30
        ,colorEndPos = 100
      )
      ,sidebarTabTextColorSelected = "rgb(0,0,0)"
      ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
      
      ,sidebarTabBackColorHover = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgba(44,222,235,1)"
        ,colorMiddle = "rgba(44,222,235,1)"
        ,colorEnd = "rgba(0,255,213,1)"
        ,colorStartPos = 0
        ,colorMiddlePos = 30
        ,colorEndPos = 100
      )
      ,sidebarTabTextColorHover = "rgb(50,50,50)"
      ,sidebarTabBorderStyleHover = "none none solid none"
      ,sidebarTabBorderColorHover = "rgb(75,126,151)"
      ,sidebarTabBorderWidthHover = 1
      ,sidebarTabRadiusHover = "0px 20px 20px 0px"
      
      ### boxes
      ,boxBackColor = "#ffffff"
      ,boxBorderRadius = 7
      ,boxShadowSize = "0px 0px 0px"
      ,boxShadowColor = "#ffffff"
      ,boxTitleSize = 20
      ,boxDefaultColor = "#00a65a"
      ,boxPrimaryColor = "#ffffff"
      ,boxInfoColor = "#00a65a"
      ,boxSuccessColor = "#00a65a"
      ,boxWarningColor = "#ffffff"
      ,boxDangerColor = "#ffffff"
      
      ,tabBoxTabColor = "#ffffff"
      ,tabBoxTabTextSize = 14
      ,tabBoxTabTextColor = "rgb(0,0,0)"
      ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
      ,tabBoxBackColor = "#ffffff"
      ,tabBoxHighlightColor = "#ffffff"
      ,tabBoxBorderRadius = 5
      
      ### inputs
      ,buttonBackColor = "#282F38"
      ,buttonTextColor = "#ffffff"
      ,buttonBorderColor = "#282F38"
      ,buttonBorderRadius = 5
      
      ,buttonBackColorHover = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgba(44,222,235,1)"
        ,colorMiddle = "rgba(44,222,235,1)"
        ,colorEnd = "rgba(0,255,213,1)"
        ,colorStartPos = 0
        ,colorMiddlePos = 30
        ,colorEndPos = 100
      )
      ,buttonTextColorHover = "#000000"
      ,buttonBorderColorHover = "transparent"
      
      ,textboxBackColor = "#ffffff"
      ,textboxBorderColor = "transparent"
      ,textboxBorderRadius = 10
      ,textboxBackColorSelect = "#ffffff"
      ,textboxBorderColorSelect = "#000000"
      
      ### tables
      ,tableBackColor = "rgb(255,255,255)"
      ,tableBorderColor = "rgb(240,240,240)"
      ,tableBorderTopSize = 1
      ,tableBorderRowSize = 1
    ),
    
    
    tabItems(
      
      
      # Tab Database        ---------------------------------------------
      
      tabItem(
        tabName = "database",
        fluidRow(
          column(
            width = 3,
            align = "center",
            h2(p("Browse Local Database"), style = "color:white"),
          )
        ),
        hr(), br(), 
        fluidRow(
          column(
            width = 4,
            align = "center",
            h3(p("cgMLST Scheme"), style = "color:white"),
            br(),
            uiOutput("no_db")
          ),
          column(
            width = 8,
            align = "center",
            br(),
            uiOutput("browse_what")
            )
        ),
        fluidRow(
          column(
            width = 4,
            uiOutput("scheme_info")
          ),
          column(
            width = 8,
            align = "center",
            conditionalPanel(
              "input.browse_what=='Loci'",
              addSpinner(
                dataTableOutput("db_loci"),
                spin = "dots", 
                color = "#ffffff")
            ),
            conditionalPanel(
              "input.browse_what=='Entries'",
              uiOutput("db_no_entries"),
              dataTableOutput("db_entries"),
              fluidRow(
                column(
                  width = 3,
                  align = "left",
                  uiOutput("edit_entries")
                )
              ),
              fluidRow(
                column(width = 1,
                       uiOutput("edit_index")),
                column(width = 2,
                       uiOutput("edit_scheme_d")),
                column(width = 2,
                       uiOutput("edit_isolation_t")),
                column(
                  width = 1,
                  br(),
                  actionButton(
                    "edit",
                    label = "Edit"
                  )
                )
              )
            )
          )
        )
      ),
      
      # Tab Initialization        ---------------------------------------------      
      
      tabItem(
        tabName = "init",
        fluidRow(
          column(
            width = 3,
            align = "center",
            h2(p("Select cgMLST Scheme"), style = "color:white"),
          )
        ),
        hr(),
        fluidRow(
          column(
            width = 3,
            align = "center",
            br(), br(), br(),  
            selectInput(
              inputId = "select_cgmlst", 
              label = NULL,
              choices = list("Acinetobacter baumanii", "Bacillus anthracis",
                             "Bordetella pertussis", "Brucella melitensis",
                             "Brucella spp.", "Burkholderia mallei (FLI)",
                             "Burkholderia mallei (RKI)", "Burkholderia pseudomallei",
                             "Campylobacter jejuni/coli", "Clostridioides difficile",
                             "Clostridium perfringens", "Corynebacterium diphtheriae",
                             "Cronobacter sakazakii/malonaticus", "Enterococcus faecalis",
                             "Enterococcus faecium", "Escherichia coli",
                             "Francisella tularensis", "Klebsiella pneumoniae/variicola/quasipneumoniae",
                             "Legionella pneumophila", "Listeria monocytogenes",
                             "Mycobacterium tuberculosis/bovis/africanum/canettii",
                             "Mycobacteroides abscessus", "Mycoplasma gallisepticum",
                             "Paenibacillus larvae", "Pseudomonas aeruginosa",
                             "Salmonella enterica", "Serratia marcescens",
                             "Staphylococcus aureus", "Staphylococcus capitis",
                             "Streptococcus pyogenes"),
              selected = "Bordetella pertussis",
              width = "300px")
          ),
          column(
            width = 2,
            align = "center",
            br(), br(), br(), 
            actionButton(inputId = "download_cgMLST", label = "Download")
          ),
          column(
            width = 1
          ),
          column(
            width = 5,
            br(), br(), br(), 
            align = "center",
            h4(p("Downloaded Loci"), style = "color:white"),
          )
        ),
        fluidRow(
          column(
            width = 5,
            align = "center",
            br(), br(), 
            tableOutput("cgmlst_scheme")
          ),
          column(
            width = 1
          ),
          column(
            width = 5,
            align = "right",
            br(), br(),
            conditionalPanel(
              "input.download_cgMLST >= 1",
              addSpinner(
                dataTableOutput("cgmlst_targets"),
                spin = "dots", 
                color = "#ffffff")
            )
          )
        )
      ),
      
      
      
      # Tab Allelic Typing ----------------------------------------------
      
      
      tabItem(
        tabName = "typing",
        fluidRow(
          column(
            width = 3,
            align = "center",
            h2(p("Generate Allelic Profile"), style = "color:white"),
          )
        ),
        hr(),
        fluidRow(
          column(
            width = 3,
            align = "center",
            br(), br(),
            h3(p("Initiate Typing"), style = "color:white"),
            br(), br(),
            shinyFilesButton("genome_file", "Select Genome" ,
                             title = "Please select the genome in .fasta format:", multiple = FALSE,
                             buttonType = "default", class = NULL),
            br(), br(),
            uiOutput("genome_path"),
            uiOutput("selected_scheme"),
            br(), br(), br(),
            uiOutput("arrow_start"),
            br(),
            uiOutput("typing_start"),
            conditionalPanel(
              "input.typing_start",
              br(),
              progressBar(
                "progress_bar",
                value = 0,
                display_pct = TRUE,
                title = ""
                )
            ),
            br(), br(),
            uiOutput("arrow_profile"),
            br(),
            uiOutput("get_allele_profile"),
            conditionalPanel(
              "input.get_allele_profile",
              br(),
              progressBar(
                "progress_profile",
                value = 0,
                display_pct = TRUE,
                title = "")
            ),
            htmlOutput("typing_fin"),
            ),
          column(
            width = 1
            ),
          column(
            width = 2,
            align = "center",
            br(), br(),
            h3(p("Typing Results"), style = "color:white"),
            br(), br(),
            uiOutput("sel_result"),
            addSpinner(
              tableOutput("typ_res_tab"),
              spin = "dots", 
              color = "#ffffff")
            ),
          column(width = 1),
          column(
            width = 4,
            align = "center",
            br(), br(),
            h3(p("Append to Database"), style = "color:white"),
            br(), br(),
            column(width = 1),
            column(width = 6,
                   box(solidHeader = TRUE,
                       status = "primary",
                       width = "100%",
                       textInput("assembly_id",
                                 "Assembly ID"),
                       textInput("assembly_name",
                                 "Assembly Name"),
                       dateInput(
                         "append_isodate",
                         label = "Isolation Date",
                         width = "50%"
                       ),
                       textInput(
                         "append_host",
                         label = "Host"
                       ),
                       pickerInput("append_country", "Country",
                                   choices = list(
                                     "Common" = sel_countries,
                                     "All Countries" = country_names
                                   ),
                                   options = list(
                                     `live-search` = TRUE,
                                     `actions-box` = TRUE,
                                     size = 10,
                                     style = "background-color: white; border-radius: 5px;"
                                   )
                       ),
                       textInput(
                         "append_city",
                         label = "City"
                       ),
                       dateInput(
                         "append_analysisdate",
                         label = "Typing Date",
                         value = Sys.Date(),
                         width = "50%"
                       )
                   )
                   ),
            column(width = 4,
                   br(), br(), br(), br(), br(), br(), br(), br(), br(),
                   actionButton(
                     inputId = "append",
                     label = "Append"
                   )
                   )
          )
        )
      ),
      
     
      # Tab Visualization -------------------------------------------------------
      
      
      tabItem(tabName = "visualization",
              
              fluidRow(
                column(width = 1),
                column(
                  width = 10,
                  br(),
                  conditionalPanel(
                    "input.generate_tree=='Random'",
                    addSpinner(
                      plotOutput("tree_random"),
                      spin = "dots", 
                      color = "#ffffff")
                  ),
                  conditionalPanel(
                    "input.generate_tree=='Local'",
                    addSpinner(
                      plotOutput("tree_local"),
                      spin = "dots", 
                      color = "#ffffff")
                  )
                ),
                column(width = 1)
              ),
              
              br(), hr(), ##### Control Panels
              conditionalPanel(
                "input.generate_tree == 'Uploaded data' && input.slot == '1'",    
                fluidRow(
                  column(width = 2,
                         h3(p("Layout"), style = "color:white"),
                         br(),
                         fluidRow(
                           column(width = 12,
                                  checkboxInput(inputId = "show_layout1",
                                                label = "Show layout options",
                                                value = TRUE)
                           )
                         ),
                         br(),
                         conditionalPanel("input.show_layout1==true",
                                          radioButtons("tree_type1", 
                                                       label = h5("Tree Type", style = "color:white"),
                                                       choices = list("Phylogram", "Chronogram", "Cladogram"), 
                                                       selected = "Phylogram"),
                                          br(),
                                          fluidRow(
                                            column(width = 10,
                                                   conditionalPanel("input.tree_type1=='Phylogram'",
                                                                    selectInput("layout1",
                                                                                h5("Select Theme", style = "color:white"),
                                                                                choices = list(linear = list("Rectangular" = "rectangular",
                                                                                                             "Roundrect"= "roundrect",
                                                                                                             "Slanted" = "slanted",
                                                                                                             "Ellipse" = "ellipse"),
                                                                                               circular = list("Circular" = "circular",
                                                                                                               "Fan" = "fan"),
                                                                                               unrooted = list("Daylight" = "daylight",
                                                                                                               "Equal Angle" = "equal_angle")),
                                                                                selected = "roundrect",
                                                                                width = "200px")
                                                   ),
                                                   conditionalPanel("input.tree_type1=='Chronogram'",
                                                                    selectInput("layout1",
                                                                                h5("Select Theme", style = "color:white"),
                                                                                choices = list(linear = list("Rectangular" = "rectangular",
                                                                                                             "Roundrect"= "roundrect",
                                                                                                             "Ellipse" = "ellipse")),
                                                                                selected = "roundrect",
                                                                                width = "200px")
                                                   ),
                                                   conditionalPanel("input.tree_type1=='Cladogram'",
                                                                    selectInput("layout1",
                                                                                h5("Select Theme", style = "color:white"),
                                                                                choices = list(linear = list("Rectangular" = "rectangular",
                                                                                                             "Roundrect"= "roundrect",
                                                                                                             "Slanted" = "slanted",
                                                                                                             "Ellipse" = "ellipse"),
                                                                                               circular = list("Circular" = "circular",
                                                                                                               "Fan" = "fan"),
                                                                                               unrooted = list("Daylight" = "daylight",
                                                                                                               "Equal Angle" = "equal_angle")),
                                                                                selected = "roundrect",
                                                                                width = "200px")
                                                   ),
                                            ),
                                          ),
                                          br(),
                                          fluidRow(
                                            column(width = 6,
                                                   colorPickr(inputId = "background_color1",
                                                              label = h5("Background", style = "color:white"),
                                                              selected = "#ffffff",
                                                              opacity = FALSE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "100%")
                                            ),
                                            column(width = 6,
                                                   colorPickr(inputId = "branch_color1",
                                                              label = h5("Branches", style = "color:white"),
                                                              selected = "#000000",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "100%")
                                            )
                                          )
                         )
                  ),
                  column(width = 2,
                         h3(p("Tip Labels"), style = "color:white"),
                         br(),
                         fluidRow(
                           column(width = 12,
                                  checkboxInput(inputId = "label1",
                                                label = "Show Tip Labels",
                                                value = TRUE)
                           )
                         ),
                         br(),
                         conditionalPanel("input.label1==true",
                                          fluidRow(
                                            column(width = 12,
                                                   checkboxInput(inputId = "label_angle1",
                                                                 label = "Correct Label Angle",
                                                                 value = FALSE),
                                                   numericInput(inputId = "label_size1",
                                                                label = h5("Size", style = "color:white"),
                                                                value = 4,
                                                                min = 1,
                                                                max = 10,
                                                                step = 1,
                                                                width = "60px"),
                                                   colorPickr(inputId = "label_color1",
                                                              label = h5("Color", style = "color:white"),
                                                              selected = "#FF0000",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "30%"))
                                          )
                         )
                  ),
                  column(width = 2,
                         h3(p("Scale"), style = "color:white"),
                         br(),
                         fluidRow(
                           column(width = 12,
                                  checkboxInput(inputId = "show_scale1",
                                                label = "Show scale",
                                                value = TRUE)
                           )
                         ),
                         br(),
                         conditionalPanel("input.show_scale1==true",
                                          fluidRow(
                                            column(width = 7,
                                                   radioButtons(inputId = "scale1", 
                                                                label = h5("Type", style = "color:white"),
                                                                choices = list("Branch Scale" = 1, "X Scale" = 2), 
                                                                selected = 1)),
                                            column(width = 5,
                                                   h5(p("Color"), style = "color:white"),
                                                   colorPickr(inputId = "scale_color1",
                                                              label = NULL,
                                                              selected = "#000000",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "30%"))
                                          ),
                                          br(),
                                          fluidRow(
                                            column(width = 5,
                                                   numericInput(inputId = "scale_x1",
                                                                label = h5("X", style = "color:white"),
                                                                value = 4,
                                                                min = 0,
                                                                max = 5,
                                                                step = 0.5,
                                                                width = "70px"),
                                                   numericInput(inputId = "scale_line1",
                                                                label = h5("Line size", style = "color:white"),
                                                                value = 0.5,
                                                                step = 0.2,
                                                                width = "70px"),
                                                   numericInput(inputId = "scale_text1",
                                                                label = h5("Font size", style = "color:white"),
                                                                value = 6.5,
                                                                step = 0.5,
                                                                max = 12,
                                                                min = 4,
                                                                width = "70px")
                                            ),
                                            column(width = 5,
                                                   numericInput(inputId = "scale_y1",
                                                                label = h5("Y", style = "color:white"),
                                                                value = 0,
                                                                step = 1,
                                                                min = 0,
                                                                max = 30,
                                                                width = "70px"),
                                                   numericInput(inputId = "scale_width1",
                                                                label = h5("Width", style = "color:white"),
                                                                value = 0.3,
                                                                step = 0.1,
                                                                width = "70px")
                                                   
                                            ),
                                          )
                         ) 
                  ),
                  column(width = 2,
                         h3(p("Nodes"), style = "color:white"),
                         br(),
                         column(width = 12,
                                fluidRow(
                                  checkboxInput(inputId = "node_highlight1",
                                                label = "Highlight nodes",
                                                value = TRUE)
                                )
                         ),
                         br(),
                         conditionalPanel("input.node_highlight1==true",
                                          fluidRow(
                                            column(width = 10,
                                                   selectInput(inputId = "node_shape1",
                                                               label = h5("Shape", style = "color:white"),
                                                               choices = c("Square" = 0, "Circle" = 1, "Triangle Point Up" = 2, 
                                                                           "Plus" = 3, "Cross" = 4, "Diamond" = 5,
                                                                           "Triangle Point Down" = 6, "Square Cross" = 7, "Star" = 8,
                                                                           "Diamond Plus" = 9, "Circle Plus" = 10, "Triangles Up Down" = 11,
                                                                           "Square Plus" = 12, "Circle Cross" = 13, "Square Triangle Down" = 14,
                                                                           "Filled Square" = 15, "Filled Circle" = 16, "Filled Triangle Up" = 17,
                                                                           "Filled Diamond" = 18, "Solid Circle" = 19, "Bullet" = 20),
                                                               selected = 16)
                                                   
                                            ),
                                          ),
                                          fluidRow(
                                            column(width = 7,
                                                   numericInput(inputId = "node_size1",
                                                                label = h5("Size", style = "color:white"),
                                                                value = 3,
                                                                min = 1,
                                                                max = 10,
                                                                step = 1,
                                                                width = "70px"),
                                                   numericInput(inputId = "node_alpha1",
                                                                label = h5("Opacity", style = "color:white"),
                                                                value = 1,
                                                                min = 0,
                                                                max = 1,
                                                                step = 0.1,
                                                                width = "70px")
                                                   
                                            ),
                                            column(width = 2,
                                                   colorPickr(inputId = "node_color1",
                                                              label = h5("Color", style = "color:white"),
                                                              selected = "#35B779",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "30%")
                                            )      
                                          )
                         )
                  ),
                  column(width = 2,
                         h3(p("Tips"), style = "color:white"),
                         br(),
                         column(width = 12,
                                fluidRow(
                                  checkboxInput(inputId = "tip_highlight1",
                                                label = "Highlight tips",
                                                value = FALSE)
                                )
                         ),
                         br(),
                         conditionalPanel("input.tip_highlight1==true",
                                          fluidRow(
                                            column(width = 10,
                                                   selectInput(inputId = "tip_shape1",
                                                               label = h5("Shape", style = "color:white"),
                                                               choices = c("Square" = 0, "Circle" = 1, "Triangle Point Up" = 2, 
                                                                           "Plus" = 3, "Cross" = 4, "Diamond" = 5,
                                                                           "Triangle Point Down" = 6, "Square Cross" = 7, "Star" = 8,
                                                                           "Diamond Plus" = 9, "Circle Plus" = 10, "Triangles Up Down" = 11,
                                                                           "Square Plus" = 12, "Circle Cross" = 13, "Square Triangle Down" = 14,
                                                                           "Filled Square" = 15, "Filled Circle" = 16, "Filled Triangle Up" = 17,
                                                                           "Filled Diamond" = 18, "Solid Circle" = 19, "Bullet" = 20),
                                                               selected = 4)
                                                   
                                            ),
                                          ),
                                          fluidRow(
                                            column(width = 7,
                                                   numericInput(inputId = "tip_size1",
                                                                label = h5("Size", style = "color:white"),
                                                                value = 4,
                                                                min = 0,
                                                                max = 10,
                                                                step = 1, width = "70px"),
                                                   numericInput(inputId = "tip_alpha1",
                                                                label = h5("Opacity", style = "color:white"),
                                                                value = 1,
                                                                min = 0,
                                                                max = 1,
                                                                step = 0.1,
                                                                width = "70px")
                                            ),
                                            column(width = 2,
                                                   colorPickr(inputId = "tip_color1",
                                                              label = h5("Color", style = "color:white"),
                                                              selected = "#000000",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "30%")
                                            )
                                          )
                         )
                  ),
                  column(width = 2,
                         h3(p("Orientation"), style = "color:white"),
                         br(),
                         column(width = 6,
                                checkboxInput(inputId = "rev_x_axis1", 
                                              label = "Reverse x-Axis", 
                                              value = FALSE),
                                checkboxInput(inputId = "rev_y_axis1",
                                              label = "Reverse y-Axis",
                                              value = FALSE)),
                         column(width = 6,
                                numericInput(inputId = "rotate1",
                                             label = h5("Angle", style = "color:white"), 
                                             value = 0,
                                             min = -180,
                                             max = 180,
                                             step = 1,
                                             width = "75px"))
                  ),
                  
                )
              ),
              conditionalPanel(
                "input.generate_tree == 'Uploaded data' && input.slot == '2'",    
                fluidRow(
                  column(width = 2,
                         h3(p("Layout"), style = "color:white"),
                         br(),
                         fluidRow(
                           column(width = 12,
                                  checkboxInput(inputId = "show_layout2",
                                                label = "Show layout options",
                                                value = TRUE)
                           )
                         ),
                         br(),
                         conditionalPanel("input.show_layout2==true",
                                          radioButtons(inputId = "tree_type2", 
                                                       label = h5("Tree Type", style = "color:white"),
                                                       choices = list("Phylogram", "Chronogram", "Cladogram"), 
                                                       selected = "Phylogram"),
                                          br(),
                                          fluidRow(
                                            column(width = 10,
                                                   conditionalPanel("input.tree_type2=='Phylogram'",
                                                                    selectInput(inputId = "layout2",
                                                                                h5("Select Theme", style = "color:white"),
                                                                                choices = list(linear = list("Rectangular" = "rectangular",
                                                                                                             "Roundrect"= "roundrect",
                                                                                                             "Slanted" = "slanted",
                                                                                                             "Ellipse" = "ellipse"),
                                                                                               circular = list("Circular" = "circular",
                                                                                                               "Fan" = "fan"),
                                                                                               unrooted = list("Daylight" = "daylight",
                                                                                                               "Equal Angle" = "equal_angle")),
                                                                                selected = "roundrect",
                                                                                width = "200px")
                                                   ),
                                                   conditionalPanel("input.tree_type2=='Chronogram'",
                                                                    selectInput("layout2",
                                                                                h5("Select Theme", style = "color:white"),
                                                                                choices = list(linear = list("Rectangular" = "rectangular",
                                                                                                             "Roundrect"= "roundrect",
                                                                                                             "Ellipse" = "ellipse")),
                                                                                selected = "roundrect",
                                                                                width = "200px")
                                                   ),
                                                   conditionalPanel("input.tree_type2=='Cladogram'",
                                                                    selectInput("layout2",
                                                                                h5("Select Theme", style = "color:white"),
                                                                                choices = list(linear = list("Rectangular" = "rectangular",
                                                                                                             "Roundrect"= "roundrect",
                                                                                                             "Slanted" = "slanted",
                                                                                                             "Ellipse" = "ellipse"),
                                                                                               circular = list("Circular" = "circular",
                                                                                                               "Fan" = "fan"),
                                                                                               unrooted = list("Daylight" = "daylight",
                                                                                                               "Equal Angle" = "equal_angle")),
                                                                                selected = "roundrect",
                                                                                width = "200px")
                                                   ),
                                            ),
                                          ),
                                          br(),
                                          fluidRow(
                                            column(width = 6,
                                                   colorPickr(inputId = "background_color2",
                                                              label = h5("Background", style = "color:white"),
                                                              selected = "#ffffff",
                                                              opacity = FALSE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "100%")
                                            ),
                                            column(width = 6,
                                                   colorPickr(inputId = "branch_color2",
                                                              label = h5("Branches", style = "color:white"),
                                                              selected = "#000000",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "100%")
                                            )
                                          )
                         )
                  ),
                  column(width = 2,
                         h3(p("Tip Labels"), style = "color:white"),
                         br(),
                         fluidRow(
                           column(width = 12,
                                  checkboxInput(inputId = "label2",
                                                label = "Show Tip Labels",
                                                value = TRUE)
                           )
                         ),
                         br(),
                         conditionalPanel("input.label2==true",
                                          fluidRow(
                                            column(width = 12,
                                                   checkboxInput(inputId = "label_angle2",
                                                                 label = "Correct Label Angle",
                                                                 value = FALSE),
                                                   numericInput(inputId = "label_size2",
                                                                label = h5("Size", style = "color:white"),
                                                                value = 4,
                                                                min = 1,
                                                                max = 10,
                                                                step = 1,
                                                                width = "60px"),
                                                   colorPickr(inputId = "label_color2",
                                                              label = h5("Color", style = "color:white"),
                                                              selected = "#ff0000",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "30%"))
                                          )
                         )
                  ),
                  column(width = 2,
                         h3(p("Scale"), style = "color:white"),
                         br(),
                         fluidRow(
                           column(width = 12,
                                  checkboxInput(inputId = "show_scale2",
                                                label = "Show scale",
                                                value = TRUE)
                           )
                         ),
                         br(),
                         conditionalPanel("input.show_scale2==true",
                                          fluidRow(
                                            column(width = 7,
                                                   radioButtons(inputId = "scale2", 
                                                                label = h5("Type", style = "color:white"),
                                                                choices = list("Branch Scale" = 1, "X Scale" = 2), 
                                                                selected = 1)),
                                            column(width = 5,
                                                   h5(p("Color"), style = "color:white"),
                                                   colorPickr(inputId = "scale_color2",
                                                              label = NULL,
                                                              selected = "#000000",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "30%"))
                                          ),
                                          br(),
                                          fluidRow(
                                            column(width = 5,
                                                   numericInput(inputId = "scale_x2",
                                                                label = h5("X", style = "color:white"),
                                                                value = 4,
                                                                min = 0,
                                                                max = 5,
                                                                step = 0.5,
                                                                width = "70px"),
                                                   numericInput(inputId = "scale_line2",
                                                                label = h5("Line size", style = "color:white"),
                                                                value = 0.5,
                                                                step = 0.2,
                                                                width = "70px"),
                                                   numericInput(inputId = "scale_text2",
                                                                label = h5("Font size", style = "color:white"),
                                                                value = 6.5,
                                                                step = 0.5,
                                                                max = 12,
                                                                min = 4,
                                                                width = "70px")
                                            ),
                                            column(width = 5,
                                                   numericInput(inputId = "scale_y2",
                                                                label = h5("Y", style = "color:white"),
                                                                value = 0,
                                                                step = 1,
                                                                min = 0,
                                                                max = 30,
                                                                width = "70px"),
                                                   numericInput(inputId = "scale_width2",
                                                                label = h5("Width", style = "color:white"),
                                                                value = 0.3,
                                                                step = 0.1,
                                                                width = "70px")
                                                   
                                            ),
                                          )
                         ) 
                  ),
                  column(width = 2,
                         h3(p("Nodes"), style = "color:white"),
                         br(),
                         column(width = 12,
                                fluidRow(
                                  checkboxInput(inputId = "node_highlight2",
                                                label = "Highlight nodes",
                                                value = TRUE)
                                )
                         ),
                         br(),
                         conditionalPanel("input.node_highlight2==true",
                                          fluidRow(
                                            column(width = 10,
                                                   selectInput(inputId = "node_shape2",
                                                               label = h5("Shape", style = "color:white"),
                                                               choices = c("Square" = 0, "Circle" = 1, "Triangle Point Up" = 2, 
                                                                           "Plus" = 3, "Cross" = 4, "Diamond" = 5,
                                                                           "Triangle Point Down" = 6, "Square Cross" = 7, "Star" = 8,
                                                                           "Diamond Plus" = 9, "Circle Plus" = 10, "Triangles Up Down" = 11,
                                                                           "Square Plus" = 12, "Circle Cross" = 13, "Square Triangle Down" = 14,
                                                                           "Filled Square" = 15, "Filled Circle" = 16, "Filled Triangle Up" = 17,
                                                                           "Filled Diamond" = 18, "Solid Circle" = 19, "Bullet" = 20),
                                                               selected = 16)
                                                   
                                            ),
                                          ),
                                          fluidRow(
                                            column(width = 7,
                                                   numericInput(inputId = "node_size2",
                                                                label = h5("Size", style = "color:white"),
                                                                value = 3,
                                                                min = 1,
                                                                max = 10,
                                                                step = 1,
                                                                width = "70px"),
                                                   numericInput(inputId = "node_alpha2",
                                                                label = h5("Opacity", style = "color:white"),
                                                                value = 1,
                                                                min = 0,
                                                                max = 1,
                                                                step = 0.1,
                                                                width = "70px")
                                                   
                                            ),
                                            column(width = 2,
                                                   colorPickr(inputId = "node_color2",
                                                              label = h5("Color", style = "color:white"),
                                                              selected = "#35B779",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "30%")
                                            )      
                                          )
                         )
                  ),
                  column(width = 2,
                         h3(p("Tips"), style = "color:white"),
                         br(),
                         column(width = 12,
                                fluidRow(
                                  checkboxInput(inputId = "tip_highlight2",
                                                label = "Highlight tips",
                                                value = FALSE)
                                )
                         ),
                         br(),
                         conditionalPanel("input.tip_highlight2==true",
                                          fluidRow(
                                            column(width = 10,
                                                   selectInput(inputId = "tip_shape2",
                                                               label = h5("Shape", style = "color:white"),
                                                               choices = c("Square" = 0, "Circle" = 1, "Triangle Point Up" = 2, 
                                                                           "Plus" = 3, "Cross" = 4, "Diamond" = 5,
                                                                           "Triangle Point Down" = 6, "Square Cross" = 7, "Star" = 8,
                                                                           "Diamond Plus" = 9, "Circle Plus" = 10, "Triangles Up Down" = 11,
                                                                           "Square Plus" = 12, "Circle Cross" = 13, "Square Triangle Down" = 14,
                                                                           "Filled Square" = 15, "Filled Circle" = 16, "Filled Triangle Up" = 17,
                                                                           "Filled Diamond" = 18, "Solid Circle" = 19, "Bullet" = 20),
                                                               selected = 4)
                                                   
                                            ),
                                          ),
                                          fluidRow(
                                            column(width = 7,
                                                   numericInput(inputId = "tip_size2",
                                                                label = h5("Size", style = "color:white"),
                                                                value = 4,
                                                                min = 0,
                                                                max = 10,
                                                                step = 1, width = "70px"),
                                                   numericInput(inputId = "tip_alpha2",
                                                                label = h5("Opacity", style = "color:white"),
                                                                value = 1,
                                                                min = 0,
                                                                max = 1,
                                                                step = 0.1,
                                                                width = "70px")
                                            ),
                                            column(width = 2,
                                                   colorPickr(inputId = "tip_color2",
                                                              label = h5("Color", style = "color:white"),
                                                              selected = "#000000",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "30%")
                                            )
                                          )
                         )
                  ),
                  column(width = 2,
                         h3(p("Orientation"), style = "color:white"),
                         br(),
                         column(width = 6,
                                checkboxInput(inputId = "rev_x_axis2", 
                                              label = "Reverse x-Axis", 
                                              value = FALSE),
                                checkboxInput(inputId = "rev_y_axis2",
                                              label = "Reverse y-Axis",
                                              value = FALSE)),
                         column(width = 6,
                                numericInput(inputId = "rotate2",
                                             label = h5("Angle", style = "color:white"), 
                                             value = 0,
                                             min = -180,
                                             max = 180,
                                             step = 1,
                                             width = "75px"))
                  ),
                  
                )
              ),
              conditionalPanel(
                "input.generate_tree == 'Uploaded data' && input.slot == '3'",    
                fluidRow(
                  column(width = 2,
                         h3(p("Layout"), style = "color:white"),
                         br(),
                         fluidRow(
                           column(width = 12,
                                  checkboxInput(inputId = "show_layout3",
                                                label = "Show layout options",
                                                value = TRUE)
                           )
                         ),
                         br(),
                         conditionalPanel("input.show_layout3==true",
                                          radioButtons(inputId = "tree_type3", 
                                                       label = h5("Tree Type", style = "color:white"),
                                                       choices = list("Phylogram", "Chronogram", "Cladogram"), 
                                                       selected = "Phylogram"),
                                          br(),
                                          fluidRow(
                                            column(width = 10,
                                                   conditionalPanel("input.tree_type3=='Phylogram'",
                                                                    selectInput("layout3",
                                                                                h5("Select Theme", style = "color:white"),
                                                                                choices = list(linear = list("Rectangular" = "rectangular",
                                                                                                             "Roundrect"= "roundrect",
                                                                                                             "Slanted" = "slanted",
                                                                                                             "Ellipse" = "ellipse"),
                                                                                               circular = list("Circular" = "circular",
                                                                                                               "Fan" = "fan"),
                                                                                               unrooted = list("Daylight" = "daylight",
                                                                                                               "Equal Angle" = "equal_angle")),
                                                                                selected = "roundrect",
                                                                                width = "200px")
                                                   ),
                                                   conditionalPanel("input.tree_type3=='Chronogram'",
                                                                    selectInput("layout3",
                                                                                h5("Select Theme", style = "color:white"),
                                                                                choices = list(linear = list("Rectangular" = "rectangular",
                                                                                                             "Roundrect"= "roundrect",
                                                                                                             "Ellipse" = "ellipse")),
                                                                                selected = "roundrect",
                                                                                width = "200px")
                                                   ),
                                                   conditionalPanel("input.tree_type3=='Cladogram'",
                                                                    selectInput("layout3",
                                                                                h5("Select Theme", style = "color:white"),
                                                                                choices = list(linear = list("Rectangular" = "rectangular",
                                                                                                             "Roundrect"= "roundrect",
                                                                                                             "Slanted" = "slanted",
                                                                                                             "Ellipse" = "ellipse"),
                                                                                               circular = list("Circular" = "circular",
                                                                                                               "Fan" = "fan"),
                                                                                               unrooted = list("Daylight" = "daylight",
                                                                                                               "Equal Angle" = "equal_angle")),
                                                                                selected = "roundrect",
                                                                                width = "200px")
                                                   ),
                                            ),
                                          ),
                                          br(),
                                          fluidRow(
                                            column(width = 6,
                                                   colorPickr(inputId = "background_color3",
                                                              label = h5("Background", style = "color:white"),
                                                              selected = "#ffffff",
                                                              opacity = FALSE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "100%")
                                            ),
                                            column(width = 6,
                                                   colorPickr(inputId = "branch_color3",
                                                              label = h5("Branches", style = "color:white"),
                                                              selected = "#000000",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "100%")
                                            )
                                          )
                         )
                  ),
                  column(width = 2,
                         h3(p("Tip Labels"), style = "color:white"),
                         br(),
                         fluidRow(
                           column(width = 12,
                                  checkboxInput(inputId = "label3",
                                                label = "Show Tip Labels",
                                                value = TRUE)
                           )
                         ),
                         br(),
                         conditionalPanel("input.label3==true",
                                          fluidRow(
                                            column(width = 12,
                                                   checkboxInput(inputId = "label_angle3",
                                                                 label = "Correct Label Angle",
                                                                 value = FALSE),
                                                   numericInput(inputId = "label_size3",
                                                                label = h5("Size", style = "color:white"),
                                                                value = 4,
                                                                min = 1,
                                                                max = 10,
                                                                step = 1,
                                                                width = "60px"),
                                                   colorPickr(inputId = "label_color3",
                                                              label = h5("Color", style = "color:white"),
                                                              selected = "#ff0000",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "30%"))
                                          )
                         )
                  ),
                  column(width = 2,
                         h3(p("Scale"), style = "color:white"),
                         br(),
                         fluidRow(
                           column(width = 12,
                                  checkboxInput(inputId = "show_scale3",
                                                label = "Show scale",
                                                value = TRUE)
                           )
                         ),
                         br(),
                         conditionalPanel("input.show_scale3==true",
                                          fluidRow(
                                            column(width = 7,
                                                   radioButtons(inputId = "scale3", 
                                                                label = h5("Type", style = "color:white"),
                                                                choices = list("Branch Scale" = 1, "X Scale" = 2), 
                                                                selected = 1)),
                                            column(width = 5,
                                                   h5(p("Color"), style = "color:white"),
                                                   colorPickr(inputId = "scale_color3",
                                                              label = NULL,
                                                              selected = "#000000",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "30%"))
                                          ),
                                          br(),
                                          fluidRow(
                                            column(width = 5,
                                                   numericInput(inputId = "scale_x3",
                                                                label = h5("X", style = "color:white"),
                                                                value = 4,
                                                                min = 0,
                                                                max = 5,
                                                                step = 0.5,
                                                                width = "70px"),
                                                   numericInput(inputId = "scale_line3",
                                                                label = h5("Line size", style = "color:white"),
                                                                value = 0.5,
                                                                step = 0.2,
                                                                width = "70px"),
                                                   numericInput(inputId = "scale_text3",
                                                                label = h5("Font size", style = "color:white"),
                                                                value = 6.5,
                                                                step = 0.5,
                                                                max = 12,
                                                                min = 4,
                                                                width = "70px")
                                            ),
                                            column(width = 5,
                                                   numericInput(inputId = "scale_y3",
                                                                label = h5("Y", style = "color:white"),
                                                                value = 0,
                                                                step = 1,
                                                                min = 0,
                                                                max = 30,
                                                                width = "70px"),
                                                   numericInput(inputId = "scale_width3",
                                                                label = h5("Width", style = "color:white"),
                                                                value = 0.3,
                                                                step = 0.1,
                                                                width = "70px")
                                                   
                                            ),
                                          )
                         ) 
                  ),
                  column(width = 2,
                         h3(p("Nodes"), style = "color:white"),
                         br(),
                         column(width = 12,
                                fluidRow(
                                  checkboxInput(inputId = "node_highlight3",
                                                label = "Highlight nodes",
                                                value = TRUE)
                                )
                         ),
                         br(),
                         conditionalPanel("input.node_highlight3==true",
                                          fluidRow(
                                            column(width = 10,
                                                   selectInput(inputId = "node_shape3",
                                                               label = h5("Shape", style = "color:white"),
                                                               choices = c("Square" = 0, "Circle" = 1, "Triangle Point Up" = 2, 
                                                                           "Plus" = 3, "Cross" = 4, "Diamond" = 5,
                                                                           "Triangle Point Down" = 6, "Square Cross" = 7, "Star" = 8,
                                                                           "Diamond Plus" = 9, "Circle Plus" = 10, "Triangles Up Down" = 11,
                                                                           "Square Plus" = 12, "Circle Cross" = 13, "Square Triangle Down" = 14,
                                                                           "Filled Square" = 15, "Filled Circle" = 16, "Filled Triangle Up" = 17,
                                                                           "Filled Diamond" = 18, "Solid Circle" = 19, "Bullet" = 20),
                                                               selected = 16)
                                                   
                                            ),
                                          ),
                                          fluidRow(
                                            column(width = 7,
                                                   numericInput(inputId = "node_size3",
                                                                label = h5("Size", style = "color:white"),
                                                                value = 3,
                                                                min = 1,
                                                                max = 10,
                                                                step = 1,
                                                                width = "70px"),
                                                   numericInput(inputId = "node_alpha3",
                                                                label = h5("Opacity", style = "color:white"),
                                                                value = 1,
                                                                min = 0,
                                                                max = 1,
                                                                step = 0.1,
                                                                width = "70px")
                                                   
                                            ),
                                            column(width = 2,
                                                   colorPickr(inputId = "node_color3",
                                                              label = h5("Color", style = "color:white"),
                                                              selected = "#35B779",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "30%")
                                            )      
                                          )
                         )
                  ),
                  column(width = 2,
                         h3(p("Tips"), style = "color:white"),
                         br(),
                         column(width = 12,
                                fluidRow(
                                  checkboxInput(inputId = "tip_highlight3",
                                                label = "Highlight tips",
                                                value = FALSE)
                                )
                         ),
                         br(),
                         conditionalPanel("input.tip_highlight3==true",
                                          fluidRow(
                                            column(width = 10,
                                                   selectInput(inputId = "tip_shape3",
                                                               label = h5("Shape", style = "color:white"),
                                                               choices = c("Square" = 0, "Circle" = 1, "Triangle Point Up" = 2, 
                                                                           "Plus" = 3, "Cross" = 4, "Diamond" = 5,
                                                                           "Triangle Point Down" = 6, "Square Cross" = 7, "Star" = 8,
                                                                           "Diamond Plus" = 9, "Circle Plus" = 10, "Triangles Up Down" = 11,
                                                                           "Square Plus" = 12, "Circle Cross" = 13, "Square Triangle Down" = 14,
                                                                           "Filled Square" = 15, "Filled Circle" = 16, "Filled Triangle Up" = 17,
                                                                           "Filled Diamond" = 18, "Solid Circle" = 19, "Bullet" = 20),
                                                               selected = 4)
                                                   
                                            ),
                                          ),
                                          fluidRow(
                                            column(width = 7,
                                                   numericInput(inputId = "tip_size3",
                                                                label = h5("Size", style = "color:white"),
                                                                value = 4,
                                                                min = 0,
                                                                max = 10,
                                                                step = 1, width = "70px"),
                                                   numericInput(inputId = "tip_alpha3",
                                                                label = h5("Opacity", style = "color:white"),
                                                                value = 1,
                                                                min = 0,
                                                                max = 1,
                                                                step = 0.1,
                                                                width = "70px")
                                            ),
                                            column(width = 2,
                                                   colorPickr(inputId = "tip_color3",
                                                              label = h5("Color", style = "color:white"),
                                                              selected = "#000000",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "30%")
                                            )
                                          )
                         )
                  ),
                  column(width = 2,
                         h3(p("Orientation"), style = "color:white"),
                         br(),
                         column(width = 6,
                                checkboxInput(inputId = "rev_x_axis3", 
                                              label = "Reverse x-Axis", 
                                              value = FALSE),
                                checkboxInput(inputId = "rev_y_axis3",
                                              label = "Reverse y-Axis",
                                              value = FALSE)),
                         column(width = 6,
                                numericInput(inputId = "rotate3",
                                             label = h5("Angle", style = "color:white"), 
                                             value = 0,
                                             min = -180,
                                             max = 180,
                                             step = 1,
                                             width = "75px"))
                  ),
                  
                )
              ),
              conditionalPanel(
                "input.generate_tree == 'Uploaded data' && input.slot == '4'",    
                fluidRow(
                  column(width = 2,
                         h3(p("Layout"), style = "color:white"),
                         br(),
                         fluidRow(
                           column(width = 12,
                                  checkboxInput(inputId = "show_layout4",
                                                label = "Show layout options",
                                                value = TRUE)
                           )
                         ),
                         br(),
                         conditionalPanel("input.show_layout4==true",
                                          radioButtons("tree_type4", 
                                                       label = h5("Tree Type", style = "color:white"),
                                                       choices = list("Phylogram", "Chronogram", "Cladogram"), 
                                                       selected = "Phylogram"),
                                          br(),
                                          fluidRow(
                                            column(width = 10,
                                                   conditionalPanel("input.tree_type4=='Phylogram'",
                                                                    selectInput("layout4",
                                                                                h5("Select Theme", style = "color:white"),
                                                                                choices = list(linear = list("Rectangular" = "rectangular",
                                                                                                             "Roundrect"= "roundrect",
                                                                                                             "Slanted" = "slanted",
                                                                                                             "Ellipse" = "ellipse"),
                                                                                               circular = list("Circular" = "circular",
                                                                                                               "Fan" = "fan"),
                                                                                               unrooted = list("Daylight" = "daylight",
                                                                                                               "Equal Angle" = "equal_angle")),
                                                                                selected = "roundrect",
                                                                                width = "200px")
                                                   ),
                                                   conditionalPanel("input.tree_type4=='Chronogram'",
                                                                    selectInput("layout4",
                                                                                h5("Select Theme", style = "color:white"),
                                                                                choices = list(linear = list("Rectangular" = "rectangular",
                                                                                                             "Roundrect"= "roundrect",
                                                                                                             "Ellipse" = "ellipse")),
                                                                                selected = "roundrect",
                                                                                width = "200px")
                                                   ),
                                                   conditionalPanel("input.tree_type4=='Cladogram'",
                                                                    selectInput("layout4",
                                                                                h5("Select Theme", style = "color:white"),
                                                                                choices = list(linear = list("Rectangular" = "rectangular",
                                                                                                             "Roundrect"= "roundrect",
                                                                                                             "Slanted" = "slanted",
                                                                                                             "Ellipse" = "ellipse"),
                                                                                               circular = list("Circular" = "circular",
                                                                                                               "Fan" = "fan"),
                                                                                               unrooted = list("Daylight" = "daylight",
                                                                                                               "Equal Angle" = "equal_angle")),
                                                                                selected = "roundrect",
                                                                                width = "200px")
                                                   ),
                                            ),
                                          ),
                                          br(),
                                          fluidRow(
                                            column(width = 6,
                                                   colorPickr(inputId = "background_color4",
                                                              label = h5("Background", style = "color:white"),
                                                              selected = "#ffffff",
                                                              opacity = FALSE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "100%")
                                            ),
                                            column(width = 6,
                                                   colorPickr(inputId = "branch_color4",
                                                              label = h5("Branches", style = "color:white"),
                                                              selected = "#000000",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "100%")
                                            )
                                          )
                         )
                  ),
                  column(width = 2,
                         h3(p("Tip Labels"), style = "color:white"),
                         br(),
                         fluidRow(
                           column(width = 12,
                                  checkboxInput(inputId = "label4",
                                                label = "Show Tip Labels",
                                                value = TRUE)
                           )
                         ),
                         br(),
                         conditionalPanel("input.label4==true",
                                          fluidRow(
                                            column(width = 12,
                                                   checkboxInput(inputId = "label_angle4",
                                                                 label = "Correct Label Angle",
                                                                 value = FALSE),
                                                   numericInput(inputId = "label_size4",
                                                                label = h5("Size", style = "color:white"),
                                                                value = 4,
                                                                min = 1,
                                                                max = 10,
                                                                step = 1,
                                                                width = "60px"),
                                                   colorPickr(inputId = "label_color4",
                                                              label = h5("Color", style = "color:white"),
                                                              selected = "#ff0000",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "30%"))
                                          )
                         )
                  ),
                  column(width = 2,
                         h3(p("Scale"), style = "color:white"),
                         br(),
                         fluidRow(
                           column(width = 12,
                                  checkboxInput(inputId = "show_scale4",
                                                label = "Show scale",
                                                value = TRUE)
                           )
                         ),
                         br(),
                         conditionalPanel("input.show_scale4==true",
                                          fluidRow(
                                            column(width = 7,
                                                   radioButtons(inputId = "scale4", 
                                                                label = h5("Type", style = "color:white"),
                                                                choices = list("Branch Scale" = 1, "X Scale" = 2), 
                                                                selected = 1)),
                                            column(width = 5,
                                                   h5(p("Color"), style = "color:white"),
                                                   colorPickr(inputId = "scale_color4",
                                                              label = NULL,
                                                              selected = "#000000",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "30%"))
                                          ),
                                          br(),
                                          fluidRow(
                                            column(width = 5,
                                                   numericInput(inputId = "scale_x4",
                                                                label = h5("X", style = "color:white"),
                                                                value = 4,
                                                                min = 0,
                                                                max = 5,
                                                                step = 0.5,
                                                                width = "70px"),
                                                   numericInput(inputId = "scale_line4",
                                                                label = h5("Line size", style = "color:white"),
                                                                value = 0.5,
                                                                step = 0.2,
                                                                width = "70px"),
                                                   numericInput(inputId = "scale_text4",
                                                                label = h5("Font size", style = "color:white"),
                                                                value = 6.5,
                                                                step = 0.5,
                                                                max = 12,
                                                                min = 4,
                                                                width = "70px")
                                            ),
                                            column(width = 5,
                                                   numericInput(inputId = "scale_y4",
                                                                label = h5("Y", style = "color:white"),
                                                                value = 0,
                                                                step = 1,
                                                                min = 0,
                                                                max = 30,
                                                                width = "70px"),
                                                   numericInput(inputId = "scale_width4",
                                                                label = h5("Width", style = "color:white"),
                                                                value = 0.3,
                                                                step = 0.1,
                                                                width = "70px")
                                                   
                                            ),
                                          )
                         ) 
                  ),
                  column(width = 2,
                         h3(p("Nodes"), style = "color:white"),
                         br(),
                         column(width = 12,
                                fluidRow(
                                  checkboxInput(inputId = "node_highlight4",
                                                label = "Highlight nodes",
                                                value = TRUE)
                                )
                         ),
                         br(),
                         conditionalPanel("input.node_highlight4==true",
                                          fluidRow(
                                            column(width = 10,
                                                   selectInput(inputId = "node_shape4",
                                                               label = h5("Shape", style = "color:white"),
                                                               choices = c("Square" = 0, "Circle" = 1, "Triangle Point Up" = 2, 
                                                                           "Plus" = 3, "Cross" = 4, "Diamond" = 5,
                                                                           "Triangle Point Down" = 6, "Square Cross" = 7, "Star" = 8,
                                                                           "Diamond Plus" = 9, "Circle Plus" = 10, "Triangles Up Down" = 11,
                                                                           "Square Plus" = 12, "Circle Cross" = 13, "Square Triangle Down" = 14,
                                                                           "Filled Square" = 15, "Filled Circle" = 16, "Filled Triangle Up" = 17,
                                                                           "Filled Diamond" = 18, "Solid Circle" = 19, "Bullet" = 20),
                                                               selected = 16)
                                                   
                                            ),
                                          ),
                                          fluidRow(
                                            column(width = 7,
                                                   numericInput(inputId = "node_size4",
                                                                label = h5("Size", style = "color:white"),
                                                                value = 3,
                                                                min = 1,
                                                                max = 10,
                                                                step = 1,
                                                                width = "70px"),
                                                   numericInput(inputId = "node_alpha4",
                                                                label = h5("Opacity", style = "color:white"),
                                                                value = 1,
                                                                min = 0,
                                                                max = 1,
                                                                step = 0.1,
                                                                width = "70px")
                                                   
                                            ),
                                            column(width = 2,
                                                   colorPickr(inputId = "node_color4",
                                                              label = h5("Color", style = "color:white"),
                                                              selected = "#35B779",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "30%")
                                            )      
                                          )
                         )
                  ),
                  column(width = 2,
                         h3(p("Tips"), style = "color:white"),
                         br(),
                         column(width = 12,
                                fluidRow(
                                  checkboxInput(inputId = "tip_highlight4",
                                                label = "Highlight tips",
                                                value = FALSE)
                                )
                         ),
                         br(),
                         conditionalPanel("input.tip_highlight4==true",
                                          fluidRow(
                                            column(width = 10,
                                                   selectInput(inputId = "tip_shape4",
                                                               label = h5("Shape", style = "color:white"),
                                                               choices = c("Square" = 0, "Circle" = 1, "Triangle Point Up" = 2, 
                                                                           "Plus" = 3, "Cross" = 4, "Diamond" = 5,
                                                                           "Triangle Point Down" = 6, "Square Cross" = 7, "Star" = 8,
                                                                           "Diamond Plus" = 9, "Circle Plus" = 10, "Triangles Up Down" = 11,
                                                                           "Square Plus" = 12, "Circle Cross" = 13, "Square Triangle Down" = 14,
                                                                           "Filled Square" = 15, "Filled Circle" = 16, "Filled Triangle Up" = 17,
                                                                           "Filled Diamond" = 18, "Solid Circle" = 19, "Bullet" = 20),
                                                               selected = 4)
                                                   
                                            ),
                                          ),
                                          fluidRow(
                                            column(width = 7,
                                                   numericInput(inputId = "tip_size4",
                                                                label = h5("Size", style = "color:white"),
                                                                value = 4,
                                                                min = 0,
                                                                max = 10,
                                                                step = 1, width = "70px"),
                                                   numericInput(inputId = "tip_alpha4",
                                                                label = h5("Opacity", style = "color:white"),
                                                                value = 1,
                                                                min = 0,
                                                                max = 1,
                                                                step = 0.1,
                                                                width = "70px")
                                            ),
                                            column(width = 2,
                                                   colorPickr(inputId = "tip_color4",
                                                              label = h5("Color", style = "color:white"),
                                                              selected = "#000000",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "30%")
                                            )
                                          )
                         )
                  ),
                  column(width = 2,
                         h3(p("Orientation"), style = "color:white"),
                         br(),
                         column(width = 6,
                                checkboxInput(inputId = "rev_x_axis4", 
                                              label = "Reverse x-Axis", 
                                              value = FALSE),
                                checkboxInput(inputId = "rev_y_axis4",
                                              label = "Reverse y-Axis",
                                              value = FALSE)),
                         column(width = 6,
                                numericInput(inputId = "rotate4",
                                             label = h5("Angle", style = "color:white"), 
                                             value = 0,
                                             min = -180,
                                             max = 180,
                                             step = 1,
                                             width = "75px"))
                  ),
                  
                )
              ),
              conditionalPanel(
                "input.generate_tree=='Random'||input.generate_tree=='Local'",
                conditionalPanel(
                  "input.tree_algo=='Minimum-Spanning'",
                  fluidRow(
                    column(
                      width = 2,
                      radioGroupButtons(
                        inputId = "graph_mst",
                        choiceNames = c("NSCA", "Circle"),
                        choiceValues = c("nsca", "circle"),
                        checkIcon = list(yes = icon("check"))
                        )
                      )
                    ),
                  hr()
                ),
                conditionalPanel(
                  "input.tree_algo=='Neighbour-Joining'",
                  fluidRow(
                    column(
                      width = 2,
                      dropdownButton(
                        label = "Edit Labels",
                        right = TRUE,
                        virtualSelectInput(
                          "label_select",
                          label = "Select Labels",
                          multiple = TRUE,
                          choices = c("Host", "Country", "City")
                          #choices = metadata_cols
                          ),
                        circle = FALSE
                      )
                    )
                  ),
                  hr()
                ),
                fluidRow(
                  column(width = 2,
                         h3(p("Layout"), style = "color:white"),
                         br(),
                         fluidRow(
                           column(width = 12,
                                  checkboxInput(inputId = "show_layout_r",
                                                label = "Show layout options",
                                                value = TRUE)
                           )
                         ),
                         br(),
                         conditionalPanel("input.show_layout_r==true",
                                          radioButtons(inputId = "tree_type_r", 
                                                       label = h5("Tree Type", style = "color:white"),
                                                       choices = list("Phylogram", "Chronogram", "Cladogram"), 
                                                       selected = "Phylogram"),
                                          br(),
                                          fluidRow(
                                            column(width = 10,
                                                   conditionalPanel("input.tree_type_r=='Phylogram'",
                                                                    selectInput(inputId = "layout_r",
                                                                                h5("Select Theme", style = "color:white"),
                                                                                choices = list(linear = list("Rectangular" = "rectangular",
                                                                                                             "Roundrect"= "roundrect",
                                                                                                             "Slanted" = "slanted",
                                                                                                             "Ellipse" = "ellipse"),
                                                                                               circular = list("Circular" = "circular",
                                                                                                               "Fan" = "fan"),
                                                                                               unrooted = list("Daylight" = "daylight",
                                                                                                               "Equal Angle" = "equal_angle")),
                                                                                selected = "roundrect",
                                                                                width = "200px")
                                                   ),
                                                   conditionalPanel("input.tree_type_r=='Chronogram'",
                                                                    selectInput("layout_r",
                                                                                h5("Select Theme", style = "color:white"),
                                                                                choices = list(linear = list("Rectangular" = "rectangular",
                                                                                                             "Roundrect"= "roundrect",
                                                                                                             "Ellipse" = "ellipse")),
                                                                                selected = "roundrect",
                                                                                width = "200px")
                                                   ),
                                                   conditionalPanel("input.tree_type_r=='Cladogram'",
                                                                    selectInput("layout_r",
                                                                                h5("Select Theme", style = "color:white"),
                                                                                choices = list(linear = list("Rectangular" = "rectangular",
                                                                                                             "Roundrect"= "roundrect",
                                                                                                             "Slanted" = "slanted",
                                                                                                             "Ellipse" = "ellipse"),
                                                                                               circular = list("Circular" = "circular",
                                                                                                               "Fan" = "fan"),
                                                                                               unrooted = list("Daylight" = "daylight",
                                                                                                               "Equal Angle" = "equal_angle")),
                                                                                selected = "roundrect",
                                                                                width = "200px")
                                                   ),
                                            ),
                                          ),
                                          br(),
                                          fluidRow(
                                            column(width = 6,
                                                   colorPickr(inputId = "background_color_r",
                                                              label = h5("Background", style = "color:white"),
                                                              selected = "#ffffff",
                                                              opacity = FALSE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "100%")
                                            ),
                                            column(width = 6,
                                                   colorPickr(inputId = "branch_color_r",
                                                              label = h5("Branches", style = "color:white"),
                                                              selected = "#000000",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "100%")
                                            )
                                          )
                         )
                  ),
                  column(width = 2,
                         h3(p("Tip Labels"), style = "color:white"),
                         br(),
                         fluidRow(
                           column(width = 12,
                                  checkboxInput(inputId = "label_r",
                                                label = "Show Tip Labels",
                                                value = TRUE)
                           )
                         ),
                         br(),
                         conditionalPanel("input.label_r==true",
                                          fluidRow(
                                            column(width = 12,
                                                   checkboxInput(inputId = "label_angle_r",
                                                                 label = "Correct Label Angle",
                                                                 value = FALSE),
                                                   numericInput(inputId = "label_size_r",
                                                                label = h5("Size", style = "color:white"),
                                                                value = 4,
                                                                min = 1,
                                                                max = 10,
                                                                step = 1,
                                                                width = "60px"),
                                                   colorPickr(inputId = "label_color_r",
                                                              label = h5("Color", style = "color:white"),
                                                              selected = "#ff0000",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "30%"))
                                          )
                         )
                  ),
                  column(width = 2,
                         h3(p("Scale"), style = "color:white"),
                         br(),
                         fluidRow(
                           column(width = 12,
                                  checkboxInput(inputId = "show_scale_r",
                                                label = "Show scale",
                                                value = TRUE)
                           )
                         ),
                         br(),
                         conditionalPanel("input.show_scale_r==true",
                                          fluidRow(
                                            column(width = 7,
                                                   radioButtons(inputId = "scale_r", 
                                                                label = h5("Type", style = "color:white"),
                                                                choices = list("Branch Scale" = 1, "X Scale" = 2), 
                                                                selected = 1)),
                                            column(width = 5,
                                                   h5(p("Color"), style = "color:white"),
                                                   colorPickr(inputId = "scale_color_r",
                                                              label = NULL,
                                                              selected = "#000000",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "30%"))
                                          ),
                                          br(),
                                          fluidRow(
                                            column(width = 5,
                                                   numericInput(inputId = "scale_x_r",
                                                                label = h5("X", style = "color:white"),
                                                                value = 4,
                                                                min = 0,
                                                                max = 5,
                                                                step = 0.5,
                                                                width = "70px"),
                                                   numericInput(inputId = "scale_line_r",
                                                                label = h5("Line size", style = "color:white"),
                                                                value = 0.5,
                                                                step = 0.2,
                                                                width = "70px"),
                                                   numericInput(inputId = "scale_text_r",
                                                                label = h5("Font size", style = "color:white"),
                                                                value = 6.5,
                                                                step = 0.5,
                                                                max = 12,
                                                                min = 4,
                                                                width = "70px")
                                            ),
                                            column(width = 5,
                                                   numericInput(inputId = "scale_y_r",
                                                                label = h5("Y", style = "color:white"),
                                                                value = 0,
                                                                step = 1,
                                                                min = 0,
                                                                max = 30,
                                                                width = "70px"),
                                                   numericInput(inputId = "scale_width_r",
                                                                label = h5("Width", style = "color:white"),
                                                                value = 0.3,
                                                                step = 0.1,
                                                                width = "70px")
                                                   
                                            ),
                                          )
                         ) 
                  ),
                  column(width = 2,
                         h3(p("Nodes"), style = "color:white"),
                         br(),
                         column(width = 12,
                                fluidRow(
                                  checkboxInput(inputId = "node_highlight_r",
                                                label = "Highlight nodes",
                                                value = TRUE)
                                )
                         ),
                         br(),
                         conditionalPanel("input.node_highlight_r==true",
                                          fluidRow(
                                            column(width = 10,
                                                   selectInput(inputId = "node_shape_r",
                                                               label = h5("Shape", style = "color:white"),
                                                               choices = c("Square" = 0, "Circle" = 1, "Triangle Point Up" = 2, 
                                                                           "Plus" = 3, "Cross" = 4, "Diamond" = 5,
                                                                           "Triangle Point Down" = 6, "Square Cross" = 7, "Star" = 8,
                                                                           "Diamond Plus" = 9, "Circle Plus" = 10, "Triangles Up Down" = 11,
                                                                           "Square Plus" = 12, "Circle Cross" = 13, "Square Triangle Down" = 14,
                                                                           "Filled Square" = 15, "Filled Circle" = 16, "Filled Triangle Up" = 17,
                                                                           "Filled Diamond" = 18, "Solid Circle" = 19, "Bullet" = 20),
                                                               selected = 16)
                                                   
                                            ),
                                          ),
                                          fluidRow(
                                            column(width = 7,
                                                   numericInput(inputId = "node_size_r",
                                                                label = h5("Size", style = "color:white"),
                                                                value = 3,
                                                                min = 1,
                                                                max = 10,
                                                                step = 1,
                                                                width = "70px"),
                                                   numericInput(inputId = "node_alpha_r",
                                                                label = h5("Opacity", style = "color:white"),
                                                                value = 1,
                                                                min = 0,
                                                                max = 1,
                                                                step = 0.1,
                                                                width = "70px")
                                                   
                                            ),
                                            column(width = 2,
                                                   colorPickr(inputId = "node_color_r",
                                                              label = h5("Color", style = "color:white"),
                                                              selected = "#35B779",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE,
                                                                                 rgba = FALSE,
                                                                                 input = TRUE,
                                                                                 save = TRUE,
                                                                                 clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "30%")
                                            )      
                                          )
                         )
                  ),
                  column(width = 2,
                         h3(p("Tips"), style = "color:white"),
                         br(),
                         column(width = 12,
                                fluidRow(
                                  checkboxInput(inputId = "tip_highlight_r",
                                                label = "Highlight tips",
                                                value = FALSE)
                                )
                         ),
                         br(),
                         conditionalPanel("input.tip_highlight_r==true",
                                          fluidRow(
                                            column(width = 10,
                                                   selectInput(inputId = "tip_shape_r",
                                                               label = h5("Shape", style = "color:white"),
                                                               choices = c("Square" = 0, "Circle" = 1, "Triangle Point Up" = 2, 
                                                                           "Plus" = 3, "Cross" = 4, "Diamond" = 5,
                                                                           "Triangle Point Down" = 6, "Square Cross" = 7, "Star" = 8,
                                                                           "Diamond Plus" = 9, "Circle Plus" = 10, "Triangles Up Down" = 11,
                                                                           "Square Plus" = 12, "Circle Cross" = 13, "Square Triangle Down" = 14,
                                                                           "Filled Square" = 15, "Filled Circle" = 16, "Filled Triangle Up" = 17,
                                                                           "Filled Diamond" = 18, "Solid Circle" = 19, "Bullet" = 20),
                                                               selected = 4)
                                                   
                                            ),
                                          ),
                                          fluidRow(
                                            column(width = 7,
                                                   numericInput(inputId = "tip_size_r",
                                                                label = h5("Size", style = "color:white"),
                                                                value = 4,
                                                                min = 0,
                                                                max = 10,
                                                                step = 1, width = "70px"),
                                                   numericInput(inputId = "tip_alpha_r",
                                                                label = h5("Opacity", style = "color:white"),
                                                                value = 1,
                                                                min = 0,
                                                                max = 1,
                                                                step = 0.1,
                                                                width = "70px")
                                            ),
                                            column(width = 2,
                                                   colorPickr(inputId = "tip_color_r",
                                                              label = h5("Color", style = "color:white"),
                                                              selected = "#000000",
                                                              opacity = TRUE,
                                                              update = "save",
                                                              interaction = list(hex = TRUE, rgba = FALSE, input = TRUE, save = TRUE, clear = FALSE),
                                                              position = "right-start",
                                                              swatches = scales::viridis_pal()(10),
                                                              theme = "nano",
                                                              useAsButton = TRUE,
                                                              width = "30%")
                                            )
                                          )
                         )
                  ),
                  column(width = 2,
                         h3(p("Orientation"), style = "color:white"),
                         br(),
                         column(width = 6,
                                checkboxInput(inputId = "rev_x_axis_r", 
                                              label = "Reverse x-Axis", 
                                              value = FALSE),
                                checkboxInput(inputId = "rev_y_axis_r",
                                              label = "Reverse y-Axis",
                                              value = FALSE)),
                         column(width = 6,
                                numericInput(inputId = "rotate_r",
                                             label = h5("Angle", style = "color:white"), 
                                             value = 0,
                                             min = -180,
                                             max = 180,
                                             step = 1,
                                             width = "75px"))
                  ),
                  
                )
              ),
              hr(), br(), br(), br(), br(), br(), br(), 
              fluidRow(
                br(), br(), br(), br(), br(),
                column(width = 4,
                       tags$a(href="https://github.com/infinity-a11y/phylo_tree", "Github Repository (Instructions, Source Code, etc.)")
                ),
                column(width = 4,
                       h5(p("Hochschule Furtwangen University"), style = "color:white")
                ),
                column(width = 4,
                       h5(p("Marian Freisleben, Jonathan Simantzik, Prof. Dr. Matthias Kohl "), style = "color:white")
                )
              )
      ), 
      
      
      # Tab Report --------------------------------------------------------------
      
      
      tabItem(
        tabName = "report",
        fluidRow(
          column(
            width = 3,
            align = "center",
            h2(p("Create Report"), style = "color:white"),
          )
        ),
        hr(),
        fluidRow(
          br(), br(),
          column(
            width = 2,
            align = "left",
            h3(p("Select Elements"), style = "color:white"),
            br(),
            uiOutput("include_general"),
            uiOutput("include_sampleinfo"),
            uiOutput("include_sequencing"),
            uiOutput("include_analysis")
          ),
          column(
            width = 3,
            align = 'left',
            h3(p("Displayed Elements"), style = "color:white"),
            br(), br(),
            box(
              solidHeader = TRUE,
              status = "primary",
              title = h4(p("General"), style = "color:white"),
              width = 12,
              conditionalPanel(
                "input.include_general.includes('Analysis Date')",
                dateInput(
                  inputId = "report_date",
                  label = "Date",
                  value = NULL,
                  width = "40%"
                )
              )
              ,
              conditionalPanel(
                "input.include_general.includes('Author')",
                textInput(
                  inputId = "author",
                  label = "Name of Author",
                  placeholder = "Institute/Working group/Responsible person"
                )
              ),
              conditionalPanel(
                "input.include_general.includes('Experiment Info')",
                textAreaInput(
                  inputId = "exp_info",
                  label = "Experiment Information",
                  value = "Comments about Experiment ...",
                  width = "100%",
                  height = NULL,
                  cols = NULL,
                  rows = NULL,
                  placeholder = NULL,
                  resize = "vertical"
                )
              ))
            ,
            br(),br(),
            box(
              solidHeader = TRUE,
              status = "primary",
              title = h4(p("Sample"), style = "color:white"),
              width = 12,
              conditionalPanel(
                "input.include_sampleinfo.includes('Sampling Date')",
                dateInput(
                  inputId = "report_sampledate",
                  label = "Sampling Date",
                  width = "40%",
                  value = NULL
                )
              ),
              conditionalPanel(
                "input.include_sampleinfo.includes('Sampling Location')",
                textInput(
                  inputId = "sample_location",
                  label = "Location",
                  width = "100%",
                  placeholder = "Place of sample collection (Country, City, Hospital, etc.)"
                )
              ),
              conditionalPanel(
                "input.include_sampleinfo.includes('Taken by (Name)')",
                textInput(
                  inputId = "sampled_by",
                  label = "Sample acquired by",
                  width = "100%",
                  placeholder = "Institute/Working group/Responsible person"
                )
              ),
              conditionalPanel(
                "input.include_sampleinfo.includes('Comment')",
                textAreaInput(
                  inputId = "sample_info",
                  label = "Comment",
                  value = "Comments about sample ...",
                  width = "100%",
                  height = NULL,
                  cols = NULL,
                  rows = NULL,
                  placeholder = NULL,
                  resize = "vertical"
                )
              ))
          ),
          column(
            width = 3,
            align = "left",
            br(), br(), br(), br(),br(),
            box(
              solidHeader = TRUE,
              status = "primary",
              title = h4(p("Sequencing"), style = "color:white"),
              width = 12,
              conditionalPanel(
                "input.include_sequencing.includes('Device')",
                selectInput(
                  inputId = "select_device",
                  label = "Sequencing Device",
                  choices = c("MinION Mk1B", "GridION"),
                  selected = "MinION Mk1B",
                  width = "50%"
                )
              ),
              conditionalPanel(
                "input.include_sequencing.includes('Flow Cell')",
                selectInput(
                  inputId = "select_flowcell",
                  label = "Flow Cell",
                  choices = c("R10.4", "R8.1"),
                  selected = "R10.5",
                  width = "50%"
                )
              ),
              conditionalPanel(
                "input.include_sequencing.includes('Run Start')",
                dateInput(
                  inputId = "report_runstart",
                  label = "Run Start",
                  width = "40%",
                  value = NULL
                )
              ),
              conditionalPanel(
                "input.include_sequencing.includes('Run Finished')",
                dateInput(
                  inputId = "report_runfinished",
                  label = "Run Finished",
                  width = "40%",
                  value = NULL
                )
              ),
              conditionalPanel(
                "input.include_sequencing.includes('Operator')",
                textInput(
                  inputId = "report_seqoperator",
                  label = "Operator",
                  width = "100%",
                  placeholder = "Responsible person"
                )
              ),
              conditionalPanel(
                "input.include_sequencing.includes('Output Size')",
                br(),
                h5(p("Output Size"), style = "color:white"),
                br()
              ),
              conditionalPanel(
                "input.include_sequencing.includes('Comment')",
                textAreaInput(
                  inputId = "report_seqcomment",
                  label = "Comments",
                  value = "Comments about sequencing/library preparation ...",
                  width = "100%",
                  height = NULL,
                  cols = NULL,
                  rows = NULL,
                  placeholder = NULL,
                  resize = "vertical"
                )
              )
            ),
            hr(), br(),
            box(
              solidHeader = TRUE,
              status = "primary",
              title = h4(p("Analysis"), style = "color:white"),
              width = 12,
              conditionalPanel(
                "input.include_analysis.includes('Analysis Date')",
                dateInput(
                  inputId = "report_analysisdate",
                  label = "Analysis Date",
                  width = "40%",
                  value = NULL
                )
              ),
              conditionalPanel(
                "input.include_analysis.includes('Assembly Parameters')",
                br(),
                h5(p("Assembly Parameters"), style = "color:white"),
                br(),
              ),
              conditionalPanel(
                "input.include_analysis.includes('cgMLST Scheme')",
                h5(p("cgMLST Scheme"), style = "color:white"),
                br()
              ),
              conditionalPanel(
                "input.include_analysis.includes('Comment')",
                textAreaInput(
                  inputId = "report_analysiscomment",
                  label = "Comments",
                  value = "Comments about cgMLST Analysis ...",
                  width = "100%",
                  height = NULL,
                  cols = NULL,
                  rows = NULL,
                  placeholder = NULL,
                  resize = "vertical"
                )
              )
            )
          ),
          column(width = 1),
          column(
            width = 2,
            align = "center",
            h3(p("Preselect Settings"), style = "color:white"),
            br(), br(), br(),
            uiOutput("selProfile"),
            br(),
            hr(),
            br(),
            textInput(
              inputId = "rep_profilename",
              label = "Save current as",
              width = "800%"
            ),
            br(),
            actionButton(
              inputId = "save_rep_profile",
              label = "Save New Profile"
            )
          )
        )
        
      )
      
    ) # End tabItems
    
  ) # End dashboardPage
  
) # end UI



################### Server ###################

server <- function(input, output, session) {
  
  # Database       -------------------------------------------------------
  DF1 <- reactiveValues()
  database <- reactiveValues()
  
  observe({
    database$available <- gsub("_", " ", basename(dir_ls(paste0(getwd(), "/Database"))))
    database$exist <- (length(dir_ls(paste0(getwd(), "/Database/"))) == 0)
    
    })
  
  observe({
  if (!database$exist){
    
    output$browse_what <- renderUI(
      radioGroupButtons(
        inputId = "browse_what",
        choices = c("Entries", "Loci"),
        checkIcon = list(yes = icon("check"))
      )
    )
    
    output$scheme_info <- renderUI(
      addSpinner(
        tableOutput("scheme_info"),
        spin = "dots", 
        color = "#ffffff")
    )
    
  if(!class(DF1$schemeinfo) == "NULL") {
    output$scheme_info <- renderTable({
      DF1$schemeinfo
    })
  } else {output$scheme_info <- NULL}
 
  output$db_loci <- renderDataTable(
    read.csv(paste0(getwd(), "/Database/", gsub(" ", "_", input$scheme_db), "/targets.csv"),
             header = TRUE, sep = "\t", row.names = NULL,
             colClasses = c("NULL", "character", "character", "integer", 
                            "integer", "character", "integer", "NULL")), 
    options = list(pageLength = 10,
                   columnDefs = list(list(searchable = FALSE, targets = "_all")))
  )
  
  if(!any(grepl("Typing.rds", dir_ls(paste0(getwd(), "/Database/", gsub(" ", "_", input$scheme_db)))))) {
    output$db_no_entries <- renderUI(
      HTML(paste("<span style='color: white;'>", "No Entries for this scheme available.",
                 "Type a genome in the section <strong>Allelic Typing</strong> and add the result to the local database.", sep = '<br/>')))
    output$db_entries <- NULL
    output$edit_index <- NULL
    output$edit_scheme_d <- NULL
    output$edit_entries <- NULL
    
    } else if (!class(DF1$data) == "NULL") {
      
    output$db_entries <- renderDataTable({
      select(DF1$data, 1:11)},
      options = list(pageLength = 25,
                     columnDefs = list(list(searchable = FALSE, targets = "_all")))
                
      )
    
    output$db_no_entries <- NULL 
    
    # Edit Database Elements    
    
    output$edit_index <- renderUI({
      typing <- DF1$data
      numericInput(
        "edit_index",
        label = "",
        value = 1,
        min = 1,
        step = 1,
        max = nrow(typing))
    })
    
    output$edit_scheme_d <- renderUI({
      typing <- DF1$data
      textInput(
        "edit_assembly_id",
        label = "",
        value = typing$`Assembly ID`[input$edit_index]
      )
    })
    
    }
  
  output$no_db <- NULL
    
  } else if (database$exist) {
    output$no_db <- renderUI(
      HTML(paste("<span style='color: white;'>", "No local Schemes or Entries available.",
                 "Download a cgMLST Scheme in the Section <strong>Add Scheme</strong>.", sep = '<br/>'))
    )
  }
    
  })
  
  # Reload database
  observeEvent(input$load, {
    
    if(any(grepl("Typing.rds", dir_ls(paste0(getwd(), "/Database/", gsub(" ", "_", input$scheme_db)))))) {
    Data <- readRDS(paste0(getwd(), "/Database/", gsub(" ", "_", input$scheme_db), "/Typing.rds"))
    typing <- Data[["Typing"]]
    typing <- select(typing, 1:11)
    DF1$data <- typing
    output$edit_entries <- renderUI(
      h4(p("Edit Entries"), style = "color:white")
    )
    }
    
    schemeinfo <- read_html(paste0(getwd(), "/Database/", gsub(" ", "_", input$scheme_db), "/scheme_info.html")) %>%
      html_table(header = FALSE) %>%
      as.data.frame(stringsAsFactors = FALSE)
    names(schemeinfo) <- NULL
    DF1$schemeinfo <- schemeinfo
  })
  
  # Render UI (dependent on database availability)
  
  observe({
    if(!database$exist) {
      output$scheme_db <- renderUI(
        prettyRadioButtons(
          "scheme_db",
          choices = database$available,
          label = "Local schemes")
      )
      
      } 
  })
  
  # Save Edits Button
  
  observeEvent(input$edit, {
    Data <- readRDS(paste0(getwd(), "/Database/", gsub(" ", "_", input$scheme_db), "/Typing.rds"))
    typing <- Data[["Typing"]]
    typing$`Assembly ID`[input$edit_index] <- input$edit_assembly_id
    DF1$data <- typing
    Data[["Typing"]] <- typing
    saveRDS(Data, paste0(getwd(), "/Database/", gsub(" ", "_", input$scheme_db), "/Typing.rds"))
  })
  
  
  
  
  # Download cgMLST       -------------------------------------------------------
  
  myReactives <- reactiveValues()
  
  observe(
    if(input$select_cgmlst == "Acinetobacter baumanii") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/3956907/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/3956907/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/3956907/locus/?content-type=csv"
      folder_name <<- "Acinetobacter_baumanii"
    } else if (input$select_cgmlst == "Bacillus anthracis") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/19008694/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/19008694/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/19008694/locus/?content-type=csv"
      folder_name <<- "Bacillus_anthracis"
    } else if (input$select_cgmlst == "Bordetella pertussis") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/29412358/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/29412358/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/29412358/locus/?content-type=csv"
      folder_name <<- "Bordetella_pertussis"
    } else if (input$select_cgmlst == "Brucella melitensis") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/6398355/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/6398355/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema//6398355/locus/?content-type=csv"
      folder_name <<- "Brucella_melitensis"
    } else if (input$select_cgmlst == "Brucella spp.") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/24062474/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/24062474/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/24062474/locus/?content-type=csv"
      folder_name <<- "Brucella_spp"
    } else if (input$select_cgmlst == "Burkholderia mallei (FLI)") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/23721348/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/23721348/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/23721348/locus/?content-type=csv"
      folder_name <<- "Burkholderia_mallei_FLI"
    } else if (input$select_cgmlst == "Burkholderia mallei (RKI)") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/23643739/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/23643739/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/23643739/locus/?content-type=csv"
      folder_name <<- "Burkholderia_mallei_RKI"
    } else if (input$select_cgmlst == "Burkholderia pseudomallei") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/18876117/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/18876117/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/18876117/locus/?content-type=csv"
      folder_name <<- "Burkholderia_pseudomallei"
    } else if (input$select_cgmlst == "Campylobacter jejuni/coli") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/145039/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/145039/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/145039/locus/?content-type=csv"
      folder_name <<- "Campylobacter_jejuni_coli"
    } else if (input$select_cgmlst == "Clostridioides difficile") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/12556067/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/12556067/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/12556067/locus/?content-type=csv"
      folder_name <<- "Clostridioides_difficile"
    } else if (input$select_cgmlst == "Clostridium perfringens") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/15017225/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/15017225/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/15017225956907/locus/?content-type=csv"
      folder_name <<- "Clostridium_perfringens"
    } else if (input$select_cgmlst == "Corynebacterium diphtheriae") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/30589266/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/30589266/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/30589266/locus/?content-type=csv"
      folder_name <<- "Corynebacterium_diphtheriae"
    } else if (input$select_cgmlst == "Cronobacter sakazakii/malonaticus") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/29420227/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/29420227/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/29420227/locus/?content-type=csv"
      folder_name <<- "Cronobacter_sakazakii_malonaticus"
    } else if (input$select_cgmlst == "Enterococcus faecalis") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/3887469/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/3887469/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/3887469/locus/?content-type=csv"
      folder_name <<- "Enterococcus_faecalis"
    } else if (input$select_cgmlst == "Enterococcus faecium") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/991893/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/991893/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/991893/locus/?content-type=csv"
      folder_name <<- "Enterococcus_faecium"
    } else if (input$select_cgmlst == "Escherichia coli") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/5064703/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/5064703/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/5064703/locus/?content-type=csv"
      folder_name <<- "Escherichia_coli"
    } else if (input$select_cgmlst == "Francisella tularensis") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/260204/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/260204/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/260204/locus/?content-type=csv"
      folder_name <<- "Francisella_tularensis"
    } else if (input$select_cgmlst == "Klebsiella pneumoniae/variicola/quasipneumoniae") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/2187931/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/2187931/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/2187931/locus/?content-type=csv"
      folder_name <<- "K_pneumoniae_variicola_quasipneumoniae"
    } else if (input$select_cgmlst == "Legionella pneumophila") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/1025099/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/1025099/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/1025099/locus/?content-type=csv"
      folder_name <<- "Legionella_pneumophila"
    } else if (input$select_cgmlst == "Listeria monocytogenes") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/690488/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/690488/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/690488/locus/?content-type=csv"
      folder_name <<- "Listeria_monocytogenes"
    } else if (input$select_cgmlst == "Mycobacterium tuberculosis/bovis/africanum/canettii") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/741110/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/741110/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/741110/locus/?content-type=csv"
      folder_name <<- "M_tuberculosis_bovis_africanum_canettii"
    } else if (input$select_cgmlst == "Mycobacteroides abscessus") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/22602285/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/22602285/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/22602285/locus/?content-type=csv"
      folder_name <<- "Mycobacteroides_abscessus"
    } else if (input$select_cgmlst == "Mycoplasma gallisepticum") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/6402012/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/6402012/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/6402012/locus/?content-type=csv"
      folder_name <<- "Mycoplasma_gallisepticum"
    } else if (input$select_cgmlst == "Paenibacillus larvae") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/17414003/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/17414003/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/17414003/locus/?content-type=csv"
      folder_name <<- "Paenibacillus_larvae"
    } else if (input$select_cgmlst == "Pseudomonas aeruginosa") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/16115339/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/16115339/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/16115339/locus/?content-type=csv"
      folder_name <<- "Pseudomonas_aeruginosa"
    } else if (input$select_cgmlst == "Salmonella enterica") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/4792159/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/4792159/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/4792159/locus/?content-type=csv"
      folder_name <<- "Salmonella_enterica"
    } else if (input$select_cgmlst == "Serratia marcescens") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/24616475/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/24616475/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/24616475/locus/?content-type=csv"
      folder_name <<- "Serratia_marcescens"
    } else if (input$select_cgmlst == "Staphylococcus aureus") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/141106/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/141106/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/141106/locus/?content-type=csv"
      folder_name <<- "Staphylococcus_aureus"
    } else if (input$select_cgmlst == "Staphylococcus capitis") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/26824796/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/26824796/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/26824796/locus/?content-type=csv"
      folder_name <<- "Staphylococcus_capitis"
    } else if (input$select_cgmlst == "Streptococcus pyogenes") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/30585223/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/30585223/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/30585223/locus/?content-type=csv"
      folder_name <<- "Streptococcus_pyogenes"
    } 
    
  )
  
  observeEvent(input$download_cgMLST, {
    
    myReactives$target_table <- NULL
    
    # Download Loci Fasta Files
    download(link_cgmlst, dest="dataset.zip", mode="wb") 
    
    unzip(zipfile = "dataset.zip", 
          exdir = paste0(getwd(), "/Database/", folder_name, paste0("/", folder_name, "_alleles")))
    
    unlink("dataset.zip")
    
    # Download Scheme Info
    download(myReactives$link_scheme, dest=paste0(getwd(), "/Database/", folder_name, "/scheme_info.html"), mode="wb")
    
    # Download Loci Info
    download(link_targets, dest=paste0(getwd(), "/Database/", folder_name, "/targets.csv"), mode="wb") 
    
    # Send downloaded scheme to datbase browser overview
    database$available <- gsub("_", " ", basename(dir_ls(paste0(getwd(), "/Database"))))
    
    myReactives$target_table <- read.csv(paste0(getwd(),"/Database/", folder_name, "/targets.csv"),
                                         header = TRUE, sep = "\t", row.names = NULL,
                                         colClasses = c("NULL", "character", "character", "integer", 
                                                        "integer", "character", "integer", "NULL"))
    
    database$exist <- (length(dir_ls(paste0(getwd(), "/Database/"))) == 0)
    
    show_toast(
      title = "Download successful",
      type = "success",
      position = "top-end",
      width = "400px",
      timer = 6000)
    
  })
  
  # Download Target Info (CSV Table)
  
  
  output$cgmlst_scheme <- renderTable({
    scheme_overview <- read_html(myReactives$link_scheme) %>%
      html_table(header = FALSE) %>%
      as.data.frame(stringsAsFactors = FALSE)
    names(scheme_overview) <- NULL
    scheme_overview
  })
  
  # Display Target Table  --------------------------------------------------
  
  output$cgmlst_targets <- renderDataTable(
    {targets_overview <- myReactives$target_table}, 
    options = list(pageLength = 10,
                   columnDefs = list(list(searchable = FALSE, targets = "_all")))
  )
  
  
  # Visualization   ---------------------------------------------------------
  
  # Render local database choice input
  observe({
    if(!database$exist) {
      output$scheme_vis <- renderUI(
        prettyRadioButtons(
          "scheme_vis",
          choices = database$available,
          label = "Local schemes")
      )
    } 
  })
  
  observeEvent(input$create_tree, {
    # Load local database
    Database <- readRDS(paste0(getwd(), "/Database/", gsub(" ", "_", input$scheme_vis), "/Typing.rds"))
    
    allelic_profile <- dplyr::select(Database$Typing, -(1:10))
    
    metadata <- dplyr::select(Database$Typing, 1:8)
    
    colnames(metadata) <- c("assembly_id", "assembly_name", "scheme", "isolation_date",
                            "host", "country", "city", "typing_date")
    
    metadata$taxa <- rownames(metadata)
    
    metadata <- relocate(metadata, taxa)
    
    # Make metadata colnames available
    metadata_cols <<- colnames(metadata)
    
    # Calculate distance matrix
    dist_matrix <- dist(allelic_profile)
    
    if (input$tree_algo == "Neighbour-Joining") {
      
      # Create phylogenetic tree
      eigen_tree <- ape::nj(dist_matrix)
      
      # Visualize the tree with metadata annotations
      ggtree(eigen_tree, layout = "daylight") %<+% metadata + 
        geom_tiplab(aes(label = assembly_id), offset = 2) +
        geom_label(aes(x=branch, label=host), fill='lightgreen', nudge_x = -1) +
        geom_label(aes(x=branch, label= country), fill='lightblue', nudge_x = 2) 
      
      output$tree_local <- renderPlot({
        
        as.ggplot(
          ggtree(
            eigen_tree,
            aes(color = I(color_r())),
            layout = input$layout_r) %<+% metadata + 
            geom_tiplab(aes(label = assembly_id), offset = 2) +
            #label() +
            geom_label(aes(x=branch, label=host), fill='lightgreen', nudge_x = -1) +
            geom_label(aes(x=branch, label= country), fill='lightblue', nudge_x = 2) +
            revx_r() +
            revy_r() +
            treescale_x_r() +
            tip_r() +
            label_r() +
            node_r() +
            theme(plot.background = element_rect(fill = b_color_r(),
                                                 color = b_color_r()),
                  panel.background = element_rect(fill = b_color_r(),
                                                  color = b_color_r())
            ), 
          angle = input$rotate_r)
      })
      
    } else {
      
      output$tree_local <- renderPlot({
        plot(mst(dist_matrix), graph = graph_mst())
      })
    }
    
  })
  
  
  # Set Minimum-Spanning Tree Appearance
  graph_mst <- reactive({input$graph_mst})
  
  label <- reactive({
    geom_label(aes(x=branch, label=input$label_select), fill='lightgreen') 
  })
  
  # Reverse X Scale ---------------------------------------------------------
  
  
  revx1 <- reactive({
    if(input$rev_x_axis1 == FALSE) {
      NULL
    } else if (input$rev_x_axis1 == TRUE) {
      scale_x_reverse()
    }
  })
  revx2 <- reactive({
    if(input$rev_x_axis2 == FALSE) {
      NULL
    } else if (input$rev_x_axis2 == TRUE) {
      scale_x_reverse()
    }
  })
  revx3 <- reactive({
    if(input$rev_x_axis3 == FALSE) {
      NULL
    } else if (input$rev_x_axis3 == TRUE) {
      scale_x_reverse()
    }
  })
  revx4 <- reactive({
    if(input$rev_x_axis4 == FALSE) {
      NULL
    } else if (input$rev_x_axis4 == TRUE) {
      scale_x_reverse()
    }
  })
  revx_r <- reactive({
    if(input$rev_x_axis_r == FALSE) {
      NULL
    } else if (input$rev_x_axis_r == TRUE) {
      scale_x_reverse()
    }
  })
  
  # Reverse Y Scale ---------------------------------------------------------
  
  
  # Reverse Y Scale
  revy1 <- reactive({
    if(input$rev_y_axis1 == FALSE) {
      NULL
    } else if (input$rev_y_axis1 == TRUE) {
      scale_y_reverse()
    }
  })
  
  revy2 <- reactive({
    if(input$rev_y_axis2 == FALSE) {
      NULL
    } else if (input$rev_y_axis2 == TRUE) {
      scale_y_reverse()
    }
  })
  
  revy3 <- reactive({
    if(input$rev_y_axis3 == FALSE) {
      NULL
    } else if (input$rev_y_axis3 == TRUE) {
      scale_y_reverse()
    }
  })
  
  revy4 <- reactive({
    if(input$rev_y_axis4 == FALSE) {
      NULL
    } else if (input$rev_y_axis4 == TRUE) {
      scale_y_reverse()
    }
  })
  
  revy_r <- reactive({
    if(input$rev_y_axis_r == FALSE) {
      NULL
    } else if (input$rev_y_axis_r == TRUE) {
      scale_y_reverse()
    }
  })
  
  
  # Treescale ---------------------------------------------------------------
  
  
  treescale_x1 <- reactive({
    if(input$scale1 == 1 & input$show_scale1 == TRUE) {
      
      geom_treescale(color = color_scale1(),
                     y = y_scale1(),
                     x = x_scale1(),
                     linesize = line_scale1(),
                     fontsize = text_scale1(),
                     width = width_scale1())
      
    } else if (input$scale1 == 2 & input$show_scale1 == TRUE) {
      
      theme_tree2()
      
    } else if (input$show_scale1 == FALSE) {
      
      NULL
      
    }
  })
  
  treescale_x2 <- reactive({
    if(input$scale2 == 1 & input$show_scale2 == TRUE) {
      
      geom_treescale(color = color_scale2(),
                     y = y_scale2(),
                     x = x_scale2(),
                     linesize = line_scale2(),
                     fontsize = text_scale2(),
                     width = width_scale2())
      
    } else if (input$scale2 == 2 & input$show_scale2 == TRUE) {
      
      theme_tree2()
      
    } else if (input$show_scale2 == FALSE) {
      
      NULL
      
    }
  })
  
  treescale_x3 <- reactive({
    if(input$scale3 == 1 & input$show_scale3 == TRUE) {
      
      geom_treescale(color = color_scale3(),
                     y = y_scale3(),
                     x = x_scale3(),
                     linesize = line_scale3(),
                     fontsize = text_scale3(),
                     width = width_scale3())
      
    } else if (input$scale3 == 2 & input$show_scale3 == TRUE) {
      
      theme_tree2()
      
    } else if (input$show_scale3 == FALSE) {
      
      NULL
      
    }
  })
  
  treescale_x4 <- reactive({
    if(input$scale4 == 1 & input$show_scale4 == TRUE) {
      
      geom_treescale(color = color_scale4(),
                     y = y_scale4(),
                     x = x_scale4(),
                     linesize = line_scale4(),
                     fontsize = text_scale4(),
                     width = width_scale4())
      
    } else if (input$scale4 == 2 & input$show_scale4 == TRUE) {
      
      theme_tree2()
      
    } else if (input$show_scale4 == FALSE) {
      
      NULL
      
    }
  })
  
  treescale_x_r <- reactive({
    if(input$scale_r == 1 & input$show_scale_r == TRUE) {
      
      geom_treescale(color = color_scale_r(),
                     y = y_scale_r(),
                     x = x_scale_r(),
                     linesize = line_scale_r(),
                     fontsize = text_scale_r(),
                     width = width_scale_r())
      
    } else if (input$scale_r == 2 & input$show_scale_r == TRUE) {
      
      theme_tree2()
      
    } else if (input$show_scale_r == FALSE) {
      
      NULL
      
    }
  })
  
  
  # Scale Color -------------------------------------------------------------
  
  
  color_scale1 <- reactive({input$scale_color1})
  color_scale2 <- reactive({input$scale_color2})
  color_scale3 <- reactive({input$scale_color3})
  color_scale4 <- reactive({input$scale_color4})
  color_scale_r <- reactive({input$scale_color_r})
  
  
  # Scale Y Position --------------------------------------------------------
  
  
  y_scale1 <- reactive({input$scale_y1})
  y_scale2 <- reactive({input$scale_y2})
  y_scale3 <- reactive({input$scale_y3})
  y_scale4 <- reactive({input$scale_y4})
  y_scale_r <- reactive({input$scale_y_r})
  
  
  # Scale X Position --------------------------------------------------------
  
  
  x_scale1 <- reactive({input$scale_x1})
  x_scale2 <- reactive({input$scale_x2})
  x_scale3 <- reactive({input$scale_x3})
  x_scale4 <- reactive({input$scale_x4})
  x_scale_r <- reactive({input$scale_x_r})
  
  
  # Scale Line Size ---------------------------------------------------------
  
  
  line_scale1 <- reactive({input$scale_line1})
  line_scale2 <- reactive({input$scale_line2})
  line_scale3 <- reactive({input$scale_line3})
  line_scale4 <- reactive({input$scale_line3})
  line_scale_r <- reactive({input$scale_line_r})
  
  
  # Scale Text Size ---------------------------------------------------------
  
  
  text_scale1 <- reactive({input$scale_text1})
  text_scale2 <- reactive({input$scale_text2})
  text_scale3 <- reactive({input$scale_text3})
  text_scale4 <- reactive({input$scale_text4})
  text_scale_r <- reactive({input$scale_text_r})
  
  
  # Scale Line Width --------------------------------------------------------
  
  
  width_scale1 <- reactive({input$scale_width1})
  width_scale2 <- reactive({input$scale_width2})
  width_scale3 <- reactive({input$scale_width3})
  width_scale4 <- reactive({input$scale_width4})
  width_scale_r <- reactive({input$scale_width_r})
  
  
  # Tips Highlight ----------------------------------------------------------
  
  
  tip1 <- reactive({
    
    if(input$tip_highlight1 == FALSE) {
      
      NULL
      
    } else if (input$tip_highlight1 == TRUE) {
      
      geom_tippoint(shape = as.numeric(input$tip_shape1),
                    color = input$tip_color1,
                    size = input$tip_size1,
                    alpha = input$tip_alpha1)
      
    }
    
  })
  
  tip2 <- reactive({
    
    if(input$tip_highlight2 == FALSE) {
      
      NULL
      
    } else if (input$tip_highlight2 == TRUE) {
      
      geom_tippoint(shape = as.numeric(input$tip_shape2),
                    color = input$tip_color2,
                    size = input$tip_size2,
                    alpha = input$tip_alpha2)
      
    }
    
  })
  
  tip3 <- reactive({
    
    if(input$tip_highlight3 == FALSE) {
      
      NULL
      
    } else if (input$tip_highlight3 == TRUE) {
      
      geom_tippoint(shape = as.numeric(input$tip_shape3),
                    color = input$tip_color3,
                    size = input$tip_size3,
                    alpha = input$tip_alpha3)
      
    }
    
  })
  
  tip4 <- reactive({
    
    if(input$tip_highlight4 == FALSE) {
      
      NULL
      
    } else if (input$tip_highlight4 == TRUE) {
      
      geom_tippoint(shape =  as.numeric(input$tip_shape4),
                    color = input$tip_color4,
                    size = input$tip_size4,
                    alpha = input$tip_alpha4)
      
    }
    
  })
  
  tip_r <- reactive({
    
    if(input$tip_highlight_r == FALSE) {
      
      NULL
      
    } else if (input$tip_highlight_r == TRUE) {
      
      geom_tippoint(shape =  as.numeric(input$tip_shape_r),
                    color = input$tip_color_r,
                    size = input$tip_size_r,
                    alpha = input$tip_alpha_r)
      
    }
    
  })
  
  #highlight nodes
  node1 <- reactive({
    
    if(input$node_highlight1 == FALSE) {
      
      NULL
      
    } else if (input$node_highlight1 == TRUE) {
      
      geom_nodepoint(shape = as.numeric(input$node_shape1),
                     color = input$node_color1,
                     size = input$node_size1,
                     alpha = input$node_alpha1)
      
    }
    
  })
  
  node2 <- reactive({
    
    if(input$node_highlight2 == FALSE) {
      
      NULL
      
    } else if (input$node_highlight2 == TRUE) {
      
      geom_nodepoint(shape = as.numeric(input$node_shape2),
                     color = input$node_color2,
                     size = input$node_size2,
                     alpha = input$node_alpha2)
      
    }
    
  })
  
  node3 <- reactive({
    
    if(input$node_highlight3 == FALSE) {
      
      NULL
      
    } else if (input$node_highlight3 == TRUE) {
      
      geom_nodepoint(shape = as.numeric(input$node_shape3),
                     color = input$node_color3,
                     size = input$node_size3,
                     alpha = input$node_alpha3)
      
    }
    
  })
  
  node4 <- reactive({
    
    if(input$node_highlight4 == FALSE) {
      
      NULL
      
    } else if (input$node_highlight4 == TRUE) {
      
      geom_nodepoint(shape = as.numeric(input$node_shape4),
                     color = input$node_color4,
                     size = input$node_size4,
                     alpha = input$node_alpha4)
      
    }
    
  })
  
  node_r <- reactive({
    
    if(input$node_highlight_r == FALSE) {
      
      NULL
      
    } else if (input$node_highlight_r == TRUE) {
      
      geom_nodepoint(shape = as.numeric(input$node_shape_r),
                     color = input$node_color_r,
                     size = input$node_size_r,
                     alpha = input$node_alpha_r)
      
    }
    
  })
  
  # Tip Labels
  label1 <- reactive ({
    
    if(input$label1 == FALSE) {
      
      NULL
      
    } else if (input$label1) {
      
      geom_tiplab(size = input$label_size1, 
                  color = input$label_color1,
                  mapping = aes(angle = angle1()))
      
    }
    
  })
  
  label2 <- reactive ({
    
    if(input$label2 == FALSE) {
      
      NULL
      
    } else if (input$label2) {
      
      geom_tiplab(size = input$label_size2, 
                  color = input$label_color2,
                  mapping = aes(angle = angle2()))
      
    }
    
  })
  
  label3 <- reactive ({
    
    if(input$label3 == FALSE) {
      
      NULL
      
    } else if (input$label3) {
      
      geom_tiplab(size = input$label_size3, 
                  color = input$label_color3,
                  mapping = aes(angle = angle3()))
      
    }
    
  })
  
  label4 <- reactive ({
    
    if(input$label4 == FALSE) {
      
      NULL
      
    } else if (input$label4) {
      
      geom_tiplab(size = input$label_size4, 
                  color = input$label_color4,
                  mapping = aes(angle = angle4()))
      
    }
    
  })
  
  label_r <- reactive ({
    
    if(input$label_r == FALSE) {
      
      NULL
      
    } else if (input$label_r) {
      
      geom_tiplab(size = input$label_size_r, 
                  color = input$label_color_r,
                  mapping = aes(angle = angle_r()))
      
    }
    
  })
  
  # Label Angle
  angle1 <- reactive({
    
    if(input$label_angle1 == FALSE) {
      
      NULL
      
    } else {
      
      angle
      
    }
    
  })
  
  angle2 <- reactive({
    
    if(input$label_angle2 == FALSE) {
      
      NULL
      
    } else {
      
      angle
      
    }
    
  })
  
  angle3 <- reactive({
    
    if(input$label_angle3 == FALSE) {
      
      NULL
      
    } else {
      
      angle
      
    }
    
  })
  
  angle4 <- reactive({
    
    if(input$label_angle4 == FALSE) {
      
      NULL
      
    } else {
      
      angle
      
    }
    
  })
  
  angle_r <- reactive({
    
    if(input$label_angle_r == FALSE) {
      
      NULL
      
    } else {
      
      angle
      
    }
    
  })
  
  # Branch Color
  color1 <- reactive({input$branch_color1})
  color2 <- reactive({input$branch_color2})
  color3 <- reactive({input$branch_color3})
  color4 <- reactive({input$branch_color4})
  color_r <- reactive({input$branch_color_r})
  
  # Background Color
  b_color1 <- reactive({input$background_color1})
  b_color2 <- reactive({input$background_color2})
  b_color3 <- reactive({input$background_color3})
  b_color4 <- reactive({input$background_color4})
  b_color_r <- reactive({input$background_color_r})
  
  
  # Generating Plots --------------------------------------------------------
  
  # Generate Random Plot
  randomtree <- reactive({
    
    rtree <- rtree(input$ntree)
    
  })
  
  observeEvent(input$random_tree, 
               {output$tree_random <- renderPlot({
                 
                 as.ggplot(ggtree(tr = randomtree(),
                                  aes(color = I(color_r())),
                                  layout = input$layout_r) +
                             revx_r() +
                             revy_r() +
                             treescale_x_r() +
                             tip_r() +
                             label_r() +
                             node_r() +
                             theme(plot.background = element_rect(fill = b_color_r(),
                                                                  color = b_color_r()),
                                   panel.background = element_rect(fill = b_color_r(),
                                                                   color = b_color_r())), 
                           angle = input$rotate_r)
               })
               })
  
  
  
  
  # Save Report -------------------------------------------------------------
  
  
  # Create a reactiveValues to store selected elements and their content
  elements_data <- reactiveValues()
  
  observe({
    selected_general <<- input$include_general
    selected_sampleinfo <<- input$include_sampleinfo
    selected_sequencing <<- input$include_sequencing
    selected_analysis <<- input$include_analysis
    
    # Store content for each selected element in reactiveValues
    if ('Analysis Date' %in% selected_general) {
      elements_data$general_date <- as.character(input$report_date)
    } else {elements_data$general_date <- NULL}
    
    if ('Author' %in% selected_general) {
      elements_data$general_author <- input$author
    } else {elements_data$general_author <- NULL}
    
    if ('Experiment Info' %in% selected_general) {
      elements_data$general_com <- input$exp_info
    } else {elements_data$general_com <- NULL}
    
    if ('Sampling Date' %in% selected_sampleinfo) {
      elements_data$sample_date <- as.character(input$report_sampledate)
    } else {elements_data$sample_date <- NULL}
    
    if ('Sampling Location' %in% selected_sampleinfo) {
      elements_data$sample_loc <- input$sample_location
    } else {elements_data$sample_loc <- NULL}
    
    if ('Taken by (Name)' %in% selected_sampleinfo) {
      elements_data$sample_op <- input$sampled_by
    } else {elements_data$sample_op <- NULL}
    
    if ('Comment' %in% selected_sampleinfo) {
      elements_data$sample_com <- input$sample_info
    } else {elements_data$sample_com <- NULL}
    
    if ('Device' %in% selected_sequencing) {
      elements_data$seq_device <- input$select_device
    } else {elements_data$seq_device <- NULL}
    
    if ('Flow Cell' %in% selected_sequencing) {
      elements_data$seq_flowcell <- input$select_flowcell
    } else {elements_data$seq_flowcell <- NULL}
    
    if ('Run Start' %in% selected_sequencing) {
      elements_data$seq_start <- as.character(input$report_runstart)
    } else {elements_data$seq_start <- NULL}
    
    if ('Run Finished' %in% selected_sequencing) {
      elements_data$seq_end <- as.character(input$report_runfinished)
    } else {elements_data$seq_end <- NULL}
    
    if ('Operator' %in% selected_sequencing) {
      elements_data$seq_op <- input$report_seqoperator
    } else {elements_data$seq_op <- NULL}
    
    if ('Comment' %in% selected_sequencing) {
      elements_data$seq_com <- input$report_seqcomment
    } else {elements_data$seq_com <- NULL}
    
    if ('Analysis Date' %in% selected_analysis) {
      elements_data$ana_date <- input$report_analysisdate
    } else {elements_data$ana_date <- NULL}
    
    if ('Comment' %in% selected_analysis) {
      elements_data$ana_com <- input$report_analysiscomment
    } else {elements_data$ana_com <- NULL}
    
  })
  
  # Generate the RDS file when the "Save Report" button is clicked
  observeEvent(input$save_report, {
    
    
    # Filter and save data for the selected elements
    selected_data <- list(
      general_date = elements_data$general_date,
      general_author = elements_data$general_author,
      general_com = elements_data$general_com,
      sample_date = elements_data$sample_date,
      sample_loc = elements_data$sample_loc,
      sample_op = elements_data$sample_op,
      sample_com = elements_data$sample_com,
      seq_device = elements_data$seq_device,
      seq_flowcell = elements_data$seq_flowcell,
      seq_start = elements_data$seq_start,
      seq_end = elements_data$seq_end,
      seq_op = elements_data$seq_op,
      seq_com = elements_data$seq_com,
      ana_date = elements_data$ana_date,
      ana_com = elements_data$ana_com
    )
    
    # Save data to an RDS file if any elements were selected
    if (length(selected_data) > 0) {
      saveRDS(selected_data, file = "selected_elements.rds")
    }
    
    rmarkdown::render("Report.Rmd")
    
  })
  
  # Save Report Profile ----------------------------------------------------  
  
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
      saveRDS(report_profile, file = paste0(getwd(), "/rep_profiles/", input$rep_profilename, ".rds"))
    }
    
    rep_profile$profile_names <- list.files(paste0(getwd(),"/rep_profiles"), full.names = TRUE)
  })
  
  # Load Report Profile ----------------------------------------------------
  
  rep_profile <- reactiveValues()
  
  observe(
    rep_profile$profile_names <- list.files(paste0(getwd(),"/rep_profiles"), full.names = TRUE)
    
  )
  
  output$selProfile <- renderUI(
    selectInput(
      inputId = "sel_rep_profile",
      label = "Select Report Profile",
      choices = append(c("None"), gsub(".*/(.*).rds", "\\1", rep_profile$profile_names))
    )
  )
  
  # General Tickbox
  general_selected <- reactive({
    if(input$sel_rep_profile %in% "None") {
      c("Analysis Date", "Author")
    } else {
      readRDS(paste0(getwd(), "/rep_profiles/", input$sel_rep_profile, ".rds"))[[1]]
    }
  })
  
  output$include_general <- renderUI(
    checkboxGroupInput(
      inputId = "include_general",
      label = "General",
      choices = c("Analysis Date", "Author", "Experiment Info"),
      selected = general_selected()
    )
  )
  
  # Sample Info Tickbox
  sampleinfo_selected <- reactive({
    if(input$sel_rep_profile %in% "None") {
      c("Sampling Date", "Sampling Location") 
    } else {
      readRDS(paste0(getwd(), "/rep_profiles/", input$sel_rep_profile, ".rds"))[[2]]
    }
  })
  
  output$include_sampleinfo <- renderUI(
    checkboxGroupInput(
      inputId = "include_sampleinfo",
      label = "Sample",
      choices = c("Sampling Date", "Sampling Location", "Taken by (Name)", "Comment"),
      selected = sampleinfo_selected()
    )
  )
  
  # Sequencing Tickbox
  sequencing_selected <- reactive({
    if(input$sel_rep_profile %in% "None") {
      c("Device", "Fow Cell", "Operator") 
    } else {
      readRDS(paste0(getwd(), "/rep_profiles/", input$sel_rep_profile, ".rds"))[[3]]
    }
  })
  
  output$include_sequencing <- renderUI(
    checkboxGroupInput(
      inputId = "include_sequencing",
      label = "Sequencing",
      choices = c("Device", "Flow Cell", "Run Start", "Run Finished", 
                  "Operator", "Output Size", "Comment"),
      selected = sequencing_selected()
    )
  )
  
  # Analysis Tickbox
  analysis_selected <- reactive({
    if(input$sel_rep_profile %in% "None") {
      c("Analysis Date", "cgMLST Scheme") 
    } else {
      readRDS(paste0(getwd(), "/rep_profiles/", input$sel_rep_profile, ".rds"))[[4]]
    }
  })
  
  output$include_analysis <- renderUI(
    checkboxGroupInput(
      inputId = "include_analysis",
      label = "Analysis",
      choices = c("Analysis Date", "Assembly Parameters", "cgMLST Scheme", "Comment"),
      selected = analysis_selected()
    )
  )
  
  
  
  # Initiate Typing  ----------------------------------------------------
  
  # Render Scheme Selector 
  
  observe({
    if(!database$exist) {
      output$cgmlst_typing <- renderUI(
        prettyRadioButtons(
          "cgmlst_typing",
          choices = database$available,
          label = "Local schemes")
      )
    } 
  })
  
  # Get genome datapath
  
  volumes = getVolumes()
  
  
  observe({  
    shinyFileChoose(input, "genome_file", roots = volumes, session = session)
    selected_genome <<- parseFilePaths(volumes, input$genome_file)
    
    if (!nrow(selected_genome) > 0) {
      output$genome_path <- renderUI(
        HTML(paste("<span style='color: white;'>", "No file selected."))
      )
      output$arrow_start <- NULL
    } else if (nrow(selected_genome) > 0) {
      output$genome_path <- renderUI(
        HTML(paste("<span style='color: white;'>", as.character(selected_genome$name)))
      )
      output$selected_scheme <- renderUI({
        HTML(paste("<span style='color: white;'>", "Typing by <strong>", input$cgmlst_typing, "</strong> scheme."))
      })
      output$typing_start <- renderUI(
        actionButton(
          inputId = "typing_start",
          label = "Start",
          width = "100px"
        )
      )
      output$arrow_start <- renderUI(HTML('<i class="fa-solid fa-arrow-down fa-beat-fade fa-xl" style="color: #ffffff;"></i>'))
      
    } 
    
  })
  
  
  #################### Run KMA  index script #########################
  
  
  observeEvent(input$typing_start, {
    
    
    selected_organism <<- input$cgmlst_typing
    
    # Locate folder containing cgMLST scheme
    
    search_string <- paste0(gsub(" ", "_", selected_organism), "_alleles")
    
    scheme_folders <- dir_ls(paste0(getwd(), "/Database/", gsub(" ", "_", selected_organism)))
    
    if(any(grepl(search_string, scheme_folders))) {
      
      # KMA initiate index
      
      scheme_select <<- as.character(scheme_folders[which(grepl(search_string, scheme_folders))])
      
      show_toast(
        title = "Typing Initiated",
        type = "success",
        position = "bottom-end",
        width = "400px",
        timer = 12000)
      
      index_kma <- paste0(
        "#!/bin/bash\n",
        "database_name=", shQuote(selected_organism), "\n",
        "genome=", shQuote(selected_genome$datapath), "\n",
        '/home/marian/miniconda3/bin/kma index -i "$genome" -o "$database_name"'
      )
      
      # Specify the path to save the script
      index_kma_path <- paste0("/home/marian/Documents/Projects/Masterthesis", "/index_kma.sh")
      
      # Write the script to a file
      cat(index_kma, file = index_kma_path)
      
      # Make the script executable
      system(paste("chmod +x", index_kma_path))
      
      # Execute the script
      system(paste(index_kma_path))
      
      # KMA Run
      
      kma_run <- paste0(
        "#!/bin/bash\n",
        "database=", shQuote(selected_organism), "\n",
        "query_folder=", shQuote(paste0(getwd(), "/Database/", gsub(" ", "_", selected_organism), "/", search_string)), "\n",
        "tmp_dir=", shQuote(tempdir()), "\n",
        'mkdir $tmp_dir/results -p', "\n",
        'echo 0 > ', shQuote(paste0(getwd(), "/execute/progress.fifo")), "\n",
        'output_folder="$tmp_dir/results"', "\n",
        'count=0', "\n",
        'for query_file in "$query_folder"/*.fasta; do', "\n",
        'if [ -f "$query_file" ]; then', "\n",
        'query_filename=$(basename "$query_file")', "\n",
        'query_filename_noext="${query_filename%.*}"', "\n",
        'output_file="$output_folder/$query_filename_noext"', "\n",
        '/home/marian/miniconda3/bin/kma -i "$query_file" -o "$output_file" -t_db "$database" -nc -status', "\n",
        '((count++))', "\n",
        'echo $count > ', shQuote(paste0(getwd(), "/execute/progress.fifo")), "\n",
        'fi', "\n",
        'done'
      )
      
      
      # Specify the path to save the script
      kma_run_path <- paste0(getwd(), "/execute", "/kma_run.sh")
      
      # Write the script to a file
      cat(kma_run, file = kma_run_path)
      
      # Make the script executable
      system(paste("chmod +x", kma_run_path))
      
      # Execute the script
      system(paste(kma_run_path), wait = FALSE)
      
      Sys.sleep(1)
      
      progress <- 0
      
      scheme_loci <- list.files(path = scheme_select, full.names = TRUE)
      
      # Filter the files that have the ".fasta" extension
      scheme_loci_f <- scheme_loci[grep(".fasta$", scheme_loci, ignore.case = TRUE)]
      
      while (progress < length(scheme_loci_f)) {
        progress <- readLines(paste0(getwd(), "/execute", "/progress.fifo"))
        progress <- as.numeric(progress)
        progress_pct <- floor((as.numeric(progress)/length(scheme_loci_f))*100)
        updateProgressBar(session = session, id = "progress_bar", value = progress_pct, 
                          total = 100, title = paste0(as.character(progress),"/", length(scheme_loci_f)))
        Sys.sleep(2.5)
      }
      
    } else {
      
      show_alert(
        title = "Error",
        text = paste0("Folder containing cgMLST alleles not in working directory.", "\n", 
                      "Download cgMLST Scheme for selected Organism first."),
        type = "error"
      )
    }
    
    output$arrow_start <- NULL
    
    output$arrow_profile <- renderUI(HTML('<i class="fa-solid fa-arrow-down fa-beat-fade fa-xl" style="color: #ffffff;"></i>'))
    
    output$get_allele_profile <- renderUI(
      actionButton(
        "get_allele_profile",
        "Get Allelic Profile"
      )
    )  
  }
  )
  
 
  
  
  
  ############## Get Allelic Profile  ######################################
  
  observeEvent(input$get_allele_profile, {
    
    output$arrow_profile <- NULL
    
    # List all the .frag.gz files in the folder
    frag_files <- list.files(paste0(tempdir(), "/results"), pattern = "\\.frag\\.gz$", full.names = TRUE)
    
    # List to store data frames
    frag_data_list <<- list()
    
    # Initialize an empty vector to store the results
    allele_vector <<- integer(length(frag_files))
    
    for (i in 1:length(frag_files)) {
      # Extract the base filename without extension
      frag_filename <- gsub(".frag", "", tools::file_path_sans_ext(basename(frag_files[i])))
      
      # Check if the file is empty
      if (file.info(frag_files[i])$size < 100) {
        # Handle empty file: Insert NA in the allele_vector and create an empty data frame
        allele_vector[i] <<- NA
        frag_data <- data.frame(Score = numeric(0), Variant = character(0))
      } else {
        # Read the .frag.gz file into a data table
        frag_data <- fread(frag_files[i], sep = "\t", header = FALSE)
        
        # Extract the third, and seventh columns
        frag_data <- frag_data[, .(V3, V7)]
        
        # Find the row with the highest value in the third field
        max_row <- which.max(frag_data$V3)
        
        # Extract the value from the seventh field in the max row
        allele_vector[i] <<- frag_data$V7[max_row]
        
        # Set column names
        setnames(frag_data, c("Score", "Variant"))
      }
      
      # Store the data frame in the list with the filename as the name
      frag_data_list[[frag_filename]] <<- frag_data
    
      
      prog_typ <- round(i/length(frag_files)*100)
      
      updateProgressBar(session = session, id = "progress_profile", value = prog_typ, 
                        total = 100)
    }
    
    output$typing_fin <- renderUI({
      length <- paste(length(allele_vector), "alleles computed.")
      int <- paste(length(allele_vector)-sum(sapply(allele_vector, is.na)), "successful attributions.")
      error <- paste(sum(sapply(allele_vector, is.na)), "unsuccessful attribution(s) (NA).")
      HTML(paste("<span style='color: white;'>", length, int, error, sep = '<br/>'))
      })
    
    output$sel_result <- renderUI({
      pickerInput("sel_result", label = "Select Locus",
                  choices = names(frag_data_list),
                  selected = names(frag_data_list)[1],
                  options = list(
                    `live-search` = TRUE,
                    `actions-box` = TRUE,
                    size = 10,
                    style = "background-color: white; border-radius: 5px;")
      )
    })
    
    output$typ_res_tab <- renderTable({
      frag_data_list[[input$sel_result]]
    })
    
  })
  
  
  
  
  
  
  ############## Append Allelic Profile  ######################################
  
  # Append as entry to local database
  
  observeEvent(input$append, {
    confirmSweetAlert(
      inputId = "append_conf",
      type = "info",
      btn_colors = c("grey", "green"),
      text = paste("Do you want to append the typed allelic profile to local", input$cgmlst_typing, "database?")
    )
  })
  
  
  observeEvent(input$append_conf, {
    
    # If first Typing Entry
    if (!any(grepl("Typing", dir_ls(paste0(getwd(), "/Database/", gsub(" ", "_", input$cgmlst_typing)))))) {
      
      Database <- list(Typing = data.frame())
      
      Typing <- data.frame(matrix(NA, nrow = 0, ncol = 11 + length(list.files(paste0(getwd(), "/Database/", gsub(" ", "_", input$cgmlst_typing), paste0("/", gsub(" ", "_", input$cgmlst_typing), "_alleles"))))))
      
      metadata <- c(1, input$assembly_id, input$assembly_name, input$cgmlst_typing, as.character(input$append_isodate), 
                    input$append_host, input$append_country, input$append_city, 
                    as.character(input$append_analysisdate), length(allele_vector)-sum(sapply(allele_vector, is.na)), sum(sapply(allele_vector, is.na)))
      
      new_row <- c(metadata, allele_vector)
      
      Typing <- rbind(Typing, new_row)
      
      colnames(Typing) <- append(c("Index", "Assembly ID", "Assembly Name", "Scheme", "Isolation Date", "Host", "Country", "City", "Typing Date", "Successes", "Errors"), 
                                 gsub(".fasta", "", basename(list.files(paste0(getwd(), "/Database/", gsub(" ", "_", input$cgmlst_typing), paste0("/", gsub(" ", "_", input$cgmlst_typing), "_alleles"))))))
      
      DF1$data <- Typing
      
      Database[["Typing"]] <- Typing
      
      saveRDS(Database, paste0(getwd(), "/Database/", folder_name, "/Typing.rds"))
      
    } else { # If not first Typing Entry
      
      Database <- readRDS(paste0(getwd(), "/Database/", gsub(" ", "_", input$cgmlst_typing), "/Typing.rds"))
      
      metadata <- c(nrow(Database[["Typing"]]) + 1, input$assembly_id, input$assembly_name, input$cgmlst_typing, as.character(input$append_isodate), 
                    input$append_host, input$append_country, input$append_city, 
                    as.character(input$append_analysisdate), length(allele_vector)-sum(sapply(allele_vector, is.na)), sum(sapply(allele_vector, is.na)))
      
      new_row <- c(metadata, allele_vector)
      
      DF1$data <- rbind(Database$Typing, new_row)
      
      Database$Typing <- rbind(Database$Typing, new_row)
      
      saveRDS(Database, paste0(getwd(), "/Database/", gsub(" ", "_", input$cgmlst_typing), "/Typing.rds"))
    }
  })
  
 
  
} # end server


################## Shiny #####################

shinyApp(ui = ui, server = server)