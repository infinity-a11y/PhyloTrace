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


################ User Interface ################

ui <- dashboardPage(
  
    # Title 
    dashboardHeader(title = span(img(src="PhyloTree.jpg", width = 190))),

# Sidebar ----   
    dashboardSidebar(
      tags$style("label{color: white;}"),
      br(), br(),
      sidebarMenu(
        id = "tabs",
        menuItem(
          text = "Initialization",
          tabName = "init",
          icon = icon("gears"),
          selected = TRUE
        ),
        menuItem(
          text = "Upload Data",
          tabName = "upload",
          icon = icon("upload"),
          menuSubItem(
            text = "NGS Pipeline",
            tabName = "ngs_pipeline"
          ),
          menuSubItem(
            text = "Tree format",
            tabName = "tree_format"
          )
          ),
        menuItem(
          text = "Visualization",
          tabName = "visualization",
          icon = icon("code-branch")
          ),
        menuItem(
           text = "Download Report",
           tabName = "report",
           icon = icon("download")
           ),
        br(), br(), br(), br(), 
        conditionalPanel(
          "input.tabs=='report'",
          column(
            width = 12,
            align = "center",
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
              choices = c("PDF", "HTML", "Word")
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
          "input.tabs=='visualization'",
          column(
            width = 12,
            h4(p("Choose data source"), style = "color:white"),
            br(),
            awesomeRadio(
              inputId = "generate_tree",
              label = NULL,
              choices = c("Uploaded data", "Random")
              ),
            conditionalPanel(
              "input.generate_tree=='Uploaded data'",
              awesomeRadio(
                 inputId = "slot", label = h5(p("Choose slot"), style = "color:white"),
                 choices = c("1", "2", "3", "4"),
                 inline = FALSE, status = "default"
                 ),
              br(),
              conditionalPanel(
                 "input.slot == '1'",
                 actionButton(inputId = "make_tree1", label = "Generate Tree"),
                 br(),hr(),
                 h4(p("Send tree to report"), style = "color:white"),
                 br(),
                 actionButton(inputId = "save_1", label = "Save")
                 ),
              conditionalPanel(
                 "input.slot == '2'",
                 actionButton(inputId = "make_tree2", label = "Generate Tree"),
                 br(),hr(),
                 h4(p("Send tree to report"), style = "color:white"),
                 br(),
                 actionButton(inputId = "save_2", label = "Save")
                 ),
              conditionalPanel(
                 "input.slot == '3'",
                 actionButton(inputId = "make_tree3", label = "Generate Tree"),
                 br(),hr(),
                 h4(p("Send tree to report"), style = "color:white"),
                 br(),
                 actionButton(inputId = "save_3", label = "Save")
                 ),
              conditionalPanel(
                 "input.slot == '4'",
                 actionButton(inputId = "make_tree4", label = "Generate Tree"),
                 br(),hr(),
                 h4(p("Send tree to report"), style = "color:white"),
                 br(),
                 actionButton(inputId = "save_4", label = "Save")
                 )),
            conditionalPanel(
              "input.generate_tree=='Random'",
              br(),
              numericInput(
                "ntree",
                label = h5("# branches", style = "color:white"), 
                value = 30,
                max = 500,
                width = "110px"
                ),
              br(),
              actionButton(inputId = "random_tree", label = "Generate Tree"),
              br(),hr(),
              h4(p("Send tree to report"), style = "color:white"),
              br(),
              actionButton(inputId = "save_r", label = "Save")
              )
            )
          )
        )
      ),

    dashboardBody(
      shinyDashboardThemeDIY(
      ### general
      appFontFamily = "Tahoma"
      ,appFontColor = "#000000"
      ,primaryFontColor = "rgb(0,0,0)"
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
      ,boxShadowSize = "0px 1px 1px"
      ,boxShadowColor = "#ffffff"
      ,boxTitleSize = 16
      ,boxDefaultColor = "#00a65a"
      ,boxPrimaryColor = "#00a65a"
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
      ,buttonBackColor = "#222829"
      ,buttonTextColor = "#ffffff"
      ,buttonBorderColor = "#222829"
      ,buttonBorderRadius = 10
      
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
      

# Tab Initialization        ---------------------------------------------      

    tabItem(
      tabName = "init",
      fluidRow(
        column(
          width = 3,
          align = "center",
          h3(p("Select cgMLST Scheme"), style = "color:white"),
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
          h4(p("Downloaded Targets"), style = "color:white"),
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
      
      
# Tab Upload - NGS pipeline ---------------------------------------------

    tabItem(
      tabName = "ngs_pipeline",
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      column(
        width = 12,
        align = "center",
        h2(p("Still in work ..."), style = "color:white")
        )
    ),
      
      
# Tab Upload - Tree format ----------------------------------------------

    
    tabItem(
       tabName = "tree_format",
       br(), 
       fluidRow(
         column(
           width = 1,
           align = "center",
           h4(p("Add Slot"), style = "color:white"),
           actionGroupButtons(
             inputIds = c("plus_slot", "minus_slot"),
             labels = list(icon("plus"), icon("minus")),
             status = c("success", "danger")
           ),
           br()
           )
         ),
       br(), br(),
       fluidRow(
         column(
           align = "center",
           width = 1,
           br(), 
           h2(p("1"), style = "color:white")
           ),
         column(
           width = 3,
           fileInput(
             inputId = "file1",
             label = h5(p("Upload data"), style = "color:white"),
             multiple = FALSE,
             placeholder = "Select file",
             width = "250px")
           ),
         column(
           width = 2,
           selectInput(
             "select1", 
             label = h5("Select format", style = "color:white"),
             choices = list("BEAST", "MEGA", "MrBayes", "PAML_Baseml", "PAML_codeml", 
                            "RAxML", "FASTA", "HYPHY", "HYPHY Ancestral", "IQ-Tree", "jplace",
                            "jtree", "Newick", "NEXUS", "NHX", "phyloXML", "r8s"),
             selected = NULL,
             width = "160px")
           ),
         column(
           width = 1,
           br(), br(), 
           actionButton(inputId = "parse1", label = "Parse Data")
         )
       ),
       fluidRow(
         column(width = 1),
         column(
           width = 11,
           tableOutput("table1")
           )
       ),
       conditionalPanel(
         "((input.plus_slot + (-input.minus_slot)) == 1) || ((input.plus_slot + (-input.minus_slot)) == 2) || ((input.plus_slot + (-input.minus_slot)) == 3)",
         hr(),
         fluidRow(
            column(
               align = "center",
               width = 1,
               br(), 
               h2(p("2"), style = "color:white")
               ),
            column(
               width = 3,
               fileInput(
                  inputId = "file2",
                  label = h5(p("Upload data"), style = "color:white"),
                  multiple = FALSE,
                  placeholder = "Select file",
                  width = "250px")
               ),
            column(
               width = 2,
               selectInput(
                  "select2", 
                  label = h5("Select format", style = "color:white"),
                  choices = list("BEAST", "MEGA", "MrBayes", "PAML_Baseml", "PAML_codeml", 
                            "RAxML", "FASTA", "HYPHY", "HYPHY Ancestral", "IQ-Tree", "jplace",
                            "jtree", "Newick", "NEXUS", "NHX", "phyloXML", "r8s"),
                  selected = NULL,
                  width = "160px")
               ),
            column(
               width = 1,
               br(), br(), 
               actionButton(inputId = "parse2", label = "Parse Data")
               )
            ),
         fluidRow(
            column(width = 1),
            column(
               width = 11,
               tableOutput("table2")
               )
            )
         ),
       conditionalPanel(
          "((input.plus_slot + (-input.minus_slot)) == 2) || (input.plus_slot + (-input.minus_slot) == 3)",
       hr(),
       fluidRow(
         column(
           align = "center",
           width = 1,
           br(), 
           h2(p("3"), style = "color:white")
         ),
         column(
           width = 3,
           fileInput(
             inputId = "file3",
             label = h5(p("Upload data"), style = "color:white"),
             multiple = FALSE,
             placeholder = "Select file",
             width = "250px")
         ),
         column(
           width = 2,
           selectInput(
             "select3", 
             label = h5("Select format", style = "color:white"),
             choices = list("BEAST", "MEGA", "MrBayes", "PAML_Baseml", "PAML_codeml", 
                            "RAxML", "FASTA", "HYPHY", "HYPHY Ancestral", "IQ-Tree", "jplace",
                            "jtree", "Newick", "NEXUS", "NHX", "phyloXML", "r8s"),
             selected = NULL,
             width = "160px")
         ),
         column(
           width = 1,
           br(), br(), 
           actionButton(inputId = "parse3", label = "Parse Data")
           )
         ),
       fluidRow(
          column(width = 1),
          column(
             width = 11,
             tableOutput("table3")
             )
          )
       ), 
       conditionalPanel(
         "input.plus_slot + (-input.minus_slot) == 3",
         hr(),
         fluidRow(
            column(
               align = "center",
               width = 1,
               br(), 
               h2(p("4"), style = "color:white")
               ),
            column(
               width = 3,
               fileInput(
                  inputId = "file4",
                  label = h5(p("Upload data"), style = "color:white"),
                  multiple = FALSE,
                  placeholder = "Select file",
                  width = "250px")
               ),
            column(
               width = 2,
               selectInput(
                  inputId = "select4", 
                  label = h5("Select format", style = "color:white"),
                  choices = list("BEAST", "MEGA", "MrBayes", "PAML_Baseml", "PAML_codeml", 
                            "RAxML", "FASTA", "HYPHY", "HYPHY Ancestral", "IQ-Tree", "jplace",
                            "jtree", "Newick", "NEXUS", "NHX", "phyloXML", "r8s"),
                  selected = NULL,
                  width = "160px")
               ),
            column(
               width = 1,
               br(), br(), 
               actionButton(inputId = "parse4", label = "Parse Data")
               )
            ),
         fluidRow(
            column(width = 1),
            column(
               width = 11,
               tableOutput("table4")
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
              "input.generate_tree == 'Uploaded data' && input.slot == '1'",
              addSpinner(
                 plotOutput("tree1"),
                 spin = "dots", 
                 color = "#ffffff")
              ),
           conditionalPanel(
              "input.generate_tree == 'Uploaded data' && input.slot == '2'",
              addSpinner(
                 plotOutput("tree2"),
                 spin = "dots", 
                 color = "#ffffff")
           ),
           conditionalPanel(
              "input.generate_tree == 'Uploaded data' && input.slot == '3'",
              addSpinner(
                 plotOutput("tree3"),
                 spin = "dots", 
                 color = "#ffffff")
           ),
           conditionalPanel(
              "input.generate_tree == 'Uploaded data' && input.slot == '4'",
              addSpinner(
                 plotOutput("tree4"),
                 spin = "dots", 
                 color = "#ffffff")
              ),
           conditionalPanel(
              "input.generate_tree=='Random'",
              addSpinner(
                 plotOutput("tree_random"),
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
      "input.generate_tree=='Random'",    
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
          br(),
          checkboxGroupInput(
            inputId = "include_sampleinfo",
            label = "Sample",
            choices = c("Sampling Date", "Sampling Location", "Taken by (Name)", "Comment"),
            selected = c("Sampling Date", "Sampling Location"),
            inline = FALSE,
            width = NULL,
            choiceNames = NULL,
            choiceValues = NULL
          ),
          checkboxGroupInput(
            inputId = "include_sequencing",
            label = "Sequencing",
            choices = c("Device", "Flow Cell", "Run Start", "Run Finished", 
                        "Operator", "Output Size", "Comment"),
            selected = c("Device", "Fow Cell", "Operator"),
            inline = FALSE,
            width = NULL,
            choiceNames = NULL,
            choiceValues = NULL
          ),
          checkboxGroupInput(
            inputId = "include_analysis",
            label = "Analysis",
            choices = c("Analysis Date", "Assembly Parameters", "cgMLST Scheme", "Comment"),
            selected = c("Analysis Date", "cgMLST Scheme"),
            inline = FALSE,
            width = NULL,
            choiceNames = NULL,
            choiceValues = NULL
          )
        ),
        column(
          width = 3,
          align = 'left',
          h3(p("Displayed Elements"), style = "color:white"),
          br(),
          h4(p("General"), style = "color:white"),
          br(),
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
          )
          ,
          hr(),
          h4(p("Sample Information"), style = "color:white"),
          br(),
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
          )
        ),
        column(
          width = 3,
          align = "left",
          br(), br(), br(), br(),
          h4(p("Sequencing"), style = "color:white"),
          br(),
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
          ),
          hr(),
          h4(p("Analysis"), style = "color:white"),
          br(),
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
  
  myReactives <- reactiveValues() 
  
# Download cgMLST       -------------------------------------------------------
  
  observe(
    if(input$select_cgmlst == "Acinetobacter baumanii") {
      link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/3956907/alleles/"
      myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/3956907/"
      link_targets <<- "https://www.cgmlst.org/ncs/schema/3956907/locus/?content-type=csv"
      folder_name <<- "Acinetobacter_baumanii_alleles"
      } else if (input$select_cgmlst == "Bacillus anthracis") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/19008694/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/19008694/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/19008694/locus/?content-type=csv"
        folder_name <<- "Bacillus_anthracis_alleles"
      } else if (input$select_cgmlst == "Bordetella pertussis") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/29412358/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/29412358/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/29412358/locus/?content-type=csv"
        folder_name <<- "Bordetella_pertussis_alleles"
      } else if (input$select_cgmlst == "Brucella melitensis") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/6398355/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/6398355/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema//6398355/locus/?content-type=csv"
        folder_name <<- "Brucella_melitensis_alleles"
      } else if (input$select_cgmlst == "Brucella spp.") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/24062474/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/24062474/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/24062474/locus/?content-type=csv"
        folder_name <<- "Brucella_spp_alleles"
      } else if (input$select_cgmlst == "Burkholderia mallei (FLI)") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/23721348/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/23721348/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/23721348/locus/?content-type=csv"
        folder_name <<- "Burkholderia_mallei_FLI_alleles"
      } else if (input$select_cgmlst == "Burkholderia mallei (RKI)") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/23643739/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/23643739/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/23643739/locus/?content-type=csv"
        folder_name <<- "Burkholderia_mallei_RKI_alleles"
      } else if (input$select_cgmlst == "Burkholderia pseudomallei") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/18876117/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/18876117/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/18876117/locus/?content-type=csv"
        folder_name <<- "Burkholderia_pseudomallei_alleles"
      } else if (input$select_cgmlst == "Campylobacter jejuni/coli") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/145039/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/145039/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/145039/locus/?content-type=csv"
        folder_name <<- "Campylobacter_jejuni_coli_alleles"
      } else if (input$select_cgmlst == "Clostridioides difficile") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/12556067/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/12556067/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/12556067/locus/?content-type=csv"
        folder_name <<- "Clostridioides_difficile_alleles"
      } else if (input$select_cgmlst == "Clostridium perfringens") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/15017225/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/15017225/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/15017225956907/locus/?content-type=csv"
        folder_name <<- "Clostridium_perfringens_alleles"
      } else if (input$select_cgmlst == "Corynebacterium diphtheriae") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/30589266/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/30589266/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/30589266/locus/?content-type=csv"
        folder_name <<- "Corynebacterium_diphtheriae_alleles"
      } else if (input$select_cgmlst == "Cronobacter sakazakii/malonaticus") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/29420227/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/29420227/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/29420227/locus/?content-type=csv"
        folder_name <<- "Cronobacter_sakazakii_malonaticus_alleles"
      } else if (input$select_cgmlst == "Enterococcus faecalis") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/3887469/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/3887469/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/3887469/locus/?content-type=csv"
        folder_name <<- "Enterococcus_faecalis_alleles"
      } else if (input$select_cgmlst == "Enterococcus faecium") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/991893/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/991893/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/991893/locus/?content-type=csv"
        folder_name <<- "Enterococcus_faecium_alleles"
      } else if (input$select_cgmlst == "Escherichia coli") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/5064703/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/5064703/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/5064703/locus/?content-type=csv"
        folder_name <<- "Escherichia_coli_alleles"
      } else if (input$select_cgmlst == "Francisella tularensis") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/260204/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/260204/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/260204/locus/?content-type=csv"
        folder_name <<- "Francisella_tularensis_alleles"
      } else if (input$select_cgmlst == "Klebsiella pneumoniae/variicola/quasipneumoniae") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/2187931/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/2187931/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/2187931/locus/?content-type=csv"
        folder_name <<- "K_pneumoniae_variicola_quasipneumoniae_alleles"
      } else if (input$select_cgmlst == "Legionella pneumophila") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/1025099/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/1025099/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/1025099/locus/?content-type=csv"
        folder_name <<- "Legionella_pneumophila_alleles"
      } else if (input$select_cgmlst == "Listeria monocytogenes") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/690488/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/690488/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/690488/locus/?content-type=csv"
        folder_name <<- "Listeria_monocytogenes_alleles"
      } else if (input$select_cgmlst == "Mycobacterium tuberculosis/bovis/africanum/canettii") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/741110/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/741110/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/741110/locus/?content-type=csv"
        folder_name <<- "M_tuberculosis_bovis_africanum_canettii_alleles"
      } else if (input$select_cgmlst == "Mycobacteroides abscessus") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/22602285/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/22602285/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/22602285/locus/?content-type=csv"
        folder_name <<- "Mycobacteroides_abscessus_alleles"
      } else if (input$select_cgmlst == "Mycoplasma gallisepticum") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/6402012/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/6402012/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/6402012/locus/?content-type=csv"
        folder_name <<- "Mycoplasma_gallisepticum_alleles"
      } else if (input$select_cgmlst == "Paenibacillus larvae") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/17414003/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/17414003/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/17414003/locus/?content-type=csv"
        folder_name <<- "Paenibacillus_larvae_alleles"
      } else if (input$select_cgmlst == "Pseudomonas aeruginosa") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/16115339/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/16115339/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/16115339/locus/?content-type=csv"
        folder_name <<- "Pseudomonas_aeruginosa_alleles"
      } else if (input$select_cgmlst == "Salmonella enterica") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/4792159/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/4792159/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/4792159/locus/?content-type=csv"
        folder_name <<- "Salmonella_enterica_alleles"
      } else if (input$select_cgmlst == "Serratia marcescens") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/24616475/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/24616475/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/24616475/locus/?content-type=csv"
        folder_name <<- "Serratia_marcescens_alleles"
      } else if (input$select_cgmlst == "Staphylococcus aureus") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/141106/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/141106/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/141106/locus/?content-type=csv"
        folder_name <<- "Staphylococcus_aureus_alleles"
      } else if (input$select_cgmlst == "Staphylococcus capitis") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/26824796/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/26824796/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/26824796/locus/?content-type=csv"
        folder_name <<- "Staphylococcus_capitis_alleles"
      } else if (input$select_cgmlst == "Streptococcus pyogenes") {
        link_cgmlst <<- "https://www.cgmlst.org/ncs/schema/30585223/alleles/"
        myReactives$link_scheme <<- "https://www.cgmlst.org/ncs/schema/30585223/"
        link_targets <<- "https://www.cgmlst.org/ncs/schema/30585223/locus/?content-type=csv"
        folder_name <<- "Streptococcus_pyogenes_alleles"
      } 
    
  )
  
  observeEvent(input$download_cgMLST, {
    
    myReactives$target_table <- NULL
    
    download(link_cgmlst, dest="dataset.zip", mode="wb") 
    
    unzip(zipfile = "dataset.zip", 
          exdir = paste0("./", folder_name))
    
    unlink("dataset.zip")
    
    download(link_targets, dest=paste0(getwd(),"/", folder_name, "/targets.csv"), mode="wb") 
    
    myReactives$target_table <- read.csv(paste0(getwd(),"/", folder_name, "/targets.csv"),
                                         header = TRUE, sep = "\t", row.names = NULL,
                                         colClasses = c("NULL", "character", "character", "integer", 
                                                        "integer", "character", "integer", "NULL"))
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
  
# Parse Slot 1 ------------------------------------------------------------

  
  observeEvent(input$parse1, {
    if (input$select1 == "BEAST") {
      
      parsed_file1 <<- read.beast(input$file1$datapath)
      phylo1 <<- as.phylo(parsed_file1)
      
      if (class(parsed_file1) == "treedata" & class(phylo1) == "phylo") {
        show_toast(
          title = "Parsing successful",
          type = "success",
          position = "top-end",
          width = "400px",
          timer = 4000)
        output$table1 <- renderTable(input$file1 %>% select(-type, -size))
        
        } else {
            show_toast(
              title = "Error",
              type = "error",
              position = "top-end",
              width = "400px",
              timer = 4000)
          }
      } else if (input$select1 == "MrBayes") {
        
          parsed_file1 <<- read.mrbayes(input$file1$datapath)
          phylo1 <<- as.phylo(parsed_file1)
          
          if (class(parsed_file1) == "treedata" & class(phylo1) == "phylo") {
            show_toast(
              title = "Parsing successful",
              type = "success",
              position = "top-end",
              width = "400px",
              timer = 4000)
            output$table1 <- renderTable(input$file1 %>% select(-type, -size))
            
            } else {
                show_toast(
                  title = "Error",
                  type = "error",
                  position = "top-end",
                  width = "400px",
                  timer = 4000)
              }
      } else if (input$select1 == "MEGA") {
        
          parsed_file1 <<- read.mega(input$file1$datapath)
          phylo1 <<- as.phylo(parsed_file1)
          
          if (class(parsed_file1) == "treedata" & class(phylo1) == "phylo") {
            show_toast(
                title = "Parsing successful",
                type = "success",
                position = "top-end",
                width = "400px",
                timer = 4000)
            output$table1 <- renderTable(input$file1 %>% select(-type, -size))
            
            } else {
                show_toast(
                  title = "Error",
                  type = "error",
                  position = "top-end",
                  width = "400px",
                  timer = 4000)
            }
          }
    })
  

# Parse Slot 2 ------------------------------------------------------------

  
  observeEvent(input$parse2, {
    if (input$select2 == "BEAST") {
      
      parsed_file2 <<- read.beast(input$file2$datapath)
      phylo2 <<- as.phylo(parsed_file2)
      
      if (class(parsed_file2) == "treedata" & class(phylo2) == "phylo") {
        show_toast(
          title = "Parsing successful",
          type = "success",
          position = "top-end",
          width = "400px",
          timer = 4000)
        output$table2 <- renderTable(input$file2 %>% select(-type, -size))
        
      } else {
        show_toast(
          title = "Error",
          type = "error",
          position = "top-end",
          width = "400px",
          timer = 4000)
      }
    } else if (input$select2 == "MrBayes") {
      
      parsed_file2 <<- read.mrbayes(input$file2$datapath)
      phylo2 <<- as.phylo(parsed_file2)
      
      if (class(parsed_file2) == "treedata" & class(phylo2) == "phylo") {
        show_toast(
          title = "Parsing successful",
          type = "success",
          position = "top-end",
          width = "400px",
          timer = 4000)
        output$table2 <- renderTable(input$file2 %>% select(-type, -size))
        
      } else {
        show_toast(
          title = "Error",
          type = "error",
          position = "top-end",
          width = "400px",
          timer = 4000)
      }
    } else if (input$select2 == "MEGA") {
      
      parsed_file2 <<- read.mega(input$file2$datapath)
      phylo2 <<- as.phylo(parsed_file2)
      
      if (class(parsed_file2) == "treedata" & class(phylo2) == "phylo") {
        show_toast(
          title = "Parsing successful",
          type = "success",
          position = "top-end",
          width = "400px",
          timer = 4000)
        output$table2 <- renderTable(input$file2 %>% select(-type, -size))
        
      } else {
        show_toast(
          title = "Error",
          type = "error",
          position = "top-end",
          width = "400px",
          timer = 4000)
      }
    }
  })
  
  

# Parse Slot 3 ------------------------------------------------------------


  observeEvent(input$parse3, {
    if (input$select3 == "BEAST") {
      
      parsed_file3 <<- read.beast(input$file3$datapath)
      phylo3 <<- as.phylo(parsed_file3)
      
      if (class(parsed_file3) == "treedata" & class(phylo3) == "phylo") {
        show_toast(
          title = "Parsing successful",
          type = "success",
          position = "top-end",
          width = "400px",
          timer = 4000)
        output$table3 <- renderTable(input$file3 %>% select(-type, -size))
        
      } else {
        show_toast(
          title = "Error",
          type = "error",
          position = "top-end",
          width = "400px",
          timer = 4000)
      }
    } else if (input$select3 == "MrBayes") {
      
      parsed_file3 <<- read.mrbayes(input$file3$datapath)
      phylo3 <<- as.phylo(parsed_file3)
      
      if (class(parsed_file3) == "treedata" & class(phylo3) == "phylo") {
        show_toast(
          title = "Parsing successful",
          type = "success",
          position = "top-end",
          width = "400px",
          timer = 4000)
        output$table3 <- renderTable(input$file3 %>% select(-type, -size))
        
      } else {
        show_toast(
          title = "Error",
          type = "error",
          position = "top-end",
          width = "400px",
          timer = 4000)
      }
    } else if (input$select3 == "MEGA") {
      
      parsed_file3 <<- read.mega(input$file3$datapath)
      phylo3 <<- as.phylo(parsed_file3)
      
      if (class(parsed_file3) == "treedata" & class(phylo3) == "phylo") {
        show_toast(
          title = "Parsing successful",
          type = "success",
          position = "top-end",
          width = "400px",
          timer = 4000)
        output$table3 <- renderTable(input$file3 %>% select(-type, -size))
        
      } else {
        show_toast(
          title = "Error",
          type = "error",
          position = "top-end",
          width = "400px",
          timer = 4000)
      }
    }
  })
  

# Parse Slot 4 ------------------------------------------------------------

  
  observeEvent(input$parse4, {
    if (input$select4 == "BEAST") {
      
      parsed_file4 <<- read.beast(input$file4$datapath)
      phylo4 <<- as.phylo(parsed_file4)
      
      if (class(parsed_file4) == "treedata" & class(phylo4) == "phylo") {
        show_toast(
          title = "Parsing successful",
          type = "success",
          position = "top-end",
          width = "400px",
          timer = 4000)
        output$table4 <- renderTable(input$file4 %>% select(-type, -size))
        
      } else {
        show_toast(
          title = "Error",
          type = "error",
          position = "top-end",
          width = "400px",
          timer = 4000)
      }
    } else if (input$select4 == "MrBayes") {
      
      parsed_file4 <<- read.mrbayes(input$file4$datapath)
      phylo4 <<- as.phylo(parsed_file4)
      
      if (class(parsed_file4) == "treedata" & class(phylo4) == "phylo") {
        show_toast(
          title = "Parsing successful",
          type = "success",
          position = "top-end",
          width = "400px",
          timer = 4000)
        output$table4 <- renderTable(input$file4 %>% select(-type, -size))
        
      } else {
        show_toast(
          title = "Error",
          type = "error",
          position = "top-end",
          width = "400px",
          timer = 4000)
      }
    } else if (input$select4 == "MEGA") {
      
      parsed_file4 <<- read.mega(input$file4$datapath)
      phylo4 <<- as.phylo(parsed_file4)
      
      if (class(parsed_file4) == "treedata" & class(phylo4) == "phylo") {
        show_toast(
          title = "Parsing successful",
          type = "success",
          position = "top-end",
          width = "400px",
          timer = 4000)
        output$table4 <- renderTable(input$file4 %>% select(-type, -size))
        
      } else {
        show_toast(
          title = "Error",
          type = "error",
          position = "top-end",
          width = "400px",
          timer = 4000)
      }
    }
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
    
    # Make Phylogram Plot
    observeEvent(input$make_tree1,
                 {output$tree1 <- renderPlot({
                    
                    as.ggplot(ggtree(tr = phylo1,
                                     aes(color = I(color1())),
                                     layout = input$layout1) +
                                 revx1() +
                                 revy1() +
                                 treescale_x1() +
                                 tip1() +
                                 label1() +
                                 node1() +
                                 theme(plot.background = element_rect(fill = b_color1(),
                                                                      color = b_color1()),
                                       panel.background = element_rect(fill = b_color1(),
                                                                       color = b_color1())), 
                              angle = input$rotate1)
                    })
                 })
    
    observeEvent(input$make_tree2,
                 {output$tree2 <- renderPlot({
                    
                    as.ggplot(ggtree(tr = phylo2,
                                     aes(color = I(color2())),
                                     layout = input$layout2) +
                                 revx2() +
                                 revy2() +
                                 treescale_x2() +
                                 tip2() +
                                 label2() +
                                 node2() +
                                 theme(plot.background = element_rect(fill = b_color2(),
                                                                      color = b_color2()),
                                       panel.background = element_rect(fill = b_color2(),
                                                                       color = b_color2())), 
                              angle = input$rotate2)
                    })
                 })
    
    observeEvent(input$make_tree3,
                 {output$tree3 <- renderPlot({
                    
                    as.ggplot(ggtree(tr = phylo3,
                                     aes(color = I(color3())),
                                     layout = input$layout3) +
                                 revx3() +
                                 revy3() +
                                 treescale_x3() +
                                 tip3() +
                                 label3() +
                                 node3() +
                                 theme(plot.background = element_rect(fill = b_color3(),
                                                                      color = b_color3()),
                                       panel.background = element_rect(fill = b_color3(),
                                                                       color = b_color3())), 
                              angle = input$rotate3)
                    })
                 })
    
    observeEvent(input$make_tree4,
                 {output$tree4 <- renderPlot({
                    
                    as.ggplot(ggtree(tr = phylo4,
                                     aes(color = I(color4())),
                                     layout = input$layout4) +
                                 revx4() +
                                 revy4() +
                                 treescale_x4() +
                                 tip4() +
                                 label4() +
                                 node4() +
                                 theme(plot.background = element_rect(fill = b_color4(),
                                                                      color = b_color4()),
                                       panel.background = element_rect(fill = b_color4(),
                                                                       color = b_color4())), 
                              angle = input$rotate4)
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
        
        rep_profile$profile_names <- list.files(paste0(getwd(),"/rep_profiles"))
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
      
      
                  
      
      
      
    } # end server
    

################## Shiny #####################

shinyApp(ui = ui, server = server)
