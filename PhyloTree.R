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
          tabName = "init"
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
            tabName = "tree_format",
            selected = TRUE
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
        br(), br(), br(), br(), br(), 
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
      ,buttonBackColor = "#282f38"
      ,buttonTextColor = "#ffffff"
      ,buttonBorderColor = "#282f38"
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
                                                    interaction = list(hex = TRUE,                                                                        rgba = FALSE,                                                                        input = TRUE,                                                                        save = TRUE,                                                                        clear = FALSE),
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
      column(
         br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), 
         br(), br(), br(), br(), br(), br(), br(), br(),
         width = 12,
         align = "center",
         actionButton(
            inputId = "save_report",
            label = "Save Report",
            icon = icon("download"),
            width = "auto"
         )
         )
      
      )
    
    ) # End tabItems
    
    ) # End dashboardPage

) # end UI



################### Server ###################

server <- function(input, output) {
  

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

   observeEvent(input$save_report, {
      ggsave(
         filename = "Tree1.png",
         device = "png"
         )
   })   

}


################## Shiny #####################

shinyApp(ui = ui, server = server)
