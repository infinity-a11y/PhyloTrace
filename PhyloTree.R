# PhyloTrace
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

if (!require(visNetwork))
  install.packages('visNetwork')
library(visNetwork)

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

# Plot download JS code

jpeg_mst <- paste0(c(
  "$(document).ready(function(){",
  "  $('#save_plot_jpeg').on('click', function(){",
  "    var el = document.querySelector('canvas');",
  "    // Clone the chart to add a background color.",
  "    var cloneCanvas = document.createElement('canvas');",
  "    cloneCanvas.width = el.width;",
  "    cloneCanvas.height = el.height;",
  "    var ctx = cloneCanvas.getContext('2d');",
  "    ctx.fillStyle = getBackgroundColor();",
  "    ctx.fillRect(0, 0, el.width, el.height);",
  "    ctx.drawImage(el, 0, 0);",
  "    // Download.",
  "    const a = document.createElement('a');",
  "    document.body.append(a);",
  "    a.download = ", shQuote(paste0(Sys.Date(), "_MST")),
  "    a.href = cloneCanvas.toDataURL('image/jpeg', 1.0);",
  "    a.click();",
  "    a.remove();",
  "    cloneCanvas.remove();",
  "  });",
  "});"
), collapse = "\n")

bmp_mst <- paste0(c(
  "$(document).ready(function(){",
  "  $('#save_plot_bmp').on('click', function(){",
  "    var el = document.querySelector('canvas');",
  "    // Clone the chart to add a background color.",
  "    var cloneCanvas = document.createElement('canvas');",
  "    cloneCanvas.width = el.width;",
  "    cloneCanvas.height = el.height;",
  "    var ctx = cloneCanvas.getContext('2d');",
  "    ctx.fillStyle = getBackgroundColorClear();",
  "    ctx.fillRect(0, 0, el.width, el.height);",
  "    ctx.drawImage(el, 0, 0);",
  "    // Download.",
  "    const a = document.createElement('a');",
  "    document.body.append(a);",
  "    a.download = ", shQuote(paste0(Sys.Date(), "_MST.bmp")),
  "    a.href = cloneCanvas.toDataURL('image/bmp');",
  "    a.click();",
  "    a.remove();",
  "    cloneCanvas.remove();",
  "  });",
  "});"
), collapse = "\n")


mergeCanvas_bmp <- paste0(c(
  "$(document).ready(function(){", "\n",
  "  $('#save_plot_merged).on('click', function(){", "\n",
  "    var allCanvases = document.querySelectorAll('canvas');", "\n",
  "    var mergedCanvas = document.createElement('canvas');", "\n",
  "    var ctx = mergedCanvas.getContext('2d');", "\n",
  "    var totalWidth = 0;", "\n",
  "    // Calculate the total width of all canvases.", "\n",
  "    allCanvases.forEach(function(canvas) {", "\n",
  "      totalWidth += canvas.width;", "\n",
  "    });", "\n",
  "    // Set the width and height of the merged canvas.", "\n",
  "    mergedCanvas.width = totalWidth;", "\n",
  "    mergedCanvas.height = allCanvases[0].height;", "\n",
  "    var offsetX = 0;", "\n",
  "    // Draw each canvas onto the merged canvas.", "\n",
  "    allCanvases.forEach(function(canvas) {", "\n",
  "      ctx.drawImage(canvas, offsetX, 0);", "\n",
  "      offsetX += canvas.width;", "\n",
  "    });", "\n",
  "    // Download the merged canvas as BMP.", "\n",
  "    const a = document.createElement('a');", "\n",
  "    document.body.append(a);", "\n",
  "    a.download = ", shQuote(paste0(Sys.Date(), "_MST")), "\n",
  "    a.href = mergedCanvas.toDataURL('image/jpeg', 1.0);", "\n",
  "    a.click();", "\n",
  "    a.remove();", "\n",
  "    mergedCanvas.remove();", "\n",
  "  });", "\n",
  "});"
))



mst_bg_clear <- paste0("// Function to get the RGB color from a specific sub-element with a specific ID and class
function getBackgroundColorClear() {
  // Check the state of the Shiny checkbox 
  var checkboxStatus = $('#mst_background_transparent').prop('checked');
                 
                 // If the checkbox is checked, return transparent RGB code
                 if (checkboxStatus) {
                   return \'rgba(0, 0, 0, 0)\';
        } else {
            // Get the sub-element by ID and class
            var targetElement = document.querySelector(\'#mst_background_color input.form-control.pickr-color\');

            // Check if the element is found
            if (targetElement) {
                // Get the computed style of the element
                var computedStyle = window.getComputedStyle(targetElement);

                // Get the background color property
                var backgroundColor = computedStyle.backgroundColor;

                // Parse the RGB values from the string
                var rgbArray = backgroundColor.match(/\\d+/g);

                // Convert the RGB values to a formatted string
                var rgbString = \'rgb(\' + rgbArray.join(\', \') + \')\';

                // Return the RGB string
                return rgbString;
            } else {
                console.error(\'Element not found.\');
                return null; // or any default value you want to return
            }
        }
    }")

mst_bg <- paste0("// Function to get the RGB color from a specific sub-element with a specific ID and class
function getBackgroundColor() {
  // Check the state of the Shiny checkbox 
  var checkboxStatus = $('#mst_background_transparent').prop('checked');
                 
                 // If the checkbox is checked, return transparent RGB code
                 if (checkboxStatus) {
                   return \'rgb(255, 255, 255)\';
        } else {
            // Get the sub-element by ID and class
            var targetElement = document.querySelector(\'#mst_background_color input.form-control.pickr-color\');

            // Check if the element is found
            if (targetElement) {
                // Get the computed style of the element
                var computedStyle = window.getComputedStyle(targetElement);

                // Get the background color property
                var backgroundColor = computedStyle.backgroundColor;

                // Parse the RGB values from the string
                var rgbArray = backgroundColor.match(/\\d+/g);

                // Convert the RGB values to a formatted string
                var rgbString = \'rgb(\' + rgbArray.join(\', \') + \')\';

                // Return the RGB string
                return rgbString;
            } else {
                console.error(\'Element not found.\');
                return null; // or any default value you want to return
            }
        }
    }")

png_mst <- paste0(c(
  "$(document).ready(function(){",
  "  $('#save_plot_png').on('click', function(){",
  "    var el = document.querySelector('canvas');",
  "    // Clone the chart to add a background color.",
  "    var cloneCanvas = document.createElement('canvas');",
  "    cloneCanvas.width = el.width;",
  "    cloneCanvas.height = el.height;",
  "    var ctx = cloneCanvas.getContext('2d');",
  "    ctx.fillStyle = getBackgroundColorClear();",
  "    ctx.fillRect(0, 0, el.width, el.height);",
  "    ctx.drawImage(el, 0, 0);",
  "    // Download.",
  "    const a = document.createElement('a');",
  "    document.body.append(a);",
  "    a.download = ", shQuote(paste0(Sys.Date(), "_MST")),
  "    a.href = cloneCanvas.toDataURL('image/png');",
  "    a.click();",
  "    a.remove();",
  "    cloneCanvas.remove();",
  "  });",
  "});"
), collapse = "\n")


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
        src = "PhyloTrace.jpg", width = 190
      )
    )
  ),
  disable = FALSE),
  
  ## Sidebar ----
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "mycss.css")
    ),
    tags$style("label{color: white;}"),
    tags$style(".white {color: white; font-size: 16px}"),
    tags$style(".main-sidebar .sidebar .sidebar-menu .treeview-menu a {color: #ffffff !important; margin-left: 25px; border-radius: 20px; margin-top: 7px; margin-bottom: 7px}"),
    tags$style(".main-sidebar .sidebar .sidebar-menu a {border: none}"),
    tags$style(".main-sidebar .sidebar .sidebar-menu .treeview-menu li.active a {color: #000000 !important; border-radius: 20px; margin-top: 7px; margin-bottom: 7px}"),
    tags$style(".main-sidebar .sidebar .sidebar-menu .treeview-menu li:hover a {color: #000000 !important; border-radius: 20px; margin-top: 7px; margin-bottom: 7px}"),
    tags$style(".main-sidebar .sidebar .sidebar-menu li:hover a {color: #000000; border: none}"),
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
    tags$style("i.fas {margin-right: 5px;}"),
    tags$style("i.fas.fa-rotate {position: relative; left: -5px;}"),
    tags$style("i.far {margin-right: 5px}"),
    tags$style("i.fas.fa-sliders {margin-right: 0px; margin-top: 5px}"),
    tags$style("i.fas.fa-xmark {margin-right: 0px}"),
    tags$style("i.fas.fa-download {margin-right: 0px}"),
    tags$style("i.far.fa-bookmark {margin-right: 0px}"),
    tags$style("i.fas.fa-triangle-exclamation {color: yellow}"),
    tags$style("i.far.fa-file-lines {margin-left: 2px; margin-right: 8px;}"),
    tags$style("button#reload_db.btn.btn-default.action-button.shiny-bound-input {height: 30px; width: 30px; position: relative; left: -20px}"),
    tags$style("button#edit_button.btn.btn-default.action-button.shiny-bound-input {background: #20E6E5; color: #000000}"),
    tags$style("button#save_plot_jpeg {font-size: 14px; height: 34px; background: #20E6E5; color: #000000; position: relative; top: 26px; right: 20px}"),
    tags$style("button#save_plot_png {font-size: 14px; height: 34px; background: #20E6E5; color: #000000; position: relative; top: 26px; right: 20px}"),
    tags$style("button#save_plot_bmp {font-size: 14px; height: 34px; background: #20E6E5; color: #000000; position: relative; top: 26px; right: 20px}"),
    tags$style("button#save_plot_html_bttn {font-size: 14px; height: 34px; background: #20E6E5; color: #000000; position: relative; top: 26px; right: 20px}"),
    tags$style("button#download_nj_bttn {font-size: 14px; height: 34px; background: #20E6E5; color: #000000; position: relative; top: 26px; right: 20px}"),
    tags$style("button#save_plot_merged {font-size: 14px; height: 34px; background: #20E6E5; color: #000000; position: relative; top: 26px; right: 20px}"),
    tags$style("#nj_scale {position: relative; right: -10px"),
    tags$style(".irs.irs--shiny.js-irs-0 {margin-right: -15px"),
    tags$style(".irs.irs--shiny.js-irs-1 {margin-right: -15px"),
    tags$style(".irs.irs--shiny.js-irs-2 {margin-right: -15px"),
    br(), br(),
    uiOutput("loaded_scheme"),
    sidebarMenu(
      id = "tabs",
      uiOutput("menu_sep1"),
      sidebarMenuOutput("menu"),
      uiOutput("menu_sep2"),
      conditionalPanel(
        "input.tabs==='db_browse_entries'",
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
        "input.tabs==='db_distmatrix'",
        column(
          width = 12,
          align = "left",
          br(), br(),
          selectInput(
            "distmatrix_label",
            label = "Variable",
            choices = c("Index", "Assembly Name", "Assembly ID"),
            selected = c("Assembly Name"),
            width = "90%"
          ),
          checkboxInput(
            "distmatrix_triangle",
            "Show upper triangle",
            value = FALSE
          ),
          fluidRow(
            tags$style("button#download_distmatrix_bttn {height: 34px; ; background: #20E6E5; color: #000000"),
            column(
              width = 4,
              actionButton(
                "distmatrix_change",
                label = "Apply"
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
      ),
      conditionalPanel(
        "input.tabs==='typing'",
        column(
          width = 12,
          align = "center",
          br(), br(),
          p(
            HTML(
              paste(
                tags$span(style='color: white; font-size: 18px; margin-bottom: 0px', 'Typing mode')
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
          tags$style("button#save_rep_profile i.far.fa-bookmark {margin-right: 5px}"),
          actionButton(
            "save_rep_profile",
            label = "Save Profile",
            icon = icon("bookmark")
          )
        ),
        column(
          width = 12,
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
                uiOutput("slot4_status")
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
      ),
      conditionalPanel(
        "input.tabs=='visualization'",
        column(
          width = 12,
          br(),
          prettyRadioButtons(
            "tree_algo",
            choices = c("Minimum-Spanning", "Neighbour-Joining"),
            label = ""
          ),
          br(),
          column(
            tags$style("button#create_tree {position: relative; left: -15px; width = 100%; border: none;"),
            tags$style("button#create_tree:hover {background: #3c8c56; border: none;}"),
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
          ), 
          conditionalPanel(
            "input.tree_algo=='Minimum-Spanning'",
            fluidRow(
              column(
                width = 8,
                selectInput(
                  inputId = "plot_format",
                  label = "",
                  choices = c("html", 
                              "jpeg", "png", "bmp")
                )
              ),
              column(
                width = 4,
                align = "left",
                conditionalPanel(
                  "input.plot_format=='jpeg'",
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
                  "input.plot_format=='png'",
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
                  "input.plot_format=='bmp'",
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
                  "input.plot_format=='html'",
                  downloadBttn(
                    "save_plot_html",
                    style = "simple",
                    label = "",
                    size = "sm",
                    icon = icon("download"),
                    color = "primary"
                  )
                ),
                conditionalPanel(
                  "input.mst_color_var==true",
                  actionBttn(
                    "save_plot_merged",
                    style = "simple",
                    label = "",
                    size = "sm",
                    icon = icon("download")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            "input.tree_algo=='Neighbour-Joining'",
            column(
              width = 12,
              fluidRow(
                column(
                  width = 3,
                  align = "left",
                  br(),
                  HTML(
                    paste(
                      tags$span(style='color: white; font-size: 14px; position: relative; bottom: -28px; margin-left: 0px ', "Zoom")
                    )
                  )
                ),
                column(
                  width = 9,
                  align = "right",
                  br(),
                  sliderTextInput(
                    "nj_scale",
                    label = NULL,
                    choices = seq(0.5, 1.5, 0.05),
                    selected = 0.95,
                    hide_min_max = TRUE
                  )
                )
              ),
              fluidRow(
                column(
                  width = 3,
                  align = "left",
                  HTML(
                    paste(
                      tags$span(style='color: white; font-size: 14px; position: relative; bottom: -28px; margin-left: 0px ', 'X Pos')
                    )
                  )
                ),
                column(
                  width = 9,
                  align = "right",
                  sliderTextInput(
                    "nj_h",
                    label = NULL,
                    choices = seq(-0.5, 0.5, 0.01),
                    selected = 0,
                    hide_min_max = TRUE
                  )
                )
              ),
              fluidRow(
                column(
                  width = 3,
                  align = "left",
                  HTML(
                    paste(
                      tags$span(style='color: white; font-size: 14px; position: relative; bottom: -28px; margin-left: 0px ', 'Y Pos')
                    )
                  )
                ),
                column(
                  width = 9,
                  align = "right",
                  sliderTextInput(
                    "nj_v",
                    label = NULL,
                    choices = seq(-0.5, 0.5, 0.01),
                    selected = 0,
                    hide_min_max = TRUE
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 8,
                selectInput(
                  inputId = "filetype_nj",
                  label = "",
                  choices = c("jpeg", "png", "svg")
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
    tags$style(".image {height: 300px;}"),
    tags$style(".select_start_radio {position: relative; right: -55px;}"),
    uiOutput("start_message"),
    
    tabItems(
      
      ## Tab Database ----
      
      ### Tab Browse Entries ----
      
      tabItem(
        tabName = "db_browse_entries",
        fluidRow(column(
          width = 3,
          align = "center",
          h2(p("Browse Local Database"), style = "color:white")
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
          column(
            width = 3,
            align = "left",
            uiOutput("delete_box"),
            uiOutput("compare_allele_box")
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
            h2(p("Browse Local Database"), style = "color:white")
          )
        ),
        hr(), br(), br(), br(),
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
      
      ### Tab Distance Matrix  ----  
      
      tabItem(
        tabName = "db_distmatrix",
        fluidRow(
          column(
            width = 3,
            align = "center",
            h2(p("Browse Local Database"), style = "color:white")
          )
        ),
        hr(), br(), br(), br(),
        fluidRow(
          tags$style("div#db_distancematrix.rhandsontable {font-size: 11px}"),
          column(1),
          column(
            width = 10,
            div(
              class = "distmatrix",
              rHandsontableOutput("db_distancematrix")
            ),
            br(),
            br()
          ),
          br() 
        ),
        br(), br(), br()
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
        fluidRow(column(
          width = 3,
          align = "center",
          h2(p("Select cgMLST Scheme"), style = "color:white")
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
            tags$style("button#download_cgMLST i.fas.fa-download {margin-right: 5px !important}"),
            br(),
            br(),
            br(),
            actionButton(
              "download_cgMLST",
              label = "Download",
              icon = icon("download")
            )
          ),
          column(width = 1),
          column(
            width = 5,
            br(),
            br(),
            br(),
            align = "center",
            h4(p("Downloaded Loci"), style = "color:white")
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
          h2(p("Generate Allelic Profile"), style = "color:white")
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
          tags$script(HTML(jpeg_mst)),
          tags$script(HTML(png_mst)),
          tags$script(HTML(bmp_mst)),
          tags$script(HTML(mergeCanvas_bmp)),
          tags$script(HTML(mst_bg)),
          tags$script(HTML(mst_bg_clear)),
          column(
            width = 12,
            br(),
            conditionalPanel(
              "input.tree_algo=='Minimum-Spanning'",
              visNetworkOutput("tree_mst", width = "100%", height = "700px")  
            ),
            conditionalPanel(
              "input.tree_algo=='Neighbour-Joining'",
              plotOutput("tree_nj", width = "100%", height = "700px")  
            ),
          )
        ),
        br(),
        hr(),
        
        ### Control panels MST ----
        conditionalPanel(
          "input.tree_algo=='Minimum-Spanning'",
          fluidRow(
            tags$style("button#mst_node_menu {height: 34px; background: #20E6E5; color: #000000; border-radius: 5px; margin-top: 20px}"),
            tags$style("button#mst_edge_menu {height: 34px; background: #20E6E5; color: #000000; border-radius: 5px}"),
            tags$style("button#mst_title_menu {height: 34px; background: #20E6E5; color: #000000; margin-top: 20px; border-radius: 5px}"),
            tags$style("button#mst_edgelabel_menu {height: 34px; background: #20E6E5; color: #000000; margin-top: 20px; border-radius: 5px}"),
            tags$style("button#mst_edgecolor_menu {height: 34px; background: #20E6E5; color: #000000; margin-top: 20px; border-radius: 5px}"),
            tags$style("button#mst_subtitle_menu {height: 34px; background: #20E6E5; color: #000000; margin-top: 20px; border-radius: 5px}"),
            tags$style("button#mst_footer_menu {height: 34px; background: #20E6E5; color: #000000; margin-top: 20px; border-radius: 5px}"),
            tags$style("button#mst_label_menu {height: 34px; background: #20E6E5; color: #000000; margin-top: 20px; border-radius: 5px}"),
            tags$style("input.form-control.pickr-color {text-align: center; font-size: 11px;}"),
            tags$style(".checkbox_bg .checkbox {margin-top: 25px !important;}"),
            tags$style(".label_sel {margin-bottom: -16px;}"),
            tags$style(".slider {margin-bottom: -6px;}"),
            tags$style(".slider_edge {margin-top: -10px;}"),
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
                                selected = "#ffffff",
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
                                  width = "100%"
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
                                selected = "#ffffff",
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
                                  width = "100%"
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
                               selected = "#ffffff",
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
                                 "mst_title_size",
                                 label = h5("Size", style = "color:white; margin-bottom: 0px;"),
                                 value = 15,
                                 min = 10,
                                 max = 30,
                                 step = 1,
                                 width = "100%"
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
                                 label = "Transparent",
                                 value = TRUE
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
                        selectInput(
                          "mst_node_label",
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
                      ),
                      fluidRow(
                        column(
                          width = 7,
                          colorPickr(
                            inputId = "node_font_color",
                            width = "100%",
                            selected = "#ffffff",
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
                              value = 1,
                              min = 0.1,
                              max = 5,
                              step = 0.1
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
                                  label = "Add variable",
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
                                    selected = "#84D9A0",
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
                                    width = "100%"
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
                    width = 12,
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
                            choices = 1:100,
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
                            selected = "#ffffff",
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
                              value = 20,
                              step = 1,
                              min = 8,
                              max = 30,
                              width = "100%"
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
                                  selected = "#ffffff",
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
                                  width = "100%"
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
                    width = 12,
                    fluidRow(
                      column(
                        width = 12,
                        align = "left",
                        h4(p("Size multiplier"), style = "color:white; position: relative; right: -15px")
                      )
                    ),
                    column(
                      width = 12,
                      align = "center",
                      br(),
                      div(
                        class = "slider_edge",
                        sliderTextInput(
                          inputId = "mst_edge_length",
                          label = NULL,
                          choices = 1:40,
                          selected = c(10),
                          hide_min_max = TRUE
                        ) 
                      ),  
                      br(), br(), br(), br(), br()
                    )
                  )
                )
              )
            )
          )
        ),
        
        ### Control Panels NJ ----
        
        conditionalPanel(
          "input.tree_algo=='Neighbour-Joining'",
          fluidRow(
            tags$style("button#nj_labeltext_menu {height: 34px; background: #20E6E5; color: #000000; border-radius: 5px; margin-top: 20px; margin-left: 10px}"),
            tags$style("button#nj_labelformat_menu {height: 34px; background: #20E6E5; color: #000000; border-radius: 5px; margin-top: 20px}"),
            tags$style("button#nj_subtitle_menu {height: 34px; background: #20E6E5; color: #000000; margin-top: 20px; border-radius: 5px}"),
            tags$style("button#nj_footer_menu {height: 34px; background: #20E6E5; color: #000000; margin-top: 20px; border-radius: 5px}"),
            tags$style("button#nj_title_menu {height: 34px; background: #20E6E5; color: #000000; margin-top: 20px; border-radius: 5px}"),
            tags$style("button#nj_branch_label_menu {height: 34px; background: #20E6E5; color: #000000; margin-top: 20px; border-radius: 5px; margin-left: 10px}"),
            tags$style("button#nj_legend_menu {height: 34px; background: #20E6E5; color: #000000; margin-top: 20px; border-radius: 5px}"),
            tags$style("button#nj_treescale_menu {height: 34px; background: #20E6E5; color: #000000; margin-top: 20px; border-radius: 5px}"),
            tags$style("button#nj_rootedge_menu {height: 34px; background: #20E6E5; color: #000000; margin-left: 10px; margin-top: 6px; border-radius: 5px}"),
            tags$style("button#nj_tippoint_menu {height: 34px; background: #20E6E5; color: #000000; margin-top: 21px; border-radius: 5px}"),
            tags$style("button#nj_nodepoint_menu {height: 34px; background: #20E6E5; color: #000000; margin-top: 21px; border-radius: 5px}"),
            tags$style("button#nj_tile_menu {height: 34px; background: #20E6E5; color: #000000; margin-left: 15px; margin-top: 21px; border-radius: 5px}"),
            tags$style("#nj_tippoint_color {margin-bottom: 16px}"),
            tags$style("#nj_nodepoint_color {margin-bottom: 16px;}"),
            tags$style("#nj_treescale_show {margin-top: 17px; "),
            tags$style("#nj_tiles_show {margin-top: 18px; margin-left: -5px}"),
            tags$style("#nodepoint_show {margin-top: 18px; margin-left: -5px}"),
            tags$style("#nj_ladder {margin-top: 7px; margin-left: -5px}"),
            tags$style("#nj_align {margin-top: 21px; margin-left: -5px}"),
            tags$style("#rootedge_show {margin-top: 7px}"),
            tags$style("#nj_mapping_show {margin-top: 20px;}"),
            tags$style("#nj_tipcolor_mapping_show {margin-top: 20px;}"),
            tags$style("#nj_tipshape_mapping_show {margin-top: 20px;}"),
            tags$style("#nj_geom {margin-top: 0px; margin-left: -5px}"),
            tags$style("#nj_tiplab_color {margin-bottom: 16px;}"),
            tags$style("#nj_show_branch_label {margin-top: 17px; margin-left: -5px}"),
            tags$style("#tippoint_show {margin-top: 17px; margin-left: -5px}"),
            tags$style("#nj_tiplab_show {margin-top: 17px; margin-left: -5px}"),
            tags$style("#tippoint_show {margin-top: 17px; margin-left: -5px}"),
            tags$style("#nj_fruit_offset {position: relative; top: -20px}"),
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
                                                "Inward" = "inward"),
                                Unrooted = list("Daylight" = "daylight",
                                                "Equal Angle" = "equal_angle")
                              ),
                              selected = "rectangular",
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
                              h5(p("Ladder"), style = "color:white; position: relative; bottom: 3px; right: -15px"),
                              value = TRUE
                            )
                          ),
                          column(
                            width = 3,
                            align = "left",
                            checkboxInput(
                              "rootedge_show",
                              h5(p("Root"), style = "color:white; position: relative; bottom: 3px; right: -5px"),
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
                                    width = "70px"
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
                                  selected = 70,
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
                                      width = "100%"
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
                                      width = "100%"
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
                        h4(p(tags$b("Tree scale")), style = "color:white; position: relative; right: -15px"),
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
                                      width = "70px"
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
                              width = 12,
                              align = "left",
                              prettyRadioButtons(
                                "nj_legend_orientation",
                                "",
                                choices = c(Horizontal = "horizontal",
                                            Vertical = "vertical"),
                                selected = c(Horizontal = "horizontal"),
                                inline = TRUE,
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 7,
                              selectInput(
                                "nj_legend_position",
                                "",
                                choices = c(Top = "top", 
                                            Right = "right", 
                                            Bottom = "bottom", 
                                            Left = "left"),
                                selected = "bottom"
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
                                      width = "100%"
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
                        selectInput(
                          "nj_tiplab",
                          label = "",
                          choices = c(
                            Index = "index",
                            `Assembly ID` = "assembly_id",
                            `Assembly Name` = "assembly_name",
                            `Isolation Date` = "Isolation_Date",
                            Host = "Host",
                            Country = "Country",
                            City = "City"
                          ),
                          selected = c(`Assembly Name` = "assembly_name"),
                          width = "100%"
                        )
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
                                width = "70px"
                              ),
                              br(),
                              numericInput(
                                "nj_tiplab_alpha",
                                label = h5("Opacity", style = "color:white; margin-bottom: 0px"),
                                min = 0.1,
                                max = 1,
                                value = 1,
                                width = "70px"
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
                              sliderTextInput(
                                inputId = "nj_tiplab_nudge_x",
                                label = h5("X Nudge", style = "color:white; margin-bottom: 0px"),
                                choices = seq(-3, 3, by = 0.1),
                                selected = 0,
                                width = "250px",
                                hide_min_max = TRUE
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
                      selectInput(
                        "nj_color_mapping",
                        "",
                        choices = c(
                          "Isolation Date" = "Isolation_Date",
                          "Host" = "Host",
                          "Country" = "Country",
                          "City" = "City"
                        ),
                        selected = c("Host" = "Host"),
                        width = "100%"
                      )
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
                            width = "70px"
                          ),
                          br(),
                          numericInput(
                            inputId = "nj_tiplab_labelradius",
                            label = h5("Smooth edge", style = "color:white; margin-bottom: 0px"),
                            min = 0,
                            max = 0.5,
                            step = 0.05,
                            value = 0.2
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
                        selectInput(
                          "nj_branch_label",
                          "",
                          choices = c(
                            "Isolation Date" = "Isolation_Date",
                            "Host" = "Host",
                            "Country" = "Country",
                            "City" = "City"
                          ),
                          selected = c("Country" = "Country"),
                          width = "100%"
                        )
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
                                width = "70px"
                              ),
                              br(),
                              numericInput(
                                "nj_branch_labelradius",
                                label = h5("Smooth edge", style = "color:white; margin-bottom: 0px"),
                                value = 0.5,
                                step = 0.05,
                                min = 0,
                                max = 0.5
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
                                width = "70px"
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
                br(), br(), br()
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
                          "tippoint_show",
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
                                width = "70px"
                              ), 
                              br(),
                              numericInput(
                                inputId = "nj_tippoint_size",
                                label = h5("Size", style = "color:white; margin-bottom: 0px"),
                                min = 1,
                                max = 20,
                                step = 1,
                                value = 5
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
                          selectInput(
                            "nj_tipcolor_mapping",
                            "",
                            choices = c(
                              "Assembly Name" = "assembly_name",
                              "Isolation Date" = "Isolation_Date",
                              "Host" = "Host",
                              "Country" = "Country",
                              "City" = "City"
                            ),
                            selected = c("Country" = "Country"),
                            width = "100%"
                          )
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
                          selectInput(
                            "nj_tipshape_mapping",
                            "",
                            choices = c(
                              "Isolation Date" = "Isolation_Date",
                              "Host" = "Host",
                              "Country" = "Country",
                              "City" = "City"
                            ),
                            selected = c("Country" = "Country"),
                            width = "100%"
                          )
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
                              "nodepoint_show",
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
                                    width = "70px"
                                  ), 
                                  br(),
                                  numericInput(
                                    inputId = "nj_nodepoint_size",
                                    label = h5("Size", style = "color:white; margin-bottom: 0px"),
                                    min = 1,
                                    max = 20,
                                    step = 1,
                                    value = 5
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
                        h4(p("Tiles"), style = "color:white; position: relative; right: -15px"),
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
                              placement = "top-start",
                              theme = "translucent",
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
                                    value = 0.5,
                                    width = "70px"
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
                                tags$span(style='color: white; font-size: 14px; margin-left: 16px; position: relative; bottom: -28px', 'Color')
                              )
                            )
                          ),
                          column(
                            width = 7,
                            align = "center",
                            selectInput(
                              "nj_fruit_variable",
                              "",
                              choices = c(
                                "Assembly Name" = "assembly_name",
                                "Isolation Date" = "Isolation_Date",
                                "Host" = "Host",
                                "Country" = "Country",
                                "City" = "City"
                              ),
                              selected = c("Country" = "Country"),
                              width = "100%"
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 3,
                            h5(p("Width"), style = "color:white; position: relative; right: -15px; margin-top: 40px")
                          ),
                          column(1),
                          column(
                            width = 7,
                            align = "right",
                            sliderTextInput(
                              "nj_fruit_width",
                              label = "",
                              choices = 1:100,
                              selected = 5,
                              hide_min_max = TRUE
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 3,
                            h5(p("Position"), style = "color:white; position: relative; right: -15px; margin-top: 40px")
                          ),
                          column(1),
                          column(
                            width = 7,
                            align = "right",
                            sliderTextInput(
                              "nj_fruit_offset",
                              label = "",
                              choices = seq(-1.5, 1.5, 0.05),
                              selected = 0,
                              hide_min_max = TRUE
                            )
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
        br(), br(), br(), br(), br(), br(), br()
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
            tags$style("button#save_report {font-size: 14px; height: 34px; background: #20E6E5; color: #000000;}"),
            tags$style("button#save_report i.fas.fa-download {margin-right: 5px !important}"),
            uiOutput("save_rep")
          )
        )
      )
    ) # End tabItems
  ) # End dashboardPage
) # end UI



# Server ----

server <- function(input, output, session) {
  
  ## Functions ----
  # Function to compute Hamming distance between two vectors
  hamming_distance <- function(x, y) {
    sum(x != y)
  }
  
  hamming_distance_with_na <- function(x, y) {
    sum((is.na(x) | is.na(y)) | (x != y))
  }
  
  observeEvent(input$test, {
    html_name <- tempfile(fileext = ".html")
    visSave(mst_tree(), html_name)
    webshot(html_name, zoom = 2, file = "ex1.png")
  })
    
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
      br(), br(), br(), br(), br(), br(),
      div( 
        class = "image",
        imageOutput("imageOutput")
      ),
      br(),
      p(
        HTML(
          paste(
            tags$span(style='color: white; font-size: 24px;', 'Welcome to PhyloTrace!')
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
    image_path <- paste0(getwd(), "/www/PhyloTrace.png")
    
    # Use HTML to display the image with the <img> tag
    list(src = image_path,
         height = 200)
  }, deleteFile = FALSE)
  
  
  ## Database ----
  DF1 <- reactiveValues()
  database <- reactiveValues()
  
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
      Database <-
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
      
      DF1$data <- Database[["Typing"]]
      
      DF1$meta <- select(DF1$data, 1:12)
      
      DF1$meta_true <- DF1$meta[which(DF1$data$Include == TRUE),]
        
      DF1$allelic_profile <- select(DF1$data, -(1:12))
      
      DF1$allelic_profile_true <- DF1$allelic_profile[which(DF1$data$Include == TRUE),]
      
      DF1$scheme <- input$scheme_db
      
      #### Render Menu Items ----
      
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
      
      if(!anyNA(DF1$allelic_profile)) {
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
            ),
            menuItem(
              text = "Report",
              tabName = "report",
              icon = icon("file-lines")
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
            ),
            menuItem(
              text = "Report",
              tabName = "report",
              icon = icon("file-lines")
            )
          )
        )
      }
      
      output$start_message <- NULL
      
      output$compare_select <- renderUI({
        
        if (input$compare_difference == FALSE) {
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
      })
      
      
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
                  rowHeaders = NULL
                ) %>%
                  hot_col(1:(12+length(input$compare_select)), valign = "htMiddle") %>%
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
                  rowHeaders = NULL
                ) %>%
                  hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
                  hot_col(2,
                          halign = "htCenter",
                          valign = "htTop",
                          width = "auto") %>%
                  hot_context_menu(allowRowEdit = FALSE,
                                   allowColEdit = FALSE,
                                   allowReadOnly = FALSE) %>%
                  hot_rows(fixedRowsTop = 0)
              })
            }
          } else {
            if (length(input$compare_select) > 0) {
              output$db_entries <- renderRHandsontable({
                rhandsontable(
                  select(DF1$data, 1:12, input$compare_select),
                  col_highlight = diff_allele()-1,
                  rowHeaders = NULL,
                  height = 900
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
                  height = 900
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
                  hot_rows(fixedRowsTop = 0)
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
          var_alleles(select(DF1$data, input$compare_select)) + 12
        }
      })
      
      ### Other UI Elements ----
      
      #### Missing Values UI ----
      
      # NA Table
      NA_table <- DF1$allelic_profile[, colSums(is.na(DF1$allelic_profile)) != 0]
      
      NA_table <- NA_table[rowSums(is.na(NA_table)) != 0,]
      
      NA_table[is.na(NA_table)] <- "NA"
      
      NA_table <- NA_table %>% 
        cbind("Assembly Name" = DF1$meta[rownames(NA_table),]$`Assembly Name`) %>%
        cbind("Errors" = DF1$meta[rownames(NA_table),]$Errors) %>%
        relocate("Assembly Name", "Errors")
      
      output$table_missing_values <- renderRHandsontable({
        rhandsontable(
          NA_table,
          rowHeaders = NULL
        ) %>%
          hot_context_menu(allowRowEdit = FALSE,
                           allowColEdit = FALSE,
                           allowReadOnly = TRUE) %>%
          hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
          hot_rows(fixedRowsTop = 0) 
      })
        
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
              checkboxInput(
                "compare_difference",
                label = h5("Only varying loci", style = "color:white; position: relative; top: -6px"),
                value = TRUE
              )
            )
          ),
          br()
        )
      })
      
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
                choiceNames = c("Loci containing missing values are ignored (default)",
                                "Missing values are allelic difference of 1",
                                "Missing values are an own category"),
                choiceValues = c("ignore", "one", "category"),
                shape = "curve",
                selected = c("ignore")
              ),
              br()
            )
          )
        )
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
      
      #### Distance Matrix ----
      
      
      
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
      
      output$db_entries <- NULL
      output$edit_index <- NULL
      output$edit_scheme_d <- NULL
      output$edit_entries <- NULL
      output$compare_select <- NULL
      output$delete_select <- NULL
      output$del_bttn <- NULL
      output$compare_allele_box <- NULL
      output$missing_values <- NULL
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
  
  
  ### Distance Matrix ---- 
  
  observe({
    if(!class(DF1$data) == "NULL") {
      output$db_distancematrix <- renderRHandsontable({
        rhandsontable(hamming_df(), digits = 1, height = 800, rowHeaders = NULL) %>%
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
          hot_col(1:(dim(DF1$ham_matrix)[1]),
                  halign = "htCenter") %>%
          hot_col(1,
                  renderer = "
                function(instance, td, row, col, prop, value, cellProperties) {
                  Handsontable.renderers.NumericRenderer.apply(this, arguments);

                    td.style.background = '#F0F0F0'
              }"
          ) 
      })
    }
  })
  
  hamming_df <- reactive({
    # Create a custom proxy object for Hamming distance
    if(input$na_handling == "one") {
    hamming_proxy <- proxy::dist(DF1$allelic_profile_true, method = hamming_distance_with_na)
    } else if(input$na_handling == "ignore"){
      allelic_profile_noNA <- DF1$allelic_profile_true[, colSums(is.na(DF1$allelic_profile_true)) == 0]
      
      hamming_proxy <- proxy::dist(allelic_profile_noNA, method = hamming_distance)
    }
    
    hamming_matrix <- as.matrix(hamming_proxy)
    
    DF1$matrix_min <- min(hamming_matrix, na.rm = TRUE)
    DF1$matrix_max <- max(hamming_matrix, na.rm = TRUE)
    
    # Convert the proxy object to a matrix
    hamming_matrix[upper.tri(hamming_matrix, diag = TRUE)] <- NA
    
    # Rownames change
    rownames(hamming_matrix) <- select(DF1$data, 1:12)[rownames(select(DF1$data, 1:12)) %in% rownames(hamming_matrix), 4]
    colnames(hamming_matrix) <- rownames(hamming_matrix)
    
    mode(hamming_matrix) <- "integer"
    
    DF1$ham_matrix <- hamming_matrix %>%
      as.data.frame() %>%
      mutate(Index = colnames(hamming_matrix)) %>%
      relocate(Index)
    DF1$ham_matrix
  })
  
  observeEvent(input$distmatrix_change, {
    
    allelic_profile <- select(DF1$data, -(1:12))
    
    allelic_profile <- allelic_profile[which(DF1$data$Include == TRUE),]
    
    allelic_profile_noNA <- allelic_profile[, colSums(is.na(allelic_profile)) == 0]
    
    # Create a custom proxy object for Hamming distance
    hamming_proxy <- proxy::dist(allelic_profile_noNA, method = hamming_distance)
    
    hamming_matrix <- as.matrix(hamming_proxy)
    
    if(input$distmatrix_triangle == FALSE) {
      # Convert the proxy object to a matrix
      hamming_matrix[upper.tri(hamming_matrix)] <- NA
    }
    
    # Rownames change
    rownames(hamming_matrix) <- select(DF1$data, 1:12)[rownames(select(DF1$data, 1:12)) %in% rownames(hamming_matrix),input$distmatrix_label]
    colnames(hamming_matrix) <- rownames(hamming_matrix)
   
    mode(hamming_matrix) <- "integer"
    
    hamming_df <- hamming_matrix %>%
      as.data.frame() %>%
      mutate(Index = colnames(hamming_matrix)) %>%
      relocate(Index)
    
    output$db_distancematrix <- renderRHandsontable({
      rhandsontable(hamming_df, digits = 1, height = 800, rowHeaders = NULL) %>%
        hot_heatmap(color_scale = c("#17F556","#ED6D47")) %>%
        hot_rows(fixedRowsTop = 0) %>%
        hot_cols(fixedColumnsLeft = 1) %>%
        hot_col(1:(dim(hamming_matrix)[1]),
                halign = "htCenter") %>%
        hot_col(1,
                renderer = "
                function(instance, td, row, col, prop, value, cellProperties) {
                  Handsontable.renderers.NumericRenderer.apply(this, arguments);

                    td.style.background = '#F0F0F0'
              }"
        ) 
    })
  })
  
  output$download_distmatrix <- downloadHandler(
    filename = function() {
      paste0("distance_matrix.csv")
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
  
  plot_loc <- reactiveValues(cluster = NULL, metadata = list())
  
  
  ### Plot Reactives ----
  
  #### MST ----
  
  mst_tree <- reactive({
    data <- toVisNetworkData(plot_loc$ggraph_1)
    data$nodes <- mutate(data$nodes, 
                         label = label_mst(),
                         value = mst_node_scaling(),
                         font.vadjust = plot_loc$unique_meta$valign,
                         font.size = plot_loc$unique_meta$font_size * node_label_fontsize(),
                         opacity = node_opacity())
    data$edges <- mutate(data$edges, 
                         length = (weight*mst_edge_length()), 
                         label = as.character(weight),
                         opacity = mst_edge_opacity())
    
    visNetwork(data$nodes, data$edges, 
               main = mst_title(),
               background = mst_background_color(),
               submain = mst_subtitle(),
               footer = mst_footer()) %>%
      visNodes(size = mst_node_size(), 
               color = mst_color_node(),
               scaling = list(min = mst_node_size_min(), 
                              max = mst_node_size_max()),
               font = list(color = node_font_color())) %>%
      visEdges(color = mst_color_edge(), 
               font = list(color = mst_edge_font_color(),
                           size = mst_edge_font_size())) %>%
      visOptions(collapse = TRUE) %>%
      visInteraction(hover = TRUE) %>%
      visLayout(randomSeed = 1) %>%
      visLegend()
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
  
  # Node Label Size
  node_label_fontsize <- reactive(
    input$node_label_fontsize
  )
  
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
  
  # Edge length multiplicator
  mst_edge_length <- reactive({
    input$mst_edge_length
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
            legend.position = input$nj_legend_position,
            legend.title = element_text(color = input$nj_color,
                                        size = input$nj_legend_size*1.2),
            legend.title.align = 0.5,
            legend.text = element_text(color = input$nj_color, 
                                       size = input$nj_legend_size),
            legend.key = element_rect(fill = input$nj_bg),
            legend.box.spacing = unit(1.5, "cm"),
            plot.background = element_rect(fill = input$nj_bg)) +
      fruit() 
    
    plot_loc$nj_plot <- ggplotify::as.ggplot(tree, 
                                             scale = input$nj_scale,
                                             hjust = input$nj_h,
                                             vjust = input$nj_v)  
    plot_loc$nj_plot
  })
  
  # Geom Fruit
  fruit <- reactive({
    if(input$nj_tiles_show == TRUE) {
      geom_fruit(
        geom = geom_tile,
        mapping = aes_string(fill=input$nj_fruit_variable),
        width = input$nj_fruit_width,
        offset = input$nj_fruit_offset,
        alpha = input$nj_fruit_alpha
      )
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
          aes_string(
            x="branch", 
            label= as.character(input$nj_branch_label)),
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
    if(input$tippoint_show == TRUE) {
      if(input$nj_tipcolor_mapping_show == TRUE & input$nj_tipshape_mapping_show == FALSE) {
        if(input$nj_mapping_show == TRUE) {
          geom_tippoint(
            aes_string(shape = input$nj_tipcolor_mapping),
            alpha = input$nj_tippoint_alpha,
            color = input$nj_tippoint_color,
            size = input$nj_tippoint_size
          )
        } else {
          geom_tippoint(
            aes_string(color = input$nj_tipcolor_mapping),
            alpha = input$nj_tippoint_alpha,
            shape = input$nj_tippoint_shape,
            size = input$nj_tippoint_size
          )
        }
      } else if (input$nj_tipcolor_mapping_show == FALSE & input$nj_tipshape_mapping_show == TRUE) {
        geom_tippoint(
          aes_string(shape = input$nj_tipshape_mapping),
          alpha = input$nj_tippoint_alpha,
          color = input$nj_tippoint_color,
          size = input$nj_tippoint_size
        )
      } else if (input$nj_tipcolor_mapping_show == TRUE & input$nj_tipshape_mapping_show == TRUE) {
        if(input$nj_mapping_show == TRUE) {
          geom_tippoint(
            aes_string(shape = input$nj_tipshape_mapping),
            color = input$nj_tippoint_color,
            alpha = input$nj_tippoint_alpha,
            size = input$nj_tippoint_size
          )
        } else {
          geom_tippoint(
            aes_string(shape = input$nj_tipshape_mapping,
                       color = input$nj_tipcolor_mapping),
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
    if(input$nodepoint_show == TRUE) {
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
      if(input$nj_layout == "circular" | input$nj_layout == "inward") {
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
            nudge_x = input$nj_tiplab_nudge_x,
            check.overlap = input$nj_tiplab_overlap
          )
        } else {
          geom_tiplab(
            color = input$nj_tiplab_color,
            geom = "text",
            size = input$tiplab_size,
            linesize = input$nj_tiplab_linesize,
            linetype = input$nj_tiplab_linetype,
            alpha = input$nj_tiplab_alpha,
            fontface = input$nj_tiplab_fontface,
            align = as.logical(input$nj_align),
            nudge_x = input$nj_tiplab_nudge_x,
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
      aes_string(label = as.character(input$nj_tiplab),
                 colour = as.character(input$nj_color_mapping))
    } else {
      aes_string(label = as.character(input$nj_tiplab))
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
        png(file, width = 1365, height = 600)
        print(nj_tree())
        dev.off()
      } else if (input$filetype_nj == "jpeg") {
        jpeg(file, width = 2730, height = 1200, quality = 100)
        print(nj_tree())
        dev.off()
      } else if (input$filetype_nj == "svg") {
        plot <- print(nj_tree())
        ggsave(file=file, plot=plot, device = svg(), width = 50, height = 22, units = "cm")
      }
    }
  )
  
  
  ### Reactive Events ----
  
  #### Generate Plot ----
  
  hamming_nj <- reactive({
    if(input$na_handling == "one") {
      proxy::dist(DF1$allelic_profile_true, method = hamming_distance_with_na)
    } else if(input$na_handling == "ignore"){
      allelic_profile_noNA <- DF1$allelic_profile_true[, colSums(is.na(DF1$allelic_profile_true)) == 0]
      proxy::dist(allelic_profile_noNA, method = hamming_distance)
    }
  })
  
  hamming_mst <- reactive({
    
    if(input$na_handling == "one") {
      
      grouped_df <- DF1$allelic_profile_true %>%
        group_by(across(everything())) %>%
        mutate(group_id = cur_group_id()) %>%
        ungroup() %>%
        relocate(group_id) %>%
        as.data.frame()
      
      group_df <<- grouped_df
      
      rownames(grouped_df) <- meta$`Assembly Name`
       
      group_df_names <<- grouped_df
      
      plot_loc$unique_allelic_profile <- grouped_df[!duplicated(grouped_df$group_id), ]
      
      unique_allelic_profile <<- plot_loc$unique_allelic_profile
      
      meta <- mutate(meta, group_id = grouped_df$group_id) %>%
        relocate(group_id)
      
      meta_test <<- meta
      
      rownames(meta) <- meta$`Assembly Name`
      
      meta_test_names <<- meta
      
      plot_loc$unique_meta <- meta[rownames(plot_loc$unique_allelic_profile), ]
      
      unique_meta_1 <<- plot_loc$unique_meta
      
      ## grouping names
      data_frame <- data.frame(group = numeric(), name = character())
      
      for (i in plot_loc$unique_meta$group_id) {
        data_frame <- rbind(data_frame, 
                            data.frame(size = length(paste(rownames(grouped_df)[which(grouped_df$group_id == i)])), 
                                       name = paste(rownames(grouped_df)[which(grouped_df$group_id == i)], collapse = "\n")))
      }
      
      font_size <- numeric(nrow(data_frame))
      
      for (i in 1:length(font_size)) {
        if(data_frame$size[i] < 3) {
          font_size[i] <- 12
        } else {
          font_size[i] <- 11
        }
      }
      
      valign <- numeric(nrow(data_frame))
      
      for (i in 1:length(valign)) {
        if(data_frame$size[i] == 1) {
          valign[i] <- -30
        } else if(data_frame$size[i] == 2) {
          valign[i] <- -38
        } else if(data_frame$size[i] == 3) {
          valign[i] <- -46
        } else if(data_frame$size[i] == 4) {
          valign[i] <- -54
        } else if(data_frame$size[i] == 5) {
          valign[i] <- -62
        } else if(data_frame$size[i] > 5) {
          valign[i] <- -70
        }
      }
      
      plot_loc$data_frame <- data_frame %>%
        cbind(font_size = font_size, valign = valign)
      
      df_test <<- plot_loc$data_frame
      
      plot_loc$unique_meta$`Assembly Name` <- plot_loc$data_frame$name
      plot_loc$unique_meta <- mutate(plot_loc$unique_meta, size = plot_loc$data_frame$Size)
      
      unique_meta2 <<- plot_loc$unique_meta
      
      proxy::dist(plot_loc$unique_allelic_profile, method = hamming_distance_with_na)
      
    } else if(input$na_handling == "ignore"){
      ########### Loci/Columns with NA are ignored #############
      
      # Remove NA columns
      noNA <- DF1$allelic_profile_true[, colSums(is.na(DF1$allelic_profile_true)) == 0]
      
      # Generate hamming distance matrix
      noNA_dist <- proxy::dist(noNA, method = hamming_distance)
      
      # Find indices of pairs with a distance of 0
      zero_distance_pairs <- as.data.frame(which(as.matrix(noNA_dist) == 0, arr.ind = TRUE))
      
      zero_distance_pairs <- zero_distance_pairs[zero_distance_pairs$row != zero_distance_pairs$col, ]
      
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
      
      
      ### Merging with original data frame / allelic profile
      
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
      
      ### Metadata completion
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
      
      
      ### final dist calculation
      
      allelic_profile_clean_noNA_names <- allelic_profile_clean[, colSums(is.na(allelic_profile_clean)) == 0]
      
      proxy::dist(allelic_profile_clean_noNA_names, method = hamming_distance)
    }
  })
  
  observeEvent(input$create_tree, {
    
    set.seed(1)
    
    if (input$tree_algo == "Neighbour-Joining") {
      
      plot_loc$meta_nj <- select(DF1$meta_true, -2)
      
      colnames(plot_loc$meta_nj) <-
        c(
          "index",
          "assembly_id",
          "assembly_name",
          "scheme",
          "Isolation_Date",
          "Host",
          "Country",
          "City",
          "typing_date",
          "successes",
          "errors"
        )
      
      plot_loc$meta_nj <- mutate(plot_loc$meta_nj, taxa = index) %>%
        relocate(taxa)
      
      # Create phylogenetic tree
      plot_loc$nj <- ape::nj(hamming_nj())
      
      output$tree_nj <- renderPlot({
        nj_tree()
      })
      
    } else {
      
      # prepare igraph object
      plot_loc$ggraph_1 <- hamming_mst() |>
        as.matrix() |>
        graph.adjacency(weighted = TRUE) |>
        igraph::mst() 
      
      output$tree_mst <- renderVisNetwork({
        mst_tree()
      })
      
    }
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
          value = "Analysis",
          placeholder = "Analysis"
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
      if (length(input$slot_name) >= 1) {
        plot_loc$slot1_getname <- substr(input$slot_name, 1, 10)
      } else {
        plot_loc$slot1_getname <- paste("Number One")
      }
      mst_tree() %>% visExport(type = "jpeg")
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
      elements_data$slot1_meta <- DF1$meta_true[,-2]
      elements_data$slot1_legend_sel <- input$slot1_legend_sel
      elements_data$slot1_legend <- input$slot1_legend
    }
    
    if(elements_data$slot2_include == TRUE) {
      elements_data$slot2_meta <- DF1$meta_true[,-2]
      elements_data$slot2_legend_sel <- input$slot2_legend_sel
      elements_data$slot2_legend <- input$slot2_legend
    }
    
    if(elements_data$slot3_include == TRUE) {
      elements_data$slot3_meta <- DF1$meta_true[,-2]
      elements_data$slot3_legend_sel <- input$slot3_legend_sel
      elements_data$slot3_legend <- input$slot3_legend
    }
    
    if(elements_data$slot4_include == TRUE) {
      elements_data$slot4_meta <- DF1$meta_true[,-2]
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
  
  # Typing reactive values
  typing_reactive <- reactiveValues(table = data.frame(), single_path = data.frame(), 
                                    progress = 0, progress_pct = 0, progress_format_start = 0, 
                                    progress_format_end = 0)
  
  # Render Single/Multi Switch
  
  readLogFile <- reactive({
    invalidateLater(5000, session)
    readLines(paste0(getwd(), "/execute/script_log.txt"))
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
            tags$span(style='color: white; font-size: 15px; margin-bottom: 0px', 'Select Assembly File (.fasta)')
          )
        )
      ),
      shinyFilesButton(
        "genome_file",
        "Browse" ,
        icon = icon("file"),
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
        'log_file=', shQuote(paste0(getwd(), "/execute/script_log.txt")), '\n',
        '# Function to log messages to the file', '\n',
        'log_message() {', '\n',
        '    echo "$(date +"%Y-%m-%d %H:%M:%S") - $1" >> "$log_file"', '\n',
        '}', '\n',
        '# Create a log file or truncate if it exists', '\n',
        'echo 0 > ',
        shQuote(paste0(getwd(), "/execute/progress.fifo")),
        "\n",
        'base_path="/home/marian/Documents/Projects/Masterthesis"', '\n',
        'kma_database="$base_path/PhyloTree/execute/kma_single/"', shQuote(paste0(gsub(" ", "_", DF1$scheme))), '\n',
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
        'results=', shQuote(paste0(getwd(),"/execute/kma_single/results")), '\n',
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
        shQuote(paste0(getwd(), "/execute/progress.fifo")), '\n'
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
          br(),
          fluidRow(
            column(
              width = 6,
              HTML(paste("<span style='color: white;'>", "Transforming data..."))
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
                            db_directory = getwd())
    
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
      'log_file=', shQuote(paste0(getwd(), "/execute/script_log.txt")), '\n',
      'echo 0 > ',
      shQuote(paste0(getwd(), "/execute/progress.fifo")), '\n'
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
          "Browse",
          icon = icon("folder-open"),
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
          title = "Please select the folder containing the genome assemblies in .fasta format",
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
      
      output$metadata_multi_box <- renderUI({
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
    
    if (nrow(typing_reactive$table) > 0) {
      output$multi_select_table <- renderRHandsontable({
        rhandsontable(typing_reactive$table, rowHeaders = NULL, stretchH = "all", height = 400) %>%
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
                            append_isodate = input$append_isodate_multi,
                            append_host = input$append_host_multi,
                            append_country = input$append_country_multi,
                            append_city = input$append_city_multi,
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
      timer = 3000
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
      
      reset_multi <- paste0(
        '#!/bin/bash', '\n',
        'log_file=', shQuote(paste0(getwd(), "/execute/script_log.txt")), '\n',
        'echo 0 > $log_file'
      )
      
      # Specify the path to save the script
      reset_multi_path <-
        paste0(getwd(), "/execute/reset_multi.sh")
      
      # Write the script to a file
      cat(reset_multi, file = reset_multi_path)
      
      # Make the script executable  
      system(paste("chmod +x", reset_multi_path))
      
      # Execute the script
      system(paste(reset_multi_path), wait = FALSE)
      
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
              title = "Please select the folder containing the genome assemblies in .fasta format",
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
      timer = 6000
    )
    
    output$test_yes_pending <- NULL
    
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
      'echo 0 > $log_file'
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
            title = "Please select the folder containing the genome assemblies in .fasta format",
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
    
    output$initiate_multi_typing_ui <- NULL
    
    output$metadata_multi_box <- NULL
    
    output$start_multi_typing_ui <-NULL
    
    # List Genome Assemblies Included in Analysis in Vector
    genome_selected <- hot_to_r(input$multi_select_table)
    
    genome_names <<- genome_selected$Files[which(genome_selected$Include == TRUE)]
    
    kma_multi <- paste0(
      '#!/bin/bash', '\n',
      'cd execute', '\n',
      'base_path="/home/marian/Documents/Projects/Masterthesis"', '\n',
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
      'echo "Start Multi Typing" > "$log_file"', '\n',
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
      'results=', shQuote(paste0(getwd(),"/execute/kma_multi/results")), '\n',
      '# Remove the existing directory (if it exists)', '\n',
      'if [ -d "$results" ]; then', '\n',
      '    rm -r "$results"', '\n',
      'fi', '\n',
      '# Create a new directory', '\n', 
      'mkdir "$results"', '\n',
      '#INDEXING GENOME AS DATABASE', '\n',
      'kma_database="$base_path/PhyloTree/execute/kma_multi/"', shQuote(paste0(gsub(" ", "_", DF1$scheme))), '\n',
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
      '    /home/marian/miniconda3/bin/kma index -i "$genome" -o "$kma_database"', '\n',
      '    fi', '\n',
      '    mkdir "$results/$genome_filename_noext"', '\n',
      '#Running Loop', '\n',
      '    for query_file in "$query_folder"/*.fasta; do', '\n',
      '        if [ -f "$query_file" ]; then', '\n',
      '        query_filename=$(basename "$query_file")', '\n',
      '        query_filename_noext="${query_filename%.*}"', '\n',
      '        output_file="$results/$genome_filename_noext/$query_filename_noext"', '\n',
      '        /home/marian/miniconda3/bin/kma -i "$query_file" -o "$output_file" -t_db "$kma_database" -nc -status', '\n',
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
  
  #### User Feedback ----
  
  observe({
    
    # Render log content
    output$logText <- renderPrint({
      readLogFile()
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
              actionButton(
                "reset_multi",
                "Terminate",
                icon = icon("ban")
              )
            )
          ),
          br(), br(),
          fluidRow(
            tags$style(type='text/css', '#logText {color:white; white-space: pre-wrap;}'),
            column(width = 1),
            column(
              width = 6,
              textOutput("logText"),
              br(),
              HTML(paste('<i class="fa fa-spinner fa-spin" style="font-size:24px;color:white;margin-top:5px"></i>'))
            )  
          )
        )
      })
    } else if(grepl("Multi Typing finalized", tail(readLogFile(), n = 1))) {
      
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
              HTML(paste("<span style='color: white;'>", "Typing finalized.", "Reset to start another typing process.", sep = '<br/>')),
              br(), br(),
              actionButton(
                "reset_multi",
                "Reset",
                icon = icon("arrows-rotate")
              )
            )
          ),
          br(), br(),
          fluidRow(
            tags$style(type='text/css', '#logText {color:white; white-space: pre-wrap;}'),
            column(width = 1),
            column(
              width = 6,
              textOutput("logText"),
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