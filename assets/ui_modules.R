# UI Modules

initiate_multi_typing_ui <- renderUI({
  column(
    width = 12,
    fluidRow(
      column(
        width = 3,
        align = "center",
        br(),
        br(),
        fluidRow(
          column(1),
          column(
            width = 11,
            align = "left",
            h3(p("Assembly Selection"), style = "color:white; margin-left: 40px"),
          )
        ),
        br(), br(),
        fluidRow(
          column(1),
          column(
            width = 11,
            align = "center",
            fluidRow(
              column(
                width = 6,
                align = "center",
                shinyFilesButton(
                  "assembly_files",
                  "Select File(s)" ,
                  icon = icon("file"),
                  title = "Select one or multiple assembly file(s)",
                  multiple = TRUE,
                  buttonType = "default",
                  class = NULL,
                  width = "120px",
                  root = path_home()
                )
              ),
              column(
                width = 6,
                align = "left",
                uiOutput("multi_file_sel_info")
              )
            ),
            br(), 
            fluidRow(
              column(
                width = 6,
                align = "center",
                shinyDirButton(
                  "assembly_folder",
                  "Select Folder",
                  icon = icon("folder-open"),
                  title = "Select folder containing assembly file(s)",
                  buttonType = "default",
                  root = path_home()
                )
              ),
              column(
                width = 6,
                align = "left",
                uiOutput("multi_folder_sel_info")
              )
            ),
            br(), br(), 
            fluidRow(
              column(1),
              uiOutput("metadata_multi_box")
            )
          )
        )
      ),
      column(1),
      column(
        width = 7,
        br(),
        br(),
        fluidRow(
          column(
            width = 10,
            uiOutput("multi_select_tab_ctrls"),
          )
        ),
        fluidRow(
          column(
            width = 12,
            rHandsontableOutput("multi_select_table")
          )
        )
      )
    )
  )
})
