# UI Modules

mst_control_box <- box(
  solidHeader = TRUE,
  status = "primary",
  width = "100%",
  title = "Controls",
  fluidRow(
    column(
      width = 2,
      align = "center",
      div(
        id = "plot-control",
        actionBttn(
          "mst_label_menu",
          label = "",
          color = "default",
          size = "sm",
          style = "material-flat",
          icon = icon("tags")
        )
      )
    ),
    column(
      width = 2,
      align = "center",
      div(
        id = "plot-control",
        actionBttn(
          "mst_variable_menu",
          label = "",
          color = "default",
          size = "sm",
          style = "material-flat",
          icon = icon("map-pin")
        )
      )
    ),
    column(
      width = 2,
      align = "center",
      div(
        id = "plot-control",
        actionBttn(
          "mst_color_menu",
          label = "",
          color = "default",
          size = "sm",
          style = "material-flat",
          icon = icon("palette")
        )
      )
    ),
    column(
      width = 2,
      align = "center",
      div(
        id = "plot-control",
        actionBttn(
          "mst_size_menu",
          label = "",
          color = "default",
          size = "sm",
          style = "material-flat",
          icon = icon("up-right-and-down-left-from-center")
        )
      )
    ),
    column(
      width = 2,
      align = "center",
      div(
        id = "plot-control",
        actionBttn(
          "mst_misc_menu",
          label = "",
          color = "default",
          size = "sm",
          style = "material-flat",
          icon = icon("ellipsis")
        )
      )
    ),
    column(
      width = 2,
      align = "center",
      div(
        id = "plot-control",
        actionBttn(
          "mst_download_menu",
          label = "",
          color = "default",
          size = "sm",
          style = "material-flat",
          icon = icon("download")
        )
      )
    )
  )
)

nj_control_box <- box(
  solidHeader = TRUE,
  status = "primary",
  width = "100%",
  title = "Controls",
  fluidRow(
    column(
      width = 1,
      div(
        id = "plot-control",
        actionBttn(
          "nj_data_menu",
          label = "",
          color = "default",
          size = "sm",
          style = "material-flat",
          icon = icon("tags")
        )
      )
    ),
    column(
      width = 1,
      div(
        id = "plot-control",
        actionBttn(
          "nj_variable_menu",
          label = "",
          color = "default",
          size = "sm",
          style = "material-flat",
          icon = icon("map-pin")
        )
      )
    ),
    column(
      width = 1,
      div(
        id = "plot-control",
        actionBttn(
          "nj_color_menu",
          label = "",
          color = "default",
          size = "sm",
          style = "material-flat",
          icon = icon("palette")
        )
      )
    ),
    column(
      width = 1,
      div(
        id = "plot-control",
        actionBttn(
          "nj_size_menu",
          label = "",
          color = "default",
          size = "sm",
          style = "material-flat",
          icon = icon("up-right-and-down-left-from-center")
        )
      )
    ),
    column(
      width = 1,
      div(
        id = "plot-control",
        actionBttn(
          "nj_misc_menu",
          label = "",
          color = "default",
          size = "sm",
          style = "material-flat",
          icon = icon("ellipsis")
        )
      )
    ),
    column(
      width = 1,
      div(
        id = "plot-control",
        actionBttn(
          "nj_download_menu",
          label = "",
          color = "default",
          size = "sm",
          style = "material-flat",
          icon = icon("download")
        )
      )
    )
  )
)

upgma_control_box <- box(
  solidHeader = TRUE,
  status = "primary",
  width = "100%",
  title = "Controls",
  fluidRow(
    column(
      width = 1,
      div(
        id = "plot-control",
        actionBttn(
          "upgma_data_menu",
          label = "",
          color = "default",
          size = "sm",
          style = "material-flat",
          icon = icon("tags")
        )
      )
    ),
    column(
      width = 1,
      div(
        id = "plot-control",
        actionBttn(
          "upgma_variable_menu",
          label = "",
          color = "default",
          size = "sm",
          style = "material-flat",
          icon = icon("map-pin")
        )
      )
    ),
    column(
      width = 1,
      div(
        id = "plot-control",
        actionBttn(
          "upgma_color_menu",
          label = "",
          color = "default",
          size = "sm",
          style = "material-flat",
          icon = icon("palette")
        )
      )
    ),
    column(
      width = 1,
      div(
        id = "plot-control",
        actionBttn(
          "upgma_size_menu",
          label = "",
          color = "default",
          size = "sm",
          style = "material-flat",
          icon = icon("up-right-and-down-left-from-center")
        )
      )
    ),
    column(
      width = 1,
      div(
        id = "plot-control",
        actionBttn(
          "upgma_misc_menu",
          label = "",
          color = "default",
          size = "sm",
          style = "material-flat",
          icon = icon("ellipsis")
        )
      )
    ),
    column(
      width = 1,
      div(
        id = "plot-control",
        actionBttn(
          "upgma_download_menu",
          label = "",
          color = "default",
          size = "sm",
          style = "material-flat",
          icon = icon("download")
        )
      )
    )
  )
)

empty_plot_field <- renderUI(
  fluidRow(
    br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
    br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
    br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
    br(), br(), br(), br(), br(), br(), br(), br(), br(), br()
  )
)

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
