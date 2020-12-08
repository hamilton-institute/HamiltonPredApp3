#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#'
#' @importFrom bs4Dash tabPanel tabsetPanel updateTabsetPanel dashboardPage dashboardControlbar dashboardHeader dashboardSidebar bs4SidebarMenu bs4SidebarMenuItem dashboardBody bs4TabItems bs4TabItem dashboardFooter
#' @importFrom shiny a tagList icon
#'
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    hamiltonThemes::use_bs4Dash_distill_css(),
    # List the first level UI elements here
    dashboardPage(
      enable_preloader = FALSE,
      old_school = FALSE,
      sidebar_mini = FALSE,
      sidebar_collapsed = FALSE,
      controlbar_collapsed = TRUE,
      controlbar_overlay = TRUE,
      # loading_background = "#4682B4",
      title = "App title",

      #---
      controlbar = dashboardControlbar(),

      #---
      navbar = bs4Dash::bs4DashNavbar(
        skin = "light",
        status = "light",
        border = TRUE,
        sidebarIcon = "bars",
        compact = FALSE,
        controlbarIcon = "th"
      ),

      #---
      sidebar = bs4Dash::bs4DashSidebar(
        skin = "light",
        status = "primary",
        brandColor = "dark",
        title = "SARS-CoV-2 Forecast",
        bs4Dash::bs4SidebarMenu(
          bs4Dash::bs4SidebarMenuItem(
            "Forecast",
            icon = "laptop",
            startExpanded = TRUE,
            bs4Dash::bs4SidebarMenuSubItem(
              "Interval Estimate",
              tabName = "plots",
              icon =  "fas fa-chart-line"
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              "Point Estimate",
              tabName = "plots2",
              icon = "fas fa-chart-area"
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              "Summary",
              tabName = "table",
              icon = "calculator"
            )
          ),
          bs4Dash::bs4SidebarMenuItem(
            "Pandemic Waves",
            tabName = "ar_comp",
            icon = "fas fa-exclamation-triangle"
          ),
          bs4Dash::bs4SidebarMenuItem(
            "Dendrogram",
            tabName = "dendogram",
            icon = "fas fa-project-diagram"
          ),
          # ----------------------------------------

          bs4Dash::bs4SidebarHeader("Additional Information"),

          # ----------------------------------------
          bs4Dash::bs4SidebarMenuItem(
            "Model Validation",
            tabName = "validation",
            icon = "bar-chart-o"
          )
         )
      ),

      #---
      body = bs4Dash::bs4DashBody(
        hamiltonThemes::use_bs4Dash_distill_theme(), # <-- use the theme
        withMathJax(),
        bs4Dash::bs4TabItems(
          bs4Dash::bs4TabItem(
            tabName = "plots",
            mod_interval_ui("interval_ui_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "plots2",
            mod_point_ui("point_ui_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "table",
            mod_table_ui("table_ui_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "ar_comp",
            mod_ar_comp_ui("ar_comp_ui_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "dendogram",
            mod_dendogram_ui("dendogram_ui_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "validation",
            mod_validation_ui("validation_ui_1")
          )
        )
      ),

      #---
      footer = hamiltonThemes::bs4dash_distill_footer()
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom shiny tags
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  
  shinyjs::useShinyjs()
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  add_resource_path(
    'www', system.file("distill", package = "hamiltonThemes")
  )

  tags$head(
    bundle_resources(
      path = system.file("distill", package = "hamiltonThemes"),
      app_title = 'hamiltonPredApp'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
  
  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'hamiltonPredApp'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

