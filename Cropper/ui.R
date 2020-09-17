
library(shiny)
library(shinyjs)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(shinyjs::useShinyjs(),
            theme = shinytheme(theme = "slate"),
            tags$script(' $(document).on("keydown", function (e) {
                                                  Shiny.onInputChange("lastkeypresscode", e.keyCode);
                                                  });
                                                  '),
            sidebarLayout(
              sidebarPanel(style = "position:fixed;width:inherit;",
                           width = 0
              ),
              mainPanel(width = 12,
                        uiOutput("SlogOutput")
              )
            )
  )
)
