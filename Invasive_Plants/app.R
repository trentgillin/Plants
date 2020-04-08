#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(dplyr)
library(shiny)
library(readr)
library(leaflet)
library(shinythemes)
library(feather)
library(forcats)
library(stringr)

# read in data
shiny_data <- read_feather(here::here("clean_data.feather"))

shiny_data <- shiny_data %>%
    mutate(ACCEPTED_COMMON_NAME = fct_relevel(ACCEPTED_COMMON_NAME, "All Species")) %>%
    arrange(ACCEPTED_COMMON_NAME) %>%
    mutate(lat = jitter(lat, factor = 10),
           lon = jitter(lon, factor = 10))

plant_names <- sort(unique(shiny_data$ACCEPTED_COMMON_NAME))

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("slate"),

    # Application title
    titlePanel("Invasive Plant Species"),

    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 4,
        
        # seleciton box
        selectInput("select", 
                    label = "Select Plant Species", 
                    choices = plant_names,
                    hr()),
        
        dateRangeInput('dateRange',
                       label = 'Date Of Collection',
                       start = min(shiny_data$DATE_COLLECTED), end = max(shiny_data$DATE_COLLECTED)
        ),
        #imageOutput('image'),
        p(
            "Sources: Latitude and longitude from ggmap package,
             D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R Journal, 5(1),
  144-161. URL http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf"
        )),

        # Show a plot of the generated distribution
        mainPanel(
            h3("Invasive Map"),
           leafletOutput("mymap"),
           h3("Wikipedia Entry"),
           textOutput("wikipedia_text"),
           uiOutput("wikipedia_page")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # filter data
   plot_data <- reactive({
       shiny_data <- shiny_data %>%
           filter(ACCEPTED_COMMON_NAME == input$select & 
                      DATE_COLLECTED %in% seq(input$dateRange[1], input$dateRange[2], by = "day"))
    })

    output$mymap <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addCircleMarkers(data = plot_data(), 
                       lat = ~lat, 
                       lng = ~lon, 
                       radius = ~(INFESTED_PERCENT *10),
                       popup = paste0("Species: ", "<b>", plot_data()$ACCEPTED_COMMON_NAME, "</b>", "<br>",
                                      "National Forest: ", plot_data()$National_Forest, "<br>",
                                      "Infested Percent: ", scales::percent(plot_data()$INFESTED_PERCENT), "<br>",
                                      "Date Collected: ", plot_data()$DATE_COLLECTED),
                       color = "#006400",
                       )
    })
    
    output$dateRangeText  <- renderText({
        paste("input$dateRange is", 
              paste(as.character(input$dateRange), collapse = " to ")
        )
    })
    
    output$wikipedia_text<- renderText({ 
        str_trunc(unique(plot_data()$wikipedia_text), 700)
    })
    
    output$wikipedia_page <- renderUI({
        selection <- input$select
        selection <- str_to_sentence(selection)
        selection <- str_replace_all(selection, " ", "_")
        a(href=paste0("https://en.wikipedia.org/wiki/",selection), "Wikipedia Page")
        
    })   
    
    # output$image <- renderImage({
    #     picture_files <- list.files(here::here("images/"))
    #     picture_files <- stringr::str_subset(str_to_lower(picture_files), str_to_lower(input$select))
    #    if (length(picture_files) > 1) {
    #         picture_files <- picture_files[1]
    #     } else if (length(picture_files) == 0) {
    #         picture_files <- "No Image.png"
    #     } else {
    #         picture_files <- picture_files
    #     }
    #     filename <- suppressWarnings(normalizePath(file.path('images', picture_files)))
    #     list(src = filename)
    # }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
