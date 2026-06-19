# ==============================================================================
# app.R
#
# Task 7, Option B: interactive plot of AIS record counts by minute within
# a user-selected hour on 2024-04-23 (adjust TARGET_DATE in
# shiny_data_prep.R if your assignment / chosen date differs), filtered by
# collection_type and one or more ship_type values.
#
# Reads the pre-aggregated 01_data/shiny_app_data.csv produced by
# 02_code/R/shiny_data_prep.R -- NO live API calls happen here, as required
# by the assignment (avoid expensive repeated API requests from the
# deployed app).
# ==============================================================================

library(shiny)
library(ggplot2)
library(dplyr)

data_path <- "data/shiny_app_data.csv"  # mounted path inside the container; see docker-compose.yaml
shiny_data <- read.csv(data_path, stringsAsFactors = FALSE)

available_hours          <- sort(unique(shiny_data$hour))
available_collection_types <- sort(unique(shiny_data$collection_type))
available_ship_types     <- sort(unique(shiny_data$ship_type))

ui <- fluidPage(
  titlePanel("AIS Records per Minute - 2024-04-23"),

  sidebarLayout(
    sidebarPanel(
      selectInput("hour", "Hour of day:",
                  choices = available_hours, selected = available_hours[1]),
      selectInput("collection_type", "Collection type:",
                  choices = available_collection_types,
                  selected = available_collection_types[1]),
      checkboxGroupInput("ship_types", "Ship type(s):",
                          choices  = available_ship_types,
                          selected = available_ship_types[1]),
      actionButton("refresh", "Refresh plot", class = "btn-primary")
    ),

    mainPanel(
      plotOutput("minute_plot", height = "500px")
    )
  )
)

server <- function(input, output, session) {

  # The plot only updates when the user clicks "Refresh plot", per the
  # assignment's requirement (eventReactive ties the computation to the
  # button rather than to every input change).
  filtered_data <- eventReactive(input$refresh, {
    shiny_data %>%
      filter(
        hour            == as.integer(input$hour),
        collection_type == input$collection_type,
        ship_type %in% input$ship_types
      ) %>%
      group_by(minute, ship_type) %>%
      summarise(n_records = sum(n_records), .groups = "drop")
  }, ignoreNULL = FALSE)

  output$minute_plot <- renderPlot({
    df <- filtered_data()

    # Ensure all 60 minutes are represented (0 where no data), so the
    # x-axis always spans 0-59 as required.
    full_minutes <- expand.grid(
      minute    = 0:59,
      ship_type = unique(c(df$ship_type, input$ship_types))
    )
    df_complete <- full_minutes %>%
      left_join(df, by = c("minute", "ship_type")) %>%
      mutate(n_records = ifelse(is.na(n_records), 0, n_records))

    ggplot(df_complete, aes(x = minute, y = n_records, color = ship_type)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 1.5) +
      scale_x_continuous(limits = c(0, 59), breaks = seq(0, 59, 10)) +
      labs(
        title = paste0("AIS records per minute - hour ", input$hour, ":00"),
        x = "Minute of hour (0-59)", y = "Number of AIS records",
        color = "Ship type"
      ) +
      theme_minimal(base_size = 13)
  })
}

shinyApp(ui = ui, server = server)
