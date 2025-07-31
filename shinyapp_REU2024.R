library(lubridate)
library(tidyverse)
library(neonUtilities)
library(patchwork)
library(lme4)
library(lmerTest)
library(car)
library(MuMIn)
library(gt)
library(ggeffects)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(devtools)
library(GGally)
devtools::install_github("ropensci/rnaturalearthhires")

#Shiny app ---- 
library(shiny)

install.packages('rsconnect')
library(rsconnect)



##Predictive Shiny app for each variable 
ui <- fluidPage(
  titlePanel("Predictive Model Visualization Tool"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("predictor", "Select Predictor to Plot:",
                  choices = c("Temperature Nov - Feb (temp1)" = "temp1",
                              "Temperature Mar - Jun (temp2)" = "temp2",
                              "Latitude" = "latitude",
                              "Precipitation Mar - Jun (precip2)" = "precip2")),
      
      # Conditional sliders for each predictor
      conditionalPanel(
        condition = "input.predictor == 'temp1'",
        sliderInput("temp1", "Temperature Nov - Feb (temp1)", min = round(min(flowering_data$temp1), 1),
                    max = round(max(flowering_data$temp1), 1), value = round(mean(flowering_data$temp1), 1))
      ),
      conditionalPanel(
        condition = "input.predictor == 'temp2'",
        sliderInput("temp2", "Temperature Mar - Jun (temp2)", min = round(min(flowering_data$temp2), 1),
                    max = round(max(flowering_data$temp2), 1), value = round(mean(flowering_data$temp2), 1))
      ),
      conditionalPanel(
        condition = "input.predictor == 'latitude'",
        sliderInput("latitude", "Latitude", min = round(min(flowering_data$latitude), 1),
                    max = round(max(flowering_data$latitude), 1), value = round(mean(flowering_data$latitude), 1))
      ),
      conditionalPanel(
        condition = "input.predictor == 'precip2'",
        sliderInput("precip2", "Precipitation Mar - Jun (precip2)", min = round(min(flowering_data$precip2), 1),
                    max = round(max(flowering_data$precip2), 1), value = round(mean(flowering_data$precip2), 1))
      )
    ),
    
    mainPanel(
      plotOutput("predictionPlot")
    )
  )
)

server <- function(input, output) {
  
  output$predictionPlot <- renderPlot({
    # Create a sequence of values for the selected predictor
    predictor_values <- seq(min(flowering_data[[input$predictor]]), max(flowering_data[[input$predictor]]), length.out = 100)
    
    # Initialize new_data with default constant values
    new_data <- data.frame(
      temp1 = rep(input$temp1, 100),
      temp2 = rep(input$temp2, 100),
      latitude = rep(input$latitude, 100),
      precip2 = rep(input$precip2, 100)
    )
    
    # Replace the selected predictor column with predictor_values
    new_data[[input$predictor]] <- predictor_values
    
    # Generate predictions for the selected predictor
    predictions <- predict(combined_model, newdata = new_data, re.form = NA)
    
    # Create a data frame for plotting
    plot_data <- data.frame(
      predictor_values = predictor_values,
      predictions = predictions
    )
    
    # Find the predicted value for the current slider position
    current_prediction <- predict(combined_model, newdata = data.frame(
      temp1 = input$temp1,
      temp2 = input$temp2,
      latitude = input$latitude,
      precip2 = input$precip2
    ), re.form = NA)
    
    # Plot using ggplot2
    ggplot(plot_data, aes(x = predictor_values, y = predictions)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(aes(x = input[[input$predictor]], y = current_prediction), color = "brown1", size = 3) +
      labs(
        title = paste("Predicted DOY vs.", input$predictor),
        x = input$predictor,
        y = "Predicted DOY"
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)

## Spatial Shiny App 

species_colors <- c(
  "Plectritis congesta" = "#cc79a7",
  "Collinsia grandiflora" = "#d55e00", 
  "Plagiobothrys figuratus" = "#0072b2", 
  "Clarkia purpurea" = "#009e73", 
  "Epilobium densiflorum" = "#56b4e9", 
  "Collomia grandiflora" = "#f0e442",
  "Navarretia squarrosa" = "#e69f00"
)

# Load map data
us_states <- ne_states(country = "United States of America", returnclass = "sf")
oregon_washington <- us_states %>% filter(name %in% c("Oregon", "Washington"))

# Define UI
ui <- fluidPage(
  titlePanel("Location of Each Species"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("species", "Select Species:",
                         choices = names(species_colors),
                         selected = names(species_colors)[1])
    ),
    
    mainPanel(
      plotOutput("mapPlot", height = "650px", width = "900px"),
      style = "display: flex; justify-content: center;"
    )
  )
)

# Define Server
server <- function(input, output) {
  
  output$mapPlot <- renderPlot({
    # Filter data based on selected species
    selected_dat_map <- selected_dat %>%
      filter(species %in% input$species) %>%
      mutate(species = factor(species, levels = input$species))
    
    # Create the map plot
    map_plot <- ggplot() +
      geom_sf(data = oregon_washington, fill = "lightgray", color = "black") +  
      geom_point(data = selected_dat_map, aes(x = longitude, y = latitude, color = species), alpha = 0.7) +  
      theme_minimal() +
      scale_color_manual(values = species_colors) +
      labs(x = "Longitude", y = "Latitude", color = "Species") +
      coord_sf(xlim = c(-125, -121), ylim = c(43.5, 49)) +  
      annotation_scale(location = "bl", width_hint = 0.5) + 
      annotation_north_arrow(location = "bl", which_north = "true", 
                             pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                             style = north_arrow_fancy_orienteering()) +
      theme(legend.position = "bottom",
            plot.margin = margin(10, 10, 10, 10))
    
    print(map_plot)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
