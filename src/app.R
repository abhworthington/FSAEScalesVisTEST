#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readxl)
library(ggplot2)
library(MASS)
library(ggbeeswarm)
library(dplyr)


Input_Data <- read_excel("Scales Input Table - FSAE 2019 - EDITED.xlsx",
                         sheet = "Export Sheet") %>%
  filter(!is.na(`Car Only`)) %>%
  mutate(Frame = factor(Frame,
                        levels = c("Tube Frame", 
                                   "Hybrid", 
                                   "Monocoque")),
         `Aerodynamic Package` = factor(`Aerodynamic Package`,
                                        levels = c("None", 
                                                   "Tray Only",
                                                   "Wings Only",
                                                   "Full Package")),
         `Wheel Diameter` = factor(`Wheel Diameter`,
                                   levels = c("10\"",
                                              "13\"",
                                              "Other")),
         Turbo = factor(Turbo,
                        levels = c("No",
                                   "Yes, No Cooler",
                                   "Yes, Cooler")),
         `Design Queue` = factor(`Design Queue`),
         `Design Time` = factor(`Design Time`,
                                levels = c("8:30AM",
                                           "9:30AM",
                                           "10:30AM",
                                           "12:30PM",
                                           "1:30PM",
                                           "2:30PM",
                                           "3:30PM",
                                           "4:30PM")),
         `Engine Cylinders` = factor(`Engine Cylinders`))

# Messing with factor level elements
frame_levels <- levels(Input_Data$Frame)
aero_levels <- levels(Input_Data$`Aerodynamic Package`)
wheel_levels <- levels(Input_Data$`Wheel Diameter`)
turbo_levels <- levels(Input_Data$Turbo)
queue_levels <- levels(Input_Data$`Design Queue`)
time_levels <- levels(Input_Data$`Design Time`)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("FSAE Scales Data EXAMPLE 2019"),
    
    tabsetPanel(
      tabPanel("Rank Graph and Table",
               plotOutput("WeightRank"),
               fluidRow(
                 column(6,
                        selectInput("rank_factor",
                                    "Select Factor for Rank Plot",
                                    c("Engine Cylinders",
                                      "Turbo",
                                      "Frame",
                                      "Aerodynamic Package",
                                      "Wheel Diameter"),
                                    selected = "Frame")),
                 
                 column(3,
                        selectInput("queue",
                                    "Design Queue:",
                                    c("All",
                                      queue_levels))),
                 column(3,
                        selectInput("time",
                                    "Design Time:",
                                    c("All",
                                      time_levels)))
               ),
               # Create a new Row in the UI for selectInputs
               fluidRow(
                 column(3,
                        selectInput("frame",
                                    "Frame Construction:",
                                    c("All",
                                      as.character(frame_levels)))
                 ),
                 column(3,
                        selectInput("aero",
                                    "Aerodynamic Package:",
                                    c("All",
                                      "None", 
                                      as.character(aero_levels)))
                 ),
                 column(3,
                        selectInput("wheel",
                                    "Wheel Size:",
                                    c("All",
                                      as.character(wheel_levels)
                                    ))
                 ),
                 column(3,
                          selectInput("cylinders",
                                      "Engine Cyl:",
                                      c("All",
                                        1:4))
                 )
               ),
               # Create a new row for the table.
               DT::dataTableOutput("table")
                ),
      tabPanel("Boxplots by Vehicle Design Factor & Queue",
         mainPanel(width = 12,
           div("In these Tukey-type boxplots, the rectangle represents the 25th to 75th 
           percentile range, the dark line the median."),
           div("The spikes cover maximum to minimum, or 1.5x the interquartile 
           range above or below the box (25th or 75th %ile) value, if outliers exist."),
           div("The dots represent the individual observations, jittered in the 
           horizontal axis as a \"beeswarm\" for legibility."),
           plotOutput("boxplotByFrame"),
           plotOutput("boxplotByAero"),
           plotOutput("boxplotByWheel"),
           plotOutput("boxplotByCylinders"),
           plotOutput("boxplotByTurbo"),
           plotOutput("boxplotByQueue"),
           plotOutput("boxplotByDesignTime")
           )),
      tabPanel("Scatterplots for Wheelbase and Balance",
               fluidRow(selectInput("scatter_factor",
                                    "Select Factor for Scatterplots",
                                    c("All",
                                      "Engine Cylinders",
                                      "Turbo",
                                      "Frame",
                                      "Aerodynamic Package",
                                      "Wheel Diameter"),
                                    selected = "All")),
          mainPanel(width = 12,
            p("Above, concentric rings represent bivariate density (joint distribution 
            of X and Y variable), where the innermost rings are areas of highest mass/density
            observed (most data/observations). Below, line is a linear fit, presented with 95% CI.
            Both computed without consideration of factor variable."),
            plotOutput("scattercenterofmass"),
            plotOutput("scatterWheelbase"),
            plotOutput("scatterWheelFront")
          ))
    )
)

server <- function(input, output) {

  
  filtered_Data <- reactive({
    data <- Input_Data
    if (input$frame != "All") {
      data <- data[data$Frame == input$frame,]
    }
    if (input$wheel != "All") {
      data <- data[data$`Wheel Diameter` == input$wheel,]
    }
    if (input$aero != "All") {
      data <- data[data$`Aerodynamic Package` == input$aero,]
    }
    if (input$cylinders != "All") {
      data <- data[data$`Engine Cylinders` == input$cylinders,]
    }
    if (input$queue != "All") {
      data <- data[data$`Design Queue` == input$queue,]
    }
    if (input$time != "All") {
      data <- data[data$`Design Time` == input$time,]
    }
    data <- data %>% filter(!is.na(`Car Only`)) %>% arrange(`Car Only`)
  })
  
  # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable(filtered_Data(),
                                                      rownames = T) %>% 
                                          DT::formatPercentage(c("Front %", "Left %"),1) %>%
                                          DT::formatRound("Car Only", digits = 1) %>%
                                          DT::formatRound("Wheelbase", digits = 2))

     output$WeightRank <- renderPlot({
      tenthpctmass <- quantile(filtered_Data()$`Car Only`, probs = .10, na.rm = T)
      firstquartmass <- quantile(filtered_Data()$`Car Only`, probs = .25, na.rm = T)
      medianmass <- quantile(filtered_Data()$`Car Only`, probs = .5, na.rm = T)
      thirdquartmass <- quantile(filtered_Data()$`Car Only`, probs = .75, na.rm = T)
      maxrank <- max(rank(filtered_Data()$`Car Only`,
                          ties.method = "random"))
      filtered_Data() %>%
        filter(!is.na(`Car Only`)) %>%
        ggplot() +
        geom_point(aes(x = rank(`Car Only`,
                                ties.method = "random"),
                       y = `Car Only`,
                       color = .data[[input$rank_factor]],
                       shape = .data[[input$rank_factor]]),
                   size = 2.5) +
        geom_hline(yintercept = medianmass, alpha = 0.5) +
        geom_hline(yintercept = firstquartmass, alpha = 0.5) +
        geom_hline(yintercept = thirdquartmass, alpha = 0.5) +
        # geom_hline(yintercept = tenthpctmass, alpha = 0.5) +
        annotate("text", x = 0.85*maxrank, y = firstquartmass - 4, label = paste("25th %ile:", round(firstquartmass, 1)), size = 11/.pt) +
        annotate("text", x = 0.85*maxrank, y = medianmass - 4, label = paste("Median:", round(medianmass, 1)), size = 11/.pt) +
        annotate("text", x = 0.85*maxrank, y = thirdquartmass - 4, label = paste("75th %ile:", round(thirdquartmass, 1)), size = 11/.pt) +
        # annotate("text", x = 0.85*maxrank, y = tenthpctmass - 4, label = paste("10th %ile:", round(tenthpctmass, 1)), size = 11/.pt) +
        labs(y = "Car Only Weight, kg", x = "Weight Rank", color = input$rank_factor) +
        theme_minimal(base_size = 14)
    })


# output$distPlot <- renderPlot({
#     # generate bins based on input$bins from ui.R
#     x    <- Input_Data$`Car Only`
#     bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 1)
# 
#     # draw the histogram with the specified number of bins
#     hist(x, breaks = bins, col = 'darkgray', border = 'white',
#          xlab = 'Car Only Weight (lb)',
#          main = 'Histogram of Car Only Weights')
# })
    # output$densityPlot <- renderPlot({
    #   # collect elements of plot
    #   x2 <- Input_Data$`Car Only`[!is.na(Input_Data$`Car Only`)]
    #   
    #   plot(density(x2))
    # })
    
    output$boxplotByFrame <- renderPlot({
      Input_Data %>% 
        filter(!is.na(`Car Only`)) %>%
        ggplot() +
        geom_boxplot(aes(y = `Car Only`,
                         x = Frame),
                     outliers = F,
                     width = 0.66) +
        geom_beeswarm(aes(y = `Car Only`,
                        x = Frame),
                    alpha = 0.6,
                    cex = 1.5) +
        theme_minimal(base_size = 14)
    })
    
    output$boxplotByAero <- renderPlot({
      Input_Data %>% 
        filter(!is.na(`Car Only`)) %>%
        ggplot() +
        geom_boxplot(aes(y = `Car Only`,
                         x = `Aerodynamic Package`),
                     outliers = F,
                     width = 0.66) +
        geom_beeswarm(aes(y = `Car Only`,
                        x = `Aerodynamic Package`),
                    alpha = 0.6,
                    cex = 1.5) +
        theme_minimal(base_size = 14)
    })
    
    output$boxplotByWheel <- renderPlot({
      Input_Data %>% 
        filter(!is.na(`Car Only`)) %>%
        ggplot() +
        geom_boxplot(aes(y = `Car Only`,
                         x = `Wheel Diameter`),
                     outliers = F,
                     width = 0.66) +
        geom_beeswarm(aes(y = `Car Only`,
                        x = `Wheel Diameter`),
                      alpha = 0.6,
                      cex = 1.5) +
        theme_minimal(base_size = 14)
    })
    
    output$boxplotByCylinders <- renderPlot({
      Input_Data %>% 
        filter(!is.na(`Car Only`)) %>%
        ggplot() +
        geom_boxplot(aes(y = `Car Only`,
                         x = `Engine Cylinders`),
                     outliers = F,
                     width = 0.66) +
        geom_beeswarm(aes(y = `Car Only`,
                        x = `Engine Cylinders`),
                      alpha = 0.6,
                      cex = 1.5) +
        theme_minimal(base_size = 14)
    })
    
    output$boxplotByTurbo <- renderPlot({
      Input_Data %>% 
        filter(!is.na(`Car Only`)) %>%
        ggplot() +
        geom_boxplot(aes(y = `Car Only`,
                         x = Turbo),
                     outliers = F,
                     width = 0.66) +
        geom_beeswarm(aes(y = `Car Only`,
                        x = Turbo),
                      alpha = 0.6,
                      cex = 1.5) +
        theme_minimal(base_size = 14)
    })
    
    output$boxplotByWheel <- renderPlot({
      Input_Data %>% 
        filter(!is.na(`Car Only`)) %>%
        ggplot() +
        geom_boxplot(aes(y = `Car Only`,
                         x = `Wheel Diameter`),
                     outliers = F,
                     width = 0.66) +
        geom_beeswarm(aes(y = `Car Only`,
                        x = `Wheel Diameter`),
                      alpha = 0.6,
                      cex = 1.5) +
        theme_minimal(base_size = 14)
    })
    
    output$boxplotByQueue <- renderPlot({
      Input_Data %>% 
        filter(!is.na(`Car Only`), !is.na(`Design Queue`)) %>%
        ggplot() +
        geom_boxplot(aes(y = `Car Only`,
                         x = `Design Queue`),
                     outliers = F,
                     width = 0.66) +
        geom_beeswarm(aes(y = `Car Only`,
                        x = `Design Queue`),
                      alpha = 0.6,
                      cex = 1.5) +
        theme_minimal(base_size = 14)
    })
    
    output$boxplotByDesignTime <- renderPlot({
      Input_Data %>% 
        filter(!is.na(`Car Only`), !is.na(`Design Time`)) %>%
        ggplot() +
        geom_boxplot(aes(y = `Car Only`,
                         x = `Design Time`),
                     outliers = F) +
        geom_beeswarm(aes(y = `Car Only`,
                        x = `Design Time`),
                      alpha = 0.6,
                      cex = 1.5) +
        theme_minimal(base_size = 14)
    })
    
    output$scatterWheelbase <- renderPlot({
      Input_Data %>% mutate(All = factor(c("All"))) %>%
        filter(!is.na(`Car Only`)) %>%
        ggplot(aes(y = `Car Only`,
                   x = Wheelbase)) +
        geom_point(aes(color = .data[[input$scatter_factor]],
                       shape = .data[[input$scatter_factor]]))+
        geom_smooth(method = "lm",
                    alpha = 0.2,
                    color = "black") +
        theme_minimal(base_size = 14)
    })
    
    output$scattercenterofmass <- renderPlot({
      Input_Data %>% mutate(All = factor(c("All"))) %>%
        filter(!is.na(`Car Only`)) %>%
        ggplot(aes(y = .data[["Front %"]],
                   x = 0.5-.data[["Left %"]])) +
        geom_point(aes(color = .data[[input$scatter_factor]],
                       shape = .data[[input$scatter_factor]])) +
        geom_density_2d(color = "black",
                        alpha = 0.4) +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0.5) +
        scale_x_continuous(labels = scales::label_percent()) +
        scale_y_continuous(labels = scales::label_percent()) +
        labs(x = "L/R Imbalance (50% - Left%)") +
        theme_minimal(base_size = 14)
    })

    output$scatterWheelFront <- renderPlot({
      Input_Data %>% mutate(All = factor(c("All"))) %>%
        filter(!is.na(`Car Only`)) %>%
        ggplot(aes(y = .data[["Front %"]],
                   x = Wheelbase)) +
        geom_point(aes(color = .data[[input$scatter_factor]],
                       shape = .data[[input$scatter_factor]]))+
        geom_smooth(method = "lm",
                    alpha = 0.2,
                    color = "black") + 
        scale_y_continuous(labels = scales::label_percent()) +
        theme_minimal(base_size = 14)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
