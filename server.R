#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Read data 
  air.data <- read_csv("fatalities_date.csv", col_names = T)
  air.data[, c(5,6,8,9,12)] <- NULL
  
  # Render functions
  output$summary <- renderPrint({
    summary(air.data[, c("Aboard", "Fatalities")])
  })
  
  air.data <- air.data %>%
    group_by(year(mdy(Date))) %>%
    mutate(Survival.Rate = (1 - mean(Fatalities, na.rm = T)/mean(Aboard, na.rm = T)))
  air.data <- air.data %>% rename(Year = `year(mdy(Date))`)
  
  # Animated visual of yearly aboard vs fatalities 
  
  output$animation <- renderPlotly({
    anim <- ggplot(air.data, aes(x = Fatalities, y = Aboard)) + 
      geom_point(aes(frame = Year), 
                 col = ifelse(air.data$Survival.Rate > 0.40, 'green', 'red'),
                 shape = 2, size = 3) +
      labs(title = "Aboard vs. Fatalities Animated")
    anim <- ggplotly(anim) %>%
      animation_opts(
        500, easing = "quad"
      ) %>%
      animation_slider(
        currentvalue = list(prefix = "YEAR ", font = list(color = "red"))
      ) 
    anim
  })
  
  output$fatality <- renderPlot({
    ggplot(air.data, aes(x = year(mdy(Date)), y = Fatalities)) + 
      geom_bar(stat = "identity", width = 0.5, fill = "blue") +
      labs(title = "Annual Number of Fatalities on Crashed Aircrafts") +
      xlab("Year") + ylab("Number of Fatalities")
  })
  
  # -- Frequency used for the next three plots 
  
  opFreq <- as.data.frame(table(air.data$Operator))
  colnames(opFreq) <- c("Op", "Freq")
  opFreq <- opFreq[order(-opFreq$Freq),]
  opFreq$Op <- factor(opFreq$Op, levels = opFreq$Op)
  opFreq1 <- head(opFreq, 30)
  opFreq2 <- head(opFreq, 60)
  opFreq2$id <- seq(1,nrow(opFreq2))
  num_bar = nrow(opFreq2)
  angle <- 90 - 360*(opFreq2$id - 0.5)/num_bar
  opFreq2$hjust <- ifelse(angle < -90, 1, 0)
  opFreq2$angle <- ifelse(angle < -90, angle + 180, angle)
  
  
  # -- Circular Bar Plot 
  
  output$cirPlot <- renderPlot({
    cir <- ggplot(opFreq2, aes(x = as.factor(id), y = Freq)) + 
      geom_bar(stat="identity", fill = alpha("skyblue", 0.7)) +
      ylim(-100, 120) +
      theme_minimal() + 
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm")
      ) +
      coord_polar(start = 0) +
      geom_text(data = opFreq2, aes(x=id, y=Freq+10, label = Op, hjust = hjust), color = "black", 
                fontface = "bold", alpha = 0.6, size = 2.5, angle = opFreq2$angle, inherit.aes = F)
    cir
  })
  
  # -----------------------------------------
  
  output$opTop <- renderPlot({
    ggplot(opFreq1, aes(x = Op, y = Freq)) +
      geom_point(size = 2) +
      geom_segment(aes(x = Op, xend = Op, y = 0, yend = Freq)) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0)) + 
      coord_flip() +
      xlab("Number of Crashes") + ylab("Operator") +
      labs(title = "Top 30 Operators involved in crashes")
  })
  
  # Investigate Air France and Deutsche Lufthansa 
  
  # Dataframe for Air France and Deutsche Lufthansa
  merged <- merge(air.data, opFreq, by.x = "Operator", by.y = "Op")
  merged <- merged %>% rename(OpFreq = Freq)
  df <- merged %>% 
    filter(Operator == "Air France" | Operator == "Deutsche Lufthansa") %>%
    select(Date, Operator, Fatalities, OpFreq, Aboard, Summary)
  # ------------------------------------------------
  
  ops <- df %>% group_by(Operator)
  
  output$air_selec_sum <- renderPrint({
    as.data.frame(summarise(ops, Crashes.Count = n(), 
                            Avg.Fatalities = mean(Fatalities, na.rm = T),
                            Avg.Aboard = mean(Aboard, na.rm = T),
                            Largest.Crash = max(Fatalities),
                            Survival.Rate = 
                              (1 - mean(Fatalities, na.rm = T)/mean(Aboard, na.rm = T))))
  })
  
  
  # In depth on the largest Air France Crash
  
  output$LargestAFCrash <- renderPrint({
    as.data.frame(ops[which.max(ops$Fatalities), c(1:5)])
  })
  
  # Largest crash 
  output$LargestCrash <- renderPrint({
    as.data.frame(air.data[which.max(air.data$Fatalities), 
                           c("Date", "Operator", "Fatalities")])
  })
  
  # ----------------------------------------------------------------
  
  # Your Analysis 
  filename <- reactive({input$file})
  file <- eventReactive(eventExpr = input$file, {paste0("",filename(), ".Rdata")})
  
  data <- reactive({get(load(file()))})
  rv <- reactiveValues()
  
  # function needed for a cummulative plot of CO2
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  # Plot renders 
  
  output$snowRidge <- renderPlot({
    df <- data() %>% select(Winter, Spring, Summer, Autumn) %>%
      gather()
    
    colnames(df) <- c("Season", "Snow")
    df <- df[which(df$Snow < 350),]
    ggplot(df, aes(x = Snow)) +
      geom_density(aes(fill = factor(Season)), alpha = 0.8, adjust = 20) +
      labs(title = "Canadian Average Snow Distributions by Season") +
      scale_fill_discrete("Season") + ylab("Density (Scaled by 20)")
    
  })
  
  output$tempRidge <- renderPlot({
    df <- data() %>% select(Winter, Spring, Summer, Autumn) %>%
      gather()
    
    colnames(df) <- c("Season", "Temperature")
    ggplot(df, aes(x = Temperature)) + 
      geom_density(aes(fill = factor(Season)), alpha = 0.8) +
      labs(title = "Canadian Average Temperature Distributions by Season")
  })
  
  output$co2Anim <- renderPlotly({
    df <- data() %>% select(Year, Month, Value) %>%
      group_by(Year, Month) %>% 
      summarise(Emissions = mean(Value))
    
    
    
    plot_ly(df, alpha = 0.5) %>%
      add_lines(x = ~Month, y = ~Emissions, frame = ~Year,
                line = list(simplify = F)) %>%
      layout(showlegend = F,
             title = "CO2 Emissions Per Month Yearly")
  })
  
  output$co2Acc <- renderPlotly({
    df <- data() %>% select(Year, Value) 
    df <- df %>% accumulate_by(~Year)
    
    df %>% plot_ly(x = ~Year, y = ~Value, 
                   frame = ~frame, 
                   type = 'scatter',
                   mode = 'lines', 
                   line = list(simplyfy = F)) %>%
      layout(
        xaxis = list(
          title = "Year",
          zeroline = T
        ),
        yaxis = list(
          title = "CO2 Emisions",
          zeroline = T
        ),
        showlegend = F,
        title = "CO2 Emisions Yearly Worldwide") %>%
      animation_opts(
        frame = 500,
        transition = 0,
        redraw = F
      ) %>%
      animation_slider(
        hide = T
      ) %>%
      animation_button(
        x = 1, xanchor = "right", y = 0, yanchor = "bottom"
      )
    
    
  })
  
  output$carAgeFreq <- renderPlotly({
    
    t1 <- data() %>% select(Year, Age1)
    t2 <- data() %>% select(Year, Age2)
    colnames(t1) <- c("Year", "Age")
    colnames(t2) <- c("Year", "Age")
    df <- rbind(t1, t2) 
    
    p <- df %>% ggplot(aes(x = Age)) + 
      geom_histogram(binwidth = 0.9, aes(fill = ..count..)) +
      labs(title = "New York Car Crash Driver Age Distribution")
    p <- ggplotly(p)
    p
    
  })
  
})
