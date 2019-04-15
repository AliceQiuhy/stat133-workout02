library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Saving Simulation"),
   
   # Create a new Row in the UI for sliderInputs
   fluidRow(
     column(4,
            sliderInput("initial",
                        "Initial Amount",
                        min = 0,
                        max = 100000,
                        step = 500,
                        value = 1000,
                        pre = "$", 
                        sep = ",")),
    column(4,
          sliderInput("rate",
                      "Return Rate (in%)",
                      min = 0,
                      max = 20,
                      step = 0.1,
                      value = 5)),
    column(4,
           sliderInput("year",
                       "Years",
                       min = 0,
                       max = 50,
                       step = 1,
                       value = 20))
   ),
   
   # Create a new Row in the UI for sliderInputs and selectInput
   fluidRow(
     column(4,
            sliderInput("contrib",
                        "Annual Contribution",
                        min = 0,
                        max = 50000,
                        step = 500,
                        value = 2000,
                        pre = "$", 
                        sep = ",")),
     column(4,
            sliderInput("growth",
                        "Growth Rate (in%)",
                        min = 0,
                        max = 20,
                        step = 0.1,
                        value = 2)),
     column(4,
            selectInput("facet",
                        "Facet?",
                        c("Yes", "No"))),
    

      # Show a plot of the generated distribution
      mainPanel(
        HTML('<hr style="color: grey;">'),
        h4("Timelines"),
        plotOutput("linePlot"),
        h4("Balances"),
        verbatimTextOutput("dataframe")
      )
   )
)

server <- function(input, output) {
  
  # define functions
  future_value <- function(amount, rate, years){
    rate <- rate/100
    fv <- amount * (1 + rate) ^ years
    return(fv)
  }
  
  annuity <- function(contrib, rate, years){
    rate <- rate/100
    fva <- contrib * (((1 + rate) ^ years - 1) / rate)
    return(fva)
  }
  
  growing_annuity <- function(contrib, rate, growth, years){
    rate <- rate/100
    growth <- growth/100
    fvga <- contrib * (((1 + rate) ^ years - (1 + growth) ^ years) / (rate - growth))
    return(fvga)
  }
  
  # data for the table
  dat <- reactive({
    dat <- data.frame(
      year = 0,
      no_contrib = input$initial,
      fixed_contrib = input$initial,
      growing_contrib = input$initial
    )
    
    for (i in 1:input$year) {
      fv <- round(future_value(input$initial, input$rate, i), 3)
      fva <- round(future_value(input$initial, input$rate, i), 3) + round(annuity(input$contrib, input$rate, i), 3)
      fvga <- round(future_value(input$initial, input$rate, i), 3) + round(growing_annuity(input$contrib, input$rate, input$growth, i), 3)
      dat <- rbind(dat,c(i, fv, fva, fvga))
    }
    
    return(dat)
  })
  
  output$dataframe <- renderPrint({
    dat()
  })
  
  # data for the plot
  plot_dat <- reactive({
    # creating data frame
    plot_dat <- data.frame(
      year = rep(0,3),
      balance = rep(input$initial,3),
      mode = c("no_contrib", "fixed_contrib", "growing_contrib")
    )
    
    # loops
    
    for (i in 1:input$year){
      fv <- round(future_value(input$initial, input$rate, i), 3)
      fva <- round(future_value(input$initial, input$rate, i), 3) + round(annuity(input$contrib, input$rate, i), 3)
      fvga <- round(future_value(input$initial, input$rate, i), 3) + round(growing_annuity(input$contrib, input$rate, input$growth, i), 3)
      plot_dat <- rbind(plot_dat,c(i, fv, "no_contrib"))
      plot_dat <- rbind(plot_dat,c(i, fva, "fixed_contrib"))
      plot_dat <- rbind(plot_dat,c(i, fvga, "growing_contrib"))
    }
    
    plot_dat <- transform(plot_dat, year = as.numeric(year))
    plot_dat <- transform(plot_dat, balance = as.numeric(balance))
    
    plot_dat$mode <-  factor(plot_dat$mode, levels = c("no_contrib", "fixed_contrib", "growing_contrib"))
    
    return(plot_dat)
  })
  
   output$linePlot <- renderPlot({
     if (input$facet == "No") {
       ymax <- max(dat()[,'growing_contrib'])
       ggplot(data = plot_dat(), aes(x = year, y = balance, color = mode)) +
         geom_point(size = 1) + geom_line() +
         scale_x_continuous(limits=c(0, input$year), breaks=seq(0, input$year, 2.5)) + 
         scale_y_continuous(limits=c(0, ymax), breaks=seq(0, ymax, 10000)) +
         ggtitle("Three modes of investing") +
         ylab("value") +
         theme_grey()
     } else if (input$facet == "Yes") {
       ymax <- max(dat()[,'growing_contrib'])
       ggplot(data = plot_dat(), aes(x = year, y = balance, color = mode)) +
         geom_point(size = 1) + geom_line() + geom_area(aes(x = year, y = balance, fill = mode, alpha = 0.7)) +
         facet_wrap(~mode)+
         scale_x_continuous(limits=c(0, input$year), breaks=seq(0, input$year, 2.5)) + 
         scale_y_continuous(limits=c(0, ymax), breaks=seq(0, ymax, 10000)) +
         ggtitle("Three modes of investing") + 
         ylab("value") +
         theme_bw()
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

