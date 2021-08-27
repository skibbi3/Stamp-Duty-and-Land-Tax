#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(plotly)


ui <- dashboardPagePlus(
    
    header = dashboardHeader(title = "Land tax and Stamp duty"),
    footer = dashboardFooter(
        left_text = "Australian Taxpayers' Alliance",
        right_text = "Sydney, 2020"
    ),
    sidebar = dashboardSidebar(
        sidebarMenu(
            #menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Estimator", tabName = "estimator", icon = icon("chart-line"))
        )
    ),
    body = dashboardBody(
        # Tab items
        tabItems(
            tabItem("Home", tabName = "home",
                        fluidRow(
                            box(solidHeader = TRUE, status = "primary", title = "About",
                                tags$p("Placeholder")
                            )
                        )
                    ),
            tabItem("Estimator", tabName = "estimator",
                    fluidRow(
                        box(solidHeader = TRUE, status = "primary", title = "Inputs",
                            #numericInput(
                            #    "landVal", "Land value", 1400000, min = 0, max = NA, step = 10000 
                            #),
                            "Type in the house value and the program will estimate the land value for you.",
                            numericInput(
                                "houseVal", "House price", 2000000, min = 0, max = NA, step = 10000
                            ),
                            
                            sliderInput(
                                "years", "Years", 1, 100, 50, step = 1
                            ),
                            radioButtons(
                              "state",
                              "State",
                              choices = c(
                                "New South Wales" = "NSW",
                                "Victoria" = "VIC",
                                "Queensland" = "QLD",
                                "South Australia" = "SA",
                                "Tasmania" = "TAS"
                              )
                            )
                          
                            
                        ),
                        
                        box(solidHeader = TRUE, status = "primary", title = "Taxes",
                            plotlyOutput("distPlot")
                        ),
                        
                        box(solidHeader = TRUE, status = "warning", collapsible = TRUE , title = "Assumptions",
                            tags$ul(
                                tags$li("Land value is unchanged since purchase"),
                                tags$li("Land tax valuations remain constant as of 2020"),
                                tags$li("Stamp duty is paid off with a 30 year loan at 2.69% interest rate")
                            )
                        )
                    )
            )
            
        ),
        
    )
)


server <- function(input, output) {
    
    #if(input$state == "NSW"){
      Estimator <- readRDS("Data/LandValEstimator.RDS")
    #}
    
    #else if(input$state == "VIC"){
    #  Estimator <- readRDS("Data/LandValEstimatorVIC.RDS")
    #}
    
    estimateLandVal <- function(houseVal){
        ndata = data.frame(
            Property.Value = c(houseVal)
        )
        
        return(predict(Estimator, newdata = ndata))
    }
    
    
    
    # NSW
    
    calculateLandTaxNSW <- function(landVal){
        
        threshold <- 734000
        premium <- 4488000
        
        if (landVal < threshold){
            return (0)
        }else if (landVal > premium) {
            return (60164 + (0.02 * (landVal - premium)))
        }else{
            return (100 + (0.016 * (landVal - threshold)))
        }
        
    }
    
    calculateStampDutyNSW <- function(houseVal){
        
        if (houseVal <= 14000) {
            return (0.0125 * houseVal)
        }else if(houseVal <= 30000){
            return (175 + (0.015 * (houseVal - 14000)))
        }else if(houseVal <= 81000){
            return (415 + (0.0175 * (houseVal - 30000)))
        }else if(houseVal <= 304000 ){
            return (1307 + 0.035 * (houseVal - 81000))
        }else if(houseVal <= 1013000){
            return (9112 + 0.045 * (houseVal - 304000))
        }else if(houseVal <= 3040000){
            return (41017 + 0.055 * (houseVal - 1013000))
        }else{
            return (152502 + 0.07 * (houseVal - 3040000))
        }
        
    }
    
    # Victoria
    
    calculateLandTaxVIC <- function(landVal){

        if (landVal < 250000){
            return (0)
        }else if (landVal < 600000) {
            return (275 + 0.002 * (landVal - 250000))
        }else if (landVal < 1000000){
            return (975 + 0.005 * (landVal - 600000))
        }else if (landVal < 1800000){
            return (2975 + 0.008 * (landVal - 1000000))
        }else if (landVal < 3000000){
            return(9375 + 0.013 * (landVal - 1800000))
        }else{
            return (24975 + 0.0225 * (landVal - 3000000))
        }
        
    }
    
    calculateStampDutyVIC <- function(houseVal){
        
        if (houseVal <= 25000) {
            return (0.0125 * houseVal)
        }else if(houseVal <= 130000){
            return (350 + (0.024 * (houseVal - 25000)))
        }else if(houseVal <= 960000){
            return (2870 + (0.06 * (houseVal - 130000)))
            
        }else{
            return (0.055 * (houseVal))
        }
        
    }
    
    # Queensland
    calculateLandTaxQLD <- function(landVal){
      
      if (landVal < 600000){
        return (0)
      }else if(landVal < 1000000){
        return (500 + 0.01*(landVal - 600000))
      }else if(landVal < 3000000){
        return (4500 + 0.0165*(landVal - 1000000))
      }else if(landVal < 5000000){
        return (37500 + 0.0125*(landVal - 3000000))
      }else if(landVal < 10000000){
        return (62500 + 0.0175*(landVal - 5000000))
      }
      
    }
    
    calculateStampDutyQLD <- function(houseVal){
      
      if (houseVal < 5000){
        return (0)
      }else if(houseVal < 75000){
        return (0.015 * (houseVal - 5000))
      }else if(houseVal < 540000){
        return (1050 + 0.035 * (houseVal - 75000))
      }else if(houseVal < 1000000){
        return (17325 + 0.045 * (houseVal - 540000))
      }else if(houseVal >= 1000000){
        return (38025 + 0.0575 * (houseVal - 1000000))
      }
      
    }
    
    # South Australia
    calculateLandTaxSA <- function(landVal){
      
      if (landVal < 450000){
        return (0)
      }else if(landVal < 723000){
        return (0.005 * (landVal - 450000))
      }else if(landVal < 1052000){
        return (1365 + 0.0125 * (landVal - 723000))
      }else if(landVal < 1350000){
        return (5477.50 + 0.02 * (landVal - 1052000))
      }else if(landVal >= 1350000){
        return (11437.5 + 0.024 * (landVal - 1350000))
      }
      
    }
    
    calculateStampDutySA <- function(houseVal){
      
      if (houseVal <= 12000){
        return (0.01 * houseVal)
      }else if(houseVal <= 30000){
        return (120 + 0.02 * (houseVal - 12000))
      }else if(houseVal <= 50000){
        return (480 + 0.03 * (houseVal - 30000))
      }else if(houseVal <= 100000){
        return (1080 + 0.035 * (houseVal - 50000))
      }else if(houseVal <= 200000){
        return (2830 + 0.04 * (houseVal - 100000))
      }else if(houseVal <= 250000){
        return (6830 + 0.0425 * (houseVal - 200000))
      }else if(houseVal <= 300000){
        return (8955 + 0.0475 * (houseVal - 250000))
      }else if(houseVal <= 500000){
        return (11330 + 0.05 * (houseVal - 300000))
      }else if(houseVal > 500000){
        return (21330 + 0.055 * (houseVal - 500000))
      }
      
    }
    
    # Tasmania
    calculateLandTaxTAS <- function(landVal){
      
      if(landVal <= 24999){
        return (0)
      }else if(landVal < 350000){
        return (50 + 0.055 * (landVal - 25000))
      }else if(landVal >= 350000){
        return (1837.5 + 0.015 * (landVal - 350000))
      }
      
    }
    
    calculateStampDutyTAS <- function(houseVal){
      
      if(houseVal <= 3000 ){
        return (50)
      }else if(houseVal <= 25000){
        return (50 + 0.0175 * (houseVal - 3000))
      }else if(houseVal <= 75000){
        return (435 + 0.0225 * (houseVal - 25000))
      }else if(houseVal <= 200000){
        return (1560 + 0.035 * (houseVal - 75000))
      }else if(houseVal <= 375000){
        return (5935 + 0.04 * (houseVal - 200000))
      }else if(houseVal <= 725000){
        return (12935 + 0.0425 * (houseVal - 375000))
      }else if(houseVal > 725000){
        return (27810 + 0.045 * (houseVal - 725000))
      }
      
    }
    
    output$distPlot <- renderPlotly({
        
        landVal <- estimateLandVal(input$houseVal)
        
        if (input$state == "NSW"){
          landTax <- calculateLandTaxNSW(landVal)
          stampDuty <- calculateStampDutyNSW(input$houseVal)
        }else if(input$state == "VIC"){
          landTax <- calculateLandTaxVIC(landVal)
          stampDuty <- calculateStampDutyVIC(input$houseVal)
        }else if(input$state == "QLD"){
          landTax <- calculateLandTaxQLD(landVal)
          stampDuty <- calculateStampDutyQLD(input$houseVal)
        }else if(input$state == "SA"){
          landTax <- calculateLandTaxSA(landVal)
          stampDuty <- calculateStampDutySA(input$houseVal)
        }else if(input$state == "TAS"){
          landTax <- calculateLandTaxTAS(landVal)
          stampDuty <- calculateStampDutyTAS(input$houseVal)
        }
       
        
        # 2.69% interest rate, paid back over 30 years, A=P(1+r/n)^nt, n = 4 (check if interest is quarterly)
        
        cumulativeLandTax <- seq(landTax, landTax*input$years, by = landTax)

        r = 2.69/100
        n = 4 # Check
        P = stampDuty
        t = seq(1,30,1) 
        
        
        stampDutyLoan <- stampDuty * (1 + r/n) ^ (n * t) #cumsum is a possibility
        stampDutyRep <- rep(stampDuty, input$years) #rep(stampDutyLoan[30], input$years)
        
        if (input$years > 30){
            stampDutyLoan <- append(stampDutyLoan, rep(stampDutyLoan[30], input$years - 30))
        }else if (input$years < 30){
            stampDutyLoan <- stampDutyLoan[1:input$years]
        }
        
        df <- data.frame(x = rep(1:input$years,1), land = cumulativeLandTax, stamp = stampDutyLoan, stampSum = stampDutyRep)
        
        ggplotly(ggplot2::ggplot(data = df, aes(x = x)) + 
                     geom_line(aes(y = land, color = "Land tax")) + 
                     geom_line(aes(y = stamp, color = "Stamp duty (interest)")) +
                     geom_line(aes(y = stampSum, color = "Stamp duty (base)")) +
                     labs(x = "Years since purchase", y = "Tax (AUD)") +
                     theme(legend.position = "right") +
                     scale_color_discrete(name = "Legend", labels = c("Land tax", "Stamp duty (interest)", "Stamp duty (base)"))
        )
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
