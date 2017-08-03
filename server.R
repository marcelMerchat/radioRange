library(shinydashboard)
library(dplyr)
library(ggplot2)
library(xtable)
library(gridExtra)

# library(MASS)
# data()
# 
# data(diamonds)
# 
# head(diamonds)

# The binomial distribution

# Argument 1: n
# number of observations
# (The number of times the poll was taken)

# Argument 2: size
# The number of trials for each observation (zero or more). 
# (the number of persons included in the poll.)

# Argument 3: probability
# The probability that a person will vote for a particular candidate 
# (the number of persons included in the poll.)

get_axis_parameters <- function(prob){
    prob <- 100 * prob
    if(prob > 98) {
        max <- 100
        min <-  95
    } else if (prob > 96){
        max <- 100
        min <-  90
    } else if (prob > 94){
        max <- 100
        min <-  85
    } else if (prob > 91){
        max <- 100
        min <-  80
    } else if (prob >= 88){
        max <- 100
        min <-  75
    } else if (prob >= 85){
        max <- 100
        min <-  70
    } else if (prob >= 80){
        max <- 100
        min <-  70
    } else if (prob >= 75){
        max <- 100
        min <-  60
    } else if (prob >= 70){
        max <- 95
        min <-  55
    } else if (prob >= 65){
        max <- 90
        min <-  50
    } else if (prob >= 60){
        max <- 85 
        min <- 45 
    } else if (prob >= 55){
        max <- 80 
        min <-  40
    } else if (prob > 50){
        max <- 75 
        min <-  35
    } else if (prob >= 45){
        max <- 70 
        min <-  30
    } else if (prob >= 40){
        max <- 65 
        min <-  25
    } else if (prob >= 35){
        max <- 60 
        min <-  20
    } else if (prob >= 30){
        max <- 55
        min <-  15
    } else if (prob >= 25){
        max <- 50
        min <-  10
    } else if (prob >= 20){
        max <- 45
        min <-  5
    } else if (prob >= 15){
        max <- 40
        min <-  0
    } else if (prob > 12){
        max <- 30 
        min <-  0
    } else if (prob > 9){
        max <- 25 
        min <-  0
    } else if (prob > 6){
        max <- 20
        min <-  0
    } else if (prob > 4){
        max <- 15
        min <-  0
    } else if (prob > 1){
        max <- 10
        min <-  0
    } else {
        max <- 5
        min <-  0
    } 
    c(min,max)
}

## server <- 
shinyServer(
  function(input, output) {
    
    plot_typical <- reactive({ 
      pt <- input$Pt
      gt <- input$Gt
      r <- input$r
      pr <- gt * pt / (4 * 3.1416 * r^2)
  
})
    
pd <- reactive({ 

    c <- 3 * 10^8 
    f <- as.numeric(input$f) * 10^6
    lambda <- c/f 
    #p <- 0.5
    pt <- as.numeric(input$Pt)
    gt <- as.numeric(input$Gt)
    r <- as.numeric(input$r)

    er <- 1/gt
    
    Ae <- er * ((c/f)^2) / (4*3.1416)
    Ar <- Ae
    
    as.character(pt * gt / (4*3.1416*(1000*r)^2))
    formatC(10^6 * pt * gt / (4*3.1416*r^2), digits = 3, format = "f")
    
    })

pr <- reactive({ 
  c <- 3 * 10^8
  f <- as.numeric(input$f) * 10^6
  lambda <- c/f 
  #p <- 0.5
  pt <- as.numeric(input$Pt)
  gt <- as.numeric(input$Gt)
  r <- as.numeric(input$r)
  er <- 1/gt
  
  Ae <- er * ((c/f)^2) / (4*3.1416)
  Ar <- Ae
  formatC(10^6 * pt * Ae * Ar / (lambda^2*r^2), digits = 3, format = "f")
  
})

prlog <- reactive({ 
  
  c <- 3 * 10^8
  f <- as.numeric(input$f) * 10^6
  lambda <- c/f 
  #p <- 0.5
  pt <- as.numeric(input$Pt)
  ptlog <- log10(pt) 
  gtdesign <- as.numeric(input$Gt)
  r <- 1000 * as.numeric(input$r)

  er <- 1/gtdesign
  Ae <- 0.7 * (lambda^2) / (4*3.1416)
  
  gt <- gtdesign
  Ar <- Ae
  
  gtlog <- 10*log10(gt) 
  grlog <- gtlog
  
  prlog <- 10*(ptlog) + gtlog + grlog - 20*log10(r/1000) - 20*log10(f/10^6) - 32.44 + 30
  
  formatC(prlog, digits = 3, format = "f")
  
})

output$pd <- renderText({pd()})
output$pr <- renderText({pr()})
output$prlog <- renderText({prlog()})

})
  
  





  
