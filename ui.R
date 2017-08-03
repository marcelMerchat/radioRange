library(shiny)

library(shinydashboard)

library(xtable)
library(gridExtra)

header <- dashboardHeader(
    title = "Transmitter Range"
)

#ui <- fluidPage(
body <- dashboardBody(theme="bootstrap.css",
    tags$style(type='text/css', ".selectize-input { font-size: 20px; line-height: 32px;} .selectize-dropdown { font-size: 20px; line-height: 28px; }"),
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
    fluidRow(
      
        column(width = 4, height = 11, 
                box(width = NULL, solidHeader = TRUE,  title = h2("Channel Path Loss"),
                h4('Marcel Merchat'),
                h4('June 1, 2017'),
                h4(''),
                tags$br(),
                h4('The range of transmitter determined by these factors.'),
                h4('(a) Transmitter power'),
                h4('(b) Transmitter antenna gain'),
                h4('(c) Receiver antenna gain'),
                h4('(d) Noise Factor (NF) of receiver low noise amplifier.'),
                h4('(e) Error-correction coding gain'),
                tags$br(),

                h4('The power budget includes these components:'),
                p('$$ \\begin{align}
                  p(r) &: \\text{Power density at distance $r$ from the transmitter.}\\\\
                  P_t  &: \\text{Power radiated by the transmitter}\\\\
                  G_t   &: \\text{Transmitter antenna gain}\\\\
                  r    &: \\text{Distance from transmitter.}\\\\
                \\end{align}$$'),
               
                p('$$p(r) = G_t \\cdot \\frac {P_t}{4 \\pi r^2}$$'),
                #withMathJax(),
                 # helpText('$$p(r) = G_t \\cdot \\frac {P_t}{4 \\pi r^2}$$'),
                h4('')),
               tags$br(),
               tags$br()
               ),
               column(width = 3, height = 11, 
                      box(width = NULL, solidHeader = TRUE, title = h2("Transmitter Data"),
                                     h3('Enter data here:'),
                                     numericInput('Pt', label=h4("Radiated transmitter power in Watts"), 0.040),
                                     tags$head(tags$style("#Pt{color: blue;
                                                          font-size: 100%;
                                                          #font-style: italic;
                                                          width: 200px;
                                                          height: 40px;
                                                          text-align:center;
                                                          display: block;
                                                          }")),
                                                          
                                     tags$br(),
                                     numericInput('Gt',  label=h4('Transmitter antenna gain factor'), 1.0),
                                     tags$head(tags$style("#Gt{color: blue;
                                                          font-size: 100%;
                                                          width: 200px;
                                                          height: 40px;
                                                          text-align:center;
                                                          }")),
                                     tags$br(),
                                     numericInput('r',  label=h4('Distance from transmitter in kilometers'), 0.1),
                                     tags$head(tags$style("#r{color: blue;
                                                          font-size: 100%;
                                                          width: 200px;
                                                          height: 40px;
                                                          text-align:center;
                                                          }")),
                                     tags$br(),
                                     numericInput('f',  label=h4('Transmitter frequency in MHz'), 5000),
                                     tags$head(tags$style("#f{color: blue;
                                                          font-size: 100%;
                                                          width: 200px;
                                                          height: 40px;
                                                          text-align:center;
                                                          }"))
                      )
               ),
               column(width = 2, height = 11, withMathJax(),
                      box(width = NULL, solidHeader = TRUE, 
                          title = h2("Power at Receiver"), align="left",
                          
                          h3('Calculated Answers:'),
                          # h5('$$ \\begin{align}
                          #   \\text{Received Signal in Decibels ($dBm$)}\\
                          #   \\end{align} $$'),
                          helpText('$$\\text{Received Signal in decibels (dBm)} $$'),
                          verbatimTextOutput('prlog'),
                          tags$head(tags$style("#prlog{color: blue;
                                 font-size: 150%;
                                               font-size: 100%;
                                               width: 200px;
                                               height: 40px;
                                               text-align:center;
                                               }"
                                             )
                                    ),
                          tags$br(),
                          #p('Received Signal in $\\mu$ W'),
                          #p('Field Strength at Receiver $$\\mu$$ W/square-meter'),
                          
                          #helpText('$$\\text{An irrational number} $\\sqrt {2}$ and a fraction $1-\\frac {1}{2}$bb$$'),
                          tags$br(),
                          tags$br(),
                          #withMathJax(sprintf("$$Received Signal in $\\mu$ W$$")),
                          #p(sprintf("$$Received Signal in $\\mu$ W$$")),
                          #p(sprintf(paste("Received Signal ","$$ in $\\mu$ W$$", sep=""))),
                          helpText('$$\\text{Received Signal (} \\mu \\text{W)} $$'),
                          #tags$div(HTML(" ")),
                          #tags$div(HTML(" ")),
                          verbatimTextOutput('pr'),
                          tags$head(tags$style("#pr{color: blue;
                                               font-size: 100%;
                                               width: 200px;
                                               height: 40px;
                                               text-align:center;
                                               }")),
                          tags$br(),
                          tags$br(),
                          tags$br(),
                          # h5('$$ \\begin{align}
                          #   \\text{Field Strength at Receiver ($\\mu$W/square-meter)}\\
                          #   \\end{align} $$'),
                          helpText('$$\\text{Field Strength at Receiver (} \\frac {\\mu W}{m^2} \\text{)}$$'),
                          

                          verbatimTextOutput('pd'),
                          tags$head(tags$style("#pd{color: blue;
                                               font-size: 100%;
                                               width: 200px;
                                               height: 40px;
                                               text-align:center;
                                               }")),
                          tags$br(),
                          tags$br()
                          )
                )
        ), 

    

    fluidRow(
      column(width = 9,
             box(width = NULL,
                 h3('Formula with Decibel Units'),
                 h4('The power budget includes these compoents:'),
                 h4(''),
                 p('$$p_r(dBm) = P_t(dBm) + G_t(dB) + G_r(dB) - 20 \\cdot log_{10} R(km) - 20 \\cdot log_{10}F(MHz) -32.44$$'),
                 withMathJax(),
                 p('The range of the radio is determined by the distance at which the ratio of the received signal
                   to the inherent noise level inside the radio is approximately 10 dB less error correction coding
                   gain.'),
                 p('The noise voltage of a well-designed receiver circuit at the attenna should be proportional to the bandwidth
                   of the baseband.'),
                 p('$$v = 4 \\cdot K \\cdot T \\cdot R \\cdot \\Delta F$$'),
                 p('$$ \\begin{align}
                   v    &: \\text{noise voltage}\\\\
                   K    &: \\text{1.38 $\\cdot 10^{-23}$ Joules/second (Boltzmann)}\\\\
                   T    &: \\text{Temperature in Degrees Kelvin}\\\\
                   R    &: \\text{Receiver circuit impedance at antenna in Ohms}\\\\
                   \\Delta F  &: \\text{Bandwidth of antenna circuit}\\
                   \\end{align}$$'),
                 p('The noise factor parameter for a well designed low noise amplifier with a narrow bandwidth 
                   should be approximately 2 dB.')
                 )
        
        
        
    )),
    
    fluidRow(
        column(width = 10,
               
               h4('Website automatically generated with R tools by Marcel Merchat.'),
               h4('June 1, 2017')
               
        )
    )
    
)


dashboardPage(
    header,
    dashboardSidebar(disable = TRUE),
    body
)


