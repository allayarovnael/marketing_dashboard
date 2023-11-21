library(shiny) 
library(semantic.dashboard)   
library(shiny.semantic)
source('global.R')

# ---------------------------------------------------------------------------------------------
#
#                                         USER INTERFACE
#
# ---------------------------------------------------------------------------------------------

ui <- dashboardPage(

  dashboardHeader(),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(tabName="model", text="Analysis", icon=icon("settings")),
      
      menuItem(tabName="impacts", text="Impacts", icon=icon("chart pie")),
      
      menuItem(tabName="saturation", text="Saturation", icon=icon("rocket")),
      
      menuItem(tabName="exploration", text="Exploration", icon=icon("chart area")),
      
      menuItem(tabName="decomposition", text="Sales", icon=icon("chart bar")),
      
      menuItem(tabName="diagnostics", text="Diagnostics", icon=icon("file"))
      
      #, menuItem(tabName="roi", text="ROI", icon=icon("dashboard"))
      
      #, menuItem(tabName="optimisation", text="Budget allocation", icon=icon("chart line"))
      
      )
    
    ),
  
  dashboardBody(
    
    tabItems(

      tabItem(tabName = "model",
        
        fluidRow(
          textOutput("range"),

          column(14, align = "center",
                 highchartOutput(outputId = "model_plot", height="230px")
                 ),
          
          column(2,
                 valueBoxOutput(outputId = "r_sq"), br(),
                 valueBoxOutput(outputId = "mape"), br(),
                 downloadButton("downloadData", "Export model")
                 )
          ),

        fluidRow(
          
          column(4, align = "center",
                 
                 div(class = "ui horizontal divider", "Model building"), br(),
                 
                 selectInput("DepVar","Dependent Variable",selected=AV_columns[1],choices=AV_columns),
                 
                 selectInput("skeleton_vars", "Choose variable", multiple=TRUE, choices=UV_columns, selected=UV_columns[1]),
                 
                 uiOutput("optimalMediaSelectorUI"), 
                 
                 uiOutput("promotion_action"), 
                 
                 br(),
                 
                 dateRangeInput(
                   inputId = "modelling_period", 
                   label = "Modelling period",
                   start = min(df_skeleton$date),
                   end = max(df_skeleton$date),
                   min = min(df_skeleton$date),
                   max = max(df_skeleton$date),
                   separator = "-"
                 )
                 
                 
                 #,sliderInput("modelling_period", "Modelling period", 
                #             step = 7,                                                            
                #             timeFormat="%Y-%m-%d",
                #             min = min(df_skeleton$date), max = max(df_skeleton$date),
                #             value=c(min(df_skeleton$date), max(df_skeleton$date)))

                 ),
          
          column(8, align = "center",
                
                 div(class = "ui horizontal divider", "Summary"), br(), 
                 
                 uiOutput("RegSum")
                 
                 ),
          
          column(4, align = "center",
                 
                 div(class = "ui horizontal divider", "Media testing"), br(),
                 
                 selectInput("test_media", "Select Media for testing:", choices=MA_columns),
                 
                 checkboxInput("media_timeline", "Include complete timeline")  ,br(), 
                 
                 #sliderInput("adstock_media", "AdStock", value=c(60,69), min=0, max=99, step=3),
                 
                 #sliderInput("hyperbel_media","Hyperbel", value=c(0,1), min=0, max=1, step=0.2),     
                 
                 sliderInput("adstock_media", "Select maximal adstock", value=60, min=0, max=99, step=3),
                 
                 sliderInput("hyperbel_media","Select maximal hyperbel", value=0.2, min=0, max=1, step=0.2),

                 br(), numericInput("hyp_div_media","Hyperbel Potenz", 0, min=-3, max=3, step=1), 
                 
                 br(), br(), 
                 
                 numericInput("divisor_media", "Divisor", 3, min=0, max=6, step=1, width='100%'),
                 
                 br(), br(), 
                 
                 actionButton("calculate_media", "Find optimal combination", width='100%'), br(), 

                 br(), htmlOutput("ff")
                 
                 )
        )
        
      ),
      
      tabItem(tabName = "impacts",
        
        column(16, align = "center",
               
               div(class = "ui horizontal divider", "Impacts"), 
               
               highchartOutput("ImpactsChart", height="350px")
               
               ),
        
        column(16, align = "center",
               
               div(class = "ui horizontal divider", "Sales decomposition"), 
               
               highchartOutput("plotDecomposition", height="400px")
               
               )
        ),
      
      tabItem(tabName = "decomposition",
              
              column(16, align = "center",
                     
                     div(class = "ui horizontal divider", "Sales decomposition"), 
                     
                     highchartOutput("plotDecomposition2", height="700px")
                     
                     )
      ),
      
      tabItem(tabName = "saturation",

              fluidRow( 
                
              column(4, align = "center",
                     
                     div(class = "ui horizontal divider", "Parameter settings"), br(),
                     
                     selectInput("media_sat", "Media", choices=MA_columns),
                     
                     sliderInput("adstock_sat", "AdStock", 75, min=0, max=99, step=3),
                     
                     sliderInput("hyperbel_sat", "Hyperbel", 1, min=0, max=1, step=0.2),
                     
                     sliderInput("hyperbel_pot_sat","Hyperbel (Potenz)", 0, min=-3, max=3, step=1),
                     
                     sliderInput("divisor_sat", "Divisor (Potenz)", 3, min=0, max=6, step=1),
                     
                     uiOutput("cutoff_slider")
                     
                     ),
              
              column(12,
                     
                     div(class = "ui horizontal divider", "Marginal utility effects"), br(),

                     highchartOutput(outputId="saturationPlot", height="500px")
                     
                     )
              ),
              
              fluidRow(
                
                highchartOutput("MediaTransformationChart", height="250px")
                
                )
      ),

      tabItem(tabName = "exploration",

        fluidRow(align = "center",

                 column(4, align = "center",
                        
                        selectInput("exploration_var1", "Variable No.1", choices=EXP_columns),
                        
                        radioButtons("exploration_type1", "Type:", inline=TRUE, choices=c("line","column","area"))
                        ),
                 
                 column(4, align = "center",
                        
                        selectInput("exploration_var2", "Variable No.2", choices=EXP_columns),
                        
                        radioButtons("exploration_type2", "Type:", inline=TRUE, choices=c("line","column","area"))
                        )#,
                 
                 #column(4, align = "center",
                #        
                #        sliderInput("exploration_timeline", "Select date range", 
                #                    timeFormat="%Y-%m-%d", step = 7,
                #                    min = min(df_skeleton$date), max = max(df_skeleton$date), 
                #                    value=c(min(df_skeleton$date), max(df_skeleton$date)))
                #        )
                 ),
        
        
        column(16, align = "center",
               
               div(class = "ui horizontal divider", "Exploration Charts: bivariate plots"), br(),
                 
               highchartOutput(outputId = "plotExploration", height="600px"),
               
               textOutput("correlation"),
               
               textOutput("Sums")

               )
        ),
      
      tabItem(tabName = "diagnostics",
              
              column(16, align = "center",
                     div(class = "ui horizontal divider", "Check of multicollinearity"), 
                     highchartOutput("VIF", height="200px"), 
                     div(class = "ui horizontal divider", "Diagnostic of residuals"),
                     highchartOutput(outputId = "plotResiduals", height="350px"), 
                     plotOutput("residuals")
                     #,shiny::verbatimTextOutput("durbin.watson")
                     #,plotOutput("autocorrelation")
                     )
              )
      
    )
  )
)


# # -------------------------------------------------------------------------------------------
#
#                                           SERVER
#
# # -------------------------------------------------------------------------------------------

server <- function(input, output, session){ 
  
  session$onSessionEnded(stopApp)   # force shiny to stop running after closing browser
  
  rv_box <- reactiveValues(   
    optimalMediaDF = select(df_skeleton, date)
  )
  
  df_skeleton_tl <- reactive({
    df_skeleton %>%
      #filter(date >= "2014-12-29") %>%
      #filter(date <= "2017-11-27") 
      filter(date >= input$modelling_period[1]) %>%
      filter(date <= input$modelling_period[2]) 
  })
  
  # Data frame for modelling ----------------------------------------------------------------------
  model_df <- reactive({
    
    model_df <- df_skeleton_tl() 

    model_df <- model_df %>%                                           
      select(date, input$DepVar, input$skeleton_vars) %>%                
      left_join(rv_box$optimalMediaDF) %>%
      left_join(promotionAction()) 

    return(model_df)
  })
  
  ##### Promotion Action --------------------------------------------------------------------------
  output$promotion_action <- renderUI({
    selectInput("dummy_vars", "Promotion action", multiple=TRUE, 
                choices = names(promotionAction())[-1])
  })
  
  promotionAction <- reactive({
    
    promo_df <- df_skeleton %>%
      select(date) %>%
      mutate(value = 1, date_col = date) %>%
      spread(date_col, value, fill=0)
    
    names(promo_df) <- gsub('-', '_', names(promo_df))
    names(promo_df)[-1] <- paste0("Woche_", names(promo_df)[-1])
    return(promo_df)
    
  })
  
  # Linear model ----------------------------------------------------------------------------------
  independentVars <- reactive({
    c(input$skeleton_vars, input$optimalMediaSelector, input$dummy_vars)          # collection of independent variables taken from different inputs
  })
  
  lm1 <- reactive({
    lm(reformulate(c('1', independentVars()), input$DepVar), model_df())          # '1' stands for intercept
  })
  
  # Graph of fitted model -------------------------------------------------------------------------
  df_fitted <- reactive({
    data.frame(week   = model_df()$date,
               fitted = lm1()$fitted.values,
               real   = lm1()$fitted.values + lm1()$residuals) %>%
      gather("type", "value", -week)
  })
  
  output$model_plot <- renderHighchart({
    hchart(df_fitted(), "line", hcaes(x=week, y=round(value,2), group=type), showInLegend = F) %>%
      hc_xAxis(title = "") %>%
      hc_yAxis(title = "") %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_tooltip(shared=TRUE)
  })  

  # Summary of fitted model -----------------------------------------------------------------------
  output$RegSum <- renderUI({
    HTML(stargazer(lm1(), type="html", title="", keep.stat="n", dep.var.labels=input$DepVar))        # html-formatted regression summary
  })
  
  # Chart with impacts
  output$ImpactsChart <- renderHighchart({
    impactsBarChart(model = lm1())
  })
  
  # Calculation of optimal media transformation
  calculateOptimalTransform <- eventReactive(input$calculate_media, {
    
    if (input$media_timeline == TRUE){
      xx <- reactive({
        AdHypCombinations(df_skeleton, input$test_media,
                          adstockRange = c(0, input$adstock_media[1]),
                          adstockStep = 1,
                          hyperbelRange = c(0*10^input$hyp_div_media,
                                            input$hyperbel_media[1]*10^input$hyp_div_media),
                          hyperbelStep = 0.1*10^input$hyp_div_media,
                          divisor = 10^(input$divisor_media), append = FALSE) %>%
          mutate(date = df_skeleton$date) %>%
          semi_join(df_skeleton_tl(), by = "date") %>%
          select(-date)
        
      })
    } else {
      xx <- reactive({
        AdHypCombinations(df_skeleton_tl(), input$test_media,
                          adstockRange = c(0, input$adstock_media[1]),
                          adstockStep = 1,
                          hyperbelRange = c(0*10^input$hyp_div_media,
                                            input$hyperbel_media[1]*10^input$hyp_div_media),
                          hyperbelStep = 0.1*10^input$hyp_div_media,
                          divisor = 10^(input$divisor_media), append = FALSE)
      })
    }

    # if (input$media_timeline == TRUE){
    #   xx <- reactive({
    #     AdHypCombinations(df_skeleton, input$test_media,
    #                       adstockRange = c(input$adstock_media[1], input$adstock_media[2]),
    #                       adstockStep = 1,
    #                       hyperbelRange = c(input$hyperbel_media[1]*10^input$hyp_div_media,
    #                                         input$hyperbel_media[2]*10^input$hyp_div_media),
    #                       hyperbelStep = 0.1*10^input$hyp_div_media,
    #                       divisor = 10^(input$divisor_media), append = FALSE) %>%
    #       mutate(date = df_skeleton$date) %>%
    #       semi_join(df_skeleton_tl(), by = "date") %>%
    #       select(-date)
    # 
    #   })
    # } else {
    #   xx <- reactive({
    #     AdHypCombinations(df_skeleton_tl(), input$test_media,
    #                       adstockRange = c(input$adstock_media[1], input$adstock_media[2]),
    #                       adstockStep = 1,
    #                       hyperbelRange = c(input$hyperbel_media[1]*10^input$hyp_div_media,
    #                                         input$hyperbel_media[2]*10^input$hyp_div_media),
    #                       hyperbelStep = 0.1*10^input$hyp_div_media,
    #                       divisor = 10^(input$divisor_media), append = FALSE)
    #   })
    # }

    yy <- reactive({lmGridOptimization(lm1(), xx(), p=0.1)})

    if (yy() != 'No significant improvement'){
      rv_box$optimalMediaDF <- rv_box$optimalMediaDF %>%
        inner_join(data.frame(date = df_skeleton_tl()$date, xx() %>% select(yy()) ))
    }

    yy()

  })
  
  output$ff <- renderText({
    paste("<font color=\"#2980b9\"><b>", calculateOptimalTransform(), "</b></font>")
    })
  


  output$optimalMediaSelectorUI <- renderUI({
    selectInput("optimalMediaSelector", "Saved media variables:", multiple=TRUE, 
                choices = names(select(rv_box$optimalMediaDF, -date)))
  })
  
  output$r_sq <- renderValueBox({
    valueBox('R^2', size="tiny", value=round(100*summary(lm1())$r.squared, 2))
  })

  output$mape <- renderValueBox({
    valueBox('Mape', size="tiny", value=round(100*mape(lm1()),2))
  })
  
  output$saturationPlot <- renderHighchart({
    plotSaturation(v = input$media_sat, 
                   media_df = df_skeleton_tl(), 
                   adstock=input$adstock_sat, 
                   hyperbel=input$hyperbel_sat*10^(input$hyperbel_pot_sat), 
                   divisor=10^(input$divisor_sat), 
                   cut_off=input$cutoff_sat)
  })
  
  output$cutoff_slider <- renderUI({
    sliderInput("cutoff_sat","Cut-Off", 0, min=0, max=round(max(pull(df_media_both(), original))), step=1)
  })

  df_media_both <- reactive({
    data.frame(
      date = pull(df_skeleton_tl(), date),
      original = pull(df_skeleton_tl(), input$media_sat),
      transformed = Adstock_Hyperbel(pull(df_skeleton_tl(),input$media_sat), 
                                     input$adstock_sat, 
                                     input$hyperbel_sat*10^(input$hyperbel_pot_sat), 
                                     10^(input$divisor_sat))
      )
  })

  output$MediaTransformationChart <- renderHighchart({
    OriginalTransformedChart(df_media_both())
  })

  output$plotDecomposition <- renderHighchart({
    DecompositionChart(model = lm1())
  })
  
  output$plotDecomposition2 <- renderHighchart({
    DecompositionChart(model = lm1())
  })

  output$plotExploration <- renderHighchart({
    ExplorationChart(df_graph = df_skeleton, 
                     #timeline = input$exploration_timeline,
                     timeline = c("2014-12-29","2017-11-27"),
                     Var_1 = input$exploration_var1, type_1 = input$exploration_type1,
                     Var_2 = input$exploration_var2, type_2 = input$exploration_type2)
  })
  
  output$correlation <- renderText({
    
    df_graph <- df_skeleton %>%
      filter(date >= "2014-12-29") %>%
      filter(date <= "2017-11-27")
      #filter(date >= input$exploration_timeline[1]) %>%
      #filter(date <= input$exploration_timeline[2])
    
    Var_1 <- input$exploration_var1
    Var_2 <- input$exploration_var2
    
    cor(df_graph[[Var_1]], df_graph[[Var_2]])
  })
  
  output$Sums <- renderText({
    
    df_graph <- df_skeleton %>%
      filter(date >= input$exploration_timeline[1]) %>%
      filter(date <= input$exploration_timeline[2])
    
    Var_1 <- input$exploration_var1
    Var_2 <- input$exploration_var2
    
    c(sum(df_graph[[Var_1]]), sum(df_graph[[Var_2]]))
  })
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_model", ".xlsx")   
    },
    content = function(file) {
      export_model(lm1(), file)
    }
  )
  
  output$residuals0 <- renderPlot({
    par(mfrow=c(2,2))
    plot(lm1())
  })
  
  output$residuals <- renderPlot({
    par(mfrow=c(1,3), cex.main=2, cex.lab=2, cex.axis=2, mar=c(4,5,2,2))
    
    # Residuals vs. Fitted:
    residuals = lm1()$residuals
    predicted = lm1()$fitted.values
    plot(residuals ~ predicted, 
         main="Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals", 
         pch=19, col = COLORS[2])
    abline(h=0, lty=2, col=COLORS[1])
    
    # Density of residuals:
    d = density(residuals)$y
    h = hist(residuals, plot=FALSE)
    hist(residuals, main="Histogram of Residuals", xlab="Residuals", 
         col=COLORS[2], prob = TRUE, ylim = c(0,max(max(d), max(h$density))))
    lines(density(residuals), lwd=2, col=COLORS[1])
    
    # QQ-Plot of residuals:
    qqnorm(residuals, pch=19, col=COLORS[2], main="Normal Q-Q Plot of Residuals")
    qqline(residuals, col=COLORS[1], lwd=2)
  }, height=280)
  
  output$VIF <- renderHighchart({
    VIF <- round(car::vif(lm1()),2)
    
    data <- as.data.frame(VIF) %>%
      tibble::rownames_to_column("Variable") %>%
      hchart(type="column", hcaes(Variable, VIF), dataLabels=list(enabled=TRUE)) %>%
      hc_add_theme(hc_theme_smpl())

  })
  
  output$plotResiduals <- renderHighchart({
    
    data <- data.frame(
      date = model_df()$date, 
      residuals = round(lm1()$residuals,2))
    
    hchart(data, hcaes(date, residuals), type="column") %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  #output$durbin.watson <- renderPrint({
  #   car::durbinWatsonTest(lm1())
  # })
   
  #output$autocorrelation <- renderPlot({
  #   acf(lm1()$residuals, lag=52)
  #})
  
}

shinyApp(ui, server)