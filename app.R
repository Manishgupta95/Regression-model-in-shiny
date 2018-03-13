library(shiny)

library(datasets)


ui<- fluidPage(
  titlePanel("Linear Regression Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset","Choose a Dataset", choices = c("BOD","esoph","infert","volcano")),
      tags$br(),
      uiOutput("dv"),
      tags$br(),
      uiOutput("idv")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", verbatimTextOutput("data"), 
                 numericInput("obs","Number of Observation",value = 10), tableOutput("view")),
        tabPanel("Summary Statistics", verbatimTextOutput("summary"), 
                 textInput("text_summary", label = "What you Conclude ?", value = "Enter here...")),
        tabPanel("Histogram",
                 plotOutput("plot_dv"),
                 sliderInput("binsdv","No. of bins",min = 1, max = 100, value = 10),
                 
                 plotOutput("plot_idv"), sliderInput("binsidv","No. of bins",min = 1, max = 100, value = 10)),
                 
        tabPanel("Scatter Plot", plotOutput("scatter")),
        tabPanel("Model", verbatimTextOutput("model"),
                 textInput("text_summary", label = "What you Conclude ?", value = "Enter here...")),
        tabPanel("Residuals",
                 plotOutput("hist_res"), plotOutput("scatter_res"),
                 plotOutput("qqline_res"))
      )
    )
  )
)

server<- function(input,output){
  dataset<- reactive({
    switch(input$dataset,
           "BOD" = BOD,
           "esoph" = esoph,
           "infert" = infert,
           "volcano" = volcano
           
           
           )
  })
  
  output$dv<- renderUI({
    selectInput("dv","Choose Dependent Variable", choices = names(dataset()))
  })
  output$idv<- renderUI({
    selectInput("idv","Choose Independent Variable", choices = names(dataset()))
  })
  formula<- reactive({
    as.formula(paste(input$dv,'~',input$idv))
  })
  
  model<- reactive({
    lm(formula(), data=dataset())
  })
  
    output$view<- renderTable({
      head(dataset(), n=input$obs)
    })
    
    output$summary<- renderPrint({
      summary(cbind(dataset()[input$dv],dataset()[input$idv]))
    })
    
    output$plot_dv<- renderPlot({
      x<- dataset()[,input$dv]
      bins<- seq(min(x), max(x), length.out = input$binsdv + 1)
      hist(x, breaks = bins, xlab = input$dv, col = "grey", main="Dependent variable")
    })
    
    output$plot_idv<- renderPlot({
      x<- dataset()[,input$idv]
      bins<- seq(min(x), max(x), length.out = input$binsidv + 1)
      hist(x, breaks = bins, xlab = input$idv, col = "grey", main="Independent variable")
    })
    
    output$scatter<- renderPlot({
      plot(dataset()[,input$idv], dataset()[,input$dv], xlab = input$idv, ylab = input$dv,
           col= "black")
      abline(lm(dataset()[,input$dv]~dataset()[,input$idv], col="grey", lwd=2))
    })
    
      output$model<- renderPrint({
      summary(model())
    })
    
    output$hist_res<- renderPlot({
      hist(model()$residuals, main = paste(input$dv, '~', input$idv), xlab="Residuals")
    })
    
    output$scatter_res<- renderPlot({
      plot(model()$residuals ~ dataset()[,input$idv], xlab = input$idv , ylab = "Residuals")
      abline(h=0, lty=3)
    })
    
    output$qqline_res<- renderPlot({
      qqnorm(model()$residuals)
      qqline(model()$residuals)
    })
  
}

shinyApp(ui = ui,server = server)