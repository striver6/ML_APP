library(shiny)
library(shinythemes)
source("step2.R")
source("step2B.R")
# Define UI for dataset viewer app ----
ui <- fluidPage(
  theme = shinytheme("superhero"),
  #themeSelector(),
  navbarPage(
    # theme = "cerulean",  # <--- To use a theme, uncomment this
    
    "Predictive Maintenance",
    
    
    tabPanel("Life prediction",
             sidebarPanel(
               selectInput(inputId = "algorithm",
                           
                           label = "Select algorithm:",
                           
                           choices = c("Decision Forest Regression", "Boosted Decision Tree Regression","Possion")),
               br(), 
               
               br(),
               sliderInput("danger", "预警寿命值：", 1, 100, 30)
               #numericInput(inputId = "danger",
               #             label = "预警寿命值：",
               #             value = 10)
             ),
             # Main panel for displaying outputs ----
             
             
             mainPanel(
               
               
               #将主页面分为两块，一块是结果，另一块是模型评估
               tabsetPanel(type = "tabs",
                           tabPanel("summary graph", plotOutput("plot")),
                           tabPanel("Insufficient engine", tableOutput("fate")),
                           tabPanel("Life of all engines", tableOutput("prediction")),
                           tabPanel("Model evaluation", tableOutput("evaluate"))
               )
               
               
             )
    ),
    ##
    tabPanel("Life warning",
             sidebarPanel(
               selectInput(inputId = "algorithm2",
                           
                           label = "Select algorithm:",
                           
                           choices = c("Two-Class Logistic Regression", "Two-Class Boosted Decision Tree","Two-Class Decision Forest","Two-Class Neural Network"))
               
             ),
             
             mainPanel(
               #灏嗕富椤甸潰鍒嗕负涓ゅ潡锛屼竴鍧楁槸缁撴灉锛屽彟涓€鍧楁槸妯″瀷璇勪及
               tabsetPanel(type = "tabs",
                           tabPanel("summary graph", plotOutput("plot2")),
                           tabPanel("Early warning engine", tableOutput("fate2")),
                           tabPanel("Full engine life", tableOutput("prediction2"))
                           #tabPanel("妯″瀷璇勪及", tableOutput("evaluate2"))
               )
               
               
               
               
             )
             
             
             
             
             
    )
    
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  datasetInput1 <- reactive({#杩欐槸A鐨勯娴嬬殑鐨勬暟鎹?
    switch(input$algorithm,
           "Decision Forest Regression" =compare[,c(1,3)] ,
           "Boosted Decision Tree Regression" = compare[,c(1,4)],
           "Possion" = compare[,c(1,5)])
    
  })
  datasetInput2 <- reactive({#杩欐槸璇勪及妯″瀷鐨勬暟鎹?
    switch(input$algorithm,
           "Decision Forest Regression" =evaluate[c(1),] ,
           "Boosted Decision Tree Regression" = evaluate[c(2),],
           "Possion" = evaluate[c(3),])
    
  })
  
  datasetInput3 <- reactive({#杩欐槸A鐨勭粺璁″浘鐨勬暟鎹?
    switch(input$algorithm,
           "Decision Forest Regression" =compare[,3] ,
           "Boosted Decision Tree Regression" = compare[,4],
           "Possion" = compare[,5])
    
  })
  #浠ヤ笅鏁版嵁鏄痵tep2B
  datasetInput4 <- reactive({#杩欐槸鐢ㄤ綔鍏ㄩ儴寮曟搸浠ュ強fate鐨勬暟鎹?
    switch(input$algorithm2,
           "Two-Class Logistic Regression" =compare2[,c(1,3)] ,
           "Two-Class Boosted Decision Tree" = compare2[,c(1,4)],
           "Two-Class Decision Forest" = compare2[,c(1,5)],
           "Two-Class Neural Network"=compare2[,c(1,6)]
    )
    
  })
  
  datasetInput5 <- reactive({#杩欐槸鐢ㄤ綔B缁熻鍥炬暟鎹?
    switch(input$algorithm2,
           "Two-Class Logistic Regression" =compare2[,3] ,
           "Two-Class Boosted Decision Tree" = compare2[,4],
           "Two-Class Decision Forest" = compare2[,5],
           "Two-Class Neural Network"=compare2[,6])
    
  })
  
  
  output$fate<- renderTable({
    library(dplyr)
    n=input$danger
    d1=datasetInput1()
    d2=d1[d1[,2]<n,]
    colnames(d2)[2] <- "Residual life"
    colnames(d2)[1] <- "Engine ID"
    m=length(d2[,1])/length(d1[,1])
    d3=data.frame(ID="Proportion",cc=m)
    d3=plyr::rename(d3, c("ID"="Engine ID",'cc'='Residual life'))
    
    newData = rbind(d2,d3)
    
  })
  
  output$evaluate <- renderTable({
    
    datasetInput2()
    
  })
  
  output$prediction <- renderTable({
    current=datasetInput1()
    colnames(current)[2] <- "Residual life"
    colnames(current)[1] <- "Engine ID"
    current
  })
  
  output$plot <- renderPlot({
    life=datasetInput3()
    par(mfrow=c(1,2))
    barplot(life, 
            main="Full engine life graph",
            ylab="life",
            xlab="Engine ID")
    hist(life,col = "orange",main='Life distribution map', border = "white")
  })
  
  output$prediction2 <- renderTable({
    current=datasetInput4()
    colnames(current)[2] <- "Residual life"
    colnames(current)[1] <- "Engine ID"
    current
  })
  
  output$plot2 <- renderPlot({
    d1=datasetInput4()
    d2=d1[d1[,2]>0.5,]
    m=length(d2[,1])
    n=length(d1[,1])-length(d2[,1])
    x=c(m,n)
    y=c("maintenance required","No maintenance required")
    colors=c("yellow","cyan")
    piepercent<- paste(round(100*x/sum(x), 2), "%")
    pie(x, labels=piepercent,  main="Early warning map",col = colors )
    legend("topright", y,cex=0.8,fill= colors)
    
  })
  
  output$fate2<- renderTable({
    library(dplyr)
    d1=datasetInput4()
    d2=d1[d1[,2]>0.5,]
    colnames(d2)[2] <- "Maintenance"
    colnames(d2)[1] <- "Engine ID"
    m=length(d2[,1])/length(d1[,1])
    
    d3=data.frame(ID="Proportion",cc=c(m))
    d3=plyr::rename(d3, c("ID"="Engine ID",'cc'='Maintenance'))
    newData2 = rbind(d2,d3)
  })
  
  
  
  
  
  
}
#鎺掑簭鍑芥暟c=compare[order(-compare[,5]),]




# Create Shiny app ----
shinyApp(ui = ui, server = server)