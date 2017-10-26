
library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)

data  <-  read.csv("/Users/patrick.chen/Desktop/Shiny/dailys.csv")
data[is.na(data)] <- "None"

plan <- unique(as.character(data$plan))
action <- unique(as.character(data$action))
recurrence <- unique(as.character(data$recurrence))
segment <- unique(as.character(data$segment))

ui<-navbarPage(
    title = "Tabs:",
    tabPanel('Main Tab',fluidPage(titlePanel("Main Accounts"),
                                  fluidRow(downloadButton('downloadData', 'Download')),
                                  
                                  fluidRow(
                                      column(4,selectInput("plan1","Plans:",c("All",plan))),
                                      column(4,selectInput("segment1","Segment:",c("All",segment))),
                                      column(4,selectInput("action1","Actions:",c("All",action)))
                                  ),
                                  # Create a new row for the table.
                                  fluidRow(
                                      DT::dataTableOutput("table")
                                  ),
                                #   create graph
                                  fluidRow(
                                      plotOutput(outputId = "graph")
                                  )
                               )
            ),
    tabPanel('Full Data Tab',DT::dataTableOutput('tab1')),
    tabPanel('Weekly MRR/N',selectInput('variables',"MRR or N: ",c('MRR','N')),
             fluidPage(navlistPanel(
                 tabPanel('Plan',fluidRow(DT::dataTableOutput('plan'))),
                 tabPanel('Recurrence',fluidRow(column(width = 5,DT::dataTableOutput('recurrence')))),
                 tabPanel('Segment',fluidRow((DT::dataTableOutput('segment'))))
             )))

    )

server <- function(input,output)
{ #creates df that is reactive to change
    data1 <-reactive({data
                      if (input$plan1!="All") {data <- data[data$plan == input$plan1,]}
                     if (input$segment1!="All") {data <- data[data$segment == input$segment1,]}
                     if (input$action1!="All") {data <- data[data$action == input$action1,]}
                    data})
    
#     adding elements to table
    output$table <- DT::renderDataTable(DT::datatable(data1()))

#     adding elements to graph
     output$graph <- renderPlot({
         data_table <-(aggregate(data1()$mrr,by=list(Category=data1()$date), FUN=sum))
         ggplot(data=data_table, aes(x=data_table$Category, y=data_table$x))+
         geom_line(group=1)+
         labs(x = "Dates",
              y = "MRR",
              title = "MRR Over Time")+
         theme(axis.text.x = element_text(angle = 45, hjust = 1))
     })
#     download data
    output$downloadData <- downloadHandler(
        filename = function() { "data.csv" },
        content = function(file) {write.csv(data1(), file)}
        )
#     tab 1
    output$tab1 <- DT::renderDataTable(DT::datatable(data1(), options = list(paging = FALSE)))
    
    
# reactive data
    plan <-reactive({data
                      if (input$variables=='MRR'){data <- dcast(data, date ~ plan, value.var="mrr", fun.aggregate=sum)}
                      if (input$variables=='N'){data <- dcast(data, date ~ plan, value.var="mrr", fun.aggregate=length)}
                      data})
    recurrence <-reactive({data
                  if (input$variables=='MRR'){data <- dcast(data, date ~ recurrence, value.var="mrr", fun.aggregate=sum)}
                  if (input$variables=='N'){data <- dcast(data, date ~ recurrence, value.var="mrr", fun.aggregate=length)}
                  data})
    
    segment <-reactive({data
                      if (input$variables=='MRR'){data <- dcast(data, date ~ segment, value.var="mrr", fun.aggregate=sum)}
                      if (input$variables=='N'){data <- dcast(data, date ~ segment, value.var="mrr", fun.aggregate=length)}
                      data})
    
    
    
    output$plan <- DT::renderDataTable(DT::datatable(plan(), options = list(paging = FALSE)))
    output$recurrence <- DT::renderDataTable(DT::datatable(recurrence(), options = list(paging = FALSE)))
    output$segment <- DT::renderDataTable(DT::datatable(segment(), options = list(paging = FALSE)))
}

# load
shinyApp(ui = ui, server = server)
