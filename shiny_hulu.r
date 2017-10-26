
library(shiny)
library(ggplot2)

data  <-read.csv("hulu_clean.csv")

data

min_clip <- min(data$show_clips_count)
max_clip <- max(data$show_clips_count)
min_ep <- min(data$show_episodes_count)
max_ep <- max(data$show_episodes_count)
min_season <- min(data$show_seasons_count)
max_season <- max(data$show_seasons_count)
min_video <- min(data$show_videos_count)
max_video <- max(data$show_videos_count)

# ui <- fluidPage( headerPanel("Data"),
#     sidebarLayout(
#         sidebarPanel(sliderInput(inputId = "range_clip",label ="Show Clip Count",
#                         min=min_clip,max=max_clip,value=c(min_clip,max_clip),step = 1),
#                     sliderInput(inputId = "range_ep",label ="Show Episode Count",
#                         min=min_ep,max=max_ep,value=c(min_ep,max_ep),step = 1),
#                     sliderInput(inputId = "range_season",label ="Show Season Count",
#                         min=min_season,max=max_season,value=c(min_season,max_season),step = 1),
#                     sliderInput(inputId = "range_video",label ="Show Video Count",
#                         min=min_video,max=max_video,value=c(min_video,max_video),step = 1)
#                      ),
#         mainPanel(
#             tabsetPanel(
#             tabPanel("Avg # of reviews",plotOutput(outputId='hist_graph')),
#             tabPanel("N of shows",plotOutput(outputId = 'n_graph')),
#             tabPanel("Data_table",DT::dataTableOutput("table"))
#             )
#         )
#     )
# )

ui <- fluidPage( headerPanel("Data"),
    sidebarLayout(
        sidebarPanel(sliderInput(inputId = "range_clip",label ="Show Clip Count",
                        min=min_clip,max=max_clip,value=c(min_clip,max_clip),step = 1),
                    sliderInput(inputId = "range_ep",label ="Show Episode Count",
                        min=min_ep,max=max_ep,value=c(min_ep,max_ep),step = 1),
                    sliderInput(inputId = "range_season",label ="Show Season Count",
                        min=min_season,max=max_season,value=c(min_season,max_season),step = 1),
                    sliderInput(inputId = "range_video",label ="Show Video Count",
                        min=min_video,max=max_video,value=c(min_video,max_video),step = 1)
                     ),
        mainPanel(
            tabsetPanel(
            tabPanel("By Genre",navlistPanel(
                tabPanel("Avg # of reviews",plotOutput(outputId='hist_graph1')),
                tabPanel("N of shows",plotOutput(outputId = 'n_graph2')),
                tabPanel("Data_table",DT::dataTableOutput("table"))
            )
                    ),
            tabPanel("By Media Company",navlistPanel(
                tabPanel("Avg # of reviews",plotOutput(outputId='hist_graph')),
                tabPanel("N of shows",plotOutput(outputId = 'n_graph')),
                tabPanel("Data_table",DT::dataTableOutput("table"))
                )
                    )
            )
        )
    )
)

server <- function(input,output) {
    data2 <- reactive({
        a <- subset(data, (show_clips_count>=input$range_clip[1]&
                           show_clips_count<=input$range_clip[2]&
                           show_episodes_count>=input$range_ep[1]&
                           show_episodes_count<=input$range_ep[2]&
                           show_seasons_count>=input$range_season[1]&
                           show_seasons_count<=input$range_season[2]&
                           show_videos_count>=input$range_video[1]&
                           show_videos_count<=input$range_video[2]
                          ))
        return(a)})
#     Graphs for Genre
    output$hist_graph1 <- renderPlot({
        ggplot(data2(),aes(x=show_company_name, y=show_rating))+
        stat_summary(fun.y="mean", geom="bar")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
    
    output$n_graph2 <- renderPlot({
        ggplot(data2(),aes(x=show_company_name)) + 
        geom_bar() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
        
#     Graphs for Media Company
    output$hist_graph <- renderPlot({
        ggplot(data2(),aes(x=show_company_name, y=show_rating))+
        stat_summary(fun.y="mean", geom="bar")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
    
    output$n_graph <- renderPlot({
        ggplot(data2(),aes(x=show_company_name)) + 
        geom_bar() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
    
    output$table <- DT::renderDataTable(DT::datatable(data2()))
    
    }

# load
shinyApp(ui = ui, server = server)


