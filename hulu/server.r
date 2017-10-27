
library(shiny)
library(ggplot2)
library(DT)

function(input,output) {
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
