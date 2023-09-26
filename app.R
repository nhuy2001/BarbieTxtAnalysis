source("data.R")

library(shiny)
library(bslib)
library(shinythemes)
library(cachem)

shinyOptions(cache = cache_disk(file.path(dirname(tempdir()), 
                                          "cache")))

# Define UI ----
ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "quartz"),
  
  "ISM 6358 Final Project",
  
  tabPanel("Title",
           sidebarLayout(
             sidebarPanel(div(img(src = "Barbie_Poster.png", height = "850px"), style = "text-align: center;")),
             
             mainPanel(
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               
               h1("Analysis of Barbie (2023) IMDB's Reviews", style = "font-size:55px;"),
               
               br(),
               br(),
               br(),
               
               h2("Business Statement", style = "font-size:40px;"),
               p("The primary objective of this analysis is to assess the reception and public perception of the movie \"Barbie 2023\" among", em("599 IMDb non-spoiler reviews"), "from 21 July 2023 to 29 July 2023. 
             By scrutinizing the reviews and ratings provided by the IMDb community, I seek to gauge audience sentiment, identify key strengths, weaknesses, and areas of improvement within the movie, and evaluate its overall impact on viewers.",
                 style = "font-size:30px;")
             )
           )),
  
  
  tabPanel("Analytics",
           h1("Findings", style = "font-size:55px;") %>% div(style = "text-align: center;"),
           radioButtons("metricChoice", label = h5("Select Metrics"),
                        choices = list("Word", "Bi-Gram"),
                        selected = "Word", inline = T),
           tabsetPanel(
             tabPanel("Term Frequency",
                      fluidRow(
                        h3("Word Frequency in Barbie (2023)'s IMDB Reviews") %>% div(style = "text-align: center;"),
                        column(6,
                             plotOutput("wordFreq_Cloud", height = "700px")),
                        column(6,
                             plotOutput("wordFreq_Tree", height = "700px"))
                        )
                      ),
             tabPanel("Sentiment Analysis",
                      fluidRow(
                        h3("Reviewers Sentiment Toward Barbie (2023)") %>% div(style = "text-align: center;"),
                        column(6,
                               plotOutput("sentiment_Cloud", height = "700px")),
                        column(6,
                               plotOutput("sentiment_Tree", height = "700px")),
                        
                        h1(br()),
                        
                        h3("Default Settings Issues") %>% div(style = "text-align: center;"),
                        column(6,
                               plotOutput("ratingSentiment", height = "700px")),
                        column(6,
                               plotOutput("deltaSentiment", height = "700px"))
                      )),
             tabPanel("Topic Modeling/Bi-Gram Network",
                      fluidRow(
                        h2("Topic Modeling and Bi-Gram Network") %>% div(style = "text-align: center;"),
                        column(6,
                               plotOutput("topicModeling", height = "700px")),
                        column(6,
                               plotOutput("bigramNetwork", height = "700px"))
                      ))
           )),
  
  tabPanel("Recommendations",
           sidebarLayout(
             position = "right",
             sidebarPanel(img(src = "ken_1.png", height = "850px") %>% div(style = "text-align: center;"),
                          em("my idol") %>% p(style = "text-align: center;")),
             mainPanel(
               h1("Recommendations", style = "font-size:55px; text-align: center;"),
               div(img(src = "recommendation.png", height = "750px"), style = "text-align: center;")
             )
           )),
  
  tabPanel("3Ws",
           h1("Thoughts on the Project (aka., 3Ws)", style = "font-size:55px; text-align: center;"),
           br(),
           img(src = "3Ws.png", height = "800px") %>% div(style = "text-align: center;")
           )
  )

# Define server logic ----
server <- function(input, output) {
  #Term Frequency
  output$wordFreq_Cloud <- renderPlot({
    if(input$metricChoice == "Word") {
      wordCount %>% 
        head(100) %>%
        ggplot(., aes(label = word, size = n, color = n))+
        geom_text_wordcloud(shape = "diamond")+
        scale_color_gradient(low = "#4CC9F0", high = "#B4E9F9")+
        scale_size_area(max_size = 40)+
        theme_void()+
        theme(text = element_text(size = 24, color = "white", face = "bold"),
              plot.title = element_text(hjust = .5),
              panel.background = element_rect(fill = "#33377f"))
    } 
    else {
      bigram_counts %>% 
        head(100) %>%
        ggplot(., aes(label = word12, size = n, color = n))+
        geom_text_wordcloud(shape = "diamond")+
        scale_color_gradient(low = "#54E637", high = "#ACF39D")+
        scale_size_area(max_size = 30)+
        theme_void()+
        theme(text = element_text(size = 24, color = "white", face = "bold"),
              plot.title = element_text(hjust = .5),
              panel.background = element_rect(fill = "#33377f"))
    }
  }) %>% bindCache(input$metricChoice)
  
  output$wordFreq_Tree <- renderPlot({
    if(input$metricChoice == "Word") {
      wordCount %>% head(100) %>%
        ggplot(., aes(area = n, fill = n, 
                      label = paste(word, "\ncount:", n)))+
        geom_treemap()+
        geom_treemap_text(place = "centre", color = "white")+
        scale_fill_gradient(low = "#052C39", high = "#0C7797")+
        theme(legend.position = "none",
              plot.background = element_rect(fill = "#33377f"))
    } 
    else {
      bigram_counts %>% head(100) %>%
        ggplot(., aes(area = n, fill = n, 
                      label = paste(word12, "\ncount:", n)))+
        geom_treemap()+
        scale_fill_gradient(low = "#0F3607", high = "#289013")+
        geom_treemap_text(place = "centre", color = "white")+
        theme(legend.position = "none",
              plot.background = element_rect(fill = "#33377f"))
    }
  })  %>% bindCache(input$metricChoice)
  
  #Sentiment
  output$sentiment_Cloud <- renderPlot({
    if(input$metricChoice == "Word") {
      wordSent %>% group_by(sentiment) %>% slice_max(n, n = 100) %>% ungroup() %>% 
        ggplot(aes(label = word, size = n, color = sentiment))+
        geom_text_wordcloud(shape = "diamond")+
        scale_color_manual(values = c("#FF470A", "#009FB7"))+
        scale_size_area(max_size = 30)+
        theme_void()+
        theme(text = element_text(size = 24, color = "white", face = "bold"),
              panel.background = element_rect(fill = "#33377f"))
    }
    else {
      sentiment_noNeutral %>% group_by(sentiment) %>% slice_max(n, n = 50) %>% ungroup() %>% 
        ggplot(aes(label = word12, size = n, color = sentiment))+
        geom_text_wordcloud(shape = "diamond")+
        scale_color_manual(values = c("#FF470A", "#289013"))+
        scale_size_area(max_size = 20)+
        theme_void()+
        theme(text = element_text(size = 24, color = "white", face = "bold"),
              panel.background = element_rect(fill = "#33377f"))
    }
  }) %>% bindCache(input$metricChoice)
  
  output$sentiment_Tree <- renderPlot({
    if(input$metricChoice == "Word") {
      wordSent %>% group_by(sentiment) %>% slice_max(n, n = 10) %>% 
        ungroup() %>% 
        mutate(word = reorder(word,n)) %>%
        ggplot(., aes(n, word, fill = value))+
        geom_col()+
        scale_fill_gradient2(position="bottom" ,
                             low = "#BF3100",
                             mid = "#E6E6EA",
                             high = "#009FB7")+
        facet_wrap(~sentiment, scales = "free_y")+
        scale_x_continuous(name = "Count", breaks = scales::pretty_breaks(n = 10))+
        theme_classic()+
        theme(axis.title.y = element_blank(),
              text = element_text(size = 24, color = "white"),
              axis.text = element_text(color = "white", size = 24),
              legend.position = "bottom",
              legend.key.size = unit(1.5, "cm"),
              strip.text = element_text(size = 24, color = "white"),
              strip.background = element_rect(fill = "#33377f"),
              panel.background = element_rect(fill = "#33377f"),
              plot.background = element_rect(fill = "#33377f"),
              legend.background = element_rect(fill = "#33377f"))
    }
    else {
      sentiment_noNeutral %>% group_by(sentiment) %>% 
        slice_max(n, n = 10) %>% 
        ungroup() %>% 
        mutate(word = reorder(word12,n)) %>%
        ggplot(., aes(n, word, fill = compound))+
        geom_col()+
        scale_fill_gradient2(position="bottom" ,
                             low = "#BF3100",
                             mid = "#E6E6EA",
                             high = "#289013")+
        facet_wrap(~sentiment, scales = "free_y")+
        theme_classic()+
        scale_x_continuous(name = "Count", breaks = scales::pretty_breaks(n = 10))+
        theme(axis.title.y = element_blank(),
              text = element_text(size = 24, color = "white"),
              axis.text = element_text(color = "white", size = 24),
              legend.position = "bottom",
              legend.key.size = unit(1.5, "cm"),
              strip.text = element_text(size = 24, color = "white"),
              strip.background = element_rect(fill = "#33377f"),
              panel.background = element_rect(fill = "#33377f"),
              plot.background = element_rect(fill = "#33377f"),
              legend.background = element_rect(fill = "#33377f"))
    }
  })  %>% bindCache(input$metricChoice)
  
  output$ratingSentiment <- renderPlot({
    ggplot(review_analysis, aes(as.factor(rating), fill = sentiment))+
      geom_bar()+
      theme_classic()+
      scale_fill_manual(values = c("#BF3100", "#009FB7"))+
      theme(text = element_text(size = 24, color = "white"),
            axis.text = element_text(color = "white", size = 24),
            legend.position = "bottom",
            legend.key.size = unit(1.5, "cm"),
            plot.title = element_text(face = "bold"),
            panel.background = element_rect(fill = "#33377f"),
            plot.background = element_rect(fill = "#33377f"),
            legend.background = element_rect(fill = "#33377f"))+
      scale_x_discrete(name = "IMDB's Rating")+
      scale_y_continuous(name = "Count")+
      labs(title = "Reviewer's Rating versus General Sentiment",
           subtitle = "A large portion of positive review got defaulted to 1* rating")
  })
  
  output$deltaSentiment <- renderPlot({
    ggplot(sentComb ,aes(date, perc, fill = sentiment))+
      geom_col()+
      scale_fill_manual(values = c("#BF3100", "#009FB7"))+
      theme_classic()+
      scale_x_date(name = "Date", date_breaks = "1 day", date_labels = "%B %d")+
      theme(text = element_text(size = 24, color = "white"),
            axis.text = element_text(color = "white", size = 24, hjust = 1),
            axis.text.x = element_text(angle = 45),
            legend.position = "none",
            legend.key.size = unit(1.5, "cm"),
            plot.title = element_text(face = "bold"),
            panel.background = element_rect(fill = "#33377f"),
            plot.background = element_rect(fill = "#33377f"),
            legend.background = element_rect(fill = "#33377f"))+
      scale_y_continuous(name = "Percentage")+
      labs(title = "IMDB's Review Sentiments One Week After Premier")
  })
  
  output$topicModeling <- renderPlot({
    ggplot(word_probs, aes(term, beta, fill = as.factor(topic)))+
      geom_col(show.legend = F)+
      facet_wrap(~topic, scales = "free_y")+
      coord_flip()+
      theme_classic()+
      theme(text = element_text(size = 18, color = "white"),
            axis.text = element_text(color = "white", size = 18, hjust = 1),
            axis.text.x = element_text(angle = 45),
            legend.position = "none",
            legend.key.size = unit(1.5, "cm"),
            plot.title = element_text(face = "bold"),
            panel.background = element_rect(fill = "#33377f"),
            strip.text = element_text(size = 24, color = "white"),
            strip.background = element_rect(fill = "#33377f"),
            plot.background = element_rect(fill = "#33377f"),
            legend.background = element_rect(fill = "#33377f"))
  })
  
  #Bi-gram Network
  output$bigramNetwork <- renderPlot({
    ggraph(bigram_graph, layout = "fr")+
      geom_edge_link(aes(edge_alpha = n), show.legend = F,
                     arrow = a, end_cap = circle(.07, "inches"))+
      geom_node_point(color = "#FF1493", size = 5)+
      geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 6, color = "white")+
      theme_void()+
      theme(plot.background = element_rect(fill = "#33377f"))
  })
}

# Run the app ----

shinyApp(ui, server)