#================================================================
# Author: William O. Agyapong
# Purpose: Shiny application development 
# Date created: 2024-03-31
# Date modified: 2024-03-31
#=================================================================

# load required packages
library(shiny)
library(ggplot2)
library(scales)
library(dplyr)
library(tidytext)
library(wordcloud)
library(lubridate)
# library(DT)


#------ read-in the data
load("obama_tweets_clean.RData")
load("tweet_words.RData")

# for some reason this part doesnt work on the deployed 
# app, so save the sentiments and load here
load("nrc_sentiments.RData") 

#------ basic data preprocessing
years <- unique(year(obama_tweets_clean$date))

#--- create new columns
# Eastern standard time was used to reflect the tweet location time zone, EST,
# since we know that the user tweeted from Washington DC.
obama_tweets_clean <- obama_tweets_clean |>
    mutate(
        hour = hour(with_tz(created_at, "EST")),
        day  = day(with_tz(created_at, "EST")),
        month = month(with_tz(created_at, "EST")),
        year = year(with_tz(created_at, "EST"))
    )

tweet_words <- tweet_words |>
    mutate(year = year(date))
    


#------ Define the user interface (UI)
frontend <- fluidPage(
    
    fluidRow(style = "text-align: center; background-color: blue;
             color:white;", 
        # App title
        titlePanel("Tweet Explorer"),
    ),
    
    tabsetPanel(
        # Home page
        tabPanel("Home",
            # create global inputs
            selectInput("tweet_year", "Select Year: ", 
                        choices = c("All", years)),
            hr(),
            fluidRow(
                column(6,
                       checkboxInput("compare_yearly_trend", 
                                     "Compare yearly trend?"
                                     ),
                       checkboxInput("facet_by_year", "Facet by year?"),
                       # output UI
                       plotOutput("tweet_trend")
                       ),
                
                column(6,
                       sliderInput("num_words", "Number of top words: ",
                                   min = 2, max = 20, value = 10),
                       # plot output UI
                       plotOutput("common_words")
                       )
            ),
            
            fluidRow(
                column(6,
                       numericInput("max_words", "Maximum words: ",
                                    value = 100, min = 20, max = 200,
                                    step = 10),
                       # wordcloud UI
                       plotOutput("wordcloud")
                       ),
                
                column(6,
                       # sentiment dimensions UI
                       plotOutput("sentiments")
                       )
            )
        ),
        
        tabPanel("Data", style = "margin-left:3%; margin-right:3%;",
            fluidRow(
                h3("Actual Data"),
                # tableOutput("data_head")
                DT::dataTableOutput("data_head")
            ),
            
            fluidRow(
                h3("Tokenized tweets"),
                # tableOutput("tweet_words_data")
                DT::dataTableOutput("tweet_words_data")
            )
        ),
        
        tabPanel("Sentiment Analysis",
            
        ),
        
        tabPanel("Work Anlytics",
                 ),
        
        tabPanel("About the App",
            HTML("
            
                 ")
        )
    )
)

# Define the back end logic (server)
backend <- function(input, output, session) {
    
    # create custom reactive elements
    selected_date <- reactive({
        if (input$tweet_year == "All" | input$compare_yearly_trend) {obama_tweets_clean$date}
        else {
            obama_tweets_clean |>
                filter(year == input$tweet_year) |>
                pull(date)
        }
    })
    
    
    selected_tweets <- reactive({
        if (input$tweet_year == "All") obama_tweets_clean
        else {
            obama_tweets_clean |>
                filter(year == input$tweet_year)
        }
    })
    
    selected_tweet_words <- reactive({
        if(input$tweet_year == "All") {tweet_words}
        else {
            tweet_words |>
                filter(year == input$tweet_year)
        }
    })
    
    
    #----- render the tweeting pattern plot
    output$tweet_trend <- renderPlot({
        
        if (input$compare_yearly_trend & input$tweet_year =="All") {
            
            
            gg_adds <- list(
                if (input$facet_by_year) {
                    list(geom_line(show.legend = FALSE),
                    facet_wrap(vars(year), scales = "free") )
                } else {
                    geom_line(show.legend = TRUE) 
                }
            )
            #----- Deriving hourly tweets in a day by year
            hourly_tweet <- selected_tweets() |>
                # get the frequency of tweets for each hour of the day
                count(year, hour) |> 
                # convert frequency (raw counts) to a percent
                group_by(year) |>
                mutate(percent = n / sum(n)) 
            
            # visualizing hour of day tweets were posted
            ggplot(hourly_tweet, aes(hour, percent, color = factor(year))) + 
                geom_point(show.legend = FALSE) +
                # geom_line(show.legend = TRUE) +
                # facet_wrap(vars(year), scales = "free") +
                gg_adds + 
                scale_y_continuous(labels = percent_format()) +
                labs(x = "Hour of day (EST)", y = "% of tweets", color = "",
                     title = "Barack Obama's tweeting pattern per hour of day by year",
                     subtitle = paste("From ", format(obama_tweets_clean$date[length(obama_tweets_clean$date)],
                                                      "%b %d, %Y"), " to ",
                                      format(obama_tweets_clean$date[1],"%b %d, %Y"))) +
                theme_bw() +
                theme(plot.title = element_text(hjust = 0.5),
                      plot.subtitle = element_text(hjust = 0.5)
                )
        } else {
            #----- Deriving hourly tweets in a day
            hourly_tweet <- selected_tweets() |>
                # filter(year == input$tweet_year) |>
                
                # get the frequency of tweets for each hour of the day
                count(hour) |> 
                # convert frequency (raw counts) to a percent
                mutate(percent = n / sum(n)) 
            
            # visualizing hour of day tweets were posted
            ggplot(hourly_tweet, aes(hour, percent)) +
                geom_line(show.legend = FALSE, color="dodgerblue") +
                geom_point(show.legend = FALSE,color="dodgerblue") +
                scale_y_continuous(labels = percent_format()) +
                labs(x = "Hour of day (EST)", y = "% of tweets", color = "",
                     title = "Barack Obama's tweeting pattern per hour of day",
                     subtitle = paste("From ", format(selected_date()[length(selected_date())],
                                                      "%b %d, %Y"), " to ",
                                      format(selected_date()[1],"%b %d, %Y"))) +
                theme_bw() +
                theme(plot.title = element_text(hjust = 0.5),
                      plot.subtitle = element_text(hjust = 0.5)
                )
        }
    })
    
    
    #----- render the most common words bar graph
    output$common_words <- renderPlot({
        # get top 20 most frequently used words by Barack Obama
        top_20_words <- selected_tweet_words()  |>
            # filter(year == input$tweet_year) |>
            group_by(word) |>
            summarise(frequency= n()) |>
            ungroup() |>
            arrange(desc(frequency)) |>
            top_n(input$num_words, frequency) |>
            mutate(word = reorder(word, frequency))
        
        # create a bar chart of top 20 commonly used words
        ggplot(top_20_words) +
            geom_bar(
                aes(x=word, y=frequency), fill = "dodgerblue", alpha=.7,stat="identity") +
            theme_classic() +
            labs(x="Words", y="Number of Times Used",
                 title=paste(input$num_words,"most commom words in Barack Obama's Tweets"),
                 subtitle = paste("From ", format(selected_date()[length(selected_date())],
                                                  "%b %d, %Y"), " to ",
                                  format(selected_date()[1],"%b %d, %Y"))
                 ) +
            theme(legend.position = "none",
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.subtitle = element_text(hjust = .5),
                  axis.text=element_text(size=14),
                  plot.title = element_text(size=18, hjust = 0.5)
            ) +
            coord_flip()
    })
    
    
    #------ render the wordcloud 
    output$wordcloud <- renderPlot({
        # setting seed for reproducibility of same results
        set.seed(22222)
        
        # most common words from Obama
        selected_tweet_words() %>%
            # filter(year == input$tweet_year) |>
            anti_join(stop_words, by = "word") %>%
            count(word) %>%
            with(wordcloud(word, n, max.words = input$max_words,
                           random.order=FALSE,
                           colors= brewer.pal(8,"Dark2"),
                           scale = c(3,0.5)))
    })
    
    
    #------ render the sentiment dimensions
    output$sentiments <- renderPlot({
        # using the nrc lexicon to assign appropriate sentiment to words
        # sentiments <- get_sentiments("nrc")
        # pos_neg_sentiments <- get_sentiments("bing") # classify words as either
        # positive/negative
        
        # count and compare the frequencies of each sentiment appearing in each device
        sentiment_summary <- selected_tweet_words() |>
            # filter(year == input$tweet_year) |>
            left_join(nrc_sentiments, by = "word") |>
            count(screen_name, sentiment) |> # find sentiment score
            tidyr::spread(screen_name, n) |> 
            rename(score = BarackObama) |>
            filter(!is.na(sentiment))
       
        # Creating a barplot to visualize sentiment scores
        ggplot(sentiment_summary) +
            geom_bar(
                aes(x=sentiment, y=score, fill=sentiment),
                stat="identity") +
            scale_fill_brewer(palette = "Set3") +
            theme_classic()+
            labs(x="Sentiment", y="Score",
                 title="Sentiment Scores for Barack Obama's Tweets",
                 subtitle = paste("From ", format(selected_date()[length(selected_date())],
                                                  "%b %d, %Y"), " to ",
                                  format(selected_date()[1],"%b %d, %Y")),
                 caption = "\n Source: Data collected from Twitter's REST API via rtweet") +
            theme(legend.position = "none",
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.subtitle = element_text(hjust = .5),
                  plot.caption = element_text(size = 13, face = "italic", hjust = 0,vjust = 0),
                  axis.text=element_text(size=14),
                  plot.title = element_text(size=18, hjust = 0.5)
            ) +
            coord_flip()
    })
    
    # displaying data
    output$data_head <- DT::renderDataTable({
        # head(obama_tweets_clean)
        obama_tweets_clean
    })
    
    output$tweet_words_data <- DT::renderDataTable({
        # head(tweet_words)
        tweet_words
    })
}

# Run the application
shinyApp(ui=frontend, server=backend)

