#SharpieGate2020
#Larri Miller
# Dec 2020

library(shiny)
#library(rCharts)
library(lubridate)
library(highcharter)
library(leaflet)
library(quanteda)

# UI
ui <- fluidPage(
    
    # Application title
    titlePanel("#SharpieGate and the 2020 U.S. Presidential Election"),
    
    p(
        class = "text-muted",
        paste("The United States 2020 election has been shaped by current president
          Donald Trump's complaints of widespread election fraud. Among the spread
          of disinformation is the Sharpie Gate conspiracy, based on claims that
          ballots marked with Sharpies could not be read by vote-scanning machines
          in Arizona. Despite Arizona officials confirming that the machines can
          read Sharpie-marked ballots, Trump lawyers and MAGA supporters have 
          latched onto the claims. This app visualizes the textual
          content of and user location for 10,174 retweeted tweets containing #SharpieGate.
          This Twitter data was collected on November 17, 2020."
        )
    ),
    
    
    sidebarLayout(
        sidebarPanel(        
            helpText(h5("Use the slider below to adjust the minimum number of retweets a tweet must have to be included in the word cloud.", style = "font-family: 'arial'; font-si12pt")),
            
            sliderInput("slider1", h3("min. retweet count for wordcloud"),
                        min = 0, max = 100, value = 0),
            
            sliderInput("slider2", h3("min. retweet count for geo-mapping"),
                        min = 0, max = 100, value = 0),
            
            p(
                class = "text-muted",
                paste("Use the slider above to adjust the minimum number of retweets a tweet must have to be included in the geo-map."
                )
            )
            
        ),
        
        
        mainPanel(
            plotOutput("wordcloud"),
            leafletOutput("mymap"),
            p(class = "text-muted",paste("Based on the word cloud, it appears that @lawcrimenews 
                                   is especially influential in shaping Twitter discourse 
                                   around #SharpieGate. Other accounts that have reached
                                   at least 100 retweets include @colinkalmbacher, @nharpermn, @marcelias, 
                                   and @popehat. Based on the geomap, it is clear that 
                                   #SharpieGate has been posted around the United States,
                                   and even in Europe. While this data is interesting, it is
                                    limited. Tweets were only collected for one day and thus
                                    represent only a snippet of the overarching conversation. To
                                    learn more, a future project could build on
                                    the insights generated here by collecting tweets
                                   for a longer duration of time, mapping out the hashtag co-occurence
                                    network, and performing more advanced textual analysis.")
            )
        )
    )
)

# SERVER 
server <- function(input, output) {
    
    geocodes <- read.csv("geocodes.csv", header = TRUE)
    geocodes$text <- as.character(geocodes$text)
    
    tweets <- read.csv("tweets.csv", header = TRUE)
    tweets$text <- as.character(tweets$text)
    
    output$wordcloud <- renderPlot({
        dfm <- dfm(tweets[tweets$retweet_count >= input$slider1,]$text, remove = c(stopwords("english"), remove_numbers = TRUE, remove_symbols = TRUE, remove_punct = TRUE))
        dfm <- dfm_select(dfm, pattern = ("#*|@*"))
        set.seed(100)
        textplot_wordcloud(dfm, min_size = 1.5, min_count = 10, max_words = 100,color = rev(RColorBrewer::brewer.pal(10, "RdBu")))
    })
    
    output$mymap <- renderLeaflet({
        
        usericon <- makeIcon(
            iconUrl = geocodes$profile_image_url,
            iconWidth = 15, iconHeight = 15
        )
        
        
        leaflet(data = geocodes[geocodes$retweet_count >= input$slider2,]) %>% 
            addTiles() %>%
            setView(lng = -98.35, lat = 39.50, zoom = 2) %>% 
            addMarkers(lng = ~lng, lat = ~lat,popup = ~ as.character(text),icon = usericon) %>% 
            addProviderTiles("Stadia.Outdoors") %>%  #more layers:http://leaflet-extras.github.io/leaflet-providers/preview/
            addCircleMarkers(
                stroke = FALSE, fillOpacity = 0.5)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)


