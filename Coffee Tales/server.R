library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(maps)
library(plotly)
library(dplyr)
library(shinyWidgets)
library(wordcloud2)
library(tm)
library(RColorBrewer)
library(scales) 
library(forcats)
library(htmlwidgets)
library(DT)


shinyServer(server <- function(input, output, session) {
  
  coffee <- read.csv("simplified_coffee.csv")
  
  coffee <- coffee %>%
    mutate(origin = fct_recode(origin,
                               "Hawaii" = "Hawai'I",
                               "Republic of Congo" = "Democratic Republic Of The Congo")) %>%
    mutate(loc_country = fct_recode(loc_country,
                                    "Taiwan" = "New Taiwan",
                                    "Hawaii" = "Hawai'i"
    ))
  
  
  my_colors <- c("#402218","#865439", "#C68B59", "#62959C", "#FCDEC0", "#AC7D88")
  
  output$pieChart <- renderPlot({
    data <- coffee %>%
      count(loc_country) %>%
      group_by(loc_country) %>%
      summarise(n = sum(n), .groups = 'drop') %>%
      mutate(prop = n / sum(n)) %>%
      arrange(desc(n)) %>%
      mutate(ymax = cumsum(prop), ymin = c(0, head(ymax, n=-1))) %>%
      mutate(loc_country = factor(loc_country, levels = unique(loc_country))) %>%
      slice_max(n, n = 6, with_ties = FALSE)
    
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=loc_country)) +
      geom_rect(color = "white", size = 0.5) +
      coord_polar(theta = "y") +
      xlim(c(2, 4)) +
      scale_fill_manual(values = rep(my_colors, length.out = n_distinct(coffee$loc_country))) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme_void() +
      theme(legend.position = "bottom",
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10))
    })
    
    output$pieChartOrigin <- renderPlot({
      data <- coffee %>%
        count(origin) %>%
        group_by(origin) %>%
        summarise(n = sum(n), .groups = 'drop') %>%
        mutate(prop = n / sum(n)) %>%
        arrange(desc(n)) %>%
        mutate(ymax = cumsum(prop), ymin = c(0, head(ymax, n = -1))) %>%
        mutate(origin = factor(origin, levels = unique(origin))) %>%
        slice_max(n, n = 10, with_ties = FALSE) 
      
      ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = origin)) +
        geom_rect(color = "white", size = 0.5) +
        coord_polar(theta = "y") +
        xlim(c(2, 4)) +
        scale_fill_manual(values = rep(my_colors, length.out = n_distinct(coffee$origin))) +
        labs(x = NULL, y = NULL, fill = NULL) +
        theme_void() +
        theme(legend.position = "bottom",
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 10))
    })
    
    output$mapView <- renderPlotly({
      # Load world map data
      worlddata <- map_data("world")
      worlddata <- worlddata %>%
        mutate(region = fct_recode(region,
                                   "Republic of Congo" = "Democratic Republic of the Congo"))
      # Aggregate coffee data
      coffee_agg <- coffee %>% 
        group_by(origin) %>% 
        summarise(
          mean_rating = mean(rating),
          avg_price = mean(X100g_USD),
          count_brands = n(),
          .groups = 'drop'
        )
      
      # Join with world map data
      wbmap <- left_join(coffee_agg, worlddata, by = c("origin" = "region"))
      
      # Plot
      plot <- ggplot() +
        geom_polygon(data = worlddata, aes(long, lat, group = group, text=region), size=0.15, fill = "gray90", color = "white") +
        geom_polygon(data = wbmap, aes(long, lat, group = group, fill = origin, text=paste("Origin:", origin, "\nBrands:", count_brands, "\nAvg. Price: $", round(avg_price,2), "\nAvg. Rating:", round(mean_rating, 2))), size=1) +
        coord_quickmap() +
        theme_void() +
        scale_fill_manual(values = rep(my_colors, length.out = n_distinct(wbmap$origin))) +
        theme(legend.position = "bottom") +
        guides(fill = 'none')
      
      # Convert ggplot to plotly for interactive tooltips
      ggplotly(plot, tooltip = "text") %>%
        config(displayModeBar = FALSE)
    })
    
    observe({
      req(coffee)  # Ensure that coffee is loaded and available
      
      # Update origin select input
      updateSelectInput(session, "origin",
                        choices = c('Any Country', unique(as.character(coffee$origin))),
                        selected = 'Any Country')

      updatePickerInput(session, "label",
                        choices = c(unique(as.character(coffee$name))))
    })
    
    
    output$userSelection <- renderDataTable({ 

      rating_bounds <- strsplit(input$ratingRange, " - ")[[1]]
      rating_low <- ifelse(input$ratingRange == "Any Rating", 0, as.numeric(rating_bounds[1]))
      rating_high <- ifelse(input$ratingRange == "Any Rating", 100, as.numeric(rating_bounds[2]))
      
      filtered_coffee <- coffee %>%
        filter((input$origin == "Any Country" | origin == input$origin),
               (input$roast == "Any Roast" | roast == input$roast),
               X100g_USD >= input$PriceRange[1],
               X100g_USD <= input$PriceRange[2],
               rating >= rating_low,
               rating <= rating_high) %>%
        select(-review_date) %>%
        rename(
          `Name` = name,
          `Roaster` = roaster,
          `Roast Type` = roast,
          `Country of Roasting` = loc_country,
          `Origin of Beans` = origin,
          `Price` = X100g_USD,
          `Rating` = rating,
          `Review` = review
        )
      
      if (nrow(filtered_coffee) > 0) {
        datatable(filtered_coffee) 
      } else {
        datatable(data.frame())
      }
      
    })
    
    output$label_plot <- renderWordcloud2({
      
      req(input$label)
      
      text <- coffee %>%
        filter(name == input$label) %>%
        pull(review)
      
      # Create a text corpus
      corpus <- Corpus(VectorSource(text))
      
      # Clean up the text data
      corpus <- tm_map(corpus, content_transformer(tolower)) # Convert to lower case
      corpus <- tm_map(corpus, removePunctuation)            # Remove punctuation
      corpus <- tm_map(corpus, removeNumbers)                # Remove numbers
      corpus <- tm_map(corpus, removeWords, stopwords("en")) # Remove common stopwords
      
      corpus <- corpus[sapply(corpus, function(doc) length(unlist(strsplit(as.character(doc), " "))) > 0)]
      
      dtm <- TermDocumentMatrix(corpus)
      
      # Convert matrix to a frequency table
      m <- as.matrix(dtm)
      word_freqs <- sort(rowSums(m), decreasing = TRUE)
      
      # Convert to dataframe
      df <- data.frame(word = names(word_freqs), frequency = word_freqs, stringsAsFactors = FALSE)
      
      # Change the background color
      wordcloud2(df, size=0.5, color=rep_len(my_colors, nrow(df)), backgroundColor=input$background, shape = 'circle')
      
    })
    
    output$price <- renderPlotly({

      req(input$compare)  
      
      data <- coffee %>%
        filter(origin != 'Tanzania') %>%
        {if (input$compare == "roast") filter(., !!rlang::sym(input$compare) != "") else .} %>%
        group_by(!!rlang::sym(input$compare)) %>%
        summarize(average_price = mean(X100g_USD, na.rm = TRUE)) %>%
        arrange(desc(average_price)) %>%
        top_n(as.numeric(input$top), average_price) %>%
        mutate(ordering = factor(!!rlang::sym(input$compare), levels = unique(!!rlang::sym(input$compare)))) 
      
      
      plot <- ggplot(data, aes(x = ordering, y = average_price, fill = ordering, text = paste("Average Price: $", format(round(average_price, 2), big.mark = ",")))) +
        geom_col(show.legend = FALSE) +
          labs(
            x = if (input$compare == "loc_country") {"\nRoaster Country"} 
            else if (input$compare == "roaster") {"\nRoaster"}
            else if (input$compare == "origin") {"\nOrigin Country"}
            else {"\nRoast Type"},
            y = "Average Price per 100g (USD)\n",
            title = paste("Top", input$top, "Most Expensive Coffees by", if (input$compare == "loc_country") {"Roaster Country"}
                          else if (input$compare == "roaster") {"Roaster"}
                          else if (input$compare == "origin") {"Origin Country"}
                          else if (input$compare == "roast") {"Roast Type"}
                          else {"Brand Name"})
          ) +
          guides(fill = FALSE) +
          theme_minimal() +
          scale_fill_manual(values = rep(my_colors, input$top)) +
          scale_y_continuous(labels = label_dollar()) + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                plot.title = element_text(size = 20),
                axis.text = element_text(size = 12))
          
          ggplotly(plot, tooltip = "text") %>%
            config(displayModeBar = FALSE)
      })
    
    output$rating <- renderPlotly({
      req(input$compare_rating, input$rated)
      
      data <- coffee %>%
        filter(rating >= as.numeric(input$rated)) %>%
        group_by(!!rlang::sym(input$compare_rating)) %>%
        summarize(count = n()) %>%
        arrange(desc(count)) %>%
        top_n(as.numeric(input$top_rating), count) %>%
        mutate(ordering = factor(!!rlang::sym(input$compare_rating), levels = unique(!!rlang::sym(input$compare_rating))))
      
      plot <- ggplot(data, aes(x = ordering, y = count, fill = ordering, text = paste("Rating:", input$rated, 'or Higher', '\nNumber of observations:', count))) +
        geom_col(show.legend = FALSE) +
        labs(
          x = if (input$compare_rating == "loc_country") {"\nRoaster Country"} 
          else if (input$compare_rating == "origin") {"\nOrigin Country"}
          else {"\nRoaster"},
          y = 'Number of Observations\n',
          title = paste("Top Coffees by", if (input$compare_rating == "loc_country") {"Roaster Country"}
                        else if (input$compare_rating == "origin") {"Origin Country"}
                        else {"Roaster"}, "with Rating", input$rated, "or Higher")
        ) +
        guides(fill = FALSE) +
        theme_minimal() +
        scale_fill_manual(values = rep(my_colors, input$top_rating)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              plot.title = element_text(size = 20),
              axis.text = element_text(size = 12))
      
      ggplotly(plot, tooltip = "text") %>%
        config(displayModeBar = FALSE)
    })
    
})
