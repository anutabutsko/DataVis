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


dashboardPage(
  dashboardHeader(title = "COFFEE TALES"),
  dashboardSidebar(
    tags$div(HTML("<div style='padding: 20px;'><p>In this App, a user can explore coffee based on the simplified version of the Coffee Analysis Dataset. The data is created from coffee reviews made between 2017-2022, and includes a lot of interesting information, like countries of the roasteries, origins of the beans, roast type, prices, and reviews for each brand of coffee.</p></div>")),
    sidebarMenu(
      menuItem("History of Coffee", tabName = "history", icon = icon("history")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Make Your Cup of Coffee", tabName = "user", icon = icon("coffee"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "history",
              fluidRow(
                tabBox(
                  id = "tabset1", height = "1000px", width = 12,
                  tabPanel("About", icon = icon("info-circle"),
                           titlePanel(h3("Coffee Background", style = "color: #402218; font-size: 36px; font-family: 'Roboto', sans-serif;")),
                           h3("Kaldi the herder and first coffee plant", style = "color: #402218; font-size: 28px; font-family: 'Roboto', sans-serif;"),
                           p("It is believed that the first plants of coffee were discovered by a goat herder named Kaldi in modern-day Ethiopia. The legend tells us that Kaldi noticed that whenever goats eat fruit from a mysterious plant, the goats become so energetic that they don't want to sleep at night. The word about this magic plant travelled quickly, and soon it spread all across the Arabian Peninsula. The cultivation of coffee plant began in 15th century in what we know today as Yemen, soon spreading to Iran, Egypt, Syria, and Turkey."),
                           div(style = "border: 1px solid gray; background-color: #62959C; padding: 10px; border-radius: 5px; margin: 25px 10px 25px 0px; width: 350px; color: white;", 
                            p(strong("Takeaway:"), "Coffee was first discovered in Ethiopia, and spread to the rest of the world through the Arabic world.")),    
                           h3("Coffee cultivation in 21st century", style = "color: #402218; font-size: 28px; font-family: 'Roboto', sans-serif;"),
                           p("Today, we mostly drink coffee made from two types of coffee plants: Arabica and Robusta. Arabica beans are thought to be particularly flavorful and aromatic, and are typically cultivated at higher altitudes, in more mountainous regions of South America, East Africa, and Arabian peninsula. The other type of coffee plant, Robusta, can grow in regions at lower altitudes, like Western and Central Africa, Brazil, and Southeast Asia. Compared to Arabica, Robusta is sturdier, cheaper, and has a higher caffeine concentration."),
                           div(style = "border: 1px solid gray; background-color: #AC7D88; padding: 10px; border-radius: 5px; margin: 25px 10px 25px 0px; width: 470px; color: white;", 
                               p(strong("Takeaway:"), "A more flavorful Arabica is grown in countries like Ethiopia and Colombia, and a more caffeinated Robusta is grown in Brazil and Indonesia.")),    
                           h3("Coffee preprocessing", style = "color: #402218; font-size: 28px; font-family: 'Roboto', sans-serif;"),
                           p("Before coffee reaches our cups, there are multiple steps involved: first, the beans are extracted from the coffee cherries, and then dried. Then, the coffee beans are classified by its characteristics, such as botanical variety, region, altitude, appearance, size, etc. Then, as the beans are classified and graded, they are sold to the roasters. The aroma and the flavor of coffee is developed during the roasting process. The coffee roasted for about 7-20 minutes: the longer the beans are roasted, the darker is the roast."),
                           div(style = "border: 1px solid gray; background-color: #62959C; padding: 10px; border-radius: 5px; margin: 25px 10px 25px 0px; width: 510px; color: white;", 
                               p(strong("Takeaway:"), "Flavour and aroma of the coffee depends on the roasting process: light roasts are more complex and floral, and dark roasts are more rich and smoky.")),    
                           fluidRow(
                             column(6, 
                                    h3("Roaster Location Countries", style = "text-align: center; color: #402218; font-size: 28px; font-family: 'Roboto', sans-serif;"),
                                    plotOutput("pieChart"),
                                    div(style = "border: 1px solid gray; background-color: #FCDEC0; padding: 10px; border-radius: 5px; margin: 25px auto; width: 500px; color: gray40;", 
                                    p("We can see that the majority of the roasteries covered in the dataset are based in the United States and Taiwan.")
                                    )
                             ),
                             column(6, 
                                    h3("Bean Origin Countries", style = "text-align: center; color: #402218; font-size: 28px; font-family: 'Roboto', sans-serif;"),
                                    plotOutput("pieChartOrigin"),
                                    div(style = "border: 1px solid gray; background-color: #FCDEC0; padding: 10px; border-radius: 5px; margin: 25px auto; width: 500px; color: gray40;", 
                                    p("Almost half of the beans in our dataset come from Ethiopia. Ethiopian coffee is considered to be one of the best premium coffee in the world thanks to its high altitude growing conditions.")))
                                    
                           )),
                  tabPanel("Map View", icon = icon("map"),
                    fluidRow(
                      column(12,
                           h3("Bean Origin Countries on the World Map", style = "color: #402218; font-size: 36px; font-family: 'Roboto', sans-serif;"), 
                           p('All of these countries are located in the so-called coffee belt , spanning the Tropic of Cancer and the Tropic of Capricorn.There are about 70 countries that fall in these coordinates, but only 40 are considered to be the "coffee" countries.', style = 'width: 850px;'),
                           div(style = "border: 1px solid gray; background-color: #AC7D88; padding: 10px; border-radius: 5px; margin: 25px 10px 25px 0px; width: 450px; color: white;", 
                           p(strong('Ideal coffee conditions:'), 'tropical climate, high altitude, and few pests.')
                           ),
                           plotlyOutput("mapView", height = 750))
                    )
                  )
                )
              )
      ),
      tabItem(
        tabName = "analysis",
        fluidRow(
          tabBox(
            id = 'tabset3', height = "1000px", width = 12,
            tabPanel('Price', icon = icon("dollar"),
                     titlePanel(h3("Explore the Price", style = "color: #402218; font-size: 36px; font-family: 'Roboto', sans-serif;")),
                         p('What the roasteries in England and Australia have in common, apart from the high prices, is the variety of the coffee they offer:', strong('Geisha.')),
                     div(style = "border: 1px solid gray; background-color: #AC7D88; padding: 10px; border-radius: 5px; margin: 25px 10px 25px 0px; width: 550px; color: white;", 
                         p(strong('FUN FACT:'),  'Geisha, a coffee variety grown in Panama, is the most scarce and expensive coffee in the world. One cup of coffee can cost somewhere between 100-200 US dollars.')),
                         p('Originated in the highlands of Ethiopia (called Gesha), Geisha variety requires very high altitude, has a complicated growing process, and a very low harvest, compared to other coffee plants. It is grown in the western Chiriqí province in Panama, close to the border with Costa Rica.', style = 'width: 850px;'),
                     h3(),
                     fluidRow(
                       column(width = 9,
                              plotlyOutput("price", height = "700px")
                       ),
                       column(width = 3,
                              div(style = "margin-top: 20px; margin-bottom: 20px;", 
                                  h3('Price Comparison by:', style = "color: #402218; font-size: 28px; font-family: 'Roboto', sans-serif;"),
                                  selectInput(inputId = 'compare',
                                              choices = c('Roaster Country' = 'loc_country',
                                                          'Origin Country' = 'origin',
                                                          'Roaster' = 'roaster',
                                                          'Roast Type' = 'roast',
                                                          'Brand Name' = 'name'),
                                              label = NULL,
                                              selected = 'Roaster Country')
                              ),
                              div(style = "margin-top: 20px; margin-bottom: 20px;", 
                                  h3('Show Top:', style = "color: #402218; font-size: 28px; font-family: 'Roboto', sans-serif;"),
                                  selectInput(inputId = 'top',
                                              choices = c(5, 10, 15, 20),
                                              label = NULL,
                                              selected = 10)
                              )
                              
                       )
                     )
            ),
            tabPanel('Rating', icon = icon("star"),
                     titlePanel(h3("Explore the Rating", style = "color: #402218; font-size: 36px; font-family: 'Roboto', sans-serif;")),
                     div(
                       HTML("
                          <p style='font-size: 20px;'>Beans from 13 countries produce exceptional coffee, but 5 countries stand out in particular:</p>
                          <ul>
                              <li>&#x2615; Ethiopia</li>
                              <li>&#x2615; Kenya</li>
                              <li>&#x2615; Panama</li>
                              <li>&#x2615; Hawai'i</li>
                              <li>&#x2615; Colombia</li>
                          </ul>
                      ")
                     ),
                     div(style = "border: 1px solid gray; background-color: #AC7D88; padding: 10px; border-radius: 5px; margin: 25px 10px 25px 0px; width: 750px; color: white;", 
                         p('FUN FACT: According to the Coffee Review website, each coffee can get between 50-100 points. Rating of 95 and higher can be interpreted as an outstanding coffee that is "perfect in structure, flawless, and shockingly distinctive and beautiful".'),
                     ),
                     fluidRow(
                       column(width = 9,
                              plotlyOutput("rating", height = "700px")
                       ),
                       column(width = 3,
                              div(style = "margin-top: 20px; margin-bottom: 20px;", 
                                  h3('Rating Comparison by:', style = "color: #402218; font-size: 28px; font-family: 'Roboto', sans-serif;"),
                                  selectInput(inputId = 'compare_rating',
                                              choices = c('Roaster Country' = 'loc_country',
                                                          'Origin Country' = 'origin',
                                                          'Roaster' = 'roaster'),
                                              label = NULL,
                                              selected = 'origin')
                              ),
                              div(style = "margin-top: 20px; margin-bottom: 20px;", 
                                  h3('Rated:', style = "color: #402218; font-size: 28px; font-family: 'Roboto', sans-serif;"),
                                  selectInput(inputId = 'rated',
                                              choices = c('97 or Higher' = 97,
                                                          '95 or Higher' = 95,
                                                          '90 or Higher' = 90,
                                                          '85 or Higher' = 85,
                                                          '80 or Higher' = 80),
                                              label = NULL,
                                              selected = 95)
                              ),
                              div(style = "margin-top: 20px; margin-bottom: 20px;", 
                                  h3('Show Top:', style = "color: #402218; font-size: 28px; font-family: 'Roboto', sans-serif;"),
                                  selectInput(inputId = 'top_rating',
                                              choices = c(5, 10, 15, 20),
                                              label = NULL,
                                              selected = 10)
                              )
                              
                       )
                     )
            )
          )
        )
      ),
      tabItem(tabName = "user",
              fluidRow(
                tabBox(
                  id='tabset2', height = "1000px", width = 12,
                  tabPanel('Search', icon = icon("search"),
                           titlePanel(h3("Find Your Ideal Coffee Brand", style = "color: #402218; font-size: 36px; font-family: 'Roboto', sans-serif;")),
                           h3(),
                           h3(),
                           fluidRow(
                             column(3,
                                    div(style = "border: 1px solid gray; background-color: rgba(242, 242, 242, 0.3); padding: 10px; border-radius: 5px; height: 220px",
                                        div(style = "margin-top: 20px; margin-bottom: 20px;", 
                                            h3('Country of Bean Origin:', style = "color: #2a2a2a; font-size: 26px; font-family: 'Roboto', sans-serif;")),
                                        div(style = "font-size: 16px;",
                                            selectInput(inputId = 'origin',
                                                        choices = NULL,
                                                        label = NULL,
                                                        width = '100%',
                                                        selected = NULL))
                                    )
                             ),
                             column(3,
                                    div(style = "border: 1px solid gray; background-color: rgba(242, 242, 242, 0.3); padding: 10px; border-radius: 5px; height: 220px",
                                        div(style = "margin-top: 20px; margin-bottom: 20px;", 
                                            h3('Darkness of the Roast:', style = "color: #2a2a2a; font-size: 26px; font-family: 'Roboto', sans-serif;")),
                                        div(style = "font-size: 16px;",
                                            selectInput(inputId = 'roast',
                                                        choices = c('Any Roast', "Light", "Medium-Light", "Medium", "Medium-Dark", "Dark"),
                                                        label = NULL,
                                                        width = '100%',
                                                        selected = 'Any Roast'))
                                    )
                             ),
                             column(3,
                                    div(style = "border: 1px solid gray; background-color: rgba(242, 242, 242, 0.3); padding: 10px; border-radius: 5px; height: 220px",
                                        div(style = "margin-top: 20px; margin-bottom: 20px;", 
                                            h3('Price per 100g (USD):', style = "color: #2a2a2a; font-size: 26px; font-family: 'Roboto', sans-serif;")),
                                        sliderInput(inputId = "PriceRange", 
                                                    label = NULL,
                                                    min = 0, max = 140, 
                                                    value = c(0,140),
                                                    width = '100%')
                                    )
                             ),
                             column(3,
                                    div(style = "border: 1px solid gray; background-color: rgba(242, 242, 242, 0.3); padding: 10px; border-radius: 5px; height: 220px",
                                        div(style = "margin-top: 20px; margin-bottom: 20px;", 
                                            h3("Rating Range:", style = "color: #2a2a2a; font-size: 26px; font-family: 'Roboto', sans-serif;")),
                                        div(style = "font-size: 16px;",
                                            radioButtons(inputId = "ratingRange",
                                                         label = NULL,
                                                         choices = list("Any Rating" = "Any Rating",
                                                                        "81 - 85" = "81 - 85",
                                                                        "86 - 90" = "86 - 90",
                                                                        "91 - 95" = "91 - 95",
                                                                        "96 - 100" = "96 - 100"),
                                                         selected = "Any Rating",
                                                         width = '100%')
                                        )
                                    )
                             )
                           ),
                           h3(),
                           fluidRow(
                             width = 12,
                             dataTableOutput("userSelection")
                           )
                  ),
                  tabPanel('Label', icon = icon("tag"),
                           titlePanel(h3("Generate Personalized Label", style = "color: #402218; font-size: 36px; font-family: 'Roboto', sans-serif;")),
                           div(style = "margin-top: 20px; margin-bottom: 20px;", 
                               h3('Select Your Favourite Coffee Brand:', style = "color: #402218; font-size: 28px; font-family: 'Roboto', sans-serif;")),
                           pickerInput(inputId = 'label',
                                       label = NULL,
                                       choices = NULL,
                                       options = list(`style` = "btn-light", `live-search` = TRUE),
                                       width = '500px',
                                       selected = NULL),
                           div(style = "margin-top: 20px; margin-bottom: 20px;", 
                               h3('Select Background Color:', style = "color: #402218; font-size: 28px; font-family: 'Roboto', sans-serif;")),
                           selectInput(inputId = 'background',
                                       choices = c('White' = 'white', 'Black' = 'black'),
                                       label = NULL,
                                       width = '200px'),
                           mainPanel(
                             wordcloud2Output("label_plot", height = 500, width=850)
                  )
                )
                )
              )

      )
    ),
    tags$head(tags$style(HTML('
    
.nav-tabs > li:not(.active) > a:hover {
    background-color: #AC7D88 !important; 
    color: #ffffff !important; 
}

.selectize-dropdown-content > .option:hover {
  background-color: #AC7D88; 
  color: #ffffff;
}

    
.radio input[type="radio"] {
    accent-color: #AC7D88; 
}

    .dataTables_filter {display: none}
    
.js-irs-0 .irs-bar {
    border-top-color: #bebebe !important;
    border-bottom-color: #bebebe !important;
    height: 20px !important;
}

.js-irs-0 .irs-bar-edge {
    border-color: #2a2a2a !important;
    height: 10px !important;
}

.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
    background: #bebebe !important;
    height: 20px !important;
}

.js-irs-0 .irs-to, .js-irs-0 .irs-from {
    background: white !important; 
    color: #2a2a2a; 
    border: 1px solid #2a2a2a; 
    font-size: 16px; 
    top: -8px; 
}
.js-irs-0 .irs-line {
    background: #AC7D88;
    height: 20px}
    
.js-irs-0 .irs-handle {
    background: #2a2a2a;
    height: 35px;
    width: 10px}
    
.js-irs-0  .irs-handle:active {
    background: #AC7D88 !important;
}
      
.js-irs-0  .irs-handle:hover {
    background: #AC7D88 !important;
}

.js-irs-0 .irs-grid-pol {
    display: none;  
}
.js-irs-0 .irs-grid-text {
    display: none; 
}

.js-irs-0 .irs-min, .irs-max {
    display: none !important; 
}

.shiny-slider-output .irs-single {
                font-size: 16px; /* Adjust font size */
                top: -30px; /* Adjust vertical position (move up by increasing negative value) */
                background-color: #337ab7; /* Optional: Change background color */
                color: white; /* Optional: Change text color */
                padding: 5px 10px; /* Optional: Adjust padding */
                border-radius: 5px; /* Optional: Round the corners */
            }

  /* logo */
  .skin-blue .main-header .logo {
    background-color: #865439;
  }
  
  /* logo when hovered */
  .skin-blue .main-header .logo:hover {
    background-color: #865439;
  }
  
  /* navbar (rest of the header) */
  .skin-blue .main-header .navbar {
    background-color: #402218;
  }
  
  /* main sidebar */
  .skin-blue .main-sidebar {
    background-color: #62959C;
  }
  
  /* active selected tab in the sidebarmenu */
  .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
    background-color: #FCDEC0;
  }
  
  /* other links in the sidebarmenu */
  .skin-blue .main-sidebar .sidebar .sidebar-menu a{
    background-color: #C68B59;
    color: #000000;
  }
  
  /* other links in the sidebarmenu when hovered */
  .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
    background-color: #AC7D88;
        color: #ffffff !important; 
  }
  /* toggle button when hovered */
  .skin-blue .main-header .navbar .sidebar-toggle:hover{
    background-color: #AC7D88;
        color: #ffffff !important; 
  }

  /* body */
  .content-wrapper, .right-side {
    background-color: #FFFFFF;
  }
')))
    
  )
)

