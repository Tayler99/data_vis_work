library(shiny)
library(ggplot2)
library(dplyr)
library(ggmap)

load("base.Rdata")
dictionary <- list("Host response time"="host_response_time",
                   "Superhost status"="host_is_superhost",
                   "Number of host verification methods"="host_verifications",
                   "Host profile picture"="host_has_profile_pic",
                   "Host identity verification"="host_identity_verified",
                   "Host acceptance rate"="host_acceptance_rate",
                   "Host response rate"="host_response_rate",
                   "Total number of host's listings"="host_total_listings_count",
                   "Type of property"="property_type",
                   "Room type"="room_type",
                   "Number of people able to accommodate"="accommodates",
                   "Number of bathrooms"="bathrooms",
                   "Number of bedrooms"="bedrooms",
                   "Number of beds"="beds",
                   "Type of bed"="bed_type",
                   "Number of amenities"="amenities",
                   "Security deposit"="security_deposit",
                   "Cleaning fee"="cleaning_fee",
                   "Minimum number of nights"="minimum_nights",
                   "30-day availability"="availability_30",
                   "60-day availability"="availability_60",
                   "90-day availability"="availability_90",
                   "Year-long availability"="availability_365",
                   "Instant booking"="instant_bookable",
                   "Cancellation policy"="cancellation_policy",
                   "Neighborhood"="neighbourhood_cleansed",
                   "Exactness of location"="is_location_exact")
dictionary1 <- list("linear model"="lm",
                    "GAM"="gam",
                    "LOWESS"="loess")
bosmap <- get_map(c(left = -71.17179, bottom = 42.23594, right =-71.00010, top = 42.38998),maptype ="terrain")
cols <- c("$~85 per day"="yellow","$85~150 per day"="orange","$150~220 per day"="red","$220~ per day"="firebrick4")

ui = fluidPage(
  theme = shinythemes::shinytheme("darkly"),
  titlePanel("Boston Airbnb Data Exploration"),
  tabsetPanel(
    tabPanel("Overview",
             sidebarLayout(
               sidebarPanel(
                 p("The data we scraped from",a("Kaggle",href="https://www.kaggle.com/airbnb/boston"),"contained 3585 observations on listings and 95 variables, one of which is the daily price of the listing. Here on the right we presented an overview of the price distribution of Boston Airbnb listings in histogram."),
                 
                 sliderInput("p1histbin","Bin width of histogram:",value=50,min=10,max=100,step=5),
                 
                 sliderInput("p1histran","Range of histogram:",value=c(0,1400),min=0,max=1400,step=100)
               ),
               mainPanel(plotOutput("p1hist"))
             ) # end sidebarLayout
    ), # end tabPanel
    tabPanel("Host",
             sidebarLayout(
               sidebarPanel(
                 p("Variables related to the host of an Airbnb listing could have predictive powers on listing price. It makes intuitive sense that when a host is responsible, takes good care of tenants and maintains a great condition of the listing, the price for staying will be raised as well since the tenant gets to enjoy a better service. Below we explored the relationship between multiple host-related variables and price."),
                 selectInput("p2var",
                             label = "Choose the host-related variable:",
                             choices = as.list(c("Host response time","Host response rate","Host acceptance rate","Superhost status","Total number of host's listings","Number of host verification methods","Host profile picture","Host identity verification")))
               ),
               mainPanel(plotOutput("p2gra"))
             ),
             fluidRow(column(6),
                      column(6,
                             selectInput("p2fit",
                                         label = "Choose a smoothing method for continuous variables:",
                                         choices = as.list(c("linear model","GAM","LOWESS")))))
    ), # end tabPanel
    tabPanel("Property",
             sidebarLayout(
               sidebarPanel(
                 p("The condition of a property is a key determinant of the pricing of listings. Features like property size, number of bedrooms/bathrooms, quality of beds, completeness of daily necessities, etc. could all have an impact on the satisfaction and comfort level of tenants, which in turn affects how much customers are willing to pay for this very Airbnb. The relationships between property-related variables and price are graphed on the right."),
                 selectInput("p3var",
                             label = "Choose the property-related variable:",
                             choices = as.list(c("Type of property","Room type","Number of people able to accommodate","Number of bathrooms","Number of bedrooms","Number of beds","Type of bed","Number of amenities")))
               ),
               mainPanel(plotOutput("p3gra"))
             ),
             fluidRow(column(6),
                      column(6,
                             selectInput("p3fit",
                                         label = "Choose a smoothing method for continuous variables:",
                                         choices = as.list(c("linear model","GAM","LOWESS")))))
    ), # end tabPanel
    tabPanel("Policies",
             sidebarLayout(
               sidebarPanel(
                 p("Price of Airbnb listings might fluctuate with the terms of rental policies. Often, a stringent policy made by hosts for tenants implies that the property is in excellent condition and thus requires careful maintenance and tenants abiding by rules; meanwhile, it also means that tenants need to pay more for such great environment. Variables relating to policies are depicted on the right in terms of relationship with price."),
                 selectInput("p4var",
                             label = "Choose the policy-related variable:",
                             choices = as.list(c("Security deposit","Cleaning fee","Minimum number of nights","30-day availability","60-day availability","90-day availability","Year-long availability","Instant booking","Cancellation policy")))
               ),
               mainPanel(plotOutput("p4gra"))
             ),
             fluidRow(column(6),
                      column(6,
                             selectInput("p4fit",
                                         label = "Choose a smoothing method for continuous variables:",
                                         choices = as.list(c("linear model","GAM","LOWESS")))))
    ), # end tabPanel
    tabPanel("Location",
             sidebarLayout(
               sidebarPanel(
                 p("The value of a property, including Airbnb listings, heavily depends on its location. The distance from the listing to the nearest transit, hospital, business center, etc. determines the number and accessibility of accrued functionalities that the listing can bring to tenants and therefore is decisive on the listing's worth. How the location of the listing and the exactness of location relates to price is described on the right."),
                 selectInput("p5var1",
                             label = "Choose the location-related variable:",
                             choices = as.list(c("Neighborhood","Exactness of location")))
               ),
               mainPanel(plotOutput("p5gra1"))
             ),
             sidebarLayout(
               mainPanel(plotOutput("p5map")),
               sidebarPanel(
                 p("All Boston Airbnb listings are mapped onto the map of Boston, with price indicated by color, for better visualization."),
                 br(),
                 selectInput("p5var2",
                             label = "Choose the neighborhood:",
                             choices = as.list(c("(all neighborhoods)",levels(df$neighbourhood_cleansed))))
               )
             )
    ) # end tabPanel
  ) # end tabsetPanel
)

server <- function(input,output){
  p1histbin <- reactive(input$p1histbin)
  p1histran <- reactive(input$p1histran)
  p2var <- reactive(input$p2var)
  p2fit <- reactive(input$p2fit)
  p3var <- reactive(input$p3var)
  p3fit <- reactive(input$p3fit)
  p4var <- reactive(input$p4var)
  p4fit <- reactive(input$p4fit)
  p5var1 <- reactive(input$p5var1)
  p5var2 <- reactive(input$p5var2)
  
  output$p1hist = renderPlot({
    df %>% ggplot(aes(x=price)) +
      geom_histogram(aes(fill = ..count..),binwidth=p1histbin()) +
      scale_x_continuous(name = "Listing Price (dollar per day)", limits = p1histran()) +
      scale_y_continuous(name = "Count") +
      ggtitle("Boston Airbnb Pricing Distribution")
  })
  
  output$p2gra = renderPlot({
    if (p2var() %in% c("Host response time","Superhost status","Host profile picture","Host identity verification")){
      df %>% ggplot(aes(x=(!!as.name(dictionary[p2var()][[1]])),y=price,fill=(!!as.name(dictionary[p2var()][[1]])))) +
        geom_boxplot() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
        xlab(p2var()) +
        scale_y_continuous(trans = "log2") +
        ylab("Daily price of listing (dollar per day)") +
        ggtitle(paste0("Boxplot of listing prices for different ",tolower(p2var())))
    } else {
      df %>% ggplot(aes(x=(!!as.name(dictionary[p2var()][[1]])),y=price)) +
        geom_point() +
        geom_smooth(method = dictionary1[p2fit()][[1]]) +
        xlab(p2var()) +
        ylab("Daily price of listing (dollar per day)") +
        ggtitle(paste0("Scatterplot of listing prices versus ",tolower(p2var())," with a fitted ",p2fit()," curve"))
    }
  })
  
  output$p3gra = renderPlot({
    if (p3var() %in% c("Type of property","Room type","Type of bed")){
      df %>% ggplot(aes(x=(!!as.name(dictionary[p3var()][[1]])),y=price,fill=(!!as.name(dictionary[p3var()][[1]])))) +
        geom_boxplot() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
        xlab(p3var()) +
        scale_y_continuous(trans = "log2") +
        ylab("Daily price of listing (dollar per day)") +
        ggtitle(paste0("Boxplot of listing prices for different ",tolower(p3var())))
    } else {
      df %>% ggplot(aes(x=(!!as.name(dictionary[p3var()][[1]])),y=price)) +
        geom_point() +
        geom_smooth(method = dictionary1[p3fit()][[1]]) +
        xlab(p3var()) +
        ylab("Daily price of listing (dollar per day)") +
        ggtitle(paste0("Scatterplot of listing prices versus ",tolower(p3var())," with a ",p3fit()," curve"))
    }
  })
  
  output$p4gra = renderPlot({
    if (p4var() %in% c("Instant booking","Cancellation policy")){
      df %>% ggplot(aes(x=(!!as.name(dictionary[p4var()][[1]])),y=price,color=(!!as.name(dictionary[p4var()][[1]])))) +
        geom_jitter(width = 0.1, alpha = 0.4, color="grey") +
        geom_boxplot() +
        theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none") +
        xlab(p4var()) +
        scale_y_continuous(trans = "log2") +
        ylab("Daily price of listing (dollar per day)") +
        ggtitle(paste0("Boxplot of listing prices for different ",tolower(p4var())))
    } else {
      df %>% ggplot(aes(x=(!!as.name(dictionary[p4var()][[1]])),y=price)) +
        geom_point() +
        geom_smooth(method = dictionary1[p4fit()][[1]]) +
        xlab(p4var()) +
        ylab("Daily price of listing (dollar per day)") +
        ggtitle(paste0("Scatterplot of listing prices versus ",tolower(p4var())," with a ",p4fit()," curve"))
    }
  })
  
  output$p5gra1 = renderPlot({
    df %>% ggplot(aes(x=(!!as.name(dictionary[p5var1()][[1]])),y=price,color=(!!as.name(dictionary[p5var1()][[1]])))) +
      geom_jitter(width = 0.1, alpha = 0.4, color="grey") +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
      xlab(p5var1()) +
      scale_y_continuous(trans = "log2") +
      ylab("Daily price of listing (dollar per day)") +
      ggtitle(paste0("Boxplot of listing prices for different ",tolower(p5var1())))
  })
  
  output$p5map = renderPlot({
    if (p5var2()=="(all neighborhoods)"){
      ggmap(bosmap) +
        geom_point(data=df,aes(x = longitude, y = latitude, size = I(0.5), color=`Price Level`),alpha = 0.7, size=0.08) +
        scale_colour_manual(values=cols) +
        xlab("Longitude") +
        ylab("Latitude") +
        ggtitle("Heatmap of listing prices across all Boston")
    } else {
      dff <- df %>% filter(neighbourhood_cleansed==p5var2())
      ggmap(bosmap) +
        geom_point(data=dff,aes(x = longitude, y = latitude, size = I(0.5), color=`Price Level`),alpha = 0.7, size=0.08) +
        scale_colour_manual(values=cols) +
        xlab("Longitude") +
        ylab("Latitude") +
        ggtitle(paste0("Heatmap of listing prices in ",p5var2()))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)