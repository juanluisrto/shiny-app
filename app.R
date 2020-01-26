# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("maps")
# install.packages("dplyr")
# install.packages("RColorBrewer")
# install.packages("ggpubr")
# install.packages("shinyWidgets")
# install.packages("fmsb")

library(shiny)
library(ggplot2)
library(maps)
library(dplyr)
library(RColorBrewer)
library(ggpubr)
library(shinyWidgets)
library(fmsb)

# Read dataset
df <- read.csv("europe.csv")

# Normalize the data
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

df_scaled <- df %>% 
  mutate(Area = min_max_norm(Area)) %>% 
  mutate(GDP = min_max_norm(GDP)) %>% 
  mutate(Inflation = min_max_norm(Inflation)) %>% 
  mutate(Life.expect = min_max_norm(Life.expect)) %>% 
  mutate(Military = min_max_norm(Military)) %>% 
  mutate(Pop.growth = min_max_norm(Pop.growth)) %>% 
  mutate(Unemployment = min_max_norm(Unemployment)) 

# Create World Map
world <- map_data("world")
worldmap <- ggplot() + theme(
  panel.background = element_rect(fill = "white",
                                  color = NA),
  panel.grid = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank()
)

# Create Europe boundaries
europe <- worldmap + coord_fixed(xlim = c(-20, 42.5),
                                 ylim = c(36, 70.1),
                                 ratio = 1.5)

# Joining our data with geopoints of the countries
joinMap <- full_join(df, world, by = c("Country" = "region"))

ui <- fluidPage(
  # App title ----
  titlePanel(
    h1("Visualization Practical Work", align = "center")
  ),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput('var',
                  'Select variable',
                  c("Gross Domestic Product"="GDP",
                    "Inflation",
                    "Life Expectancy" = "Life.expect", 
                    "Military",
                    "Population Growth" = "Pop.growth", 
                    "Unemployment"))
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Map
      plotOutput(outputId = "map"),
    )
  ),
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      radioButtons("radioButton_graph","Type of graph",
                   c("Histogram","Correlation between two variables")),
      selectInput('corr_first', 'First Variable', 
                  c("Gross Domestic Product"="GDP",
                    "Inflation",
                    "Life Expectancy" = "Life.expect", 
                    "Military",
                    "Population Growth" = "Pop.growth", 
                    "Unemployment")),
      conditionalPanel(
        condition = "input.radioButton_graph != 'Histogram'",        
        selectInput('corr_second', 'Second Variable', 
                    c("Gross Domestic Product"="GDP",
                      "Inflation",
                      "Life Expectancy" = "Life.expect", 
                      "Military",
                      "Population Growth" = "Pop.growth", 
                      "Unemployment"), 
                    selected = "Inflation")),
    ),
    # Main panel for displaying outputs
    mainPanel(
      # Output: Correlation Matrix
      plotOutput(outputId = "correlation_matrix")
    )
  ),
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      radioButtons("radioButton_country","",
                   c("Country exploration","Country comparison")),
      selectInput('country_1',
                  'Select Country',
                  df$Country),
      conditionalPanel(
        condition = "input.radioButton_country != 'Country exploration'",        
        selectInput('country_2', 'Select Country', 
                    df$Country, 
                    selected = "Belgium")),
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Country
      plotOutput(outputId = "country"),
    )
  ),
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Map output
  output$map <- renderPlot({
    symbol <- sym(input$var)
    europe2 <-
      europe + geom_polygon(data = joinMap,
                            aes(
                              fill = !! symbol,
                              x = long,
                              y = lat,
                              group = group
                            ),
                            color = "grey70") + 
      scale_colour_gradient()
    plot(europe2)
  })
  
  # Correlation-Histogram output
  output$correlation_matrix <- renderPlot({
    corr_first <- sym(input$corr_first)
    corr_second <- sym(input$corr_second)
    
    # If only a variable is selected, the histogram is shown
    if(input$radioButton_graph=="Histogram")
    {
      ggplot(df %>% select(!! corr_first), aes(x=!! corr_first)) + 
        geom_histogram(color="black", fill="#2e608a") +
        theme(
          # Deleting the background
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.box.background = element_rect(fill = "transparent", colour = NA),
          # Hide panel borders and add grid lines
          panel.border = element_blank(),
          panel.grid.major = element_line(colour = "grey"),
          panel.grid.minor = element_line(colour = "grey"),
          # Change axis line
          axis.line = element_line(colour = "black"))
    }
    # If two different variables are selected, the correlation scatterplot is shown
    else if (input$radioButton_graph == "Correlation between two variables"){
      ggscatter(df_scaled %>% select(c(!! corr_first, !! corr_second)),
                x = input$corr_first, y =  input$corr_second,
                # Add regressin line
                add = "reg.line",  
                # Customize reg. line
                add.params = list(color = "blue", fill = "white"), 
                # Add confidence interval
                conf.int = TRUE 
      ) + stat_cor(method = "pearson", 
                   label.x = 0, 
                   label.y = 0)
    }
  })
  # Country output
  output$country <- renderPlot({
    
    if(input$radioButton_country=="Country exploration")
    {
      df_scaled <- df_scaled %>%
        filter(Country==input$country_1) %>% 
        select(-Country)
      
      df_scaled <- rbind(rep(1,7) , rep(0,7) , df_scaled)
      
      radarchart(df_scaled ,
                 #Axist Type
                 axistype=1 , 
                 #custom polygon
                 pcol=rgb(0.19,0.39,0.59,0.9) , pfcol=rgb(0.2,0.55,0.94,0.4) , plwd=4 ,
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,25), cglwd=0.8,
                 #custom labels
                 vlcex=0.8 
      ) 
    }
    else if (input$radioButton_country=="Country comparison")
    {
      df_scaled <- df_scaled %>%
        filter(Country==input$country_1 | Country==input$country_2)
      
      rownames(df_scaled) <- df_scaled[,1]
      
      df_scaled <- df_scaled %>% 
        select(-Country)
      
      df_scaled <- rbind(rep(1,7) , rep(0,7) , df_scaled)
      
      colors_border=c(rgb(0.19,0.39,0.59,0.9), rgb(0.79,0.22,0.08,0.9))
      colors_in=c( rgb(0.2,0.55,0.94,0.4), rgb(0.98,0.27,0.1,0.4))
      
      radarchart( df_scaled  , axistype=1 , 
                  #custom polygon
                  pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                  #custom the grid
                  cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,25), cglwd=0.8,
                  #custom labels
                  vlcex=0.8 
      )
      
      # Add a legend
      legend(x=1.5, y=1, legend = rownames(df_scaled[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
    }
    
  })
}

shinyApp(ui = ui, server = server)
