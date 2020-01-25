# install.packages("shinyWidgets")

library(shiny)
library(ggplot2)
library(maps)
library(dplyr)
library(RColorBrewer)
library(ggpubr)
library(shinyWidgets)

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
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
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
  
}

shinyApp(ui = ui, server = server)
