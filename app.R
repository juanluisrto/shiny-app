library(shiny)
library(ggplot2)
library(maps)





ui <- fluidPage(
  
  # App title ----
  titlePanel("Visualization Practical Work"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      sidebarPanel(
        selectInput('var', 'Select variable', names(df))
      )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "map")
      
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  df <- read.csv("europe.csv")
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
  europe <- worldmap + coord_fixed(xlim = c(-20, 42.5),
                                   ylim = c(36, 70.1),
                                   ratio = 1.5)
  
  joinMap <- full_join(df, world, by = c("Country" = "region"))
  
  
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
              scale_fill_viridis(
              option = "plasma",
              direction = -1,
              name = "",
              na.value = "grey80",
              guide = guide_colorbar(
                barheight = unit(140, units = "mm"),
                barwidth = unit(6, units = "mm")
              )
            )
    plot(europe2)
  })
  
}

shinyApp(ui = ui, server = server)