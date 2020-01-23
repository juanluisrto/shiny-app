rm(list = ls())
library(countrycode)
library(rio)
library(dplyr)
library(ggplot2)
library(maps)
library(viridis)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


df <- read.csv("europe.csv")
rownames(df) <- df$Country

df$Country["United Kingdom"] <- as.factor("UK")

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

View(world)


europe2 <-
  europe + geom_polygon(data = joinMap,
                        aes(
                          fill = GDP,
                          x = long,
                          y = lat,
                          group = group
                        ),
                        color = "grey70") 
europe2

plot(europe2)

library(dplyr)

all %>%
  group_by(GDP)


png(
  filename = "testPlot.png",
  width = 1300,
  height = 1000,
  pointsize = 70
)

dev.off()