library(maps)
library(mapproj)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(plotly)


quakes = read.csv("data/quakes.csv")
trend = read.csv("data/trend.csv")
trend$Date = as.Date(trend$Date)
head(quakes)
head(trend)

quakes$mag = round(quakes$mag)
