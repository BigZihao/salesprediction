## Forecast
## Forecast using different machine learning method on Scotts dataset
## zihao.zhang@analyticpartners.com
## have fun



setwd("C:/Users/zihao.zhang/Desktop")

target_name = "Lawn.Fertilizer.Sales.Units"
SampleSize = 1


#########################################
#######    Data Processing     #########
#########################################

source("scotts_dataprocessing.R")


#########################################
#######      Setup training     #########
#########################################

names(scotts)

train = scotts[scotts$Week.Ending.Sunday<="2016-06-25",-c(17:33)]
test = scotts[scotts$Week.Ending.Sunday>="2016-06-25"&scotts$Week.Ending.Sunday<"2017-05-01",-c(17:33)]

dim(train)
Sample =  round(dim(train)[1]*SampleSize)

summary(scotts$Week.Ending.Sunday)

names(train)


library(xgboost)
train_matrix =  as.matrix(train[1:Sample, -which(names(train) %in% c("target"))])
test_matrix =  as.matrix(test[, -which(names(test) %in% c("target"))])
train_label = as.numeric(train$target[1:Sample])
dim(train_matrix)
mode(train_matrix) = "numeric"
mode(test_matrix) = "numeric"
length(train_label)



#########################################
#######      Random Forest      #########
#########################################

library(randomForest)
rf = randomForest(target ~ ., data=train[1:Sample,-1], maxnodes=20, ntree=100)
summary(rf)
varImpPlot(rf,type=2)

rf_predict = predict(rf, test)
length(rf_predict)
sqrt(mean((rf_predict-test$target)^2))
mean(abs((rf_predict-test$target)))

cbind(test,rf_predict) %>% group_by(Date = Week.Ending.Sunday) %>% 
  summarise(predicted = sum(rf_predict),actual= sum(target)) %>%
  melt(id="Date") %>% ggplot(aes(Date,value,colour=variable)) + geom_line()



library(reprtree)
reprtree:::plot.getTree(rf,depth = 4)

library(party)
plot(rf, type="simple")


save(rf,file = "scotts_rf.RData")
load("scotts_rf.RData")
print(rf)


names(train)
partialPlot(rf, train,Avg_AppTempF_Avg, "versicolor")


#########################################
#######         Regression      #########
#########################################

dim(train)
names(train)
lm1 = lm(target ~ ., data=train[1:Sample,-1])
summary(lm1)

lm_predict = predict(lm1,test)
sqrt(mean((lm_predict-test$target)^2))
mean(abs((lm_predict-test$target)))


cbind(test,lm_predict) %>% group_by(Date = Week.Ending.Sunday) %>% 
  summarise(predicted = sum(lm_predict),actual= sum(target)) %>%
  melt(id="Date") %>% ggplot(aes(Date,value,colour=variable)) + geom_line()




names(train)


#########################################
#######          XGBoost        #########
#########################################


param <- list("objective" = "reg:linear",    # multiclass classification 
              "eval_metric" = "rmse",    # evaluation metric 
              "nthread" = 8,   # number of threads to be used 
              "max_depth" = 20,    # maximum depth of tree 
              "eta" = 0.1,    # step size shrinkage 
              "gamma" = 0,    # minimum loss reduction 
              "subsample" =0.8,    # part of data instances to grow tree 
              "colsample_bytree" = 0.8  # subsample ratio of columns when constructing each tree 
)


# set random seed, for reproducibility 
set.seed(1234)
# k-fold cross validation, with timing
nround.cv = 200
system.time( bst.cv <- xgb.cv(param=param, data=train_matrix, label=train_label, 
                              nfold=10, nrounds=nround.cv, prediction=TRUE, verbose=FALSE) )


# index of minimum merror
min.rmse.idx = which.min(bst.cv$evaluation_log[, test_rmse_mean]) 
min.rmse.idx 
bst.cv$evaluation_log[min.rmse.idx,]


xgb <- xgboost(data =train_matrix , label =train_label , 
               max.depth = 20, eta = 0.1, nthread = 8, 
               nround = round(min.rmse.idx*1.2) , objective = "reg:linear",
               subsample = 0.8,colsample_bytree = 0.8)

# get the trained model
model = xgb.dump(xgb, with.stats=TRUE)
# get the feature real names
names = dimnames(train_matrix)[[2]]
# compute feature importance matrix
importance_matrix = xgb.importance(names, model=xgb)[1:20]
# plot
gp = xgb.plot.importance(importance_matrix)
print(gp) 



xgb = xgb.load('scotts_best_xgboost.model')


xgb_predict = predict(xgb,test_matrix)

sqrt(mean((xgb_predict-test$target)^2))
 
cbind(test,xgb_predict) %>% group_by(Date = Week.Ending.Sunday) %>% 
  summarise(predicted = sum(xgb_predict),actual= sum(target)) %>%
  melt(id="Date") %>% ggplot(aes(Date,value,colour=variable)) + geom_line()




# 20: 2011.88





dim(scotts_dimension[scotts_dimension$Week.Ending.Sunday>"2016-06-25",]) 
length(xgb_predict)

  



cbind(test,scotts_dimension[scotts_dimension$Week.Ending.Sunday>"2016-06-25",c("DMA","Retailer")],xgb_predict) %>% group_by(Date = Week.Ending.Sunday,Retailer) %>% 
  summarise(predicted = sum(xgb_predict),actual= sum(target)) %>%
  melt(id=c("Date","Retailer")) %>% ggplot(aes(Date,value,colour=variable)) + geom_line() +
   facet_grid(. ~ Retailer, scales = "free")


cbind(test,,xgb_predict)


p <- xgb.plot.multi.trees(model = xgb, feature_names = colnames(train_matrix),
                          features_keep = 3)
print(p)


gr <- xgb.plot.tree(model=xgb, n_first_tree = 1)
library(DiagrammeR)
export_graph(gr, 'tree.pdf', width=3000, height=4000)

xgb.plot.tree(feature_names = names((Icecream[,-1])), model = bst)

library(DiagrammeR) 

xgb.save(xgb, "scotts_best_xgboost.model")






#########################################
#######      Visualization      #########
#########################################



## just checking ...




library(sp)
library(maps)
library(maptools)
library(ggmap)

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
  
}


cities <- as.character(unique(scotts_dimension[,'DMA']))


cities  <- gsub("-", " ", cities , fixed = TRUE)
cities  <- gsub(",", " ", cities , fixed = TRUE)
cities  <- gsub("\\(.*?\\)", "", cities, perl=TRUE)
cities  <- gsub("&", " ", cities , fixed = TRUE)


# Test the function using points in Wisconsin and Oregon.
geo_code=geocode(cities)
testPoints <- data.frame(DMA = as.character(unique(scotts_dimension[,'DMA'])),x = geo_code[,1], y = geo_code[,2])
length(testPoints[is.na(testPoints$x),2])



states = data.frame(states =latlong2state(testPoints[!is.na(testPoints$x),2:3]),x = geo_code[!is.na(testPoints$x),1], y = geo_code[!is.na(testPoints$x),2])

states = merge(testPoints,states,id=c(x,y),all.x = TRUE)
length(cities)
dim(states)
dim(testPoints)

names(states)
names(scotts_dimension)
dim(scotts_dimension)





summary(xgb_predict-test$target)
summary(test$target)
length(test$target)


Prediction_visualize = data.frame(cbind(scotts_dimension[scotts_dimension$Week.Ending.Sunday>"2016-06-25",c("DMA","Retailer","Week.Ending.Sunday")] ,xgb_predict,target = test[,"target"]))


Prediction_visualize = merge(Prediction_visualize,states,by="DMA")

Prediction_visualize %>%
  group_by(DMA) %>% summarise(sqrt(mean((xgb_predict - target)^2)))


Prediction_visualize %>%
  group_by(Retailer) %>% summarise(sqrt(mean((xgb_predict - target)^2)))



Prediction_visualize %>%
  group_by(Retailer) %>% summarise(sqrt(mean((xgb_predict - target)^2)))


Prediction_visualize %>%
  group_by(states) %>% summarise(sqrt(mean((xgb_predict - target)^2)))






#########################################
#######         shiny app       #########
#########################################





names(Prediction_visualize)

quakes = data.frame(Prediction_visualize %>% group_by(x,y,DMA) %>% 
                      summarise(prediction = sum(xgb_predict)))

quakes = quakes[!is.na(quakes$x),]

names(quakes) = c("long","lat","DMA","mag") 


library(plotly)
ggplotly(cbind(test,xgb_predict) %>% group_by(Date = Week.Ending.Sunday) %>% 
  summarise(predicted = sum(xgb_predict),actual= sum(target)) %>%
  melt(id="Date") %>% ggplot(aes(Date,value,colour=variable)) + geom_line())


trend = data.frame(Date=test$Week.Ending.Sunday,
                   Retalier = scotts_dimension[scotts_dimension$Week.Ending.Sunday>"2016-06-25","Retailer"],
                   xgb_predict)


quakes = quakes[quakes$long<(-50)&quakes$lat>30,]
dim(quakes)


summary(quakes)

write.csv(quakes,"S:/Tools/Excel Planner/Modeling/Scotts Prediction/data/quakes.csv", row.names=FALSE)
write.csv(trend,"S:/Tools/Excel Planner/Modeling/Scotts Prediction/data/trend.csv", row.names=FALSE)





library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  fluidPage( leafletOutput("map", width = "80%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Range", min(quakes$mag), max(quakes$mag),
                            value = range(quakes$mag), step = 0.1
                )
  ),
  
  plotlyOutput('plottrend')
  
  )
)

server <- function(input, output, session) {
  
  output$plottrend <-  renderPlotly({
    
    ggplotly(trend %>% group_by(Date = Date) %>% 
               summarise(predicted = sum(xgb_predict)) %>%
                ggplot(aes(Date,predicted)) + geom_line())
  })
    
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric("BrBG", quakes$mag)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(quakes) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~mag/5, weight = 1, color = "#777777",
                 fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = quakes)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()

      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~mag
      )
    
  })
}

shinyApp(ui, server)

