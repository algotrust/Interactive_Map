
#Packages Needed
library(shiny)
library(shinythemes)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)


d <- readRDS("data/data.rds")
d$FEATURE_NAME <- as.character(d$FEATURE_NAME)
d$FEATURE_CLASS <- as.character(d$FEATURE_CLASS)
d$STATE_ALPHA <- as.character(d$STATE_ALPHA)
d$COUNTY_NAME <- as.character(d$COUNTY_NAME)
d$ELEV_IN_M <- as.numeric(d$ELEV_IN_M)
d$ELEV_IN_FT <- as.numeric(d$ELEV_IN_FT)
d$classID <- as.numeric(d$classID)
d$Group <- as.numeric(d$Group)


cleantable <- d %>%
  select(
    Group = Group,
    FeatureID = FEATURE_ID,
    FeatureName = FEATURE_NAME,
    FeatureClass = FEATURE_CLASS,
    County = COUNTY_NAME,
    State = STATE_ALPHA,
    Elevation_Mass = ELEV_IN_M,
    Percent_Mass = percentM,
    Elevation_Feet = ELEV_IN_FT,
    Percent_Feet = percentF,
    Lat = lat,
    Long = long
  )

featuredata <- d

shinyServer(function(input, output, session) {
  
  # Create the map
  map <- createLeafletMap(session, "map")
  
  
 
  # The features that are within the visible bounds of the map
  featuresInBounds <- reactive({
    #if (input$go == 0) return(NULL)
    if (is.null(input$map_bounds))
      return(featuredata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    featuredata <- featuredata[featuredata$FEATURE_CLASS %in% input$Featclass,]
    
    subset(featuredata,
           lat >= latRng[1] & lat <= latRng[2] &
             long >= lngRng[1] & long <= lngRng[2])
  })
  
  # session$onFlushed is necessary to work around a bug in the Shiny/Leaflet
  # integration; without it, the addCircle commands arrive in the browser
  # before the map is created.
  session$onFlushed(once=TRUE, function() {
    paintObs <- observe({
      colorBy <- input$color
      sizeBy <- input$size
      featuredata <- featuredata[featuredata$FEATURE_CLASS %in% input$Featclass,]
      
      colorData <- d[[colorBy]]

      colors <- brewer.pal(8, "Dark2")[cut(colorData, 8, labels = FALSE)]
      colors <- colors[match(featuredata$classID, d$classID)]
      
      # Clear existing circles before drawing
      map$clearShapes()

        radiusFactor <- 1000
        try(
          map$addCircle(
            featuredata$lat, 
            featuredata$long,
            ((featuredata[[sizeBy]] * 165) / max(d[[sizeBy]])) * 1500,
            featuredata$FEATURE_NAME,
            list(stroke=FALSE, 
                 weight=1.2,
                 fill=TRUE, 
                 fillOpacity=0.5),
            list(color = colors)
          )
       )
   })
      

    
    # TIL this is necessary in order to prevent the observer from
    # attempting to write to the websocket after the session is gone.
    session$onSessionEnded(paintObs$suspend)
  })
  
# Show a popup at the given location
showFeaturePopup <- function(feature, lat, lng) {
  selectedFeature <- d[d$FEATURE_NAME == feature,]
  content <- as.character(tagList(
    tags$h4("Feature Class:", selectedFeature$FEATURE_CLASS),
    tags$strong(HTML(sprintf("%s, %s %s",
                             selectedFeature$FEATURE_NAME, selectedFeature$COUNTY_NAME, selectedFeature$STATE_ALPHA
    ))), 
    tags$br(),
    sprintf("Elevation in Mass: %s", selectedFeature$ELEV_IN_M), tags$br(),
    sprintf("Elevation in Feet: %s", selectedFeature$ELEV_IN_FT)
  ))
  map$showPopup(lat, lng, content, feature)
}
  # When map is clicked, show a popup with city info
  clickObs <- observe({
    map$clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showFeaturePopup(event$id, event$lat, event$lng)
    })
  })
  
  session$onSessionEnded(clickObs$suspend)
  
  
  ## EXAMPLE 1: Data Explorer ###########################################

observe({
  
  cleantable <- cleantable[cleantable$FeatureClass %in% input$Featclass,]
  county <- if (is.null(input$states)) character(0) else {
    filter(cleantable, State %in% input$states) %>%
      `$`('County') %>%
      unique() %>%
      sort()
  }
  stillSelected <- isolate(input$county[input$county %in% county])
  updateSelectInput(session, "county", choices = county,
                    selected = stillSelected)
})

observe({
  
  cleantable <- cleantable[cleantable$FeatureClass %in% input$Featclass,]
  classes <- if (is.null(input$states)) character(0) else {
    cleantable %>%
      filter(State %in% input$states,
             is.null(input$county) | County %in% input$county) %>%
      `$`('FeatureClass') %>%
      unique() %>%
      sort()
  }
  stillSelected <- isolate(input$classes[input$classes %in% classes])
  updateSelectInput(session, "classes", choices = classes,
                    selected = stillSelected)
})


observe({
  
  cleantable <- cleantable[cleantable$FeatureClass %in% input$Featclass,]
  names <- if (is.null(input$states)) character(0) else {
    cleantable %>%
      filter(State %in% input$states,
             is.null(input$county) | County %in% input$county,
             is.null(input$classes) | FeatureClass %in% input$classes) %>%
      `$`('FeatureName') %>%
      unique() %>%
      sort()
  }
  stillSelected <- isolate(input$names[input$names %in% names])
  updateSelectInput(session, "names", choices = names,
                    selected = stillSelected)
})
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map$clearPopups()
      dist <- 0.5
      feature <- input$goto$feature
      lat <- input$goto$lat
      lng <- input$goto$lng
      showFeaturePopup(feature, lat, lng)
      map$fitBounds(lat - dist, lng - dist,
                    lat + dist, lng + dist)
    })
  })
  
  output$table1 <- renderDataTable({
    
    if (nrow(featuresInBounds()) == 0)
      return(NULL)
    
  cleantable <- cleantable[cleantable$FeatureClass %in% input$Featclass,]
  cleantable$Group <- cleantable$Lat <- cleantable$Long <- NULL
  cleantable %>%
      filter(
        is.null(input$states) | State %in% input$states,
        is.null(input$county) | County %in% input$county,
        is.null(input$classes) | FeatureClass %in% input$classes,
        is.null(input$names) | FeatureName %in% input$names
      ) #%>%
    #mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', FeatureName, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
  })#, escape = FALSE)
  
  
})
