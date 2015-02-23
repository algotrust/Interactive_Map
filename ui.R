
###Packages Needed
library(shiny)
library(shinythemes)
library(leaflet)

#### This app closely follows and resembles the SuperZip app on RStudio page,
#### https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example.
#### This started as a personal project/proof of concept to apply the SuperZip app to a different data source.
#### Throughout the building process, definitely ran into some issues in creating the map due to lack of understanding of the bindings.
#### After reading up on leaflet, shiny bindings, and trial/error re-creating this app to resemble SuperZip app,
#### I am now comfortable applying leaflet and other javascript visualizations to make more customizable applications based on my interest.
#### Special thanks to RStudio team and jCheng (https://github.com/jcheng5/leaflet-shiny , https://github.com/jcheng5/superzip)


d <- readRDS("data/data.rds")
d$FEATURE_NAME <- as.character(d$FEATURE_NAME)
d$FEATURE_CLASS <- as.character(d$FEATURE_CLASS)
d$STATE_ALPHA <- as.character(d$STATE_ALPHA)
d$COUNTY_NAME <- as.character(d$COUNTY_NAME)
d$ELEV_IN_M <- as.numeric(d$ELEV_IN_M)
d$ELEV_IN_FT <- as.numeric(d$ELEV_IN_FT)
d$classID <- as.numeric(d$classID)
d$Group <- as.numeric(d$Group)

vars =c(
  "Features by Class Type" = "classID",
  "Elevation in Mass (Percent)" = "percentM",
  "Elevation in Feet (Percent)" = "percentF"

)


featuredata <- d

shinyUI(navbarPage("Interactive Map", theme = shinytheme("spacelab"),
                   
                  
                   tabPanel("Features within the US",
                            div(class="outer",
                                
                                tags$head(
                                  # Include our custom CSS - based off example (SuperZip). Definitely should be modified if creating
                                  #custom app. Using for reference, learning, understaning purposes.
                                  includeCSS("styles.css"),
                                  includeScript("gomap.js")
                                ),
                                #Maptiles are created using MapBox. I have an account with the main open source maps; openstreetmaps, mapbox, cartoDB,
                                #etc. When I was learning about leaflet, I signed up for anything. Now, I choose to stick with Mapbox. 
                                leafletMap("map", width="100%", height="100%",
                                           initialTileLayer = "//{s}.tiles.mapbox.com/v3/familyp.h93enobn/{z}/{x}/{y}.png",
                                           initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
                                           options=list(
                                             center = c(37.45, -93.85),
                                             zoom = 5,
                                             maxBounds = list(list(15.961329,-129.92981), list(52.908902,-56.80481)) # Show US only
                                           )
                                ),
                                
                                
                                # Shiny versions prior to 0.11 should use class="modal" instead.
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = 400,
                                              
                                              
                                              h2("National Features"),
                                              
                                              selectizeInput("Featclass", 'Select Feature Class Type:', 
                                                             choices = sort(unique(featuredata$FEATURE_CLASS)), multiple = TRUE,
                                                             selected = unique(featuredata$FEATURE_CLASS)[12:15]
                                              ),
                                              
                                              selectInput("color", "Color", vars),
                                              selectInput("size", "Size", vars, selected = "percentM")                                       
#                                               plotOutput("hist", height = 200),
#                                               plotOutput("scatter", height = 250)
                                )
                            )
                        ),
                   
                   tabPanel("Data Table",
                            helpText("Note: On the far right of table,",
                                     "there is an ACTION button that is currently",
                                     "Disabled."),
                            
                            fluidRow(
                              column(3,
                                     selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
                              ),
                              column(3,
                                     conditionalPanel("input.states",
                                                      selectInput("county", "County", c("All Counties"=""), multiple=TRUE)
                                     )
                              ),
                              column(3,
                                     conditionalPanel("input.states",
                                                      selectInput("classes", "Feature Class", c("All Classes"=""), multiple=TRUE)
                                     )
                              ),
                               column(3,
                                     conditionalPanel("input.states",
                                                      selectInput("names", "Feature Name", c("All Names"=""), multiple=TRUE)
                                     )
                              )
                            ),
                            
                            hr(),
                            
                            dataTableOutput("table1")
                   ),

                  conditionalPanel("false", icon("crosshair"))
                   
))         

