# Load the colourpicker package
#install.packages("colourpicker")
#install.packages("rgdal")
#install.packages("WDI")
#install.packages("tigris")
#install.packages("raster")
#install.packages("rjson")
#install.packages("DT")


library(colourpicker)
library(shiny)
library(ggplot2)
library(dplyr)
library(DT) # library to make interactive table
library(plotly)
library(shinythemes)
library(RColorBrewer)
library(raster)
library(leaflet)
library(sp)
library(maptools)
library(rgdal)
library(rjson)
library(sf)

############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
#get gapminderdataset
final_df <- read.csv("C:/Users/blake/OneDrive/GITHUBJUPITERPROJECT/Worldwide Covid/Shiny/finaldataframe.csv") # load updated dataframe from markdown rsconnect automatic update
final_df <-final_df[-c(1)] # remove irrelevant column
colnames(final_df)[1] <- "ISO3" # change name of country  to iso3 to match shape file for merging
rm
# maybe change date from factor to non
final_df$date <- as.Date(final_df$date) # make date column actually date
final_df$date2 <- lubridate::yday(final_df$date) # make a day of year column for different kind of slider
final_df$date2 <- as.numeric(final_df$date2) # make it numeric
dft <- final_df
dff <- dft 
############################################################################################################################################
# read data from world shapefile and from dff 
covid_shape <- shapefile("C:/Users/blake/OneDrive/GITHUBJUPITERPROJECT/Worldwide Covid/Shiny/TM_WORLD_BORDERS_SIMPL-0.3.shp")

# perhaps save as shapefile again
#shapefile(p, "C:/Users/blake/OneDrive/GITHUBJUPITERPROJECT/Worldwide Covid/Shiny/covidshape.csv",overwrite = TRUE)
#covid_shape <-readOGR(dsn=path.expand("C:/Users/blake/OneDrive/GITHUBJUPITERPROJECT/Worldwide Covid/Shiny/covidshape.shp"), layer="covidshape")

############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
max <- max(final_df$date2) # find max date of new dataframe
min <- min(final_df$date2) # find min date

ui <- fluidPage(navbarPage("Blake's Covid App",
                           tabPanel("Covid Data Download",
                                    sidebarLayout(
                                      sidebarPanel(sliderInput("date",
                                                               # step = "Date",
                                                               label = h5("Select date"),
                                                               min = as.Date("2020-01-22","%Y-%m-%d"),
                                                               max = as.Date("2020-07-16","%Y-%m-%d"),
                                                               value = as.Date("2020-04-19"),
                                                               timeFormat = "%y %b"),  
                                                   selectInput('country', 'Select country',choices = c("All", levels(final_df$ISO3))),
                                                   downloadButton("download_data")  ),
                                      mainPanel(
                                        tabsetPanel(
                                          # Plot output
                                          tabPanel("Table",DT::DTOutput('table'))
                                        )            
                                      )
                                    )
                           ),
                           tabPanel("Covid Data Download",
                                    sidebarLayout(
                                      sidebarPanel(sliderInput("Dayofyear", "Day of Year",
                                                               min =as.numeric(min), max = as.numeric(max),
                                                               value =50)),  
                                      mainPanel(
                                        tabsetPanel(
                                          # Plot output
                                          leafletOutput("plot")
                                        )
                                      )
                                    )
                           )
)
)

# Make the server
server <- function(input,output,session){ 
  
  #make the table with the full gapminder dataset
  filtered_data <- reactive({
    data <- final_df
    data <- subset(data, date == input$date)
    if (input$country != "All") {
      data <- subset(data,ISO3 == input$country
      )
    }
    data
  })
  
  output$table <- DT::renderDataTable({
    data <- filtered_data()
    data
  })
  
  output$download_data <- downloadHandler(
    filename = "current_worldwide_covid.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE,)
    }
  )
  
 
    
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "Country: ", covid_shape@data$NAME,"<br/>", 
      "Covid Count: ", covid_shape@data$deaths, "<br/>",
      " Google Trend: ", covid_shape@data$Coronavirus, "<br/>",
      "Temperature: ", covid_shape@data$tmpf, "<br/>",
      "Population: ", covid_shape@data$POP2005, "<br/>",
      sep="") %>%
      lapply(htmltools::HTML)
    
    mybins <- c(0, 100, 500, 1000 ,10000, 100000,1000000,5000000,Inf)
    mypalette <- colorBin( palette="YlOrBr", domain=covid_shape@data$confirmed, na.color="transparent", bins=mybins)
    
  
  output$plot <- renderLeaflet({
    dft <- final_df
    dff <- dft 
    #datafiltered <- dff[which(dff$date2 == input$Dayofyear), ]
    # this returns positions of map@data$NAME in datafiltered$county
    #ordercounties <- match(covid_shape@data$ISO3, datafiltered$ISO3)
    #covid_shape@data <- datafiltered[ordercounties, ]
    
    leaflet(covid_shape) %>% 
      addTiles()  %>% 
      setView( lat=10, lng=0 , zoom=2) %>%
      addPolygons( 
        fillColor = ~mypalette(dff$confirmed), 
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="white", 
        weight=0.3,
        label = mytext,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addLegend( pal=mypalette, values=~dff$confirmed, opacity=0.9, title = "Confirmed (M)", position = "bottomleft" )
  })
  
  
}


shinyApp(ui=ui, server=server)