library(shiny)
library(dplyr)

setwd('/Users/Chansoo/Desktop/Spatial_Project/')


#########################


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Stop and Frisk: Doughnut"),
    
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot"),
    plotOutput("distPlot2")
  ),
  
  fluidRow(
    column(12,
           dataTableOutput('table')
    )
  )
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ##### Load and Prep #####
  
  # Stop and Frisk Data
  load('Data/stops2013clean.RData')
  stops2013clean = as.data.frame(stops2013clean)
  
  # CSV containing precincts and corresponding borough
  prec_bor = read.csv('Data/prec_to_boroughs.csv', header = F, stringsAsFactors = F)
  
  # Column names of prec_bor
  colnames(prec_bor) = c('precinct','borough')
  
  # Subset stop and frisk data
  cols_to_keep = c("id",
                   "lon",
                   "lat",
                   "year",
                   "precinct",
                   "found.contraband",
                   "found.pistol",
                   "found.rifle",
                   "found.assault",
                   "found.knife",
                   "found.machinegun",
                   "found.other",
                   "found.gun",
                   "found.weapon")
  df = stops2013clean[,cols_to_keep]
  
  # Create "Hits" column in Stop and Frisk Data
  cols_hit = c("found.contraband",
               "found.pistol",
               "found.rifle",
               "found.assault",
               "found.knife",
               "found.machinegun",
               "found.other",
               "found.gun",
               "found.weapon")
  # hits column
  df$hit = ifelse( apply(df[,cols_hit],1,sum)>0, 1, 0)
  
  # Merge Stop and Frisk Data and Precinct_Borough Data
  df$precinct = as.integer(as.character(df$precinct))
  df = df %>% 
    left_join(prec_bor, by = 'precinct')
  
  
  ##### High Concentration Circle #####
  
  # Radius
  radius = 0.01
  
  load('Data/app_radius_candidates.RData')
  # boroughs = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")
  # result_files = paste('results/',boroughs,'_',radius,'_result.csv',sep='')
  # 
  # candidate_centers = as.data.frame(matrix(0,nrow=5,ncol=4))
  # colnames(candidate_centers) = c('X','lon','lat','n')
  # 
  # 
  # for (i in 1:length(result_files)) {
  #   result_file = read.csv(result_files[i], header=T)
  #   candidate = result_file[1,c('X','lon','lat')]
  #   # Distances between Center and All Stops
  #   dists = sqrt((df$lon - candidate$lon)^ 2 + (df$lat - candidate$lat)^ 2)
  #   # Count Stops in Circle
  #   n = sum(ifelse(dists < radius, 1, 0))
  #   
  #   candidate_centers[i,] = c(candidate, n)
  # }
  # save(candidate_centers, file='Data/app_radius_candidates.RData')
  
  # Winner
  center = candidate_centers %>%
    arrange(-n) %>%
    dplyr::select(X,lon,lat) %>%
    head(1)
  
  
  # Distances between Center and All Stops
  df$dists = sqrt((df$lon - center$lon)^ 2 + (df$lat - center$lat)^ 2)
  
  # Hit Rate of Stops within Radius
  in_circle = ifelse(df$dists < radius, 1, 0)
  n_in_circle = sum(in_circle)
  center_hit_rate = sum(in_circle * df$hit) / n_in_circle
  
  # Doughnut 1
  # Expand Radius until Same Number of Stops Contained in Doughnut Around Circle
  doughnut = df[df$dists > radius,] %>%
                arrange(dists) %>%
                head(n_in_circle)
  doughnut_hit_rate = sum(doughnut$hit) / length(doughnut$hit)
  
  # Doughnut 2
  # Expand Radius until Same Number of Stops Contained in Doughnut Around Doughnut
  big_doughnut = df[df$dists > max(doughnut$dists),] %>%
    arrange(dists) %>%
    head(n_in_circle)
  big_doughnut_hit_rate = sum(big_doughnut$hit) / length(big_doughnut$hit)
  
  
  
  
  ##### Make Circles #####
  make_circles <- function(centers, radius, nPoints = 100){
    circleDF <- data.frame(ID = rep(centers$X, each = nPoints))
    angle <- seq(0,2*pi,length.out = nPoints)
    circleDF$lon = unlist(lapply(centers$lon, function(x) x + radius * cos(angle)))
    circleDF$lat = unlist(lapply(centers$lat, function(x) x + radius * sin(angle)))
    return(circleDF)
  }
  
  # here is the data frame for all circles
  myCircle <- make_circles(center, 0.01)
  myDoughnut <- make_circles(center, max(doughnut$dists))
  myBiggerDoughnut <- make_circles(center, max(big_doughnut$dists))
  
  ##### PLOT #####
  
  register_google(key = "AIzaSyC4ABT6F3JznHUr-uCTAFs4R3lgDYNul_k")
  
  nyc_map = get_map(location = c(lon = -73.94316, lat = 40.6965), 
                    zoom = 11, 
                    maptype = "satellite",
                    source = "google")
  nycMap = ggmap(nyc_map, extent = "panel", legend = "bottomright")
  
  
  nyc_map_zoomed = get_map(location = c(lon = -73.9236, lat = 40.6705), 
                           zoom = 13, 
                           maptype = "satellite",
                           source = "google")
  nycMap_zoomed = ggmap(nyc_map_zoomed, extent = "panel", legend = "bottomright")
  
  #  10% Sample
  df.sampled = df[sample(nrow(df),nrow(df)*0.1),]
  stops = geom_point(aes(x=lon, y = lat), 
                     data = df.sampled, 
                     color = df.sampled$hit+1,
                     alpha = 0.2)
  
  # All Stops         
  all_stops = geom_point(aes(x=lon, y = lat), 
                     data = df, 
                     color = df$hit+1,
                     alpha = 0.2)
  
  
  
  ########################################
  
  output$distPlot <- renderPlot({
    
    # Show Map
    nycMap + stops +
      geom_polygon(data = myCircle, aes(lon, lat), color = "red", alpha = 0) +
      geom_polygon(data = myDoughnut, aes(lon, lat), color = "orange", alpha = 0) +
      geom_polygon(data = myBiggerDoughnut, aes(lon, lat), color = "yellow", alpha = 0) +
      labs(caption="Showing 10% of Stops")
  })
  
  
  ########################################
  
  output$distPlot2 <- renderPlot({
    
    # Show Map
    nycMap_zoomed + all_stops +
      geom_polygon(data = myCircle, aes(lon, lat), color = "red", alpha = 0) +
      geom_polygon(data = myDoughnut, aes(lon, lat), color = "orange", alpha = 0) +
      geom_polygon(data = myBiggerDoughnut, aes(lon, lat), color = "yellow", alpha = 0) +
      labs(caption="Showing 100% of Stops")
  })
  
  ########################################
  res = as.data.frame(t(c(center_hit_rate,doughnut_hit_rate,big_doughnut_hit_rate)))
  colnames(res) = c('center hit rate','doughnut hit rate','bigger doughnut hit rate')
  res = round(res,3)
  output$table <- renderDataTable(res)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

