library(shiny)
library(dplyr)

setwd('/Users/Chansoo/Desktop/Spatial_Project/')


#########################


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Stop and Frisk: Radius"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("circles",
                     "Number of Circles:",
                     min = 1,
                     max = 1000,
                     width = '200%',
                     value = 2)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         plotOutput("newPlot")
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
    dists = sqrt((df$lon - center$lon)^ 2 + (df$lat - center$lat)^ 2)
    
    # Hit Rate of Stops within Radius
    in_circle = ifelse(dists < radius, 1, 0)
    center_hit_rate = sum(in_circle * df$hit) / sum(in_circle)
    
    ##### Random Circles #####
    
    # Random sample of centers
    n_samples = 1000
    samples_idx = sample(nrow(df), n_samples)
    centers = df[samples_idx,c("id","lon","lat","hit")]
    
    
    
    ### Comment out code below bc it takes long time to run.
    ### Instead, just saved output from the for loop and load it.
    load('Data/app_radius_centers.RData')
    # Get Hit Rates for Circles of Fixed Radius around this random sample of centers
    # hit_rates_radius = rep(NA, n_samples)
    # for(i in 1:n_samples){
    #   # Longitude and Latitude of Center
    #   center_lon = centers[i,c("lon")]
    #   center_lat = centers[i,c("lat")]
    # 
    #   # Distances between Center and All Stops
    #   dists = sqrt((df$lon - center_lon)^ 2 + (df$lat - center_lat)^ 2)
    # 
    #   # Hit Rate of Stops within Radius
    #   in_circle = ifelse(dists < radius, 1, 0)
    # 
    #   hit_rates_radius[i] = sum(in_circle * df$hit) / sum(in_circle)
    # }
    # save(hit_rates_radius, file = 'Data/app_radius_centers.RData')
    
    
    ##### Make Circles #####
    
    make_circles <- function(centers, radius, nPoints = 100){
      circleDF <- data.frame(ID = rep(centers$X, each = nPoints))
      angle <- seq(0,2*pi,length.out = nPoints)
      circleDF$lon = unlist(lapply(centers$lon, function(x) x + radius * cos(angle)))
      circleDF$lat = unlist(lapply(centers$lat, function(x) x + radius * sin(angle)))
      return(circleDF)
    }
    
    # here is the data frame for all circles
    colnames(centers) = c('X','lon','lat','hit')
    myCircle <- make_circles(center, 0.01)
    myCircles <- make_circles(centers[1:100,], 0.01)
    
    ##### PLOT #####
    
    register_google(key = "AIzaSyC4ABT6F3JznHUr-uCTAFs4R3lgDYNul_k")
    nyc_map = get_map(location = c(lon = -73.94316, lat = 40.6965), 
                      zoom = 11, 
                      maptype = "satellite",
                      source = "google")
    nycMap = ggmap(nyc_map, extent = "panel", legend = "bottomright")
    
    #  10% Sample
    df.sampled = df[sample(nrow(df),nrow(df)*.1),]
    stops = geom_point(aes(x=lon, y = lat), 
                       data = df.sampled, 
                       color = df.sampled$hit+1,
                       alpha = 0.2)
    
    # IDs of Random Circles
    circle.IDs = unique(myCircles$ID)
    
    
    
    ########################################
    
    # Load Hit Rate Distribution Computed by Drawing Samples over All of NYC
    # Without any radius or population restrictions
    load('Data/hit_rate_dist_over_all_NYC.RData')
    
    ########################################
    
    output$distPlot <- renderPlot({
      
       # Show Map
       nycMap + stops +
         geom_polygon(data = myCircles[myCircles$ID %in% circle.IDs[1:input$circles],], aes(lon, lat, group=ID), color = "blue", alpha = 0) +
         geom_polygon(data = myCircle, aes(lon, lat), color = "red", alpha = 0) +
         labs(caption="Showing 10% of Stops")
    })
   
    ########################################
    
    output$newPlot <- renderPlot({
     
      plot(density(hit_rates_radius[1:input$circles]), main = 'Sampling Distribution of Hit Rates', col='blue', ylim=c(0,240))
      abline(v = mean(hit_rates_radius[1:input$circles]), col = 'blue', lty=2)
      lines(density(hit_rates), col = 'green')
      abline(v = mean(hit_rates), col = 'green', lty=2)
      abline(v = center_hit_rate, col = 'red', lty=2)
   
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

