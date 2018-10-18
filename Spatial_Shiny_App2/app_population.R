library(shiny)
library(dplyr)
library(ggmap)

setwd('/Users/Chansoo/Desktop/Spatial_Project/')

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Stop and Frisk: # of Stops"),
  
  actionButton("go", "Go"),
  numericInput("multiplier", "Skip (# of Go's at Once. Max = 10):", 1, min = 1, max = 100),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("newPlot")
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  sample.no <- reactiveVal(2)
  observeEvent(input$go, {
    newSampleNo = sample.no() + 1 * input$multiplier
    sample.no(newSampleNo)
  })
  
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
  
  load('Data/app_population_candidates.RData')
  # boroughs = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")
  # result_files = paste('results/',boroughs,'_',radius,'_result.csv',sep='')
  # 
  # candidate_centers = as.data.frame(matrix(0,nrow=5,ncol=4))
  # colnames(candidate_centers) = c('X','lon','lat','n')
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
  # save(candidate_centers, file='Data/app_population_candidates.RData')

  
  # Winner
  center = candidate_centers %>%
    arrange(-n) %>%
    dplyr::select(X,lon,lat) %>%
    head(1)
  
  # Distances between Center and All Stops
  dists = sqrt((df$lon - center$lon)^ 2 + (df$lat - center$lat)^ 2)
  center_neighbors = df[which(dists < radius),]
  
  # Hit Rate of Stops within Radius
  in_circle = ifelse(dists < radius, 1, 0)
  center_hit_rate = sum(in_circle * df$hit) / sum(in_circle)
  n_in_circle = sum(in_circle)
  
  ##### Random Areas #####
  
  # Random sample of centers
  n_samples = 1000
  samples_idx = sample(nrow(df), n_samples)
  centers = df[samples_idx,c("id","lon","lat","hit")]
  
  load('Data/app_population_hitrates.RData')
  load('Data/app_population_shortDistList.RData')
  # # Get Hit Rates for Circles of Fixed Radius around this random sample of centers
  # hit_rates_n = rep(NA, n_samples)
  # idx_short_dists_list = list()
  # for(i in 1:n_samples){
  #   # Longitude and Latitude of Center
  #   center_lon = centers[i,c("lon")]
  #   center_lat = centers[i,c("lat")]
  # 
  #   # Distances between Center and All Stops
  #   dists = sqrt((df$lon - center_lon)^ 2 + (df$lat - center_lat)^ 2)
  # 
  #   # Hit Rate of N number of nearest Stops to Center
  #   n_short_dists = sort(dists, decreasing = F)[1:n_in_circle]
  #   idx_short_dists = which(dists %in% n_short_dists)
  #   idx_short_dists_list[[i]] = idx_short_dists
  # 
  #   hit_rates_n[i] = sum(df$hit[idx_short_dists]) / n_in_circle
  # }
  # save(hit_rates_n, file='Data/app_population_hitrates.RData')
  # save(idx_short_dists_list, file='Data/app_population_shortDistList.RData')

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
  center_neighbors.sampled = center_neighbors[sample(nrow(df),nrow(df)*.1),]
  stops = geom_point(aes(x=lon, y = lat), 
                     data = center_neighbors.sampled, 
                     color = 'red',
                     alpha = 0.2)
  
  
  ########################################
  
  # Load Hit Rate Distribution Computed by Drawing Samples over All of NYC
  # Without any radius or population restrictions
  load('Data/hit_rate_dist_over_all_NYC.RData')
  
  
  ########################################
  
  output$distPlot <- renderPlot({
    
    # Random Areas
    random_areas = df[idx_short_dists_list[[sample.no()]],]
    stops2 = geom_point(aes(x=lon, y = lat), 
                        data = random_areas, 
                        color = 'blue',
                        alpha = 0.2)  
    # Show Map
    nycMap + stops + stops2 +
      labs(caption="Showing 10% of Stops")
      
  })
  
  ########################################
  
  output$newPlot <- renderPlot({
    
    plot(density(hit_rates_n[1:sample.no()]), main = 'Sampling Distribution of Hit Rates', col='blue', ylim=c(0,240))
    abline(v = mean(hit_rates_n[1:sample.no()]), col = 'blue', lty = 2)
    lines(density(hit_rates), col = 'green')
    abline(v = mean(hit_rates), col = 'green', lty=2)
    abline(v = center_hit_rate, col = 'red', lty=2)
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

