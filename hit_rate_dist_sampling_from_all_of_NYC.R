library(shiny)
library(dplyr)

setwd('/Users/Chansoo/Desktop/Spatial_Project/')

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

n = 1000
hit_rates = rep(NA,n)
for( i in 1:n) hit_rates[i] = mean(sample(df$hit,10000))

plot(density(hit_rates))

save(hit_rates,file='Data/hit_rate_dist_over_all_NYC.RData')
