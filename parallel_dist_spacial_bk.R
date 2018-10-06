require(parallelDist)
require(dplyr)

radius = 0.005

#test

# Load Data
# setwd('/Users/Chansoo/Desktop/Spatial_Project/')
load('Data/stops2013clean.RData')
prec_bor = read.csv('Data/prec_to_boroughs.csv', header = F, stringsAsFactors = F)
colnames(prec_bor) = c('precinct','borough')

# Subset data
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
df$precinct = as.integer(as.character(df$precinct))
df = df %>% 
  left_join(prec_bor, by = 'precinct')



#########################
##### Start Dist Mat
#########################

########### BROOKLYN ONLY BC SO MANY OBS IN BROOKLYN ##############

# For each borough, create distance matrix, then identify centroid
# that contains the most obs within radius, r

# Set Borough
borough = "Brooklyn"

# Prepare data for distance calculations
df2 = df[df$borough == borough,c('lon','lat')]

# df2 = df2[sample(nrow(df2),nrow(df2)*.1),]
df2 = as.matrix(df2)

# Distance Matrix
dist.euclidean = parDist(df2, method='euclidean')
dist.mat = as.matrix(dist.euclidean)

# Set Radius
r = radius

# Count # of Observations within Radius
idx = apply(dist.mat,2,function(x){sum(x < r)})


idx = as.numeric(names(sort(idx, decreasing = T)))

result = df2[idx[1:50],]

print(borough)
print(result)
write.csv(result,paste('results/',paste(borough,radius,'result.csv',sep='_'),sep=''))

rm(dist.euclidean)
rm(dist.mat)


