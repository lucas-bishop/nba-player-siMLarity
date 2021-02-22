library(tidyverse)
library(rvest)
library(BBmisc)
library(ggrepel)
library(corrplot)
library(factoextra)

# this function needs to be tuned to incclude all table elements from each of the 3 pages
# then select the ones we need as nodes
# pull_player_data <- function(url){
#   print(url)
#   read_html(url) %>%
#     html_table("table#table-4978.tablesaw.compact.tablesaw-stack", header = NA, fill = TRUE)
# }

# html_pages <- c(1:3) %>% 
#  paste("https://basketball.realgm.com/nba/stats/2019/Misc_Stats/Qualified/dbl_dbl/All/desc/",.)

######
#data#
######
setwd("C:/Users/bisho/Documents/NBA player siMLarity/")
traditional <- read_csv("data/36minutes.csv") %>% select(-badcol)
advanced <- read_csv("data/advancedNBA.csv") %>% select(-badcol)
# should make last df with our above function and map it to the html_pages list:
# extra <- map_dfr(html_pages, pull_player_data)
  
# extra pages for misc df have only qualified players, qualifications found here: https://basketball.realgm.com/info/glossary
extra1 <- read_html("https://basketball.realgm.com/nba/stats/2019/Misc_Stats/Qualified/dbl_dbl/All/desc/ 1") %>% 
  html_table("table#table-7945.tablesaw.compact.tablesaw-stack", header = NA, fill = TRUE)
extra2 <- read_html("https://basketball.realgm.com/nba/stats/2019/Misc_Stats/Qualified/dbl_dbl/All/desc/ 2") %>% 
  html_table("table#table-4389.tablesaw.compact.tablesaw-stack", header = NA, fill = TRUE)
extra3 <- read_html("https://basketball.realgm.com/nba/stats/2019/Misc_Stats/Qualified/dbl_dbl/All/desc/ 3") %>% 
  html_table("table#table-9644.tablesaw.compact.tablesaw-stack", header = NA, fill = TRUE)

# need to get rid of positions with dashes in them too
# get rid of the win share stats that are redundant in misc dataset
misc <- rbind(extra1[[13]], extra2[[13]], extra3[[13]]) %>% select(-'#', -"OWS", -"DWS", -"WS")
merged_stats <- inner_join(traditional, advanced) %>% select(-Rk) 
  

## cleanup before misc merge

misc$Player=gsub(",", "", misc$Player, fixed = TRUE)
# Had initally tried to replace all of the special characters in foreign names, but eventually just changed the csv files before reading in

#Now have full data frame with combined data - can add more columns if I find extra exportable stats websites
full_stats <- full_join(merged_stats, misc, by = 'Player') %>% 
  unique() %>% select(-Team) %>% mutate(MPG = MP / G)

#This full Df will have plenty of NA values, because the misc dataset has qualifiers that filter out irrelevant players based on min, pts, ast, stl, blk, etc.
full_stats_qual <- full_stats %>% drop_na()
#This leaves us with a data frame of players that have data entries in all 65 variables, or 'relevant' players
# now to subset this data for only looking at players that play significant minutes. We decide this. I based it on the average of qualified players.
# could use a rarefaction curve to see what mpg cutoff to rarefy to
cutoff <- 25.5
full_stats_qual <- subset(full_stats_qual, MPG >= cutoff)

## Normalize data? So values with large ranges like pts don't have more weight than small range values like blk/stl
# Use BBmisc::normalize to (for each value) subtract the mean of the feature-range from that value and divide by the SD of the feature
normalized_numeric <- normalize(full_stats_qual, method = "standardize")
# Now we have a tidy clean dfs to work with, can start analysis

normalized_numeric <- normalized_numeric[c(7:65)]
# Heat map of all features
cplot_big <- corrplot::corrplot(cor(normalized_numeric), method = "circle")
# ggsave(cplot_big) not sure if need to save this

###########
#analysis#
###########

### PCoA ###

# covariance matrix creation
cov_matrix <- cov(normalized_numeric) %>% round(3)

# find eigenvectors
eigenvectors <- eigen(cov_matrix)$vectors

# select first 2 eigenvectors
eigenv2 <- eigenvectors[,(1:2)]

# convert to matrix for ordination
statsmatrix <- data.matrix(normalized_numeric)

# matrix multiplication (multiplying the arrays element-wise?)
pc2 <- statsmatrix%*%eigenv2

# covert back for ggplot
numeric_df <- as_tibble(pc2)

# plot ordination
player_ordination <- ggplot(numeric_df,aes(numeric_df$V1, numeric_df$V2)) +
  labs(x="PC1",y="PC2", title = "2019 NBA Season")+
  # coloring by position and distinguishing by Value Over Replacement Player lets us draw some conclusions
  geom_point(data=full_stats_qual,aes(color = Pos, size= VORP))+
  geom_text_repel(data=full_stats_qual, aes(label=Player))

ggsave("player_ordination.pdf", player_ordination, width = 15, height = 15, units = "in")
ggsave("player_ordination.jpg", player_ordination, width = 12, height = 12, units = "in")

# Need to place even higher qualifications for MPG so the ordination is not as crowded
# can add ordinations on only offensive/defensince stats

### K-means clustering ###
row.names(normalized_numeric) <- full_stats_qual$Player 

# Clarify distance measures
res_dist <- get_dist(normalized_numeric, stand = TRUE, method = "euclidean")
distance_heatmap <- fviz_dist(res_dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
ggsave("distance_heatmap.pdf", distance_heatmap, width = 15, height = 15, units = "in")
ggsave("distance_heatmap.jpg", distance_heatmap, width = 12, height = 12, units = "in")


# Determinie optimal clusters through different methods
fviz_nbclust(normalized_numeric, kmeans, method = "wss")
fviz_nbclust(normalized_numeric, kmeans, method = "silhouette")
fviz_nbclust(normalized_numeric, kmeans, method = "gap_stat")
# Looks like it is 9 (7-10 are all decent)
# this number of clusters may come down with a strciter minute cutoff

km_res <- kmeans(normalized_numeric, 9, nstart = 25)

# Visualize Kmeans 
# This gives us an ordination of ~9 player types
kmeans <- fviz_cluster(km_res, normalized_numeric, ellipse = TRUE, ellipse.alpha= 0.1,
            palette = "viridis",  repel = TRUE, ggtheme = theme_classic(), 
             main= FALSE, xlab= FALSE, ylab = FALSE)

# Russell Westbrook is his own cluster (#7). LOL
ggsave("kmeans.pdf", kmeans, width = 15, height = 15, units = "in")
ggsave("kmeans.jpg", kmeans, width = 12, height = 12, units = "in")

# can add clustering based on offensive or defensive stats once those df's are made above

### Hierarchical clusterin ###
clusters <- data.frame(sort(km_res$cluster));
view(km_res)

# hierarchial clustering --------------------------------------------------

res_hc <- hclust(res_dist, method = "ward.D2" )

# Visualize using factoextra
dendrogram <- fviz_dend(res_hc, k = 9, # Cut in 8 groups
          cex = 0.5, # label size
          horiz= TRUE, rect = TRUE,
          main = "player dendrogram", ylab = "relative distance"# Add rectangle around groups
)
ggsave("dendrogram.pdf", dendrogram, width = 15, height = 15, units = "in")
ggsave("dendrogram.jpg", dendrogram, width = 12, height = 12, units = "in")













