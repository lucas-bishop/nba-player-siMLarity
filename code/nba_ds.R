library(tidyverse)
library(rvest)
library(BBmisc)

# this function needs to be tuned to incclude all table elements from each of the 3 pages
# then select the ones we need as nodes
pull_player_data <- function(url){
  print(url)
  read_html(url) %>%
    html_table("table#table-4978.tablesaw.compact.tablesaw-stack", header = NA, fill = TRUE)
}


html_pages <- c(1:3) %>% 
  paste("https://basketball.realgm.com/nba/stats/2019/Misc_Stats/Qualified/dbl_dbl/All/desc/",.)


traditional <- read_csv("data/36minutes.csv") %>% select(-badcol)
advanced <- read_csv("data/advancedNBA.csv") %>% select(-badcol)
# make last df with our above functions
#extra <- map_dfr(html_pages, pull_player_data)
  
#check if these table numbers change uponreload: they do, change script to pull all tables
extra1 <- read_html("https://basketball.realgm.com/nba/stats/2019/Misc_Stats/Qualified/dbl_dbl/All/desc/ 1") %>% 
  html_table("table#table-7945.tablesaw.compact.tablesaw-stack", header = NA, fill = TRUE)
extra2 <- read_html("https://basketball.realgm.com/nba/stats/2019/Misc_Stats/Qualified/dbl_dbl/All/desc/ 2") %>% 
  html_table("table#table-4389.tablesaw.compact.tablesaw-stack", header = NA, fill = TRUE)
extra3 <- read_html("https://basketball.realgm.com/nba/stats/2019/Misc_Stats/Qualified/dbl_dbl/All/desc/ 3") %>% 
  html_table("table#table-9644.tablesaw.compact.tablesaw-stack", header = NA, fill = TRUE)

# get rid of the win share stats that are redundant in misc dataset
misc <- rbind(extra1[[1]], extra2[[1]], extra3[[1]]) %>% select(-'#', -"OWS", -"DWS", -"WS")
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
cutoff <- 24.5
full_stats_qual <- subset(full_stats_qual, MPG >= cutoff)

## Normalize data? So values with large ranges like pts don't have more weight than small range values like blk/stl
# Use BBmisc::normalize to (for each value) subtract the mean of the feature-range from that value and divide by the SD of the feature
normalized_numeric <- normalize(full_stats_qual, method = "standardize")
# Now we have a tidy clean dfs to work with, can start analysis

normalized_numeric <- normalized_numeric[c(7:65)]
# Heat map of all features
corrplot::corrplot(cor(corr_tbl), method = "circle")

###########
#analysis#
###########

# PCoA
# covariance matrix creation
cov_matrix <- cov(normalized_numeric) %>% round(3)

# find eigenvectors
eigenvectors <- eigen(cov_matrix)$vectors

# select first 2 eigenvectors
eigenv2 <- eigenvectors[,(1:2)]

# convert to matrix for ordination
statsmatrix <- tibble(normalized_numeric)

#  
























