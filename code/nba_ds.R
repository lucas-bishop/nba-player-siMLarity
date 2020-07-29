library(tidyverse)
library(rvest)

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
  html_table("table#table-4978.tablesaw.compact.tablesaw-stack", header = NA, fill = TRUE)
extra2 <- read_html("https://basketball.realgm.com/nba/stats/2019/Misc_Stats/Qualified/dbl_dbl/All/desc/ 1") %>% 
  html_table("table#table-3944.tablesaw.compact.tablesaw-stack", header = NA, fill = TRUE)
extra3 <- read_html("https://basketball.realgm.com/nba/stats/2019/Misc_Stats/Qualified/dbl_dbl/All/desc/ 1") %>% 
  html_table("table#table-4392.tablesaw.compact.tablesaw-stack", header = NA, fill = TRUE)

# get rid of the win share stats that are redundant in misc dataset
misc <- rbind(extra1[[1]], extra2[[1]], extra3[[1]]) %>% select(-'#', -"OWS", -"DWS", -"WS")
merged_stats <- inner_join(traditional, advanced) %>% select(-Rk) 
  
## cleanup before misc merge

misc$Player=gsub(",", "", misc$Player, fixed = TRUE)
# Had initally tried to replace all of the special characters in foreign names, but eventually just changed the csv files before reading in

#Now have full data frame with combined data - can add more columns if I find extra exportable stats websites
full_stats <- full_join(merged_stats, misc, by = 'Player') %>% unique()



























