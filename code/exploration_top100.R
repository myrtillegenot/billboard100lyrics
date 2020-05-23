# Author: Myrtille Genot
# Code : Exploration Top 100 Data
# Date: 04/14/2020


top100 <- read_csv("~/Desktop/portfolio/projects/Billboard/top100data.csv")

#change ome variable formats

top100$rank <- as.integer(top100$rank) 

#Note how the string matches are least accurate in earlier years which makes sense. 
tab1 <- top100 %>% 
  group_by(billboard_year) %>% 
  arrange(billboard_year, rank)

#