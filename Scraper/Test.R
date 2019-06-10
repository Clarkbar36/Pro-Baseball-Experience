library(rvest)


links <- c("http://probaseballexperience.jcink.net/index.php?showforum=62","http://probaseballexperience.jcink.net/index.php?showforum=62&prune_day=100&sort_by=Z-A&sort_key=last_post&st=15")
TPE <- data.frame(`Topic Title`=character())
for (l in links){

  sloths <- read_html(l)
  test <- sloths %>% html_node(xpath = '//*[@id="topic-list"]/form[1]/table') %>% html_table(header = TRUE)
  test <- test[3]
  TPE <- rbind(test,TPE)

}
