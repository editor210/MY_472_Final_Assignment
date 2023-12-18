library(rvest)
library(dplyr)
library(RSelenium)

rD <- rsDriver(browser=c("firefox"), verbose = F, chromever = NULL)

driver <- rD[["client"]] 
url <- "https://www.rollingstone.com/music/music-lists/100-greatest-artists-147446/"

driver$navigate(url)
#Rejecting cookies
cookie <- driver$findElement(using = 'css', value = '#onetrust-reject-all-handler')
cookie$clickElement()

#Getting to the bottom of the page
webElem <- driver$findElement("css", "body")
webElem$sendKeysToElement(list(key = "end"))

#Getting the full page html
page_source <- driver$getPageSource()[[1]]

#Extracting the rankings
html_elements_ranking <- read_html(page_source) %>%
  html_nodes(".c-gallery-vertical-album__number") %>% html_text()

#Extracting the Artists for each ranking
html_elements_titles <- read_html(page_source) %>%
  html_nodes(".c-gallery-vertical-album__title") %>% html_text()

#NO NEED - Rolling the page to the load more button
#driver$executeScript("window.scrollBy(0, -3000);")

#Finding the load_more button
load_more <- driver$findElement(using = 'css', value = ".c-gallery-vertical__load-button")
load_more$clickElement()

#Going to the bottom of the page to load every artist
webElem <- driver$findElement("css", "body")
webElem$sendKeysToElement(list(key = "end"))

page_source <- driver$getPageSource()[[1]]
#Extracting the rankings
html_elements_ranking <- c(html_elements_ranking, read_html(page_source) %>%
  html_nodes(".c-gallery-vertical-album__number") %>% html_text())

#Extracting the Artists for each ranking
html_elements_titles <- c(html_elements_titles, read_html(page_source) %>%
  html_nodes(".c-gallery-vertical-album__title") %>% html_text())

#Closing the port 
driver$close()
rD$server$stop()

# close the associated Java processes if necessary:
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

rankings_df <- data.frame(ranking = html_elements_ranking, artist = html_elements_titles)

# Save the data frame as a CSV file
write.csv(rankings_df, file = "rs_ranking.csv", row.names = FALSE)
