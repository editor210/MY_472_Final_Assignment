library(RSelenium)
library(httr)
library(rvest)

artist_info_ranking <- readRDS("artist_info_ranking.rds")

rD <- rsDriver(browser=c("firefox"), verbose = F, chromever = NULL)
driver <- rD[["client"]] 
url <- "https://musicbrainz.org/"

driver$navigate(url)
mbdata <- data.frame()
all_results <- data.frame()
for (artist in artist_info_ranking$artist_spotify){
      # Find the search field and perform the search
      while (TRUE){
        print(artist)
        search_field <- driver$findElement(using = 'css', value = '#headerid-query')
        search_field$sendKeysToElement(list(artist))
        search_field$sendKeysToElement(list(key = "enter"))
        
        #Giving time for the search results to show
        Sys.sleep(2)
        
        #Getting the search results
        search_results <- try(driver$findElement(using = 'css', value = ".tbl"), silent = TRUE)
        
        #If there is an internal error, try the search again
        if ("try-error" %in% class(search_results)){
          print("Trying search again")
          Sys.sleep(2)
        } else {break}
      }
  
  # Extracting the outer HTML of the table
  results_html <- search_results$getElementAttribute("outerHTML")
  
  #Reading the HTML element of the results
  results_html <- read_html(results_html[[1]])
  
  #Extracting the table of the results
  results_table <- html_table(results_html)[[1]]
  
  #selecting the first row as the correct result
  new_row <- results_table[1,]
  new_row <- cbind(artist_spotify = artist, new_row)
  mbdata <- rbind(mbdata, new_row)
  
  #Saving all other results in case there is mismatch
  result_rows <- cbind(artist_spotify = artist, results_table)
  all_results <- rbind(all_results, result_rows)
}

#Closing the port 
driver$close()
rD$server$stop()

# close the associated Java processes if necessary:
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)


#Correcting the result for The Drifters that found a Taiwanese group
the_drifters <- all_results[(all_results$artist == "The Drifters"),]
#The correct result is the second row
the_drifters <- the_drifters[2,]

#replacing the row in the mbdata df
#Finding the index of the original data
drifters_index <- which(mbdata$artist == "The Drifters")
# Update the corresponding row in mbdata with the corrected information
mbdata[drifters_index, ] <- the_drifters


#Treating the Begin and End datas to make it in uniform format
#If there is only the year and month, the day 01 is assigned
#If there is only the year, the first of july of the year is assigned

mbdata$Begin <- ifelse(nchar(mbdata$Begin) == 4, paste0(mbdata$Begin, "-07-01"), mbdata$Begin)
mbdata$Begin <- ifelse(nchar(mbdata$Begin) == 7, paste0(mbdata$Begin, "-01"), mbdata$Begin)

mbdata$End <- ifelse(is.na(mbdata$End), "", mbdata$End)
mbdata$End <- ifelse(nchar(mbdata$End) == 4, paste0(mbdata$End, "-07-01"), mbdata$End)
mbdata$End <- ifelse(nchar(mbdata$End) == 7, paste0(mbdata$End, "-01"), mbdata$End)

saveRDS(mbdata, ".\\data\\mbdata.rds")
