file_path <- "C:\\Users\\edimi\\Downloads\\artist.tar.xz"


fls <- list.files(pattern = "\\.xz")
untar(file_path, verbose = TRUE)
# 


library(jsonlite)

# Specify the complete path to the JSON file
json_file_path <- "C:\\Users\\edimi\\Downloads\\artist\\mbdump\\artist.json"

# Read the JSON file
json_data <- stream_in(file(json_file_path, "r"), n_max = 100)

# Print the resulting data
print(json_data)


library(jsonlite)
artist_info_ranking <- readRDS("artist_info_ranking.rds")

json_file_path <- "C:/Users/edimi/Downloads/artist/mbdump/artist.json"
# Specify the number of records to read
records_to_read <- 5000

# Read the specified number of lines from the file
json_lines <- readLines(json_file_path, n = records_to_read)

count <- 0
for (i in 1:records_to_read){
  json <- fromJSON(json_lines[i])
  
  if (json$name %in% artist_info_ranking$artist_spotify){
    count <- count +1
  # Accessing variables
  name <- json$name
  id <- json$id
  begin <- json$`life-span`$begin
  end <- json$`life-span`$end
  ended <- json$`life-span`$ended
  
  # Printing the extracted information
  cat("Name:", name, "\n")
  cat("ID:", id, "\n")
  cat("Begin:", begin, "\n")
  cat("End:", end, "\n")
  cat("Ended:", ended, "\n")
  print(count)
  }
}


# Accessing variables
name <- json$name
id <- json$id
begin <- json$`life-span`$begin
end <- json$`life-span`$end
ended <- json$`life-span`$ended

# Printing the extracted information
cat("Name:", name, "\n")
cat("ID:", id, "\n")
cat("Begin:", begin, "\n")
cat("End:", end, "\n")
cat("Ended:", ended, "\n")














library(jsonlite)
artist_info_ranking <- readRDS("artist_info_ranking.rds")

json_file_path <- "C:/Users/edimi/Downloads/artist/mbdump/artist.json"
#records_to_read <- 100

# Open the file for reading
con <- file(json_file_path, "r")

# Initialize counter
count <- 0
i <- 0
life_span <- data.frame()
# Read and process each line
while (length(line <- readLines(con, n = 1)) > 0) {
  json <- fromJSON(line)
  i <- i + 1
  # Check if name is in artist_spotify
  if (json$name %in% artist_info_ranking$artist_spotify) {
    count <- count + 1
    
    # Accessing variables
    name <- ifelse(is.null(json$name), "N/A", json$name)
    id <- json$id
    begin <- ifelse(is.null(json$`life-span`$begin), "N/A", json$`life-span`$begin)
    end <- ifelse(is.null(json$`life-span`$end), "N/A", json$`life-span`$end)
    ended <- json$`life-span`$ended
    
    new_row <- cbind(name, id, begin, end, ended)
    life_span <- rbind(life_span, new_row)
    
    # Printing the extracted information
    cat("Name:", name, "\n")
    cat("ID:", id, "\n")
    cat("Begin:", begin, "\n")
    cat("End:", end, "\n")
    cat("Ended:", ended, "\n")
    print(count)
    print(i)
    saveRDS(life_span, file = "life_span.rds")
  }
}


# Close the file
close(con)
rm(con)


read
