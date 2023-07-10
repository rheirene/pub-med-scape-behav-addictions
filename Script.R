############################################
# This works for a single page:

# Load required libraries
install.packages(c("rvest", "stringr"))
library(rvest)
library(stringr)

# Define the URL
url <- "https://scholar.google.com/scholar?start=0&q=allintitle:+%22gambling+disorder%22&hl=en&as_sdt=0,5"

# Read and parse the webpage
webpage <- read_html(url)

# Extract article titles
titles <- webpage %>%
  html_nodes(".gs_rt a") %>%
  html_text()

# Extract authors and publication information
author_info <- webpage %>%
  html_nodes(".gs_a") %>%
  html_text()

# Extract the year
years <- str_extract(author_info, "\\d{4}")

# Extract the cited count
cited_by <- webpage %>%
  html_nodes(".gs_fl a:nth-child(3)") %>%
  html_text() %>%
  str_extract("\\d+")

# Create a data frame with the extracted data
results <- data.frame(Title = titles,
                      Author_Info = author_info,
                      Year = years,
                      Cited_By = cited_by)

# Print the results
print(results)










###################################

# NOT TESTED AS BLOCKED OUT AT 15;23 on uni vpn

# Load required libraries
install.packages(c("rvest", "stringr"))
library(rvest)
library(stringr)

# Define the base URL
base_url <- "https://scholar.google.com/scholar?start="

# Define the search query
query <- "&q=allintitle:+%22gambling+disorder%22&hl=en&as_sdt=0,5"

# Initialize the results data frame
results <- data.frame()

# Scrape data from all pages
for (i in seq(0, 1140, 114)) { # You can adjust the range here depending on the number of pages you want to scrape
  # Define the URL for the current page
  url <- paste0(base_url, i, query)
  
  # Read and parse the webpage
  webpage <- read_html(url)
  
  # Extract article titles
  titles <- webpage %>%
    html_nodes(".gs_rt a") %>%
    html_text()
  
  # Extract authors and publication information
  author_info <- webpage %>%
    html_nodes(".gs_a") %>%
    html_text()
  
  # Extract the year
  years <- str_extract(author_info, "\\d{4}")
  
  # Extract the cited count
  cited_by <- webpage %>%
    html_nodes(".gs_fl a:nth-child(3)") %>%
    html_text() %>%
    str_extract("\\d+")
  
  # Create a data frame with the extracted data
  page_results <- data.frame(Title = titles,
                             Author_Info = author_info,
                             Year = years,
                             Cited_By = cited_by)
  
  # Append the current page results to the overall results
  results <- rbind(results, page_results)
  
  # Wait to avoid overwhelming the server or getting blocked
  sleep_time <- runif(1, min = 5, max = 35)
  Sys.sleep(sleep_time)
}

# Print the results
print(results)


























####################

# Load required libraries
install.packages(c("rvest", "stringr", "dplyr"))
library(rvest)
library(stringr)
library(dplyr)


# Define the base URL
base_url <- "https://scholar.google.com/scholar?start=%%d&q=allintitle:+%%22gambling+disorder%%22&hl=en&as_sdt=0,5"

# Function to scrape a single page
scrape_page <- function(url) {
  webpage <- read_html(url)
  
  titles <- webpage %>%
    html_nodes(".gs_rt a") %>%
    html_text()
  
  author_info <- webpage %>%
    html_nodes(".gs_a") %>%
    html_text()
  
  years <- str_extract(author_info, "\\d{4}")
  
  cited_by <- webpage %>%
    html_nodes(".gs_fl a:nth-child(3)") %>%
    html_text() %>%
    str_extract("\\d+")
  
  data.frame(Title = titles,
             Author_Info = author_info,
             Year = years,
             Cited_By = cited_by)
}

# Initialize variables
results <- data.frame()
page_number <- 0
has_results <- TRUE

# Loop through pages
while (has_results) {
  # Generate URL for the current page
  url <- sprintf("https://scholar.google.com/scholar?start=%d&q=allintitle:+%%22gambling+disorder%%22&hl=en&as_sdt=0,5", page_number * 10)
  
  # Scrape the current page
  current_page_results <- scrape_page(url)
  
  # Check if the current page has results
  has_results <- nrow(current_page_results) > 0
  
  # If there are results, append them to the existing results data frame
  if (has_results) {
    results <- rbind(results, current_page_results)
    page_number <- page_number + 1
  }
  
  # Add a random sleep time between requests to avoid being blocked
  sleep_time <- runif(1, min = 5, max = 10)
  Sys.sleep(sleep_time)
}
# Print the results
print(results)
