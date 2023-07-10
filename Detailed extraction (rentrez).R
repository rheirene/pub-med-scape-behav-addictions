# Load required libraries:
library(rentrez)
library(dplyr)
library(purrr)

################# Work addiction search
# "Work addiction" [https://pubmed.ncbi.nlm.nih.gov/30920291/]
# "workaholism" [https://pubmed.ncbi.nlm.nih.gov/28425778/] '

# (("work addiction"[Title/Abstract]) OR ("addiction to work"[Title/Abstract])) OR ("workaholism"[Title/Abstract])
# Again, I manually inspected all of the results directly in the PubMed site. All appeared relevant.

# START OF ANALYSIS (performed on 16/05/2022)
#  Clean environment:
rm(list = ls())
# Define the search term:
search_term_work <- '("work addiction"[Title/Abstract]) OR ("addiction to work"[Title/Abstract]) OR ("workaholism"[Title/Abstract])'

# Use entrez_search to get the IDs of the articles:
search_results_work <- entrez_search(db="pubmed", term=search_term_work, retmax=20000)
id_list_work <- search_results_work$ids

# Split id list into chunks of 500:
chunks <- split(id_list_work, ceiling(seq_along(id_list_work)/500))

# Fetch details for each chunk of articles:
article_details_work <- map_dfr(chunks, function(ids) {
  # Fetch details of the articles:
  articles <- entrez_fetch(db="pubmed", id=ids, rettype="medline", retmode="text")
  
  # Split the articles into individual articles:
  articles <- strsplit(articles, "\n\n")[[1]]
  
  # Process each article:
  map_dfr(articles, function(article) {
    # Split the article into lines:
    lines <- strsplit(article, "\n")[[1]]
    
    # Get the details we're interested in:
    details_work <- list(
      PMID = if (any(grepl("^PMID", lines))) lines[grepl("^PMID", lines)] else NA,
      DP = if (any(grepl("^DP", lines))) lines[grepl("^DP", lines)] else NA,
      TI = if (any(grepl("^TI", lines))) lines[grepl("^TI", lines)] else NA,
      LID = if (any(grepl("^LID", lines))) paste(lines[grepl("^LID", lines)], collapse="; ") else NA,
      AB = if (any(grepl("^AB", lines))) paste(lines[grepl("^AB", lines)], collapse=" ") else NA,
      FAU = if (any(grepl("^FAU", lines))) paste(lines[grepl("^FAU", lines)], collapse="; ") else NA,
      AD = if (any(grepl("^AD", lines))) paste(lines[grepl("^AD", lines)], collapse="; ") else NA,
      LA = if (any(grepl("^LA", lines))) paste(lines[grepl("^LA", lines)], collapse="; ") else NA,
      PT = if (any(grepl("^PT", lines))) paste(lines[grepl("^PT", lines)], collapse="; ") else NA,
      TA = if (any(grepl("^TA", lines))) lines[grepl("^TA", lines)] else NA,
      COIS = if (any(grepl("^COIS", lines))) lines[grepl("^COIS", lines)] else NA,
      JT = if (any(grepl("^JT", lines))) lines[grepl("^JT", lines)] else NA
    )
    
    # Convert the list of details into a one-row data frame:
    details_df_work <- bind_rows(details_work)
    
    return(details_df_work)
  })
})

# Check the results:
article_details_work %>%
  as_tibble() %>%
  print()

# Let's remove the identifiers at the beginning of each data point:
rep_str <- c('PMID- ' = '', 'DP  - ' = '', 'TI  - ' = '', 'LID - ' = '', 'AB  - ' = '',
             'FAU - ' = '', 'AD  - ' = '', 'LA  - ' = '', 'PT  - ' = '', 'TA  - ' = '',
             'COIS- ' = '', 'JT  - ' = '')

results_work <- article_details_work %>%
  mutate(across(everything(), ~str_replace_all(., rep_str))) %>% # Remove identifiers
  # Now separate the date year and month/day info:
  separate(DP, c("Year", "Month"), sep = "\\ ") %>%
  rename("PubMed_ID" = "PMID", # Let's also provide more descriptive names for each of the columns
         "Title" = "TI", 
         "DOI" = "LID", 
         "Abstract" = "AB", 
         "Full_Author_Name" = "FAU", 
         "Author_Address" = "AD", 
         "Language" = "LA", 
         "Publication_Type" = "PT", 
         "Journal_Issue" = "TA",
         "Conflict_of_Interest_Statement" = "COIS",
         "Journal_Title" = "JT") %>%
  print()

