# Load required packages:
# install.packages(c("rvest", "stringr", "dplyr", "tidyr"))
library(rvest)
library(stringr)
library(dplyr)
library(tidyr)


################# Gambling search
# ("gambling disorder"[Title/Abstract]) OR ("disordered gambling"[Title/Abstract] OR "gambling addiction"[Title/Abstract]) OR ("addicted gambler*"[Title/Abstract]) OR ("pathological gambl*"[Title/Abstract]) OR ("compulsive gambl*"[Title/Abstract])
# Mesh terms can be found here: https://www.ncbi.nlm.nih.gov/mesh/?term=gaming+disorder

# Tried adding problem gambling and it adds around 1,200 more studies, but the overall distribution over time remains very similar:
# - (("gambling disorder"[Title/Abstract] OR "disordered gambling"[Title/Abstract] OR "gambling addiction"[Title/Abstract] OR "addicted gambler*"[Title/Abstract] OR "pathological gambl*"[Title/Abstract])) OR ("compulsive gambl*"[Title/Abstract]) OR ("problem gambl*"[Title/Abstract])(("gambling disorder"[Title/Abstract] OR "disordered gambling"[Title/Abstract] OR "gambling addiction"[Title/Abstract] OR "addicted gambler*"[Title/Abstract] OR "pathological gambl*"[Title/Abstract])) OR ("compulsive gambl*"[Title/Abstract]) OR ("problem gambl*"[Title/Abstract])

# One unavoidable issue with the gambling search is 2 of the 3 gambling-specific journals (International Gambling Studies & Journal of Gambling Issues) are not indexed by PubMed. That said, most behavioural addiction and addiction-focused journals are, fortunately. 
# List pf journals indexed on PubMed: https://ftp.ncbi.nih.gov/pubmed/J_Medline.txt

# START OF ANALYSIS (performed on 10/05/2022)

#  Clean environment:
rm(list = ls())

# Define the URL:
url_gambling <- "https://pubmed.ncbi.nlm.nih.gov/?term=%28%22gambling+disorder%22%5BTitle%2FAbstract%5D%29+OR+%28%22disordered+gambling%22%5BTitle%2FAbstract%5D+OR+%22gambling+addiction%22%5BTitle%2FAbstract%5D%29+OR+%28%22addicted+gambler*%22%5BTitle%2FAbstract%5D%29+OR+%28%22pathological+gambl*%22%5BTitle%2FAbstract%5D%29+OR+%28%22compulsive+gambl*%22%5BTitle%2FAbstract%5D%29&size=200"
webpage <- read_html(url_gambling)

# Get the total number of search results:
results_count <- webpage %>%
  html_node(".results-amount .value") %>%
  html_text() %>%
  str_replace(",", "") %>%
  as.numeric()

# Calculate the number of pages:
results_per_page <- 10
total_pages <- ceiling(results_count[1]/ results_per_page)
# Print results_count and total_pages:
print(results_count)
print(total_pages)


# Make an empty data frame:
results_gambling2 <- data.frame()

# Loop through each page and scrape the data:
for (page in 0:(total_pages - 1)) {
  # Update the page parameter in the URL for the current page:
  current_url <- paste0(url_gambling, "&page=", page + 1)
  
  # Read and parse the current webpage:
  current_page <- read_html(current_url)
  
  # Extract article titles:
  titles <- current_page %>%
    html_nodes(".docsum-title") %>%
    html_text(trim = TRUE)
  
  # Extract authors:
  author_info <- tryCatch({
    current_page %>%
      html_nodes(".docsum-authors.short-authors") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting author info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Extract the journal and year
  journal_year_info <- tryCatch({
    current_page %>%
      html_nodes(".docsum-journal-citation.short-journal-citation") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting journal info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Create a data frame with the extracted data:
  current_results <- data.frame(Title = titles,
                                Author_Info = author_info,
                                Journal_Year_Info = journal_year_info)
  
  # Combine the current results with the previous results:
  results_gambling2 <- rbind(results_gambling2, current_results)
  
  # Add a delay to avoid overloading the server and getting booted off!
  sleep_time <- runif(1, min = 10, max = 20)
  Sys.sleep(sleep_time)
}

# Check outcomes:
print(length(titles))
print(length(author_info))
print(length(journal_year_info))

as_tibble(results_gambling2) %>% 
  print()

names(results_gambling2)

# Visually inspect results:
View(results_gambling2)
# It appears that there are some duplicates where the title and journal/date info are the same, but the authors are different

# Now save the initial results before proceeding so we can't lose them!
write.csv(results_gambling2, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/gambling_data.csv", row.names=FALSE)

# Read written csv. file if required:
results_gambling2 <- read.csv("gambling_data.csv")
# Now let's explore the data and prepare it for analysis
# Total number returned:
results_gambling2 %>%
  count() %>% 
  print()

# Total number with basic duplication removal based on title (there are some distinct papers with the same title like "gambling disorder", Sso this is too simplified, but it'll do for a quick check):
Simple_duplicate_removal_n <- results_gambling2 %>%
  distinct(Title) %>%
  count() %>% 
  print()

# How many duplicates (based on title alone) does this remove?
count(results_gambling2) - Simple_duplicate_removal_n # n = 579

# Have a look at duplicates where the title, year, and authors are all the same:
results_gambling2 %>% 
  group_by(Title, Journal_Year_Info, Author_Info) %>% 
  filter(n()>1) %>% View()

# Have a look at duplicates were just the title, journal and year are the same:
results_gambling2 %>% 
  group_by(Title, Journal_Year_Info) %>% 
  filter(n()>1) %>% View() 
# There are a few more of these, suggesting there are some papers with the same title, journal and date but different authors. 
# I did a bit of manual research and it seems that there are some commentaries on papers that lead to duplicate titles and journals/dates, but different authors. So, We need to remove duplicates based only on identical titles and **authors**.

# Remove duplicates based on only duplicated titles and authors:
results_gambling_sans_duplicates<- results_gambling2 %>%
  distinct(Title, Author_Info, .keep_all = TRUE) %>% 
  print()

# Here are our very few studies with duplicate titles and journal/date info, but different authors. These seem to be legitimate duplicates that don't require removal:
results_gambling_sans_duplicates %>% 
  group_by(Title, Journal_Year_Info) %>% 
  filter(n()>1) %>% print()
# Notes on these duplicates:
# The Gambling disorder paper from 2019 duplicate is a graphical view of gambling disorder published seperate to the paper.
# The Gambling disorder paper from the Lancet Psychiatry 2022 are commentaries
# There do appear to be two separate papers from the Lancet with the same title in 1968 (https://pubmed.ncbi.nlm.nih.gov/4172811/) and (https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(68)90288-2/fulltext)

# Visually inspect results:
# View(results_gambling_sans_duplicates) # Okay, this seems to have worked in removing proper duplicates and has removed less than using just title alone.


# Let's see how many Duplicates this process removed:
count(results_gambling2) - count(results_gambling_sans_duplicates) # 557

# Inspect the duplicates the more complicated process removed to double check we are removing genuine duplications:
results_gambling2 %>% 
  group_by(Title, Journal_Year_Info) %>% 
  mutate(dupe = n()>1) %>% (View)

# Now separate the journal and date info:
results_gambling_sans_duplicates_split<- as_tibble(results_gambling_sans_duplicates) %>%
  separate(Journal_Year_Info, c("Journal", "Year"), sep = "\\. ") %>%
  print()

# We need to remove the period from the end of the year data:
Year_numeric<- as.numeric(stringr::str_remove(results_gambling_sans_duplicates_split$Year, "\\.$"))

# Now append our updated year column to to the main dataset, first checking the new and old dates match to create a cleam, useable dataset:
results_gambling_clean<- results_gambling_sans_duplicates_split %>% 
  bind_cols(Year_numeric)  %>% 
  select(-Year) %>% 
  rename(Year = 4) # Checked dates matched before removal and finalisation


#  Okay, now we have a clean dataset, let's explore!
# Number of publications per year:
Gambling_hist<- hist(results_gambling_clean$Year,
                     xlim = c(1960,2023),
                     breaks = 50,
                     main = "Papers published per year on Gambling Disorder (PubMed)",
                     xlab = "Year")

# Summary of the Year  variable:
summary(results_gambling_clean$Year)

# Number of separate journals:
results_gambling_clean %>% 
  distinct(Journal) %>% 
  arrange(Journal) %>% 
  print(n = 100)# 601!

# Most popular journals:
results_gambling_clean %>% 
  group_by(Journal) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(desc(n)) %>%
  print(n = 100)

# Well the above code returns a weird paper where the journal name is the date and there is no journal name:
results_gambling_clean %>% filter(Journal == "2022 Aug 29")
# I took a look this manually and it appears that this is some kind of information resource and not a paper so I'm going to remove it. But before we do this, there is another paper with NA for the date so let's look a that:
results_gambling_clean %>% filter(is.na(Year))
# It's a book! Delete also

results_gambling_clean2 <- results_gambling_clean  %>% 
  filter(Journal != "2022 Aug 29" & Title != "Pathological Gambling: A Critical Review.")

# Let's check this has removed two studies:
count(results_gambling_clean) - count(results_gambling_clean2)

# And now a clear way to see if the NAs have been removed from year:
summary(results_gambling_clean2$Year)

# Now I need to add a label to all of these studies to signify that they Were returned from the gambling search so that we can distinguish them from other studies when we later joined the datasets together:
count(results_gambling_clean2) # 2917 rows
Label <- rep("gambling", times = count(results_gambling_clean2))

results_gambling_final <- results_gambling_clean2 %>% bind_cols(Label) %>%
  rename(Label = 5) %>%
  print()

# Now save the cleaned results
write.csv(results_gambling_final, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/gambling_data_cleaned.csv", row.names=FALSE)






################# Gaming search
# ((((("gaming disorder"[Title/Abstract]) OR ("internet gaming disorder"[Title/Abstract])) OR ("gaming addiction"[Title/Abstract])) OR ("video game addiction"[Title/Abstract])) OR ("video game disorder"[Title/Abstract])) OR ("gaming dependence")
# "video game dependence" returned no results and so was removed
# Mesh terms can be found here: https://www.ncbi.nlm.nih.gov/mesh/?term=gaming+disorder -- didn't use all of these as some relate to other behavioural addictions

# START OF ANALYSIS (performed on 10/05/2022)

rm(list = ls())

# Define the URL
url_gaming <- "https://pubmed.ncbi.nlm.nih.gov/?term=%28%28%28%28%28%22gaming+disorder%22%5BTitle%2FAbstract%5D%29+OR+%28%22internet+gaming+disorder%22%5BTitle%2FAbstract%5D%29%29+OR+%28%22gaming+addiction%22%5BTitle%2FAbstract%5D%29%29+OR+%28%22video+game+addiction%22%5BTitle%2FAbstract%5D%29%29+OR+%28%22video+game+disorder%22%5BTitle%2FAbstract%5D%29%29+OR+%28%22gaming+dependence%22%29&size=200"

# Read and parse the webpage
webpage_gaming <- read_html(url_gaming)

# Get the total number of search results
results_count_gaming <- webpage_gaming %>%
  html_node(".results-amount .value") %>%
  html_text() %>%
  str_replace(",", "") %>%
  as.numeric()

# Calculate the number of pages
results_per_page_gaming <- 200
total_pages_gaming <- ceiling(results_count_gaming[1] / results_per_page_gaming)
# Print results_count_gaming and total_pages_gaming
print(results_count_gaming)
print(total_pages_gaming)


# Initialize an empty data frame
results_gaming <- data.frame()

# Loop through each page and scrape the data
for (page in 0:(total_pages_gaming - 1)) {
  # Update the page parameter in the URL for the current page
  current_url <- paste0(url_gaming, "&page=", page + 1)
  
  # Read and parse the current webpage
  current_page <- read_html(current_url)
  
  # Extract article titles
  titles <- current_page %>%
    html_nodes(".docsum-title") %>%
    html_text(trim = TRUE)
  
  # Extract authors
  author_info <- tryCatch({
    current_page %>%
      html_nodes(".docsum-authors.short-authors") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting author info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Extract the journal and year
  journal_year_info <- tryCatch({
    current_page %>%
      html_nodes(".docsum-journal-citation.short-journal-citation") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting journal info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Create a data frame with the extracted data
  current_results_gaming <- data.frame(Title = titles,
                                       Author_Info = author_info,
                                       Journal_Year_Info = journal_year_info)
  
  # Combine the current results with the previous results
  results_gaming <- rbind(results_gaming, current_results_gaming)
  
  # Introduce a delay to avoid overloading the server
  sleep_time <- runif(1, min = 5, max = 15)
  Sys.sleep(sleep_time)
}

print(length(titles))
print(length(author_info))
print(length(journal_year_info))

as_tibble(results_gaming) %>%
  print(n=50)

names(results_gaming)

# Visually inspect results:
View(results_gaming)

# Now save the initial results before proceeding so we can't lose them!
write.csv(results_gaming, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/gaming_data.csv", row.names=FALSE)

# Read written csv. file if required:
results_gaming <- read.csv("gaming_data.csv")
# Now let's explore the data and prepare it for analysis
# Total number returned:
results_gaming %>%
  count() %>% 
  print()

# Total number with basic duplication removal based on title (there are some distinct papers with the same title like "gaming disorder", Sso this is too simplified, but it'll do for a quick check):
Simple_duplicate_removal_gaming_n <- results_gaming %>%
  distinct(Title) %>%
  count() %>% 
  print()

# How many duplicates (based on title alone) does this remove?
count(results_gaming) - Simple_duplicate_removal_gaming_n # n = 1

# Have a look at duplicates where the title, year, and authors are all the same:
results_gaming %>% 
  group_by(Title, Journal_Year_Info, Author_Info) %>% 
  filter(n()>1) %>% View()
# None!

# Have a look at duplicates where just the title, journal and year are the same:
results_gaming %>% 
  group_by(Title, Journal_Year_Info) %>% 
  filter(n()>1) %>% View() 
# None!

# Have a look at duplicates where just the title is the same:
results_gaming %>% 
  group_by(Title) %>% 
  filter(n()>1) %>% View() 
# There is just one and there are two genuine studies with the same name and similar authors (https://pubmed.ncbi.nlm.nih.gov/35792965/  AND https://pubmed.ncbi.nlm.nih.gov/36055740/)
# No need to remove any duplicates

# Now separate the journal and date info:
results_gaming_split<- as_tibble(results_gaming) %>%
  separate(Journal_Year_Info, c("Journal", "Year"), sep = "\\. ") %>%
  print()

# We need to remove the period from the end of the year data:
Year_numeric_gaming<- as.numeric(stringr::str_remove(results_gaming_split$Year, "\\.$"))

# Now append our updated year column to to the main dataset, first checking the new and old dates match to create a cleam, useable dataset:
results_gaming_clean<- results_gaming_split %>% 
  bind_cols(Year_numeric_gaming)  %>% 
  select(-Year) %>% 
  rename(Year = 4) # Checked dates matched before removal and finalisation


#  Okay, now we have a clean dataset, let's explore!
# Number of publications per year:
gaming_hist<- hist(results_gaming_clean$Year,
                     xlim = c(1960,2023),
                     breaks = 40,
                     main = "Papers published per year on Gaming Disorder (PubMed)",
                     xlab = "Year")

# Summary of the Year  variable:
summary(results_gaming_clean$Year)

# Number of separate journals:
results_gaming_clean %>% 
  distinct(Journal) %>% 
  arrange(Journal) %>% 
  print(n = 100)

# Most popular journals:
results_gaming_clean %>% 
  group_by(Journal) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(desc(n)) %>%
  print(n = 100)

# Now I need to add a label to all of these studies to signify that they Were returned from the gaming search so that we can distinguish them from other studies when we later joined the datasets together:
count(results_gaming_clean) # 1526 rows
Label_gaming <- rep("gaming", times = count(results_gaming_clean))

results_gaming_final <- results_gaming_clean %>% bind_cols(Label_gaming) %>%
  rename(Label = 5) %>%
  print()

# Now save the cleaned results
write.csv(results_gaming_final, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/gaming_data_cleaned.csv", row.names=FALSE)









################# Exercise search
# No MeSH terms for this, so I looked up the most recent systematic review on the topic that I could find (https://doi.org/10.1007/s11469-021-00568-1) and then  examine their search strategy. They use the following terms:  exercise addiction, exercise dependence, compulsory exercise, obligatory exercise
# (((("Exercise addiction"[Title/Abstract]) OR ("exercise dependence"[Title/Abstract])) OR ("compulsory exercise"[Title/Abstract])) OR ("obligatory exercise"[Title/Abstract])) OR ("Addiction to exercise"[Title/Abstract])

# START OF ANALYSIS (performed on 10/05/2022)

#  Clean environment:
rm(list = ls())

# Define the URL:
url_exercise <- "https://pubmed.ncbi.nlm.nih.gov/?term=%28%28%28%28%22Exercise+addiction%22%5BTitle%2FAbstract%5D%29+OR+%28%22exercise+dependence%22%5BTitle%2FAbstract%5D%29%29+OR+%28%22compulsory+exercise%22%5BTitle%2FAbstract%5D%29%29+OR+%28%22obligatory+exercise%22%5BTitle%2FAbstract%5D%29%29+OR+%28%22Addiction+to+exercise%22%5BTitle%2FAbstract%5D%29&size=200"
webpage_exercise <- read_html(url_exercise)

# Get the total number of search results:
results_count_exercise <- webpage_exercise %>%
  html_node(".results-amount .value") %>%
  html_text() %>%
  str_replace(",", "") %>%
  as.numeric()

# Calculate the number of pages:
results_per_page <- 10
total_pages_exercise <- ceiling(results_count_exercise[1]/ results_per_page)
# Print results_count and total_pages:
print(results_count_exercise)
print(total_pages_exercise)


# Make an empty data frame:
results_exercise <- data.frame()

# Loop through each page and scrape the data:
for (page in 0:(total_pages_exercise - 1)) {
  # Update the page parameter in the URL for the current page:
  current_url_exercise <- paste0(url_exercise, "&page=", page + 1)
  
  # Read and parse the current webpage:
  current_page_exercise <- read_html(current_url_exercise)
  
  # Extract article titles:
  titles <- current_page_exercise %>%
    html_nodes(".docsum-title") %>%
    html_text(trim = TRUE)
  
  # Extract authors:
  author_info <- tryCatch({
    current_page_exercise %>%
      html_nodes(".docsum-authors.short-authors") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting author info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Extract the journal and year
  journal_year_info <- tryCatch({
    current_page_exercise %>%
      html_nodes(".docsum-journal-citation.short-journal-citation") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting journal info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Create a data frame with the extracted data:
  current_results_exercise <- data.frame(Title = titles,
                                Author_Info = author_info,
                                Journal_Year_Info = journal_year_info)
  
  # Combine the current results with the previous results:
  results_exercise <- rbind(results_exercise, current_results_exercise)
  
  # Add a delay to avoid overloading the server and getting booted off!
  sleep_time <- runif(1, min = 10, max = 20)
  Sys.sleep(sleep_time)
}

# Check outcomes:
print(length(titles))
print(length(author_info))
print(length(journal_year_info))

as_tibble(results_exercise) %>% 
  print()

names(results_exercise)

# Visually inspect results:
View(results_exercise)

# Now save the initial results before proceeding so we can't lose them!
write.csv(results_exercise, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/exercise_data.csv", row.names=FALSE)

# Read written csv. file if required:
results_exercise <- read.csv("exercise_data.csv")

# Now let's explore the data and prepare it for analysis
# Total number returned:
count(results_exercise)

# Any duplicates?
count(distinct(results_exercise)) # Doesn't appear so

# Have a look at duplicates where the title, year, and authors are all the same:
results_exercise %>% 
  group_by(Title, Journal_Year_Info, Author_Info) %>% 
  filter(n()>1) %>% print()
# None!

# Have a look at duplicates where just the title, journal and year are the same:
results_exercise %>% 
  group_by(Title, Journal_Year_Info) %>% 
  filter(n()>1) %>% print() 
# None!

# Have a look at duplicates where just the title is the same:
results_exercise %>% 
  group_by(Title) %>% 
  filter(n()>1) %>% 
  print() 
# There is two, but they seem distinct.
# No need to remove any duplicates

# Now separate the journal and date info:
results_exercise_split<- as_tibble(results_exercise) %>%
  separate(Journal_Year_Info, c("Journal", "Year"), sep = "\\. ") %>%
  print()

# We need to remove the period from the end of the year data:
Year_numeric_exercise<- as.numeric(stringr::str_remove(results_exercise_split$Year, "\\.$"))

# Now append our updated year column to to the main dataset, first checking the new and old dates match to create a cleam, useable dataset:
results_exercise_clean<- results_exercise_split %>% 
  bind_cols(Year_numeric_exercise)  %>% 
  select(-Year) %>% 
  rename(Year = 4) # Checked dates matched before removal and finalisation


#  Okay, now we have a clean dataset, let's explore!
# Number of publications per year:
exercise_hist<- hist(results_exercise_clean$Year,
                   xlim = c(1960,2023),
                   breaks = 40,
                   main = "Papers published per year on Exercise Addiction (PubMed)",
                   xlab = "Year")

# Summary of the Year  variable:
summary(results_exercise_clean$Year)

# Number of separate journals:
results_exercise_clean %>% 
  distinct(Journal) %>% 
  arrange(Journal) %>% 
  print(n = 100)

# Most popular journals:
results_exercise_clean %>% 
  group_by(Journal) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(desc(n)) %>%
  print(n = 100)

# Now I need to add a label to all of these studies to signify that they Were returned from the exercise search so that we can distinguish them from other studies when we later joined the datasets together:
count(results_exercise_clean) # 379 rows
Label_exercise <- rep("exercise", times = count(results_exercise_clean))

results_exercise_final <- results_exercise_clean %>% bind_cols(Label_exercise) %>%
  rename(Label = 5) %>%
  print()

# Now save the cleaned results
write.csv(results_exercise_final, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/exercise_data_cleaned.csv", row.names=FALSE)








################# Behavioural addiction search
# (((("behavioural addiction"[Title/Abstract]) OR ("behavioral addiction"[Title/Abstract])) OR ("non-drug addiction"[Title/Abstract])) OR ("non-substance addiction"[Title/Abstract])) OR ("non-chemical addiction"[Title/Abstract])
# MeSH terms were pretty useless here so I relied on my knowledge of the field.

# I was unsure about the results from this one at first so I manually inspected most of the results directly in the PubMed site. After doing so, I'm confident that the results are all appropriate. There will definitely be a lot of overlap between the studies returned from this and the other, more specific searches..

# START OF ANALYSIS (performed on 12/05/2022)

#  Clean environment:
rm(list = ls())

# Define the URL:
url_behavioural_addiction <- "https://pubmed.ncbi.nlm.nih.gov/?term=((((%22behavioural%20addiction%22%5BTitle%2FAbstract%5D)%20OR%20(%22behavioral%20addiction%22%5BTitle%2FAbstract%5D))%20OR%20(%22non-drug%20addiction%22%5BTitle%2FAbstract%5D))%20OR%20(%22non-substance%20addiction%22%5BTitle%2FAbstract%5D))%20OR%20(%22non-chemical%20addiction%22%5BTitle%2FAbstract%5D)&sort="
webpage_behavioural_addiction <- read_html(url_behavioural_addiction)

# Get the total number of search results:
results_count_behavioural_addiction <- webpage_behavioural_addiction %>%
  html_node(".results-amount .value") %>%
  html_text() %>%
  str_replace(",", "") %>%
  as.numeric()

# Calculate the number of pages:
results_per_page <- 10
total_pages_behavioural_addiction <- ceiling(results_count_behavioural_addiction[1]/ results_per_page)
# Print results_count and total_pages:
print(results_count_behavioural_addiction)
print(total_pages_behavioural_addiction)


# Make an empty data frame:
results_behavioural_addiction <- data.frame()

# Loop through each page and scrape the data:
for (page in 0:(total_pages_behavioural_addiction - 1)) {
  # Update the page parameter in the URL for the current page:
  current_url_behavioural_addiction <- paste0(url_behavioural_addiction, "&page=", page + 1)
  
  # Read and parse the current webpage:
  current_page_behavioural_addiction <- read_html(current_url_behavioural_addiction)
  
  # Extract article titles:
  titles <- current_page_behavioural_addiction %>%
    html_nodes(".docsum-title") %>%
    html_text(trim = TRUE)
  
  # Extract authors:
  author_info <- tryCatch({
    current_page_behavioural_addiction %>%
      html_nodes(".docsum-authors.short-authors") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting author info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Extract the journal and year
  journal_year_info <- tryCatch({
    current_page_behavioural_addiction %>%
      html_nodes(".docsum-journal-citation.short-journal-citation") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting journal info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Create a data frame with the extracted data:
  current_results_behavioural_addiction <- data.frame(Title = titles,
                                             Author_Info = author_info,
                                             Journal_Year_Info = journal_year_info)
  
  # Combine the current results with the previous results:
  results_behavioural_addiction <- rbind(results_behavioural_addiction, current_results_behavioural_addiction)
  
  # Add a delay to avoid overloading the server and getting booted off!
  sleep_time <- runif(1, min = 10, max = 20)
  Sys.sleep(sleep_time)
}

# Check outcomes:
print(length(titles))
print(length(author_info))
print(length(journal_year_info))

as_tibble(results_behavioural_addiction) %>% 
  print()

names(results_behavioural_addiction)

# Visually inspect results:
View(results_behavioural_addiction)

# Now save the initial results before proceeding so we can't lose them!
write.csv(results_behavioural_addiction, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/behavioural_addiction_data.csv", row.names=FALSE)

# Read written csv. file if required:
results_behavioural_addiction <- read.csv("behavioural_addiction_data.csv")

# Now let's explore the data and prepare it for analysis
# Total number returned:
count(results_behavioural_addiction)

# Any duplicates?
count(distinct(results_behavioural_addiction)) # Doesn't appear so

# Have a look at duplicates where the title, year, and authors are all the same:
results_behavioural_addiction %>% 
  group_by(Title, Journal_Year_Info, Author_Info) %>% 
  filter(n()>1) %>% print()
# None!

# Have a look at duplicates where just the title, journal and year are the same:
results_behavioural_addiction %>% 
  group_by(Title, Journal_Year_Info) %>% 
  filter(n()>1) %>% print() 
# There is two, but they are distinct.

# Have a look at duplicates where just the title is the same:
results_behavioural_addiction %>% 
  group_by(Title) %>% 
  filter(n()>1) %>% 
  print() 
# There is the same two, but they are distinct.
# No need to remove any duplicates

# Now separate the journal and date info:
results_behavioural_addiction_split<- as_tibble(results_behavioural_addiction) %>%
  separate(Journal_Year_Info, c("Journal", "Year"), sep = "\\. ") %>%
  print()

# We need to remove the period from the end of the year data:
Year_numeric_behavioural_addiction<- as.numeric(stringr::str_remove(results_behavioural_addiction_split$Year, "\\.$"))

# Now append our updated year column to to the main dataset, first checking the new and old dates match to create a cleam, useable dataset:
results_behavioural_addiction_clean<- results_behavioural_addiction_split %>% 
  bind_cols(Year_numeric_behavioural_addiction)  %>% 
  select(-Year) %>% 
  rename(Year = 4) # Checked dates matched before removal and finalisation


#  Okay, now we have a clean dataset, let's explore!
# Number of publications per year:
behavioural_addiction_hist<- hist(results_behavioural_addiction_clean$Year,
                         xlim = c(1960,2023),
                         breaks = 30,
                         main = "Papers published per year on Behavioural addiction Addiction (PubMed)",
                         xlab = "Year")

# Summary of the Year  variable:
summary(results_behavioural_addiction_clean$Year)

# Number of separate journals:
results_behavioural_addiction_clean %>% 
  distinct(Journal) %>% 
  arrange(Journal) %>% 
  print(n = 20)

# Most popular journals:
results_behavioural_addiction_clean %>% 
  group_by(Journal) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(desc(n)) %>%
  print(n = 20)

# Now I need to add a label to all of these studies to signify that they Were returned from the behavioural_addiction search so that we can distinguish them from other studies when we later joined the datasets together:
count(results_behavioural_addiction_clean) # 390 rows
Label_behavioural_addiction <- rep("behavioural_addiction", times = count(results_behavioural_addiction_clean))

results_behavioural_addiction_final <- results_behavioural_addiction_clean %>% bind_cols(Label_behavioural_addiction) %>%
  rename(Label = 5) %>%
  print()

# Now save the cleaned results
write.csv(results_behavioural_addiction_final, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/behavioural_addiction_data_cleaned.csv", row.names=FALSE)





























################# Social media/ network search 
# "disordered online social networking use" used here: https://pubmed.ncbi.nlm.nih.gov/25170590/
# "social-network-use disorder"[Title/Abstract] OR "social media addiction"[Title/Abstract] OR "social media dependence"[Title/Abstract] OR "social network addiction"[Title/Abstract] OR "social network dependence"[Title/Abstract] OR "Instagram addiction"[Title/Abstract] OR "Facebook addiction"[Title/Abstract] OR "social-network disorder"[Title/Abstract]

# social-network-use disorder is the term used in https://doi.org/10.1556/2006.2020.00035
# Not found so removed from search terms: "SnapChat addiction" AND "Twitter addiction"


# I was unsure about the results from this one at first so I manually inspected most of the results directly in the PubMed site. There were a few result we are the key search terms weren’t in the title or abstract, but they were in the keywords for the studies, which seems perfectly acceptable considering the types of studies we're trying to detect with this process.
# There was one study, however, that does not appear to be relevant based on my manual search and I will need to remove this later on. The title is: [Study protocol concerning the determining factors of physical and psychosocial destabilisation in the elderly]
# START OF ANALYSIS (performed on 11/05/2022)

#  Clean environment:
rm(list = ls())

# Define the URL:
url_social_media <- "https://pubmed.ncbi.nlm.nih.gov/?term=%22social-network-use+disorder%22%5BTitle%2FAbstract%5D+OR+%22social+media+addiction%22%5BTitle%2FAbstract%5D+OR+%22social+media+dependence%22%5BTitle%2FAbstract%5D+OR+%22social+network+addiction%22%5BTitle%2FAbstract%5D+OR+%22social+network+dependence%22%5BTitle%2FAbstract%5D+OR+%22Instagram+addiction%22%5BTitle%2FAbstract%5D+OR+%22Facebook+addiction%22%5BTitle%2FAbstract%5D+OR+%22social-network+disorder%22%5BTitle%2FAbstract%5D+OR+%22disordered+online+social+networking+use%22%5BTitle%2FAbstract%5D+&size=200"
webpage_social_media <- read_html(url_social_media)

# Get the total number of search results:
results_count_social_media <- webpage_social_media %>%
  html_node(".results-amount .value") %>%
  html_text() %>%
  str_replace(",", "") %>%
  as.numeric()

# Calculate the number of pages:
results_per_page <- 10
total_pages_social_media <- ceiling(results_count_social_media[1]/ results_per_page)
# Print results_count and total_pages:
print(results_count_social_media)
print(total_pages_social_media)


# Make an empty data frame:
results_social_media <- data.frame()

# Loop through each page and scrape the data:
for (page in 0:(total_pages_social_media - 1)) {
  # Update the page parameter in the URL for the current page:
  current_url_social_media <- paste0(url_social_media, "&page=", page + 1)
  
  # Read and parse the current webpage:
  current_page_social_media <- read_html(current_url_social_media)
  
  # Extract article titles:
  titles <- current_page_social_media %>%
    html_nodes(".docsum-title") %>%
    html_text(trim = TRUE)
  
  # Extract authors:
  author_info <- tryCatch({
    current_page_social_media %>%
      html_nodes(".docsum-authors.short-authors") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting author info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Extract the journal and year
  journal_year_info <- tryCatch({
    current_page_social_media %>%
      html_nodes(".docsum-journal-citation.short-journal-citation") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting journal info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Create a data frame with the extracted data:
  current_results_social_media <- data.frame(Title = titles,
                                         Author_Info = author_info,
                                         Journal_Year_Info = journal_year_info)
  
  # Combine the current results with the previous results:
  results_social_media <- rbind(results_social_media, current_results_social_media)
  
  # Add a delay to avoid overloading the server and getting booted off!
  sleep_time <- runif(1, min = 10, max = 20)
  Sys.sleep(sleep_time)
}

# Check outcomes:
print(length(titles))
print(length(author_info))
print(length(journal_year_info))

as_tibble(results_social_media) %>% 
  print()

names(results_social_media)

# Visually inspect results:
View(results_social_media)

# Now save the initial results before proceeding so we can't lose them!
write.csv(results_social_media, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/social_media_data.csv", row.names=FALSE)

# Read written csv. file if required:
results_social_media <- read.csv("social_media_data.csv")

# Now let's explore the data and prepare it for analysis
# Total number returned:
count(results_social_media)

# Any duplicates?
count(distinct(results_social_media)) # Doesn't appear so

# Have a look at duplicates where the title, year, and authors are all the same:
results_social_media %>% 
  group_by(Title, Journal_Year_Info, Author_Info) %>% 
  filter(n()>1) %>% print()
# None!

# Have a look at duplicates where just the title, journal and year are the same:
results_social_media %>% 
  group_by(Title, Journal_Year_Info) %>% 
  filter(n()>1) %>% print() 
# None!

# Have a look at duplicates where just the title is the same:
results_social_media %>% 
  group_by(Title) %>% 
  filter(n()>1) %>% 
  print() 
# There is two, but they are distinct.
# No need to remove any duplicates

# Now separate the journal and date info:
results_social_media_split<- as_tibble(results_social_media) %>%
  separate(Journal_Year_Info, c("Journal", "Year"), sep = "\\. ") %>%
  print()

# We need to remove the period from the end of the year data:
Year_numeric_social_media<- as.numeric(stringr::str_remove(results_social_media_split$Year, "\\.$"))

# Now append our updated year column to to the main dataset, first checking the new and old dates match to create a cleam, useable dataset:
results_social_media_clean<- results_social_media_split %>% 
  bind_cols(Year_numeric_social_media)  %>% 
  select(-Year) %>% 
  rename(Year = 4) %>% # Checked dates matched before removal and finalisation
  filter(Title != "[Study protocol concerning the determining factors of physical and psychosocial destabilisation in the elderly].") # Let's also remove our 1 irrelevant study now before proceeding

#  Okay, now we have a clean dataset, let's explore!
# Number of publications per year:
social_media_hist<- hist(results_social_media_clean$Year,
                     xlim = c(1960,2023),
                     breaks = 10,
                     main = "Papers published per year on Social Media Addiction (PubMed)",
                     xlab = "Year")

# Summary of the Year  variable:
summary(results_social_media_clean$Year)

# Number of separate journals:
results_social_media_clean %>% 
  distinct(Journal) %>% 
  arrange(Journal) %>% 
  print(n = 20)

# Most popular journals:
results_social_media_clean %>% 
  group_by(Journal) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(desc(n)) %>%
  print(n = 20)

# Now I need to add a label to all of these studies to signify that they Were returned from the social_media search so that we can distinguish them from other studies when we later joined the datasets together:
count(results_social_media_clean) # 390 rows
Label_social_media <- rep("social_media", times = count(results_social_media_clean))

results_social_media_final <- results_social_media_clean %>% bind_cols(Label_social_media) %>%
  rename(Label = 5) %>%
  print()

# Now save the cleaned results
write.csv(results_social_media_final, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/social_media_data_cleaned.csv", row.names=FALSE)















#################  Internet addiction search
# MeSH terms: https://www.ncbi.nlm.nih.gov/mesh/?term=internet+addiction
# ((("internet addiction"[Title/Abstract]) OR ("addictive internet use"[Title/Abstract])) OR ("internet use disorder"[Title/Abstract])) OR ("addiction to the internet"[Title/Abstract])

# Again, I manually inspected most of the results directly in the PubMed site. All studies appear to be relevant.

# START OF ANALYSIS (performed on 15/05/2022)

#  Clean environment:
rm(list = ls())

# Define the URL:
url_internet <- "https://pubmed.ncbi.nlm.nih.gov/?term=(((%22internet%20addiction%22%5BTitle%2FAbstract%5D)%20OR%20(%22addictive%20internet%20use%22%5BTitle%2FAbstract%5D))%20OR%20(%22internet%20use%20disorder%22%5BTitle%2FAbstract%5D))%20OR%20(%22addiction%20to%20the%20internet%22%5BTitle%2FAbstract%5D)&sort="
webpage_internet <- read_html(url_internet)

# Get the total number of search results:
results_count_internet <- webpage_internet %>%
  html_node(".results-amount .value") %>%
  html_text() %>%
  str_replace(",", "") %>%
  as.numeric()

# Calculate the number of pages:
results_per_page <- 10
total_pages_internet <- ceiling(results_count_internet[1]/ results_per_page)
# Print results_count and total_pages:
print(results_count_internet)
print(total_pages_internet)


# Make an empty data frame:
results_internet <- data.frame()

# Loop through each page and scrape the data:
for (page in 0:(total_pages_internet - 1)) {
  # Update the page parameter in the URL for the current page:
  current_url_internet <- paste0(url_internet, "&page=", page + 1)
  
  # Read and parse the current webpage:
  current_page_internet <- read_html(current_url_internet)
  
  # Extract article titles:
  titles <- current_page_internet %>%
    html_nodes(".docsum-title") %>%
    html_text(trim = TRUE)
  
  # Extract authors:
  author_info <- tryCatch({
    current_page_internet %>%
      html_nodes(".docsum-authors.short-authors") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting author info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Extract the journal and year
  journal_year_info <- tryCatch({
    current_page_internet %>%
      html_nodes(".docsum-journal-citation.short-journal-citation") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting journal info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Create a data frame with the extracted data:
  current_results_internet <- data.frame(Title = titles,
                                             Author_Info = author_info,
                                             Journal_Year_Info = journal_year_info)
  
  # Combine the current results with the previous results:
  results_internet <- rbind(results_internet, current_results_internet)
  
  # Add a delay to avoid overloading the server and getting booted off!
  sleep_time <- runif(1, min = 10, max = 20)
  Sys.sleep(sleep_time)
}

# Check outcomes:
print(length(titles))
print(length(author_info))
print(length(journal_year_info))

as_tibble(results_internet) %>% 
  print()

names(results_internet)

# Visually inspect results:
View(results_internet)

# Now save the initial results before proceeding so we can't lose them!
write.csv(results_internet, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/internet_data.csv", row.names=FALSE)

# Read written csv. file if required:
results_internet <- read.csv("internet_data.csv")

# Now let's explore the data and prepare it for analysis
# Total number returned:
count(results_internet)

# Any duplicates?
count(distinct(results_internet)) # 1

# Have a look at duplicates where the title, year, and authors are all the same:
results_internet %>% 
  group_by(Title, Journal_Year_Info, Author_Info) %>% 
  filter(n()>1) %>% print()
# There's one study which is duplicated, and it appears to be because there is a corrected version, so I will delete one of the entries. Here's the study link: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7113173/
# "The Current Situation of Internet Addiction and Its Impact on Sleep Quality and Self-Injury Behavior in Chinese Medical Students."

# Have a look at duplicates where just the title, journal and year are the same:
results_internet %>% 
  group_by(Title, Journal_Year_Info) %>% 
  filter(n()>1) %>% print() 
# Same as above

# Have a look at duplicates where just the title is the same:
results_internet %>% 
  group_by(Title) %>% 
  filter(n()>1) %>% 
  print() 
# There is three, but one is the correction mentioned above and the other two are distinct.

# Now separate the journal and date info:
results_internet_split<- as_tibble(results_internet) %>%
  separate(Journal_Year_Info, c("Journal", "Year"), sep = "\\. ") %>%
  print()

# We need to remove the period from the end of the year data:
Year_numeric_internet<- as.numeric(stringr::str_remove(results_internet_split$Year, "\\.$"))

# Now append our updated year column to to the main dataset, first checking the new and old dates match to create a cleam, useable dataset:
results_internet_clean<- results_internet_split %>% 
  bind_cols(Year_numeric_internet)  %>% 
  select(-Year) %>% 
  rename(Year = 4) %>% # Checked dates matched before removal and finalisation
  distinct() # Let's also remove our 1 irrelevant study now before proceeding

#  Okay, now we have a clean dataset, let's explore!
# Number of publications per year:
internet_hist<- hist(results_internet_clean$Year,
                         xlim = c(1960,2023),
                         breaks = 30,
                         main = "Papers published per year on Internet Addiction (PubMed)",
                         xlab = "Year")

# Summary of the Year  variable:
summary(results_internet_clean$Year)

# There are two NA values. let's take a look at them:
results_internet_clean %>% filter(is.na(Year))
# Both appear to be "StatPearls"  and are not peer-reviewed papers. Remove:
results_internet_clean2 <- results_internet_clean %>% 
  filter(Title != "Drug Addiction." &  
        Title != "Impulse Control Disorders.")

# Check this has removed the two entries:
count(results_internet_clean) - count(results_internet_clean2) #  Yarp

# Number of separate journals:
results_internet_clean2 %>% 
  distinct(Journal) %>% 
  arrange(Journal) %>% 
  print(n = 20)

# Most popular journals:
results_internet_clean2 %>% 
  group_by(Journal) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(desc(n)) %>%
  print(n = 20)

# Now I need to add a label to all of these studies to signify that they Were returned from the internet search so that we can distinguish them from other studies when we later joined the datasets together:
count(results_internet_clean2)
Label_internet <- rep("internet", times = count(results_internet_clean2))

results_internet_final <- results_internet_clean2 %>% bind_cols(Label_internet) %>%
  rename(Label = 5) %>%
  print()

# Now save the cleaned results
write.csv(results_internet_final, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/internet_data_cleaned.csv", row.names=FALSE)









#################  Smartphone addiction search
# ("smartphone addiction"[Title/Abstract]) OR ("mobile phone addiction"[Title/Abstract]) OR ("smartphone dependence"[Title/Abstract]) OR ("mobile phone dependence"[Title/Abstract])

# Again, I manually inspected most of the results directly in the PubMed site. All studies appear to be relevant.

# START OF ANALYSIS (performed on 15/05/2022)

#  Clean environment:
rm(list = ls())

# Define the URL:
url_smartphone <- "https://pubmed.ncbi.nlm.nih.gov/?term=(%22smartphone%20addiction%22%5BTitle%2FAbstract%5D)%20OR%20(%22mobile%20phone%20addiction%22%5BTitle%2FAbstract%5D)%20OR%20(%22smartphone%20dependence%22%5BTitle%2FAbstract%5D)%20OR%20(%22mobile%20phone%20dependence%22%5BTitle%2FAbstract%5D)&size=200"
webpage_smartphone <- read_html(url_smartphone)

# Get the total number of search results:
results_count_smartphone <- webpage_smartphone %>%
  html_node(".results-amount .value") %>%
  html_text() %>%
  str_replace(",", "") %>%
  as.numeric()

# Calculate the number of pages:
results_per_page <- 200
total_pages_smartphone <- ceiling(results_count_smartphone[1]/ results_per_page)
# Print results_count and total_pages:
print(results_count_smartphone)
print(total_pages_smartphone)


# Make an empty data frame:
results_smartphone <- data.frame()

# Loop through each page and scrape the data:
for (page in 0:(total_pages_smartphone - 1)) {
  # Update the page parameter in the URL for the current page:
  current_url_smartphone <- paste0(url_smartphone, "&page=", page + 1)
  
  # Read and parse the current webpage:
  current_page_smartphone <- read_html(current_url_smartphone)
  
  # Extract article titles:
  titles <- current_page_smartphone %>%
    html_nodes(".docsum-title") %>%
    html_text(trim = TRUE)
  
  # Extract authors:
  author_info <- tryCatch({
    current_page_smartphone %>%
      html_nodes(".docsum-authors.short-authors") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting author info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Extract the journal and year
  journal_year_info <- tryCatch({
    current_page_smartphone %>%
      html_nodes(".docsum-journal-citation.short-journal-citation") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting journal info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Create a data frame with the extracted data:
  current_results_smartphone <- data.frame(Title = titles,
                                         Author_Info = author_info,
                                         Journal_Year_Info = journal_year_info)
  
  # Combine the current results with the previous results:
  results_smartphone <- rbind(results_smartphone, current_results_smartphone)
  
  # Add a delay to avoid overloading the server and getting booted off!
  sleep_time <- runif(1, min = 10, max = 20)
  Sys.sleep(sleep_time)
}

# Check outcomes:
print(length(titles))
print(length(author_info))
print(length(journal_year_info))

as_tibble(results_smartphone) %>% 
  print()

names(results_smartphone)

# Visually inspect results:
View(results_smartphone)

# Now save the initial results before proceeding so we can't lose them!
write.csv(results_smartphone, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/smartphone_data.csv", row.names=FALSE)

# Read written csv. file if required:
results_smartphone <- read.csv("smartphone_data.csv")

# Now let's explore the data and prepare it for analysis
# Total number returned:
count(results_smartphone)

# Any duplicates?
count(distinct(results_smartphone)) # Doesn't appear so

# Have a look at duplicates where the title, year, and authors are all the same:
results_smartphone %>% 
  group_by(Title, Journal_Year_Info, Author_Info) %>% 
  filter(n()>1) %>% print()
# None!

# Have a look at duplicates where just the title, journal and year are the same:
results_smartphone %>% 
  group_by(Title, Journal_Year_Info) %>% 
  filter(n()>1) %>% print() 
Well# Same as above

# Have a look at duplicates where just the title is the same:
results_smartphone %>% 
  group_by(Title) %>% 
  filter(n()>1) %>% 
  print() 
# None. No need to remove any duplicates

# Now separate the journal and date info:
results_smartphone_split<- as_tibble(results_smartphone) %>%
  separate(Journal_Year_Info, c("Journal", "Year"), sep = "\\. ") %>%
  print()

# We need to remove the period from the end of the year data:
Year_numeric_smartphone<- as.numeric(stringr::str_remove(results_smartphone_split$Year, "\\.$"))

# Now append our updated year column to to the main dataset, first checking the new and old dates match to create a cleam, useable dataset:
results_smartphone_clean<- results_smartphone_split %>% 
  bind_cols(Year_numeric_smartphone)  %>% 
  select(-Year) %>% 
  rename(Year = 4) # Checked dates matched before removal and finalisation


#  Okay, now we have a clean dataset, let's explore!
# Number of publications per year:
smartphone_hist<- hist(results_smartphone_clean$Year,
                     xlim = c(1960,2023),
                     breaks = 15,
                     main = "Papers published per year on smartphone Addiction (PubMed)",
                     xlab = "Year")

# Summary of the Year  variable:
summary(results_smartphone_clean$Year)

# There is 1 NA value. let's take a look at them:
results_smartphone_clean %>% filter(is.na(Year))
# It's another "StatPearl" which is not a not peer-reviewed paper. Remove:
results_smartphone_clean2 <- results_smartphone_clean %>% 
  filter(Title != "Drug Addiction.")

# Check this has removed the entry:
count(results_smartphone_clean) - count(results_smartphone_clean2) #  Yarp

# Number of separate journals:
results_smartphone_clean2 %>% 
  distinct(Journal) %>% 
  arrange(Journal) %>% 
  print(n = 20)

# Most popular journals:
results_smartphone_clean2 %>% 
  group_by(Journal) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(desc(n)) %>%
  print(n = 20)

# Now I need to add a label to all of these studies to signify that they Were returned from the smartphone search so that we can distinguish them from other studies when we later joined the datasets together:
count(results_smartphone_clean2) 
Label_smartphone <- rep("smartphone", times = count(results_smartphone_clean2))

results_smartphone_final <- results_smartphone_clean2 %>% bind_cols(Label_smartphone) %>%
  rename(Label = 5) %>%
  print()

# Now save the cleaned results
write.csv(results_smartphone_final, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/smartphone_data_cleaned.csv", row.names=FALSE)












################# Shopping search
# Buying-shopping disorder is the term used in https://doi.org/10.1556/2006.2020.00035
# "Pathological buying" used here: https://pubmed.ncbi.nlm.nih.gov/25393125/
# None of these returned any results: "pathological shopping", "addiction to shopping", "addiction to buying", "shopping dependence", "buying dependence"


# ("buying-shopping disorder"[Title/Abstract]) OR ("shopping addiction"[Title/Abstract]) OR ("buying addiction"[Title/Abstract])
# Again, I manually inspected most of the results directly in the PubMed site. All studies appear to be relevant.

# START OF ANALYSIS (performed on 15/05/2022)

#  Clean environment:
rm(list = ls())

# Define the URL:
url_shopping <- "https://pubmed.ncbi.nlm.nih.gov/?term=%28%22buying-shopping+disorder%22%5BTitle%2FAbstract%5D%29+OR+%28%22shopping+addiction%22%5BTitle%2FAbstract%5D%29+OR+%28%22buying+addiction%22%5BTitle%2FAbstract%5D%29&size=200"
webpage_shopping <- read_html(url_shopping)

# Get the total number of search results:
results_count_shopping <- webpage_shopping %>%
  html_node(".results-amount .value") %>%
  html_text() %>%
  str_replace(",", "") %>%
  as.numeric()

# Calculate the number of pages:
results_per_page <- 200
total_pages_shopping <- ceiling(results_count_shopping[1]/ results_per_page)
# Print results_count and total_pages:
print(results_count_shopping)
print(total_pages_shopping)


# Make an empty data frame:
results_shopping <- data.frame()

# Loop through each page and scrape the data:
for (page in 0:(total_pages_shopping - 1)) {
  # Update the page parameter in the URL for the current page:
  current_url_shopping <- paste0(url_shopping, "&page=", page + 1)
  
  # Read and parse the current webpage:
  current_page_shopping <- read_html(current_url_shopping)
  
  # Extract article titles:
  titles <- current_page_shopping %>%
    html_nodes(".docsum-title") %>%
    html_text(trim = TRUE)
  
  # Extract authors:
  author_info <- tryCatch({
    current_page_shopping %>%
      html_nodes(".docsum-authors.short-authors") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting author info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Extract the journal and year
  journal_year_info <- tryCatch({
    current_page_shopping %>%
      html_nodes(".docsum-journal-citation.short-journal-citation") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting journal info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Create a data frame with the extracted data:
  current_results_shopping <- data.frame(Title = titles,
                                           Author_Info = author_info,
                                           Journal_Year_Info = journal_year_info)
  
  # Combine the current results with the previous results:
  results_shopping <- rbind(results_shopping, current_results_shopping)
  
  # Add a delay to avoid overloading the server and getting booted off!
  sleep_time <- runif(1, min = 10, max = 20)
  Sys.sleep(sleep_time)
}

# Check outcomes:
print(length(titles))
print(length(author_info))
print(length(journal_year_info))

as_tibble(results_shopping) %>% 
  print()

names(results_shopping)

# Visually inspect results:
View(results_shopping)

# Now save the initial results before proceeding so we can't lose them!
write.csv(results_shopping, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/shopping_data.csv", row.names=FALSE)

# Read written csv. file if required:
results_shopping <- read.csv("shopping_data.csv")

# Now let's explore the data and prepare it for analysis
# Total number returned:
count(results_shopping)

# Any duplicates?
count(distinct(results_shopping)) # Doesn't appear so

# Have a look at duplicates where the title, year, and authors are all the same:
results_shopping %>% 
  group_by(Title, Journal_Year_Info, Author_Info) %>% 
  filter(n()>1) %>% print()
# None!

# Have a look at duplicates where just the title, journal and year are the same:
results_shopping %>% 
  group_by(Title, Journal_Year_Info) %>% 
  filter(n()>1) %>% print() 
# Same as above

# Have a look at duplicates where just the title is the same:
results_shopping %>% 
  group_by(Title) %>% 
  filter(n()>1) %>% 
  print() 
# One duplicate but I've come across this before and it is genuinely two separate papers with the same name by the same authors.
# No need to remove duplicates

# Now separate the journal and date info:
results_shopping_split<- as_tibble(results_shopping) %>%
  separate(Journal_Year_Info, c("Journal", "Year"), sep = "\\. ") %>%
  print()

# We need to remove the period from the end of the year data:
Year_numeric_shopping<- as.numeric(stringr::str_remove(results_shopping_split$Year, "\\.$"))

# Now append our updated year column to to the main dataset, first checking the new and old dates match to create a cleam, useable dataset:
results_shopping_clean<- results_shopping_split %>% 
  bind_cols(Year_numeric_shopping)  %>% 
  select(-Year) %>% 
  rename(Year = 4) # Checked dates matched before removal and finalisation


#  Okay, now we have a clean dataset, let's explore!
# Number of publications per year:
shopping_hist<- hist(results_shopping_clean$Year,
                       xlim = c(1960,2023),
                       breaks = 15,
                       main = "Papers published per year on shopping Addiction (PubMed)",
                       xlab = "Year")

# Summary of the Year  variable:
summary(results_shopping_clean$Year)

# Number of separate journals:
results_shopping_clean %>% 
  distinct(Journal) %>% 
  arrange(Journal) %>% 
  print(n = 20)

# Most popular journals:
results_shopping_clean %>% 
  group_by(Journal) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(desc(n)) %>%
  print(n = 20)

# Now I need to add a label to all of these studies to signify that they Were returned from the shopping search so that we can distinguish them from other studies when we later joined the datasets together:
count(results_shopping_clean) 
Label_shopping <- rep("shopping", times = count(results_shopping_clean))

results_shopping_final <- results_shopping_clean %>% bind_cols(Label_shopping) %>%
  rename(Label = 5) %>%
  print()

# Now save the cleaned results
write.csv(results_shopping_final, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/shopping_data_cleaned.csv", row.names=FALSE)














################# Pornography search
# Pornography-use disorder is the term used in https://doi.org/10.1556/2006.2020.00035
# Cybersex addiction seems to refer to a more broad focus on internet-related sexual stimulation, but it seems like searching the two seperately returns the same results so I have merged them here. People either use the term interchangeably or include both phrases then their keywords.
# None of these returned any results: "pornography dependence", "addiction to pornography", "cybersex dependence"

# (("pornography-use disorder"[Title/Abstract]) OR ("pornography addiction"[Title/Abstract]) OR ("cybersex addiction"[Title/Abstract])) OR ("porn addiction")
# Again, I manually inspected all of the results directly in the PubMed site. All studies appear to be relevant.

# START OF ANALYSIS (performed on 15/05/2022)

#  Clean environment:
rm(list = ls())

# Define the URL:
url_pornography <- "https://pubmed.ncbi.nlm.nih.gov/?term=%28%28%22pornography-use+disorder%22%5BTitle%2FAbstract%5D%29+OR+%28%22pornography+addiction%22%5BTitle%2FAbstract%5D%29+OR+%28%22cybersex+addiction%22%5BTitle%2FAbstract%5D%29%29+OR+%28%22porn+addiction%22%29&sort="
webpage_pornography <- read_html(url_pornography)

# Get the total number of search results:
results_count_pornography <- webpage_pornography %>%
  html_node(".results-amount .value") %>%
  html_text() %>%
  str_replace(",", "") %>%
  as.numeric()

# Calculate the number of pages:
results_per_page <- 10
total_pages_pornography <- ceiling(results_count_pornography[1]/ results_per_page)
# Print results_count and total_pages:
print(results_count_pornography)
print(total_pages_pornography)


# Make an empty data frame:
results_pornography <- data.frame()

# Loop through each page and scrape the data:
for (page in 0:(total_pages_pornography - 1)) {
  # Update the page parameter in the URL for the current page:
  current_url_pornography <- paste0(url_pornography, "&page=", page + 1)
  
  # Read and parse the current webpage:
  current_page_pornography <- read_html(current_url_pornography)
  
  # Extract article titles:
  titles <- current_page_pornography %>%
    html_nodes(".docsum-title") %>%
    html_text(trim = TRUE)
  
  # Extract authors:
  author_info <- tryCatch({
    current_page_pornography %>%
      html_nodes(".docsum-authors.short-authors") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting author info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Extract the journal and year
  journal_year_info <- tryCatch({
    current_page_pornography %>%
      html_nodes(".docsum-journal-citation.short-journal-citation") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting journal info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Create a data frame with the extracted data:
  current_results_pornography <- data.frame(Title = titles,
                                         Author_Info = author_info,
                                         Journal_Year_Info = journal_year_info)
  
  # Combine the current results with the previous results:
  results_pornography <- rbind(results_pornography, current_results_pornography)
  
  # Add a delay to avoid overloading the server and getting booted off!
  sleep_time <- runif(1, min = 10, max = 20)
  Sys.sleep(sleep_time)
}

# Check outcomes:
print(length(titles))
print(length(author_info))
print(length(journal_year_info))

as_tibble(results_pornography) %>% 
  print()

names(results_pornography)

# Visually inspect results:
View(results_pornography)

# Now save the initial results before proceeding so we can't lose them!
write.csv(results_pornography, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/pornography_data.csv", row.names=FALSE)

# Read written csv. file if required:
results_pornography <- read.csv("pornography_data.csv")

# Now let's explore the data and prepare it for analysis
# Total number returned:
count(results_pornography)

# Any duplicates?
count(distinct(results_pornography)) # 1

# Have a look at duplicates where the title, year, and authors are all the same:
results_pornography %>% 
  group_by(Title, Journal_Year_Info, Author_Info) %>% 
  filter(n()>1) %>% print()
# One duplicate. I've checked this and it genuinely is a duplicate that will need to be removed. The title is: "Pornography Addiction: An Exploration of the Association Between Use, Perceived Addiction, Erectile Dysfunction, Premature (Early) Ejaculation, and Sexual Satisfaction in Males Aged 18-44 Years."

# Have a look at duplicates where just the title, journal and year are the same:
results_pornography %>% 
  group_by(Title, Journal_Year_Info) %>% 
  filter(n()>1) %>% print() 
# Same as above

# Have a look at duplicates where just the title is the same:
results_pornography %>% 
  group_by(Title) %>% 
  filter(n()>1) %>% 
  print() 
# Again, just the one above.

# Now separate the journal and date info:
results_pornography_split<- as_tibble(results_pornography) %>%
  separate(Journal_Year_Info, c("Journal", "Year"), sep = "\\. ") %>%
  print()

# We need to remove the period from the end of the year data:
Year_numeric_pornography<- as.numeric(stringr::str_remove(results_pornography_split$Year, "\\.$"))

# Now append our updated year column to to the main dataset, first checking the new and old dates match to create a cleam, useable dataset:
results_pornography_clean<- results_pornography_split %>% 
  bind_cols(Year_numeric_pornography)  %>% 
  select(-Year) %>% 
  rename(Year = 4) %>% # Checked dates matched before removal and finalisation
  distinct()

#  Okay, now we have a clean dataset, let's explore!
# Number of publications per year:
pornography_hist<- hist(results_pornography_clean$Year,
                     xlim = c(1960,2023),
                     breaks = 15,
                     main = "Papers published per year on Pornography Addiction (PubMed)",
                     xlab = "Year")

# Summary of the Year  variable:
summary(results_pornography_clean$Year)

# Number of separate journals:
results_pornography_clean %>% 
  distinct(Journal) %>% 
  arrange(Journal) %>% 
  print(n = 20)

# Most popular journals:
results_pornography_clean %>% 
  group_by(Journal) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(desc(n)) %>%
  print(n = 20)

# Now I need to add a label to all of these studies to signify that they Were returned from the pornography search so that we can distinguish them from other studies when we later joined the datasets together:
count(results_pornography_clean) 
Label_pornography <- rep("pornography", times = count(results_pornography_clean))

results_pornography_final <- results_pornography_clean %>% bind_cols(Label_pornography) %>%
  rename(Label = 5) %>%
  print()

# Now save the cleaned results
write.csv(results_pornography_final, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/pornography_data_cleaned.csv", row.names=FALSE)














################# Sex search
# "Sexual addicts" [https://www.sciencedirect.com/science/article/abs/pii/S0272735820301136; https://pubmed.ncbi.nlm.nih.gov/27774812/]
# Didn't return any results: "addicted to sex"

# ("sexual addiction"[Title/Abstract]) OR ("sex addiction"[Title/Abstract])
# 
# Originally included “sex dependence” as a search term, but it didn’t return anything related to sexual addiction and returned lots of studies that said the expression/presence of something e.g., a disease) was dependent on sex. See below examples:
# - https://pubmed.ncbi.nlm.nih.gov/28777306/
# - https://pubmed.ncbi.nlm.nih.gov/35194594/

# Again, I manually inspected all of the results directly in the PubMed site.
# The manual search of results revealed that there were multiple irrelevant studies returned here. This is because there were several studies that listed sex and then addiction as variables of interest like “…, sex, addiction, …”. Here’s the full list of irrelevant studies:
#   
# - The SCOPE study: health-care consumption related to patients with chronic obstructive pulmonary disease in France
# - Correlation of addictive factors, human papilloma virus infection and histopathology of oral submucous fibrosis
# - Serum malondialdehyde level: Surrogate stress marker in the Sikkimese diabetics
# - Design in topographical space of peptide and peptidomimetic ligands that affect behavior. A chemist's glimpse at the mind--body problem
# - Tricyclic antidepressants intoxication in Tehran, Iran: epidemiology and associated factors
# - Biochemical aspects of effects of mesenchymal stem cell treatment in chronic wounds progressive healing

# START OF ANALYSIS (performed on 15/05/2022)

#  Clean environment:
rm(list = ls())

# Define the URL:
url_sex <- "https://pubmed.ncbi.nlm.nih.gov/?term=(%22sexual%20addiction%22%5BTitle%2FAbstract%5D)%20OR%20(%22sex%20addiction%22%5BTitle%2FAbstract%5D)&size=200"
webpage_sex <- read_html(url_sex)

# Get the total number of search results:
results_count_sex <- webpage_sex %>%
  html_node(".results-amount .value") %>%
  html_text() %>%
  str_replace(",", "") %>%
  as.numeric()

# Calculate the number of pages:
results_per_page <- 10
total_pages_sex <- ceiling(results_count_sex[1]/ results_per_page)
# Print results_count and total_pages:
print(results_count_sex)
print(total_pages_sex)


# Make an empty data frame:
results_sex <- data.frame()

# Loop through each page and scrape the data:
for (page in 0:(total_pages_sex - 1)) {
  # Update the page parameter in the URL for the current page:
  current_url_sex <- paste0(url_sex, "&page=", page + 1)
  
  # Read and parse the current webpage:
  current_page_sex <- read_html(current_url_sex)
  
  # Extract article titles:
  titles <- current_page_sex %>%
    html_nodes(".docsum-title") %>%
    html_text(trim = TRUE)
  
  # Extract authors:
  author_info <- tryCatch({
    current_page_sex %>%
      html_nodes(".docsum-authors.short-authors") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting author info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Extract the journal and year
  journal_year_info <- tryCatch({
    current_page_sex %>%
      html_nodes(".docsum-journal-citation.short-journal-citation") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting journal info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Create a data frame with the extracted data:
  current_results_sex <- data.frame(Title = titles,
                                            Author_Info = author_info,
                                            Journal_Year_Info = journal_year_info)
  
  # Combine the current results with the previous results:
  results_sex <- rbind(results_sex, current_results_sex)
  
  # Add a delay to avoid overloading the server and getting booted off!
  sleep_time <- runif(1, min = 10, max = 20)
  Sys.sleep(sleep_time)
}

# Check outcomes:
print(length(titles))
print(length(author_info))
print(length(journal_year_info))

as_tibble(results_sex) %>% 
  print()

names(results_sex)

# Visually inspect results:
View(results_sex)

# Now save the initial results before proceeding so we can't lose them!
write.csv(results_sex, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/sex_data.csv", row.names=FALSE)

# Read written csv. file if required:
results_sex <- read.csv("sex_data.csv")

# Now let's explore the data and prepare it for analysis
# Total number returned:
count(results_sex)

# Any duplicates?
count(distinct(results_sex)) # 2

# Have a look at duplicates where the title, year, and authors are all the same:
results_sex %>% 
  group_by(Title, Journal_Year_Info, Author_Info) %>% 
  filter(n()>1) %>% print()
# There are two duplicates.  These seem to be genuine duplicates that require removal. Here are the titles:
#  - Estimated Prevalence and Demographic Correlates of Compulsive Sexual Behavior Among Gay Men in the United States.
#  - What is Normal Pornography Use in a Highly Religious Area? Exploring Patterns of Pornography Use in Utah.

# Have a look at duplicates where just the title, journal and year are the same:
results_sex %>% 
  group_by(Title, Journal_Year_Info) %>% 
  filter(n()>1) %>% print() 
# Same as above

# Have a look at duplicates where just the title is the same:
results_sex %>% 
  group_by(Title) %>% 
  filter(n()>1) %>% 
  print() 
# Again, just the one above.

# Now separate the journal and date info:
results_sex_split<- as_tibble(results_sex) %>%
  separate(Journal_Year_Info, c("Journal", "Year"), sep = "\\. ") %>%
  print()

# We need to remove the period from the end of the year data:
Year_numeric_sex<- as.numeric(stringr::str_remove(results_sex_split$Year, "\\.$"))

# Now append our updated year column to to the main dataset, first checking the new and old dates match to create a cleam, useable dataset:
results_sex_clean<- results_sex_split %>% 
  bind_cols(Year_numeric_sex)  %>% 
  select(-Year) %>% 
  rename(Year = 4) %>% # Checked dates matched before removal and finalisation
  distinct()

#  Okay, now we have a clean dataset, let's explore!
# Number of publications per year:
sex_hist<- hist(results_sex_clean$Year,
                        xlim = c(1960,2023),
                        breaks = 30,
                        main = "Papers published per year on Sex Addiction (PubMed)",
                        xlab = "Year")

# Summary of the Year  variable:
summary(results_sex_clean$Year)

# There is 1 NA value. let's take a look at them:
results_sex_clean %>% filter(is.na(Year))
# It's another "StatPearl" which is not a not peer-reviewed paper. Remove:
results_sex_clean2 <- results_sex_clean %>% 
  filter(Title != "Drug Addiction.")

# Check this has removed the entry:
count(results_sex_clean) - count(results_sex_clean2) #  Yarp

# Number of separate journals:
results_sex_clean2 %>% 
  distinct(Journal) %>% 
  arrange(Journal) %>% 
  print(n = 20)

# Most popular journals:
results_sex_clean2 %>% 
  group_by(Journal) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(desc(n)) %>%
  print(n = 20)

# Now I need to add a label to all of these studies to signify that they Were returned from the sex search so that we can distinguish them from other studies when we later joined the datasets together:
count(results_sex_clean2) 
Label_sex <- rep("sex", times = count(results_sex_clean2))

results_sex_final <- results_sex_clean2 %>% bind_cols(Label_sex) %>%
  rename(Label = 5) %>%
  print()

# Now save the cleaned results
write.csv(results_sex_final, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/sex_data_cleaned.csv", row.names=FALSE)









################# Work addiction search
# "Work addiction" [https://pubmed.ncbi.nlm.nih.gov/30920291/]
# "workaholism" [https://pubmed.ncbi.nlm.nih.gov/28425778/] '

# (("work addiction"[Title/Abstract]) OR ("addiction to work"[Title/Abstract])) OR ("workaholism"[Title/Abstract])
# Again, I manually inspected all of the results directly in the PubMed site. All appeared relevant.

# START OF ANALYSIS (performed on 16/05/2022)
#  Clean environment:
rm(list = ls())

# Define the URL:
url_work <- "https://pubmed.ncbi.nlm.nih.gov/?term=%28%28%22work+addiction%22%5BTitle%2FAbstract%5D%29+OR+%28%22addiction+to+work%22%5BTitle%2FAbstract%5D%29%29+OR+%28%22workaholism%22%5BTitle%2FAbstract%5D%29&size=200"
webpage_work <- read_html(url_work)

# Get the total number of search results:
results_count_work <- webpage_work %>%
  html_node(".results-amount .value") %>%
  html_text() %>%
  str_replace(",", "") %>%
  as.numeric()

# Calculate the number of pages:
results_per_page <- 10
total_pages_work <- ceiling(results_count_work[1]/ results_per_page)
# Print results_count and total_pages:
print(results_count_work)
print(total_pages_work)


# Make an empty data frame:
results_work <- data.frame()

# Loop through each page and scrape the data:
for (page in 0:(total_pages_work - 1)) {
  # Update the page parameter in the URL for the current page:
  current_url_work <- paste0(url_work, "&page=", page + 1)
  
  # Read and parse the current webpage:
  current_page_work <- read_html(current_url_work)
  
  # Extract article titles:
  titles <- current_page_work %>%
    html_nodes(".docsum-title") %>%
    html_text(trim = TRUE)
  
  # Extract authors:
  author_info <- tryCatch({
    current_page_work %>%
      html_nodes(".docsum-authors.short-authors") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting author info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Extract the journal and year
  journal_year_info <- tryCatch({
    current_page_work %>%
      html_nodes(".docsum-journal-citation.short-journal-citation") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting journal info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Create a data frame with the extracted data:
  current_results_work <- data.frame(Title = titles,
                                    Author_Info = author_info,
                                    Journal_Year_Info = journal_year_info)
  
  # Combine the current results with the previous results:
  results_work <- rbind(results_work, current_results_work)
  
  # Add a delay to avoid overloading the server and getting booted off!
  sleep_time <- runif(1, min = 10, max = 20)
  Sys.sleep(sleep_time)
}

# Check outcomes:
print(length(titles))
print(length(author_info))
print(length(journal_year_info))

as_tibble(results_work) %>% 
  print()

names(results_work)

# Visually inspect results:
View(results_work)

# Now save the initial results before proceeding so we can't lose them!
write.csv(results_work, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/work_data.csv", row.names=FALSE)

# Read written csv. file if required:
results_work <- read.csv("work_data.csv")

# Now let's explore the data and prepare it for analysis
# Total number returned:
count(results_work)

# Any duplicates?
count(distinct(results_work)) # 2

# Have a look at duplicates where the title, year, and authors are all the same:
results_work %>% 
  group_by(Title, Journal_Year_Info, Author_Info) %>% 
  filter(n()>1) %>% print()
# There are two duplicates.  These seem to be genuine duplicates that require removal. Here are the titles:
#  - Estimated Prevalence and Demographic Correlates of Compulsive workual Behavior Among Gay Men in the United States.
#  - What is Normal Pornography Use in a Highly Religious Area? Exploring Patterns of Pornography Use in Utah.

# Have a look at duplicates where just the title, journal and year are the same:
results_work %>% 
  group_by(Title, Journal_Year_Info) %>% 
  filter(n()>1) %>% print() 
# Same as above

# Have a look at duplicates where just the title is the same:
results_work %>% 
  group_by(Title) %>% 
  filter(n()>1) %>% 
  print() 
# Again, just the one above.

# Now separate the journal and date info:
results_work_split<- as_tibble(results_work) %>%
  separate(Journal_Year_Info, c("Journal", "Year"), sep = "\\. ") %>%
  print()

# We need to remove the period from the end of the year data:
Year_numeric_work<- as.numeric(stringr::str_remove(results_work_split$Year, "\\.$"))

# Now append our updated year column to to the main dataset, first checking the new and old dates match to create a cleam, useable dataset:
results_work_clean<- results_work_split %>% 
  bind_cols(Year_numeric_work)  %>% 
  select(-Year) %>% 
  rename(Year = 4) %>% # Checked dates matched before removal and finalisation
  distinct()

#  Okay, now we have a clean dataset, let's explore!
# Number of publications per year:
work_hist<- hist(results_work_clean$Year,
                xlim = c(1960,2023),
                breaks = 30,
                main = "Papers published per year on work Addiction (PubMed)",
                xlab = "Year")

# Summary of the Year  variable:
summary(results_work_clean$Year)

# There is 1 NA value. let's take a look at them:
results_work_clean %>% filter(is.na(Year))
# It's another "StatPearl" which is not a not peer-reviewed paper. Remove:
results_work_clean2 <- results_work_clean %>% 
  filter(Title != "Drug Addiction.")

# Check this has removed the entry:
count(results_work_clean) - count(results_work_clean2) #  Yarp

# Number of separate journals:
results_work_clean2 %>% 
  distinct(Journal) %>% 
  arrange(Journal) %>% 
  print(n = 20)

# Most popular journals:
results_work_clean2 %>% 
  group_by(Journal) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(desc(n)) %>%
  print(n = 20)

# Now I need to add a label to all of these studies to signify that they Were returned from the work search so that we can distinguish them from other studies when we later joined the datasets together:
count(results_work_clean2) 
Label_work <- rep("work", times = count(results_work_clean2))

results_work_final <- results_work_clean2 %>% bind_cols(Label_work) %>%
  rename(Label = 5) %>%
  print()

# Now save the cleaned results
write.csv(results_work_final, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/work_data_cleaned.csv", row.names=FALSE)
















################# Extreme sports search
# PubMed did not like the proper search I developed one bit and said none of the terms were indexed by them so I searched “extreme sports addiction” OR “Adventure sports addiction” without  specifying the location of the terms, and then manually search the 50 results returned for relevant studies. Below I extract the few relevant studies from these 50. 
# Original search: ((((((((((("extreme sports addiction"[Title/Abstract]) OR ("addicted to extreme sports"[Title/Abstract])) OR ("addiction to extreme sports"[Title/Abstract])) OR ("adventure sports addiction"[Title/Abstract])) OR ("addicted to adventure sports"[Title/Abstract])) OR ("addiction to adventure sports"[Title/Abstract])) OR ("skydiving addiction"[Title/Abstract])) OR ("addicted to skydiving"[Title/Abstract])) OR ("addiction to skydiving"[Title/Abstract])) OR ("rock climbing addiction"[Title/Abstract])) OR ("addicted to rock climbing"[Title/Abstract])) OR ("addiction to rock climbing"[Title/Abstract])

# Actual search: "adventure sports addiction" OR "extreme sports addiction"

# START OF ANALYSIS (performed on 15/05/2022)

#  Clean environment:
rm(list = ls())

# Define the URL:
url_extreme_sports <- "https://pubmed.ncbi.nlm.nih.gov/?term=%22adventure+sports+addiction%22+OR+%22extreme+sports+addiction%22&size=200" 
webpage_extreme_sports <- read_html(url_extreme_sports)

# Get the total number of search results:
results_count_extreme_sports <- webpage_extreme_sports %>%
  html_node(".results-amount .value") %>%
  html_text() %>%
  str_replace(",", "") %>%
  as.numeric()

# Calculate the number of pages:
results_per_page <- 10
total_pages_extreme_sports <- ceiling(results_count_extreme_sports[1]/ results_per_page)
# Print results_count and total_pages:
print(results_count_extreme_sports)
print(total_pages_extreme_sports)


# Make an empty data frame:
results_extreme_sports <- data.frame()

# Loop through each page and scrape the data:
for (page in 0:(total_pages_extreme_sports - 1)) {
  # Update the page parameter in the URL for the current page:
  current_url_extreme_sports <- paste0(url_extreme_sports, "&page=", page + 1)
  
  # Read and parse the current webpage:
  current_page_extreme_sports <- read_html(current_url_extreme_sports)
  
  # Extract article titles:
  titles <- current_page_extreme_sports %>%
    html_nodes(".docsum-title") %>%
    html_text(trim = TRUE)
  
  # Extract authors:
  author_info <- tryCatch({
    current_page_extreme_sports %>%
      html_nodes(".docsum-authors.short-authors") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting author info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Extract the journal and year
  journal_year_info <- tryCatch({
    current_page_extreme_sports %>%
      html_nodes(".docsum-journal-citation.short-journal-citation") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting journal info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Create a data frame with the extracted data:
  current_results_extreme_sports <- data.frame(Title = titles,
                                    Author_Info = author_info,
                                    Journal_Year_Info = journal_year_info)
  
  # Combine the current results with the previous results:
  results_extreme_sports <- rbind(results_extreme_sports, current_results_extreme_sports)
  
  # Add a delay to avoid overloading the server and getting booted off!
  sleep_time <- runif(1, min = 10, max = 20)
  Sys.sleep(sleep_time)
}

# Check outcomes:
print(length(titles))
print(length(author_info))
print(length(journal_year_info))

as_tibble(results_extreme_sports) %>% 
  print()

names(results_extreme_sports)

# Visually inspect results:
View(results_extreme_sports)

# Now save the initial results before proceeding so we can't lose them!
write.csv(results_extreme_sports, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/extreme_sports_data.csv", row.names=FALSE)

# Read written csv. file if required:
results_extreme_sports <- read.csv("extreme_sports_data.csv")

# Now let's explore the data and prepare it for analysis
# Total number returned:
count(results_extreme_sports)

# Any duplicates?
count(distinct(results_extreme_sports)) # 0

# Now separate the journal and date info:
results_extreme_sports_split<- as_tibble(results_extreme_sports) %>%
  separate(Journal_Year_Info, c("Journal", "Year"), sep = "\\. ") %>%
  print()

# We need to remove the period from the end of the year data:
Year_numeric_extreme_sports<- as.numeric(stringr::str_remove(results_extreme_sports_split$Year, "\\.$"))

# Now append our updated year column to to the main dataset, first checking the new and old dates match to create a cleam, useable dataset:
results_extreme_sports_clean<- results_extreme_sports_split %>% 
  bind_cols(Year_numeric_extreme_sports)  %>% 
  select(-Year) %>% 
  rename(Year = 4) %>% # Checked dates matched before removal and finalisation
  filter(Title == "Addiction in Extreme Sports: An Exploration of Withdrawal States in Rock Climbers." |
           Title == "Commentary on: Addiction in extreme sports: An exploration of withdrawal states in rock climbers." |
           Title == "[Social-psychological characteristics of different types addictive behavior]." |
           Title == "Why do we climb mountains? An exploration of features of behavioural addiction in mountaineering and the association with stress-related psychiatric disorders." |
           Title == "Response to \"Nature fix: Addiction to outdoor activities\"R. C. Buckley's commentary on Heirene, R. M., Shearer, D., Roderique-Davies, G., & Mellalieu, S. D. (2016). Addiction in extreme sports: An exploration of withdrawal states in rock climbers. Journal of Behavioral Addictions, 5, 332-341." |
           Title == "Adventure Thrills are Addictive."|
           Title == "Development and Initial Validation of a Rock Climbing Craving Questionnaire (RCCQ).") # Now let's isolate our relevant results!

#  Okay, now we have a clean dataset, let's explore!
# Number of publications per year:
extreme_sports_hist<- hist(results_extreme_sports_clean$Year,
                xlim = c(1960,2023),
                breaks = 10,
                main = "Papers published per year on Extreme Sports Addiction (PubMed)",
                xlab = "Year")

# Summary of the Year  variable:
summary(results_extreme_sports_clean$Year)

# Number of separate journals:
results_extreme_sports_clean %>% 
  distinct(Journal) %>% 
  arrange(Journal) %>% 
  print(n = 6)

# Most popular journals:
results_extreme_sports_clean %>% 
  group_by(Journal) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(desc(n)) %>%
  print(n = 20)

# Now I need to add a label to all of these studies to signify that they Were returned from the extreme_sports search so that we can distinguish them from other studies when we later joined the datasets together:
Label_extreme_sports <- rep("extreme_sports", times = count(results_extreme_sports_clean))

results_extreme_sports_final <- results_extreme_sports_clean %>% bind_cols(Label_extreme_sports) %>%
  rename(Label = 5) %>%
  print()

# Now save the cleaned results
write.csv(results_extreme_sports_final, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/extreme_sports_data_cleaned.csv", row.names=FALSE)








################# Dance search
# "dance addiction" OR "dancing addiction"

# Again, PubMed did not like the proper search I developed and said none of the terms were indexed by them so I searched key terms without specifying the location of the terms, and then manually search the results returned for relevant studies. Below I extract the few relevant studies from these. 
# Original search: (("dance addiction"[Title/Abstract]) OR ("dancing addiction"[Title/Abstract])) OR ("addicted to dancing"[Title/Abstract])

# Relevant articles identified manually:
# Argentine tango: Another behavioral addiction?
# An empirical investigation of dance addiction


# Actual search: "dance addiction" OR "dancing addiction"

# START OF ANALYSIS (performed on 16/05/2022)

#  Clean environment:
rm(list = ls())

# Define the URL:
url_dance <- "https://pubmed.ncbi.nlm.nih.gov/?term=%22dance+addiction%22+OR+%22dancing+addiction%22&size=200"
webpage_dance <- read_html(url_dance)

# Get the total number of search results:
results_count_dance <- webpage_dance %>%
  html_node(".results-amount .value") %>%
  html_text() %>%
  str_replace(",", "") %>%
  as.numeric()

# Calculate the number of pages:
results_per_page <- 10
total_pages_dance <- ceiling(results_count_dance[1]/ results_per_page)
# Print results_count and total_pages:
print(results_count_dance)
print(total_pages_dance)


# Make an empty data frame:
results_dance <- data.frame()

# Loop through each page and scrape the data:
for (page in 0:(total_pages_dance - 1)) {
  # Update the page parameter in the URL for the current page:
  current_url_dance <- paste0(url_dance, "&page=", page + 1)
  
  # Read and parse the current webpage:
  current_page_dance <- read_html(current_url_dance)
  
  # Extract article titles:
  titles <- current_page_dance %>%
    html_nodes(".docsum-title") %>%
    html_text(trim = TRUE)
  
  # Extract authors:
  author_info <- tryCatch({
    current_page_dance %>%
      html_nodes(".docsum-authors.short-authors") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting author info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Extract the journal and year
  journal_year_info <- tryCatch({
    current_page_dance %>%
      html_nodes(".docsum-journal-citation.short-journal-citation") %>%
      html_text(trim = TRUE)
  }, error = function(e) {
    print(paste("Error extracting journal info on page", page + 1))
    rep(NA, length(titles))  # Return a vector of NAs with the same length as titles
  })
  
  # Create a data frame with the extracted data:
  current_results_dance <- data.frame(Title = titles,
                                               Author_Info = author_info,
                                               Journal_Year_Info = journal_year_info)
  
  # Combine the current results with the previous results:
  results_dance <- rbind(results_dance, current_results_dance)
  
  # Add a delay to avoid overloading the server and getting booted off!
  sleep_time <- runif(1, min = 10, max = 20)
  Sys.sleep(sleep_time)
}

# Check outcomes:
print(length(titles))
print(length(author_info))
print(length(journal_year_info))

as_tibble(results_dance) %>% 
  print()

names(results_dance)

# Visually inspect results:
View(results_dance)

# Now save the initial results before proceeding so we can't lose them!
write.csv(results_dance, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/dance_data.csv", row.names=FALSE)

# Read written csv. file if required:
results_dance <- read.csv("dance_data.csv")

# Now let's explore the data and prepare it for analysis
# Total number returned:
count(results_dance)

# Any duplicates?
count(distinct(results_dance)) # 0

# Now separate the journal and date info:
results_dance_split<- as_tibble(results_dance) %>%
  separate(Journal_Year_Info, c("Journal", "Year"), sep = "\\. ") %>%
  print()

# We need to remove the period from the end of the year data:
Year_numeric_dance<- as.numeric(stringr::str_remove(results_dance_split$Year, "\\.$"))

# Now append our updated year column to to the main dataset, first checking the new and old dates match to create a cleam, useable dataset:
results_dance_clean<- results_dance_split %>% 
  bind_cols(Year_numeric_dance)  %>% 
  select(-Year) %>% 
  rename(Year = 4) %>% # Checked dates matched before removal and finalisation
  filter(Title == "Argentine tango: Another behavioral addiction?" |
           Title == "An empirical investigation of dance addiction.") # Now let's isolate our relevant results!

#  Okay, now we have a clean dataset, let's explore!
# Number of publications per year:
dance_hist<- hist(results_dance_clean$Year,
                           xlim = c(1960,2023),
                           breaks = 2,
                           main = "Papers published per year on Dance Addiction (PubMed)",
                           xlab = "Year")

# Summary of the Year  variable:
summary(results_dance_clean$Year)

# Number of separate journals:
results_dance_clean %>% 
  distinct(Journal) %>% 
  arrange(Journal) %>% 
  print(n = 6)

# Most popular journals:
results_dance_clean %>% 
  group_by(Journal) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(desc(n)) %>%
  print(n = 20)

# Now I need to add a label to all of these studies to signify that they Were returned from the dance search so that we can distinguish them from other studies when we later joined the datasets together:
Label_dance <- rep("dance", times = count(results_dance_clean))

results_dance_final <- results_dance_clean %>% bind_cols(Label_dance) %>%
  rename(Label = 5) %>%
  print()

# Now save the cleaned results
write.csv(results_dance_final, "C:/Users/rheirene/Dropbox (Personal)/Sci-Software/Blog posts/PubMed Article Scraping/Web scrape scholar/dance_data_cleaned.csv", row.names=FALSE)









################# Tanning search


################# Selfie search


################# Flying search


################# Fortune telling search