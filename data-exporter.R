################################################################################################
# /email-to-table/
# Extracts gmail email data from .mbox files and saves it to a CSV file.
# version 0.1

# thanks
# start idea: https://medium.com/@houbensarah/data-extraction-from-gmail-with-r-4fd6253ae1f8
# Adapted and improved with Claude.ai

################################################################################################

#install.packages("tm")
#install.packages("stringr")
#install.packages("stringi")
#install.packages("lubridate")


library(tm)
library(stringr)
library(stringi)
library(lubridate)


# Load the data
mbox_path <- "data/"
mbox_file <- paste0(mbox_path, "data-analysis-adopter-emails.mbox")
mbox_content <- readLines(mbox_file, warn = FALSE)

# Function to extract information from .mbox file
extract_email_data <- function(mbox_content) {
  # Initialize vectors to store extracted data
  dates <- character()
  froms <- character()
  subjects <- character()
  content_texts <- character()
  
  # Regex patterns
  date_pattern <- "^Date: (.+)$"
  from_pattern <- "^From: (.+)$"
  subject_pattern <- "^Subject: (.+)$"
  content_start_pattern <- "^Content-Type: text/plain"
  content_end_pattern <- "^--"
  
  in_content <- FALSE
  current_content <- character()
  
  for (line in mbox_content) {
    if (grepl(date_pattern, line, ignore.case = TRUE)) {
      date_str <- str_match(line, date_pattern)[, 2]
      dates <- c(dates, date_str)
    } else if (grepl(from_pattern, line, ignore.case = TRUE)) {
      from <- str_match(line, from_pattern)[, 2]
      froms <- c(froms, from)
    } else if (grepl(subject_pattern, line, ignore.case = TRUE)) {
      subject <- str_match(line, subject_pattern)[, 2]
      subjects <- c(subjects, subject)
    } else if (grepl(content_start_pattern, line, ignore.case = TRUE)) {
      in_content <- TRUE
      current_content <- character()
    } else if (in_content) {
      if (grepl(content_end_pattern, line)) {
        in_content <- FALSE
        content_texts <- c(content_texts, paste(current_content, collapse = "\n"))
      } else {
        current_content <- c(current_content, line)
      }
    }
  }
  
  # Process dates
  parsed_dates <- parse_date_time(dates, orders = c("a, d b Y H:M:S", "d b Y H:M:S"), quiet = TRUE)
  
  # Clean email addresses
  froms <- str_extract(froms, "\\S+@\\S+")
  
  # Print debugging information
  cat("Dates extracted:", length(dates), "\n")
  cat("Dates parsed successfully:", sum(!is.na(parsed_dates)), "\n")
  cat("From addresses extracted:", length(froms), "\n")
  cat("Subjects extracted:", length(subjects), "\n")
  cat("Content texts extracted:", length(content_texts), "\n")
  
  # Ensure all vectors have the same length
  max_length <- max(length(parsed_dates), length(froms), length(subjects), length(content_texts))
  
  parsed_dates <- c(parsed_dates, rep(NA, max_length - length(parsed_dates)))
  froms <- c(froms, rep(NA, max_length - length(froms)))
  subjects <- c(subjects, rep(NA, max_length - length(subjects)))
  content_texts <- c(content_texts, rep(NA, max_length - length(content_texts)))
  
  # Create a data frame
  email_df <- data.frame(
    Date_time = parsed_dates,
    From = froms,
    Subject = subjects,
    Text = content_texts,
    stringsAsFactors = FALSE
  )
  
  return(email_df)
}

# Apply the function to extract emails
emails_df <- extract_email_data(mbox_content)

# Remove rows where all fields are NA
emails_df <- emails_df[rowSums(is.na(emails_df)) != ncol(emails_df), ]

# Write to a CSV file
timestamp <- format(Sys.time(), "%Y%m%d")
output_filename <- paste0("data-analysis_", timestamp, ".csv")
write.csv(emails_df, file = output_filename, row.names = FALSE, quote = TRUE)

# Print summary
cat("Processed", nrow(emails_df), "emails. Results saved to", output_filename, "\n")