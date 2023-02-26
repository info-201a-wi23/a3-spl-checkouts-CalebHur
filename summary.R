
# stuff
library("tidyverse")
library("dplyr")
library("ggplot2")

ancient_library_data <- read.csv("~/2013-2023-5-Checkouts-SPL.csv")

ebook_library_data <- ancient_library_data %>%
  filter(MaterialType == "EBOOK")

audiobook_library_data <- ancient_library_data %>%
  filter(MaterialType == "AUDIOBOOK")

book_library_data <- ancient_library_data %>%
  filter(MaterialType == "BOOK")

sounddisc_library_data <- ancient_library_data %>%
  filter(MaterialType == "SOUNDDISC")

videodisc_library_data <- ancient_library_data %>%
  filter(MaterialType == "VIDEODISC")

physical_library_data <- ancient_library_data %>%
  filter(UsageClass == "Physical")

digital_library_data <- ancient_library_data %>%
  filter(UsageClass == "Digital")

# ---

# e-book popularity from Jan 2013 to Jan 2020
ebook_checkouts_prior_covid <- ebook_library_data %>%
  filter(CheckoutYear >= "2013" & CheckoutYear <= "2019") %>%
  group_by(CheckoutYear) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))
ebook_checkouts_prior_covid_ratio <- 
  (tail(ebook_checkouts_prior_covid$Checkouts, n = 1) - 
  ebook_checkouts_prior_covid$Checkouts[[1]]) / 6 / 12

# audio book popularity from Jan 2013 to Jan 2020
audiobook_checkouts_prior_covid <- audiobook_library_data %>%
  filter(CheckoutYear >= "2013" & CheckoutYear <= "2019") %>%
  group_by(CheckoutYear) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))
audiobook_checkouts_prior_covid_ratio <- 
  (tail(audiobook_checkouts_prior_covid$Checkouts, n = 1) - 
     audiobook_checkouts_prior_covid$Checkouts[[1]]) / 6 / 12

# traditional book popularity from Jan 2013 to Jan 2020
book_checkouts_prior_covid <- book_library_data %>%
  filter(CheckoutYear >= "2013" & CheckoutYear <= "2019") %>%
  group_by(CheckoutYear) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))
book_checkouts_prior_covid_ratio <- 
  (tail(book_checkouts_prior_covid$Checkouts, n = 1) - 
     book_checkouts_prior_covid$Checkouts[[1]]) / 6 / 12

# sound disc popularity from Jan 2013 to Jan 2020
sounddisc_checkouts_prior_covid <- sounddisc_library_data %>%
  filter(CheckoutYear >= "2013" & CheckoutYear <= "2019") %>%
  group_by(CheckoutYear) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))
sounddisc_checkouts_prior_covid_ratio <- 
  (tail(sounddisc_checkouts_prior_covid$Checkouts, n = 1) - 
     sounddisc_checkouts_prior_covid$Checkouts[[1]]) / 6 / 12

# video disc popularity from Jan 2013 to Jan 2020
videodisc_checkouts_prior_covid <- videodisc_library_data %>%
  filter(CheckoutYear >= "2013" & CheckoutYear <= "2019") %>%
  group_by(CheckoutYear) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))
videodisc_checkouts_prior_covid_ratio <- 
  (tail(videodisc_checkouts_prior_covid$Checkouts, n = 1) - 
     videodisc_checkouts_prior_covid$Checkouts[[1]]) / 6 / 12

# physical popularity from Jan 2013 to Jan 2020
physical_checkouts_prior_covid <- physical_library_data %>%
  filter(CheckoutYear >= "2013" & CheckoutYear <= "2019") %>%
  group_by(CheckoutYear) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))
physical_checkouts_prior_covid_ratio <- 
  (tail(physical_checkouts_prior_covid$Checkouts, n = 1) - 
     physical_checkouts_prior_covid$Checkouts[[1]]) / 6 / 12

# digital popularity from Jan 2013 to Jan 2020
digital_checkouts_prior_covid <- digital_library_data %>%
  filter(CheckoutYear >= "2013" & CheckoutYear <= "2019") %>%
  group_by(CheckoutYear) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))
digital_checkouts_prior_covid_ratio <- 
  (tail(digital_checkouts_prior_covid$Checkouts, n = 1) - 
     digital_checkouts_prior_covid$Checkouts[[1]]) / 6 / 12

# ---

ebook_library_data <- ancient_library_data %>%
  filter(MaterialType == "EBOOK")

book_library_data <- ancient_library_data %>%
  filter(MaterialType == "BOOK")

# e-book popularity from Feb 2020 to Mar 2020
ebook_checkouts_during_covid <- ebook_library_data %>%
  filter(CheckoutYear == "2020") %>%
  filter(CheckoutMonth == "3" | CheckoutMonth == "4") %>%
  group_by(CheckoutMonth) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))
ebook_checkouts_during_covid_ratio <- 
  ebook_checkouts_during_covid$Checkouts[[2]] - 
     ebook_checkouts_during_covid$Checkouts[[1]]

# traditional book popularity from Apr 2020 to May 2020
book_checkouts_during_covid <- ebook_library_data %>%
  filter(CheckoutYear == "2020") %>%
  filter(CheckoutMonth == "4" | CheckoutMonth == "5") %>%
  group_by(CheckoutMonth) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))
book_checkouts_during_covid_ratio <- 
  book_checkouts_during_covid$Checkouts[[2]] - 
  book_checkouts_during_covid$Checkouts[[1]]

# ---

physical_library_data <- ancient_library_data %>%
  filter(UsageClass == "Physical")

digital_library_data <- ancient_library_data %>%
  filter(UsageClass == "Digital")

# physical popularity at Jan 2020
physical_checkouts_prior_covid <- physical_library_data %>%
  filter(CheckoutYear == "2020" & CheckoutMonth == "1") %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))
physical_checkouts_prior_covid_value <- 
  physical_checkouts_prior_covid[[1]]

# digital popularity at Jan 2020
digital_checkouts_prior_covid <- digital_library_data %>%
  filter(CheckoutYear == "2020" & CheckoutMonth == "1") %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))
digital_checkouts_prior_covid_value <- 
  digital_checkouts_prior_covid[[1]]

# physical popularity at Jan 2023
physical_checkouts_post_covid <- physical_library_data %>%
  filter(CheckoutYear == "2023" & CheckoutMonth == "1") %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))
physical_checkouts_post_covid_value <- 
  physical_checkouts_post_covid[[1]]

# digital popularity at Jan 2023
digital_checkouts_post_covid <- digital_library_data %>%
  filter(CheckoutYear == "2023" & CheckoutMonth == "1") %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))
digital_checkouts_post_covid_value <- 
  digital_checkouts_post_covid[[1]]
