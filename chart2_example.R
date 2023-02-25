
# stuff
library("tidyverse")
library("dplyr")
library("ggplot2")

ancient_library_data <- read.csv("~/2013-2023-5-Checkouts-SPL.csv")

# was there ever a surge of interest in EBOOKS?
book_library_data <- ancient_library_data %>%
  filter(MaterialType == "BOOK")
sounddisc_library_data <- ancient_library_data %>%
  filter(MaterialType == "SOUNDDISC")
videodisc_library_data <- ancient_library_data %>%
  filter(MaterialType == "VIDEODISC")

# summarize all checkouts into years
book_library_data <- book_library_data %>%
  group_by(CheckoutYear, CheckoutMonth) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))
sounddisc_library_data <- sounddisc_library_data %>%
  group_by(CheckoutYear, CheckoutMonth) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))
videodisc_library_data <- videodisc_library_data %>%
  group_by(CheckoutYear, CheckoutMonth) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))

combined_physical_library_data <- 
  left_join(book_library_data, sounddisc_library_data, 
            by = c("CheckoutYear", "CheckoutMonth"))
combined_physical_library_data <- 
  left_join(combined_physical_library_data, videodisc_library_data, 
            by = c("CheckoutYear", "CheckoutMonth"))

combined_physical_library_data <- combined_physical_library_data %>%
  mutate(time = CheckoutYear + CheckoutMonth / 12)

ggplot(data = combined_physical_library_data, aes(x = time)) +
  geom_line(aes(y = Checkouts.x)) +
  geom_line(aes(y = Checkouts.y)) +
  geom_line(aes(y = Checkouts))
