
# stuff
library("tidyverse")
library("dplyr")
library("ggplot2")

ancient_library_data <- read.csv("~/2013-2023-5-Checkouts-SPL.csv")

# so ebooks took off, but did traditional books do the same?
ebook_library_data <- ancient_library_data %>%
  filter(MaterialType == "EBOOK")
audiobook_library_data <- ancient_library_data %>%
  filter(MaterialType == "AUDIOBOOK")

# summarize all checkouts into years
ebook_library_data <- ebook_library_data %>%
  group_by(CheckoutYear, CheckoutMonth) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))
audiobook_library_data <- audiobook_library_data %>%
  group_by(CheckoutYear, CheckoutMonth) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))

combined_digital_library_data <- 
  left_join(ebook_library_data, audiobook_library_data, 
            by = c("CheckoutYear", "CheckoutMonth"))

combined_digital_library_data <- combined_digital_library_data %>%
  mutate(time = CheckoutYear + CheckoutMonth / 12)

ggplot(data = combined_digital_library_data, aes(x = time)) +
  geom_line(aes(y = Checkouts.x)) +
  geom_line(aes(y = Checkouts.y))
