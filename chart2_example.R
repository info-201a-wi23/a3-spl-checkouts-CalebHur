
# stuff
library("tidyverse")
library("dplyr")
library("ggplot2")

ancient_library_data <- read.csv("~/2013-2023-5-Checkouts-SPL.csv")

# So if there was a surge in e-books, what about books?
book_library_data <- ancient_library_data %>%
  filter(MaterialType == "BOOK")
sounddisc_library_data <- ancient_library_data %>%
  filter(MaterialType == "SOUNDDISC")
videodisc_library_data <- ancient_library_data %>%
  filter(MaterialType == "VIDEODISC")

# Summarize all checkouts into years.
book_library_data <- book_library_data %>%
  group_by(CheckoutYear, CheckoutMonth) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))
sounddisc_library_data <- sounddisc_library_data %>%
  group_by(CheckoutYear, CheckoutMonth) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))
videodisc_library_data <- videodisc_library_data %>%
  group_by(CheckoutYear, CheckoutMonth) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))

# Combining into one useful data frame.
combined_physical_library_data <- 
  left_join(book_library_data, sounddisc_library_data, 
            by = c("CheckoutYear", "CheckoutMonth"))
combined_physical_library_data <- 
  left_join(combined_physical_library_data, videodisc_library_data, 
            by = c("CheckoutYear", "CheckoutMonth"))
combined_physical_library_data <- combined_physical_library_data %>%
  mutate(time = CheckoutYear + CheckoutMonth / 12)

# Graph.
ggplot(data = combined_physical_library_data, aes(x = time)) +
  geom_line(aes(y = Checkouts.x, color = "Books"), size = 1) +
  geom_line(aes(y = Checkouts.y, color = "Sound Discs"), size = 1) +
  geom_line(aes(y = Checkouts, color = "Video Discs"), size = 1) + 
  labs(x = "Date (Years)", y = "Daily Checkouts", color = "Material Type") + 
  scale_color_manual(values = c("skyblue", "darkgreen", "green")) + 
  scale_x_continuous(limits = c(2013, 2023), breaks = 0:2100) +
  scale_y_continuous(breaks = seq(0, 300000, 30000))
