
# stuff
library("tidyverse")
library("dplyr")
library("ggplot2")

ancient_library_data <- read.csv("~/2013-2023-5-Checkouts-SPL.csv")

# Did e-books or audiobooks take off?
ebook_library_data <- ancient_library_data %>%
  filter(MaterialType == "EBOOK")
audiobook_library_data <- ancient_library_data %>%
  filter(MaterialType == "AUDIOBOOK")

# Summarize all checkouts into years.
ebook_library_data <- ebook_library_data %>%
  group_by(CheckoutYear, CheckoutMonth) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))
audiobook_library_data <- audiobook_library_data %>%
  group_by(CheckoutYear, CheckoutMonth) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))

# Combining into one useful dataframe.
combined_digital_library_data <- 
  left_join(ebook_library_data, audiobook_library_data, 
            by = c("CheckoutYear", "CheckoutMonth"))
combined_digital_library_data <- combined_digital_library_data %>%
  mutate(time = CheckoutYear + CheckoutMonth / 12)

# Graph.
ggplot(data = combined_digital_library_data, aes(x = time)) +
  geom_line(aes(y = Checkouts.x, color = "E-Books"), size = 1) +
  geom_line(aes(y = Checkouts.y, color = "Audiobook"), size = 1) + 
  labs(x = "Date (Years)", y = "Daily Checkouts", color = "Material Type") +
  scale_color_manual(values = c("darkred", "darkorange")) + 
  scale_x_continuous(limits = c(2013, 2023), breaks = 0:2100) +
  scale_y_continuous(breaks = seq(0, 150000, 30000))
