
# stuff
library("tidyverse")
library("dplyr")
library("ggplot2")

ancient_library_data <- read.csv("~/2013-2023-5-Checkouts-SPL.csv")

# Do people just prefer online media (comfort) 
#   or physical copies (tangible)?
physical_library_data <- ancient_library_data %>%
  filter(UsageClass == "Physical")
digital_library_data <- ancient_library_data %>%
  filter(UsageClass == "Digital")

# Summarize all checkouts into years.
physical_library_data <- physical_library_data %>%
  group_by(CheckoutYear, CheckoutMonth) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))
digital_library_data <- digital_library_data %>%
  group_by(CheckoutYear, CheckoutMonth) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))

# Combining into one useful data frame.
combined_library_data <- 
  left_join(physical_library_data, digital_library_data, 
            by = c("CheckoutYear", "CheckoutMonth"))
combined_library_data <- combined_library_data %>%
  mutate(time = CheckoutYear + CheckoutMonth / 12)

# Graph.
ggplot(data = combined_library_data, aes(x = time)) +
  geom_line(aes(y = Checkouts.x, color = "Physical"), size = 1) +
  geom_line(aes(y = Checkouts.y, color = "Digital"), size = 1) + 
  labs(x = "Date (Years)", y = "Daily Checkouts", color = "Material Type") + 
  scale_color_manual(values = c("red", "blue")) + 
  scale_x_continuous(limits = c(2013, 2023), breaks = 0:2100) +
  scale_y_continuous(breaks = seq(0, 480000, 80000)) 
