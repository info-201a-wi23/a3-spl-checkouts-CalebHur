
# stuff
library("tidyverse")
library("dplyr")
library("ggplot2")

ancient_library_data <- read.csv("~/2013-2023-5-Checkouts-SPL.csv")

# so, then, do people just prefer 
#   online media (comfort) or physical copies (tangible)?

physical_library_data <- ancient_library_data %>%
  filter(UsageClass == "Physical")
digital_library_data <- ancient_library_data %>%
  filter(UsageClass == "Digital")

# summarize all checkouts into years
physical_library_data <- physical_library_data %>%
  group_by(CheckoutYear, CheckoutMonth) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))
digital_library_data <- digital_library_data %>%
  group_by(CheckoutYear, CheckoutMonth) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE))

combined_library_data <- 
  left_join(physical_library_data, digital_library_data, 
            by = c("CheckoutYear", "CheckoutMonth"))

combined_library_data <- combined_library_data %>%
  mutate(time = CheckoutYear + CheckoutMonth / 12)

ggplot(data = combined_library_data, aes(x = time)) +
  geom_line(aes(y = Checkouts.x)) +
  geom_line(aes(y = Checkouts.y))
