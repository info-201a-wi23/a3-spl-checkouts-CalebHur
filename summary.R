
# stuff
library("tidyverse")
library("dplyr")
library("ggplot2")

ancient_library_data <- read.csv("~/2013-2023-5-Checkouts-SPL.csv")


# max ebooks in 2020
max_ebooks <- ancient_library_data %>%
  filter(MaterialType == "EBOOK") %>%
  group_by(CheckoutYear) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE)) %>%
  filter(Checkouts == max(Checkouts, na.rm = TRUE))

# max traditional books in 2020
max_books <- ancient_library_data %>%
  filter(MaterialType == "BOOK") %>%
  group_by(CheckoutYear) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = FALSE)) %>%
  filter(Checkouts == max(Checkouts, na.rm = TRUE))

# slope of physical books prior to 2020
# slope of digital books prior to 2020
# slope of physical books after 2020
# slope of digital books after 2020

# just a bunch of numbers yeah

# so they manitained their popularity even after covid
# then again, more people can checkout the same book, 
#   so its not necessarily that they got more popular
# nonetheless, there is great interest in them, and it would seem more people
#   appreciate the accessibility
# however, this data is limiting as it does not show if a person would
#   PREFER a tradiitonal book over a screen, there is no rating, only checkouts

# so basically, books have been going down, surge of excitement
# digital books have always been on the rise, and covid (2020) boosted,
#   but it leveled off and might decrease
