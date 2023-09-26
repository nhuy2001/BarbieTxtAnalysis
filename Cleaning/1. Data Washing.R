library(tidyverse)

dat <- read.csv("Barbie.csv")

dat$ID <- 1:length(dat$text)

dat$date <- mark::str_extract_date(dat$text, format = "%d %B %Y")
dat$thesisUsername <- stringr::str_extract(dat$text, "^\\D+")

dat$rating <- as.numeric(dat$rating)

writexl::write_xlsx(dat, "BarbieWash.xlsx")

#GOD BLESS str_extract(_date). HALLELUYA!