library(tidyverse)
library(rvest)


patents <- read_html("https://www.ificlaims.com/rankings-top-50-2018.htm") %>%
  html_nodes("#sortable_table_5e4a16b5bc804") %>%
  html_table(fill = TRUE)
patents <- patents[[1]]

colnames(patents) <- c("deletethisone", "Company", "Grants2018", "Grants2017", "PercentChange", "PreviousRank", "RankChange")
patents <- patents %>%
  transmute(Company, Grants2018, Grants2017, PercentChange, PreviousRank, RankChange)

patents <- as_tibble(patents)
patents
