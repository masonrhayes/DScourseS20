library(jsonlite)
library(tidyverse)

# Use this later for head command
n = 5

# system(wget) command did not work in R (neither locally, where wget was not recognized, nor in OSCER, where wget was recognized but the system command always returned some syntax error)

nflstats <- fromJSON('nflstats.json')

class(nflstats)

# nflstats is a list

class(nflstats$players)

# nflstats$players is a data frame

head(nflstats$players, n)




