# # At first i tried to use tidyverse, then I realized that using tidyverse broke some of the functions in sparkR
# select, filter, etc were overridden by tidyverse and caused many problems in sparkR

# don't ever use tidyverse in spark!
# library(tidyverse)

df1 <- tibble(iris)
df <- createDataFrame(iris)

# Find class of df1
class(df1)
# it is a tibble/data frame

# find class of df
class(df)
# it is a SparkDataFrame

# Select first six rows of df
head(select(df, df$Sepal_Length, df$Species))
## This does not work! (when tidyverse is enabled!)

## Returns the following message (when tidyverse is enabled):
# no applicable method for 'filter_' applied to an object of class "SparkDataFrame"

# Select rows from df1 where sepal length is greater than 5.5
head(filter(df1, df1$iris$Sepal.Length > 5.5))
# This works fine with tidyverse

# Apparently the select and filter options in spark are only for RDD
# tidyverse has convenient functions that do these operations on non RDDs
# but then the functions break each other, cannot be used together?

# Nest the commands
head(select(filter(df, df$Sepal_Length>5.5), df$Sepal_Length, df$Species))

# Use groupby command
head(summarize(groupBy(df, df$Species), mean=mean(df$Sepal_Length), count=n(df$Sepal_Length)))

