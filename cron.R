#
library(DBI)
library(tidyverse)

host <- "34.123.54.80"  # replace it with your server ip

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "demo",
  user = "postgres", password = Sys.getenv("DATABASEPW"), host = host)

?dbWriteTable
if (!("table1" %in% dbListTables(con))) {
  # create table if it doesn't exist
  dbWriteTable(
    con,
    "table1",
    data.frame(x = rnorm(10), y = rnorm(10))#fix this
  )
}


# randomly generate some data
data <- data.frame(x = rnorm(10), y = rnorm(10))

# update the databse
dbWriteTable(con, "table1", data, overwrite = TRUE)

dbDisconnect(con)