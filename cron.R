#
library(DBI)
library(tidyverse)
library(reader)

host <- "34.123.54.80"  # replace it with your server ip
NASDAQ <- read.delim("~/Google Drive/Programming Folder (4Transfer to Collin's Files V2)/R/STA141B/data_technologies_project/NASDAQ.txt")

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "postgres",
  user = "postgres", password = Sys.getenv("DATABASEPW"), host = host)


if (!("tickerTable" %in% dbListTables(con))) {
  # create table if it doesn't exist
  dbWriteTable(
    con,
    "tickerTable",
    NASDAQ#fix this
  )
}

con %>% dbListTables()
con %>% tbl("table1")

# randomly generate some data
data <- data.frame(x = rnorm(10), y = rnorm(10))

# update the databse
dbWriteTable(con, "table1", data, overwrite = TRUE)

dbDisconnect(con)