#
library(DBI)
library(tidyverse)
library(reader)
?library(httr)
library(rvest)

# host <- "34.123.54.80"  # replace it with your server ip
# # NASDAQ <- read.delim("~/Google Drive/Programming Folder (4Transfer to Collin's Files V2)/R/STA141B/data_technologies_project/NASDAQ.txt")
# 
# 
# con <- dbConnect(
#   RPostgres::Postgres(),
#   dbname = "postgres",
#   user = "postgres", password = Sys.getenv("DATABASEPW"), host = host)




host = "ec2-3-230-199-240.compute-1.amazonaws.com"
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "dbukerqeq48ibb",
  user = "wsomssizlrlnbm", 
  port = 5432,
  password = Sys.getenv("DATABASEPW"), 
  host = host,
  sslmode = "require")


#scrape for inflatoin data, then add to database:
web_source= read_html("https://inflationdata.com/Inflation/Inflation_Rate/HistoricalInflation.aspx")
table = web_source %>% 
  html_nodes("table") %>% 
  html_table()

inflation_table = as_tibble(table[[1]]) 


inflation_table = inflation_table %>% 
  slice(2:208) %>% 
  select(Year, Ave.) %>% 
  rename("average_inflation" = "Ave.")

avg_inf = inflation_table %>% 
  pull %>% parse_number()
avg_inf = avg_inf/100 

inflation_table = inflation_table %>% 
  select(Year) %>% 
  bind_cols(avg_inf) %>% 
  rename("average_inflation" = "...2")

#write to database/update database with inflation data
dbWriteTable(con, "inflationTable", inflation_table, overwrite = TRUE)



#get SP500 Shiller PE ratio
web_page = read_html("https://www.gurufocus.com/shiller-PE.php")
scraped = web_page %>% html_nodes("h3") %>% html_text()
sp500_shiller_pe = str_extract(scraped[5],"\\d\\d[.]\\d*")
sp500_shiller_table = tibble(sp500_shiller_pe = as.numeric(sp500_shiller_pe))
dbWriteTable(con,"SP500ShillerTable", sp500_shiller_table, overwrite = TRUE)


if (!("tickerTable" %in% dbListTables(con))) {
  # create table if it doesn't exist
  dbWriteTable(
    con,
    "tickerTable",
    NASDAQ#fix this
  )
}


#S&P 500 Data
if (!("SP500Table" %in% dbListTables(con))) {
  # create table if it doesn't exist
  dbWriteTable(
    con,
    "SP500Table",
    SP500#fix this
  )
}



# con %>% dbListTables()
# con %>% tbl("tickerTable") %>% print

# con %>% dbRemoveTable("table1")


# randomly generate some data
# data <- data.frame(x = rnorm(10), y = rnorm(10))

# update the databse
# dbWriteTable(con, "table1", data, overwrite = TRUE)


#When a user requests Shiller P/E for a certain company, first check if the data is already in the table. If it is, use the data from
#the database. If its not, make the API call, append it to a table in the database, and then use it.
# con %>% dbListFields("tickerTable")

# con %>%
#   tbl("tickerTable") %>% 
#   print




dbDisconnect(con)