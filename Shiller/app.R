#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(DBI)
library(rvest)
library(tidyverse)
library(ggplot2)
library(pool)
library(httr)
library(jsonlite)

# devtools::install_github("rstudio/pool")

#access my database
host = host <- "34.123.54.80"
con <- dbPool(
    RPostgres::Postgres(),
    dbname = "postgres",
    user = "postgres", password = Sys.getenv("DATABASEPW"), host = host
)
#access the inflation data, store it for easy use
inflation_table = as_tibble(con %>% tbl("inflationTable")) #both of these are successfully accessed
nasdaq = as_tibble(con %>% tbl("tickerTable"))



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Shiller P/E (CAPE) Calculator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "stockChoice",
                        label = "Select a Stock on the NASDAQ:",
                        choices = c("-",sort(nasdaq$Description)))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("test"),
            tableOutput("test2"),
           plotOutput("distPlot"),
           plotOutput("marketShillerRatio"), #this will display a histogram of the market CAPE versus the chosen companies CAPE
           textOutput("overOrUnderValued"), #will display whether or not the stock is over or under valued based on the market
           textOutput("earningsPerShareHistogramTitle"),
           plotOutput("earningsPerShare"), #plot the companies 10 years worth of inflation_adjusted earnings 2011-2020
        
        )
    )
)




server <- function(input, output,session) {
    
    #The company that the user chooses
    company_data = reactive({
        chosenCompany = nasdaq %>% 
            filter(Description == input$stockChoice)
        return(chosenCompany)
    })
    
    

        #the company data is what we are gonna use to calculate the SHiller P/E ratio, and plot inflation adjusted earnings per share
        #this is the company that the user selects:
        
        #now construct the main data used to get inflation adjusted eps and shiller ratio, based on the ticker of the company the user chose
        shiller_data = reactive({
            api_call = GET(str_glue("https://www.alphavantage.co/query?function=EARNINGS&symbol={symbol}&apikey={apikey}",symbol = company_data()$Symbol, 
                                    apikey = Sys.getenv("ALPHA_VANTAGE_APIKEY") ))
            data = fromJSON(content(api_call, type = "text", encoding = "UTF-8"))
            
            
            #use regex to convert the date into just a 4 digit year
            annual_eps = data$annualEarnings
            fiscal_year = annual_eps %>% pull(fiscalDateEnding)
            
            pattern = "\\d\\d\\d\\d"
            simple_fiscal_year = str_extract(fiscal_year,regex(pattern) )
            str(simple_fiscal_year) #the fiscalDateEnding variable is a string/char
            
            
            #convert year from api call to int:
            simple_fiscal_year = as.numeric(simple_fiscal_year) #now its "num"...
            
            
            #reattach the modified Year column to the annual_eps df:
            annual_eps = annual_eps %>% 
                select(reportedEPS) %>% 
                bind_cols(simple_fiscal_year) %>% 
                rename("Year" = "...2")
            
            
            
            #join the year/eps (api call) table with the year/inflation table (from db)
            inflation_table = as_tibble(con %>% tbl("inflationTable"))
            
            intermediate_df = inner_join(inflation_table,annual_eps) #This is the joined table
            
            
            #calculate an inflation adjusted earnings per share
            #restrict to only 2011-2020
            inflation_adjusted_eps_df= intermediate_df %>% 
                mutate(inf_adj_eps = as.numeric(reportedEPS)*(1 - average_inflation)) %>% 
                slice(1:10)
            
            #Obtain stock price from previous day with another API call (maybe move this piece up)
            api_call_price = GET(str_glue("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol={symbol}&apikey={apikey}&outputsize=compact",symbol = company_data()$Symbol, 
                                          apikey = Sys.getenv("ALPHA_VANTAGE_APIKEY") ))
            price_data = fromJSON(content(api_call_price, type = "text", encoding = "UTF-8"))
            
            most_recent_price = as.numeric(price_data$`Time Series (Daily)`[[1]]$`4. close`)#accesses the closing price of the previous (market) day, use this to calculate price
            
            
            #calculate shiller PE
            shiller_ratio = inflation_adjusted_eps_df %>% 
                summarize(shiller_ratio = `most_recent_price`/mean(inf_adj_eps))
            return(shiller_ratio)
            
            
            
            })
    
    
    output$test = renderTable({ #making sure I can at least query the database properly
        summary(inflation_table)
        
    })
    output$test2 = renderTable({
        shiller_data() %>% head(5)
    })
    
    
    #title for the earnings per share histrogram
    output$earningsPerShareHistogramTitle = renderText({
        paste("Inflation Adjusted Earnings per Share: 2011-2020")
    })
    
    output$earningsPerShare = renderPlot({
        ggplot(data = shiller_data())+
            geom_histogram(mapping = aes(x = Year, y = inf_adj_eps))

    }
}


# Run the application 
shinyApp(ui = ui, server = server)
