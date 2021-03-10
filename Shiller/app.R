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

# devtools::install_github("rstudio/pool")

#access my database
host = host <- "34.123.54.80"
con <- dbConnect(
    RPostgres::Postgres(),
    dbname = "postgres",
    user = "postgres", password = Sys.getenv("DATABASEPW"), host = host
)
#access the inflation data, store it for easy use
inflation_table = as_tibble(con %>% tbl("inflationTable"))
nasdaq = as_tibble(con %>% tbl("tickerTable"))
inflation_table
nasdaq


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
           plotOutput("distPlot"),
           plotOutput("marketShillerRatio"), #this will display a histogram of the market CAPE versus the chosen companies CAPE
           textOutput("overOrUnderValued"), #will display whether or not the stock is over or under valued based on the market
           plotOutput("earningsPerShare") #plot the companies 10 years worth of inflation_adjusted earnings 2011-2020
            
            tableOutput("test")
        )
    )
)
?sidebarPanel
# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    

    #the company data is what we are gonna use to calculate the SHiller P/E ratio, and plot inflation adjusted earnings per share
    company_data = reactive({
        chosenCompany = nasdaq %>% 
            filter(Description == input$stockChoice)
        return(chosenCompany)
    })
    
    #now construct the main data used to get inflation adjusted eps and shiller ratio
    shiller_data = reactive({
        api_call = GET(str_glue("https://www.alphavantage.co/query?function=EARNINGS&symbol={symbol}&apikey={apikey}",symbol = company_data()$Symbol,
                                apikey = Sys.getenv("ALPHA_VANTAGE_APIKEY") ))
        data = fromJSON(content(api_call, type = "text", encoding = "UTF-8"))

        #use regex to convert the date into just a 4 digit year
        annual_eps = data$annualEarnings
        fiscal_year = annual_eps %>% pull(fiscalDateEnding)
        pattern = "\\d\\d\\d\\d"
        simple_fiscal_year = str_extract(fiscal_year,regex(pattern) )
        #the fiscalDateEnding variable is a string/char

        #need to check what object type the date in the inflation table is: update; its int

        #convert year from api call to int:
        simple_fiscal_year = as.numeric(simple_fiscal_year) #now its "num"...


        #reattach the modified Year column to the annual_eps df:
        annual_eps = annual_eps %>%
            select(reportedEPS) %>%
            bind_cols(simple_fiscal_year) %>%
            rename("Year" = "...2")

        #join the year/eps (api call) table with the year/inflation table (from db)
        intermediate_df = inner_join(inflation_table,annual_eps)

        inflation_adjusted_eps_df= intermediate_df %>%
            mutate(inf_adj_eps = as.numeric(reportedEPS)*(1 - average_inflation)) %>%
            slice(1:10)
        return(infation_adjusted_eps_df)
    })
    
    api_call = GET(str_glue("https://www.alphavantage.co/query?function=EARNINGS&symbol={symbol}&apikey={apikey}",symbol = "IBM", 
                            apikey = Sys.getenv("ALPHA_VANTAGE_APIKEY") ))
    data = fromJSON(content(api_call, type = "text", encoding = "UTF-8"))
    
    
 
    output$earningsPerShare = renderPlot({
        ggplot(data = shiller_data())+
            geom_histogram(mapping = aes(x = Year, y = inf_adj_eps))
    
    output$test = renderPlot({
        print(inflation_table)
    })
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
