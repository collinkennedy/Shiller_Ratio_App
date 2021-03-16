#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# library(DBI)
library(rvest)
library(tidyverse)
library(ggplot2)
library(pool)
library(httr)
library(jsonlite)


#access my database
host = host <- "34.123.54.80"
con <- dbPool(
    RPostgres::Postgres(),
    dbname = "postgres",
    user = "postgres", password = Sys.getenv("DATABASEPW"), host = host)
#access the inflation data, store it for easy use
inflation_table = as_tibble(con %>% tbl("inflationTable")) #both of these are successfully accessed
sp500 = as_tibble(con %>% tbl("SP500Table"))
nasdaq = as_tibble(con %>% tbl("tickerTable"))
sp500shiller = as_tibble(con %>% tbl("SP500ShillerTable"))
sectors = con %>% tbl("SP500Table") %>% 
    group_by(Sector) %>% summarize(comp_per_sector = n()) %>% select(Sector) #Will be used as the list of possibilities of sectors
sectors = as_tibble(sectors)

# Define UI for application that draws a histogram
ui = fluidPage(
    titlePanel("Shiller (Cyclically Adjusted Price to Earnings) Ratio Calculator: "),
    sidebarLayout(
        sidebarPanel(
            
            #3 selectInputs to get each of the user inputs needed
            selectInput(inputId = "industryChoice",
                        label = "Select an Industry",
                        choices = c("-",sort(sectors$Sector))),
            
            
            selectInput(inputId = "stockChoice",
                        label = "Select a Company",
                        choices = c("-", sort(sp500$Name)))
            
        ),      
        mainPanel(
            textOutput("sp500shiller"),
            textOutput("shillerTitle"),
            tableOutput("shillerRatio"),
            textOutput("overOrUnder"),
            textOutput("eps_title"),
            plotOutput("eps_hist")
        )
    )
)

server = function(input, output, session) {
    
    

    #chosen company data here
    company_data = reactive({
        req(input$stockChoice, cancelOutput = TRUE)
        
        
        chosenCompany = sp500 %>% 
            filter(Name == input$stockChoice)
        return(chosenCompany)
    })
    
    #inflation adjusted and shiller ratio calculations here
    shiller_data = reactive({
        req(company_data()$Symbol, cancelOutput = TRUE)
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
    
    inflation_data = reactive({
        req(company_data()$Symbol, cancelOutput = TRUE)
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
        return(inflation_adjusted_eps_df)
    })
    
    
    observe({ #gets 1st input from user
    
        chosenIndustry = input$industryChoice
        
        #need the available options to only be of the chosen industry type

        data = sp500 %>% 
            filter(Sector == chosenIndustry)
        
        #from Randh's example, allows me to isolate input$origin, make it the "original" selected input
        origSelectInp = isolate(input$industryChoice)
        if(is.null(origSelectInp) || !(origSelectInp %in% chosenIndustry)) {
            origSelectInp = head(chosenIndustry, 1)
        }
        ##update destinations once an origin is chosen
        updateSelectInput(session, "stockChoice",
                          label = "Select a Company",
                          choices = c("-", sort(data$Name)),
                          selected = origSelectInp)
    })
    
    
    
    
    
    
    
    #will use this also as a condition on whether or not to plot the routes in a given month or entire year
    output$sp500shiller = renderText({
        paste("S&P 500's Shiller P/E: ", sp500shiller[1])
    })
    
    output$shillerTitle = renderText({
        #Shiller Ratio of ____ and S&P 500
        if(req(nrow(shiller_data()>0))){
            if(shiller_data()[1] > as.numeric(sp500shiller[1])){
                paste("Shiller P/E for ", company_data()$Name, ":", shiller_data()[1], "(Overvalued)")
                
            }
            else if(shiller_data()[1] == as.numeric(sp500shiller[1])){
                paste("Shiller P/E for ", company_data()$Name, ":", shiller_data()[1])
            }else{
                paste("Shiller P/E for ", company_data()$Name, ":", shiller_data()[1], "(Undervalued)")
            }
            

        }
    })
    
    #Shiller table output
    # output$shillerRatio = renderTable({
    #     shiller_data() %>% head(5)
    # 
    # 
    #     #shiller table there, eventually just add in the S&P 500 shiller
    #     #have an if/else that displays whether or not this stock is over or under valued based on the shiller pe ratio
    # })
    output$overOrUnder = renderText({
        paste("")
        
    })
    
    
    output$eps_title = renderText({
        if(req(nrow(shiller_data()>0))){
        paste(company_data()$Name,"'s Inflation-Adjusted Earnings Per Share: 2011 - 2020")
        }
    })
    
    output$eps_hist = renderPlot({
        #inflation adjusted data here
        ggplot(data = inflation_data(),mapping = aes(x = Year, y = inf_adj_eps))+
            geom_col(colour = "black", fill = "darkgreen")+xlab("Year")+ scale_x_continuous(breaks = seq(2011, 2020, by = 1))+
            ylab("Inflation-adjusted Earnings per Share")+
            theme_minimal()
    })
    
}


?geom_col
shinyApp(ui, server)

