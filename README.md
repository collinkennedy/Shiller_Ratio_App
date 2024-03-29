# Shiller Ratio Application
## OVERVIEW:

The Shiller Ratio (otherwise known as ‘CAPE’ or the Cyclically-Adjusted Price to Earnings Ratio) is a financial metric developed by Nobel Prize-winning Economist Robert Shiller.  Building off the work of value investing forefathers Benjamin Graham and David Dodd, Shiller  theorized that a moving average of inflation adjusted earnings, as opposed to nominal earnings  (which is typically used in standard price-to-earnings calculations), allowed for better predictability of future (real) dividends, and consequently future returns as well. 

My app allows a user to select an industry, and then a company, all of which are listed on the S&P 500. I have a Postgres database running on Google Cloud Platform (UPDATE: now running on a Postgres Heroku database) which includes several tables that include S&P 500 companies, their respective tickers, as well as a table of inflation for the past 100+ years. I have a CRON job set up on Github to update this database regularly, and includes code to scrape inflationdata.com for (surprise surprise) inflation data, as well as  https://www.multpl.com/shiller-pe for the current Shiller Ratio of the S&P 500. All of this information is written to the database, and then my app accesses the database for this information. 

For current price and earnings data, I used the https://www.alphavantage.co API. API’s calls are made directly in the Shiny app. I opted to do this as opposed to making 500+ API calls everyday (then writing to the database), because I only have access to the free version of the API, so it makes more sense to call the API on a need-to-know basis, depending on which handful of stocks the user selects. With this in mind, if you select too many stocks within a certain time frame when using the app (and consequently make that many API requests), an error may be thrown. I’ve suppressed most errors for now, but if you get: "no applicable method for 'pull' applied to an object of class "NULL"", this is due to making too many API requests.

Some other things to note: The AlphaVantage API only gives me access to a company’s earnings/ earnings per share. Some websites use diluted earnings per share (ie, GuruFocus). This is admittedly a better metric to use, but since I didn’t have access to it, I make do with good ‘ol earnings. In terms of actually calculating the inflation adjusted earnings per share (E10), I referred to this website: https://www.educba.com/cape-ratio/.

Here is the url for the app:
https://collinkennedy.shinyapps.io/shiller_app_2/

Update: my database on GCP is not currently running since my credit for the service expired. Currently looking for a free Postgres database solution so I can get the app back up and running
