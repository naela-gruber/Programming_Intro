#####
## GENERAL INFO
# Authors: Weine Nyberg Lagström & Naéla Angelina Gruber
# Last Updated: 24.05.24
# Topic: Stock Market Analysis Toolkit: Understanding Stock Behavior Across Market Phases
# Topic Explanation: The project analyzes daily stock returns across pre, during, and post-COVID-19 periods to understand stock behavior and assess statistical properties for investment. It includes functionalities for data validation, transformation, autocorrelation, and Ljung-Box tests to aid investment decision-making. 
# While we use pre, during, and post-COVID-19 periods to illustrate the functionality, any market event of interest can be studied eg. the subprime crisis, dotcom-bubble and other.
# Disclaimer: We have used ChatGPT to help us with our project, however we want to emphasize that the structure, intuition, and ideas were developed entirely by us.


##### 
## PREPARATION
# Load and install necessary packages
if (!require(quantmod)) { # Checks if the necessary package is already installed.
  install.packages("quantmod") # If not, it gets installed automatically.
}
library(quantmod) # This loads the necessary package.


#####
## USER INPUTS
# Here, periods and stocks of interest can be changed individually.

# Function to define the 3 periods we want to look at. (hardcoded)
#Input should be in format yyyy-mm-dd
# Periods should be in chronological order and not overlap, gaps are allowed
define_periods <- function() {
  periods <- list(
    period1 = c("2019-01-01", "2020-02-29"),  # Before COVID-19
    period2 = c("2020-03-01", "2021-12-31"),  # During COVID-19
    period3 = c("2022-01-01", "2023-12-31")   # After COVID-19
  )
  return(periods) # If this function gets called (define_periods()), our 3 periods get printed as a list.
}

# Function to define our stock tickers of interest. (hardcoded)
# Input should be in ticker format. Use both the ticker and the exchange or country if needed. Recommendation is 4 stocks
#Only listed companies can be analyzed. I.e. no private companies or companies that have turned private/public during the period.
define_tickers <- function() {
  tickers <- c("AAPL", "GOOGL", "MSFT", "HARVIA.HE")
  return(tickers)  # If this function gets called (define_tickers()), our 4 tickets get printed as a list.
}


#####
## VALIDATION FUNCTIONS
# We define a bunch of functions that are used later on to check that the input data meets certain criteria.

# Function to validate our desired date format (yyyy-mm-dd)
is_valid_date <- function(date) {
  return(grepl("^\\d{4}-\\d{2}-\\d{2}$", date) && !is.na(as.Date(date, format="%Y-%m-%d")))
} # This validates if a date is valid and of the format "YYYY-MM-DD".

# Function to validate periods
validate_periods <- function(periods) { # Checks if each period defined by start and end dates is valid.
  invalid_periods <- list()
  for (i in 1:length(periods)) { # This for loop checks every period that we defined above.
    start_date <- periods[[i]][1]
    end_date <- periods[[i]][2]
    
    # Call the date validation function for start_date and end_date
    if (!is_valid_date(start_date) || !is_valid_date(end_date) || as.Date(start_date) > as.Date(end_date)) { # Checks that the start and end date are valid and that the start date is before the end date.
      invalid_periods[[as.character(i)]] <- periods[[i]]
    }
  }
  return(invalid_periods) # This returns a list of any invalid periods.
}

# Function to check chronological order and overlaps
check_periods_order_and_overlaps <- function(periods) {
  invalid_order_periods <- list()
  for (i in 1:(length(periods) - 1)) { # Again we use a for loop to check every period individually.
    end_current_period <- as.Date(periods[[i]][2])
    start_next_period <- as.Date(periods[[i + 1]][1])
    
    # Check if periods are in chronological order and do not overlap
    if (end_current_period >= start_next_period) {
      invalid_order_periods[[paste0("Between Period ", i, " and Period ", i + 1)]] <- list(periods[[i]], periods[[i + 1]])
    }
  }
  return(invalid_order_periods) # This returns a list of any pairs of periods that violate the conditions of chronological order and no overlap.
}

# Function to calculate the number of days between two dates
calculate_total_days <- function(start_date, end_date) {
  all_dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  total_days <- length(all_dates)
  return(total_days) # It returns the count of days in the given range.
}

# Function to validate stock tickers
validate_tickers <- function(tickers) {
  valid_tickers <- c()
  invalid_tickers <- c()
  
  for (ticker in tickers) {
    stock_data <- tryCatch({
      getSymbols(ticker, src = "yahoo", auto.assign = FALSE)
      TRUE
    }, error = function(e) {
      FALSE
    })
    
    if (stock_data) {
      valid_tickers <- c(valid_tickers, ticker)
    } else {
      invalid_tickers <- c(invalid_tickers, ticker)
    }
  }
  
  return(list(valid = valid_tickers, invalid = invalid_tickers))
}

# Function to download daily returns for each stock and period
download_and_validate_returns <- function(tickers, periods) { # We use the above defined tickers and periods as inputs to the function.
  stock_returns <- list() # Creates an empty list to store the daily returns of each valid stock.
  valid_stocks <- c() # Creates an empty vector to store the ticker symbols of the valid stocks.
  invalid_stocks <- c() # Creates an empty vector to store the ticker symbols of the invalid stocks.
  
  for (ticker in tickers) { # for loop to retrieve stock data for each ticker symbol from the Yahoo Finance API.
    stock_data <- tryCatch({
      getSymbols(ticker, src = "yahoo", auto.assign = FALSE)
    }, error = function(e) { # This defines an anonymous function to handle potential errors that occur during the execution.
      return(NULL)
    })
    
    if (!is.null(stock_data)) { # It checks if stock_data is not null.
      valid_for_all_periods <- TRUE
      for (i in 1:length(periods)) { # Then it iterates through each period to validate if the stock data covers all specified periods.
        start_date <- periods[[i]][1]
        end_date <- periods[[i]][2]
        
        period_data <- stock_data[paste(start_date, end_date, sep = "/")]
        total_days <- calculate_total_days(start_date, end_date)
        actual_trading_days <- nrow(period_data) # Determines the actual number of trading days available in the selected data subset.
        
        cat("Ticker:", ticker, "Period:", i, "\n") # The cat function concatenates and prints its arguments.
        cat("Total days:", total_days, "\n")
        cat("Actual trading days:", actual_trading_days, "\n")
        
        if (actual_trading_days == 0 || actual_trading_days / total_days < 0.5) { # This checks if the actual number of trading days within a period is zero or less than half of the total days in that period, indicating insufficient data availability. We assume that in general stock exchanges are open ~ 250 days a year = ~70%. If we want to have min. 70% of trading days of data we can approximate 0,7*0,7 = ~ 0,5. This way, we don't have to gather all opening days for all exchanges.
          valid_for_all_periods <- FALSE
          break # If the condition above is met (trading days = 0 or <0.5 of total days), the variable valid_for_all_periods is set to FALSE, and the loop is terminated.
        }
      }
      
      if (valid_for_all_periods) { # This retrieves the corresponding stock data if valid_for_all_periods is true, indicating that there is sufficient data available for all periods.
        for (i in 1:length(periods)) {
          start_date <- periods[[i]][1]
          end_date <- periods[[i]][2]
          
          period_data <- stock_data[paste(start_date, end_date, sep = "/")] # Extracts the stock data for each period.
          period_returns <- periodReturn(period_data, period = 'daily', type = 'log') # And calculates the period returns for each stock within each period.
          stock_returns[[paste(ticker, "Period", i, sep = "_")]] <- period_returns # And stores them in the stock_returns list.
        }
        valid_stocks <- c(valid_stocks, ticker)
      } else {
        invalid_stocks <- c(invalid_stocks, ticker) # If the retrieval of the data was successful (TRUE), the stock ticker is added to the valid_stocks list.
      }
    } else {
      invalid_stocks <- c(invalid_stocks, ticker) # If the retrieval of the data was not successful (FALSE), the stock ticker is added to the invalid_stocks list.
    }
  }
  
  return(list(returns = stock_returns, valid = valid_stocks, invalid = invalid_stocks)) # This returns a list containing three elements: returns, which stores the retrieved stock returns data; valid, which contains the list of valid stock tickers for which data was successfully retrieved; and invalid, which contains the list of invalid stock tickers for which data retrieval failed.
}


#####
## DATA TRANSFORMATION FUNCTIONS

# Function to convert stock returns to long format for further analysis.
convert_to_long_format <- function(stock_returns) {
  long_format <- data.frame(Ticker = character(), # This part initializes an empty dataframe with columns for ticker, period, date, and return..
                            Period = character(),
                            Date = as.Date(character()),
                            Return = numeric(),
                            stringsAsFactors = FALSE)
  
  for (name in names(stock_returns)) { # This iterates over the names of elements in stock_returns...
    parts <- unlist(strsplit(name, "_")) # ...splits each name by underscores...
    ticker <- parts[1] #...extracts the ticker and period information...
    period <- paste(parts[2], parts[3]) #... and assigns them to variables ticker and period.
    
    # The following lines extracts data for each stock period, formats it with date and return columns, and adds ticker symbol and period columns before appending it to the long-format dataframe.
    period_data <- stock_returns[[name]]
    period_data <- data.frame(Date = index(period_data), Return = coredata(period_data))
    period_data$Ticker <- ticker
    period_data$Period <- period
    
    long_format <- rbind(long_format, period_data)
  }
  
  return(long_format) # It return the new long format of the data frame.
}


#####
## MAIN SCRIPT

# Define periods and tickers.
periods <- define_periods()
tickers <- define_tickers()

# Validate the defined periods
invalid_periods <- validate_periods(periods)

# Check if there are any invalid periods 
if (length(invalid_periods) > 0) { # If there are periods with an invalid date format, the following statement gets printed and a list with the invalid periods.
  cat("The following periods have invalid date formats or chronological order issues:\n")
  for (i in names(invalid_periods)) { # Again we use a for loop to iterate over all periods.
    cat(paste("Period", i, ":", invalid_periods[[i]][1], "to", invalid_periods[[i]][2], "\n"))
  }
} else { # If all periods have valid date formats, the following statement gets printed.
  cat("All periods have valid date formats.\n") 
  
  # Check chronological order and overlaps
  invalid_order_periods <- check_periods_order_and_overlaps(periods)
  
  if (length(invalid_order_periods) > 0) { # If there are periods with overlap or chronology issues, they will get printed.
    cat("The following periods have chronological order issues or overlaps:\n")
    for (i in names(invalid_order_periods)) { # Another for loop to iterate over the periods to check each one individually.
      cat(paste(i, ":", invalid_order_periods[[i]][[1]][1], "to", invalid_order_periods[[i]][[1]][2], "and", invalid_order_periods[[i]][[2]][1], "to", invalid_order_periods[[i]][[2]][2], "\n"))
    }
  } else { # If there are no such issues, the follwing sentence gets printed.
    cat("All periods are in chronological order and do not overlap.\n")
    
    # Display the defined periods
    cat("The following periods are defined:\n")
    for (i in 1:length(periods)) { # This iterates through each defined period, printing out its start and end dates alongside a label indicating the period number.
      cat(paste("Period", i, ":", periods[[i]][1], "to", periods[[i]][2], "\n"))
    }
  }
}

# Validate the defined tickers
validation_results <- validate_tickers(tickers) # We validate the tickers and store them in a new variable.
valid_tickers <- validation_results$valid
invalid_tickers <- validation_results$invalid

# Display the results
if (length(invalid_tickers) > 0) {
  cat("The following tickers are invalid or not listed:\n")
  for (ticker in invalid_tickers) {
    cat(paste("Ticker:", ticker, "\n"))
  }
} else {
  cat("All tickers are valid and listed.\n")
  
  # Display the valid tickers
  cat("The following tickers are defined:\n")
  for (i in 1:length(valid_tickers)) {
    cat(paste("Ticker", i, ":", valid_tickers[i], "\n"))
  }
  
  # Download and validate daily returns for each stock and period
  return_results <- download_and_validate_returns(valid_tickers, periods)
  stock_returns <- return_results$returns
  valid_stocks <- return_results$valid
  invalid_stocks <- return_results$invalid
  
  # Display the valid and invalid stocks
  if (length(invalid_stocks) > 0) {
    cat("\nThe following stocks have insufficient data for at least one period and are excluded:\n")
    for (ticker in invalid_stocks) {
      cat(paste("Ticker:", ticker, "\n"))
    }
  }
  
  cat("\nThe following stocks have sufficient data for all periods:\n")
  for (ticker in valid_stocks) {
    cat(paste("Ticker:", ticker, "\n"))
  }
  
  # Convert stock returns to long format
  long_format_returns <- convert_to_long_format(stock_returns)
  
  #We plot the daily returns for the stocks to get an overview of the market periods
  # Function to plot daily returns for each valid stock
  plot_stock_returns <- function(long_format_returns, valid_stocks) {
    # Determine the number of rows and columns for the plot layout
    n <- length(valid_stocks)
    rows <- ceiling(sqrt(n))
    cols <- ceiling(n / rows)
    
    # Set up the multi-panel plot layout
    par(mfrow = c(rows, cols), mar = c(4, 4, 2, 1))
    
    # Define colors for the periods
    period_colors <- c("Period 1" = "blue", "Period 2" = "red", "Period 3" = "green")
    
    # Plot each stock's daily returns
    for (ticker in valid_stocks) {
      stock_data <- subset(long_format_returns, Ticker == ticker)
      
      # Create an empty plot with the appropriate limits
      plot(stock_data$Date, stock_data$daily.returns, type = "n",
           main = paste("Daily Returns for", ticker),
           xlab = "Date", ylab = "Daily Return")
      
      # Add lines for each period
      for (period in unique(stock_data$Period)) {
        period_data <- subset(stock_data, Period == period)
        lines(period_data$Date, period_data$daily.returns, col = period_colors[period])
      }
      
      # Add a legend
      legend("topright", legend = unique(stock_data$Period), col = period_colors, lty = 1, bty = "n")
    }
    
    # Reset the plotting parameters to default
    par(mfrow = c(1, 1))
  }
  
  # Call the plot function for returns
  plot_stock_returns(long_format_returns, valid_stocks)
  
  # Transform daily.returns to natural logarithm for mathematical convenience
  long_format_returns$log_returns <- log(1 + long_format_returns$daily.returns)
  

  
  # Function to plot autocorrelations for each valid stock, with separate subplots for each period
  plot_stock_autocorrelations <- function(long_format_returns, valid_stocks, periods) {
    # Define period names
    period_names <- c("Period 1", "Period 2", "Period 3")
    
    # Plot each stock's autocorrelations for each period
    for (ticker in valid_stocks) {
      # Create a new plot window for each stock
      dev.new(height = 12, width = 8)  # Adjust the height and width for better readability
      
      stock_data <- subset(long_format_returns, Ticker == ticker)
      
      # Set up the multi-panel plot layout for three rows (one for each period)
      par(mfrow = c(3, 1), mar = c(4, 4, 2, 4), oma = c(1, 1, 2, 3))
      
      for (i in seq_along(periods)) {
        period <- periods[[i]]
        period_name <- period_names[i]
        
        period_data <- subset(stock_data, Date >= as.Date(period[1]) & Date <= as.Date(period[2]))
        
        if (nrow(period_data) > 0) {
          acf_data <- acf(period_data$log_returns, plot = FALSE)
          
          # Plot the autocorrelations
          plot(acf_data, main = "", ylim = c(-1, 1))
          mtext(period_name, side = 4, line = 2, cex = 1.0)  # Add the period title to the right
        } else {
          plot.new()
          title(main = paste("No data for", ticker, "-", period_name))
        }
      }
      
      # Add a main title for each stock's plot
      mtext(paste("Autocorrelation Plots for", ticker), outer = TRUE, cex = 1.5)
    }
    
    # Reset the plotting parameters to default
    par(mfrow = c(1, 1))
  }
  

  plot_stock_autocorrelations(long_format_returns, valid_stocks, periods)  # Call plot function of autocorrelation using the valid stocks and periods
  
  # Function to perform Ljung-Box test for each stock and each period and return results in a data frame
  # The Ljung-Box test is a statistical test used to determine whether there are significant autocorrelations in a time series such as the stock returns we analyse
  #  A low p-value indicates significant autocorrelation, suggesting that further analysis is needed
  perform_ljung_box_tests <- function(long_format_returns, valid_stocks, periods) {
    # Create an empty data frame to store the results
    results <- matrix(NA, nrow = length(valid_stocks), ncol = length(periods))
    rownames(results) <- valid_stocks
    colnames(results) <- c("Period 1", "Period 2", "Period 3")
    
    # Perform Ljung-Box test for each stock and each period
    for (ticker in valid_stocks) {
      stock_data <- subset(long_format_returns, Ticker == ticker)
      
      for (i in seq_along(periods)) {
        period <- periods[[i]]
        period_name <- colnames(results)[i]
        
        period_data <- subset(stock_data, Date >= as.Date(period[1]) & Date <= as.Date(period[2]))
        
        if (nrow(period_data) > 0) {
          ljung_box_test <- Box.test(period_data$log_returns, lag = 20, type = "Ljung-Box")
          
          # Store the results
          results[ticker, period_name] <- ljung_box_test$p.value
        } else {
          # Store NA if no data
          results[ticker, period_name] <- NA
        }
      }
    }
    
    return(as.data.frame(results))
  }
  
  ##Call the ljung-box function
  ljung_box_results <- perform_ljung_box_tests(long_format_returns, valid_stocks, periods)
  
  # Display the results as a table
  print("Ljung-Box p-values:")
  print(ljung_box_results)
}

