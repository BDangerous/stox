
#' @param ticker of stock 
#' @return stock_table data frame of stock
#' @export
stox_valuation<- function(ticker){
  
  #assign url to appropriate web address
  url <- paste0("https://finance.yahoo.com/quote/",ticker, "/key-statistics?p=", ticker)
  
  #read webpage and extract table
  webpage <- read_html(url)
  
  stock_table <- webpage %>%
    rvest::html_nodes("table") %>%
    .[2] %>%
    rvest::html_table(header = FALSE, fill = TRUE) %>%
    as.data.frame()
  
  #add new row to table and assign ticker to row
  stock_table<-rbind(data.frame(X1= "Ticker",X2 = ticker), stock_table)
  
  #transpose table
  stock_table<-data.table::transpose(stock_table)
  
  #remove table labels
  stock_table<-stock_table[-1,]
  
  #rename columns
  colnames(stock_table)<- c('Ticker','Market Cap','EV','Trailing PE','Forward PE','PEG', 'Price/Book','Price/Sales', 'EV/Revenue','EV/EBITDA')
  
  return (stock_table)
  
}

devtools::document()
devtools::load_all()
devtools::install()
