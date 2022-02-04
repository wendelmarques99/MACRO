


# China 10 years 
url <- "https://www.investing.com/rates-bonds/china-10-year-bond-yield-historical-data"


rvest::read_html(url) %>% 
  html_table() %>% 
  pluck(1)
  
