### Indices e moedas

# Bibliotecas -------------------------------------------------------------
library(ipeadatar)
library(stringr)
library(sidrar)
library(GetBCBData)
library(BatchGetSymbols)
library(reticulate)
library(fredr)
library(GetTDData)
library(ggplot2)
library(rbcb)
library(BETS)
library(quantmod)
library(eurostat)
library(rdbnomics)
library(tidyverse)
library(WDI)
library(scales)
library(timetk)
library(ipeadatar)

# Moedas ------------------------------------------------------------------

# USD/BRL -----------------------------------------------------------------
df_ticker <- BatchGetSymbols("BRL=X",
                             first.date = "2011-01-01", 
                             last.date = Sys.Date()) %>% 
  pluck(2)
  
last_value <- df_ticker$price.adjusted[nrow(df_ticker)]

last_date <- df_ticker$ref.date[nrow(df_ticker)]


df_expec_dol <- tibble(x = c(last_date,
                             as.Date("2021-12-31")), 
                       y = c(last_value, 5.05))
                            
df_ticker %>% 
  
  ggplot(aes(x = ref.date, y = price.adjusted)) + 
  
  geom_line(color = "#181F40") +
  
  geom_line(data = df_expec_dol,
            
            aes(x = x,
                y = y),
            color = "red",
            linetype ="dashed",
            size = .2) +
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y - %m") + 
  
  labs(title = "USD/BRL") +
  
  ylab("") + 
    
  ggthemes::theme_fivethirtyeight() +
  
  theme(axis.text.x=element_text(angle = 90, 
                                 hjust = 1)) +
  
  labs(title = "USD/BRL", 
       caption = "\nFonte: Banco Central | Dapes Investimentos") +
  
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "serif"), 
        plot.caption = element_text(family = "serif")) + 

  annotate(x =df_expec_dol$x[2]+.0005 , y = last_value + .0005 , label = round(df_expec_dol$y[2], 2),"text",
           size = 3.2) +
  scale_y_continuous(breaks = round(seq(min(df_ticker$price.adjusted), max(df_ticker$price.adjusted), 0.5), 2))


# USD/CNY -----------------------------------------------------------------
DOLARCHINA <- BatchGetSymbols("CNY=X",
                              first.date = "2011-01-01",
                              last.date = Sys.Date()) %>% 
  pluck(2)


last_value_china <- DOLARCHINA$price.adjusted[nrow(DOLARCHINA)]
last_data_china <- DOLARCHINA$ref.date[nrow(DOLARCHINA)]

DOLARCHINA %>% 
  
  ggplot(aes(x = ref.date, y = price.close)) + 
  
  geom_line(color = "#181F40") +
  
  ylab("") +
  
  ggthemes::theme_fivethirtyeight() + 
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y - %m")  +
  labs(title = "USD/CNY", 
       caption = "\nFonte: Yahoo Finance | Dapes Investimentos") +
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "serif"), 
        plot.caption = element_text(family = "serif")) +
  theme(axis.text.x=element_text(angle = 90, 
                                 hjust = 1)) + 
  scale_y_continuous(breaks = round(seq(min(DOLARCHINA$price.adjusted), max(DOLARCHINA$price.adjusted), .3), 2))



# USD/EUR -----------------------------------------------------------------
DOLAREURO <- BatchGetSymbols("EUR=X",
                             first.date = "2011-01-01",
                             last.date = Sys.Date()) %>% 
  pluck(2)


last_value_euro <- DOLAREURO$price.adjusted[nrow(DOLAREURO)]
last_data_euro <- DOLAREURO$ref.date[nrow(DOLAREURO)]

DOLAREURO %>% 
  
  ggplot(aes(x = ref.date, y = price.close)) + 
  
  geom_line(color = "#181F40") +
  
  ylab("") +
  
  ggthemes::theme_fivethirtyeight()  +
  
  labs(title = "USD/EUR", 
       caption = "\nFonte: Yahoo Finance | Dapes Investimentos") +
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "serif"), 
        plot.caption = element_text(family = "serif")) + 
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y - %m") +
  theme(axis.text.x=element_text(angle = 90, 
                                 hjust = 1)) + 
  scale_y_continuous(breaks = round(seq(min(DOLAREURO$price.adjusted), max(DOLAREURO$price.adjusted), .05), 2))

# Oil price Brent ---------------------------------------------------------
brent <- ipeadata(code = "EIA366_PBRENT366") %>% 
  filter( date >= "2011-01-01")
  
last_value_brent <- brent$value[nrow(brent)]
last_data_brent <- brent$date[nrow(brent)]   
  
brent %>% 
  
  ggplot(aes(x = date, y = value)) + 
  
  geom_line(color = "#181F40") +
  
  ylab("") +
  
  ggthemes::theme_fivethirtyeight() +
  
  labs(title = "Oil Brent", 
       subtitle = "Doláres por barril",
       caption = "\nFonte: Energy Information Administration | Dapes Investimentos") +
  
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "serif"), 
        plot.caption = element_text(family = "serif"), 
        plot.subtitle = element_text(family = "serif", hjust = .5)) + 
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y - %m") +
  theme(axis.text.x=element_text(angle = 90, 
                                 hjust = 1)) + 
  scale_y_continuous(breaks = round(seq(min(brent$value), max(brent$value), 29), 0))

# Global price of Iron Ore ------------------------------------------------
getSymbols.FRED(Symbols = "PIORECRUSDM",
                src =  "FRED",
                from = "2011-01-01",
                to =  Sys.Date(),
                env=globalenv())

date <- index(PIORECRUSDM)

df_iron <- PIORECRUSDM %>% 
  as_tibble() %>% 
  mutate(date = date) %>% 
  rename("value" = PIORECRUSDM) %>% 
  filter(date >="2011-01-01")

last_value_iron_ore <- df_iron$value[nrow(df_iron)]
last_data_iron_ore <- df_iron$date[nrow(df_iron)]

df_iron %>% 
  
  ggplot(aes(x = date, y = value)) +
  
  geom_line(color = "#181F40") +
  
  ylab("") +
  
  ggthemes::theme_fivethirtyeight() +
  
  labs(title = "Global price of Iron Ore",
       subtitle = "Doláres por tonelada", 
       caption = "\nInternational Monetary Fund | Dapes Investimentos") +
  
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "serif"), 
        axis.text.x=element_text(angle = 90, 
                                 hjust = 1), 
        plot.subtitle = element_text(hjust = .5, family = "serif"), 
        plot.caption = element_text(family = "serif")) +
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y - %m") +
  scale_y_continuous(breaks = round(seq(min(df_iron$value), max(df_iron$value), 21), 0))

# Indices de bolsa --------------------------------------------------------

# S & P  ------------------------------------------------------------------
s_and_p <- BatchGetSymbols("^GSPC",
                           first.date = "2011-01-01",
                           last.date = Sys.Date()) %>% 
  pluck(2)

last_value_s_and_p <- s_and_p$price.adjusted[nrow(s_and_p)]
last_data_s_and_p <- s_and_p$ref.date[nrow(s_and_p)]

s_and_p %>% 
  
  ggplot(aes(x = ref.date, y = price.adjusted)) +
  
  geom_line(color = "#181F40") +
  
  ylab("") +
  
  ggthemes::theme_fivethirtyeight() +
  
  labs(title = "S&P 500",
       caption = "\nFonte: Yahoo Finance | Dapes Investimentos") +
  
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "serif"), 
        axis.text.x=element_text(angle = 90, 
                                 hjust = 1), 
        plot.caption = element_text(family = "serif")) + 
  
  scale_x_date(date_breaks = "3 month",
               date_labels = "%Y - %m") +
  
  scale_y_continuous(breaks = round(seq(min(s_and_p$price.adjusted), max(s_and_p$price.adjusted), 250), 0))

# MSCI CHINA --------------------------------------------------------------
MSCI_CHINA <- BatchGetSymbols("MCHI",
                              first.date = "2011-01-01",
                              last.date = Sys.Date()) %>% 
  pluck(2)


last_value_MSCI_CHINA <- MSCI_CHINA$price.adjusted[nrow(MSCI_CHINA)]
last_data_MSCI_CHINA <- MSCI_CHINA$ref.date[nrow(MSCI_CHINA)]

MSCI_CHINA %>% 
  
ggplot(aes(x = ref.date, y = price.adjusted)) +
  
  geom_line(color = "#181F40") +
  
  ylab("") +
  
  ggthemes::theme_fivethirtyeight() +
  
  labs(title = "MSCI CHINA", 
       caption = "\nFonte: Yahoo Finance | Dapes Investimentos") +
  
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "serif"), 
        plot.caption = element_text(family = "serif")) + 
  
  theme(axis.text.x=element_text(angle = 90, 
                                 hjust = 1)) + 
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y - %m") + 
  scale_y_continuous(breaks = round(seq(min(MSCI_CHINA$price.adjusted), max(MSCI_CHINA$price.adjusted), 18), 0))

# MSCI_EURO ---------------------------------------------------------------
MSCI_EURO <- BatchGetSymbols("EZU",
                             first.date = "2011-01-01",
                             last.date = Sys.Date()) %>% 
  pluck(2)

last_value_MSCI_EURO <- MSCI_EURO$price.adjusted[nrow(MSCI_EURO)]
last_data_MSCI_EURO <- MSCI_EURO$ref.date[nrow(MSCI_EURO)]

MSCI_EURO %>% 
  
  ggplot(aes(x = ref.date, y = price.adjusted)) +
  
  geom_line(color = "#181F40") +
  
  ylab("") +
  
  ggthemes::theme_fivethirtyeight() +
  
  labs(title = "MSCI EURO", 
       caption = "\nFonte: Yahoo Finance | Dapes Investimentos") +
  
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "serif"), 
        axis.text.x=element_text(angle = 90, 
                                 hjust = 1), 
        plot.caption = element_text(family = "serif")) + 
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y - %m") 
# IBOV --------------------------------------------------------------------

ibov <- BatchGetSymbols("^BVSP",
                        first.date = "2011-01-01",
                        last.date = Sys.Date()) %>% 
         pluck(2)



last_value_ibov <- ibov$price.adjusted[nrow(ibov)]
last_data_ibov <- ibov$ref.date[nrow(ibov)]

ibov %>% 
  
  ggplot(aes(x = ref.date, y = price.adjusted)) +
  
  geom_line(color = "#181F40") +
  
  ylab("") +
  
  ggthemes::theme_fivethirtyeight() +
  
  labs(title = "Ibovespa", 
       caption = "\nFonte: Yahoo Finance | Dapes Investimentos") +
  
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "serif"), 
        axis.text.x=element_text(angle = 90, 
                                 hjust = 1), 
        plot.caption = element_text(family = "serif")) +
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y - %m") 


# VIX ---------------------------------------------------------------------
VIX <- BatchGetSymbols("^VIX",
                        first.date = "2011-01-01",
                        last.date = Sys.Date()) %>% 
  pluck(2)


last_value_VIX <- VIX$price.adjusted[nrow(VIX)]
last_data_VIX <- VIX$ref.date[nrow(VIX)]

VIX %>%
  
ggplot(aes(x = ref.date, y = price.adjusted)) +
  
  geom_line(color = "#181F40") +
  
  ylab("") +
  
  ggthemes::theme_fivethirtyeight()  +
  
  labs(title = "CBOE Volatility Index - VIX", 
       caption = "\nFonte: Yahoo Finance | Dapes Investimentos") +
  
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "serif"), 
        axis.text.x=element_text(angle = 90, 
                                 hjust = 1), 
        plot.caption = element_text(family = "serif")) +
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y - %m") 

