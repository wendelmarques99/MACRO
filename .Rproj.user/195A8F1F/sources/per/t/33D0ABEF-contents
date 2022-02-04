
# EUA ---------------------------------------------------------------------

# PIB USA -  ------------------------------------------
# Percent Change from Preceding Period,
# Seasonally Adjusted Annual Rate

getSymbols.FRED(Symbols = "A191RL1Q225SBEA",
                src =  "FRED",
                from = "2011-01-01",
                to =  Sys.Date(),
                env = globalenv())

date <- index(A191RL1Q225SBEA)

df_gdp <- A191RL1Q225SBEA %>% 
  as_tibble() %>% 
  mutate(date = date) %>% 
  rename("value" = A191RL1Q225SBEA) %>% 
  filter(date >= "2011-01-01")


# Data trimestral 
df_gdp_eua <- df_gdp %>% 
  
mutate(data_q = c(rep(1:4, 10), 1, 2),
       data_q = as.yearqtr(
         paste0(format(date, "%Y"), " Q", data_q)))



df_gdp_eua %>% 
  
  ggplot(aes(x = data_q,
             y = value)) +
  
  geom_bar(stat = "identity", 
           fill = "#181F40",
           position = "dodge",
           width = .1)  +
  
  xlab("") +
  
  ylab("") +
  
  labs(title = "PIB - EUA - Trimestral",
       caption = "\nFonte: U.S. Bureau of Economic Analysis | Dapes Investimentos",
       subtitle = "Unidade: Taxa de variação do período anterior\nAjustado sazonalmente") + 
  
  ggthemes::theme_fivethirtyeight() +
  
  theme(plot.title = element_text(hjust = 0.5, family = "serif"), 
        plot.subtitle = element_text(family = "serif", hjust = .5), 
        plot.caption = element_text(family = "serif"), 
        axis.text.x=element_text(angle = 90, 
                                 hjust = 1)) +
  
  scale_x_yearqtr(breaks = df_gdp_eua$data_q) + 
  scale_y_continuous(breaks = seq(min(df_gdp_eua$value), max(df_gdp_eua$value), 5), 
                     labels = percent_format(scale = 1)) + 
  
  geom_text(aes(label = round(value, 2),
                vjust = if_else(value > 0, -1.4, .7)),
            color = "black", 
            size = 2)

# Taxa de desemprego ------------------------------------------------------
getSymbols.FRED("UNRATE",
                src = "FRED", 
                to =  Sys.Date(), 
                env = globalenv())

date <- index(UNRATE)

df_unrate <- UNRATE %>% 
  as_tibble() %>% 
  mutate(date = date) %>% 
  rename("value" = UNRATE) %>% 
  filter(date >= "2011-01-01")


df_unrate %>% 
 
  ggplot(aes(x = date,
             y = value)) +  
  
  ggthemes::theme_fivethirtyeight()  +  
  
  theme(axis.text.x=element_text(angle = 90, 
                                 hjust = 1)) +
  
  geom_bar(stat = "identity", 
           fill = "#181F40",
           position = "dodge", width = 15) + 
  
  geom_point(color = "#181F40") +
  
  xlab("") +
  
  ylab("%") +
  
  labs(title = "Taxa de desemprego mensal - EUA\n",
       caption = "\nFonte: U.S. Bureau of Economic Analysis | Dapes Investimentos") + 
  
  theme(plot.title = element_text(hjust = 0.5, family = "serif"), 
        axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.caption = element_text(family = "serif")) +
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y-%m") + 
  
  scale_y_continuous(labels = percent_format(scale = 1), 
                     breaks = seq(min(df_unrate$value), max(df_unrate$value), 1)) 

# Consumer price index EUA ------------------------------------------------

# Inflacao EUA
getSymbols.FRED("CPALTT01USM659N",
                src = "FRED",
                to = Sys.Date(),
                env = globalenv())





date <- index(CPALTT01USM659N)

df_inflation <- CPALTT01USM659N %>% 
  
  as_tibble() %>% 
  
  mutate(date = date) %>%
  
  rename("value" = CPALTT01USM659N) %>% 
  
  filter(date >="2011-01-01")


df_inflation %>% 
  
  ggplot(aes(x = date,
             y = value)) +  
  
  ggthemes::theme_fivethirtyeight() +  
  
  theme(axis.text.x=element_text(angle = 90, 
                                 hjust = 1)) + 
  geom_point(color = "red") +
  
  geom_bar(stat = "identity", 
           fill = "#181F40",
           position = "dodge", width = 15) +
  
  xlab("") +
  
  ylab("%") +
  
  labs(title = "Inflação - EUA",
       caption = "\nFonte: Organization for Economic Co-operation and Development | Dapes Investimentos", 
       subtitle = "Consumer Price Index: Total All Items for the United States\nUnidade: Taxa de crescimento do mesmo período do ano anterior") + 
  

  theme(plot.title = element_text(hjust = 0.5, family = "serif"),
        plot.subtitle = element_text(hjust = .5, family = "serif"), 
        plot.caption = element_text(family = "serif")) +
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y-%m") 

# PIB Per capita -----------------------------------------------------------
getSymbols.FRED("A939RC0Q052SBEA",
                src = "FRED",
                to = Sys.Date(),
                env = globalenv())


date <- index(A939RC0Q052SBEA)

df_gdp_percapita <- A939RC0Q052SBEA %>% 
  
  as_tibble() %>% 
  
  mutate(date = date) %>%
  
  rename("value" = A939RC0Q052SBEA) %>% 
  
  filter(date >="2011-01-01") %>% 
  
  mutate(data_q = c(rep(1:4, 10), 1, 2),
         data_q = as.yearqtr(
           paste0(format(date, "%Y"), " Q", data_q)))

df_gdp_percapita %>% 
  
  ggplot(aes(x = date,
             y = value)) +  
  
  ggthemes::theme_fivethirtyeight() +
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y - %m") +  
  
  theme(axis.text.x = element_text(angle = 90, 
                                 hjust = 1)) + 
  geom_line(size = .5) +
  
  xlab("") +
  
  ylab("") +
  
  labs(title = "PIB PER CAPITA ANUAL - EUA",
       
       caption = "\nFonte: U.S. Bureau of Economic Analysis | Dapes Investimentos",
       
       subtitle = "Em doláres") + 
  
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "serif"), 
        
        axis.text.y = element_blank(),
        
        plot.subtitle = element_text(hjust = .5, family = "serif"), 
        plot.caption = element_text(family = "serif")) +
  
  geom_label(data = filter(df_gdp_percapita,
                           date == last(date)),
             aes(label = value))
  
  

# Balanca comercial EUA ---------------------------------------------------
getSymbols.FRED("BOPGSTB",
                src = "FRED",
                to = Sys.Date(),
                env = globalenv()) 


date <- index(BOPGSTB)

df_balcomercial <- BOPGSTB %>% 
  
  as_tibble() %>% 
  
  mutate(date = date) %>%
  
  rename("value" = BOPGSTB) %>%
  
  filter(date >= "2011-01-01") %>% 
  
  mutate(value = value/1000)


df_balcomercial %>% 
  
  ggplot(aes(x = date,
             y = value)) +  
  
  ggthemes::theme_fivethirtyeight() +
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y - %m") +  
  
  theme(axis.text.x = element_text(angle = 90, 
                                 hjust = 1)) +
  
  geom_bar(stat = "identity", 
           fill = "#181F40",
           position = "dodge", 
           width = 15)  + 
  
  geom_point(color = "red") +
  
  xlab("") +
  
  ylab("%") +
  
  labs(title = "Balança comercial - EUA",
       caption = "\nFonte: U.S. Census Bureau | Dapes Investimentos", 
       subtitle = "Em Bilhões de doláres") + 
  
  theme(plot.title = element_text(hjust = 0.5, family = "serif"),
        plot.subtitle = element_text(hjust = .5, family = "serif"), 
        plot.caption = element_text(family = "serif")) + 
  scale_y_continuous(breaks = round(seq(min(df_balcomercial$value), max(df_balcomercial$value), 5)))


# Total debt public as percent of GDP -------------------------------------
getSymbols.FRED("GFDEGDQ188S",
                src = "FRED",
                to = Sys.Date(),
                env = globalenv())

date <- index(GFDEGDQ188S)


df_Total_debt <- GFDEGDQ188S %>% 
  
  as_tibble() %>% 
  
  mutate(date = date) %>%
  
  rename("value" = GFDEGDQ188S) %>% 
  
  filter(date >="2011-01-01") %>% 
  
  mutate(data_q = c(rep(1:4, 10), 1),
         data_q = as.yearqtr(
           paste0(format(date, "%Y"), " Q", data_q)))
# 
df_Total_debt %>% 
  
  ggplot(aes(x = data_q,
             y = value)) +  
  
  ggthemes::theme_fivethirtyeight() +   
  
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +
  
  geom_bar(stat = "identity", 
           fill = "#181F40",
           position = "dodge",
           width = .1) + 
  
  xlab("") +
  
  ylab("%") +
  
  labs(title = "Dívida Pública Total - EUA",
       caption = "\nFonte: U.S. Office of Management and Budget | Dapes Investimentos", 
       subtitle = "% do PIB\n")  +  
  
    scale_x_yearqtr(breaks = df_Total_debt$data_q) + 

  theme(plot.title = element_text(hjust = 0.5, family = "serif"),
        plot.subtitle = element_text(hjust = .5, family = "serif"), 
        plot.caption = element_text( family = "serif")) + 
  
  geom_text(aes(label = round(value, 0),
                vjust = if_else(value > 0, -1.4, .7)),
            color = "black", 
            size = 2)

# 10 y yield -------------------------------------------------------------- 
getSymbols.FRED("DGS10",
                src = "FRED",
                to = Sys.Date(),
                env = globalenv())



date <- index(DGS10)

df_10_yield <- DGS10 %>% 
  
  as_tibble() %>% 
  
  mutate(date = date) %>%
  
  rename("value" = DGS10) %>% 
  
  filter(date >="2019-01-01") %>% 
  
  na.omit()


last_value_10_yield <- df_10_yield$value[nrow(df_10_yield)]
last_data_10_yield <- df_10_yield$date[nrow(df_10_yield)]


df_10_yield %>% 
  
  ggplot(aes(x = date, y = value)) + 
  
  geom_line(size = .5, color = "#181F40") +
  
  ggthemes::theme_fivethirtyeight() + 
  
  xlab("") +
  
  ylab("") + 
  
  labs(caption = "\nFonte: Board of Governors of the Federal Reserve System (US) | Dapes investimentos", 
       title = "10 - Year Treasury - US", subtitle = "%")  +
  
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%Y - %m") +
  
  theme(axis.text.x=element_text(angle = 90, 
                                 hjust = 1)) +
  
  theme(plot.title = element_text(hjust = 0.5, family = "serif"),
        plot.subtitle = element_text(hjust = .5), 
        plot.caption = element_text(family = "serif")) + 
  
  scale_y_continuous(labels = percent_format(scale = 1)) + 
  
  
  geom_label(data = filter(df_10_yield,
                           date == last(date)),
             aes(label = value))



# Txa de juros EUA --------------------------------------------------------
getSymbols.FRED(c("DFEDTARU", "DFEDTARL"),
                src = "FRED",
                to = Sys.Date(),
                env = globalenv())


# Limite inferior
limite_inferior_df <- DFEDTARL %>% 
  
  as_tibble() %>% 
  
  mutate(date = date) %>%
  
  rename("value" = DFEDTARL) %>% 
  
  filter(date >="2019-01-01") %>% 
  
  na.omit()

date <- index(DFEDTARU)

# Limite superior
df_tx_juros_eua <- DFEDTARU %>% 
  
  as_tibble() %>% 
  
  mutate(date = date) %>%
  
  rename("value" = DFEDTARU) %>% 
  
  filter(date >="2011-01-01") %>% 
  
  na.omit()


df_tx_juros_eua %>% 
  left_join(limite_inferior_df, by = "date") %>% 
  rename("Inferior" = value.y) %>% 
  rename("Superior" = value.x) %>% 
  pivot_longer(., cols = c(Inferior, Superior)) %>% 
  ggplot(aes(x = date, y = value, color = name)) + 
  geom_line(size = .7) +
  ggthemes::theme_fivethirtyeight() +
  labs(color='Limite',
       title = "Federal Funds Target Range",
       caption = "Board of Governors of the Federal Reserve System (US) | Dapes Investimentos") +
  theme(legend.position="top", 
        legend.text = element_text(family = "serif"), 
        legend.title = element_text(family = "serif"), 
        plot.title = element_text(hjust = .5, family = "serif")) +
  
  scale_colour_manual(values = c("#181F40", "red")) + 
  
  geom_label(data = filter(df_tx_juros_eua,
                           date == last(date)),
             aes(label = value), color = "#181F40") + 
  
  geom_label(data = filter(limite_inferior_df,
                           date == last(date)),
             aes(label = value), color = "#181F40") + 
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) 
  
  
  
  
  
# Industrial Production ---------------------------------------------------
getSymbols.FRED("INDPRO",
                src = "FRED",
                to = Sys.Date(),
                env = globalenv())

date <- index(INDPRO)

ind_prodoc <- INDPRO %>% 
  
  as_tibble() %>% 
  
  mutate(date = date) %>% 
  
  filter(date >="2011-12-01") %>% 
  
  mutate(ret = INDPRO/lag(INDPRO, n = 12) -1, 
         
         ret = ret*100) %>% 
  
  na.omit()  
  
  
  ind_prodoc %>% 
    
  filter(date > "2011-12-01") %>% 
  
  rename("value" = INDPRO)  %>% 
  
  ggplot(aes(x = date, y = ret)) + 
  
  ggthemes::theme_fivethirtyeight()+
  
  geom_bar(stat = "identity", 
           fill = "#181F40",
           position = "dodge", 
           size = 17, width = 10) +
  
  xlab("") +
  
  ylab("%") +
  
  labs(title = "Produção Industrial - EUA",
       caption = "\nFonte: Board of Governors of the Federal Reserve System (US) | Dapes Investimentos", 
       subtitle = "Unidade: Taxa de crescimento do mesmo período do ano anterior") + 
  
  theme(plot.title = element_text(hjust = 0.5, family = "serif"),
        plot.subtitle = element_text(hjust = .5, family = "serif"), 
        plot.caption = element_text(family = "serif")) +
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y - %m") +
  
  theme(axis.text.x = element_text(angle = 90, 
                                 hjust = 1)) + 
  scale_y_continuous(labels = percent_format(scale = 1), 
                     breaks = seq(min(ind_prodoc$ret), max(ind_prodoc$ret), 5))
  
  
                     