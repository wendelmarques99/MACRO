
# China -------------------------------------------------------------------

# Consumer price index ----------------------------------------------------

# Inflation INDEX 2015 = 100
getSymbols.FRED("CPALTT01CNM657N",
                src = "FRED",
                env = globalenv())

date <- index(CPALTT01CNM657N)


df_infla_china <-  CPALTT01CNM657N %>% 
  
  as_tibble() %>% 
  
  mutate(date = date) %>% 
  
  rename("value" = CPALTT01CNM657N) %>% 
  
  filter(date>="2011-01-01")



df_infla_china %>% 
  
  ggplot(aes(x = date,
             y = value)) +  
  
  ggthemes::theme_fivethirtyeight() +
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y - %m") +  
  
  theme(axis.text.x=element_text(angle = 90, 
                                 hjust = 1)) +
  
  geom_bar(stat = "identity", 
           fill = "#181F40",
           position = "dodge", width = 15) + 
  
  # geom_point(color = "red") +
  
  # geom_text(aes(label = round(value, 1),
  #               
  #               vjust = if_else(value > 0, -.7, 1.5)),
  #           color = "black") +
  
  xlab("") +
  
  labs(title = "Inflação - China",
       caption = "\nFonte: Organization for Economic Co-operation and Development | Dapes Investimentos", 
       subtitle = " Consumer Price Index: All items: Total: Total for China\nUnidade: Taxa de variação do mesmo período anterior") + 
  
  theme(plot.title = element_text(hjust = 0.5, 
                                  family = "serif"),
        plot.subtitle = element_text(hjust = .5, 
                                     family = "serif"), 
        plot.caption = element_text(family = "serif")) +
  
  scale_y_continuous(labels = percent_format(scale = 1), breaks = seq(min(df_infla_china$value),
                                                                      max(df_infla_china$value), .5)) + 
  
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1))

# GDP CHINA ---------------------------------------------------------------
getSymbols.FRED("CHNGDPNQDSMEI",
                src = "FRED",
                to = Sys.Date(),
                env = globalenv())


date <- index(CHNGDPNQDSMEI)

df_GDP_china_FRED <- CHNGDPNQDSMEI %>% 
  as_tibble() %>% 
  mutate(date = date) %>% 
  mutate(ret = CHNGDPNQDSMEI/lag(CHNGDPNQDSMEI, n = 4) -1, 
         ret = ret*100) %>% 
  filter(date >="2011-01-01")  %>% 
  
  mutate(data_q = c(rep(1:4, 10), 1),
         data_q = as.yearqtr(
           paste0(format(date, "%Y"), " Q", data_q))) %>% 
  
  rename("value" = "CHNGDPNQDSMEI")


df_GDP_china_FRED %>% 
  
  ggplot(aes(x = data_q,
             y = ret)) +
  
  geom_bar(stat = "identity", 
           fill = "#181F40",
           position = "dodge",
           width = .1) +
  
  geom_text(aes(label = paste0(round(ret, 2), ""),
                
                vjust = if_else(ret > 0, -.8, 1.5)),
            color = "black", size = 1.9) +
  
  xlab("") +
  
  ylab("") +
  
  labs(title = "PIB - China",
       caption = "\nFonte: Organization for Economic Co-operation and Development | Dapes Investimentos", 
       subtitle = "Em Chinese Yuans\nUnidade: Taxa de crescimento do mesmo período do ano anterior\n") + 
  
  ggthemes::theme_fivethirtyeight() +
  
  theme(plot.title = element_text(hjust = 0.5, family = "serif"), 
        plot.subtitle = element_text(hjust = 0.5, family = "serif"), 
        plot.caption = element_text(family = "serif")) +
  
  scale_x_yearqtr(breaks = df_GDP_china_FRED$data_q) + 
  theme(axis.text.y = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1))


# PIB PERCAPITA -----------------------------------------------------------
getSymbols.FRED("NYGDPPCAPKDCHN",
                src = "FRED",
                to = Sys.Date(),
                env = globalenv())

date <- index(NYGDPPCAPKDCHN)

df_pibperc_china <- NYGDPPCAPKDCHN %>% 
  
  as_tibble() %>% 
  
  mutate(date = date) %>%
  
  rename("value" = NYGDPPCAPKDCHN) %>% 
  
  filter(date >="2006-01-01")


df_pibperc_china %>% 
  
 ggplot(aes(x = date,
           y = value)) +  
  
  ggthemes::theme_fivethirtyeight()  +
  
  geom_line(size = .5) +
  
  xlab("") +
  
  ylab("") +
  
  labs(title = "PIB PER CAPITA ANUAL - China",
       
       caption = "\nFonte: World Bank | Dapes Investimentos",
       
       subtitle = "Em doláres") + 
  
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "serif"),
        
        plot.subtitle = element_text(hjust = .5, family = "serif"), 
        plot.caption = element_text(family = "serif"))  + 
  geom_text(aes(label = format(round(value),
                               big.mark = ",") ,
                
                vjust = if_else(value > 0, -.8, 1.5)),
            
            color = "black", size = 4) 


# INdustrial production CHNPRINTO01IXPYM
getSymbols.FRED("CHNPRINTO01IXPYM",
                src = "FRED",
                to = Sys.Date(),
                env = globalenv())

date <- index(CHNPRINTO01IXPYM)

df_produc_industrial <- CHNPRINTO01IXPYM %>% 
  as_tibble() %>%  
  rename("value" = CHNPRINTO01IXPYM) %>% 
  mutate(date = date) %>% 
  filter(date >= "2011-01-01") %>% 
  na.omit() %>% 
  mutate(ret = value/100 -1, 
         ret = ret*100)


df_produc_industrial %>% 
  
  ggplot(aes(x = date, y = ret))+
  
  geom_bar(stat = "identity", 
                         fill = "#181F40",
                         position = "dodge",
                         width = 15)+
  labs(title = "Produção industrial - China",
       caption = "Fonte: Organization for Economic Co-operation and Development | Dapes Investimentos", 
       subtitle = "Unidade: Taxa de crescimento do mesmo período do ano anterior") + 
  
  ggthemes::theme_fivethirtyeight() +
  
  theme(plot.title = element_text(hjust = .5, 
                                  family = "serif"), 
        plot.subtitle = element_text(hjust = .5, 
                                     family = "serif")) + 
  
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y - %m") +
  
  scale_y_continuous(breaks = seq(min(df_produc_industrial$ret), max(df_produc_industrial$ret), 5), 
                     labels = percent_format(scale = 1))



# Unemployment ------------------------------------------------------------


readxl::read_excel("Unemplyment_China.xlsx") %>% 
  mutate(ret = `Unemployment Rate`, 
         Date = as.Date(Date)) %>% 
  
  filter(Date >= "2011-01-01") %>% 
  
  ggplot(aes(x = Date,
             y = ret)) + 
  
  ggthemes::theme_fivethirtyeight() +
  
  geom_line(color = "#181F40", 
            size = .5) +
  
  labs(title = "Taxa de desemprego trimestral - China",
       caption = "\nFonte: National Bureau of Statistics of China  | Dapes Investimentos")  +
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y - %m") + 
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1), 
        plot.title = element_text(hjust= .5, family = "serif"), 
        plot.caption = element_text(family = "serif")) +
  scale_y_continuous(labels = percent_format(scale = 1))






# 10 years ----------------------------------------------------------------

url <- "http://www.worldgovernmentbonds.com/bond-historical-data/china/10-years/"

ten_years_china <- rvest::read_html(url) %>% 
  html_table() %>% 
  `[[`(2) %>% 
  mutate(across(.cols = Min:Max, ~.x %>% str_squish() %>% str_replace("%", ""))) %>% 
  tidyr::pivot_longer(., cols = c(Min, Max)) %>% 
  mutate(value = word(string = value, 1)) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(Year = paste0(Year, "-01-01") %>% as.Date()) 

ten_years_china %>% 
  ggplot(., aes(x = Year, y = value, color = name)) +
  geom_line() +
  ggthemes::theme_fivethirtyeight()  +
  labs(title = "China 10 Years Bond", 
       caption = "Fonte: World Government Bonds | Dapes Investimentos", color = "") + 
  theme(plot.title = element_text(hjust = .5, family = "serif"), 
        plot.caption = element_text( family = "serif")) + 
  geom_label(data = filter(ten_years_china, Year == last(Year)), 
             mapping = aes(label = round(value, 2)), color = "#181F40") + 
  scale_colour_manual(values = c("#181F40", "red")) + 
  
  scale_y_continuous(breaks = round(seq(min(ten_years_china$value), max(ten_years_china$value), .5), 2), 
                     label = percent_format(scale = 1)) + 
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y")


# Txa de juros China ------------------------------------------------------
getSymbols.FRED("IRSTCB01CNM156N",
                src = "FRED",
                to = Sys.Date(),
                env = globalenv())

date <- index(IRSTCB01CNM156N)

df_china_tx <- IRSTCB01CNM156N %>% 
  as_tibble() %>%  
  rename("value" = IRSTCB01CNM156N) %>% 
  mutate(date = date)


df_china_tx %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line(color = "#181F40", 
            size = .5) +
  ggthemes::theme_fivethirtyeight() +
  labs(title = "Taxa de juros - China",
       caption = "\nFonte: Organization for Economic Co-operation and Development | Dapes Investimentos") +
  theme(plot.title = element_text(family = "serif", hjust = .5), 
        plot.caption = element_text(family = "serif")) + 
  geom_label(data = filter(df_china_tx,
                           date == last(date)),
             aes(label = value), color = "#181F40") + 
  
  scale_y_continuous(labels = function(x) paste0(x*1, "%"))
  
  

  

