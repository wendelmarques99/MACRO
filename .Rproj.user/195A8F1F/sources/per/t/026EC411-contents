###### Brasil 


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
library(kableExtra)
library(gt)

# Produto REAL ------------------------------------------------------------
getSymbols.FRED("NAEXKP01BRQ657S",
                src = "FRED",
                to = Sys.Date(),
                env = globalenv())

date <- index(NAEXKP01BRQ657S)

df_GDP_br <- NAEXKP01BRQ657S %>% 
  as_tibble() %>% 
  mutate(date = date) %>% 
  rename("value" = NAEXKP01BRQ657S) %>% 
  filter(date >="2011-01-01")


df_gdp_br <- df_GDP_br %>% 
  
  mutate(data_q = c(rep(1:4, 10), 1),
         data_q = as.yearqtr(
           paste0(format(date, "%Y"), " Q", data_q)))

# PIB BRASIL --------------------------------------------------------------
df_gdp_br %>% 
  
  ggplot(aes(x = data_q,
             y = value)) +
  
  geom_bar(stat = "identity", 
           fill = "#181F40",
           position = "dodge",
           width = .05) +

  geom_text(aes(label = paste0(round(value, 1), "%"),
                
                vjust = if_else(value > 0, -.8, 1.5)),
            color = "black", 
            size = 2) +
  
  xlab("") +
  
  ylab("") +
  
  labs(title = "PIB - Brasil", 
       caption = "\nFonte: Organization for Economic Co-operation and Development | Dapes Investimentos", 
       subtitle = "Unidade: Taxa de crescimento do período anterior") + 
  
  ggthemes::theme_fivethirtyeight() +
  
  theme(plot.title = element_text(hjust = 0.5, family = "serif"), 
        plot.subtitle = element_text(hjust = 0.5, family = "serif"), 
        plot.caption = element_text(family = "serif"), 
        axis.text.y = element_blank(), 
        axis.text.x = element_text(angle = 90, 
                                 hjust = 1)) +
  
  scale_x_yearqtr(breaks = df_gdp_br$data_q) 

# PIB EXPECTATIVA --------------------------------------------------------
df_pib_exp <- tibble(x = c(2020, 2021, 2022, 2023, 2024),
                     y = c(-4.06, 5.26, 2.09, 2.5, 2.5))



ipeadata(code = "SCN10_PIBG10",
         language = "br") %>% 
  
  filter(date >= "2011-01-01") %>% 
  
  mutate(date = lubridate::year(date)) %>% 
  
  ggplot(aes(x = date, 
             y = value)) +
  
  geom_bar(stat = "identity", 
           fill = "#181F40",
           position = "dodge",
           width = .2) +
  
  ggthemes::theme_fivethirtyeight() + 
  
  geom_line(data = df_pib_exp,
            aes(x = x,
                y = y),
            color = "red",
            linetype ="dashed",
            size = .7) +
  
  scale_x_continuous(breaks = seq(2006, 2024, 1)) + 
  
  geom_bar(data = df_pib_exp, stat = "identity", 
           fill = "#181F40",
           position = "dodge",
           width = .2, 
           aes(x = x, 
               y = y)) +
  
  geom_text(aes(label = paste0(round(value, 2), "%"),
                vjust = if_else(value > 0, -1., 1.8)),
            size = 3,
            color = "black") + 
  
  annotate("text", 
           x = c(2021,
                 2022,
                 2023,
                 2024),
           y = df_pib_exp$y[-1] +.3,
           label = paste0(df_pib_exp$y[-1],"%"),
           color = "black",
           size = 3) + 
  
  labs(title = "PIB Variação anual - Brasil",
       caption = "\nFonte: IBGE | Dapes Investimentos",
       subtitle = "Unidade: Taxa de crescimento do ano anterior") +
  
  theme(plot.title = element_text(hjust = .5, 
                                  family = "serif"),
        plot.subtitle = element_text(hjust = .5, 
                                     family = "serif"),
        plot.caption = element_text(family = "serif"),
        axis.text.y = element_blank()) 


# PIB PER CAPITA ----------------------------------------------------------
df_pib_percapita <- WDI(country = "BR",
    indicator = "NY.GDP.PCAP.CD") %>% 
  mutate(date = lubridate::ymd(paste0(year, "-01-01"))) %>% 
  na.omit() 

df_pib_percapita %>% 
  ggplot(aes(x = date, 
             y = NY.GDP.PCAP.CD)) +
  geom_line(size = 0.7, 
            color = "#181F40") + 
  ggthemes::theme_fivethirtyeight() +
  labs(title = "PIB PER CAPITA - Brasil") +
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "serif")) +
  
  scale_y_continuous(breaks = c(0, 2000, 5000, 7500, 10000, 12000)) +

  labs(subtitle = "Em doláres",
       caption = "\nFonte: Banco mundial | Dapes Investimentos") +
  
  theme(plot.subtitle = element_text(hjust = .5, family = "serif")) + 
  
  annotate("text", 
           x = df_pib_percapita$date[1],
           y = df_pib_percapita$NY.GDP.PCAP.CD[1] -100,
           label = round(df_pib_percapita$NY.GDP.PCAP.CD[1], 0),
           color = "black",
           size = 3) 

# PAN4_QIIGG4 - Producao industrial br ----------------------------------
prod_indust <- ipeadata(code = "PAN4_QIIGG4",
                        language = "br") %>% 
  
  filter(date >= "2011-01-01") %>% 
  mutate(date_q = as.yearqtr(date))


prod_indust %>%   
  
  ggplot(aes(x = date_q, y = value)) +
  
  geom_bar(stat = "identity", 
           fill = "#181F40",
           position = "dodge",
           width = .1)  +
  
  geom_text(aes(label = paste0(round(value, 1), "%"),
                vjust = if_else(value > 0, -.6, 1.)), 
            color = "black", 
            size = 2) +
  
  labs(title = "Produção industrial - Brasil",
       caption = "Fonte: IBGE | Dapes Investimentos", 
       subtitle = "Unidade: Taxa de crescimento do mesmo período do ano anterior") + 
  
  ggthemes::theme_fivethirtyeight() +
  
  theme(plot.title = element_text(hjust = .5, 
                                  family = "serif"), 
        
        axis.text.y = element_blank(), 
        plot.subtitle = element_text(hjust = .5, 
        family = "serif")) + 
  
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +
  
  scale_x_yearqtr(breaks = prod_indust$date_q) 



# expectativa produção industrial BR --------------------------------------
df_expc_prod <- tibble(x = c(2020, 2021, 2022, 2023, 2024), 
                       y = c(-4.46, 6.3, 2.2, 3, 2.5))

ipeadata(code = "PAN_QIIGG",
         language = "br") %>% 
  filter(date >= "2011-01-01") %>% 
  mutate(date = lubridate::year(date)) %>% 
  ggplot(aes(x = date, 
             y = value)) +
  
  
  geom_bar(stat = "identity", 
           fill = "#181F40",
           position = "dodge",
           width = .2) +
  
  geom_line(data = df_expc_prod,
            aes(x = x,
                y = y),
            color = "red",
            linetype ="dashed",
            size = .7) + 
  
  scale_x_continuous(breaks = seq(2006, 2024, 1)) +
  
  ggthemes::theme_fivethirtyeight() +
  
  labs(title = "Produção industrial anual - Brasil", 
       caption = "\nFonte: IBGE/PIM-PF | Dapes Investimentos",
       subtitle = "Unidade: Taxa de crescimento do ano anterior") +
  
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "serif"), 
        axis.text.y = element_blank(), 
        plot.subtitle =  element_text(hjust = 0.5,
                                  family = "serif")) +
  
  geom_bar(data = df_expc_prod,
           stat = "identity", 
           fill = "#181F40",
           position = "dodge",
           width = .2, 
           aes(x = x, 
               y = y)) +
  
  geom_text(aes(label = paste0(round(value, 2), "%"),
                vjust = if_else(value > 0, -1, 1.8)),
            size = 3,
            color = "black") +
  
  annotate("text", 
           x = c(2021,
                 2022,
                 2023,
                 2024),
           y = df_expc_prod$y[-1] +.5,
           label = paste0(df_expc_prod$y[-1],"%"),
           color = "black",
           size = 3)


# Taxa de desocupacao Mensal - IBGE/PNAD ----------------------------------
desemprego <- ipeadata(code = "PNADC12_TDESOC12", 
         language = "br") %>% 
  
  filter(date >= "2011-01-01")

desemprego %>%  
  
  ggplot(aes(x = date,
             y = value), color = "red") + 
  
  ggthemes::theme_fivethirtyeight() +
  
  geom_line(color = "#181F40", 
            size = 1.2) +
  
  labs(title = "Taxa de desemprego mensal",
       caption = "\nFonte: IBGE/PNAD | Dapes Investimentos") +
  
  theme(plot.title = element_text(hjust = .5,
                                  family = "serif"), 
        axis.text.x = element_text(angle = 90, 
                                   hjust = 1), 
        plot.caption = element_text(family = "serif")) +
  ylab("") +
  
  xlab("") + 
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y - %m") + 
  geom_label(data = filter(desemprego, date == last(date)), aes(label = value))



# Inflacao mensal (ipca) --------------------------------------------------

ipca_tri <- ipeadata(code = "PRECOS12_IPCAG12") %>% 
  filter(date >= "2011-01-01") %>% 
  mutate(value = round(value, 2))

ipca_tri %>% 
  ggplot(aes(x = date,
             y = value)) + 
  
  ggthemes::theme_fivethirtyeight() +
  
  geom_bar(stat = "identity", 
           fill = "#181F40",
           position = "dodge", 
           width = 15) +
  
  xlab("") +
  
  ylab("") +
  
  labs(title = "IPCA mensal - Brasil\n",
       
       caption = "\nFonte: IBGE/SNIPC | Dapes Investimentos") + 
  
  
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "serif")) +
  
  scale_y_continuous(labels = percent_format(scale = 1), 
                     breaks = seq(min(ipca_tri$value), max(ipca_tri$value), 0.1)) +  
  
  
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y-%m")


# Tabela IPCA -------------------------------------------------------------

# tabela
acumulado_ipca <- ipca_tri %>% 
mutate(ano = format(date, "%Y")) %>% 
  group_by(ano) %>% 
  summarise(acumulado = cumsum(value+1)-1) %>% 
  group_by(ano) %>%
  summarise(value = last(acumulado))


acumulado_ipca %>% 
  mutate(value = paste0(value, "%")) %>% 
  pivot_wider(., names_from = ano, values_from = value) %>% 
  kable(caption = "IPCA ANUAL", align = "c") %>%
  kable_classic(full_width = F, html_font = "Cambria")  %>% 
  kable_styling(position = "center")




# Expectativa IPCA --------------------------------------------------------
df_expec_ipca <- tibble(x = c(2020:2024),
                        y = c(4.52, 6.11, 3.75, 3.25, 3.16))

ipeadata(code = "PAN_IPCAG") %>%
  
  filter(date >= "2011-01-01") %>% 
  
  mutate(date = lubridate::year(date)) %>% 
  
  ggplot(aes(x = date, 
             y = value)) +
  
  geom_bar(stat = "identity", 
           fill = "#181F40",
           position = "dodge",
           width = .2) +
  geom_line(data = df_expec_ipca,
            aes(x = x,
                y = y),
            color = "red",
            linetype ="dashed",
            size = .7) + 
  
  scale_x_continuous(breaks = seq(2011, 2024, 1)) +
  
  ggthemes::theme_fivethirtyeight() +
  
  labs(title = "IPCA anual - Brasil", 
       caption = "\nFonte: IBGE/PIM-PF | Dapes Investimentos") +
  
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "serif"),
        axis.text.y = element_blank(), 
        plot.caption = element_text(family = "serif")) +
  
  geom_bar(data = df_expec_ipca,
           stat = "identity", 
           fill = "#181F40",
           position = "dodge",
           width = .2, 
           aes(x = x, 
               y = y)) +
  
  geom_text(aes(label = paste0(round(value, 2), "%"),
                vjust = if_else(value >0, -1.2, .7)),
            size = 3,
            color = "black") +
  
  annotate("text", 
           x = c(2021:2024),
           y = df_expec_ipca$y[-1] +.5,
           label = paste0(df_expec_ipca$y[-1],"%"),
           color = "black",
           size = 3) 


# INCC - DI em milhoes -  Mensal ------------------------------------------
incc <- ipeadata(code = "IGP12_INCCG12") %>% 
  
  filter(date >= "2011-01-01")


incc %>% 
  
  ggplot(aes(x = date,
             y = value)) +
  
  geom_bar(stat = "identity", 
           fill = "#181F40",
           position = "dodge", 
           width = 15)  +
  
  xlab("") +
  
  ylab("") +
  
  labs(title = "INCC mensal - Brasil", 
       caption = "\nFonte: FGV | Dapes Investimentos", 
       subtitle = "Unidade: Mudança percentual do mês anterior") + 
  
  ggthemes::theme_fivethirtyeight() +
  
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "serif"), 
        plot.subtitle = element_text(hjust = .5, family  = "serif"))  + 
  
  scale_y_continuous(labels = percent_format(scale = 1), 
                     breaks = seq(min(incc$value), max(incc$value), .2)) + 
  
  geom_point(color = "red") +
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y - %m") +  
  
  theme(axis.text.x=element_text(angle = 90, 
                                 hjust = 1)) 
  
  


# Risco pais - EMBI + RISCO BRASIL ----------------------------------------

ipeadata(code = "JPM366_EMBI366") %>%
  
  filter(date >= "2011-01-01") %>% 
  
  ggplot(aes(x = date,
             y = value)) +
  
  geom_line(color = "#181F40") +
  
  xlab("") +
  
  ylab("") +
  
  labs(title = "EMBI +", 
       caption = "\nFonte: JP MORGAN | Dapes Investimentos", 
       subtitle = "Risco Brasil") + 
  
  ggthemes::theme_fivethirtyeight() +
  
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "serif"), 
        axis.text.x=element_text(angle = 90, 
                                 hjust = 1), 
        plot.caption = element_text(family = "serif"), 
        plot.subtitle = element_text(hjust = .5, family = "serif")) + 
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y - %m")
  


# Divida publica % PIB ----------------------------------------------------


df_expc_div_pub <- tibble(x = c(2020:2024),
                       y = c(62.7, 61.55, 62.9, 64.85, 66.15))


ipeadata(code = "PAN_DTSPY") %>% 
  
  filter(date >= "2011-01-01") %>% 
  
  mutate(date = lubridate::year(date)) %>% 
  
  ggplot(aes(x = date, 
             y = value)) +
  
  geom_bar(stat = "identity", 
           fill = "#181F40",
           position = "dodge",
           width = .2)  +
  
  geom_line(data = df_expc_div_pub,
            aes(x = x,
                y = y),
            color = "red",
            linetype ="dashed",
            size = .8) + 
  
  scale_x_continuous(breaks = seq(2006, 2024, 1)) +
  
  ggthemes::theme_fivethirtyeight() +
  
  labs(title = "Dívida Líquida do Setor Público (% PIB) - Brasil", 
       caption = "\nFonte: BACEN | Dapes Investimentos") +
  
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "serif"),
        axis.text.y = element_blank(), 
        plot.caption = element_text(family = "serif")) +
  
  geom_bar(data = df_expc_div_pub,
           stat = "identity", 
           fill = "#181F40",
           position = "dodge",
           width = .2, 
           aes(x = x, 
               y = y)) +
  
  geom_text(aes(label = paste0(round(value, 2), "%"),
                vjust = if_else(value >0, -1.4, .7)),
            size = 3,
            color = "black") +
  
  annotate("text", 
           x = c(2021:2024),
           y = df_expc_div_pub$y[-1] +2,
           label = paste0(df_expc_div_pub$y[-1],"%"),
           color = "black",
           size = 3) 




# Balança comercial -------------------------------------------------------

# Importacao
importacao <- ipeadata(code = "FUNCEX_MDVT") %>% 
  filter(date >= "2011-01-01") 
  
# Exportacao
exportacao <- ipeadata(code = "FUNCEX_XVT") %>% 
  filter(date >= "2011-01-01")

# Visualizacao

df_expc_balcomercial <- tibble(x = c(2020:2024),
                          y = c(50.9, 70, 60.2, 60.5, 62))

importacao %>% 
  
  left_join(exportacao, by = "date") %>% 
  
  mutate(balanca_comercial = (value.y - value.x) / 1000) %>% 
  
  mutate(date = lubridate::year(date)) %>% 
  
  ggplot(aes(x = date, 
             y = balanca_comercial)) +
  
  geom_line(color = "#181F40",
            size = .7) +
  
  scale_x_continuous(breaks = seq(2006, 2024, 1)) + 
  
  geom_line(data = df_expc_balcomercial,
            aes(x = x,
                y = y),
            color = "red",
            linetype ="dashed",
            size = .7) + 
  
  ggthemes::theme_fivethirtyeight() +
  
  labs(title = "Balança comercial anual - Brasil", 
       caption = "\nFonte: FUNCEX | Dapes Investimentos",
       subtitle = "Em bilhões de reais") +
  
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "serif"),
        axis.text.y = element_blank(),
        plot.subtitle = element_text(hjust = .5, family = "serif"), 
        plot.caption = element_text(family = "serif")) +
  
  geom_text(aes(label = round(balanca_comercial, 2),
                vjust = if_else(balanca_comercial > 0, -1.4, .7)),
            size = 3,
            color = "black") +
  
  annotate("text", 
           x = c(2021:2024),
           y = df_expc_balcomercial$y[-1] +2,
           label = df_expc_balcomercial$y[-1],
           color = "black",
           size = 3)
  

# Taxa de juros -----------------------------------------------------------
seq(as.Date("2022-01-01"),
    as.Date("2033-01-01"),
    by = "year") -> datas

datas[-11] -> datas

lubridate::year(datas) -> datas

readxl::read_excel("Curva de Juros.xlsx") %>% 
  
  mutate(date = datas) %>% 
  
  ggplot(aes(x = date, y = Value)) +
  
  scale_x_continuous(breaks = c(2022:2031, 2033)) +
  
  ggthemes::theme_fivethirtyeight() + 
  
  geom_line(size = .7, color = "#181F40") +
  
  geom_text(aes(label = paste0(round(Value, 2), "%"),
                vjust = if_else(Value > 0, -1.4, .7)),
            size = 3,
            color = "black")  + 
  labs(title = "Curva de juros - DI1F - Brasil")+
  
  theme(axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  family = "serif")) 


# SELIC -------------------------------------------------------------------
df_selic <- ipeadata(code = "BM366_TJOVER366") %>% 
  filter(date >="2011-01-01")

df_selic %>% 
  ggplot(aes(x = date, y = value)) + 
  geom_line(color = "#181F40",
            size = .7) +
  ggthemes::theme_fivethirtyeight() + 
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y - %m") +
  theme(axis.text.x=element_text(angle = 90, 
                                 hjust = 1)) + 
  
  geom_label(data = filter(df_selic,
                           date == last(date)),
             aes(label = value), 
             color = "#181F40")  + 
  labs(title = "Taxa de juros - Selic - fixada pelo Copom",
        caption = "\nFonte: Bacen | Dapes Investiementos") +
   
  
  theme(plot.title = element_text(hjust = .2, family = "serif"), 
        plot.caption = element_text(family = "serif")) %>% 
 

# Resultado Primario ------------------------------------------------------
resultado_primario <- ipeadata(code = "BM12_NFSPPNS12") %>% 
  
  mutate(value = value*-1/1000) %>% 
  
filter(date >= "2011-01-01")
  
resultado_primario %>% 
  
ggplot(aes(x = date,
           y = value)) +
  
  geom_bar(stat = "identity", 
           fill = "#181F40",
           position = "dodge", 
           width = 15)  +
  
  xlab("") +
  
  ylab("") +
  
  labs(title = "Resultado primário mensal - Brasil", 
       caption = "\nFonte: Bacen/ Notas Imprensa/ F. Púb. | Dapes Investimentos", 
       subtitle = "Em bilhões") + 
  
  ggthemes::theme_fivethirtyeight() +
  
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "serif"), 
        plot.subtitle = element_text(hjust = .5, family  = "serif"))  + 
  
  scale_y_continuous( 
                     breaks = round(seq(min(resultado_primario$value), max(resultado_primario$value), 20), 0)) + 
  
  
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y - %m") +  
  
  theme(axis.text.x=element_text(angle = 90, 
                                 hjust = 1)) 

