#Se usará la librería igraph
library(dplyr)
library(ggplot2)
library(lubridate)
library(tvthemes)
library(extrafont)


# Se carga el df
df <- read.csv('datasets\\ADB Climate Change Financing - 2023-based on commitments.csv')

#Se transforma la columna de fecha de texto a fecha, y se limpia el df de nulos
df1 <- df %>%
  select(Date.Signed, Sector, Signed.amount....million.) %>%
  mutate(Signed.amount = suppressWarnings(as.numeric(Signed.amount....million.))) %>%
  select(-Signed.amount....million.)%>%
  mutate(Date.Signed = parse_date_time(Date.Signed, orders = 'd-b-y')) %>%
  mutate(Date.Signed = as.Date(Date.Signed))%>%
  arrange(Date.Signed) %>%
  na.omit()

#Se cambia la fecha a semanas
df1 <- df1 %>%
  mutate(Week.Signed = floor_date(Date.Signed, "week"))%>%
  group_by(Week.Signed, Sector) %>%
  summarise(Signed.amount = sum(Signed.amount)) %>%
  ungroup()

#Se calcula el acumulado por semana y sector
df1 <- df1 %>%
  group_by(Sector) %>%                         
  mutate(Signed.amount.cumsum = cumsum(Signed.amount)) %>%  
  ungroup()

#Se grafica la cantidad de datos existentes según el tipo de Fuel
library(ggthemes)
ggplot(df1, aes(Week.Signed, Signed.amount.cumsum, fill = Sector))+
  geom_area(alpha = 0.8, colour = 'darkblue') +
  theme_simpsons(title.font = "Lucida Sans Unicode",
                 text.font = "Akbar")+
  scale_x_date(breaks = seq(min(df1$Week.Signed), max(df1$Week.Signed), "1 month"),
               date_labels = "%b")+
  scale_y_continuous(labels = scales::dollar)+
  scale_fill_simpsons() +
  labs(y = "Acumulador en millones de $ americanos",
       x = "2023",
       title = "¿Qué sector está marcando la diferencia en la lucha contra el cambio climático?",
       caption = "Cristian Guerrero Balber| Datos: used_cars_data.csv (kaggle)")

library(reticulate)
# Usar la versión de Python en tu máquina
use_python("C:/Users/Cristian/AppData/Local/Programs/Python/Python312/python.exe", required = TRUE)
py_config()
