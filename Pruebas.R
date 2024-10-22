#Se usará la librería igraph
library(dplyr)
library(ggplot2)

# Se carga el df
df <- read.csv('datasets\\used_cars_data.csv')

#Se seleccionan las instancias a graficar
df1 <- select(df, Year, Fuel_Type, Price)


#Se eliminan valores faltantes
df1 <- na.omit(df1)
View(df1)
#Se itera entre los distintos tipos de fuel para ver si hay una cantidad
#suficientemente representativa de cada uno para graficar
fuel_types <- c(unique(df$Fuel_Type))
ctd_datos <- c()
for (fuel_type in fuel_types){
  df_ <- df1 %>%
    filter(Fuel_Type == fuel_type) %>%
    select(Fuel_Type)
  ctd_datos <- append(ctd_datos, nrow(df_))
}
fuel_types

ctd_datos
df_to_plot <- data.frame(fuel_types, ctd_datos)

View(df_to_plot)
ggplot(x = fuel_types,y = ctd_datos) 

