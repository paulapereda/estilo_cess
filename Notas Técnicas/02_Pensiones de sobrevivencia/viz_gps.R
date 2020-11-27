library(lubridate)
library(tidyverse)
library(readxl)
library(scales)

source(here::here("estilo_cess.R"))

# Gráfico 1 - Gasto en pensiones de sobrevivencia (En % del PIB)

g1 <- read_excel("Notas Técnicas/02_Pensiones de sobrevivencia/gps.xlsx", 
                 sheet = "g1") %>% 
  mutate(Country = reorder(Country, Total)) %>% 
  select(- Total) %>% 
  pivot_longer(- Country, names_to = "origen", values_to = "valor") %>% 
  mutate(origen = factor(origen, levels = c("Público", "Privado", "BPS", "Resto de los servicios")))
  
g1 %>% 
  ggplot(aes(Country, valor, fill = origen)) +
  geom_col(position = "stack", stat = "identity") +
  scale_y_continuous(expand = expansion(add = c(0, 0)),
                     limits = c(0, 3),
                     breaks = c(0, 1, 2, 3)) +
  scale_fill_manual(values = c(verde_cess, violeta_cess, amarillo_cess, rosado_cess)) +
  coord_flip() +
  labs(x = "",
       y = "",
       fill = "", 
       title = "Gráfico 1 - Gasto en pensiones de sobrevivencia",
       subtitle = "En % del PIB",
       caption = "Uruguay año 2019, países de la OCDE año 2017 o último disponible.\n Fuente: OCDE (2018)") +
  theme(legend.position = "bottom") +
  ggsave(here::here("Notas Técnicas", "02_Pensiones de sobrevivencia", "plots", "graf1.png"), dpi = 300, width = 10, height = 10)

# Gráfico 2 - Relación entre el gasto en pensiones de sobrevivencia y el gasto en pasividades  

g2 <- read_excel("Notas Técnicas/02_Pensiones de sobrevivencia/gps.xlsx", 
                 sheet = "g2") %>% 
  mutate(distincion = case_when(
    country == "Uruguay" ~ "Uruguay",
    country == "Promedio OCDE" ~ "Promedio OCDE",
    T ~ "Otros"
  ),
  country = reorder(country, Ratio)) 

g2 %>% 
  ggplot(aes(country, Ratio, fill = distincion)) +
  geom_col() +
  scale_y_continuous(expand = expansion(add = c(0, 0)),
                     limits = c(0, .3),
                     breaks = c(0, .1, .2, .3)) +
  scale_fill_manual(values = c(verde_cess, rosado_cess, amarillo_cess)) +
  coord_flip() +
  labs(x = "",
       y = "",
       fill = "", 
       title = "Gráfico 2 - Relación entre el gasto en pensiones de \nsobrevivencia y el gasto en pasividades",
       caption = "Uruguay año 2019, países de la OCDE año 2017 o último disponible.\n Fuente: OCDE (2018)") +
  theme(legend.position = "bottom") +
  guides(fill = FALSE) +
  ggsave(here::here("Notas Técnicas", "02_Pensiones de sobrevivencia", "plots", "graf2.png"), dpi = 300, width = 10, height = 10)

  
