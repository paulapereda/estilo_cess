library(lubridate)
library(patchwork)
library(tidyverse)
library(scales)
library(readxl)
library(here)

source(here::here("estilo_cess.R"))

# Gráfico 1.1 - Población menor de 15 años

g1 <- read_excel(here("Diagnóstico", "05_Juventud y educación", "data_educacion.xlsx"),
                 sheet = "1.1") %>% 
  mutate(value = round(value))

g1 %>% 
  ggplot(aes(anio, value)) +
  geom_bar(stat = "identity", fill = verde_cess) +
  scale_x_continuous(breaks = seq(1996, 2020, by = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, .01)),
                     limits = c(0, 850000),
                     breaks = seq(0, 850000, by = 50000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
  labs(x = "", 
       y = "", 
       color = "",
       title = "Población menor de 15 años",
       subtitle = "",
       caption = "Fuente: Elaboración propia en base a datos del INE.") +
  ggsave(here("Diagnóstico", "05_Juventud y educación", "plots", "graf1.1.png"), 
         dpi = 300, width = 10, height = 7)

# Gráfico 1.2 - Variación de menores de 15 años

g2 <- read_excel(here("Diagnóstico", "05_Juventud y educación", "data_educacion.xlsx"),
                 sheet = "1.2") 

g2 %>% 
  ggplot(aes(anio, value)) +
  geom_line(size = 1.5, color = verde_cess) +
  scale_y_continuous(limits = c(-.014, .008),
                     breaks = seq(-.014, .008, by = .002),
                     labels = scales::percent_format(),
                     expand = expansion(mult = c(0, .01))) +
  labs(x = "", 
       y = "", 
       color = "",
       title = "Variación de menores de 15 años",
       subtitle = "",
       caption = "Fuente: Elaboración propia en base a datos del INE.") +
  ggsave(here("Diagnóstico", "05_Juventud y educación", "plots", "graf1.2.png"), 
         dpi = 300, width = 10, height = 7)

# Gráfico 1.3 - Porcentaje de población menor de 15 años

g3 <- read_excel(here("Diagnóstico", "05_Juventud y educación", "data_educacion.xlsx"),
                 sheet = "1.3") %>% 
  mutate(value = value/100)

g3 %>% 
  ggplot(aes(anio, value)) +
  geom_bar(stat = "identity", fill = verde_cess) +
  scale_x_continuous(breaks = seq(1996, 2020, by = 1)) +
  scale_y_continuous(limits = c(0, .25),
                     breaks = seq(0, .25, by = .05),
                     labels = scales::percent_format(1L),
                     expand = expansion(mult = c(0, .01))) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
  labs(x = "", 
       y = "", 
       color = "",
       title = "Porcentaje de población menor de 15 años",
       subtitle = "",
       caption = "Fuente: Elaboración propia en base a datos del INE.") +
  ggsave(here("Diagnóstico", "05_Juventud y educación", "plots", "graf1.3.png"), 
         dpi = 300, width = 10, height = 7)

# Gráfico 1.4 - Proyección de población menor a 14 años al 2100 por sexo

g4 <- read_excel(here("Diagnóstico", "05_Juventud y educación", "data_educacion.xlsx"),
                 sheet = "1.4") %>% 
  pivot_longer(- anio, names_to = "sexo", values_to = "value")

g4 %>% 
  ggplot(aes(anio, value, fill = sexo)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, .01)),
                     limits = c(0, 400000),
                     breaks = seq(0, 400000, by = 50000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_fill_manual(values = c(verde_cess, violeta_cess)) +
  labs(x = "", 
       y = "", 
       fill = "",
       title = "Proyección de población menor a 14 años al 2100 por sexo",
       subtitle = "",
       caption = "Ver Nota Técnica 1: Proyección de Población de Uruguay a 2100.") +
  ggsave(here("Diagnóstico", "05_Juventud y educación", "plots", "graf1.4.png"), 
         dpi = 300, width = 10, height = 7)

# Gráfico 1.5 - Relación de dependencia de los niños

g5 <- read_excel(here("Diagnóstico", "05_Juventud y educación", "data_educacion.xlsx"),
                 sheet = "1.5") 

g5 %>% 
  ggplot(aes(anio, RD)) +
  geom_line(size = 1.5, color = verde_cess) +
  scale_x_continuous(breaks = seq(1996, 2020, by = 1)) +
  scale_y_continuous(limits = c(20, 45),
                     breaks = seq(20, 45, by = 5),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                     expand = expansion(mult = c(0, .01))) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
  labs(x = "", 
       y = "", 
       fill = "",
       title = "Relación de dependencia de los niños",
       subtitle = "",
       caption = "Fuente: Elaboración propia en base a datos del INE.") +
  ggsave(here("Diagnóstico", "05_Juventud y educación", "plots", "graf1.5.png"), 
         dpi = 300, width = 10, height = 7)

# Grafico 1.6 - Incidencia de la pobreza en personas por grupos de edades

g6 <- read_excel(here("Diagnóstico", "05_Juventud y educación", "data_educacion.xlsx"),
                 sheet = "1.6") %>% 
  pivot_longer(- anio, names_to = "edad", values_to = "value") %>% 
  mutate(value = value/100)

g6 %>% 
  ggplot(aes(anio, value, color = edad, group = edad)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = c(verde_cess, rosado_cess, violeta_cess, amarillo_cess)) +
  scale_x_continuous(breaks = seq(2006, 2019, by = 1)) +
  scale_y_continuous(limits = c(0, .6),
                     breaks = seq(0, .6, by = .1),
                     labels = scales::percent_format(1L),
                     expand = expansion(mult = c(0, .01))) +
  labs(x = "", 
       y = "", 
       color = "",
       title = "Incidencia de la pobreza en personas por grupos de edades",
       subtitle = "",
       caption = "Fuente: Elaboración propia en base a datos del INE.") +
  ggsave(here("Diagnóstico", "05_Juventud y educación", "plots", "graf1.6.png"), 
         dpi = 300, width = 10, height = 7)

# Gráfico 7 - Porcentaje de población con al menos una NBI

g7 <- read_excel(here("Diagnóstico", "05_Juventud y educación", "data_educacion.xlsx"),
                 sheet = "1.7") %>% 
  mutate(valor = valor/100)

g7 %>% 
  ggplot(aes(edad, valor)) +
  geom_bar(stat = "identity", fill = verde_cess) +
  scale_y_continuous(expand = expansion(mult = c(0, .01)),
                     limits = c(0, .5),
                     breaks = seq(0, .5, by = .05),
                     labels = scales::percent_format(1L)) +
  labs(x = "", 
       y = "", 
       color = "",
       title = "Porcentaje de población con al menos una NBI",
       subtitle = "",
       caption = "Fuente: Elaboración propia a partir del Observatorio de la educación.") +
  ggsave(here("Diagnóstico", "05_Juventud y educación", "plots", "graf1.7.png"), 
         dpi = 300, width = 10, height = 7)

# Gráfico 8 - Matrícula de estudiantes de primer año de primaria

g8 <- read_excel(here("Diagnóstico", "05_Juventud y educación", "data_educacion.xlsx"),
                 sheet = "1.8") 

g8 %>% 
  ggplot(aes(anio, valor)) +
  geom_bar(stat = "identity", fill = verde_cess) +
  scale_x_continuous(breaks = seq(2005, 2019, by = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, .01)),
                     limits = c(0, 70000),
                     breaks = seq(0, 70000, by = 10000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  labs(x = "", 
       y = "", 
       color = "",
       title = "Matrícula de estudiantes de primer año de primaria",
       subtitle = "",
       caption = "Fuente: Elaboración propia a partir del Observatorio de la educación.") +
  ggsave(here("Diagnóstico", "05_Juventud y educación", "plots", "graf1.8.png"), 
         dpi = 300, width = 10, height = 7)

# Gráfico 9 - Matrícula de educación inicial

g9 <- read_excel(here("Diagnóstico", "05_Juventud y educación", "data_educacion.xlsx"),
                 sheet = "1.9") 

g9 %>% 
  ggplot(aes(anio, valor)) +
  geom_bar(stat = "identity", fill = verde_cess) +
  scale_x_continuous(breaks = seq(2005, 2019, by = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, .01)),
                     limits = c(0, 120000),
                     breaks = seq(0, 120000, by = 20000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  labs(x = "", 
       y = "", 
       color = "",
       title = "Matrícula de educación inicial",
       subtitle = "",
       caption = "Fuente: Elaboración propia a partir del Observatorio de la educación.") +
  ggsave(here("Diagnóstico", "05_Juventud y educación", "plots", "graf1.9.png"), 
         dpi = 300, width = 10, height = 7)