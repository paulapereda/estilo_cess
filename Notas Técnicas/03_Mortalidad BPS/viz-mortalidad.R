library(lubridate)
library(tidyverse)
library(readxl)
library(scales)

source(here::here("estilo_cess.R"))

# Gráfico 1 - Esperanza de vida de los jubilados por vejez según sexo 
## Tablas de mortalidad de momento y dinámicas – Año 2020

g1 <- read_excel("Notas Técnicas/03_Mortalidad BPS/Cuadros y gráficos Nota tecnica tablas de mortalidad.xlsx", 
                 range = "I1:L13") %>% 
  mutate(esp_vida = as.factor(esp_vida),
         label = format(round(valor, 1), decimal.mark = ","))


g1 %>% 
  ggplot(aes(tabla, valor, fill = esp_vida, label = label)) +
  geom_col(position = "dodge") +
  geom_text(position = position_dodge(width = .9), vjust = -.3, size = 5) +
  facet_wrap(~ sexo) +
  scale_fill_manual(values = c(verde_cess, amarillo_cess, rosado_cess)) +
  scale_y_continuous(expand = expansion(add = c(0, 0)),
                     limits = c(0, 30),
                     breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  labs(x = "", 
       y = "", 
       fill = "",
       title = "Gráfico 1 - Esperanza de vida de los jubilados por vejez según sexo",
       subtitle = "Tablas de mortalidad de momento y dinámicas – Año 2020") +
  ggsave(here::here("Notas Técnicas", "03_Mortalidad BPS", "plots", "graf1.png"), 
         dpi = 300, width = 10, height = 10)

## Gráfico 2 - Jubilados por Vejez: diferencias de esperanzas de vida de mujeres y hombres
## Tablas de mortalidad dinámicas					

g2 <- read_excel("Notas Técnicas/03_Mortalidad BPS/Cuadros y gráficos Nota tecnica tablas de mortalidad.xlsx", 
                 range = "N1:P7") %>% 
  mutate(esp_vida = as.factor(esp_vida),
         label = format(round(valor, 2), decimal.mark = ","))


g2 %>% 
  ggplot(aes(as.factor(anio), valor, fill = esp_vida, label = label)) +
  geom_col(position = "dodge") +
  geom_text(position = position_dodge(width = .9), vjust = -.3, size = 5) +
  scale_fill_manual(values = c(verde_cess, amarillo_cess, rosado_cess)) +
  scale_y_continuous(expand = expansion(add = c(0, 0)),
                     limits = c(0, 7),
                     breaks = c(0, 1, 2, 3, 4, 5, 6, 7)) +
  labs(x = "", 
       y = "", 
       fill = "",
       title = "Gráfico 2 - Jubilados por Vejez: diferencias de esperanzas de vida\n de mujeres y hombres",
       subtitle = "Tablas de mortalidad dinámicas") +
  ggsave(here::here("Notas Técnicas", "03_Mortalidad BPS", "plots", "graf2.png"), 
         dpi = 300, width = 10, height = 10)

# Gráfico 3 - Esperanza de vida de los jubilados por invalidez según sexo
## Tablas de mortalidad de momento y dinámicas – Año 2020

g3 <- read_excel("Notas Técnicas/03_Mortalidad BPS/Cuadros y gráficos Nota tecnica tablas de mortalidad.xlsx", 
                 sheet = "Jub Invalidez",
                 range = "O1:R13") %>% 
  mutate(esp_vida = as.factor(esp_vida),
         label = format(round(valor, 1), decimal.mark = ","))


g3 %>% 
  ggplot(aes(tabla, valor, fill = esp_vida, label = label)) +
  geom_col(position = "dodge") +
  geom_text(position = position_dodge(width = .9), vjust = -.3, size = 5) +
  facet_wrap(~ sexo) +
  scale_fill_manual(values = c(verde_cess, amarillo_cess, rosado_cess)) +
  scale_y_continuous(expand = expansion(add = c(0, 0)),
                     limits = c(0, 25),
                     breaks = c(0, 5, 10, 15, 20, 25)) +
  labs(x = "", 
       y = "", 
       fill = "",
       title = "Gráfico 3 - Esperanza de vida de los jubilados por invalidez según sexo",
       subtitle = "Tablas de mortalidad de momento y dinámicas – Año 2020") +
  ggsave(here::here("Notas Técnicas", "03_Mortalidad BPS", "plots", "graf3.png"), 
         dpi = 300, width = 10.5, height = 10)

# Gráfico 4 - Comparación de esperanzas de vida:  jubilados por vejez respecto a jubilados por invalidez 
## Tablas de mortalidad dinámicas

g4 <- read_excel("Notas Técnicas/03_Mortalidad BPS/Cuadros y gráficos Nota tecnica tablas de mortalidad.xlsx", 
                 sheet = "Jub Invalidez",
                 range = "T1:W13") %>% 
  mutate(esp_vida = as.factor(esp_vida),
         anio = as.factor(anio),
         label = format(round(valor, 2), decimal.mark = ","))

g4 %>% 
  ggplot(aes(sexo, valor, fill = esp_vida, label = label)) +
  geom_col(position = "dodge") +
  geom_text(position = position_dodge(width = .9), vjust = -.3, size = 5) +
  facet_wrap(~ anio) +
  scale_fill_manual(values = c(verde_cess, amarillo_cess, rosado_cess)) +
  scale_y_continuous(expand = expansion(add = c(0, 0)),
                     limits = c(0, 7),
                     breaks = c(0, 1, 2, 3, 4, 5, 6, 7)) +
  labs(x = "", 
       y = "", 
       fill = "",
       title = "Gráfico 4 - Comparación de esperanzas de vida: jubilados por vejez \nrespecto a jubilados por invalidez",
       subtitle = "Tablas de mortalidad dinámicas") +
  ggsave(here::here("Notas Técnicas", "03_Mortalidad BPS", "plots", "graf4.png"), 
         dpi = 300, width = 10.5, height = 10)
