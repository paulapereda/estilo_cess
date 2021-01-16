library(lubridate)
library(tidyverse)
library(readxl)
library(scales)
library(here)

source(here("estilo_cess.R"))

# Gráfico 1 

g1 <- read_excel(here("Notas Técnicas", "05_Proyección BPS", "Graficos y cuadros Nota Proyección.xlsx"))

g1 %>% 
  ggplot(aes(anio, valor, color = pea)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(verde_cess, rosado_cess, amarillo_cess)) +
  scale_x_continuous(breaks = seq(2020, 2100, by = 5)) +
  scale_y_continuous(limits = c(1000000, 1900000),
                     breaks = seq(1000000, 1900000, by = 100000),
                     expand = expansion(mult = c(0, .01)),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  labs(x = "",
       y = "", 
       color = "") +
  ggsave(here("Notas Técnicas", "05_Proyección BPS", "plots", "graf1.png"), 
         dpi = 300, width = 13, height = 7)

# Gráfico 2

g2 <- read_excel(here("Notas Técnicas", "05_Proyección BPS", "Graficos y cuadros Nota Proyección.xlsx"),
                 sheet = "Sheet2")

g2 %>% 
  ggplot(aes(anio, valor, color = resultado)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(verde_cess, rosado_cess)) +
  scale_y_continuous(expand = expansion(mult = c(0, .01)),
                     labels = scales::percent_format(accuracy = 1),
                     limits = c(0, NA)) +
  labs(x = "",
       y = "", 
       color = "") +
  ggsave(here("Notas Técnicas", "05_Proyección BPS", "plots", "graf2.png"), 
         dpi = 300, width = 13, height = 7)
