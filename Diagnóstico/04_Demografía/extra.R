ibrary(lubridate)
library(patchwork)
library(tidyverse)
library(scales)
library(readxl)
library(here)

source(here::here("estilo_cess.R"))

g <- read_excel(here("Diagnóstico", "04_Demografía", "Piramides_1908_2100.xlsx"),
                 sheet = "Sheet1") %>%
  mutate(valor = abs(valor),
         valor = ifelse(sexo == "Mujeres", valor*-1, valor),
         grupo = factor(grupo, levels = c("0-4",
                                          "5-9",
                                          "10-14",
                                          "15-19",
                                          "20-24",
                                          "25-29",
                                          "30-34",
                                          "35-39",
                                          "40-44",
                                          "45-49",
                                          "50-54",
                                          "55-59",
                                          "60-64",
                                          "65-69",
                                          "70-74",
                                          "75-79",
                                          "80+")))  
g %>% 
  filter(anio %in% c(1995, 2050, 2070, 2100)) %>% 
  ggplot(aes(grupo, valor)) +
  geom_col(aes(fill = sexo)) +
  coord_flip() +
  scale_fill_manual(values = c(verde_cess, violeta_cess), 
                    name = "") +
  scale_y_continuous(expand = expansion(add = c(1, 1)),
                     breaks = c(-8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8)) +
  facet_wrap(~ anio, nrow = 2) +
  labs(x = "",
       y = "") + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        legend.position = "bottom") +
  ggsave(here("Diagnóstico", "04_Demografía", "plots", "extra.png"), 
         dpi = 300, width = 11, height = 9)

g2 <- read_excel(here("Diagnóstico", "04_Demografía", "Piramides_1908_2100.xlsx"),
                sheet = "Sheet2") %>%
  mutate(valor = abs(valor),
         valor = ifelse(sexo == "Mujeres", valor*-1, valor),
         grupo = factor(grupo, levels = c("0-4",
                                          "5-9",
                                          "10-14",
                                          "15-19",
                                          "20-24",
                                          "25-29",
                                          "30-34",
                                          "35-39",
                                          "40-44",
                                          "45-49",
                                          "50-54",
                                          "55-59",
                                          "60-64",
                                          "65-69",
                                          "70-74",
                                          "75-79",
                                          "80+")))  
g2 %>% 
  filter(anio %in% c(1995, 2050, 2070, 2100)) %>% 
  ggplot(aes(grupo, valor)) +
  geom_col(aes(fill = sexo)) +
  coord_flip() +
  scale_fill_manual(values = c(verde_cess, violeta_cess), 
                    name = "") +
  scale_y_continuous(expand = expansion(add = c(1, 1)),
                     breaks = c(-8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8)) +
  facet_wrap(~ anio, nrow = 2) +
  labs(x = "",
       y = "") + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        legend.position = "bottom") +
  ggsave(here("Diagnóstico", "04_Demografía", "plots", "extra2.png"), 
         dpi = 300, width = 11, height = 9)

