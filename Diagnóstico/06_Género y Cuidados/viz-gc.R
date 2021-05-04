library(lubridate)
library(patchwork)
library(tidyverse)
library(scales)
library(readxl)
library(here)

source(here::here("estilo_cess.R"))

# Cuidados

## Gráfico 1 

g1_c <- read_excel(here("Diagnóstico", "06_Género y Cuidados", "Graficos anexo cuidados.xlsx"),
                 sheet = "Dependencia cuidados") %>% 
  pivot_longer(- anio, names_to = "grupo", values_to = "value") %>% 
  mutate(grupo = factor(grupo, levels = c("Niños", "Adultos")))

g1_c %>% 
  ggplot(aes(anio, value, color = grupo, group = grupo)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(verde_cess, rosado_cess)) +
  scale_x_continuous(breaks = seq(1996, 2020, by = 1)) +
  scale_y_continuous(limits = c(5, 25),
                     breaks = seq(5, 25, by = 5),
                     expand = expansion(mult = c(0, .01))) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
  labs(x = "", 
       y = "", 
       color = "",
       title = "",
       subtitle = "",
       caption = "") +
  ggsave(here("Diagnóstico", "06_Género y Cuidados", "plots", "cuidados - graf1.png"), 
         dpi = 300, width = 10, height = 7)

## Gráfico 2 

g2_c <- read_excel(here("Diagnóstico", "06_Género y Cuidados", "Graficos anexo cuidados.xlsx"),
                 sheet = "Distribucion AP") %>% 
  pivot_longer(- edad, names_to = "sexo", values_to = "value") %>% 
  mutate(edad = factor(edad, levels = c("Menores de 30", "30-49", "50-64", "65+")),
         label = paste0(round(value*100, 2), "%"))

g2_c %>% 
  ggplot(aes(edad, value, fill = sexo)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c(verde_cess, violeta_cess)) +
  scale_y_continuous(expand = expansion(mult = c(0, .01)),
                     limits = c(0, .5),
                     breaks = seq(0, .5, by = .1),
                     labels = scales::percent_format(1L)) +
  geom_text(aes(label = label), position = position_dodge(width = .9), 
            vjust = -.25, size = 4.5) +
  labs(x = "", 
       y = "", 
       fill = "",
       title = "",
       subtitle = "",
       caption = "") +
  ggsave(here("Diagnóstico", "06_Género y Cuidados", "plots", "cuidados - graf2.png"), 
         dpi = 300, width = 10, height = 7)

# Género

## Gráfico 1

g1 <- read_excel(here("Diagnóstico", "06_Género y Cuidados", "Graficos anexo genero.xlsx"),
                 sheet = "Tasa de actividad") %>% 
  pivot_longer(- `...1`, names_to = "sexo", values_to = "value") %>% 
  mutate(anio = `...1`,
         sexo = factor(sexo, levels = c("Hombres", "Mujeres", "Total")),
         label = round(value, 1))

g1_labels <- filter(g1, anio %in% c(2006, 2009,  2012, 2015, 2018, 2019))

g1 %>% 
  ggplot(aes(anio, value, color = sexo, group = sexo)) +
  geom_line(size = 1.1) +
  geom_point() +
  scale_color_manual(values = c(verde_cess, rosado_cess, amarillo_cess)) +
  scale_x_continuous(breaks = seq(2006, 2019, by = 1)) +
  scale_y_continuous(limits = c(50, 75),
                     breaks = seq(50, 75, by = 5),
                     expand = expansion(mult = c(0, .03))) +
  geom_text(data = g1_labels, 
            size = 5,
            aes(label = label),
            color = "black",
            vjust = -.6)+
  labs(x = "", 
       y = "", 
       color = "",
       title = "",
       subtitle = "",
       caption = "") +
  ggsave(here("Diagnóstico", "06_Género y Cuidados", "plots", "graf1.png"), 
         dpi = 300, width = 10, height = 7)

## Gráfico 2

g2 <- read_excel(here("Diagnóstico", "06_Género y Cuidados", "Graficos anexo genero.xlsx"),
                 sheet = "IVS - altas", range = "F4:H15") %>% 
  pivot_longer(- `...1`, names_to = "sexo", values_to = "value") %>% 
  mutate(anio = `...1`,
         sexo = factor(sexo, levels = c("Hombres", "Mujeres")),
         label = round(value, 1))

g2 %>% 
  ggplot(aes(anio, value, color = sexo, group = sexo)) +
  geom_line(size = 1.1) +
  geom_point() +
  scale_color_manual(values = c(verde_cess, rosado_cess, amarillo_cess)) +
  scale_x_continuous(breaks = seq(2009, 2019, by = 1)) +
  scale_y_continuous(limits = c(40, 60),
                     breaks = seq(40, 60, by = 5),
                     expand = expansion(mult = c(0, .03))) +
  geom_text(size = 5,
            aes(label = label),
            color = "black",
            vjust = -.6)+
  labs(x = "", 
       y = "", 
       color = "",
       title = "",
       subtitle = "",
       caption = "") +
  ggsave(here("Diagnóstico", "06_Género y Cuidados", "plots", "graf2.png"), 
         dpi = 300, width = 10, height = 7)

## Gráfico 3

g3 <- read_excel(here("Diagnóstico", "06_Género y Cuidados", "Graficos anexo genero.xlsx"),
                 sheet = "Computo por hijo", range = "A4:C15") %>% 
  pivot_longer(- `...1`, names_to = "causal", values_to = "value") %>% 
  mutate(anio = `...1`)

g3 %>% 
  ggplot(aes(anio, value, color = causal, group = causal)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(verde_cess, rosado_cess)) +
  scale_x_continuous(breaks = seq(2009, 2019, by = 1)) +
  scale_y_continuous(limits = c(.3, .7),
                     breaks = seq(.3, .7, by = .1),
                     labels = scales::percent_format(1L),
                     expand = expansion(mult = c(0, .01))) +
  labs(x = "", 
       y = "", 
       color = "",
       title = "",
       subtitle = "",
       caption = "") +
  ggsave(here("Diagnóstico", "06_Género y Cuidados", "plots", "graf3.png"), 
         dpi = 300, width = 10, height = 7)

## Gráfico 4

g4 <- read_excel(here("Diagnóstico", "06_Género y Cuidados", "Graficos anexo genero.xlsx"),
                 sheet = "Monto promedio", range = "H4:K14") %>% 
  pivot_longer(- anio, names_to = "causal", values_to = "value") %>% 
  mutate(label = round(value, 2))

g4 %>% 
  ggplot(aes(anio, value, fill = causal))  +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c(verde_cess, rosado_cess, amarillo_cess)) +
  scale_x_continuous(breaks = seq(2010, 2019, by = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, .01)),
                     limits = c(NA, 1)) +
  geom_text(aes(label = label), position = position_dodge(width = .9), 
            vjust = -.25, size = 3.5) +
  labs(x = "", 
       y = "", 
       fill = "",
       title = "",
       subtitle = "",
       caption = "") +
  ggsave(here("Diagnóstico", "06_Género y Cuidados", "plots", "graf4.png"), 
         dpi = 300, width = 10, height = 7)
