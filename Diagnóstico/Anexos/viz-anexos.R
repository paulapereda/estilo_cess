library(lubridate)
library(tidyverse)
library(readxl)
library(scales)
library(here)

source(here("estilo_cess.R"))

# Gráfica 1 - Pirámides de Población (1995, 2050, 2070, 2100)

g1 <- read_excel(here("Diagnóstico", "04_Demografía", "Piramides_1908_2100.xlsx"),
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
g1 %>% 
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
  ggsave(here("Diagnóstico", "Anexos", "plots", "piramides.png"),
         dpi = 300, width = 11, height = 9)

## ¡Gráfica para que Pía chequee!

# g2 <- read_excel(here("Diagnóstico", "04_Demografía", "Piramides_1908_2100.xlsx"),
#                  sheet = "Sheet2") %>%
#   mutate(valor = abs(valor),
#          valor = ifelse(sexo == "Mujeres", valor*-1, valor),
#          grupo = factor(grupo, levels = c("0-4",
#                                           "5-9",
#                                           "10-14",
#                                           "15-19",
#                                           "20-24",
#                                           "25-29",
#                                           "30-34",
#                                           "35-39",
#                                           "40-44",
#                                           "45-49",
#                                           "50-54",
#                                           "55-59",
#                                           "60-64",
#                                           "65-69",
#                                           "70-74",
#                                           "75-79",
#                                           "80+")))  
# g2 %>% 
#   filter(anio %in% c(1995, 2050, 2070, 2100)) %>% 
#   ggplot(aes(grupo, valor)) +
#   geom_col(aes(fill = sexo)) +
#   coord_flip() +
#   scale_fill_manual(values = c(verde_cess, violeta_cess), 
#                     name = "") +
#   scale_y_continuous(expand = expansion(add = c(1, 1)),
#                      breaks = c(-8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8)) +
#   facet_wrap(~ anio, nrow = 2) +
#   labs(x = "",
#        y = "") + 
#   theme(panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         axis.line.x = element_line(),
#         axis.ticks.x = element_line(),
#         legend.position = "bottom") +
#   ggsave(here("Diagnóstico", "04_Demografía", "plots", "extra2.png"), 
#          dpi = 300, width = 11, height = 9)

## Anexo 5 - Régimen de ahorro individual

# Gráfico 2 - Comisiones AFAP

g2 <- read_excel(here("Diagnóstico", "Anexos", "Gráficos - Anexo 5.xlsx"), 
                 sheet = "comisiones") %>% 
  pivot_longer(- anio, names_to = "afap", values_to = "value") %>% 
  mutate(value = value/100,
         value = ifelse(value == 0, NA_real_, value),
         label = ifelse(value == 0, NA, paste0(round(value*100, 1), "%")))

g2_labels <- filter(g2, anio == 2020)

g2 %>% 
  ggplot(aes(anio, value, color = afap)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = c(rosado_cess, verde_cess, amarillo_cess, violeta_cess)) +
  scale_x_continuous(breaks = seq(1996, 2020, by = 2)) +
  scale_y_continuous(limits = c(0, .16),
                     breaks = seq(0, .16, by = .02),
                     labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, .01))) +
  geom_text(data = g2_labels, 
            size = 4.5,
            vjust = -.4,
            hjust = .55,
            aes(label = label),
            color = "black") + 
  labs(x = "", 
       y = "", 
       color = "",
       title = "",
       subtitle = "",
       caption = "Fuente: elaboración propia en base a BCU") +
  ggsave(here("Diagnóstico", "Anexos", "plots", "comisiones.png"),
         dpi = 300, width = 10, height = 7)

# Gráfico 3 - Primas AFAP

g3 <- read_excel(here("Diagnóstico", "Anexos", "Gráficos - Anexo 5.xlsx"), 
                 sheet = "primas") %>% 
  pivot_longer(- anio, names_to = "afap", values_to = "value") %>% 
  mutate(value = value/100,
         value = ifelse(value == 0, NA_real_, value),
         label = ifelse(value == 0, NA, paste0(round(value*100, 1), "%")))   

g3_labels <- filter(g3, anio == 2020)

g3 %>% 
  ggplot(aes(anio, value, color = afap)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = c(rosado_cess, verde_cess, amarillo_cess, violeta_cess)) +
  scale_x_continuous(breaks = seq(1996, 2020, by = 2)) +
  scale_y_continuous(limits = c(0, .18),
                     breaks = seq(0, .18, by = .02),
                     labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, .01))) +
  geom_text(data = g3_labels, 
            size = 4.5,
            vjust = -.4,
            hjust = .55,
            aes(label = label),
            color = "black") + 
  labs(x = "", 
       y = "", 
       color = "",
       title = "",
       subtitle = "",
       caption = "Fuente: elaboración propia en base a BCU") +
  ggsave(here("Diagnóstico", "Anexos", "plots", "primas.png"),
         dpi = 300, width = 10, height = 7)

# Gráfico 4 - ROE

g4 <- read_excel(here("Diagnóstico", "Anexos", "Gráficos - Anexo 5.xlsx"), 
                 sheet = "roe") %>% 
  mutate(label = paste0(round((value*100), 1), "%"))

g4_labels <- filter(g4, anio == 2019)

g4 %>% 
  ggplot(aes(anio, value, color = afap)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = c(rosado_cess, verde_cess, amarillo_cess, violeta_cess)) +
  scale_x_continuous(breaks = seq(2014, 2020, by = 1)) +
  scale_y_continuous(breaks = seq(.2, 1, by = 0.2),
                     limits = c(.2, 1),
                     labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(.005, 0))) +
  geom_text(data = g3_labels, 
            size = 5,
            vjust = -.4,
            hjust = .55,
            aes(label = label),
            color = "black") + 
  labs(x = "", 
       y = "", 
       color = "",
       title = "",
       subtitle = "",
       caption = "Fuente: elaboración propia en base a BCU") +
  ggsave(here("Diagnóstico", "Anexos", "plots", "roe.png"),
         dpi = 300, width = 10, height = 7)

# Gráfico 5 - Cotizantes actualizado

g5 <- read_excel(here("Diagnóstico", "Anexos", "Cotizantes actualizado.xlsx"), 
                 sheet = "cotizantes") %>% 
 mutate(value = round(value))

g5 %>% 
  ggplot(aes(anio, value)) +
  geom_line(size = 1.2, color = verde_cess) +
  scale_x_continuous(breaks = seq(2004, 2020, by = 1)) +
  scale_y_continuous(breaks = seq(800000, 1600000, by = 100000),
                     limits = c(800000, 1600000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                     expand = expansion(mult = c(0, 0))) + 
  labs(x = "", 
       y = "", 
       color = "",
       title = "Puestos cotizantes al BPS",
       subtitle = "Promedio del año",
       caption = "Fuente: elaboración propia en base a BPS") +
  ggsave(here("Diagnóstico", "Anexos", "plots", "cotizantes.png"),
         dpi = 300, width = 11, height = 7)

# Gráfico 6 - Cantidad de jubilados - Servicios estatales

g6 <- read_excel(here("Diagnóstico", "Anexos", "graficos.xlsx"), 
                 sheet = "estatales") %>% 
  pivot_longer(- anio, names_to = "estatales", values_to = "value") %>% 
  mutate(value_aux = ifelse(estatales == "SRPP", value*2, value),
         label = format(value, big.mark = "."))

g6_labels <- filter(g6, anio == 2019)


g6 %>% 
  ggplot(aes(anio, value_aux, color = estatales, group = estatales)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(rosado_cess, verde_cess)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(2008, 2019, by = 1)) +
  scale_y_continuous(breaks = c(28000, 30000, 32000, 34000, 34000, 36000, 38000, 40000),
                     limits = c(28000, 40000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                     sec.axis = sec_axis(trans = ~./2, 
                                         breaks = c(14000, 15000, 16000, 17000, 18000, 19000, 20000),
                                         labels = function(x) format(x, big.mark = ".", scientific = FALSE)),
                     expand = expansion(mult = c(.001, .1))) +
  geom_text(data = g6_labels, 
            size = 5,
            vjust = -.4,
            hjust = 1,
            aes(label = label),
            color = "black") + 
  labs(x = "", 
       y = "", 
       color = "",
       title = "Cantidad de jubilados - Servicios estatales",
       subtitle = "",
       caption = "") +
  ggsave(here("Diagnóstico", "Anexos", "plots", "estatales.png"),
         dpi = 300, width = 11, height = 7)

# Gráfico 7 - Cantidad de jubilados - Cajas paraestatales

g7 <- read_excel(here("Diagnóstico", "Anexos", "graficos.xlsx"), 
                 sheet = "paraestatales") %>% 
  pivot_longer(- anio, names_to = "paraestatales", values_to = "value") %>% 
  mutate(value_aux = ifelse(paraestatales == "CNSS (eje derecho)", value*6, value),
         label = format(value, big.mark = "."))

g7_labels <- filter(g7, anio == 2019)


g7 %>% 
  ggplot(aes(anio, value_aux, color = paraestatales, group = paraestatales)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(rosado_cess, verde_cess, amarillo_cess)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(2005, 2019, by = 1)) +
  scale_y_continuous(breaks = c(5000, 7500, 10000, 12500, 15000),
                     limits = c(5000, 15000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                     sec.axis = sec_axis(trans = ~./6, 
                                         breaks = c(1000, 1300, 1600, 1900, 2200, 2500),
                                         labels = function(x) format(x, big.mark = ".", scientific = FALSE)),
                     expand = expansion(mult = c(.001, .1))) +
  geom_text(data = g7_labels, 
            size = 5,
            vjust = - .01,
            hjust = 1,
            aes(label = label),
            color = "black") + 
  labs(x = "", 
       y = "", 
       color = "",
       title = "Cantidad de jubilados - Cajas paraestatales",
       subtitle = "",
       caption = "") +
  ggsave(here("Diagnóstico", "Anexos", "plots", "paraestatales.png"),
         dpi = 300, width = 11, height = 7)

# Gráfico 8 - Altas Jubilatorias

g8 <- read_excel(here("Diagnóstico", "Anexos", "graficos.xlsx"), 
                 sheet = "altas_jubilatorias") %>% 
  pivot_longer(-anio, names_to = "causal", values_to = "value") %>% 
  mutate(causal = factor(causal, levels = c("Vejez", "Invalidez", "Edad avanzada", "Total"))) %>% 
  filter(causal != "Total")

g8 %>% 
  ggplot(aes(anio, value, fill = causal)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c(verde_cess, amarillo_cess, rosado_cess)) +
  scale_x_continuous(breaks = seq(2005, 2019, by = 1)) +
  scale_y_continuous(breaks = seq(0, 35000, by = 5000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                     expand = expansion(mult = c(0, .01))) + 
  labs(x = "", 
       y = "", 
       fill = "",
       title = "Altas jubilatorias",
       subtitle = "",
       caption = "") +
  ggsave(here("Diagnóstico", "Anexos", "plots", "altas_jubilatorias.png"),
         dpi = 300, width = 11, height = 7)

# Gráfico 9 - Altas jubilatorias por invalidez - BPS

g9 <- filter(g8, causal == "Invalidez") %>% 
  mutate(label = format(value, big.mark = "."))

g9_labels <- filter(g9, anio %in% c(2005, 2019))

g9 %>% 
  ggplot(aes(anio, value)) +
  geom_line(size = 1.5, color = verde_cess) +
  scale_x_continuous(breaks = seq(2005, 2019, by = 1)) +
  scale_y_continuous(limits = c(1000, 7000),
                     breaks = seq(1000, 7000, by = 1000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                     expand = expansion(mult = c(0, .01))) + 
  geom_text(data = g9_labels, 
            size = 5,
            vjust = 1.5,
            hjust = .55,
            aes(label = label),
            color = "black") + 
  labs(x = "", 
       y = "", 
       color = "",
       title = "Altas jubilatorias por invalidez - BPS",
       subtitle = "",
       caption = "") +
  ggsave(here("Diagnóstico", "Anexos", "plots", "altas_jubilatorias_invalidez.png"),
         dpi = 300, width = 11, height = 7)
