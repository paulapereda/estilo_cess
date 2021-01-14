library(lubridate)
library(tidyverse)
library(readxl)
library(scales)
library(here)

source(here("estilo_cess.R"))

# Gráfico 1 - 

g1 <- read_excel("Notas Técnicas/04_Densidad de cotización/Gráficas densidad.xlsx", 
                 range = "A1:B16") 

g1 %>% 
  ggplot(aes(edad_al_inicio, n)) +
  geom_col(fill = verde_cess) +
  scale_x_continuous(breaks = seq(18, 32, by = 1),
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(limits = c(0, 12000),
                     breaks = seq(0, 12000, by = 3000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                     expand = expansion(mult = c(0, 0))) +
  geom_vline(aes(xintercept = 19.2),
             color = "grey35", size = .7) +
  annotate("text", 
           x = 20.5, 
           y = 10500, 
           size = 5,
           label = "Promedio: 19,2") +
  labs(x = "Edad al inicio", 
       y = "Cantidad de cotizantes", 
       title = "Gráfico 1 - Edad de inicio laboral formal desde 2005",
       caption = "Fuente: elaboración propia en base a datos de historia laboral de BPS") +
  ggsave(here("Notas Técnicas", "04_Densidad de cotización", "plots", "graf1.png"),
         dpi = 300, width = 10, height = 7)

# Gráfico 2 - 

g2 <- read_excel("Notas Técnicas/04_Densidad de cotización/Gráficas densidad.xlsx", 
                 sheet = "Gráfica 2",
                 range = "A1:B51") 

g2 %>% 
  ggplot(aes(Edad, prom)) +
  geom_line(color = verde_cess, size = 1.1) +
  scale_y_continuous(limits = c(0.5, 0.8),
                     breaks = seq(0.5, 0.8, by = 0.05),
                     labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0))) +
  geom_hline(aes(yintercept = 0.661),
             color = "grey35", size = .7) +
  annotate("text",
           x = 24,
           y = 0.667,
           size = 5,
           label = "Promedio: 66,1%") +
  labs(x = "Edad al 12/2019", 
       y = "Densidad de cotización", 
       title = "Gráfico 2 - Densidad promedio de cotizantes de 2019",
       caption = "Fuente: elaboración propia en base a datos de historia laboral de BPS") +
  ggsave(here("Notas Técnicas", "04_Densidad de cotización", "plots", "graf2.png"),
         dpi = 300, width = 10, height = 7)

# Gráfico 3 - 

g3 <- read_excel("Notas Técnicas/04_Densidad de cotización/Gráficas densidad.xlsx", 
                 sheet = "Gráfica 3",
                 range = "A1:C23") %>% 
  mutate(se_jubila = factor(`Se jubila a esa edad`, levels = c("Sí", "No")),
         label = round(densidad*100))

g3_labels <- filter(g3, Edad %in% c(60, 62, 64, 66, 68, 70))

g3 %>% 
  ggplot(aes(Edad, densidad, color = se_jubila)) +
  geom_line(size = 1.1) +
  geom_point() +
  scale_color_manual(values = c(rosado_cess, verde_cess)) +
  scale_x_continuous(limits = c(60, 70),
                    breaks = seq(60, 70, by = 2)) +
  scale_y_continuous(limits = c(0.65, 0.9),
                     breaks = seq(0.65, 0.9, by = 0.05),
                     labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0))) +
  geom_text(data = g3_labels, 
            nudge_y = .55e-3,
            size = 5,
            vjust = -.25,
            hjust = .65,
            aes(label = label),
            color = "black") + 
  labs(x = "Edad al 31/12/2019", 
       y = "Densidad", 
       color = "Se jubila a esa edad",
       title = "Gráfico 3 - Densidad promedio desde 04/96",
       subtitle = "Cotizantes entre 60 y 70 años según si se jubilan o no a esas edades",
       caption = "Fuente: elaboración propia en base a datos de historia laboral de BPS") +
  ggsave(here("Notas Técnicas", "04_Densidad de cotización", "plots", "graf3.png"),
         dpi = 300, width = 10, height = 7)

# Gráfico 4 -

g4 <- read_excel("Notas Técnicas/04_Densidad de cotización/Gráficas densidad.xlsx", 
                 sheet = "Gráfica 4",
                 range = "A1:C101") %>% 
  mutate(sexo = ifelse(desc_sexo == "MASCULINO", "Hombres", "Mujeres"))

g4 %>% 
  ggplot(aes(Edad, prom, color = sexo)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = c(verde_cess, violeta_cess)) +
  scale_y_continuous(limits = c(0.5, 0.8),
                     breaks = seq(0.5, 0.8, by = 0.1),
                     labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0))) + 
  labs(x = "Edad al 12/2019", 
       y = "Densidad de cotización", 
       color = "",
       title = "Gráfico 4 - Densidad promedio de cotizantes de 2019 por edad y sexo",
       caption = "Fuente: elaboración propia en base a datos de historia laboral de BPS") +
  ggsave(here("Notas Técnicas", "04_Densidad de cotización", "plots", "graf4.png"),
         dpi = 300, width = 10.5, height = 7)

# Gráfico 5 -

g5 <- read_excel("Notas Técnicas/04_Densidad de cotización/Gráficas densidad.xlsx", 
                 sheet = "Gráfica 5",
                 range = "A1:C251") %>% 
  mutate(quintil = factor(quintil, levels = c("1", "2", "3", "4", "5"), labels = c("Quintil 1", 
                                                                                   "Quintil 2", 
                                                                                   "Quintil 3", 
                                                                                   "Quintil 4", 
                                                                                   "Quintil 5")))

g5 %>% 
  ggplot(aes(Edad, densidad, color = quintil)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = c(verde_cess, violeta_cess, amarillo_cess, rosado_cess, violeta_cess2)) +
  scale_y_continuous(limits = c(0.3, 1),
                     breaks = seq(0.3, 1, by = 0.1),
                     labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0))) + 
  labs(x = "Edad al 31/12/2019", 
       y = "Densidad de cotización", 
       color = "",
       title = "Gráfico 5 - Densidad promedio de cotizantes de 2019 por edad y quintil",
       caption = "Fuente: elaboración propia en base a datos de historia laboral de BPS") +
  ggsave(here("Notas Técnicas", "04_Densidad de cotización", "plots", "graf5.png"),
         dpi = 300, width = 11, height = 7)

# Gráfico 6 

g6 <- read_excel("Notas Técnicas/04_Densidad de cotización/Gráficas densidad.xlsx", 
                 sheet = "Gráfica 6",
                 range = "A1:B16")

g6 %>% 
  ggplot(aes(Edad, promedio)) +
  geom_line(size = 1.1, color = verde_cess) +
  scale_x_continuous(limits = c(20, 34),
                     breaks = seq(20, 34, by = 1)) +
  scale_y_continuous(limits = c(0.5, .8),
                     breaks = seq(0.5, .8, by = 0.1),
                     labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0))) + 
  labs(x = "Edad al 31/12/2019", 
       y = "Densidad promedio", 
       title = "Gráfico 6 - Densidad promedio desde los 20 años",
       subtitle = "Cotizantes que cumplen 20 de 2005 en adelante",
       caption = "Fuente: elaboración propia en base a datos de historia laboral de BPS") +
  ggsave(here("Notas Técnicas", "04_Densidad de cotización", "plots", "graf6.png"),
         dpi = 300, width = 10.5, height = 7)

# Gráfico 7 - 
  
g7 <- read_excel("Notas Técnicas/04_Densidad de cotización/Gráficas densidad.xlsx", 
                 sheet = "Gráfica 7",
                 range = "A1:D131") %>% 
  mutate(densidad_desde = factor(`Densidad promedio desde`, 
                                 levels = c("último año", "5 años", "10 años", 
                                            "20 años", "desde 04/1996"), 
                                 labels = c("Último año", 
                                            "Últimos 5 años", 
                                            "Últimos 10 años", 
                                            "Últimos 20 años", 
                                            "Desde 04/1996")))

g7 %>% 
  ggplot(aes(Edad, densidad, color = densidad_desde)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = c(verde_cess, violeta_cess, amarillo_cess, rosado_cess, violeta_cess2)) +
  scale_y_continuous(limits = c(0.6, 1),
                     breaks = seq(0.6, 1, by = 0.1),
                     labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0))) + 
  geom_vline(aes(xintercept = 60),
             color = "grey35", size = .7) +
  labs(x = "Edad al 31/12/2019", 
       y = "Densidad de cotización", 
       color = "",
       title = "Gráfico 7 - Densidad promedio según período considerado",
       subtile = "Edades 45 a 70 a 12/2019",
       caption = "Fuente: elaboración propia en base a datos de historia laboral de BPS") +
  ggsave(here("Notas Técnicas", "04_Densidad de cotización", "plots", "graf7.png"),
         dpi = 300, width = 10.5, height = 7)

# Gráfico 8

g8 <- read_excel("Notas Técnicas/04_Densidad de cotización/Gráficas densidad.xlsx", 
                 sheet = "Gráfica 8",
                 range = "A1:D261") %>% 
  mutate(sexo = ifelse(desc_sexo == "MASCULINO", "Hombres", "Mujeres"),
         densidad_desde = factor(`Densidad promedio desde`, 
                                 levels = c("último año", "5 años", "10 años", 
                                            "20 años", "desde 04/1996"), 
                                 labels = c("Último año", 
                                            "Últimos 5 años", 
                                            "Últimos 10 años", 
                                            "Últimos 20 años", 
                                            "Desde 04/1996")))

g8 %>% 
  ggplot(aes(Edad, densidad, color = densidad_desde)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = c(verde_cess, violeta_cess, amarillo_cess, rosado_cess, violeta_cess2)) +
  facet_wrap(~ sexo) +
  scale_y_continuous(limits = c(0.5, 1),
                     breaks = seq(0.5, 1, by = 0.1),
                     labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Edad al 31/12/2019", 
       y = "Densidad de cotización", 
       color = "",
       title = "Gráfico 7 - Densidad promedio por sexo", 
       subtitle = "Edades 45 a 70 a 12/2019",
       subtile = "Edades 45 a 70 a 12/2019",
       caption = "Fuente: elaboración propia en base a datos de historia laboral de BPS") +
  ggsave(here("Notas Técnicas", "04_Densidad de cotización", "plots", "graf8.png"),
         dpi = 300, width = 10.5, height = 7)
