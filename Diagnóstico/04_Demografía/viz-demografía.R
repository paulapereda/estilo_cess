library(lubridate)
library(tidyverse)
library(scales)
library(readxl)
library(here)

source(here::here("estilo_cess.R"))

# Gráfico 1 - Esperanza de vida al nacer

g1 <- read_excel("Notas Técnicas/01_Población/insumos graficos Nota tecnica 1.xlsx", 
                 sheet = "gráfico 3", range = "A3:AE5") %>% 
  rename(sexo = "E(0)") %>% 
  mutate(sexo = str_to_title(sexo)) %>% 
  pivot_longer(- sexo, names_to = "anio", values_to = "valor") %>% 
  mutate(label = paste(format(valor, decimal.mark = ","), "%", sep = "")) %>% 
  mutate(proyeccion = list("1950-1955" = F,  "1955-1960" = F,  "1960-1965" = F, 
                           "1965-1970" = F,  "1970-1975" = F,  "1975-1980" = F, 
                           "1980-1985" = F,  "1985-1990" = F,  "1990-1995" = F, 
                           "1995-2000" = F,  "2000-2005" = F,  "2005-2010" = F, 
                           "2010-2015" = F,  "2015-2020" = c(T, F), "2020-2025" = T,
                           "2025-2030" = T,  "2030-2035" = T,  "2035-2040" = T,
                           "2040-2045" = T,  "2045-2050" = T,  "2050-2055" = T,
                           "2055-2060" = T,  "2060-2065" = T,  "2065-2070" = T, 
                           "2070-2075" = T,  "2075-2080" = T,  "2080-2085" = T, 
                           "2085-2090" = T,  "2090-2095" = T,  "2095-2100" = T)[anio]) %>% 
  unnest(proyeccion)

evn_labels <- filter(g1, anio %in% c("2020-2025", "2095-2100"))

ggplot(g1, aes(anio, valor, color = sexo, group = interaction(sexo, proyeccion))) +
  geom_line(aes(linetype = proyeccion), size = 1.1) +
  scale_color_manual(values = c(verde_cess, violeta_cess)) +
  scale_x_discrete(expand = expansion(mult = c(0.03, 0.06))) +
  scale_y_continuous(limits = c(0, 100),
                     expand = expansion(mult = c(0, 0))) +
  geom_text(data = evn_labels, 
            nudge_y = .6e-2,
            size = 4.5,
            vjust = -.25,
            hjust = .65,
            aes(label = label),
            color = "black") + 
  labs(x = "",
       y = "",
       color = "",
       title = "Esperanza de vida al nacer",
       caption = "Fuente: WPP - Naciones Unidas. Rev. 2019") +
  guides(linetype = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        legend.position = "bottom") +
  ggsave(here("Diagnóstico", "04_Demografía", "plots", "graf1.png"),
         dpi = 300, width = 11, height = 7)

# Gráfico 2 - Proyección de población total

g2 <- read_excel("Notas Técnicas/01_Población/insumos graficos Nota tecnica 1.xlsx", 
                 sheet = "gráfico 4 y 5", range = "A3:D12") %>% 
  rename(anio = `...1`) %>% 
  mutate(label = format(round(Total), big.mark = "."))

polacion_labels <- filter(g2, anio %in% c(2020, 2040, 2060, 2080, 2100))

ggplot(g2, aes(anio, Total)) +
  geom_col(position = "dodge", fill = verde_cess) +
  scale_x_continuous(expand = expansion(mult = c(0.03, 0.06)),
                     breaks = c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08)),
                     limits = c(0, 4000000),
                     breaks = c(0, 500000, 1000000, 1500000, 2000000, 2500000, 3000000, 3500000, 4000000),
                     labels = c("0", "500.000", "1.000.000", "1.500.000", "2.000.000", "2.500.000", 
                                "3.000.000", "3.500.000", "4.000.000")) +
  geom_text(data = polacion_labels, 
            position = position_dodge(1),
            vjust = -0.25,
            size = 5,
            aes(label = label)) + 
  labs(x = "",
       y = "",
       title = "Proyección de población total",
       caption = "Fuente: elaboración propia") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(),
        axis.ticks.x = element_line()) +
  ggsave(here("Diagnóstico", "04_Demografía", "plots", "graf2.png"),
         dpi = 300, width = 11, height = 7)

# Gráfico 3 - Proyección de población por sexo

g3 <- read_excel("Notas Técnicas/01_Población/insumos graficos Nota tecnica 1.xlsx", 
                 sheet = "gráfico 4 y 5", range = "A3:D12") %>% 
  rename(anio = `...1`) %>% 
  select(- Total) %>% 
  pivot_longer(- anio, names_to = "sexo", values_to = "valor") %>% 
  mutate(label = format(round(valor), big.mark = "."))

 
ggplot(g3, aes(anio, valor, group = sexo)) +
  geom_col(aes(fill = sexo), position = "dodge") +
  scale_x_continuous(expand = expansion(mult = c(0.03, 0.06)),
                     breaks = c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08)),
                     limits = c(0, 2000000),
                     breaks = c(0, 500000, 1000000, 1500000, 2000000),
                     labels = c("0", "500.000", "1.000.000", "1.500.000", "2.000.000")) +
  scale_fill_manual(values = c(verde_cess, violeta_cess)) +
  labs(x = "",
       y = "",
       fill = "",
       title = "Proyección de población por sexo",
       caption = "Fuente: elaboración propia") +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(),
        axis.ticks.x = element_line()) +
  ggsave(here("Diagnóstico", "04_Demografía", "plots", "graf3.png"),
         dpi = 300, width = 11, height = 7)

# Gráfico 4 - Proyección de población por sexo y edad

g4 <- read_excel("Notas Técnicas/01_Población/insumos graficos Nota tecnica 1.xlsx", 
                 sheet = "gráfico 6", range = "M4:Q10") %>% 
  pivot_longer(starts_with("2"), names_to = "anio", values_to = "valor") %>% 
  mutate(label = paste(round(valor*100), "%", sep = ""))

ggplot(g4, aes(anio, valor, group = grupoedad)) +
  geom_col(aes(fill = grupoedad), position = "dodge") +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8),
                     labels = c("0%", "20%", "40%", "60%", "80%"),
                     expand = expansion(mult = c(0, .08))) +
  scale_fill_manual(values = c(amarillo_cess, verde_cess, violeta_cess)) +
  facet_wrap(~ sexo) +
  geom_text(position = position_dodge(1),
            vjust = -.25,
            size = 5,
            aes(label = label)) + 
  theme(legend.position = "bottom") +
  labs(x = "",
       y = "",
       fill = "",
       title = "Proyección de población por sexo y edad",
       caption = "Fuente: elaboración propia") +
  ggsave(here("Diagnóstico", "04_Demografía", "plots", "graf4.png"), 
         dpi = 300, width = 11, height = 7)

# Gráfico 5 - Pirámide de población

g5 <- read_excel("Notas Técnicas/01_Población/insumos graficos Nota tecnica 1.xlsx",
                 sheet = "gráfico 7", range = "P1:T22") %>%
  mutate(edad = fct_inorder(grupoedad))  %>%
  select( - grupoedad) %>%
  pivot_longer(- edad) %>%
  mutate(value = value*100) %>% 
  separate(name, into = c("sexo", "year"))

ggplot(g5, aes(edad, value)) +
  geom_col(aes(fill = sexo)) +
  coord_flip() +
  scale_fill_manual(values = c(verde_cess, violeta_cess), 
                    name = "") +
  scale_y_continuous(expand = expansion(add = c(1, 1)),
                     breaks = c(-4, -3, -2, -1, 0, 1, 2, 3, 4)) +
  facet_wrap(~ year, nrow = 1) +
  labs(x = "",
       y = "",
       caption = "Fuente: elaboración propia",
       title = "Pirámide de población") + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        legend.position = "bottom") +
  ggsave(here("Diagnóstico", "04_Demografía", "plots", "graf5.png"), 
         dpi = 300, width = 10, height = 6)
