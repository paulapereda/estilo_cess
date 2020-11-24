library(lubridate)
library(patchwork)
library(tidyverse)
library(scales)
library(readxl)
source(here::here("estilo_cess.R"))

# Gráfico 1 - Natalidad y Tasa Global de Fecundidad

g1 <- read_excel("Notas Técnicas/01_Población/insumos graficos Nota tecnica 1.xlsx", 
                 range = "H1:J24") %>% 
  transmute(anio = `...1`,
            Nacimientos = Nacimientos,
            TGF = TGF) %>% 
  pivot_longer(- anio, names_to = "variable", values_to = "valor") %>% 
  mutate(label = ifelse(valor < 3, round(valor, 2), format(round(valor), big.mark = ".")))

nacimiento_labels <- filter(g1 %>% filter(variable == "Nacimientos"), 
                              anio %in% c(1996, 1999, 2002, 2005, 2008, 2011,
                                          2014, 2017, 2019)) 

(nacimientos <- g1 %>% 
  filter(variable == "Nacimientos") %>% 
   ggplot(aes(anio, valor)) +
    geom_line(color = verde_cess, size = 1.1) +
    scale_x_continuous(limits = c(1996, 2019),
                       breaks = c(1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
                                2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,
                                2014, 2015, 2016, 2017, 2018, 2019)) +
    scale_y_continuous(limits = c(0, 60000),
                     breaks = c(0, 20000, 40000, 60000),
                     labels = c("0", "20.000", "40.000", "60.000")) +
  geom_text(data = nacimiento_labels, 
            nudge_y = .6e-2,
            size = 5,
            aes(label = label)) + 
  labs(x = "",
       y = "",
       subtitle = "Natalidad") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

tgf_labels <- filter(g1 %>% filter(variable == "TGF"), 
                            anio %in% c(1996, 1999, 2002, 2005, 2008, 2011,
                                        2014, 2017, 2019)) 
(tgf <- g1 %>% 
  filter(variable == "TGF") %>% 
  ggplot(aes(anio, valor)) +
  geom_line(color = verde_cess, size = 1.1) +
  scale_x_continuous(limits = c(1996, 2019),
                     breaks = c(1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
                                2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,
                                2014, 2015, 2016, 2017, 2018, 2019)) +
  scale_y_continuous(limits = c(0, 2.5)) +
  geom_text(data = tgf_labels, 
            nudge_y = .6e-2,
            size = 5,
            aes(label = label)) + 
  labs(x = "",
       y = "",
       subtitle = "Tasa Global de Fecundidad",
       caption = "Fuente: Elaboración en base a estadísticas vitales (MSP)") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)))

nacimientos + tgf + 
  plot_layout(ncol = 1) + 
  plot_annotation(title = "Gráfico 1 - Nacimientos y Tasa Global de Fecundidad")

ggsave(here::here("Notas Técnicas", "01_Población", "plots", "graf1_nac_tgf.png"), dpi = 300, width = 10, height = 10)

# Gráfico 3 - Esperanza de vida al nacer

g3 <- read_excel("Notas Técnicas/01_Población/insumos graficos Nota tecnica 1.xlsx", 
                 sheet = "gráfico 3", range = "A3:AE5") %>% 
  rename(sexo = "E(0)") %>% 
  mutate(sexo = str_to_title(sexo)) %>% 
  pivot_longer(- sexo, names_to = "anio", values_to = "valor") %>% 
  mutate(label = paste(valor, "%", sep = ""))


evn_labels <- filter(g2, anio %in% c("2020-2025", "2095-2100"))

g3 %>% 
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
  unnest(proyeccion) %>% 
  ggplot(aes(anio, valor, color = sexo, group = interaction(sexo, proyeccion))) +
  geom_line(aes(linetype = proyeccion), size = 1.1) +
  scale_color_manual(values = c(verde_cess, violeta_cess)) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(data = evn_labels, 
            nudge_y = .6e-2,
            size = 5,
            vjust = -.25,
            hjust = .65,
            aes(label = label),
            color = "black") + 
  labs(x = "",
       y = "",
       color = "",
       title = "Gráfico 3 - Esperanza de vida al nacer",
       caption = "Fuente: WPP - Naciones Unidas. Rev. 2019") +
  guides(linetype = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  ggsave(here::here("Notas Técnicas", "01_Población", "plots", "graf3_esperanza_vida.png"), dpi = 300, width = 11, height = 7)

# Gráfico 4 - Proyección de población total

g4 <- read_excel("Notas Técnicas/01_Población/insumos graficos Nota tecnica 1.xlsx", 
              sheet = "gráfico 4 y 5", range = "A3:D12") %>% 
  rename(anio = `...1`) %>% 
  mutate(label = format(round(Total), big.mark = "."))

polacion_labels <- filter(g4, anio %in% c(2020, 2040, 2060, 2080, 2100))
  
g4 %>% 
  ggplot(aes(anio, Total)) +
  geom_col(position = "dodge", fill = verde_cess) +
  scale_x_continuous(breaks = c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)) +
  scale_y_continuous(breaks = c(0, 500000, 1000000, 1500000, 2000000, 2500000, 3000000, 3500000),
                     labels = c("0", "500.000", "1.000.000", "1.500.000", "2.000.000", "2.500.000", 
                                "3.000.000", "3.500.000")) +
  geom_text(data = polacion_labels, 
            position = position_dodge(1),
            vjust = -0.25,
            size = 5,
            aes(label = label)) + 
  labs(x = "",
       y = "",
       title = "Gráfico 4 - Proyección de población total",
       caption = "Fuente: elaboración propia") +
  ggsave(here::here("Notas Técnicas", "01_Población", "plots", "graf4_proyeccion_poblacion.png"), dpi = 300, width = 11, height = 7)

# Gráfico 5 - Proyección de población por sexo

g5 <- read_excel("Notas Técnicas/01_Población/insumos graficos Nota tecnica 1.xlsx", 
                 sheet = "gráfico 4 y 5", range = "A3:D12") %>% 
  rename(anio = `...1`) %>% 
  select(- Total) %>% 
  pivot_longer(- anio, names_to = "sexo", values_to = "valor") %>% 
  mutate(label = format(round(valor), big.mark = "."))

g5 %>% 
  ggplot(aes(anio, valor, group = sexo)) +
  geom_col(aes(fill = sexo), position = "dodge") +
  scale_x_continuous(breaks = c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)) +
  scale_y_continuous(breaks = c(0, 500000, 1000000, 1500000, 2000000),
                     labels = c("0", "500.000", "1.000.000", "1.500.000", "2.000.000")) +
  scale_fill_manual(values = c(verde_cess, violeta_cess)) +
  labs(x = "",
       y = "",
       fill = "",
       title = "Gráfico 5 - Proyección de población por sexo",
       caption = "Fuente: elaboración propia") +
  theme(legend.position = "bottom") +
  ggsave(here::here("Notas Técnicas", "01_Población", "plots", "graf5_proyeccion_sexo_poblacion.png"), dpi = 300, width = 11, height = 7)

# Gráfico 6 - Proyección de población por sexo y edad

g6 <- read_excel("Notas Técnicas/01_Población/insumos graficos Nota tecnica 1.xlsx", 
                 sheet = "gráfico 6", range = "M4:Q10") %>% 
  pivot_longer(starts_with("2"), names_to = "anio", values_to = "valor") %>% 
  mutate(label = paste(round(valor*100), "%", sep = ""))

g6 %>% 
  ggplot(aes(anio, valor, group = grupoedad)) +
  geom_col(aes(fill = grupoedad), position = "dodge") +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8),
                     labels = c(0, 20, 40, 60, 80)) +
  scale_fill_manual(values = c(amarillo_cess, verde_cess, violeta_cess)) +
  facet_wrap(~ sexo) +
  geom_text(position = position_dodge(1),
            vjust = -0.25,
            size = 5,
            aes(label = label)) + 
  theme(legend.position = "bottom") +
  labs(x = "",
       y = "",
       fill = "",
       title = "Gráfico 6 - Proyección de población por sexo y edad",
       caption = "Fuente: elaboración propia") +
  ggsave(here::here("Notas Técnicas", "01_Población", "plots", "graf6_proyeccion_sexo_edad_poblacion.png"), dpi = 300, width = 11, height = 7)

# Gráfico 7 - Pirámide de población
# 
# g7 <- read_excel("Notas Técnicas/01_Población/insumos graficos Nota tecnica 1.xlsx", 
#                  sheet = "gráfico 7", range = "P1:T22") %>% 
#   mutate(edad = fct_inorder(grupoedad))  %>% 
#   select( - grupoedad) %>% 
#   pivot_longer(- edad) %>% 
#   separate(name, into = c("sexo", "year")) %>% 
  
