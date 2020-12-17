library(tidyverse)
library(scales)
library(readxl)
library(here)

source(here::here("estilo_cess.R"))

# Gráfico 1 - Tasa bruta reemplazo 

g1 <- read_excel(here("Diagnóstico", "00_Cobertura y suficiencia", "gross replacement rates.xlsx")) %>%
  filter(LOCATION == "OAVG" | LOCATION == "NOR" | LOCATION == "DNK" | LOCATION == "PRT" |
         LOCATION == "EU28" | LOCATION == "USA" | LOCATION == "CHL" | LOCATION == "BEL" | 
         LOCATION == "FRA"  | LOCATION == "JPN" | LOCATION == "KOR" | LOCATION == "NZL")  %>%
  transmute(Año = as.factor(TIME),
            Tasa = Value,
            Pais = case_when(
                          LOCATION == "OAVG" ~ "OCDE",
                          LOCATION == "EU28" ~ "Unión \nEuropea",
                          LOCATION == "NOR" ~ "Noruega",
                          LOCATION == "DNK" ~ "Dinamarca",
                          LOCATION == "PRT" ~ "Portugal",
                          LOCATION == "USA" ~ "Estados \nUnidos",
                          LOCATION == "CHL" ~ "Chile",
                          LOCATION == "BEL" ~ "Bélgica",
                          LOCATION == "FRA" ~ "Francia",
                          LOCATION == "JPN" ~ "Japón",
                          LOCATION == "KOR" ~ "Corea del \nSur",
                          LOCATION == "NZL" ~ "Nueva \nZelanda"),
            distincion = case_when(
              Pais == "OCDE" ~ T,
              T ~ F)) 


ggplot(g1, aes(reorder(Pais, Tasa), Tasa, fill = distincion)) +
  geom_bar(stat = "identity", width = .3) + 
  geom_text(aes(label = Tasa), vjust = - .5, size = 5) +
  scale_fill_manual(values = c(verde_cess, rosado_cess)) +
  scale_y_continuous(limits = c(0, 80),
                     expand = expansion(mult = c(0, 0))) +
  guides(fill = FALSE) +
  labs(x = "",
       y = "",
       title = "Tasa bruta de reemplazo en los países de la OCDE (2018)",
       caption = "Fuente: elaboración propia con base en OCDE") + 
  ggsave(here("Diagnóstico", "00_Cobertura y suficiencia", "plots", "graf1.png"), 
         dpi = 300, width = 12, height = 7)

# Gráfico 2 - Tasa de cobertura activa (Latinoamérica)

g2 <- read_excel(here("Diagnóstico", "00_Cobertura y suficiencia", "ocupados.xlsx")) %>% 
  transmute(Países = case_when(
                            Países == "ARG" ~ "Argentina",
                            Países == "BOL" ~ "Bolivia",
                            Países == "BRA" ~ "Brasil",
                            Países == "CHI" ~ "Chile",
                            Países == "COL" ~ "Colombia",
                            Países == "CRI" ~ "Costa Rica",
                            Países == "EC" ~ "Ecuador",
                            Países == "SAL" ~ "El Salvador",
                            Países == "GUA" ~ "Guatemala",
                            Países == "GUY" ~ "Guyana",
                            Países == "HON" ~ "Honduras",
                            Países == "MEX" ~ "México",
                            Países == "PAN" ~ "Panamá",
                            Países == "PAR" ~ "Paraguay",
                            Países == "PER" ~ "Perú",
                            Países == "RD"  ~ "República Dominicana",
                            Países == "SUR" ~ "Surinam",
                            Países == "URU" ~ "Uruguay"),
            value = coalesce(`2019`, `2018`, `2017`)) 
  
ggplot(g2, aes(reorder(Países, value), value)) + 
  geom_bar(position = "dodge", stat = "identity", fill = verde_cess) + 
  coord_flip() +
  scale_y_continuous(limits = c(0, 80),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "",
       y = "",
       title = "Tasa de cobertura activa (Latinoamérica)",
       subtitle = "Ocupados cotizantes a la seguridad social (en % de la población ocupada)",
       caption = "Nota: último disponible (2017, 2018 o 2019).\nFuente: elaboración propia con base en SIMS-BID") + 
  ggsave(here("Diagnóstico", "00_Cobertura y suficiencia", "plots", "graf2.png"), 
         dpi = 300, width = 12, height = 8)

# Gráfico 3 - 

g3 <- read_excel(here("Diagnóstico", "00_Cobertura y suficiencia", "Cobertura pensiones.xlsx")) %>%
  filter(Año == 2018 | (Año == 2017 & Pais == "CHI")) %>%
  transmute(Pais = case_when(
                          Pais == "ARG" ~ "Argentina",
                          Pais == "BOL" ~ "Bolivia",
                          Pais == "BRA" ~ "Brasil",
                          Pais == "CHI" ~ "Chile",
                          Pais == "COL" ~ "Colombia",
                          Pais == "CRI" ~ "Costa Rica",
                          Pais == "EC" |  Pais == "ECU"  ~ "Ecuador",
                          Pais == "SAL" ~ "El Salvador",
                          Pais == "GUA" ~ "Guatemala",
                          Pais == "GUY" ~ "Guyana",
                          Pais == "HON" ~ "Honduras",
                          Pais == "MEX" ~ "México",
                          Pais == "PAN" ~ "Panamá",
                          Pais == "PAR" ~ "Paraguay",
                          Pais == "PER" ~ "Perú",
                          Pais == "RD"  ~ "República Dominicana",
                          Pais == "SUR" ~ "Surinam",
                          Pais == "URU" ~ "Uruguay"),
            Pensión = ifelse(Pensión == "PC", "Pensión Contributiva", "Pensión No Contributiva"),
            tasa = as.numeric(valor))

ggplot(g3, aes(Pais, tasa, fill = Pensión)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c(verde_cess, violeta_cess)) +
  scale_y_continuous(limits = c(0, 100),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = "",
       y = "",
       fill = "",
       title = "Tasa de cobertura pasiva (Latinoamérica, 2018)",
       subtitle = "Personas de 65 o más años que declaran un monto recibido por pensión \ncontributiva/no contributiva (en %)",
       caption = "Fuente: elaboración propia con base en SIMS-BID") +
  ggsave(here("Diagnóstico", "00_Cobertura y suficiencia", "plots", "graf3.png"),
         dpi = 300, width = 11, height = 8)


g4 <- read_excel(here("Diagnóstico", "00_Cobertura y suficiencia", "informalidad europa.xlsx")) %>% 
  mutate(Pais = case_when(
    Pais == "ALE" ~ "Alemania",
    Pais == "EU" ~ "Unión Europea",
    Pais == "ITA" ~ "Italia",
    Pais == "SUE" ~ "Suecia"))

ggplot(g4, aes(reorder(Pais, tasa), tasa)) +
  geom_bar(stat = "identity", width = .3, fill = verde_cess) +
  geom_text(aes(label = tasa), vjust = -.5, size = 5) +
  scale_y_continuous(limits = c(0, 25),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "",
       y = "",
       title = "Tasa de informalidad en la Unión Europea",
       caption = "Fuente: elaboración propia con base en OIT") + 
  ggsave(here("Diagnóstico", "00_Cobertura y suficiencia", "plots", "graf4.png"), 
         dpi = 300, width = 10, height = 7)


# Gráfico 5

g5 <- read_excel(here("Diagnóstico", "00_Cobertura y suficiencia", "Cobertura y suficiencia.xlsx"), 
                 sheet = "g5") %>% 
  mutate(label = paste0(round(`Cobertura efectiva`*100, 1), "%", sep = " "))

g5_labels <- filter(g5, Año == 2018)

ggplot(g5, aes(as.factor(Año), `Cobertura efectiva`)) +
  geom_bar(stat = "identity", width = .4, fill  = verde_cess) + 
  geom_text(data = g5_labels,
            aes(label = label), vjust = - .5, size = 5) +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     limits = c(0, 0.85),
                     labels = c(0, 20, 40, 60, 80)) +
  labs(x = "",
       y = "",
       title = "Cobertura activa del Sistema de Seguridad Social",
       subtitle = "(Cotizantes/PEA)",
       caption = "Fuente: elaboración propia con base en BPS") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1.2)) +
  ggsave(here("Diagnóstico", "00_Cobertura y suficiencia", "plots", "graf5.png"), 
         dpi = 300, width = 12, height = 7)

# Gráfico 6

g6 <- read_excel(here("Diagnóstico", "00_Cobertura y suficiencia", "Cobertura y suficiencia.xlsx"), 
                 sheet = "g6") %>%
  pivot_longer(- Año, names_to = "tipo_pasividad", values_to = "value") %>% 
  mutate(label = paste0(round(value*100, 1), "%", sep = " "))

g6_labels <- filter(g6, Año %in% c(2004, 2018))

ggplot(g6, aes(as.factor(Año), value, fill = tipo_pasividad)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c(verde_cess, amarillo_cess, rosado_cess)) +
  geom_text(data = g6_labels,
            aes(label = label,
                fontface = 2), 
            position = position_stack(.5),
          size = 5) +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     labels = c("", "", "", "", "")) +
  labs(x = "",
       y = "",
       fill = "",
       title = "Cobertura del Sistema de Seguridad Social",
       subtitle = "Población de 65 años y más",
       caption = "Fuente: elaboración propia con base en BPS") + 
  ggsave(here("Diagnóstico", "00_Cobertura y suficiencia", "plots", "graf6.png"), 
         dpi = 300, width = 12, height = 7)

# Gráfico 7

g7 <- read_excel(here("Diagnóstico", "00_Cobertura y suficiencia", "Cobertura y suficiencia.xlsx"), 
                 sheet = "g7") %>%
  pivot_longer(- Año, names_to = "grupo_etario", values_to = "value") %>% 
  mutate(grupo_etario = factor(grupo_etario, levels = c("Menos de 6", "65 y más", "Total")))


ggplot(g7, aes(as.factor(Año), value, color = grupo_etario, group = grupo_etario)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(verde_cess, amarillo_cess, rosado_cess)) +
  scale_y_continuous(limits = c(0, 60), 
                     breaks = c(0, 10, 20, 30, 40, 50, 60),
                     expand = expansion(mult = c(0, .05))) +
  labs(x = "",
       y = "",
       color = "",
       title = "Pobreza en personas por grupos de edades",
       subtitle = "Total país, en porcentaje",
       caption = "Fuente: elaboración propia con base en INE") + 
  ggsave(here("Diagnóstico", "00_Cobertura y suficiencia", "plots", "graf7.png"), 
         dpi = 300, width = 12, height = 7)
