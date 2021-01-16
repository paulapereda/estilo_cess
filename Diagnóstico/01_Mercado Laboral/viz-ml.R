library(lubridate)
library(tidyverse)
library(scales)
library(readxl)
library(here)

source(here::here("estilo_cess.R"))

# Gráfico - automatización

g <- read_excel(here("Diagnóstico", "01_Mercado Laboral", "SectoresAutoat.xlsx"))

# Gráfico 0 - Informalidad según sexo

g0 <- read_excel(here("Diagnóstico", "01_Mercado Laboral", "Informalidad CESS.xlsx"), 
                 sheet = "Sheet1") %>% 
  mutate(valor = valor/100)

g0 %>% 
  ggplot(aes(anio, valor, color = sexo)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(verde_cess, violeta_cess, amarillo_cess)) +
  scale_x_continuous(breaks = seq(2006, 2018, by = 1)) +
  scale_y_continuous(limits = c(.2, .4),
                     expand = expansion(mult = c(0, NA)),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "",
       y = "", 
       color = "",
       title = "Tasa de informalidad por sexo") +
  ggsave(here("Diagnóstico", "01_Mercado Laboral", "plots", "graf0.png"), 
         dpi = 300, width = 12, height = 7)

# Gráfico 1 - Tasa de actividad por sexo

g1 <- read_excel(here("Diagnóstico", "01_Mercado Laboral", "Graficos_cap1.xlsx"), sheet = "g1") %>% 
  pivot_longer(- anio, names_to = "tasa_actividad", values_to = "value") %>% 
  mutate(tasa_actividad = factor(tasa_actividad, levels = c("Hombres", "Mujeres", "Global")))

ggplot(g1, aes(as.numeric(anio), value, color = tasa_actividad, group = tasa_actividad)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(verde_cess, violeta_cess, amarillo_cess)) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 5)) +
  scale_y_continuous(limits = c(.3, .8),
                     expand = expansion(mult = c(0, NA)),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "",
       y = "", 
       color = "",
       title = "Tasa de actividad por sexo") +
  ggsave(here("Diagnóstico", "01_Mercado Laboral", "plots", "graf1.png"), 
         dpi = 300, width = 12, height = 7)

# Gráfico 2 - Tasa de actividad por sexo y tramo de edad

g2 <- read_excel(here("Diagnóstico", "01_Mercado Laboral", "Graficos_cap1.xlsx"), sheet = "g2") %>% 
  pivot_longer(- c(anio, sexo), names_to = "tramo_edad", values_to = "value") 

ggplot(g2, aes(anio, value, color = tramo_edad, group = tramo_edad)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(verde_cess, rosado_cess, amarillo_cess)) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 5)) +
  scale_y_continuous(limits = c(0, 1),
                     expand = expansion(mult = c(0, NA)),
                     labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~ sexo) +
  theme(panel.spacing = unit(2, "lines")) +
  labs(x = "",
       y = "", 
       color = "",
       title = "Tasa de actividad por sexo y tramo de edad") +
  ggsave(here("Diagnóstico", "01_Mercado Laboral", "plots", "graf2.png"), 
         dpi = 300, width = 11, height = 6)

# Gráfico 3 - Tasa de actividad por tramo de edad

g3 <- read_excel(here("Diagnóstico", "01_Mercado Laboral", "Graficos_cap1.xlsx"), sheet = "g3") %>% 
  pivot_longer(- anio, names_to = "tramo_edad", values_to = "value") 

ggplot(g3, aes(anio, value, color = tramo_edad, group = tramo_edad)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(verde_cess, rosado_cess, amarillo_cess)) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 5)) +
  scale_y_continuous(limits = c(.3, .9),
                     breaks = seq(.3, .9, by = .1),
                     expand = expansion(mult = c(0, .01)),
                     labels = scales::percent_format(accuracy = 1)) +
  theme(panel.spacing = unit(2, "lines")) +
  labs(x = "",
       y = "", 
       color = "",
       title = "Tasa de actividad por tramo de edad") +
  ggsave(here("Diagnóstico", "01_Mercado Laboral", "plots", "graf3.png"), 
         dpi = 300, width = 11, height = 6)

# Gráfico 4 - Tasa de empleo por sexo

g4 <- read_excel(here("Diagnóstico", "01_Mercado Laboral", "Graficos_cap1.xlsx"), sheet = "g4") %>% 
  pivot_longer(- anio, names_to = "tasa_empleo", values_to = "value") %>% 
  mutate(tasa_empleo = factor(tasa_empleo, levels = c("Hombres", "Mujeres", "Total")))

ggplot(g4, aes(anio, value, color = tasa_empleo, group = tasa_empleo)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(verde_cess, violeta_cess, amarillo_cess)) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 5)) +
  scale_y_continuous(limits = c(.3, .75),
                     breaks = seq(.3, .75, by = .1),
                     expand = expansion(mult = c(0, NA)),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "",
       y = "", 
       color = "",
       title = "Tasa de empleo por sexo") +
  ggsave(here("Diagnóstico", "01_Mercado Laboral", "plots", "graf4.png"), 
         dpi = 300, width = 12, height = 7)

# Gráfico 5 - Tasa de empleo por tramo de edad

g5 <- read_excel(here("Diagnóstico", "01_Mercado Laboral", "Graficos_cap1.xlsx"), sheet = "g5") %>% 
  pivot_longer(- anio, names_to = "tramo_edad", values_to = "value") 

ggplot(g5, aes(anio, value, color = tramo_edad, group = tramo_edad)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(verde_cess, rosado_cess, amarillo_cess)) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 5)) +
  scale_y_continuous(limits = c(.2, .85),
                     breaks = seq(.2, .85, by = .1),
                     expand = expansion(mult = c(0, .01)),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "",
       y = "", 
       color = "",
       title = "Tasa de empleo por tramo de edad") +
  ggsave(here("Diagnóstico", "01_Mercado Laboral", "plots", "graf5.png"), 
         dpi = 300, width = 11, height = 6)

# Gráfico 6 - Evolución de la cantidad de ocupados totales

g6 <- read_excel(here("Diagnóstico", "01_Mercado Laboral", "Graficos_cap1.xlsx"), sheet = "g6")

ggplot(g6, aes(anio, valor)) +
  geom_line(size = 1.5, color = verde_cess) +
  scale_x_continuous(breaks = seq(1998, 2018, by = 2)) +
  scale_y_continuous(limits = c(800000, 1500000),
                     breaks = seq(800000, 1500000, by = 200000),
                     expand = expansion(mult = c(0, .01)),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  labs(x = "",
       y = "", 
       color = "",
       title = "Evolución de la cantidad de ocupados totales") +
  ggsave(here("Diagnóstico", "01_Mercado Laboral", "plots", "graf6.png"), 
         dpi = 300, width = 11, height = 6)

# Gráfico 7 - Evolución de las categorías de ocupación  

g7 <- read_excel(here("Diagnóstico", "01_Mercado Laboral", "Graficos_cap1.xlsx"), sheet = "g7") %>% 
  pivot_longer(- anio, names_to = "categoria_ocupacion", values_to = "value") 

ggplot(g7, aes(anio, value, fill = categoria_ocupacion)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = c(verde_cess, amarillo_cess, verde_cess2, violeta_cess, violeta_cess2, amarillo_cess2, rosado_cess)) +
  scale_x_continuous(breaks = seq(1981, 2019, by = 3)) +
  scale_y_continuous(expand = expansion(mult = c(0, .01)),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "",
       y = "", 
       fill = "",
       title = "Evolución de las categorías de ocupación",
       caption = "Fuente: elaboración propia con base en ECH") +
  ggsave(here("Diagnóstico", "01_Mercado Laboral", "plots", "graf7.png"), 
         dpi = 300, width = 11, height = 6)

# Gráfico 8 - Distribución de los ocupados totales según categoría ocupacional (2011 - 2019)

g8 <- read_excel(here("Diagnóstico", "01_Mercado Laboral", "Graficos_cap1.xlsx"), sheet = "g8") %>% 
  pivot_longer(- anio, names_to = "categoria_ocupacion", values_to = "value") %>% 
  mutate(categoria_ocupacion = case_when(
    categoria_ocupacion == "Asalariado Privado" ~ "Asalariado \nPrivado",
    categoria_ocupacion == "Asalariado Público" ~ "Asalariado \nPúblico",
    categoria_ocupacion == "Cuenta propia sin local" ~ "Cuenta propia \nsin local",
    categoria_ocupacion == "Cuenta propia con local" ~ "Cuenta propia \ncon local",
    T ~ categoria_ocupacion))

ggplot(g8, aes(categoria_ocupacion, value, fill = as.character(anio))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c(verde_cess, rosado_cess)) +
  scale_y_continuous(limits = c(0, .6),
                     expand = expansion(mult = c(0, .01)),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "",
       y = "", 
       fill = "",
       title = "Distribución de los ocupados totales según categoría ocupacional (2011 - 2019)") +
  ggsave(here("Diagnóstico", "01_Mercado Laboral", "plots", "graf8.png"), 
         dpi = 300, width = 12, height = 7)

# Gráfico 9 - Proyección de la Población en Edad de Trabajar (2020 - 2100)

g9 <- read_excel(here("Diagnóstico", "01_Mercado Laboral", "Graficos_cap1.xlsx"), sheet = "g9") %>% 
  pivot_longer(- anio, names_to = "sexo", values_to = "value") 

ggplot(g9, aes(anio, value, fill = as.character(sexo))) +
  geom_area() +
  scale_fill_manual(values = c(verde_cess, violeta_cess)) +
  scale_x_continuous(breaks = seq(2015, 2100, by = 5)) +
  scale_y_continuous(limits = c(0, 3500000),
                     breaks = seq(0, 3500000, by = 500000),
                     expand = expansion(mult = c(0, .01)),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  labs(x = "",
       y = "", 
       fill = "",
       title = "Población en Edad de Trabajar proyectada a 2100") +
  ggsave(here("Diagnóstico", "01_Mercado Laboral", "plots", "graf9.png"), 
         dpi = 300, width = 12, height = 7)

# Gráfico 10 - Brecha de género en el mundo

ta_todos <- read_rds(here("Diagnóstico", "01_Mercado Laboral", "ta_todos.rds"))

ta_todos %>% 
  mutate(country = case_when(
    country == "Azerbaijan" ~ "Azerbaiyán",
    country == "Brunei Darussalam" ~ "Brunéi",
    country == "Brazil" ~ "Brasil",
    country == "Canada" ~ "Canadá",
    country == "Switzerland" ~ "Suiza",
    country == "Cyprus" ~ "Chipre",
    country == "Czech Republic" ~ "Chequia",
    country == "Germany" ~ "Alemania",
    country == "Denmark" ~ "Dinamarca",
    country == "Dominican Republic" ~ "República Dominicana",
    country == "Spain" ~ "España",
    country == "Finland" ~ "Finlandia",
    country == "United Kingdom" ~ "Reino Unido",
    country == "Greece" ~ "Grecia",
    country == "Hungary" ~ "Hungría",
    country == "Ireland" ~ "Irlanda",
    country == "Japan" ~ "Japón",
    country == "Korea, Rep." ~ "Corea del Sur",
    country == "St. Lucia" ~ "Santa Lucía",
    country == "Luxembourg" ~ "Luxemburgo",
    country == "Latvia" ~ "Letonia",
    country == "North Macedonia" ~ "República de Macedonia",
    country == "Myanmar" ~ "Birmania",
    country == "Malta" ~ "República de Malta",
    country == "Mexico" ~ "México",
    country == "Netherlands" ~ "Holanda",
    country == "Norway" ~ "Noruega",
    country == "Panama" ~ "Panamá",
    country == "Philippines" ~ "Filipinas",
    country == "Poland" ~ "Polonia",
    country == "Romania" ~ "Rumania",
    country == "Russian Federation" ~ "Rusia",
    country == "Rwanda" ~ "Ruanda",
    country == "Singapore" ~ "Singapur",
    country == "Slovenia" ~ "Eslovenia",
    country == "Slovak Republic" ~ "Eslovaquia",
    country == "Thailand" ~ "Tailandia",
    country == "Ukraine" ~ "Ucrania",
    country == "United States" ~ "Estados Unidos",
    T ~ country)) %>% 
  arrange(brecha_na) %>%
  mutate(country = fct_inorder(country)) %>%
  ggplot(aes(country, brecha_na)) +
  geom_col(aes(fill = country == "Uruguay")) +
  scale_fill_manual(values = c(violeta_cess, verde_cess)) +
  scale_y_continuous(expand = expansion(add = c(0, 0)),
                     limits = c(0, 40),
                     breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40)) +
  coord_flip() +
  geom_hline(aes(yintercept = mean(brecha_na)),
             color = "grey35", size = .7) +
  annotate("text", 
           x = 10, 
           y = 20, 
           size = 5,
           label = glue::glue("Media: {round(mean(ta_todos$brecha_na), 2)}%")) +
  labs(y = "Brecha de género en la tasa de actividad (%)",
       x = "",
       caption = "Fuente: Organización Internacional del Trabajo",
       title = "Brecha de género en el mundo") +
  theme(legend.position = "none") +
  ggsave(here("Diagnóstico", "01_Mercado Laboral", "plots", "graf10.png"),
         dpi = 300, width = 12, height = 10)


# Gráfico 11 - Cierre de brecha proyectada a 2100

# un df con sexo, tramo, year y tasas
forecast_tasa <- readRDS(here("Diagnóstico", "01_Mercado Laboral", "forecast_tasas_modelo.rds"))

poblacion_forecast <- readRDS(here("Diagnóstico", "01_Mercado Laboral", "forecast_poblacion.rds")) %>% 
  filter(year(year) >= 1990)

# Pegar tablas
df <- poblacion_forecast %>% 
  left_join(forecast_tasa, by = c("year", "sexo", "tramo")) %>% 
  mutate(pea = (tasa * pob) / 100)

# Calcular brecha global
df_tasas_globales <- df %>% 
  group_by(sexo, year = year(year)) %>% 
  summarize(pea = sum(pea),
            pob = sum(pob),
            tasa = pea/pob)

brecha <- df_tasas_globales %>% 
  pivot_wider(id_cols = year, names_from = sexo, values_from = tasa) %>% 
  mutate(brecha = Hombres - Mujeres) 

brecha_labels <- filter(brecha, year %in% c(1990, 2100))

brecha %>% 
  ggplot(aes(year, brecha)) + 
  geom_line(color = verde_cess, size = 1.5) + 
  geom_point(data = brecha_labels, color = verde_cess, size = 1.7) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08)),
                     limits = c(0.08, 0.32), 
                     breaks = seq(0.08, 0.32, by = 0.04),
                     labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020, 2030, 2040, 2050, 
                                2060, 2070, 2080, 2090, 2100)) +
  geom_text(data = brecha_labels, 
            size = 5,
            nudge_y = .75e-2,
            aes(label = scales::percent_format(scale = 100, accuracy = .1)(brecha))) + 
  labs(y = "",
       x = "Año",
       caption = "Nota: la brecha se define la diferencia entre tasas de actividad masculina y femenina.\nFuente: elaboración propia con base en Organización Internacional del Trabajo", 
       title = "Cierre de brecha proyectada a 2100") +
  ggsave(here("Diagnóstico", "01_Mercado Laboral", "plots", "graf11.png"), 
         dpi = 300, width = 10, height = 7)

# Gráfico 12 

pea_proyectada <- read_excel(here("Diagnóstico", "01_Mercado Laboral", "Graficos_cap1.xlsx"), sheet = "g10") %>% 
  select(- PET) %>% 
  mutate(anio = as.numeric(year(`Año`))) %>%
  mutate(proyeccion = year(`Año`) >= 2020) %>%
  add_row(`Año` = as.Date("2020-01-01"), PEA = 1774834, anio = 2020, proyeccion = FALSE)

# PEA Total

pea_labels <- filter(pea_proyectada, anio == 2100)

pea_proyectada %>% 
  ggplot(aes(anio, PEA)) +
  geom_line(aes(linetype = proyeccion), color = verde_cess, size = 1.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08)),
                     limits = c(1000000, 2000000), 
                     breaks = seq(1000000, 2000000, by = 250000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020, 2030, 2040, 2050, 
                                2060, 2070, 2080, 2090, 2100)) +
  annotate("text", x = 2100, y = 1320000, label = "1.343.991", size = 5) +
  labs(x = "",
       y = "",
       caption = "Fuente: elaboración propia con base en Organización Internacional del Trabajo",
       title = "Población Económicamente Activa proyectada a 2100") + 
  guides(linetype = FALSE) +
  ggsave(here("Diagnóstico", "01_Mercado Laboral", "plots", "graf12.png"),
         dpi = 300, width = 11, height = 7)
