library(lubridate)
library(tidyverse)
library(scales)

source(here::here("estilo_cess.R"))

# Gráfico 1 - Brecha de género en la tasa de actividad

ta_todos <- read_rds(here::here("Notas Técnicas", "02_PEA", "ta_todos.rds"))

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
       title = "Gráfico 1 - Brecha de género en el mundo") +
  theme(legend.position = "none") +
  ggsave(here::here("Notas Técnicas", "02_PEA", "plots", "ta_todos.png"), dpi = 300, width = 10, height = 10)

# Gráfico 2 - Cierre de brecha proyectada a 2100

# un df con sexo, tramo, year y tasas
forecast_tasa <- readRDS(here::here("Notas Técnicas", "02_PEA", "forecast_tasas_modelo.rds"))

poblacion_forecast <- readRDS(here::here("Notas Técnicas", "02_PEA", "forecast_poblacion.rds")) %>% 
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
  geom_line(color = verde_cess, size = 1.1) + 
  geom_point(data = brecha_labels, color = verde_cess, size = 1.7) +
  scale_y_continuous(limits = c(0, 0.35), labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020, 2030, 2040, 2050, 
                                2060, 2070, 2080, 2090, 2100)) +
  geom_text(data = brecha_labels, 
            size = 5,
            nudge_y = .75e-2,
            aes(label = scales::percent_format(scale = 100, accuracy = .1)(brecha))) + 
  labs(y = "",
       x = "Año",
       caption = "Fuente: elaboración propia con base en Organización Internacional del Trabajo", 
       title = "Gráfico 2 - Cierre de brecha proyectada a 2100") +
  ggsave(here::here("Notas Técnicas", "02_PEA", "plots", "brecha_proyectada.png"), dpi = 300, width = 10, height = 7)

# Gráfico 3

pea_proyectada <- df %>% 
  select(- rojos) %>% 
  group_by(`Año` = year) %>% 
  summarize(PEA = round(sum(pea)),
            PET = round(sum(pob))) %>% 
  mutate(anio = as.numeric(year(`Año`)))

# PEA Total

pea_labels <- filter(pea_proyectada, anio == 2100)

pea_proyectada %>% 
  mutate(proyeccion = year(`Año`) >= 2020) %>%
  ggplot(aes(anio, PEA)) +
  geom_line(aes(linetype = proyeccion), color = verde_cess, size = 1.1) +
  scale_y_continuous(limits = c(0, 1900000),
                     breaks = c(0, 500000, 1000000, 1500000),
                     labels = c("0", "500.000", "1.000.000", "1.500.000")) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020, 2030, 2040, 2050, 
                                2060, 2070, 2080, 2090, 2100)) +
  annotate("text", x = 2100, y = 1270000, label = "1.301.961", size = 5) +
  labs(x = "Año",
       y = "Población Económicamente Activa",
       caption = "Fuente: elaboración propia con base en Organización Internacional del Trabajo",
       title = "Gráfico 3 - Población Económicamente Activa proyectada a 2100") + 
  guides(linetype = FALSE) +
  ggsave(here::here("Notas Técnicas", "02_PEA", "plots", "pea_proyectada.png"), dpi = 300, width = 11, height = 7)
