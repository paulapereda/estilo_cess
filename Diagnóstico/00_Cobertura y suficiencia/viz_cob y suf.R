library(tidyverse)
library(scales)
library(readxl)
library(here)

source(here::here("estilo_cess.R"))

## Tasa bruta reemplazo 

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
       title = "Tasa bruta de reemplazo en los países de la OCDE",
       caption = "Fuente: elaboración propia con base en OCDE") + 
  ggsave(here("Diagnóstico", "00_Cobertura y suficiencia", "plots", "graf1.png"), 
         dpi = 300, width = 10, height = 7)


## Tasa cobertura activa LATAM, ESTRE GRAFICO TBIEN QUEDA FALTA OBVIAMENTE EMPROLIJAR
## TAMBIEN SE LE SACAN LAS LABELS
## TAMBIEN SE ORDENARIA POR EJEMPLO EN BASE A ALGUN AÑO EN EL QUE HAYA INFO PARA TODOS LOS PAISES
## EL TITULO SERIA TASA DE COBERTURA (Ocupados cotizantes a la seguridad social (en % de la población ocupada))
## FUENTE ELABORACION PROPIA CON DATOS BID SIMS

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


## Tasa cobertura pasiva LATAM ##
##TAMBIEN LES SACAMOS LAS LABELS X, Y
## TAMBIEN SE ORDENARIA 
## EL TITULO SERIA TASA DE COBERTURA PASIVA en 2018(Personas de 65 o más años que declaran un monto recibido por pensión contributiva/ no contributiva (en %))
## FUENTE ELABORACION PROPIA CON DATOS BID SIMS

g3 <- read_excel(here("Diagnóstico", "00_Cobertura y suficiencia", "Cobertura pensiones.xlsx")) %>%
  filter(Año == 2018) %>%
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
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "",
       y = "",
       fill = "",
       title = "Tasa de cobertura pasiva (Latinoamérica, 2018)",
       subtitle = "Personas de 65 o más años que declaran un monto recibido por pensión \ncontributiva/no contributiva (en %)",
       caption = "Fuente: elaboración propia con base en SIMS-BID") +
  ggsave(here("Diagnóstico", "00_Cobertura y suficiencia", "plots", "graf3.png"),
         dpi = 300, width = 12, height = 8)


## Tasa informalidad EU ##
## titulo sería Tasa de Informalidad en Países Desarrollados
## FUente: Elaboración propia datos OIT
## sacamos las labels
## falta emprolijar ejes

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


