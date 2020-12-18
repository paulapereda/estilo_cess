library(tidyverse)
library(scales)
library(readxl)
library(here)

source(here::here("estilo_cess.R"))

# Gráfico 1 - Gasto en pasividades

g1 <- read_excel(here("Diagnóstico", "02_Gasto público previsional", "gps.xlsx")) %>% 
  pivot_longer(-caja, names_to = "anio", values_to = "value") %>% 
  mutate(caja = case_when(
    caja == "BPS" ~ "BPS (eje izquierdo)",
    caja == "DNASSP" ~ "DNASSP (eje derecho)", 
    caja == "SRPFFAA" ~ "SRPFFAA (eje derecho)"),
    value_aux = ifelse(caja != "BPS (eje izquierdo)", value*10, value))

ggplot(g1, aes(anio, value_aux, color = caja, group = caja)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(verde_cess, amarillo_cess, rosado_cess)) +
  scale_x_discrete(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(breaks = c(60000, 80000, 100000, 120000, 140000, 160000, 180000,  
                                200000, 220000, 240000),
                     limits = c(60000, 240000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                     sec.axis = sec_axis(trans = ~./10, 
                                         breaks = c(6000, 8000, 10000, 12000, 14000, 16000, 18000,
                                                    20000, 22000, 24000),
                                         labels = function(x) format(x, big.mark = ".", scientific = FALSE)),
                     expand = expansion(mult = c(.001, .1))) +
  labs(x = "",
       y = "", 
       color = "",
       title = "Gasto en pasividades", 
       subtitle = "$ constantes 2019") +
  ggsave(here("Diagnóstico", "02_Gasto público previsional", "plots", "graf1.png"), 
         dpi = 300, width = 12, height = 7)

# Gráfico 2 - Índice de revaluación de pasividades

g2 <- read_excel(here("Diagnóstico", "02_Gasto público previsional", "gps.xlsx"), sheet = "g2") 

ggplot(g2, aes(anio, value, group = 1)) +
  geom_line(size = 1.5, color = verde_cess) +
  geom_curve(aes(x = 2004, y = 72, xend = 2019, yend = 120), curvature = 0.2, color = "grey25",
             lineend = "round", arrow = arrow(length = unit(0.3, "inches"))) +
  scale_y_continuous(limits = c(70, NA),
                     expand = expansion(mult = c(0, .05))) +
  scale_x_continuous(breaks = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
                                2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)) +  
  annotate("text", x = 2016.5, y = 100, label = "+ 63%", size = 5.5) +
  labs(x = "",
       y = "", 
       color = "",
       title = "Índice de revaluación pasividades (2000 = 100)") +
  ggsave(here("Diagnóstico", "02_Gasto público previsional", "plots", "graf2.png"), 
         dpi = 300, width = 12, height = 7)


# Gráfico 3 - Cantidad de pasividades por categoría - BPS

g3 <- read_excel(here("Diagnóstico", "02_Gasto público previsional", "gps.xlsx"), sheet = "g3") %>% 
  pivot_longer(- anio, names_to = "tipo_pasividad", values_to = "value") %>% 
  mutate(value_aux = ifelse(tipo_pasividad != "Jubilaciones (eje izquierdo)", value*1.6, value))

ggplot(g3, aes(anio, value_aux, color = tipo_pasividad, group = tipo_pasividad)) +
  geom_line(size = 1.5) +
  geom_curve(aes(x = 2008, y = 354130, xend = 2019, yend = 458261), curvature = 0.2, color = "grey25",
             lineend = "round", arrow = arrow(length = unit(0.3, "inches"))) +
  scale_color_manual(values = c(rosado_cess, verde_cess)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)),
                   breaks = c(1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
                              2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)) +
  scale_y_continuous(breaks = c(320000, 340000, 360000, 380000, 400000, 420000, 440000,  
                                460000, 480000),
                     limits = c(320000, 480000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                     sec.axis = sec_axis(trans = ~./1.6, 
                                         breaks = c(200000, 210000, 220000, 230000, 240000,
                                                    250000, 260000, 270000, 280000, 290000, 300000),
                                         labels = function(x) format(x, big.mark = ".", scientific = FALSE)),
                     expand = expansion(mult = c(.001, .1))) +
  annotate("text", x = 2015, y = 390000, label = "+ 29%", size = 5.5) +
  labs(x = "",
       y = "", 
       color = "",
       title = "Cantidad de pasividades por categoría - BPS") +
  ggsave(here("Diagnóstico", "02_Gasto público previsional", "plots", "graf3.png"), 
         dpi = 300, width = 14, height = 7)

# Gráfico 4 - Altas jubilatorias

g4 <- read_excel(here("Diagnóstico", "02_Gasto público previsional", "gps.xlsx"), sheet = "g4") %>% 
  pivot_longer(- anio, names_to = "tipo_pasividad", values_to = "value")

ggplot(g4, aes(anio, value, fill = tipo_pasividad)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c(rosado_cess, amarillo_cess, verde_cess)) +
  scale_x_continuous(breaks = c(2005, 2006, 2007, 2008, 2009, 2010, 2011,
                                2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)) +  
  scale_y_continuous(limits = c(0, 35000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "",
       y = "", 
       fill = "",
       title = "Altas jubilatorias") +
  ggsave(here("Diagnóstico", "02_Gasto público previsional", "plots", "graf4.png"), 
         dpi = 300, width = 12, height = 7)

# Gráfico 5 - Cantidad de pasivos por servicio de retiro y categoría

g5 <- read_excel(here("Diagnóstico", "02_Gasto público previsional", "gps.xlsx"), sheet = "g5") %>% 
  mutate(value_aux = ifelse(servicio != "SRPFFAA - Jubilaciones (eje izquierdo)", value*2, value),
         value = round(value),
         label = format(value, big.mark = "."),
         label = ifelse(anio == 2019, label, NA))

ggplot(g5, aes(as.factor(anio), value_aux, color = servicio, group = servicio, label = label)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(rosado_cess, verde_cess, violeta_cess, amarillo_cess)) +
  scale_y_continuous(limits = c(26000, 40000),
                     breaks = c(26000, 28000, 30000, 32000, 34000, 36000, 38000, 40000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                     sec.axis = sec_axis(trans = ~./2, 
                                         breaks = c(13000, 14000, 15000, 16000, 17000, 18000, 19000, 20000),
                                         labels = function(x) format(x, big.mark = ".", scientific = FALSE)),
                     expand = expansion(mult = c(0, .25))) + 
  geom_text(size = 5,
            nudge_y = -400, 
            color = "black") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) + 
  labs(x = "",
       y = "", 
       color = "",
       title = "Cantidad de pasivos por servicio de retiro y categoría") +
  ggsave(here("Diagnóstico", "02_Gasto público previsional", "plots", "graf5.png"), 
         dpi = 300, width = 12, height = 7)

# Gráfico 6 - Pasividad mínima y Salario Mínimo Nacional

g6 <- read_excel(here("Diagnóstico", "02_Gasto público previsional", "gps.xlsx"), sheet = "g6") 

ggplot(g6) +
  geom_col(aes(anio, `Salario Mínimo Nacional`), fill = verde_cess) +
  geom_line(aes(anio, `Pasividad Mínima`), size = 1, group = 1, color = amarillo_cess) +
  geom_point(aes(anio, `Pasividad Mínima`), size = 3, group = 1, color = amarillo_cess) +
  scale_x_continuous(breaks = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 
                                2016, 2017, 2018, 2019, 2020)) +
  scale_y_continuous(limits = c(0, 16500),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                     expand = expansion(mult = c(0, 0))) +
  annotate("text", x = 2008, y = 5643, label = "53%", size = 5.5) +
  annotate("text", x = 2020, y = 11953, label = "85%", size = 5.5) +
  labs(x = "",
       y = "", 
       fill = "",
       title = "Pasividad mínima y Salario Mínimo Nacional",
       subtitle = "$ constantes 2019",
       caption = "El año 2020 incorpora el ajuste de julio del mismo año") +
  ggsave(here("Diagnóstico", "02_Gasto público previsional", "plots", "graf6.png"), 
         dpi = 300, width = 12, height = 7)
# 
# g <- read_excel(here("Diagnóstico", "02_Gasto público previsional", "gps.xlsx"), sheet = "g6") %>% 
#   pivot_longer(- anio)
# 
# ggplot(g, aes(anio, value, fill = name)) +
#   geom_col() +
#   scale_fill_manual(values = c(amarillo_cess, verde_cess))
#   

# Gráfico 7 - Gasto en pasividades

g7 <- read_excel(here("Diagnóstico", "02_Gasto público previsional", "gps.xlsx"), sheet = "g7") %>% 
  pivot_longer(- anio, names_to = "caja", values_to = "value") %>% 
  mutate(label = paste0(round(value*100, 1), "%", sep = " "))

g7_labels <- filter(g7, anio %in% c(2008, 2019))

ggplot(g7, aes(as.character(anio), value, fill = caja)) + 
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c(verde_cess, amarillo_cess, rosado_cess)) +
  geom_text(data = g7_labels,
            aes(label = label,
                fontface = 2), 
            position = position_stack(.5),
            size = 5) +
  scale_y_continuous(limits = c(NA, 0.115),
                     expand = expansion(mult = c(0, NA)),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "",
       y = "", 
       fill = "",
       title = "Gasto en pasividades",
       subtitle = "% del PIB",
       caption = "Fuente: elaboración propia con base en BCU, BPS, MEF") +
  ggsave(here("Diagnóstico", "02_Gasto público previsional", "plots", "graf7.png"), 
         dpi = 300, width = 12, height = 7)

# Gráfico 8 - Puestos cotizantes BPS

g8 <- read_excel(here("Diagnóstico", "02_Gasto público previsional", "gps.xlsx"), sheet = "g8") 

ggplot(g8, aes(anio, value)) +
  geom_line(size = 1.5, color = verde_cess) +
  scale_x_continuous(breaks = c(2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012,
                                2013, 2014, 2015, 2016, 2017, 2018, 2019)) +
  scale_y_continuous(limits = c(500000, 1500000),
                     breaks = c(500000, 750000, 1000000, 1250000, 1500000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "",
       y = "", 
       fill = "",
       title = "Puestos cotizantes (BPS)",
       subtitle = "Promedio anual") +
  ggsave(here("Diagnóstico", "02_Gasto público previsional", "plots", "graf8.png"), 
         dpi = 300, width = 12, height = 7)

# Gráfico 9 - Asistencia financiera de Rentas Generales

g9 <- read_excel(here("Diagnóstico", "02_Gasto público previsional", "gps.xlsx"), sheet = "g9") %>% 
  pivot_longer(- anio, names_to = "caja", values_to = "value") %>% 
  mutate(label = paste0(round(value*100, 1), "%", sep = " ")) 

g9_labels <- filter(g9, anio %in% c(2008, 2019))

ggplot(g9, aes(as.character(anio), value, fill = caja)) + 
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c(verde_cess, amarillo_cess, rosado_cess)) +
  geom_text(data = g9_labels,
            aes(label = label,
                fontface = 2), 
            position = position_stack(.5),
            size = 5) +
  scale_y_continuous(limits = c(0, 0.025),
                     breaks = c(0, 0.005, 0.01, 0.015, 0.02, 0.025),
                     expand = expansion(mult = c(0, 0)),
                     labels = c("0%", "0,5%", "1%", "1,5%", "2%", "2,5%")) +
  labs(x = "",
       y = "", 
       fill = "",
       title = "Asistencia financiera de Rentas Generales",
       subtitle = "% del PIB",
       caption = "Fuente: elaboración propia con base en MEF") +
  ggsave(here("Diagnóstico", "02_Gasto público previsional", "plots", "graf9.png"), 
         dpi = 300, width = 12, height = 7)
