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
  pivot_longer(- anio, names_to = "variable", values_to = "valor") 

nacimiento_labels <- filter(g1 %>% filter(variable == "Nacimientos"), 
                              anio %in% c(1996, 1999, 2002, 2005, 2008, 2011,
                                          2014, 2017, 2019)) 

nacimientos <- g1 %>% 
  filter(variable == "Nacimientos") %>% 
   ggplot(aes(anio, valor)) +
    geom_line(color = verde_cess, size = 1.1) +
    scale_x_continuous(breaks = c(1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
                                2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,
                                2014, 2015, 2016, 2017, 2018, 2019)) +
    scale_y_continuous(limits = c(0, 60000),
                     breaks = c(0, 20000, 40000, 60000),
                     labels = c("0", "20.000", "40.000", "60.000")) +
  geom_text(data = nacimiento_labels, 
            nudge_y = .6e-2,
            aes(label = scales::percent_format(scale = 100, accuracy = .1)(brecha))) + 
  labs(x = "",
       y = "",
       subtitle = "Natalidad")

tgf_labels <- filter(g1 %>% filter(variable == "Nacimientos"), 
                            anio %in% c(1996, 1999, 2002, 2005, 2008, 2011,
                                        2014, 2017, 2019)) 
tgf <- g1 %>% 
  filter(variable == "TGF") %>% 
  ggplot(aes(anio, valor)) +
  geom_line(color = verde_cess, size = 1.1) +
  scale_x_continuous(breaks = c(1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
                                2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,
                                2014, 2015, 2016, 2017, 2018, 2019)) +
  scale_y_continuous(limits = c(0, 2.5)) +
  labs(x = "",
       y = "",
       subtitle = "Tasa Global de Fecundidad",
       caption = "Fuente: Elaboración en base a estadísticas vitales (MSP)")

View(insumos_graficos_Nota_tecnica_1)