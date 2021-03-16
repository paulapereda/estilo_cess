library(lubridate)
library(patchwork)
library(tidyverse)
library(ggrepel)
library(scales)
library(readxl)
library(here)

source(here::here("estilo_cess.R"))

# Gráfico 1 - Evolución de la estructura en Uruguay (1970 - 2015)

g1 <- read_excel("Notas Técnicas/01_Población/insumos graficos Nota tecnica 1.xlsx", 
                 range = "H1:J24") %>% 
  transmute(anio = `...1`,
            Nacimientos = Nacimientos,
            TGF = TGF) %>% 
  pivot_longer(- anio, names_to = "variable", values_to = "valor") %>% 
  mutate(label = ifelse(valor < 3, format(round(valor, 2), decimal.mark = ","), 
                        format(round(valor), big.mark = ".")))

nacimiento_labels <- filter(g1 %>% filter(variable == "Nacimientos"), 
                            anio %in% c(1996, 1999, 2002, 2005, 2008, 2011,
                                        2014, 2017, 2019)) 

(nacimientos <- g1 %>% 
    filter(variable == "Nacimientos") %>% 
    ggplot(aes(anio, valor)) +
    geom_line(color = verde_cess, size = 1.5) +
    geom_point(data = nacimiento_labels, color = verde_cess, size = 2) +
    scale_x_continuous(breaks = c(1996, 1999, 2002, 2005, 2008, 2011, 2014, 2017, 2020),
                       expand = expansion(mult = c(0.04, 0.06))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       limits = c(0, 65000),
                       breaks = c(0, 20000, 40000, 60000),
                       labels = c("0", "20.000", "40.000", "60.000")) +
    geom_text(data = nacimiento_labels, 
              nudge_y = 2700,
              size = 5,
              aes(label = label)) + 
    labs(x = "",
         y = "",
         subtitle = "Nacimientos") + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.line.x = element_line(),
          axis.ticks.x = element_line(),
          legend.position = "bottom"))

tgf_labels <- filter(g1 %>% filter(variable == "TGF"), 
                     anio %in% c(1996, 1999, 2002, 2005, 2008, 2011,
                                 2014, 2017, 2019)) 
(tgf <- g1 %>% 
    filter(variable == "TGF") %>% 
    ggplot(aes(anio, valor)) +
    geom_point(data = tgf_labels, color = verde_cess, size = 2) +
    geom_line(color = verde_cess, size = 1.5) +
    scale_x_continuous(breaks = c(1996, 1999, 2002, 2005, 2008, 2011, 2014, 2017, 2020),
                       expand = expansion(mult = c(0.03, 0.06))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08)),
                       limits = c(0, 2.7),
                       breaks = c(0, .5, 1, 1.5, 2, 2.5),
                       labels = c(0, "0,5", "1,0", "1,5", "2,0", "2,5")) +
    geom_text(data = tgf_labels, 
              nudge_y = 0.13,
              size = 5,
              aes(label = label)) + 
    labs(x = "",
         y = "",
         subtitle = "Tasa Global de Fecundidad",
         caption = "Fuente: Elaboración en base a estadísticas vitales (MSP)") + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.line.x = element_line(),
          axis.ticks.x = element_line(),
          legend.position = "bottom"))

nacimientos + tgf + 
  plot_layout(ncol = 1) + 
  plot_annotation(title = "Nacimientos y Tasa Global de Fecundidad")

ggsave(here("Diagnóstico", "Correcciones", "plots", "graf1.png"), 
       dpi = 300, width = 12, height = 8)

# Gráfico 2 - 

g2 <- read_excel(here("Diagnóstico", "Correcciones", "Datos AFAP balances.xls"),
                 sheet = "graf_1") %>%
  mutate(label = paste0(round(value*100, 1), "%"))

g2_labels <- filter(g2, anio == 2020)

g2 %>% 
  ggplot(aes(anio, value)) +
  geom_line(size = 1.5, color = verde_cess) +
  scale_x_continuous(breaks = seq(2003, 2020, by = 1)) +
  scale_y_continuous(limits = c(.3, .65),
                     breaks = seq(0, .65, by = .05),
                     labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, .01))) +
  geom_text(data = g2_labels, 
            size = 5,
            vjust = -.1,
            hjust = .2,
            aes(label = label),
            color = "black") + 
  labs(x = "", 
       y = "", 
       color = "",
       title = "",
       subtitle = "",
       caption = "Fuente: elaboración propia en base a BCU") +
  ggsave(here("Diagnóstico", "Correcciones", "plots", "graf2.png"), 
         dpi = 300, width = 11.5, height = 7)

# Gráfico 3 - 

g3 <- read_excel(here("Diagnóstico", "Correcciones", "Datos AFAP balances.xls"),
                 sheet = "graf_2") %>%
  pivot_longer(- afap, names_to = "anio", values_to = "value") %>% 
  mutate(anio = as.numeric(anio),
         label = paste0(round(value*100, 1), "%"))

g3_labels <- filter(g3, anio == 2020)

g3 %>% 
  ggplot(aes(anio, value, color = afap)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(rosado_cess, verde_cess, amarillo_cess, violeta_cess)) +
  scale_x_continuous(breaks = seq(2003, 2020, by = 1)) +
  scale_y_continuous(limits = c(-.1, 1),
                     breaks = seq(-.1, 1, by = .1),
                     labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, .01))) +
  # geom_text(data = g3_labels, 
  #           size = 5,
  #           vjust = .1,
  #           hjust = .5,
  #           aes(label = label),
  #           color = "black") + 
  labs(x = "", 
       y = "", 
       color = "",
       title = "",
       subtitle = "",
       caption = "Fuente: elaboración propia en base a BCU") +
  ggsave(here("Diagnóstico", "Correcciones", "plots", "graf3.png"), 
         dpi = 300, width = 11, height = 7)

# Gráfico 4 -  

g4 <- read_excel(here("Diagnóstico", "04_Demografía", "Book1.xlsx"), sheet = "g") %>% 
  pivot_longer(-anio, names_to = "grupo", values_to = "value") %>% 
  mutate(grupo = factor(grupo, levels = c("Relación de dependencia - total", 
                                          "Relación de dependencia - niñez",
                                          "Relación de dependencia - vejez"),
                        labels =  c("Total", 
                                    "Niñez",
                                    "Vejez")))


ggplot(g4, aes(anio, value, color = grupo, group = grupo)) +
  geom_line(size = 1.5) + 
  scale_color_manual(values = c(violeta_cess, verde_cess, rosado_cess)) +
  scale_y_continuous(limits = c(0, 1),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "",
       y = "", 
       color = "",
       title = "",
       caption = "Fuente: elaboración propia con base en INE") +
  ggsave(here("Diagnóstico", "Correcciones", "plots", "graf4.png"), 
         dpi = 300, width = 12, height = 7)

# Gráfico 5 - already done in "Anexos"

# Gráfico 6 - 

# g6 <- read_excel(here("Diagnóstico", "Correcciones", "Gráfico altas invalidez.xlsx"),
#                  sheet = "Sheet1") %>%
#   mutate(value_aux = ifelse(tipo == "Altas de invalidez/Altas totales", value*25000, value))
# 
# g6 %>% 
#   ggplot(aes(anio, value_aux, color = tipo, group = tipo)) +
#   geom_line(size = 1.5) +
#   scale_color_manual(values = c(rosado_cess, verde_cess)) +
#   scale_x_continuous(expand = expansion(mult = c(0, 0)),
#                      breaks = seq(2005, 2020, by = 1)) +
#   scale_y_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000),
#                      limits = c(0, 7000),
#                      labels = function(x) format(x, big.mark = ".", scientific = FALSE),
#                      sec.axis = sec_axis(trans = ~./25000, 
#                                          breaks = c(0, .02, .04, .06, .08, .1, .12, .14, .16, .18, .2, .22),
#                                          labels = function(x) format(x, big.mark = ".", scientific = FALSE)),
#                      expand = expansion(mult = c(.001, .1))) +
#   labs(x = "", 
#        y = "", 
#        color = "",
#        title = "",
#        subtitle = "",
#        caption = "") +
#   ggsave(ggsave(here("Diagnóstico", "Correcciones", "plots", "graf6.png"), 
#                 dpi = 300, width = 12, height = 7)

# Gráfico 7 -

g7 <- read_excel(here("Diagnóstico", "Correcciones", "afesenfuerzaventas.xls"),
                  sheet = "Sheet1")

g7 %>% 
 ggplot(aes(as.character(anio_mes), total)) +
  geom_bar(stat = "identity", fill = verde_cess) +
  scale_x_discrete(breaks = c("199612", "199806", "199912", "200106", "200212", "200406", "200512",
                              "200706", "200812", "201006", "201112", "201306", "201412", "201606",
                              "201712", "201906", "202012")) +
  scale_y_continuous(limits = c(0, 2000),
                     breaks = seq(0, 2000, by = 200),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                     expand = expansion(mult = c(0, .01))) +
  labs(x = "",
       y = "", 
       fill = "",
       title = "",
       caption = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
  ggsave(here("Diagnóstico", "Correcciones", "plots", "graf7.png"), 
         dpi = 300, width = 12, height = 8)

# Gráfico 8 - Tasa de contribución efectiva total

g8 <- read_excel(here("Diagnóstico", "Correcciones", "Tasas de contribución.xlsx"),
                 sheet = "Sheet1") %>% 
  mutate(tipo = case_when(
    pais == "Promedio OCDE"  ~ "B",
    pais == "Promedio Latam" ~ "B",
    pais == "Uruguay" ~ "C",
    TRUE ~ "A"))

ggplot(g8, aes(reorder(pais, value), value, fill = tipo)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(verde_cess, rosado_cess, amarillo_cess)) +
  scale_y_continuous(limits = c(0, .35),
                     breaks = seq(0, .35, by = .05),
                     labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, .01))) +
  guides(fill = FALSE) +
  labs(x = "",
       y = "", 
       fill = "",
       title = "",
       caption = "Fuente: BID (2018). Presente y futuro de las pensiones en América Latina y el Caribe (Altamirano et al.)
OCDE (2019). Pensions at a Glance 2019: OECD and G20 Indicators, OECD Publishing") + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
  ggsave(here("Diagnóstico", "Correcciones", "plots", "graf8.png"), 
         dpi = 300, width = 12, height = 8)

# Gráfico 9 - 

# g9 <- read_excel(here("Diagnóstico", "Correcciones", "Comisiones sobre FAP.xlsx"),
#                  sheet = "Sheet1") %>%
#   pivot_longer(- afap, names_to = "anio", values_to = "value") %>%
#   mutate(anio = as.numeric(anio),
#          label = paste0(round(value*100, 3), "%"))
# 
# g9_labels <- filter(g9, anio == 2020) %>% 
#   filter(!(afap %in% c("SURA AFAP", "Unión AFAP")))
# 
# g9 %>% 
#   ggplot(aes(anio, value, color = afap)) +
#   geom_line(size = 1.5) +
#   scale_color_manual(values = c(rosado_cess, verde_cess, amarillo_cess, violeta_cess)) +
#   scale_x_continuous(breaks = seq(2008, 2020, by = 1)) +
#   scale_y_continuous(limits = c(.002, .022),
#                      breaks = c(.002, .004, .006, .008, .01, .012, .014, .016, .018, .02, .022),
#                      labels = c("0.2%", "0.4%", "0.6%", "0.8%", "1.0%", "1.2%", "1.4%", "1.6%", 
#                                 "1.8%", "2.0%", "2.0%"),
#                      expand = expansion(mult = c(0, .01))) +
#   geom_text(data = g9_labels, 
#             size = 4.5,
#             vjust = -.8,
#             hjust = .5,
#             aes(label = label),
#             color = "black") + 
#   labs(x = "", 
#        y = "", 
#        color = "",
#        title = "",
#        subtitle = "",
#        caption = "Nota: el año 2020 es una estimación.") +
#   ggsave(here("Diagnóstico", "Correcciones", "plots", "graf9.png"), 
#          dpi = 300, width = 11.5, height = 7)


# Gráfico 10 -

g10 <- read_excel(here("Diagnóstico", "Correcciones", "/Gasto en pensiones sobre relacion de dependencia (1) (1).xlsx"),
                 sheet = "Sheet1") 

g10 %>% 
  ggplot(aes(eje1, eje2)) +
  geom_point(aes(color = distincion), size = 4) +
  geom_smooth(method = "lm", se = FALSE, color = "grey45", formula = y ~ x) +
  scale_color_manual(values = c(verde_cess, amarillo_cess)) +
  geom_text_repel(aes(eje1, eje2, label = pais), size = 4.5) +
  scale_x_continuous(limits = c(7, 37),
                     breaks = seq(7, 37, by = 5),
                     expand = expansion(mult = c(0, .01))) +
  scale_y_continuous(limits = c(0, 12),
                     breaks = seq(0, 12, by = 2),
                     expand = expansion(mult = c(0, .01))) + 
  guides(color = F) +
  labs(x = "Relación de dependencia 65+/15-64 (2020)", 
       y = "Gasto público en pensiones como % PIB (2017*)", 
       color = "",
       title = "",
       subtitle = "",
       caption = "* Para Uruguay e Israel datos de 2019 Para Canadá, Hungría y Francia datos de 2018.\nFuentes: OECD Social Expenditure Database.\nA. Arenas de Mesa, “Los sistemas de pensiones en América Latina: institucionalidad, gasto público y sostenibilidad financiera en tiempos del COVID-19”, serie\nMacroeconomía del Desarrollo, N° 212 (LC/TS.2020/99), Santiago, Comisión Económica para América Latina y el Caribe (CEPAL), 2020. United Nations,\nDepartment of Economic and Social Affairs, Population Division (2019). World Population Prospects 2019, Online Edition. Rev. 1.") +
  ggsave(here("Diagnóstico", "Correcciones", "plots", "graf10.png"), 
         dpi = 300, width = 15, height = 8.5)

# Gráfico 11 - 

g11 <- read_excel(here("Diagnóstico", "Correcciones", "Gráfico jubilados nuevo.xlsx"),
                  sheet = "Sheet1") %>% 
  pivot_longer(- caja, names_to = "anio", values_to = "value") %>% 
  mutate(anio = as.numeric(anio),
         label = paste0(format(round(value), big.mark = ".")))

g11_labels <- filter(g11, anio == 2019)

g11 %>% 
  ggplot(aes(anio, value, color = caja, group = caja)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(rosado_cess, verde_cess, amarillo_cess, violeta_cess)) +
  scale_x_continuous(expand = expansion(mult = c(.01, .01)),
                     breaks = seq(2008, 2019, by = 1)) +
  scale_y_continuous(limits = c(1000, 13000),
                     breaks = seq(1000, 13000, by = 2000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                     expand = expansion(mult = c(0, .01))) +
  geom_text(data = g11_labels, 
            size = 5,
            vjust = -.4,
            hjust = 1,
            aes(label = label),
            color = "black") + 
  labs(x = "", 
       y = "", 
       color = "",
       title = "",
       subtitle = "",
       caption = "") +
  ggsave(here("Diagnóstico", "Correcciones", "plots", "graf11.png"), 
         dpi = 300, width = 11.5, height = 7)

# Gráfico 12

g12 <- read_excel(here("Diagnóstico", "Correcciones", "Gráfico jubilados nuevo.xlsx"),
                  sheet = "Sheet2") %>% 
  mutate(anio = as.numeric(anio),
         label = paste0(format(round(value), big.mark = ".")))

g12_labels <- filter(g12, anio == 2019)

g12 %>% 
  ggplot(aes(anio, value, color = caja, group = caja)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(rosado_cess, verde_cess, amarillo_cess, violeta_cess)) +
  scale_x_continuous(expand = expansion(mult = c(.01, .01)),
                     breaks = seq(2008, 2019, by = 1)) +
  scale_y_continuous(limits = c(12500, 37500),
                     breaks = seq(12500, 37500, by = 5000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                     expand = expansion(mult = c(0, .01))) +
  geom_text(data = g12_labels, 
            size = 4.5,
            vjust = .2,
            hjust = 1,
            aes(label = label),
            color = "black") + 
  labs(x = "", 
       y = "", 
       color = "",
       title = "",
       subtitle = "",
       caption = "") +
  ggsave(here("Diagnóstico", "Correcciones", "plots", "graf12.png"), 
         dpi = 300, width = 11.5, height = 7)

# Gráfico 13 

g13 <- read_excel(here("Diagnóstico", "Correcciones", "rentabilidad IPC.xlsx"),
                  sheet = "Sheet1") %>% 
  pivot_longer(- anio, names_to = "afap", values_to = "value") %>% 
  mutate(anio = as.numeric(anio))

g13 %>% 
  ggplot(aes(anio, value, color = afap)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(rosado_cess, verde_cess, amarillo_cess, violeta_cess)) +
  scale_x_continuous(expand = expansion(mult = c(0, .01)),
                     limits = c(2001, 2020),
                     breaks = seq(2001, 2020, by = 1)) +
  scale_y_continuous(limits = c(-7, 21),
                     breaks = seq(-7, 21, by = 7),
                     expand = expansion(mult = c(0, .01))) +
  labs(x = "", 
       y = "", 
       color = "",
       title = "",
       subtitle = "",
       caption = "") +
  ggsave(here("Diagnóstico", "Correcciones", "plots", "graf13.png"), 
         dpi = 300, width = 11.5, height = 7)

# Gráfico 14

g14 <- read_excel(here("Diagnóstico", "Correcciones", "articulo_8.xlsx")) %>%
  mutate(nivel = factor(nivel, levels = c("Nivel 1", "Nivel 2", "Nivel 3")),
         inst = factor(inst, levels = c("BPS", "AFAP")),
         art = factor(art, levels = c("T", "F"), 
                      labels = c("Distribución de aportes aplicando Art. 8", 
                                 "Distribución de aportes sin aplicar Art. 8")))

g14 %>% 
  ggplot(aes(nivel, value, fill = inst))  + 
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c(rosado_cess, verde_cess)) +
  facet_wrap(~ art) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)),
                     breaks = c(0, 67564, 101346, 202693),
                     labels = c("$0", "$67.564", "$101.346", "$202.693")) +
  labs(x = "", 
       y = "", 
       fill = "",
       title = "",
       subtitle = "",
       caption = "") +
  ggsave(here("Diagnóstico", "Correcciones", "plots", "graf14_.png"), 
         dpi = 300, width = 11.5, height = 7)

  
# Gráfico 15

g15 <- read_excel(here("Diagnóstico", "Correcciones", "cuadro_rentabilidad_ipc_plots.xlsx"), 
                  sheet = "acumulacion") %>%
  pivot_longer(- anio, names_to = "afap", values_to = "value")

g15 %>% 
  ggplot(aes(anio, value, color = afap, group = afap)) +
  geom_line(size = 1) +
  scale_color_manual(values = c(rosado_cess, verde_cess, amarillo_cess, violeta_cess)) +
  scale_x_discrete(expand = expansion(mult = c(0, .01)),
                     breaks = c("2000.12", "2001.12", "2002.12", "2003.12", "2004.12", "2005.12",
                                "2006.12", "2007.12", "2008.12", "2009.12", "2010.12", "2011.12",
                                "2012.12", "2013.12", "2014.12", "2015.12", "2016.12", "2017.12",
                                "2018.12", "2019.12", "2020.12")) +
  scale_y_continuous(limits = c(-7, 21),
                     breaks = seq(-7, 21, by = 7),
                     expand = expansion(mult = c(0, .01))) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
  labs(x = "", 
       y = "", 
       color = "",
       title = "",
       subtitle = "",
       caption = "") +
  ggsave(here("Diagnóstico", "Correcciones", "plots", "graf15.png"), 
         dpi = 300, width = 11.5, height = 7)
