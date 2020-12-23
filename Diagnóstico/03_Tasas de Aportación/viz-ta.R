library(lubridate)
library(tidyverse)
library(scales)
library(readxl)
library(here)

source(here::here("estilo_cess.R"))

g1 <- read_excel(here("Diagnóstico", "03_Tasas de Aportación", "Tasa de aporte.xlsx")) %>% 
  mutate(`Tasa de contribución obligatoria` = round(`Tasa de contribución obligatoria`),
         distinction = ifelse(País == "Uruguay (CD)" | País == "Uruguay", T, F))

ggplot(g1, aes(reorder(País, `Tasa de contribución obligatoria`), `Tasa de contribución obligatoria`,
               fill = distinction)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = `Tasa de contribución obligatoria`), 
            size = 5,
            position = position_stack(vjust = 0.25)) +
  scale_fill_manual(values = c(verde_cess, amarillo_cess)) +
  scale_y_continuous(limits = c(0, 35),
                     breaks = seq(0, 35, by = 5),
                     labels = c("0%", "5%", "10%", "15%", "20%", "25%", "30%", "35%"),
                     expand = expansion(mult = c(0, .01))) +
  geom_hline(yintercept = 11.4, linetype = "dashed") + 
  annotate("text", label = "Promedio Latinoamérica = 11,4%", x = 3, y = 12, hjust = 0) + 
  geom_hline(yintercept = 18.4) + 
  annotate("text", label = "Promedio ODCE = 18,4%", x = 3, y = 19, hjust = 0) + 
  guides(fill = FALSE) +
  labs(x = "",
       y = "", 
       title = "Tasa de contribución obligatoria",
       caption = "Fuente: elaboración propia con base en BID") + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
  ggsave(here("Diagnóstico", "03_Tasas de Aportación", "plots", "graf1.png"),
         dpi = 300, width = 12, height = 8)
