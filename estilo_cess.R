library(ggplot2)

# install.packages("extrafont")
# library(extrafont)
# font_import()
# You may see some warnings, but you should be able to ignore them. After the fonts are imported, 
# you can view the available fonts by running fonts() or fonttable():
# fonttable()

amarillo_cess <- "#FBB040"
violeta_cess <- "#A183BC"
rosado_cess <- "#EC878D"
verde_cess <- "#7FC371"
gris_cess <- "#808285" 

estilo_cess <- function() {
  theme_minimal(base_family = "Gotham-Book",
                base_size = 16) +
    theme(axis.line = ggplot2::element_blank(), 
          panel.grid.minor = ggplot2::element_blank(), 
          panel.background = ggplot2::element_blank(), 
          plot.title = element_text(hjust = .5, 
                                    family = "Gotham-Bold"),
          plot.subtitle = element_text(hjust = .5),
          plot.caption = element_text(family = "Gotham-Light", hjust = 1, 
                                      margin = margin(t = 10)),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.line.x = element_line(),
          axis.ticks.x = element_line(),
          legend.position = "bottom")
}

coloriza_sexo <- function(vector) {
  levs <- if(is.factor(vector)) 
    levels(vector) else levels(factor(vector))
  
  predefinidos <- c("Mujeres", "Hombres")
  
  pal <- c("#A183BC", "#7FC371")
  
  pal <- pal[match(levs, predefinidos)]
  
  blanks <- which(is.na(pal))
  
  pal[blanks] <- sample(colours(100), length(blanks))
  
  pal
}

# Para settear el tema en los gráficos que prosiguen:

theme_set(estilo_cess())
update_geom_defaults("text", list(family = theme_get()$text$family))

facet_wrap_custom <- function(..., scale_overrides = NULL) {
  # take advantage of the sanitizing that happens in facet_wrap
  facet_super <- facet_wrap(...)
  
  # sanitize scale overrides
  if(inherits(harpVis::scale_overrides, "scale_override")) {
    scale_overrides <- list(scale_overrides)
  } else if(!is.list(scale_overrides) || 
            !all(vapply(scale_overrides, inherits, "scale_override", FUN.VALUE = logical(1)))) {
    stop("scale_overrides must be a scale_override object or a list of scale_override objects")
  }
  
  facet_super$params$scale_overrides <- scale_overrides
  
  ggproto(NULL, CustomFacetWrap,
          shrink = facet_super$shrink,
          params = facet_super$params
  )
}