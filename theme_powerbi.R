library(extrafont)

theme_powerbi <- function(){
  powerbi_theme <- theme(text = element_text(family = "Segoe UI"),
                         plot.title = element_text(size = 16, family = "Segoe UI", colour = "#2A82CB"),
                         axis.text = element_text(size=11, family="Segoe UI", colour="#5F6B6D"),
                         axis.ticks = element_blank(),
                         axis.title = element_text(size=11, family="Segoe UI", colour="#5F6B6D"),
                         legend.text = element_text(size=11, family="Segoe UI"),
                         panel.grid.major.y = element_line( size=.1, color= "lightgrey"),
                         panel.grid.major.x = element_blank(),
                         panel.background = element_blank()
  )
  return(powerbi_theme)
}

powerbi_palette <- function(palette = "main", reverse = FALSE, ...){
  
  palette_options <- function(...){
    PowerBIPalette <- c("bluegreen" = "#01B8AA",
                        "darkgrey" = "#374649",
                        "redorange" = "#FD625E",
                        "yellow" = "#F2C80F",
                        "grey" = "#5F6B6D",
                        "skyblue" = "#8AD4EB",
                        "orange" = "#FE9666",
                        "purple" = "#A66999",
                        "blue" = "#3599B8",
                        "pink" = "#DFBFBF")
    
    cols <- c(...)
    if(is.null(cols))
      return (PowerBIPalette)
    
    PowerBIPalette[cols]
  }
  
  palettes <- list(
    `main` = palette_options(c("bluegreen",
                               "darkgrey",
                               "redorange",
                               "yellow",
                               "grey",
                               "skyblue",
                               "orange",
                               "purple",
                               "blue",
                               "pink")
    )
  )
  
  pal <- palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


scale_fill_powerbi <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- powerbi_palette(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("powerbi_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}




scale_color_powerbi <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- powerbi_palette(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("color", paste0("powerbi_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}
