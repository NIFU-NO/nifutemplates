options(knitr.kable.NA = "")
options(dplyr.summarise.inform = FALSE)
options(OutDec= ",") 

# Følgende pakker bør være unødvendige å lese inn, bør alltid bruke pakkenavn:: Unntaket er magrittr
for(x in c("dplyr", "stringr", "tibble", "forcats", "tidyr", "ggplot2",
			"labelled", "gt",  "ggiraph", "knitr", "saros")) {
	suppressPackageStartupMessages(library(x, warn.conflicts = FALSE, quietly = TRUE, character.only=TRUE))
}
font_color <- "#404040"
#extrafont::font_import(prompt = FALSE, pattern = "[Cc]alibri\\.ttf") #Time consuming, and probably requires more setup than this
#extrafont::loadfonts(quiet = TRUE)

fix_fontsize <- function(ggobj, 
                         main_font_size=10, 
                         data_label_font_size=3, 
                         legend_font_size=7,
                         strip_font_size = 7, 
                         strip_width = 15, 
                         x_label_width = 20, 
                         strip_angle = nifu_strip_angle) {
  library(magrittr)
  ggobj$theme$text$size <- main_font_size
  ggobj$layers[[2]]$aes_params$size <- data_label_font_size
  ggobj$theme$legend.text$size <- legend_font_size
  ggobj$theme$strip.clip <- "off"
  ggobj$theme$legend.justification <- "left"
  
  
  # Fikse ting i tidlig saros-versjon
  if(!is.null(ggobj$layers[[1]]$computed_mapping$tooltip) &&
     !is.null(attr(ggobj$layers[[1]]$computed_mapping$tooltip, ".Environment")$p$labels$colour)) {
    attr(ggobj$layers[[1]]$computed_mapping$tooltip, ".Environment")$p$labels$colour <- 
      ggobj$labels$colour <- 
      "hex_bw(.data$fill)"
  }
  
  if(!is.null(ggobj$layers[[1]]$computed_mapping$tooltip) &&
     !is.null(attr(ggobj$layers[[1]]$computed_mapping$tooltip, ".Environment")) &&
     !is.null(attr(ggobj$layers[[1]]$computed_mapping$tooltip, ".Environment")$p$layers[[2]])) {
    attr(ggobj$layers[[1]]$computed_mapping$tooltip, ".Environment")$p$layers[[2]]$computed_mapping$colour %<>% 
      rlang::quo_set_expr(expr = quote(ggplot2::after_scale(x = saros::hex_bw(.data$fill))))
    attr(ggobj$layers[[1]]$computed_mapping$tooltip, ".Environment")$p$layers[[2]]$mapping$colour %<>% 
      rlang::quo_set_expr(expr = quote(ggplot2::after_scale(x = saros::hex_bw(.data$fill))))
  }
  
  
  
  if(any(c("Annet", "Ingen") %in% levels(ggobj$data$.variable_label))) {
  ggobj$data <- 
    ggobj$data |>
    dplyr::mutate(.variable_label =
                    forcats::fct_relevel(.variable_label, 
                                         "Annet", "Ingen", after = length(levels(.variable_label))))
  }
  ggobj <- 
    ggobj + 
    ggplot2::scale_x_discrete(label = ggplot2::label_wrap_gen(width = x_label_width),
                              limits = if(any(levels(ggobj$data$.category) == "Valgt") && 
                                          all(stringr::str_detect(colnames(ggobj$data), "^\\."))) rev else NULL) +
    ggplot2::theme(legend.key.size = ggplot2::unit(4, "mm")) +
    ggplot2::labs(fill=NULL, x=NULL, y=NULL, title=NULL, caption=NULL, subtitle=NULL, colour=NULL, alpha=NULL, shape=NULL)
  
  if(!all(stringr::str_detect(colnames(ggobj$data), "^\\.")) && #bivariate
     dplyr::n_distinct(ggobj$data$.variable_label) > 1 # with multiple deps
     ) {
    ggobj$theme$strip.text.y.left$size <- strip_font_size
    ggobj$theme$strip.text.y.left$angle <- strip_angle
    ggobj$theme$strip.text.y.left$hjust <- 0
    ggobj$theme$strip.text.y.left$colour <- "grey20"
    
    
    ggobj <- ggobj + 
    ggiraph::facet_grid_interactive(rows = ggplot2::vars(.variable_label), 
                                    switch = "both",
                                    labeller = ggplot2::label_wrap_gen(width = strip_width))
  }
  
  if(all(stringr::str_detect(colnames(ggobj$data), "^\\.")) && # univariate
     dplyr::n_distinct(ggobj$data$.variable_label) == 1) {
    ggobj$theme$axis.text.y <- ggplot2::element_blank()
  }
  if(!all(stringr::str_detect(colnames(ggobj$data), "^\\.")) && # bivariate
     dplyr::n_distinct(ggobj$data$.variable_label) == 1) {
    ggobj$theme$strip.text.y.left <- ggplot2::element_blank()
  }
  
  ggobj
}
fargepalett <- c(
'#C84957',
'#363636',
'#EDE2D2',
'#2D8E9F',
'#DBD2E0',
'#E8AE59',
'#E8B0B7',
'#90D4E0',
'#7E2630',
'#1B555F')
ggiraph::set_girafe_defaults(
  opts_hover = ggiraph::opts_hover(css = ggiraph::girafe_css(
    css = glue::glue("fill:{font_color};"), #"stroke:white;fill:#2D8E9F;"
    text = glue::glue("stroke:none;fill:{saros::hex_bw(font_color)};fill-opacity:1;"))),
  opts_hover_inv = ggiraph::opts_hover_inv(css = "opacity:0.4"))
# print.ggplot <- function(x, ...) {
#   ggiraph::girafe(ggobj = x)
# }
# registerS3method("print", "ggplot", print.ggplot)

knit_print.data.frame <- function(x, ...) {
  x[[1]] <- as.character(x[[1]])
  x <- 
    x |>
      gt::gt(locale = "nb") |>
      gt::tab_options(table.font.size = 10.5) |>
      gt::opt_table_font(font = "Calibri") |>
      # gt::opt_interactive(use_pagination = FALSE, use_pagination_info = FALSE, use_sorting = FALSE, use_highlight = TRUE) |>
      gt::sub_missing(missing_text = "") |>
      gt::opt_align_table_header(align = "left") |>
      gt::tab_style(style = gt::cell_text(weight = "bold"),
      locations = gt::cells_title("title")) |>
      gt::cols_align(align = "left", columns = 1) |>
      gt::as_raw_html()
  stringr::str_c(
    "<div style='all:initial';>\n",
    x,
    "\n</div>") |>
    knitr::asis_output()
}
registerS3method(
  "knit_print", 'data.frame', knit_print.data.frame#, envir = asNamespace("gt") 
)
