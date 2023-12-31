options(knitr.kable.NA = "")
options(dplyr.summarise.inform = FALSE)
options(OutDec= ",") 

# Følgende pakker bør være unødvendige å lese inn, bør alltid bruke pakkenavn:: Unntaket er magrittr
library(dplyr, warn.conflicts = FALSE)
library(saros)
library(ggplot2)
library(ggiraph)
library(knitr)
font_color <- "#404040"
extrafont::font_import(prompt = FALSE, pattern = "[Cc]alibri\\.ttf")
extrafont::loadfonts(quiet = TRUE)
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
    gt::gt() |>
    gt::sub_missing(missing_text = "") |>
    gt::opt_align_table_header(align = "left") |>
    gt::tab_style(style = gt::cell_text(weight = "bold"), 
                  locations = gt::cells_title("title")) |>
    # gt::tab_spanner_delim(delim = "_") |>
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