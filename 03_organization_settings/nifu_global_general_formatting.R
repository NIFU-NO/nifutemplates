
#### KUN STEPHAN DAUS ELLER HENRIK KARLSTRØM BØR REDIGERE DENNE FILEN. ####

#### LASTE INN VANLIGE PAKKER OG SETTE NOEN STANDARDINNSTILLINGER ####

# Følgende pakker bør være unødvendige å lese inn, bør alltid bruke pakkenavn:: Unntaket er magrittr
for(x in c("dplyr", "stringr", "tibble", "forcats", "tidyr", "ggplot2",
			"labelled", "gt",  "ggiraph", "knitr", "saros")) {
	suppressPackageStartupMessages(library(x, warn.conflicts = FALSE, quietly = TRUE, character.only=TRUE))
}
options(dplyr.summarise.inform = FALSE)
qs::set_trust_promises(TRUE) # For å kunne lese inn eldre qs-filer etter sikkerhetspatch

#### FORMATERING AV TALL I FIGURER OG TABELLER ####
options(OutDec= ",")


#### FORMATERING AV TEKST I (ggplot2)-FIGURER ####
nifu_strip_angle <- 0
global_font_size <- 7
ggplot2::theme_set(
  ggplot2::theme_classic(base_size = global_font_size) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "sans", size = global_font_size),
      axis.text.x = ggiraph::element_text_interactive(size = global_font_size),
      axis.text.y = ggiraph::element_text_interactive(data_id = "axis.text.y", size = global_font_size),
      legend.location = "plot",
      legend.position = "bottom",
      legend.justification.bottom = "left",
      legend.direction = "horizontal",
      legend.key.size = ggplot2::unit(4, "mm"),
      legend.text = ggiraph::element_text_interactive(data_id = "legend.text", size = global_font_size),
      title = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.clip = "off",
      strip.text = ggplot2::element_text(size = global_font_size),
      strip.text.x = ggplot2::element_text(margin = ggplot2::margin(l = 0, t = 0, r = 0, b = 2, "cm"),
                                           size = global_font_size),
      strip.text.y.left = ggiraph::element_text_interactive(data_id = "strip.text",
                                                            angle = 0,
                                                            hjust = 0,
                                                            colour = "grey20",
                                                            size = global_font_size),
      strip.text.y.right = ggiraph::element_text_interactive(data_id = "strip.text",
                                                             angle = 0,
                                                             hjust = 0,
                                                             colour = "grey20",
                                                             size = global_font_size), # if(length(indep_vars)>0) ggplot2::element_blank() else
      strip.background = ggiraph::element_rect_interactive(colour = NA)
    )
  )
ggplot2::update_geom_defaults(geom = ggiraph::GeomInteractiveText,
                              new = ggplot2::aes(size = ggplot2::rel(3)))

  # ggplot2::labs(fill = NULL, colour = NULL, title = NULL, x = NULL, y = NULL, subtitle = NULL,
  #               caption = NULL, tag = NULL)
options(ggplot2.discrete.colour = c("black", "white"))


#### FORMATERING AV FARGER I (ggplot2)-FIGURER ####
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
font_color <- "#404040"


# Custom discrete fill scale that takes matching vectors from a list, or uses the last vector in the list if no matches are found. ####
# This is useful for creating a scale that can be used with multiple palettes, where the correct palette is chosen based on the data.


palette_list <- list(
  c("Ikke valgt" = "white",
    "Valgt" = "#C84957"),
  c("Vet ikke" = "gray80",
    "Nei" = "#C84957",
    "Ja" = "#404040"),
  c("Nei" = "#C84957",
    "Ja" = "#404040"),
  c("Ikke i det hele tatt" = "#C84957",
    "I liten grad" = "#404040",
    "I noen grad" = "#EDE2D2",
    "I stor grad" = "#2D8E9F",
    "I svært stor grad" = "#E8AE59"),
  c("Svært uenig" = "#7E2630",
    "Uenig" = "#BC3848",
    "Litt uenig" = "#D5727D",
    "Litt enig" = "#70C7D7",
    "Enig" = "#2D8E9F",
    "Svært enig" = "#1B555F"),
  c("Svært uenig" = "#7E2630",
    "Uenig" = "#BC3848",
    "Litt uenig" = "#D5727D",
    "Verken enig eller uenig" = "#E8AE59",
    "Litt enig" = "#70C7D7",
    "Enig" = "#2D8E9F",
    "Svært enig" = "#1B555F"),
  fargepalett
)


custom_palette <- function(palette_codes, fct_levels) {
  function(n, lvls = fct_levels) {
    matched_palette <- purrr::detect(palette_codes, function(palette) {
      all(lvls %in% names(palette))
    })
    if (is.null(matched_palette)) {
      matched_palette <- palette_codes[[length(palette_codes)]]
    }

    return(matched_palette)
  }
}

guess_legend_ncols <- function(ggobj, char_limit = 100) {
  fill_var <- rlang::as_label(ggobj$mapping$fill)
  if(!is.null(fill_var))  {

    lvls <- as.character(unique(ggobj$data[[fill_var]]))
    # print(lvls)
    max_chars <- max(nchar(lvls), na.rm = TRUE)
    for(i in 2:15) {
      if((max_chars+5)*i >= char_limit) {
        return(i-1)
      }
    }
  }
  return(NULL)
}

scale_discrete_special <- function(aesthetics = "fill", palette_codes, lvls = NULL, ncol = NULL, ...) {
  if(is.null(lvls)) {
    ggiraph::scale_discrete_manual_interactive(
      aesthetics = aesthetics,
      name = "",
      values = palette_codes[[length(palette_codes)]],
      guide = ggiraph::guide_legend_interactive(title = "",
                                                ncol = ncol),
      ...)
  } else {
    ggplot2::discrete_scale(
      aesthetics = aesthetics,
      name = "",
      palette = custom_palette(palette_codes, fct_levels = lvls),
      guide = ggiraph::guide_legend_interactive(title = "",
                                                ncol = ncol),
      ...
    )
  }
}

convert_to_checkbox_plot <- function(ggobj, checked = "Valgt", not_checked = "Ikke valgt",
                                     checked_colour = fargepalett[1]) {
  ggobj$data <-
    ggobj$data %>%
    dplyr::mutate(.data_label = ifelse(.category == not_checked, "", .data_label))
  ggobj
}

girafe <- function(ggobj, ..., char_limit = 100) {
  fill_var <- rlang::as_label(ggobj$mapping$fill)
  checkbox <- FALSE
  if(!is.null(fill_var) && fill_var != "NULL")  {

    fill_levels <-
      if(is.factor(ggobj$data[[fill_var]])) {
        levels(ggobj$data[[fill_var]])
        } else unique(ggobj$data[[fill_var]])


    if(all(fill_levels %in% c("Valgt", "Ikke valgt"))) {
      checkbox <- TRUE
      ggobj <- convert_to_checkbox_plot(ggobj, checked = "Valgt", not_checked = "Ikke valgt")
    }


  # lvls <- levels(ggobj$data[[fill_var]])
  ggobj <-
    suppressMessages(
      ggobj +
        scale_discrete_special(palette_codes = palette_list, lvls = fill_levels) +
        ggplot2::guides(fill = if(isTRUE(checkbox)) "none" else guide_legend(ncol =
                                              guess_legend_ncols(ggobj = ggobj,
                                                                 char_limit = char_limit))),
      classes = "rlang_message")

  ggiraph::girafe(ggobj = ggobj, ...)
  } else ggobj
}

#### FORMATERING AV INTERAKTIVITET I (ggiraph)-FIGURER ####

ggiraph::set_girafe_defaults(
  opts_hover = ggiraph::opts_hover(css = ggiraph::girafe_css(
    css = glue::glue("fill:{font_color};"), #"stroke:white;fill:#2D8E9F;"
    text = glue::glue("stroke:none;fill:{saros::hex_bw(font_color)};fill-opacity:1;"))),
  opts_hover_inv = ggiraph::opts_hover_inv(css = "opacity:0.3"),
  opts_toolbar = ggiraph::opts_toolbar(position = "bottom"))#, fixed = TRUE))


#### TABELLER (FORELØPIG IKKE TYPST/PDF) ####
options(knitr.kable.NA = "")
gt_nifu_style <- function(x) {
  if(!gt:::is_gt_tbl(x)) x <- gt::gt(x, locale = "nb")
  gt::tab_options(data = x,
                  table.font.size = 10.5,
                  table.font.names = "Calibri",
                  # table.align = "center",
                  table.width = gt::px(550),
                  table.margin.left = gt::px(55),
                  table.margin.right = gt::px(0),
                  heading.align = "left",

                  # table.layout = "auto",
                  quarto.use_bootstrap = TRUE) |>
    # gt::opt_interactive(use_pagination = FALSE, use_pagination_info = FALSE, use_sorting = FALSE, use_highlight = TRUE) |>
    gt::sub_missing(missing_text = "") |>
    # gt::opt_align_table_header(align = "left") |>
    gt::tab_style(style = gt::cell_text(weight = "bold"),
                  locations = list(gt::cells_title(),
                                   gt::cells_column_labels(),
                                   gt::cells_column_spanners())) |>
    gt::cols_align(align = "left",
                   columns = 1)
}

saros.contents::makeme_global_settings_set(
  new = list(crowd = 'all',
             mesos_var = NULL,
             mesos_group = if(exists("params")) params$mesos_group,
             hide_for_crowd_if_valid_n_below = 8,
             hide_for_crowd_if_category_n_below = 0,
             hide_for_crowd_if_cell_n_below = 0,
             hide_for_all_crowds_if_hidden_for_crowd = "target",
             categories_treated_as_na = c('Benyttes ikke',
                                          'Vet ikke',
                                          'Usikker',
                                          'Har ikke brukt støtteressursen',
                                          'Ikke hatt',
                                          'Annet',
                                          'Ikke relevant'),
             variables_always_at_bottom = c('Annet',
                                            'Ingen',
                                            'Annet, spesifiser',
                                            'Ingen av disse'),
             showNA = "never",
             data_label_decimal_symbol = ",",
             serialized_format = 'qs',
             colour_na = '#A6A6A6',
             colour_2nd_binary_cat = '#ffffff',
             translations = list(
               last_sep = ' og ',
               by_breakdown = ' etter ',
               table_heading_N = "Totalt (N)",
               by_total = "Alle",
               sigtest_variable_header_1 = "Var 1",
               sigtest_variable_header_2 = "Var 2",
               mesos_group_prefix = " For gruppe = ",
               mesos_group_suffix = "",
               mesos_label_all_others = 'Alle andre'
             ))
)

saros.contents::make_link_global_settings_set(
  new = list(
    file_suffix = ".xlsx",
    save_fn = writexl::write_xlsx,
    link_prefix = "[Last ned figurdata](",
    width = 16, height = 16, units = "cm",
    scale = 1.2,  dpi = "retina",
    create.dir = TRUE,
))


#### SKAL KUNNE DROPPES ETTER HVERT; NØDLØSNING ####

fix_fontsize <- function(ggobj,
#                         main_font_size = 6,
#                         axis_font_size = ggplot2::rel(1),
#                         data_label_font_size = ggplot2::rel(2),
#                         legend_font_size = ggplot2::rel(1),
#                         strip_font_size = ggplot2::rel(1),
                         strip_width = 15,
                         x_label_width = 20,
                         strip_angle = nifu_strip_angle,
                         adjust_scale_x_limit_right = NA,
                         variables_always_at_bottom = c("Annet", "Ingen")) {
  library(magrittr)

  # Set font sizes
  # ggobj$theme$text$size <- main_font_size
  # ggobj$theme$axis.text$size <- axis_font_size
  # ggobj$theme$axis.text.y$size <- axis_font_size
  # ggobj$theme$axis.text.x$size <- axis_font_size
  ggobj$layers[[2]]$aes_params$size <- data_label_font_size
  # ggobj$theme$legend.text$size <- legend_font_size

  # Remove clipping on strips
  # ggobj$theme$strip.clip <- "off"
  # Justify legends to the left
  # ggobj$theme$legend.justification <- "left"


  # Fikse hex_bw i tidlig saros-versjon som ikke er nødvendig lenger
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


  # Sett Annet og Ingen nederst
  if(any(variables_always_at_bottom %in% levels(ggobj$data$.variable_label))) {
  ggobj$data <-
    ggobj$data |>
    dplyr::mutate(.variable_label =
                    forcats::fct_relevel(.variable_label,
                                         variables_always_at_bottom, after = length(levels(.variable_label))))
  }
  ggobj <-
    ggobj +
    # Endre bredde på x_label
    ggplot2::scale_x_discrete(label = ggplot2::label_wrap_gen(width = x_label_width),
                              limits = if(any(levels(ggobj$data$.category) == "Valgt") &&
                                          all(stringr::str_detect(colnames(ggobj$data), "^\\."))) rev else NULL) +
    # ggplot2::theme(legend.key.size = ggplot2::unit(4, "mm")) +
    # Fjern alle titler
    ggplot2::labs(fill=NULL, x=NULL, y=NULL, title=NULL, caption=NULL, subtitle=NULL, colour=NULL, alpha=NULL, shape=NULL)

  if(!all(stringr::str_detect(colnames(ggobj$data), "^\\.")) && #bivariate
     dplyr::n_distinct(ggobj$data$.variable_label) > 1 # with multiple deps
     ) {


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

  # Adjust scale_x_continuous right side
  if(!is.na(adjust_scale_x_limit_right)) {
  ggobj <-
    ggobj +
    ggplot2::scale_y_continuous(
      limits = c(-.003, adjust_scale_x_limit_right),
      expand = c(0, 0),
      labels = scales::percent
    )
  }

  ggobj
}



