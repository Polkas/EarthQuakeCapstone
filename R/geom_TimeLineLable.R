
#' @title Additional geom for plotting Lables over Time Line of earthquake occurences.
#'
#' @description new geom build under grid textGrob and segmentsGrob
#'
#' @seealso \code{\link{eq_ReadQuake}} \code{\link{eq_ReadQuake}} \code{\link{eq_GenLocationName}} \code{\link{geom_TimeLineLable}}
#'
#' @inheritParams ggplot2::geom_bar
#'
#' @examples
#'
#'  df = eq_ReadQuake()
#'
#'  library(dplyr)
#'  library(ggplot2)
#'
#'  eq_CleanData(df) %>%
#'  filter_at(c("COUNTRY"),all_vars(.%in% c("TURKEY","ITALY"))) %>%
#'  filter(.data$DATE > "1970-01-01") %>%
#'  eq_GenLocationName() %>%
#'  ggplot(aes(x = DATE, y = COUNTRY, fill = DEATHS, size = MAGNITUDE)) +
#'  geom_TimeLine() +
#'  geom_TimeLineLable(aes(label=LOCATION),check.overlap=TRUE)
#'
#' @export

geom_TimeLineLable <- function(mapping = NULL, data = NULL,
                               stat = "identity", position = "identity",
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE,...){
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTLL,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      ...
    )
  )
}


GeomTLL <- ggplot2::ggproto("GeomTLL", ggplot2::Geom,
                                           required_aes = c("x", "label"),
                                           default_aes = ggplot2::aes(y = 0.5),
                                           draw_panel = function(data, panel_scales, coord,check.overlap=T) {

                                             # Transform the data
                                             coords <- coord$transform(data, panel_scales)

                                             Timeline_seg_grobs <- grid::segmentsGrob(x0 = coords$x,
                                                                                      y0 = coords$y,
                                                                                      x1 = coords$x,
                                                                                      y1 = coords$y + 0.05/length(unique(coords$y)),
                                                                                      #default.units = "npc",
                                                                                      gp = grid::gpar(),
                                                                                      vp = NULL)

                                             Earthquake_text_grobs <- grid::textGrob(label = coords$label,
                                                                                     x = coords$x,
                                                                                     y = coords$y + 0.05/length(unique(coords$y)),
                                                                                     #default.units = "npc",
                                                                                     rot = 40,
                                                                                     just = "left",
                                                                                     gp = grid::gpar(fontsize = 8),
                                                                                     check.overlap = check.overlap
                                                                                     )

                                             grid::gTree(children = grid::gList(Timeline_seg_grobs, Earthquake_text_grobs))
                                           }
)

