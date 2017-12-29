
#' @title Additional geom for plotting Time Line of earthquake occurences.
#'
#' @seealso \code{\link{eq_ReadQuake}} \code{\link{eq_ReadQuake}} \code{\link{geom_TimeLineLable}}
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
#'  ggplot(aes(x = DATE, y = COUNTRY, fill = DEATHS, size = MAGNITUDE)) +
#'  geom_TimeLine()
#'
#' @export

geom_TimeLine <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTL, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(...)
  )
}


GeomTL <- ggplot2::ggproto("GeomTL", ggplot2::Geom,
                           required_aes = c("x", "y"),
                           draw_key = ggplot2::draw_key_point,
                           default_aes = ggplot2::aes(shape = 21,
                                                      fill=NA,
                                                      size=5,
                                                      alpha=0.2,
                                                      colour="black",
                                                      stroke=1),
                           draw_key = ggplot2::draw_key_point,
                           draw_panel= function(data, panel_scales, coord) {
                             ## Transform the data - grid works on 0,1 scale

                             coords <- coord$transform(data, panel_scales)

                             ## Building a plot

                             points = grid::pointsGrob(
                               x = coords$x,
                               y = coords$y,
                               pch = coords$shape,
                               gp = grid::gpar(
                                 col = scales::alpha(coords$colour, coords$alpha),
                                 fill = scales::alpha(coords$fill, coords$alpha),
                                 fontsize = coords$size * .pt )
                             )
                             u = unique(coords$y)

                             lines = list()

                             for(i in seq_along(u)) lines[[i]] = grid::linesGrob(x=c(0,1),y=c(u[i],u[i]))

                             grid::gTree(children=do.call(grid::gList,c(lines,list(points))))

                           }
)
