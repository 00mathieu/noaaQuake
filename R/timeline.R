#-------------------------------------------------------------------------------
#' Build layer of geom
#'
#' Function to build layer of ggproto
#'
#' @inheritParams ggplot2::geom_point
#' @param stat defauilt is timeline
#'
#' @import ggplot2
#' @importFrom ggplot2 layer
#'
#' @return a geom
#' @export
#'
#' @examples \dontrun{
#' db <- clean_raw_data()
# ggplot(db[COUNTRY %in%c("CHINA","GREECE")],
#        aes (x = as.Date(DATE), y = COUNTRY, color = as.numeric(DEATHS)/1000, size = as.numeric(EQ_PRIMARY))) +
#     geom_timeline(aes(xmin = as.Date("2000-01-01"), xmax = as.Date("2018-01-01"))) +
#     geom_timeline_label(n_max = 15, aes(label = LOCATION_NAME, xmin = as.Date("2000-01-01"), xmax = as.Date("2018-01-01")))+
#     xlab("Date") + ylab("") + ggtitle("Timeline of Earthquakes") +
#     labs(size = "Richter scale", color = "# deaths (k)" )+
#     theme_bw() +
#     theme(legend.position="bottom")
#'
#' }
geom_timeline <- function(mapping = NULL,
                          data = NULL,
                          stat = "timeline",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
    ggplot2::layer(
        geom = geom_timeline_proto,
        mapping = mapping,
        data = data,
        stat = stat,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )

}
geom_timeline_proto <- ggplot2::ggproto(
    "GeomTimeline",
    ggplot2::Geom,
    required_aes = c("x"),
    default_aes = ggplot2::aes(colour = "blue", size = 3, alpha = 0.4, shape = 20, fill = "grey", stroke = 0.5),
    draw_key = ggplot2::draw_key_point,
    draw_panel = function(data, panel_scales, coord) {

        if (!("y" %in% colnames(data))) {data$y <- 0}

        coords <- coord$transform(data, panel_scales)

        points <- grid::pointsGrob(
            x = coords$x,
            y = coords$y,
            pch = coords$shape,
            size = grid::unit(coords$size * 0.40, "char"),
            gp = grid::gpar(
                col = scales::alpha(coords$colour, coords$alpha),
                fill = scales::alpha(coords$colour, coords$alpha)
            )
        )
        levels <- unique(coords$y)

        lines <- grid::polylineGrob(
            x = grid::unit(rep(c(0, 1), each = length(levels)), "npc"),
            y = grid::unit(c(levels, levels), "npc"),
            id = rep(c(1:length(levels)), 2),
            gp = grid::gpar(col = "blue")
        )

        grid::gList(lines, points)
    }
)
#-------------------------------------------------------------------------------
#' stat for timeline
#'
#' building a stat so that we can subset the data according to xmin and xmax
#'
#' @inheritParams ggplot2::geom_point
#' @param geom default is timeline
#' @param n_max max number of annotations to make
#'
#' @return a stat
#' @export
#'
#' @examples \dontrun{
#' db <- clean_raw_data()
# ggplot(db[COUNTRY %in%c("CHINA","GREECE")],
#        aes (x = as.Date(DATE), y = COUNTRY, color = as.numeric(DEATHS)/1000, size = as.numeric(EQ_PRIMARY))) +
#     geom_timeline(aes(xmin = as.Date("2000-01-01"), xmax = as.Date("2018-01-01"))) +
#     geom_timeline_label(n_max = 15, aes(label = LOCATION_NAME, xmin = as.Date("2000-01-01"), xmax = as.Date("2018-01-01")))+
#     xlab("Date") + ylab("") + ggtitle("Timeline of Earthquakes") +
#     labs(size = "Richter scale", color = "# deaths (k)" )+
#     theme_bw() +
#     theme(legend.position="bottom")
#'
#' }
stat_timeline <- function(
    mapping = NULL,
    data = NULL,
    geom = "timeline",
    position = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    n_max = NULL,
    ...
    ) {
    ggplot2::layer(
        stat = StatTimeline,
        data = data,
        mapping = mapping,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, n_max = n_max, ...)
    )
}
StatTimeline <- ggplot2::ggproto(
    "StatTimeline",
    ggplot2::Stat,
    compute_group = function(data, scales){
        data <- data[data$x>data$xmin & data$x<data$xmax,]
        },
    default_aes = ggplot2::aes(xmin = min(data$x),
                               xmax = max(data$x)),
    required_aes = c("x")
)
#-------------------------------------------------------------------------------

#' geom to add lines and annotations to timeline plot
#'
#' @inheritParams ggplot2::geom_point
#' @param n_max max number of annotations to make
#' @param stat timeline
#'
#' @return a geom
#' @export
#'
#' @importFrom ggplot2 layer
#'
#' @examples \dontrun{
#' db <- clean_raw_data()
# ggplot(db[COUNTRY %in%c("CHINA","GREECE")],
#        aes (x = as.Date(DATE), y = COUNTRY, color = as.numeric(DEATHS)/1000, size = as.numeric(EQ_PRIMARY))) +
#     geom_timeline(aes(xmin = as.Date("2000-01-01"), xmax = as.Date("2018-01-01"))) +
#     geom_timeline_label(n_max = 15, aes(label = LOCATION_NAME, xmin = as.Date("2000-01-01"), xmax = as.Date("2018-01-01")))+
#     xlab("Date") + ylab("") + ggtitle("Timeline of Earthquakes") +
#     labs(size = "Richter scale", color = "# deaths (k)" )+
#     theme_bw() +
#     theme(legend.position="bottom")
#'
#' }
geom_timeline_label <- function(
    mapping = NULL,
    data = NULL,
    n_max = NULL,
    stat = "timeline",
    position = "identity",
    na.rm = FALSE, show.legend = NA,
    inherit.aes = TRUE,
    ...) {

    ggplot2::layer(
        geom = GeomTimelineLabel, mapping = mapping,
        data = data, stat = stat, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, n_max = n_max, ...) ## add n_max
    )
}
GeomTimelineLabel <- ggplot2::ggproto(
    "GeomTimelineLabel",
    ggplot2::Geom,
    required_aes = c("x","label"),
    default_aes = ggplot2::aes(xmin = min(data$x),xmax = max(data$x)),
    draw_key = ggplot2::draw_key_blank,
    setup_data = function(data, params){
        if (!("y" %in% colnames(data))) {data$y <- 0}
        data <- data %>%
            dplyr::group_by(y) %>%
            dplyr::arrange(desc(size)) %>%
            dplyr::slice(1:params$n_max) %>%
            dplyr::ungroup()
        data
        },
    draw_panel = function(data, panel_scales, coord, n_max) {
        coords <- coord$transform(data, panel_scales)
        gap <- rep(0.1, length(levels))
        ver_lines <- grid::polylineGrob(
        x = c(coords$x, coords$x),
        y = grid::unit(c(coords$y, coords$y+gap), "npc"),
        id = rep(c(1:length(coords$x)), 2),
        gp = grid::gpar(col = "blue")
        )
     labels <- grid::textGrob(label = coords$label,
                              x = unit(coords$x, "npc"),
                              y = unit(coords$y + gap, "npc"),
                              rot = 30,
                              just = c("left","top"),
                              check.overlap = TRUE,
                              gp = grid::gpar(fontsize = 12))

     grid::gList(ver_lines, labels)
                     }

    )
