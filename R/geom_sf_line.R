geom_sf_line<-function(mapping = aes(), data = NULL,
         stat = "sf_coordinates", position = "identity",
         nudge_x = NULL,
         nudge_y = NULL,
         na.rm = FALSE,
         show.legend = NA,
         inherit.aes = TRUE,
         fun.geometry = NULL,
         ...) {
    if (!is.null(nudge_x) || !is.null(nudge_y)) {
        message("You specify position by `nudge_x` and `nudge_y`.")
        position <- position_nudge(nudge_x, nudge_y)
    }
    layer_sf(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomLine,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            fun.geometry = fun.geometry,
            ...
        )
    )
}
