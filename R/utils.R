.initial <- function() {
    pos <- 1
    envir <- as.environment(pos)
    assign(".mapitEnv", new.env(), envir = envir)
    assign(".mapitCache", new.env(), envir = envir)
    .mapitEnv <- get(".mapitEnv", envir=.GlobalEnv)

    tryCatch(utils::data(list="mapdata",
                         package="mapit"))
    mapdata <- get("mapdata")
    assign("mapdata", mapdata, envir = .mapitEnv)
    rm(mapdata, envir = .GlobalEnv)
}
