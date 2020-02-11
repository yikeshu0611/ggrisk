#' Plot Heat Map
#'
#' @param data data
#' @param shapename shape name variable
#' @param choice default is NULL
#' @param heat.variable heat variable
#' @param color color, which be more than one, default is grey and red
#' @param lgd.title legend title
#' @param fill fill color for base map
#' @param border.color map border color
#' @param border.size map border size
#' @param text.show logical, whether to show text on map, TRUE is default
#' @param min.segment.length \code{\link[ggrepel]{geom_text_repel}}. See the documentation for those
#' @param force \code{\link[ggrepel]{geom_text_repel}}. See the documentation for those
#' @param family \code{\link[gdtools]{font_family_exists}}. See the documentation for those
#' @param fontface face, plain, bold, italic
#' @param text.alpha alpha, default is 1
#' @param text.size size for text, default is 5
#' @param text.color text color, default is black
#'
#' @return a ggplot2 heat map
#' @export
#'
#' @examples
#' plot(1:10)
heat <- function(data,
                       shapename,choice=NULL,
                       heat.variable=NULL,
                       color=c('gray','red'),
                       lgd.title=NULL,
                       fill='#e5e5e5',
                       border.color='black',
                       border.size=0.5,
                       text.show=TRUE,
                       min.segment.length = 0.5,
                       force = 0.5,
                       family='sans',
                       fontface='plain',
                       text.alpha=1,
                       text.size=5,
                       text.color='black'){
    res=checkname(data = data,shapename = shapename,choice = choice)
    map.ready0=res$map.ready0
    data=res$data
    map.ready=NULL
    if (!is.null(heat.variable)){
        #data prepare
        if (any(duplicated(data[,shapename]))) stop(tmcn::toUTF8('\u5730\u540D\u6709\u91CD\u590D'))
        dd.value=data[,c(shapename,heat.variable)]
        map.ready=merge(x = map.ready0,y = dd.value,by.x='Name',by.y=shapename)
        map.df=data.frame(map.ready,check.names = FALSE)
        map.ready
        #title
        if (is.null(lgd.title)) lgd.title=heat.variable
        #plot with value
        p_heat<-ggplot(data = map.ready,
                       aes_string(fill=heat.variable)) +
            geom_sf(color=border.color,size=border.size)+
            theme_half_open()
        if (is.factor(map.df[,heat.variable])){
            p_heat<- p_heat+scale_fill_manual(name=lgd.title,
                                              values = colorRampPalette(color)(length(unique(map.df[,heat.variable]))))
        }else{
            p_heat<- p_heat+scale_fill_gradientn(name=lgd.title,
                                                 colours=color)
        }
    }else{
        p_heat<-ggplot(data = map.ready0) +
            geom_sf(fill=fill,color=border.color,size=border.size)+
            theme_half_open()
    }
    if (text.show){
        if (is.null(map.ready)) text.data=map.ready0
        if (!is.null(map.ready)) text.data=map.ready
        p_heat<-p_heat+geom_text_repel(
            data = text.data,
            aes_string(label = 'Name', geometry = 'geometry'),
            stat = "sf_coordinates",
            min.segment.length = min.segment.length,
            force = force,
            family=family,
            fontface=fontface,
            alpha=text.alpha,
            size=text.size,
            color=text.color
        )
    }
    p_heat<-p_heat+
        theme(axis.title = element_blank())
    return(p_heat)
}

