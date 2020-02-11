#' Plot Bubble Map
#'
#' @param data data
#' @param shapename shape name variable
#' @param choice default is NULL
#' @param color.variable color variable
#' @param size.variable size variable
#' @param lgd.color.title colored legend title
#' @param lgd.size.title sized legend title
#' @param color color, which be more than one, default is grey and red
#' @param size size for all bubble, default is 6
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
#' @param basemap logical, default is TRUE
#' @return a ggplot bubble map
#' @export
#'
#' @examples
#' plot(1:10)
bubble <- function(data,shapename,
                         choice=NULL,
                         color.variable=NULL,
                         size.variable=NULL,
                         lgd.color.title=NULL,
                         lgd.size.title=NULL,
                         color=c('grey','red'),
                         size=6,
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
                         text.color='black',
                         basemap=TRUE){
    res=checkname(data = data,shapename = shapename,choice = choice)
    map.ready0=res$map.ready0
    data=res$data
    map.ready=NULL
    if (!is.null(color.variable) |
        !is.null(size.variable) ){
        #data prepare
        k.color=is.null(color.variable)
        k.size=is.null(size.variable)
        variable=shapename
        if (!k.color) variable=c(variable,color.variable)
        if (!k.size) variable=c(variable,size.variable)
        variable=unique(variable)
        dd.value=data[,variable]
        map.ready=merge(x = map.ready0,y = dd.value,by.x='Name',by.y=shapename)
        map.df=data.frame(map.ready,check.names = FALSE)
        map.ready
        map.df
        #title
        if (is.null(lgd.color.title)) lgd.color.title=color.variable
        if (is.null(lgd.size.title)) lgd.size.title=size.variable
        #plot
        #base map
        if (basemap){
            p_bubble<-ggplot(data = map.ready) +
                geom_sf(fill=fill,color=border.color,size=border.size) +
                theme_half_open()
        }else{
            p_bubble<-ggplot(data = map.ready) +
                geom_sf(color=NA,size=NA,fill=NA) +
                theme_half_open()
        }
        #1:color
        if(!k.color & k.size){
            display=map.df[,color.variable]
            p_bubble<-p_bubble+geom_sf_point(aes(color=display),
                                             size=size,
                                             fill=NA,
                                             shape = 20)
            if (is.numeric(display)){
                p_bubble<-p_bubble+scale_color_gradientn(name=lgd.color.title,colours = color)
            }else{
                p_bubble<-p_bubble+scale_color_manual(name=lgd.color.title,
                                                      values = colorRampPalette(color)(length(unique(display))))
            }
        }
        #1:size
        if(k.color & !k.size){
            display=map.df[,size.variable]
            p_bubble<-p_bubble+geom_sf_point(aes(size=display),
                                             color=color[1],
                                             fill=NA,
                                             shape = 20)
            if (is.numeric(display)){
                p_bubble<-p_bubble+ scale_size(name=lgd.size.title)
            }else{
                p_bubble<-p_bubble+ scale_size_discrete(name=lgd.size.title)
            }
        }
        #2：color and size
        if(!k.color & !k.size){
            display1=map.df[,color.variable]
            display2=map.df[,size.variable]
            p_bubble<-p_bubble+geom_sf_point(aes(color=display1,
                                                 size=display2),
                                             fill=NA,
                                             shape=20)
            if (is.numeric(display1)){
                p_bubble<-p_bubble+ scale_color_gradientn(name=lgd.color.title,colours = color)
            }else{
                p_bubble<-p_bubble+ scale_color_manual(name=lgd.color.title,
                                                       values = colorRampPalette(color)(length(unique(display1))))
            }
            if (is.numeric(display2)){
                p_bubble<-p_bubble+ scale_size(name=lgd.size.title)
            }else{
                p_bubble<-p_bubble+ scale_size_discrete(name=lgd.size.title)
            }
        }
        p_bubble
    }else{
        if (basemap){
            p_bubble<-ggplot(data = map.ready0) +
                geom_sf(fill=fill,color=border.color,size=border.size) +
                geom_sf_point()+
                theme_half_open()
            p_bubble
        }else{
            p_bubble<-ggplot(data = map.ready0) +
                geom_sf(color=NA,size=NA,fill=NA) +
                geom_sf_point()+
                theme_half_open()
            p_bubble
        }
    }
    if (text.show){
        if (is.null(map.ready)) text.data=map.ready0
        if (!is.null(map.ready)) text.data=map.ready
        p_bubble<-p_bubble+geom_text_repel(
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
    p_bubble<-p_bubble+
        theme(axis.title = element_blank())
    return(p_bubble)
}

