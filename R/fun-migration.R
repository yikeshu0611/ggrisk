#' Plot Migration Map
#'
#' @param data data
#' @param start start position
#' @param destination destination
#' @param choice default is NULL
#' @param color.variable color variable
#' @param size.variable size variable
#' @param type.variable type variable
#' @param lgd.color.title colored legend title
#' @param lgd.size.title sized legend title
#' @param lgd.type.title typed legend title
#' @param size size for all line, default is 1
#' @param color color, which be more than one, default is black and red
#' @param type line type, default is 1
#' @param fill fill color for base map
#' @param border.color map border color
#' @param border.size map border size
#' @param basemap logical, default is TRUE
#'
#' @return a ggplot2 migration map
#' @export
#'
#' @examples
#' plot(1:10)
migration<-function(data,
                     start,
                     destination,
                     choice=NULL,
                     color.variable=NULL,
                     size.variable=NULL,
                     type.variable=NULL,
                     lgd.color.title=NULL,
                     lgd.size.title=NULL,
                     lgd.type.title=NULL,
                     size=1,
                     color=c('black','red'),
                     type=1,
                     fill='#e5e5e5',
                     border.color='black',
                     border.size=0.5,
                     basemap=TRUE){
    data$migrationid=1:nrow(data)
    data2=do::reshape_toLong(data = data,
                             var.names = c(start,destination))
    data3=data2[,-(ncol(data2)-1)]
    colnames(data3)[ncol(data3)]='shapename'
    shapename='shapename'
    group='migrationid'
    res=checkname(data = data3,shapename = shapename,choice = choice)
    map.ready0=res$map.ready0
    data=res$data
    #data prepare
    variable=c(shapename,group)
    k.color=is.null(color.variable)
    k.size=is.null(size.variable)
    k.type=is.null(type.variable)
    if (!k.color) variable=c(variable,color.variable)
    if (!k.size) variable=c(variable,size.variable)
    if (!k.type) variable=c(variable,type.variable)
    variable=unique(variable)
    dd.value=data[,variable]
    map.ready=merge(x=map.ready0,y=dd.value,by.x='Name',by.y=shapename)
    map.df=data.frame(map.ready,check.names = FALSE)
    map.ready
    map.df
    ####plot
    if (basemap){
        p0<-ggplot(data = map.ready) +
            geom_sf(color=border.color,size=border.size,fill=fill) +
            theme_half_open()
        p0
    }else{
        p0<-ggplot(data = map.ready) +
            geom_sf(color=NA,size=NA,fill=NA) +
            theme_half_open()
        p0
    }
    #title
    if (is.null(lgd.color.title)) lgd.color.title=color.variable
    if (is.null(lgd.size.title)) lgd.size.title=size.variable
    if (is.null(lgd.type.title)) lgd.type.title=type.variable
    #0:NULL
    if (k.color & k.size & k.type){
        p_line=p0+geom_sf_line(aes_string(group=group),
                               color=color[1],
                               size=size,
                               linetype=type)
    }
    #1: color
    if (!k.color & k.size & k.type){
        p_line=p0+geom_sf_line(aes_string(group=group,
                                          color=color.variable),
                               size=size,
                               linetype=type)
        if (is.numeric(map.df[,color.variable])){
            p_line<-p_line+scale_color_gradientn(name=lgd.color.title,
                                                 colours = color)
        }else{
            p_line<-p_line+scale_color_manual(name=lgd.color.title,
                values = colorRampPalette(color)(length(unique(map.df[,color.variable]))))
        }
    }
    #1:size
    if (k.color & !k.size & k.type){
        p_line<-p0+geom_sf_line(aes_string(group=group,
                                           size=size.variable),
                                color=color[1],
                                linetype=type)
        if (is.numeric(map.df[,size.variable])){
            p_line<-p_line+scale_size(name=lgd.size.title)
        }else{
            p_line<-p_line+scale_size_discrete(name=lgd.size.title)
        }
        p_line
    }
    #1:type
    if (k.color & k.size & !k.type){
        p_line=p0+geom_sf_line(aes_string(group=group,
                                          linetype=factor(map.df[,type.variable])),
                               color=color[1],
                               size=size)
        p_line<-p_line+scale_linetype(name=lgd.type.title)

    }
    #2:color and size
    if (!k.color & !k.size & k.type){
        p_line<-p0+geom_sf_line(aes_string(group=group,
                                           color=color.variable,
                                           size=size.variable),
                                linetype=type)
        if (is.numeric(map.df[,size.variable])){
            p_line<-p_line+scale_size(name=lgd.size.title)
        }else{
            p_line<-p_line+scale_size_discrete(name=lgd.size.title)
        }
        if (is.numeric(map.df[,color.variable])){
            p_line<-p_line+scale_color_gradientn(name=lgd.color.title,
                                                 colours = color)
        }else{
            p_line<-p_line+scale_color_manual(name=lgd.color.title,
                values = colorRampPalette(color)(length(unique(map.df[,color.variable]))))
        }
        p_line
    }
    #2:color and type
    if (!k.color & k.size & !k.type){
        p_line<-p0+geom_sf_line(aes_string(group=group,
                                           color=color.variable,
                                           linetype=factor(map.df[,type.variable])),
                                size=size)
        if (is.numeric(map.df[,color.variable])){
            p_line<-p_line+scale_color_gradientn(name=lgd.color.title,
                                                 colours = color)
        }else{
            p_line<-p_line+scale_color_manual(name=lgd.color.title,
                values = colorRampPalette(color)(length(unique(map.df[,color.variable]))))
        }
        p_line<-p_line+scale_linetype(name=lgd.type.title)
        p_line
    }
    #2:size and type
    if (k.color & !k.size & !k.type){
        p_line<-p0+geom_sf_line(aes_string(group=group,
                                           size=size.variable,
                                           linetype=factor(map.df[,type.variable])),
                                color=color[1])
        if (is.numeric(map.df[,size.variable])){
            p_line<-p_line+scale_size(name=lgd.size.title)
        }else{
            p_line<-p_line+scale_size_discrete(name=lgd.size.title)
        }
        p_line<-p_line+scale_linetype(name=lgd.type.title)
        p_line
    }
    #3:all
    if (!k.color & !k.size & !k.type){
        p_line<-p0+geom_sf_line(aes_string(group=group,
                                           size=size.variable,
                                           linetype=factor(map.df[,type.variable]),
                                           color=color.variable))
        if (is.numeric(map.df[,color.variable])){
            p_line<-p_line+scale_color_gradientn(name=lgd.color.title,
                                                 colours = color)
        }else{
            p_line<-p_line+scale_color_manual(name=lgd.color.title,
                                              values = color)
        }
        if (is.numeric(map.df[,size.variable])){
            p_line<-p_line+scale_size(name=lgd.size.title)
        }else{
            p_line<-p_line+scale_size_discrete(name=lgd.size.title)
        }
        p_line<-p_line+scale_linetype(name=lgd.type.title)
        p_line
    }
    p_line<-p_line+
        theme(axis.title = element_blank())
    return(p_line)
}
