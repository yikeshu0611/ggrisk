#' Risk Plot for Cox Regression
#' @description A ggplot style risk plot for cox regression.
#'
#' @param data dataframe data
#' @param time variable name for following time
#' @param event variable name for event time
#' @param cutoff a number or string, which can be roc, median or cutoff
#' @param cutoff.show a list contains a logical argument show, default is TRUE,
#'     x and y coordinates, label and size.
#' @param size size for y-axis labels, points, vertical line, legend title,
#'     and legend text in figure A and B
#' @param height.ratio relative height
#' @param family family
#' @param code0 string, code as 0 in event, default is Alive
#' @param code1 string, code as 1 in event, default is Dead
#' @param code.highrisk string, code as high risk group in risk score, default is high
#' @param code.lowrisk string, code as high low group in risk score, default is low
#' @param ylab.title a string vector of label titles for figure A, B and C
#' @param legend.title a string vector of legend titles for figure A, B and C
#' @param color a list of three string vectors of colors for figure A, B and C
#' @param ylab.vjust vertical just for figure A and figure B
#'
#' @importFrom ggplot2 aes aes_string geom_point geom_vline theme element_blank element_text scale_colour_hue coord_trans
#' @importFrom ggplot2 ylab geom_tile unit scale_fill_gradient2 scale_x_continuous geom_raster theme_classic annotate
#' @importFrom ggplot2 scale_color_manual element_line scale_fill_manual ggplot scale_fill_manual
#' @importFrom stats as.formula median sd
#' @return a ggplot picture
#' @export
#'
#' @examples
#' library(ggrisk)
#' ggrisk(
#'     data=mtcars,
#'     time='qsec',
#'     event='am'
#'     )
#' ggrisk(
#'     data=mtcars,
#'     time='qsec',
#'     event='am',
#'     cutoff.show=list(show=FALSE)
#'     )
ggrisk <- function(data,time,event,
                    code0='Alive',
                    code1='Dead',
                    code.highrisk='high',
                    code.lowrisk='low',
                    cutoff='roc',
                    ylab.title=c(A='Risk Score',B='Survival Time'),
                    ylab.vjust=c(A=3.6,B=1),

                    cutoff.show=list(show=TRUE,
                                     x=NULL,
                                     y=NULL,
                                     label=NULL),
                    legend.title=c(A='Risk Group',B='Status',C='Expression'),
                    size=list(ABC=1.5,
                              ylab.title=14,
                              ytext=11,
                              yticks=0.5,
                              AB.points=2,
                              dashline=1,
                              cutoff=5,
                              legendtitle=13,
                              legendtext=12),
                    color=list(A=c(low='#00bfc4',high='#f8766d'),
                               B=c(code0='#00bfc4',code1='#f8766d'),
                               C=c(low='#00bfc4',median='white',high='#f8766d')),
                    height.ratio=c(0.1,0.1,0.01,0.15),
                    family='serif') {
    #  1.regression
    x = do::inner_Add_Symbol(set::not(colnames(data), c(time, event)))
    formu = paste0('survival::Surv(', time, ',', event, ')~', x)
    f = survival::coxph(formula = as.formula(formu), data = data)
    #  2.risk point nomgram.points and lp
    riskscore = f$linear.predictors
    #  3.cbind and rank
    data2 = cbind(data, riskscore)
    data3 = data2[order(data2$riskscore), ]
    #  4.cutoff
    if (cutoff == 'roc') {
        cutoff.point = cutoff::roc(score = data3$riskscore,
                                   class = data3[,event])$cutoff
    } else if (cutoff == 'cutoff') {
        rs = cutoff::cox(
            data = data3,
            time = time,
            y = event,
            x = 'riskscore',
            cut.numb = 1,
            n.per = 0.1,
            y.per = 0.1,
            round = 20
        )
        fastStat::to.numeric(rs$p.adjust)=1
        cutoff.point = (rs$cut1[rs$p.adjust == min(rs$p.adjust)])
        if (length(cutoff.point)>1) cutoff.point=cutoff.point[1]
    } else if (cutoff == 'median') {
        cutoff.point=median(x = data3$riskscore,na.rm = TRUE)
    }else{
        cutoff.point=cutoff
    }
    if (cutoff.point < min(riskscore) || cutoff.point>max(riskscore)){
        stop('cutoff must between ',min(riskscore),' and ',max(riskscore))
    }
    # 5.risk for low and high
    type = cutoff::roc(score = data3$riskscore,
                       class = data3[,event])$type
    if (type == 'positive classification') {
        `Risk Group` = ifelse(data3$riskscore > cutoff.point,code.highrisk,code.lowrisk)
    } else{
        `Risk Group` = ifelse(data3$riskscore < cutoff.point,code.highrisk,code.lowrisk)
    }
    data4 = cbind(data3, `Risk Group`)
    cut.position=(1:nrow(data4))[data4$riskscore == cutoff.point]
    if (length(cut.position)==1){
        cut.position=which.min(abs(data4$riskscore - cutoff.point))
    }else if (length(cut.position)>1){
        cut.position=cut.position[length(cut.position)]
    }
    data4$riskscore=round(data4$riskscore,1)
    data4[, time]=round(data4[, time],1)
    #figure A risk plot
    #rearange colorA
    color$A=c(color$A['low'],color$A['high'])
    names(color$A)=c(code.lowrisk,code.highrisk)
    fA = ggplot(data = data4,
                         aes_string(
                             x = 1:nrow(data4),
                             y = data4$riskscore,
                             color=factor(`Risk Group`)
                             )
                         ) +
        geom_point(size = size$AB.points) +
        scale_color_manual(name=legend.title['A'],values = color$A) +
        geom_vline(
            xintercept = cut.position,
            linetype = 'dotted',
            size = size$dashline
        ) +
        theme(
            panel.grid = element_blank(),
            panel.background = element_blank(),
            axis.ticks.x = element_blank(),
            axis.line.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y=element_text(family = family,size=15),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = size$ylab.title)
        ) +
        theme(axis.line.y = element_line(colour = "black",size=size$yline),
              axis.text.y = element_text(size=size$ytext),
              axis.ticks.y = element_line(size = size$yticks))+
        theme(legend.title = element_text(size = size$legendtitle,
                                          family = family),
              legend.text = element_text(size=size$legendtext,
                                         family = family))+
        coord_trans()+
        ylab(ylab.title['A'])+
        theme(axis.title.y = element_text(
            angle = 90,
            family=family,
            vjust = ylab.vjust['A'])
        )+
        scale_x_continuous(expand = c(0,0.4))
    fA
    if (cutoff.show$show){
        fA=fA+ annotate("text",
                    x=ifelse(is.null(cutoff.show$x),
                             cut.position+3,cutoff.show$x),
                    y=ifelse(is.null(cutoff.show$y),cutoff.point,cutoff.show$y),
                    label=paste0('cutoff: ',
                               round(cutoff.point,2)),
                    family=family,
                    fontface="plain",
                    colour="black",
                    size=size$cutoff)
    }
    #fB
    #forB is the y lab style
    forB=fA$theme$axis.title.y
    forB$vjust=ylab.vjust['B']
    color$B=c(color$B['code0'],color$B['code1'])
    names(color$B)=c(code0,code1)
    fB=ggplot(data = data4,
                       aes_string(
                           x = 1:nrow(data4),
                           y = data4[, time],
                           color=factor(ifelse(data4[,event]==1,code1,code0)))
                       ) +
        geom_point(size=size$AB.points)+
        scale_color_manual(name=legend.title['B'],values = color$B) +
        geom_vline(
            xintercept = cut.position,
            linetype = 'dotted',
            size = size$dashline
        )  +
        theme(
            panel.grid = element_blank(),
            panel.background = element_blank(),
            axis.ticks.x = element_blank(),
            axis.line.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y=element_text(family = family,size=15),
            axis.title.y = element_text(size = size$ylab.title)
        ) +
        theme(axis.line.y = element_line(colour = "black",size=size$yline),
              axis.text.y = element_text(size=size$ytext),
              axis.ticks.y = element_line(size = size$yticks))+
        theme(legend.title = element_text(size = size$legendtitle,family = family),
              legend.text = element_text(size=size$legendtext,family = family))+
        ylab(ylab.title['B'])+
        theme(axis.title.y = forB)+
        coord_trans()+
        scale_x_continuous(expand = c(0,0.4))
    fB
    # middle
    middle = ggplot(data4, aes(
        x = 1:nrow(data4),
        y = 1)
        ) +
        geom_tile(aes(fill = data4$`Risk Group`))+
        scale_fill_manual(name=legend.title['A'],values = color$A)+
        theme(
            panel.grid = element_blank(),
            panel.background = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            plot.margin = unit(c(0.15,0,-0.3,0), "cm")
        )+
        theme(legend.title = element_text(size = size$legendtitle,family = family),
              legend.text = element_text(size=size$legendtext,family = family))+
        scale_x_continuous(expand = c(0,0.4))
    middle
    #fC
    data5 = data4[, set::not(colnames(data4), c(time, event,
                                                'Risk Group',
                                                'riskscore'))]
    for (i in 1:ncol(data5)) {
        data5[, i] = (data5[, i] - mean(data5[, i], na.rm = TRUE)) / sd(data5[, i], na.rm = TRUE)
    }
    data6 = cbind(id = 1:nrow(data5), data5)
    data7 = do::reshape_toLong(data = data6,
                               var.names = colnames(data5))
    A.style=fA$theme$axis.title.y
    A.style$family=family
    A.style$angle=0
    A.style$vjust=NULL
    fC = ggplot(data7, aes_string(x = 'id',
                                           y = 'variable',
                                           fill = 'value')) +
        geom_raster() +
        theme(
            panel.grid = element_blank(),
            panel.background = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.title = element_blank(),
            plot.background = element_blank() #the key to avoide legend overlap
        ) +
        scale_fill_gradient2(
            name = legend.title['C'],
            low = color$C[1],
            mid = color$C[2],
            high = color$C[3]
        ) +
        theme(axis.text = A.style)+
        theme(legend.title = element_text(size = size$legendtitle,family = family),
              legend.text = element_text(size=size$legendtext,family = family))+
        scale_x_continuous(expand = c(0,0.4))
    fC
    egg::ggarrange(
        fA,
        fB,
        middle,
        fC,
        ncol = 1,
        labels = c('A', 'B', 'C', ''),
        label.args = list(gp = grid::gpar(font = 2, cex =size$ABC,
                                          family=family)),
        heights = height.ratio
    )
}
