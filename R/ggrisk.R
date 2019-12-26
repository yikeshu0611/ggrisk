#' Risk Plot for Cox Regression
#' @description A ggplot style risk plot for cox regression.
#' @param data dataframe data
#' @param time variable name for following time
#' @param event variable name for event time
#' @param cutoff a number or string, which can be roc, median or cutoff
#' @param lab ylab for figure A and B
#' @param legend legend title for figure A, B and C
#' @param size size for y-axis labels, points, vertical line, legend title,
#'     and legend text in figure A and B
#' @param height.ratio relative height
#' @param family family
#' @importFrom ggplot2 aes aes_string geom_point geom_vline theme element_blank element_text scale_colour_hue
#' @importFrom ggplot2 ylab geom_tile unit scale_fill_gradient2 scale_x_continuous geom_raster theme_classic
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
ggrisk <- function(data,time,event,
                   cutoff='roc',
                   lab=list(fA.ylab='Risk Score',
                            fB.ylab='Survival Time'),
                   legend=list(fA.title='Risk Group',
                               fB.title='Status',
                               fC.title='Expression'),
                   size=list(ylab=14,
                             fAB.points=2,
                             fAB.vline=1,
                             fAB.legendtitle=12,
                             fAB.legendtext=10),
                   height.ratio=c(0.1,0.1,0.01,0.15),
                   family='serif') {
    color=list(fAB=c(low='#00bfc4', high='#f8766d'),
               fC=c(low='#00bfc4', mid='white',high='#f8766d'))
    ylab.angle=c(AB=90,C=0)
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
        `Risk Group` = ifelse(data3$riskscore > cutoff.point, 'high', 'low')
        fAB.color = ifelse(data3$riskscore > cutoff.point, color$fAB[1], color$fAB[2])
        lab.A = c('high', 'low')
    } else{
        `Risk Group` = ifelse(data3$riskscore < cutoff.point, 'high', 'low')
        fAB.color = ifelse(data3$riskscore < cutoff.point, color$fAB[1], color$fAB[2])
        lab.A = c('high', 'low')
    }
    data4 = cbind(data3, `Risk Group`)
    cut.position=(1:nrow(data4))[data4$riskscore == cutoff.point]
    if (!length(cut.position)){
        cut.position=which.min(abs(data4$riskscore - cutoff.point))
    }
    if (length(cut.position)>1){
        cut.position=cut.position[length(cut.position)]
    }
    data4$riskscore=round(data4$riskscore,1)
    data4[, time]=round(data4[, time],1)
    #figure A risk plot
    fA = ggplot2::ggplot(data = data4, aes(x = 1:nrow(data4),
                                  y = data4$riskscore)) +
        geom_point(aes(colour = fAB.color,
                       group = `Risk Group`),
                   size = size$fAB.points) +
        geom_vline(
            xintercept = cut.position,
            linetype = 'dotted',
            size = size$fAB.vline
        ) + theme_classic() +
        theme(
            panel.grid = element_blank(),
            panel.background = element_blank(),
            axis.ticks.x = element_blank(),
            axis.line.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = size$ylab),
            legend.title = element_text(size = size$fAB.legendtitle),
            legend.text = element_text(size = size$fAB.legendtext)
        ) + 
        scale_colour_hue(legend$fA,
                         labels = lab.A) +
        ylab(lab$fA.ylab)+
        theme(axis.title.y = element_text(
            angle = ylab.angle['AB'],
            family=family)
              )+
      scale_x_continuous(expand = c(0,0.3))
    #fB
    if (cutoff::judge_123(order(color$fAB))) {
        fB.cor = ifelse(data4[, event] == 1, color$fAB[1], color$fAB[2])
        lab.B = c('Dead', 'Alive')
    } else{
        fB.cor = ifelse(data4[, event] == 1, color$fAB[2], color$fAB[1])
        lab.B = c('Alive', 'Dead')
    }
    fB=ggplot2::ggplot(data = data4,
                         aes(x = 1:nrow(data4), y = data4[, time])) +
        geom_point(aes(color = fB.cor,
                       group = data4[, event]),
                   size = size$fAB.points) +
        geom_vline(
            xintercept = cut.position,
            linetype = 'dotted',
            size = size$fAB.vline
        ) +theme_classic() +
        theme(
            panel.grid = element_blank(),
            panel.background = element_blank(),
            axis.ticks.x = element_blank(),
            axis.line.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = size$ylab),
            legend.title = element_text(size = size$fAB.legendtitle),
            legend.text = element_text(size = size$fAB.legendtext)
        ) +
        scale_colour_hue(legend$fB, labels = lab.B) +
        ylab(lab$fB.ylab)+
        theme(axis.title.y = fA$theme$axis.title.y)+
      scale_x_continuous(expand = c(0,0.3))
    # middle
    middle = ggplot2::ggplot(data4, aes(
        x = 1:nrow(data4),
        y = 1,
        fill = `Risk Group`
    )) +
        geom_tile() +
        theme(
            panel.grid = element_blank(),
            panel.background = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.title = element_text(size = size$fAB.legendtitle),
            legend.text = element_text(size = size$fAB.legendtext),
            plot.margin = unit(c(0.15,0,-0.3,0), "cm")
        )+
      scale_x_continuous(expand = c(0,0.3))

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
    fA.title.style=fA$theme$axis.title.y
    fA.title.style$family=family
    fA.title.style$angle=ylab.angle['C']
    fC = ggplot2::ggplot(data7, aes_string(x = 'id', y = 'variable',
                           fill = 'value')) +
        geom_raster() +
        theme(
            panel.grid = element_blank(),
            panel.background = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.title = element_blank(),
            legend.title = element_text(size = size$fAB.legendtitle),
            legend.text = element_text(size = size$fAB.legendtext),
            plot.background = element_blank() #the key to avoide legend overlap
        ) +
        scale_fill_gradient2(
            name = legend$fC.title,
            low = color$fC[1],
            mid = color$fC[2],
            high = color$fC[3]
        ) + theme(
            axis.text = fA.title.style)+
      scale_x_continuous(expand = c(0,0.3))
    fC
   egg::ggarrange(
      fA,
      fB,
      middle,
      fC,
      ncol = 1,
      labels = c('A', 'B', 'C', ''),
      heights = height.ratio
    )
}
