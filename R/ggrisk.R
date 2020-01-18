#' @title Riskscore Plot for Cox Regression
#' @param data dataframe data
#' @param time numeric variable. Name for following time
#' @param event must be numeric variable. Name for event, which must be coded as 0 and 1
#' @param heatmap.genes (optional) numeric variables. Name for genes
#' @param code.0 string. Code for event 0. Default is 'Alive'
#' @param code.1 string. Code for event 1. Default is 'Dead'
#' @param code.highrisk string. Code for highrisk in risk score. Default is 'High'
#' @param code.lowrisk string. Code for lowrisk in risk score. Default is 'Low'
#' @param cutoff.show logical, whether to show text for cutoff in figure A. Default is TRUE
#' @param cutoff.value string, which can be 'median', 'roc' or 'cutoff'. Even you can define it by yourself
#' @param cutoff.x numeric (optional), ordination x for cutoff text
#' @param cutoff.y numeric (optional), ordination y for cutoff text
#' @param cutoff.label (should be) string. Define cutoff label by yourself
#' @param title.A.ylab string, y-lab title for figure A. Default is 'Riskscore'
#' @param title.B.ylab string, y-lab title for figure B. Default is 'Survival Time'
#' @param title.A.legend string, legend title for figure A. Default is 'Risk Group'
#' @param title.B.legend string, legend title for figure B. Default is 'Status'
#' @param title.C.legend string, legend title for figure C. Default is 'Expression'
#' @param size.ABC numeric, size for ABC. Default is 1.5
#' @param size.ylab.title numeric, size for y-axis label title. Default is 14
#' @param size.Atext numeric, size for y-axis text in figure A. Default is 11
#' @param size.Btext numeric, size for y-axis text in figure B. Default is 11
#' @param size.Ctext numeric, size for y-axis text in figure C. Default is 11
#' @param size.yticks numeric, size for y-axis ticks. Default is 0.5
#' @param size.yline numeric, size for y-axis line. Default is 0.5
#' @param size.points numeric, size for scatter points. Default is 2
#' @param size.dashline numeric, size for dashline. Default is 1
#' @param size.cutoff numeric, size for cutoff text. Default is 5
#' @param size.legendtitle numeric, size for legend title. Default is 13
#' @param size.legendtext numeric, size for legend text. Default is 12
#' @param color.A color for figure A. Default is low = 'blue', high = 'red'
#' @param color.B color for figure B. Default is code.0 = 'blue', code.1 = 'red'
#' @param color.C color for figure C. Default is low = 'blue', median = 'white', high = 'red'
#' @param vjust.A.ylab numeric, vertical just for y-label in figure A. Default is 1
#' @param vjust.B.ylab numeric, vertical just for y-label in figure B. Default is 2
#' @param family family, default is sans
#' @param expand.x  numeric, expand for x-axis
#' @param relative_heights numeric, relative heights for figure A, B, colored side bar and heatmap. Default is 0.1 0.1 0.01 and 0.15
#' @importFrom ggplot2 aes aes_string geom_point geom_vline theme element_blank element_text scale_colour_hue coord_trans
#' @importFrom ggplot2 ylab geom_tile unit scale_fill_gradient2 scale_x_continuous geom_raster theme_classic annotate
#' @importFrom ggplot2 scale_color_manual element_line scale_fill_manual ggplot scale_fill_manual
#' @importFrom stats as.formula median sd cor
#' @return A riskscore picture
#' @export
#'
#' @examples
#' #plot
#' ggrisk(data=LIRI,time='time',event='status')
#'
#' #heatmap.genes
#' ggrisk(data=LIRI,time='time',event='status',
#'        heatmap.genes=c('GPR182','CENPA','BCO2'))
#'
#' #cutoff
#' ggrisk(data=LIRI,time='time',event='status',
#'        cutoff.value='median') #default
#' ggrisk(data=LIRI,time='time',event='status',
#'        cutoff.value='roc')
#' ggrisk(data=LIRI,time='time',event='status',
#'        cutoff.value='cutoff')
#' ggrisk(data=LIRI,time='time',event='status',
#'        cutoff.value=-1)
#' ggrisk(data=LIRI,time='time',event='status',
#'        cutoff.value='median',
#'        cutoff.x = 145,
#'        cutoff.y = -0.8)
#' ggrisk(data=LIRI,time='time',event='status',
#'        cutoff.value='median',
#'        cutoff.x = 145,
#'        cutoff.y = -0.8,
#'        cutoff.label='This is cutoff')
#'
#' #code for 0 and 1
#' ggrisk(data=LIRI,time='time',event='status',
#'        cutoff.value='median',
#'        cutoff.x = 145,
#'        cutoff.y = -0.8,
#'        code.0 = 'Still Alive',
#'        code.1 = 'Already Dead')
#'
#' #code for high and low risk group
#' ggrisk(data=LIRI,time='time',event='status',
#'        cutoff.value='median',
#'        cutoff.x = 145,
#'        cutoff.y = -0.8,
#'        code.0 = 'Still Alive',
#'        code.1 = 'Already Dead',
#'        code.highrisk = 'High Risk',
#'        code.lowrisk = 'Low Risk')
#' #title
#' ggrisk(data=LIRI,time='time',event='status',
#'        cutoff.value='median',
#'        cutoff.x = 145,
#'        cutoff.y = -0.8,
#'        code.0 = 'Still Alive',
#'        code.1 = 'Already Dead',
#'        code.highrisk = 'High Risk',
#'        code.lowrisk = 'Low Risk',
#'        title.A.ylab='Risk Score',
#'        title.B.ylab='Survival Time(year)',
#'        title.A.legend='Risk Group',
#'        title.B.legend='Status',
#'        title.C.legend='Expression')
#' #size
#' ggrisk(data=LIRI,time='time',event='status',
#'        cutoff.value='median',
#'        cutoff.x = 145,
#'        cutoff.y = -0.8,
#'        code.0 = 'Still Alive',
#'        code.1 = 'Already Dead',
#'        code.highrisk = 'High Risk',
#'        code.lowrisk = 'Low Risk',
#'        title.A.ylab='Risk Score',
#'        title.B.ylab='Survival Time(year)',
#'        title.A.legend='Risk Group',
#'        title.B.legend='Status',
#'        title.C.legend='Expression',
#'        size.ABC=1.5,
#'        size.ylab.title=14,
#'        size.Atext=11,
#'        size.Btext=11,
#'        size.Ctext=11,
#'        size.yticks=0.5,
#'        size.yline=0.5,
#'        size.points=2,
#'        size.dashline=1,
#'        size.cutoff=5,
#'        size.legendtitle=13,
#'        size.legendtext=12)
#' #color
#' ggrisk(data=LIRI,time='time',event='status',
#'        cutoff.value='median',
#'        cutoff.x = 145,
#'        cutoff.y = -0.8,
#'        code.0 = 'Still Alive',
#'        code.1 = 'Already Dead',
#'        code.highrisk = 'High Risk',
#'        code.lowrisk = 'Low Risk',
#'        title.A.ylab='Risk Score',
#'        title.B.ylab='Survival Time(year)',
#'        title.A.legend='Risk Group',
#'        title.B.legend='Status',
#'        title.C.legend='Expression',
#'        size.ABC=1.5,
#'        size.ylab.title=14,
#'        size.Atext=11,
#'        size.Btext=11,
#'        size.Ctext=11,
#'        size.yticks=0.5,
#'        size.yline=0.5,
#'        size.points=2,
#'        size.dashline=1,
#'        size.cutoff=5,
#'        size.legendtitle=13,
#'        size.legendtext=12,
#'        color.A=c(low='blue',high='red'),
#'        color.B=c(code.0='blue',code.1='red'),
#'        color.C=c(low='blue',median='white',high='red'))
#'
#' #vjust
#' ggrisk(data=LIRI,time='time',event='status',
#'        cutoff.value='median',
#'        cutoff.x = 145,
#'        cutoff.y = -0.8,
#'        code.0 = 'Still Alive',
#'        code.1 = 'Already Dead',
#'        code.highrisk = 'High Risk',
#'        code.lowrisk = 'Low Risk',
#'        title.A.ylab='Risk Score',
#'        title.B.ylab='Survival Time(year)',
#'        title.A.legend='Risk Group',
#'        title.B.legend='Status',
#'        title.C.legend='Expression',
#'        size.ABC=1.5,
#'        size.ylab.title=14,
#'        size.Atext=11,
#'        size.Btext=11,
#'        size.Ctext=11,
#'        size.yticks=0.5,
#'        size.yline=0.5,
#'        size.points=2,
#'        size.dashline=1,
#'        size.cutoff=5,
#'        size.legendtitle=13,
#'        size.legendtext=12,
#'        color.A=c(low='blue',high='red'),
#'        color.B=c(code.0='blue',code.1='red'),
#'        color.C=c(low='blue',median='white',high='red'),
#'        vjust.A.ylab=1,
#'        vjust.B.ylab=2)
#'
#' #family, expand, relative height
#' ggrisk(data=LIRI,time='time',event='status',
#'        cutoff.value='median',
#'        cutoff.x = 145,
#'        cutoff.y = -0.8,
#'        code.0 = 'Still Alive',
#'        code.1 = 'Already Dead',
#'        code.highrisk = 'High Risk',
#'        code.lowrisk = 'Low Risk',
#'        title.A.ylab='Risk Score',
#'        title.B.ylab='Survival Time(year)',
#'        title.A.legend='Risk Group',
#'        title.B.legend='Status',
#'        title.C.legend='Expression',
#'        size.ABC=1.5,
#'        size.ylab.title=14,
#'        size.Atext=11,
#'        size.Btext=11,
#'        size.Ctext=11,
#'        size.yticks=0.5,
#'        size.yline=0.5,
#'        size.points=2,
#'        size.dashline=1,
#'        size.cutoff=5,
#'        size.legendtitle=13,
#'        size.legendtext=12,
#'        color.A=c(low='blue',high='red'),
#'        color.B=c(code.0='blue',code.1='red'),
#'        color.C=c(low='blue',median='white',high='red'),
#'        vjust.A.ylab=1,
#'        vjust.B.ylab=2,
#'        family='sans',
#'        expand.x=3,
#'        relative_heights=c(0.1,0.1,0.01,0.15))
ggrisk <- function(data,time,event,heatmap.genes,
                   code.0='Alive',
                   code.1='Dead',
                   code.highrisk='High',
                   code.lowrisk='Low',
                   cutoff.show=TRUE,
                   cutoff.value='median',
                   cutoff.x,
                   cutoff.y,
                   cutoff.label,
                   title.A.ylab='Risk Score',
                   title.B.ylab='Survival Time',
                   title.A.legend='Risk Group',
                   title.B.legend='Status',
                   title.C.legend='Expression',
                   size.ABC=1.5,
                   size.ylab.title=14,
                   size.Atext=11,
                   size.Btext=11,
                   size.Ctext=11,
                   size.yticks=0.5,
                   size.yline=0.5,
                   size.points=2,
                   size.dashline=1,
                   size.cutoff=5,
                   size.legendtitle=13,
                   size.legendtext=12,
                   color.A=c(low='blue',high='red'),
                   color.B=c(code.0='blue',code.1='red'),
                   color.C=c(low='blue',median='white',high='red'),
                   vjust.A.ylab=1,
                   vjust.B.ylab=2,
                   family='sans',
                   expand.x=3,
                   relative_heights=c(0.1,0.1,0.01,0.15)) {
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
    if (cutoff.value == 'roc') {
        cutoff.point = cutoff::roc(score = data3$riskscore,
                                   class = data3[,event])$cutoff
    } else if (cutoff.value == 'cutoff') {
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
    } else if (cutoff.value == 'median') {
        cutoff.point=median(x = data3$riskscore,na.rm = TRUE)
    }else{
        cutoff.point=cutoff.value
    }
    if (cutoff.point < min(riskscore) || cutoff.point>max(riskscore)){
        stop('cutoff must between ',min(riskscore),' and ',max(riskscore))
    }
    # 5.risk for low and high
    reg_cph=suppressWarnings(rms::cph(formula = as.formula(formu), data = data,surv=TRUE))
    df2=nomogramFormula::prob_cal(reg = reg_cph,times = median(data3[,time],na.rm = TRUE))
    #plot(x=df2$linear.predictors,df2[,2])
    correlaiton=cor(df2[,1],df2[,2],method = 'spearman')
    if (correlaiton<0) {
        #correlaiton <0, meaning that high-score is shorter life, high risk
        `Risk Group` = ifelse(data3$riskscore > cutoff.point,code.highrisk,code.lowrisk)
    } else{
        #correlaiton>0 means that low-scoe is high-risk
        `Risk Group` = ifelse(data3$riskscore < cutoff.point,code.highrisk,code.lowrisk)
    }
    data4 = cbind(data3, `Risk Group`)
    cut.position=(1:nrow(data4))[data4$riskscore == cutoff.point]
    if (length(cut.position)==0){
        cut.position=which.min(abs(data4$riskscore - cutoff.point))
    }else if (length(cut.position)>1){
        cut.position=cut.position[length(cut.position)]
    }
    data4$riskscore=round(data4$riskscore,1)
    data4[, time]=round(data4[, time],1)
    #figure A risk plot
    #rearange colorA
    color.A=c(color.A['low'],color.A['high'])
    names(color.A)=c(code.lowrisk,code.highrisk)
    fA = ggplot(data = data4,
                aes_string(
                    x = 1:nrow(data4),
                    y = data4$riskscore,
                    color=factor(`Risk Group`)
                    )
                ) +
        geom_point(size = size.points) +
        scale_color_manual(name=title.A.legend,values = color.A) +
        geom_vline(
            xintercept = cut.position,
            linetype = 'dotted',
            size = size.dashline
        ) +
        #bg
        theme(
            panel.grid = element_blank(),
            panel.background = element_blank())+
        #x-axis
        theme(
            axis.ticks.x = element_blank(),
            axis.line.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank()
        ) +
        #y-axis
        theme(
            axis.title.y = element_text(
                size = size.ylab.title,vjust = vjust.A.ylab,angle = 90,family=family),
            axis.text.y = element_text(size=size.Atext,family = family),
            axis.line.y = element_line(size=size.yline,colour = "black"),
            axis.ticks.y = element_line(size = size.yticks,colour = "black"))+
        #legend
        theme(legend.title = element_text(size = size.legendtitle,family = family),
              legend.text = element_text(size=size.legendtext,family = family))+
        coord_trans()+
        ylab(title.A.ylab)+
        scale_x_continuous(expand = c(0,expand.x))
    fA
    if (cutoff.show){
        if (missing(cutoff.label)) cutoff.label=paste0('cutoff: ',round(cutoff.point,2))
        if (missing(cutoff.x)) cutoff.x=cut.position+3
        if (missing(cutoff.y)) cutoff.y=cutoff.point
        fA=fA+ annotate("text",
                    x=cutoff.x,
                    y=cutoff.y,
                    label=cutoff.label,
                    family=family,
                    size=size.cutoff,
                    fontface="plain",
                    colour="black")
    }
    fA
    #fB
    color.B=c(color.B['code.0'],color.B['code.1'])
    names(color.B)=c(code.0,code.1)
    fB=ggplot(data = data4,
                       aes_string(
                           x = 1:nrow(data4),
                           y = data4[, time],
                           color=factor(ifelse(data4[,event]==1,code.1,code.0)))
                       ) +
        geom_point(size=size.points)+
        scale_color_manual(name=title.B.legend,values = color.B) +
        geom_vline(
            xintercept = cut.position,
            linetype = 'dotted',
            size = size.dashline
        )  +
        theme(
            panel.grid = element_blank(),
            panel.background = element_blank())+
        #x a-xis
        theme(
            axis.ticks.x = element_blank(),
            axis.line.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank()
        ) +
        #y-axis
        theme(
            axis.title.y = element_text(
                size = size.ylab.title,vjust = vjust.B.ylab,angle = 90,family=family),
            axis.text.y = element_text(size=size.Btext,family = family),
            axis.ticks.y = element_line(size = size.yticks),
            axis.line.y = element_line(size=size.yline,colour = "black")
            )+
        theme(legend.title = element_text(size = size.legendtitle,family = family),
              legend.text = element_text(size=size.legendtext,family = family))+
        ylab(title.B.ylab)+
        coord_trans()+
        scale_x_continuous(expand = c(0,expand.x))
    fB
    # middle
    middle = ggplot(data4, aes(
        x = 1:nrow(data4),
        y = 1)
        ) +
        geom_tile(aes(fill = data4$`Risk Group`))+
        scale_fill_manual(name=title.A.legend,values = color.A)+
        theme(
            panel.grid = element_blank(),
            panel.background = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            plot.margin = unit(c(0.15,0,-0.3,0), "cm")
        )+
        theme(legend.title = element_text(size = size.legendtitle,family = family),
              legend.text = element_text(size=size.legendtext,family = family))+
        scale_x_continuous(expand = c(0,expand.x))
    middle
    #fC
    if (missing(heatmap.genes))  heatmap.genes=set::not(colnames(data4),
                                                        c(time, event,
                                                          'Risk Group',
                                                          'riskscore'))
    data5 = data4[,heatmap.genes]
    if (length(heatmap.genes)==1){
        data5=data.frame(data5)
        colnames(data5)=heatmap.genes
    }
    for (i in 1:ncol(data5)) {
        data5[, i] = (data5[, i] - mean(data5[, i], na.rm = TRUE)) / sd(data5[, i], na.rm = TRUE)
    }
    data6 = cbind(id = 1:nrow(data5), data5)
    data7 = do::reshape_toLong(data = data6,
                               var.names = colnames(data5))
    fC = ggplot(data7, aes_string(x = 'id',y = 'variable',fill = 'value')) +
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
            name = title.C.legend,
            low = color.C[1],
            mid = color.C[2],
            high = color.C[3]
        ) +
        theme(axis.text = element_text(size=size.Ctext,family = family))+
        theme(legend.title = element_text(size = size.legendtitle,family = family),
              legend.text = element_text(size=size.legendtext,family = family))+
        scale_x_continuous(expand = c(0,expand.x))
    fC
    egg::ggarrange(
        fA,
        fB,
        middle,
        fC,
        ncol = 1,
        labels = c('A', 'B', 'C', ''),
        label.args = list(gp = grid::gpar(font = 2,
                                          cex =size.ABC,
                                          family=family)),
        heights = relative_heights
    )
}
