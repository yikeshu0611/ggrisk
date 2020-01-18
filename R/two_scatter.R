#' @title Two Scatter Plot Plot for Cox Regression
#'
#' @param data dataframe data
#' @param time numeric variable. Name for following time
#' @param event must be numeric variable. Name for event, which must be coded as 0 and 1
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
#' @param title.xlab string, x-lab title for figure B. Default is 'Rank'
#' @param title.A.legend string, legend title for figure A. Default is 'Risk Group'
#' @param title.B.legend string, legend title for figure B. Default is 'Status'
#' @param size.AB numeric, size for ABC. Default is 1.5
#' @param size.ylab.title numeric, size for y-axis label title. Default is 14
#' @param size.xlab.title numeric, size for x-axis lab title. Default is 11
#' @param size.Atext numeric, size for y-axis text in figure A. Default is 11
#' @param size.Btext numeric, size for y-axis text in figure B. Default is 11
#' @param size.xtext numeric, size for x-axis text. Default is 11
#' @param size.xyticks numeric, size for y-axis ticks. Default is 0.5
#' @param size.xyline numeric, size for y-axis line. Default is 0.5
#' @param size.points numeric, size for scatter points. Default is 2
#' @param size.dashline numeric, size for dashline. Default is 1
#' @param size.cutoff numeric, size for cutoff text. Default is 5
#' @param size.legendtitle numeric, size for legend title. Default is 13
#' @param size.legendtext numeric, size for legend text. Default is 12
#' @param color.A color for figure A. Default is low = 'blue', high = 'red'
#' @param color.B color for figure B. Default is code.0 = 'blue', code.1 = 'red'
#' @param vjust.A.ylab numeric, vertical just for y-label in figure A. Default is 1
#' @param vjust.B.ylab numeric, vertical just for y-label in figure B. Default is 2
#' @param family family, default is sans
#' @param expand.x  numeric, expand for x-axis
#'
#' @importFrom ggplot2 aes aes_string geom_point geom_vline theme element_blank element_text scale_colour_hue coord_trans
#' @importFrom ggplot2 ylab geom_tile unit scale_fill_gradient2 scale_x_continuous geom_raster theme_classic annotate
#' @importFrom ggplot2 scale_color_manual element_line scale_fill_manual ggplot scale_fill_manual xlab
#' @importFrom stats as.formula median sd cor
#' @return A riskscore picture
#' @export
#'
#' @examples
#' #plot
#' two_scatter(data=LIRI,time='time',event='status')
#' #regulate cutoff
#' ##hidden cutoff
#' two_scatter(data=LIRI,time='time',event='status',
#'             cutoff.show = FALSE)
#' two_scatter(data=LIRI,time='time',event='status',
#'             cutoff.value = 'median')
#' two_scatter(data=LIRI,time='time',event='status',
#'             cutoff.value = 'roc')
#' two_scatter(data=LIRI,time='time',event='status',
#'             cutoff.value = 'cutoff')
#' two_scatter(data=LIRI,time='time',event='status',
#'             cutoff.value = -1)
#' two_scatter(data=LIRI,time='time',event='status',
#'             cutoff.value = 'median',
#'             cutoff.x = 142,
#'             cutoff.y = -0.5)
#' #code for 0 and 1
#' two_scatter(data=LIRI,time='time',event='status',
#'             cutoff.value = 'median',
#'             cutoff.x = 142,
#'             cutoff.y = -0.5,
#'             code.0 = 'Still Alive',
#'             code.1 = 'Dead')
#' #code for high and low risk group
#' two_scatter(data=LIRI,time='time',event='status',
#'             cutoff.value = 'median',
#'             cutoff.x = 142,
#'             cutoff.y = -0.5,
#'             code.0 = 'Still Alive',
#'             code.1 = 'Dead',
#'             code.highrisk = 'High Group',
#'             code.lowrisk = 'Low Group')
#' #title for legend, x and y lab
#' two_scatter(data=LIRI,time='time',event='status',
#'             cutoff.value = 'median',
#'             cutoff.x = 142,
#'             cutoff.y = -0.5,
#'             code.0 = 'Still Alive',
#'             code.1 = 'Dead',
#'             code.highrisk = 'High Group',
#'             code.lowrisk = 'Low Group',
#'             title.A.legend = 'Riskscore',
#'             title.B.legend = 'Event Status',
#'             title.A.ylab = 'Riskscore',
#'             title.B.ylab = 'Survival Time(year)',
#'             title.xlab = 'This is rank')
#' #vertical just for y-axis lab
#' two_scatter(data=LIRI,time='time',event='status',
#'             cutoff.value = 'median',
#'             cutoff.x = 142,
#'             cutoff.y = -0.5,
#'             code.0 = 'Still Alive',
#'             code.1 = 'Dead',
#'             code.highrisk = 'High Group',
#'             code.lowrisk = 'Low Group',
#'             title.A.legend = 'Riskscore',
#'             title.B.legend = 'Event Status',
#'             title.A.ylab = 'Riskscore',
#'             title.B.ylab = 'Survival Time(year)',
#'             title.xlab = 'This is rank',
#'             vjust.A.ylab = 1,
#'             vjust.B.ylab = 3)
#' #size
#' two_scatter(data=LIRI,time='time',event='status',
#'             cutoff.value = 'median',
#'             cutoff.x = 142,
#'             cutoff.y = -0.5,
#'             code.0 = 'Still Alive',
#'             code.1 = 'Dead',
#'             code.highrisk = 'High Group',
#'             code.lowrisk = 'Low Group',
#'             title.A.legend = 'Riskscore',
#'             title.B.legend = 'Event Status',
#'             title.A.ylab = 'Riskscore',
#'             title.B.ylab = 'Survival Time(year)',
#'             title.xlab = 'This is rank',
#'             vjust.A.ylab = 1,
#'             vjust.B.ylab = 3,
#'             size.AB = 2,
#'             size.ylab.title = 14,
#'             size.xlab.title = 14,
#'             size.Atext = 12,
#'             size.Btext = 12,
#'             size.xtext = 12,
#'             size.xyticks = 0.5,
#'             size.xyline = 0.5,
#'             size.dashline = 1.5,
#'             size.points = 1,
#'             size.cutoff = 5,
#'             size.legendtitle = 14,
#'             size.legendtext = 13)
#' #color
#' two_scatter(data=LIRI,time='time',event='status',
#'             cutoff.value = 'median',
#'             cutoff.x = 142,
#'             cutoff.y = -0.5,
#'             code.0 = 'Still Alive',
#'             code.1 = 'Dead',
#'             code.highrisk = 'High Group',
#'             code.lowrisk = 'Low Group',
#'             title.A.legend = 'Riskscore',
#'             title.B.legend = 'Event Status',
#'             title.A.ylab = 'Riskscore',
#'             title.B.ylab = 'Survival Time(year)',
#'             title.xlab = 'This is rank',
#'             vjust.A.ylab = 1,
#'             vjust.B.ylab = 3,
#'             size.AB = 2,
#'             size.ylab.title = 14,
#'             size.xlab.title = 14,
#'             size.Atext = 12,
#'             size.Btext = 12,
#'             size.xtext = 12,
#'             size.xyticks = 0.5,
#'             size.xyline = 0.5,
#'             size.dashline = 1.5,
#'             size.points = 1,
#'             size.cutoff = 5,
#'             size.legendtitle = 14,
#'             size.legendtext = 13,
#'             color.A = c(low='green',high='red'),
#'             color.B = c(code.0='green',code.1='red'))
#' #famli and expand
#' two_scatter(data=LIRI,time='time',event='status',
#'             cutoff.value = 'median',
#'             cutoff.x = 142,
#'             cutoff.y = -0.5,
#'             code.0 = 'Still Alive',
#'             code.1 = 'Dead',
#'             code.highrisk = 'High Group',
#'             code.lowrisk = 'Low Group',
#'             title.A.legend = 'Riskscore',
#'             title.B.legend = 'Event Status',
#'             title.A.ylab = 'Riskscore',
#'             title.B.ylab = 'Survival Time(year)',
#'             title.xlab = 'This is rank',
#'             vjust.A.ylab = 1,
#'             vjust.B.ylab = 3,
#'             size.AB = 2,
#'             size.ylab.title = 14,
#'             size.xlab.title = 14,
#'             size.Atext = 12,
#'             size.Btext = 12,
#'             size.xtext = 12,
#'             size.xyticks = 0.5,
#'             size.xyline = 0.5,
#'             size.dashline = 1.5,
#'             size.points = 1,
#'             size.cutoff = 5,
#'             size.legendtitle = 14,
#'             size.legendtext = 13,
#'             color.A = c(low='green',high='red'),
#'             color.B = c(code.0='green',code.1='red'),
#'             family = 'sans', # sans for Arail, serif for Times New Roman
#'             expand.x=10)
two_scatter <- function(data,time,event,
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
                   title.xlab='Rank',
                   title.A.legend='Risk Group',
                   title.B.legend='Status',
                   size.AB=1.5,
                   size.ylab.title=14,
                   size.xlab.title=14,
                   size.Atext=11,
                   size.Btext=11,
                   size.xtext=11,
                   size.xyticks=0.5,
                   size.xyline=0.5,
                   size.points=2,
                   size.dashline=1,
                   size.cutoff=5,
                   size.legendtitle=13,
                   size.legendtext=12,
                   color.A=c(low='blue',high='red'),
                   color.B=c(code.0='blue',code.1='red'),
                   vjust.A.ylab=1,
                   vjust.B.ylab=2,
                   family='sans',
                   expand.x=3) {
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
                        axis.line.y = element_line(size=size.xyline,colour = "black"),
                        axis.ticks.y = element_line(size = size.xyticks,colour = "black"))+
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
                        axis.line.x = element_line(size=size.xyline,colour = "black"),
                        axis.ticks.x = element_line(size=size.xyline,colour = "black"),
                        axis.text.x = element_text(size=size.xtext,family = family),
                        axis.title.x = element_text(size = size.xlab.title,family=family)
                ) +
                #y-axis
                theme(
                        axis.title.y = element_text(
                                size = size.ylab.title,vjust = vjust.B.ylab,angle = 90,family=family),
                        axis.text.y = element_text(size=size.Btext,family = family),
                        axis.ticks.y = element_line(size = size.xyticks),
                        axis.line.y = element_line(size=size.xyline,colour = "black")
                )+
                theme(legend.title = element_text(size = size.legendtitle,family = family),
                      legend.text = element_text(size=size.legendtext,family = family))+
                ylab(title.B.ylab)+xlab(title.xlab)+
                coord_trans()+
                scale_x_continuous(expand = c(0,expand.x))
        fB

        egg::ggarrange(
                fA,
                fB,
                ncol = 1,
                labels = c('A', 'B'),
                label.args = list(gp = grid::gpar(font = 2, cex =size.AB,
                                                  family=family))
        )
}
