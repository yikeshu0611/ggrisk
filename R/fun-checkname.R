checkname <- function(data,shapename,choice){
    if (!exists(".mapitEnv")) .initial()
    .mapitEnv <- get(".mapitEnv", envir=.GlobalEnv)
    mapdata <- get("mapdata", envir=.mapitEnv)
    name=as.character(data[,shapename])
    check= name%==% mapdata$Name
    CHECK=FALSE
    if (length(check)==0){
        model.check=1
        CHECK=TRUE
    }else{
        model.check=2
        if (any(check=='integer(0)')) CHECK=TRUE
    }
    if (CHECK) {
        #library(crayon)
        if (model.check==1){
            unpaired=name
        }else{
            unpaired = names(check)[check=='integer(0)']
        }
        cat(tmcn::toUTF8('\n\u5B58\u5728'),length(unpaired),tmcn::toUTF8('\u4E2A'),red$bold(tmcn::toUTF8('\u672A\u80FD\u5339\u914D')),tmcn::toUTF8('\u7684\u5730\u533A\u540D\u79F0\n'))
        cat(tmcn::toUTF8('\u4ED6\u4EEC\u662F\n'))
        cat(paste0(unpaired,collapse = ', '))
        cat(red(tmcn::toUTF8('\n\n\u8F93\u51651\u4E2A\u6570\u5B57\u9009\u62E9\u4ED6\u4EEC\u5BF9\u5E94\u7684\u5730\u5740\n')))
        for (i in 1:length(unpaired)) {
            cat(red$bold(unpaired[i]),'\n')
            unp.i=unlist(strsplit(unpaired[i],''))
            for (j in 1:length(unp.i)) {
                if (j==1) res.tump=NULL
                res.tump=c(res.tump,unique(mapdata$Name[grepl(unp.i[j],mapdata$Name)]))
            }
            res.table=table(res.tump)
            res.t2=res.table[order(res.table,decreasing = TRUE)]
            res.names=names(res.t2)[res.t2>1]
            for (j in 1:length(res.names)) {
                if (j==1) res.similar=NULL
                rev.res=unlist(strsplit(res.names[j],''))
                res.similar=c(res.similar,sum(unlist(lapply(rev.res %==% unp.i,length)))/length(rev.res))
            }
            res.similar
            names(res.similar)=res.names
            names.choice=names(res.similar)[order(res.similar,decreasing = TRUE)]
            j=1
            for (j in 1:length(names.choice)) {
                cat(paste0(red(j),': ',names.choice[j],'\n'))
            }
            cat(paste0(red(j+1),': ',tmcn::toUTF8('\u4E0D\u7EE7\u7EED\u4E86'),'\n'))
            if (!is.null(choice)){
                n=choice[i]
                cat(n,'\n')
            }else{
                if (i==1) n.all=NULL
                n=readline()
                while(n > (j+1) | n<1){
                    cat(tmcn::toUTF8('\u8F93\u5165\u8303\u56F4\u5FC5\u987B\u662F1~'),j+1,tmcn::toUTF8(',\u4F60\u8F93\u51FA\u8D85\u51FA\u8303\u56F4,\u8BF7\u91CD\u65B0\u8F93\u5165'))
                    n=readline()
                }
                if (n==(j+1)) return()
                n.all=c(n.all,n)
            }
            name[unpaired[i] %==% name]=names.choice[as.numeric(n)]
            cat('\n\n')
        }
        data[,shapename]=name
        cat('\n')
        cat(tmcn::toUTF8('\u6700\u7EC8\u7684'),length(name),tmcn::toUTF8('\u4E2A\u5730\u533A\u540D\u79F0\u662F:\n'))
        cat(name)
        cat('\n\n')
        if (is.null(choice)){
            cat(tmcn::toUTF8('\u53EF\u4EE5\u5411\u547D\u4EE4\u4E2D\u6DFB\u52A0choice\u53C2\u6570,\u6765\u907F\u514D\u6BCF\u6B21\u9009\u62E9\n'))
            cat(paste0(', choice = c(',paste0(n.all,collapse = ','),')'))
            cat('\n')
        }
    }
    #check again and plot
    check=as.character(name) %==% mapdata$Name
    CHECK=FALSE
    if (length(check)==0){
        model.check=1
        CHECK=TRUE
    }else{
        model.check=2
        if (any(check=='integer(0)')) CHECK=TRUE
    }
    if (CHECK) {
        #library(crayon)
        cat(tmcn::toUTF8('\n\u4ECD\u5B58\u5728'),
            red$bold(tmcn::toUTF8('\u672A\u80FD\u5339\u914D')),
            tmcn::toUTF8('\u7684\u5730\u533A\u540D\u79F0\n'))
        cat(tmcn::toUTF8('\u4ED6\u4EEC\u662F\n'))
        if (model.check==1){
            unpaired=name
        }else{
            unpaired = names(check)[check=='integer(0)']
        }
        return(paste0(unpaired,collapse = ', '))
    }
    loc=unlist(name %==% mapdata$Name)
    map.ready0=unique(mapdata[loc,])
    #check dup in quxian
    if (any(table(map.ready0$Name)>1)){
        #whether in the same city
        left.5=left(map.ready0$Code,5)
        delet.5=names(table(left.5))[table(left.5) ==1]
        map.ready0=map.ready0[-unlist(delet.5 %==% left(map.ready0$Code,5)),]
    }
    map.ready0
    res=list(data,map.ready0)
    names(res)=c('data','map.ready0')
    return(res)
}

