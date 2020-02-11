#' @title New Coronavirus Data in 2019 China
#'
#' @return data of new coronavirus in 2019 china
#' @export
#'
nCov2019<-function(){
    province = c(
        toUTF8('\u6E56\u5317\u7701'),
        toUTF8('\u5E7F\u4E1C\u7701'),
        toUTF8('\u6D59\u6C5F\u7701'),
        toUTF8('\u6CB3\u5357\u7701'),
        toUTF8('\u6E56\u5357\u7701'),
        toUTF8('\u5B89\u5FBD\u7701'),
        toUTF8('\u6C5F\u897F\u7701'),
        toUTF8('\u6C5F\u82CF\u7701'),
        toUTF8('\u91CD\u5E86\u5E02'),
        toUTF8('\u5C71\u4E1C\u7701'),
        toUTF8('\u56DB\u5DDD\u7701'),
        toUTF8('\u5317\u4EAC\u5E02'),
        toUTF8('\u9ED1\u9F99\u6C5F\u7701'),
        toUTF8('\u4E0A\u6D77\u5E02'),
        toUTF8('\u798F\u5EFA\u7701'),
        toUTF8('\u9655\u897F\u7701'),
        toUTF8('\u6CB3\u5317\u7701'),
        toUTF8('\u5E7F\u897F\u58EE\u65CF\u81EA\u6CBB\u533A'),
        toUTF8('\u4E91\u5357\u7701'),
        toUTF8('\u6D77\u5357\u7701'),
        toUTF8('\u5C71\u897F\u7701'),
        toUTF8('\u8FBD\u5B81\u7701'),
        toUTF8('\u8D35\u5DDE\u7701'),
        toUTF8('\u5929\u6D25\u5E02'),
        toUTF8('\u7518\u8083\u7701'),
        toUTF8('\u5409\u6797\u7701'),
        toUTF8('\u5185\u8499\u53E4\u81EA\u6CBB\u533A'),
        toUTF8('\u65B0\u7586\u7EF4\u543E\u5C14\u81EA\u6CBB\u533A'),
        toUTF8('\u5B81\u590F\u56DE\u65CF\u81EA\u6CBB\u533A'),
        toUTF8('\u9999\u6E2F\u7279\u522B\u884C\u653F\u533A'),
        toUTF8('\u9752\u6D77\u7701'),
        toUTF8('\u53F0\u6E7E\u7701'),
        toUTF8('\u6FB3\u95E8\u7279\u522B\u884C\u653F\u533A'),
        toUTF8('\u897F\u85CF\u81EA\u6CBB\u533A')
    )
    diagnose = c(27100,1120,1075,1033,838,779,740,468,446,435,386,
                 326,307,292,250,208,206,195,140,128,115,105,96,90,
                 79,78,54,45,45,26,18,17,10,1)
    cure=c(1439,126,185,131,159,59,72,61,39,52,61,37,13,44,24,24,30,
           18,17,15,21,8,7,4,12,5,5,0,13,1,3,1,1,0)
    death=c(780,1,0,6,1,1,0,0,2,1,1,2,6,1,0,0,2,1,0,
            2,0,0,1,1,1,1,0,0,0,1,0,0,0,0)
    data.frame(province,diagnose,cure,death)
}
