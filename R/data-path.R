#' Generate Path Data
#' @description Generate path data for example
#' @return a dataframe
#' @export
#'
pathdata <- function(){
    province=c(toUTF8('\u6E56\u5357\u7701'),
               toUTF8('\u5E7F\u897F\u58EE\u65CF\u81EA\u6CBB\u533A'),
               toUTF8('\u6E56\u5317\u7701'),
               toUTF8('\u8D35\u5DDE\u7701'),
               toUTF8('\u6CB3\u5357\u7701'),
               toUTF8('\u6C5F\u897F\u7701'),
               toUTF8('\u798F\u5EFA\u7701'),
               toUTF8('\u9655\u897F\u7701'),
               toUTF8('\u56DB\u5DDD\u7701'),
               toUTF8('\u91CD\u5E86\u5E02'),
               toUTF8('\u6D59\u6C5F\u7701'),
               toUTF8('\u5317\u4EAC\u5E02'),
               toUTF8('\u5B89\u5FBD\u7701'),
               toUTF8('\u6CB3\u5317\u7701'),
               toUTF8('\u5C71\u4E1C\u7701'),
               toUTF8('\u9999\u6E2F\u7279\u522B\u884C\u653F\u533A'),
               toUTF8('\u6FB3\u95E8\u7279\u522B\u884C\u653F\u533A'),
               toUTF8('\u6C5F\u82CF\u7701'),
               toUTF8('\u5C71\u897F\u7701'),
               toUTF8('\u4E91\u5357\u7701'),
               toUTF8('\u4E0A\u6D77\u5E02'),
               toUTF8('\u6D77\u5357\u7701'),
               toUTF8('\u8FBD\u5B81\u7701'),
               toUTF8('\u7518\u8083\u7701'),
               toUTF8('\u53F0\u6E7E\u7701'),
               toUTF8('\u5929\u6D25\u5E02'),
               toUTF8('\u65B0\u7586\u7EF4\u543E\u5C14\u81EA\u6CBB\u533A'),
               toUTF8('\u5409\u6797\u7701'),
               toUTF8('\u9ED1\u9F99\u6C5F\u7701'),
               toUTF8('\u5185\u8499\u53E4\u81EA\u6CBB\u533A'),
               toUTF8('\u9752\u6D77\u7701'),
               toUTF8('\u897F\u85CF\u81EA\u6CBB\u533A'),
               toUTF8('\u5B81\u590F\u56DE\u65CF\u81EA\u6CBB\u533A'))
    people=c(11580,11207,10000,9689,9378,9067,8756,8445,
             8134,7823,7512,7201,6890,6579,6268,5957,5646,
             5335,5024,4713,4402,4091,3469,3158,2847,1914,
             1603,670,359,48,1,1,1)
    destination=rep(toUTF8('\u5E7F\u4E1C\u7701'),length(province))
    data.frame(province,destination,people,stringsAsFactors = F)
}
