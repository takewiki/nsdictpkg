
#' 获取当前版本
#'
#' @param brand 品牌
#' @param NSOwn 是否当保存网商专业词汇
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_ques_segment_current();
nsim_ques_segment_current <- function(brand='JBLH',NSOwn = 1){
  conn <- conn_nsim();
  version <- nsim_version_getCurrentVersion(brand,'nsdict');
  sql <- paste("select FId as FQuestionId,FentryId,FSegment  from ques_segment_version
where  FBrand = '",brand,"' and  fversiontxt ='",version,"' and  FNSOwn = ",NSOwn," ",sep="")
  res <- sql_select(conn,sql);
  return(res)
}


#' 处理答案分词的当前版本
#'
#' @param brand 品牌
#' @param NSOwn 是否只保留网商专业词汇报
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_answ_segment_current()
nsim_answ_segment_current <- function(brand='JBLH',NSOwn =1 ){
  conn <- conn_nsim();
  version <- nsim_version_getCurrentVersion(brand,'nsdict');
  sql <- paste("select FId as FAnswerId,FentryId,FSegment  from answ_segment_version
where  FBrand = '",brand,"' and  fversiontxt ='",version,"' and  FNSOwn = ",NSOwn," ",sep="")
  res <- sql_select(conn,sql);
  return(res)
}

#' 查询标准问题
#'
#' @param data  数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_find_top_Answer()
nsim_find_top_Answer <- function(data){
  ncount <- nrow(data);
  data_named <-data[data$FCategory !='未分类',]
  ncount_named <- nrow(data_named);
  #处理不同情况
  if (ncount_named >= 1){
    data_named$FChecked <- rep(0L,ncount_named);
    data_named[1,'FChecked'] <-1L;
    res <-data_named
  }else{
    data$FChecked <- rep(0L,ncount);
    data[1,'FChecked'] <-1L;
    res <-data
  }

  return(res);
}
#' 返回最新的问题分类数据
#'  sql:vw_ques_category_byDictSegment_current
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' ques_category_byDictSegment_current
ques_category_byDictSegment_current <- function(brand='JBLH'){
  conn <- conn_nsim();
  sql <-paste("select a.FId as FQuestionId,a.FEntryId,a.FSegment,b.FCategory from ques_segment_version  a
inner join dict b
on a.FBrand= b.FBrand
and a.FSegment =  b.FWord
inner join nsim_version  v
on a.FVersionTxt = v.FVersionTxt
and a.FBrand = v.FBrand
where a.FNSOwn =1 and
 v.FCurrentVersion = 1   and v.FType='nsdict'
 and a.FBrand='",brand,"'
 order by a.FId,FEntryId,b.FCategory",sep="")
  res <- sql_select(conn,sql);
  return(res)


}

#' 针对问题进行处理
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @import tsda
#' @import tsdo
#' @export
#'
#' @examples
#' ques_category_byDictSegment_underline();
ques_category_byDictSegment_underline <- function(brand='JBLH') {
  bb <-ques_category_byDictSegment_current(brand = brand);
  res <-combine_rows_into_longColumn(bb,id_var = 'FQuestionId',value_var = 'FCategory');
  ncount <- nrow(res);
  var_version <- tsda::nsim_version_getCurrentVersion(brand,'nsdict');
  res$FBrand <- rep(brand,ncount);
  res$FVersionTxt <-rep(var_version,ncount);
  nsim_save(res,'ques_category_version')
  #处理不在的记录,标记为未分类
  item_ques_all <- nsim_item_ques_currentVersion(brand);
  FQuestionId_all <- item_ques_all$FQuestionId;
  FQuestionId_part <-res$FQuestionId
  FQuestionId <- vector_not_in(FQuestionId_all,FQuestionId_part);
  uncategoryCount <- length(FQuestionId);
  FCategory <-rep('未分类',uncategoryCount);
  FBrand <- rep(brand,uncategoryCount);
  FVersionTxt <-rep(var_version,uncategoryCount);
  data_uncategoried <- data.frame(FQuestionId,FCategory,FBrand,FVersionTxt,stringsAsFactors = F);
  nsim_save(data_uncategoried,'ques_category_version')







}

#' 查询设置问题的分类当前分类，经过分词处理后
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' ques_category_query_current();
ques_category_query_current <- function(brand='JBLH'){

  conn <- conn_nsim();
  var_version <-nsim_version_getCurrentVersion(brand,'nsdict');
  sql <- paste("select FQuestionId,FCategory from ques_category_version
where fbrand ='",brand,"' and fversiontxt ='",var_version,"'
order by  FQuestionId
",sep="")
  res <- sql_select(conn,sql)
  return(res)
}

#' 答案分类根据问题分类进行处理
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' answ_category_by_ques_category_via_qalist();
answ_category_by_ques_category_via_qalist <-function(brand='JBLH'){
  var_version <- nsim_version_getCurrentVersion(brand,'nsdict');
  qalist <-nsim_qalist_fullTxt_current(brand);
  qalist_id <-unique(qalist[,c('FQuestionId','FAnswerId')]);
  ques_category <-ques_category_query_current(brand);
  res <- merge(ques_category,qalist_id);
  res <-res[,c('FAnswerId','FCategory')];
  res <- unique(res);
  res <- res[order(res$FAnswerId),]
  rownames(res)<-NULL
  res$FBrand <- rep(brand,nrow(res));
  res$FVersionTxt <-rep(var_version,nrow(res));
  col_names <- names(res);
  #处理一个答案多个分类的问题
  res <- split(res,res$FAnswerId);
  res <- lapply(res,nsim_find_top_Answer);
  res <- do.call('rbind',res);
  res <- res[res$FChecked == 1, ];
  res <- res[,col_names];
  #return(res)
  nsim_save(res,'answ_category_version')
}


#' 查询最新版本的答案分类
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' answ_category_query_current();
answ_category_query_current <- function(brand='JBLH'){

  conn <- conn_nsim();
  var_version <-nsim_version_getCurrentVersion(brand,'nsdict');
  sql <- paste("select FAnswerId,FCategory from answ_category_version
where fbrand ='",brand,"' and fversiontxt ='",var_version,"'
order by  FAnswerId
",sep="")
  res <- sql_select(conn,sql)
  return(res)
}

