#' 查询标准问题
#'
#' @param data  数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_find_standardQues()
nsim_find_standardQues <- function(data){
  ncount <- nrow(data);
  data$FStandard <- rep(0L,ncount);
  data[1,'FStandard'] <-1L;
  return(data);
}


#' 处理名称
#'
#' @param data 数据
#'
#' @return 返回值'
#' @import tsda
#' @import tsdo
#' @export
#'
#' @examples
#' nsim_aux_combine_ques();
nsim_aux_combine_ques <- function(data){
  FQuestion <-vect_as_dbl_equal(data$FQuestion)
  FAnswer <- data$FAnswer[1]
  FAnswerId <-data$FAnswerId[1]
  res <- data.frame(FQuestion,FAnswer,FAnswerId,stringsAsFactors = F)
  return(res)

}
#提取所有问题-------
#' 提供标准问题,进行版本化存储
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' ques_getStandarOne();
#'
#'
ques_combine_ByAnsw <- function(brand='JBLH',version='V1'){
  data <- nsim_qalist_fullTxt_current(brand = brand);
  ques_count <-nsim_item_ques_count_currentVersion(brand);
  res <- merge(data,ques_count);
  res <- res[order(res$FAnswerId,-res$ques_count),]
  #针对数据进行分类处理；
  FAnswerId_str <- as.character(res$FAnswerId);
  res2 <- split(res,FAnswerId_str);
  res2 <- lapply(res2, nsim_find_standardQues);
  res2 <-do.call('rbind',res2);
  # var_version <- nsim_version_getCurrentVersion(brand,'nsdict');
  res2$FVersionTxt <- rep(version,nrow(res2));
  rownames(res2) <-NULL
  #return(res2)
  nsim_save(res2,'qalist_ques_combined_by_answer');

}

#提取标准问题-----
#' 查询
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' ques_combine_ByAnsw_query_standard();
ques_combine_ByAnsw_query_standard <- function(brand='JBLH') {
  conn <- conn_nsim();
  var_version <-nsim_version_getCurrentVersion(brand,'nsdict');
  sql <- paste("select FQuestion,FAnswer,FAnswerId from qalist_ques_combined_by_answer
where fbrand ='",brand,"' and FStandard =1 and FVersionTxt='",var_version,"'",sep="");
  res <- sql_select(conn,sql);
  return(res);

}


#'处理问题合并，生成类似问题
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' ques_combine_ByAnsw_query_like();
ques_combine_ByAnsw_query_like <-function(brand='JBLH') {
  conn <- conn_nsim();
  var_version <-nsim_version_getCurrentVersion(brand,'nsdict');
  sql <- paste("select FQuestion,FAnswer,FAnswerId from qalist_ques_combined_by_answer
where fbrand ='",brand,"' and FStandard = 0 and FVersionTxt='",var_version,"'",sep="");
  res <- sql_select(conn,sql);
  FAnswerId_str <- as.character(res$FAnswerId)
  res2 <- split(res,FAnswerId_str);
  res2 <- lapply(res2,nsim_aux_combine_ques)
  res2 <- do.call('rbind',res2);
  return(res2);

}



