#1.处理分词通用处理方式------
#' 处理分词通用处理方式
#'
#' @param id ID
#' @param txt 内容将被进行分词处理
#' @param brand 品牌
#' @param version 版本
#' @param table_name 数据库名称
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_segment_version();
nsim_segment_version <- function(id,txt,brand,version,table_name){
  library(Rwordseg);
  data <- nsim_dict_FWord(brand,unique = TRUE);
  Rwordseg::insertWords(data);
  id <-as.character(id);
  txt_seg <-Rwordseg::segmentCN(txt,nosymbol = T)
  names(txt_seg) <-id;
  seg_count <- length(txt_seg);
  res <- tsdo::list_init(seg_count);
  for ( i in 1:seg_count){
    item <- txt_seg[[i]];
    #每个元素的数量
    item_count <- length(item);
    item_name <- id[i];
    #需要进行处理，将它变为整数型
    FId <- rep(as.integer(item_name),item_count);
    FEntryId <-1:item_count;
    FSegment <- item;
    FNSOwn <- rep(0,item_count);
    item_table <-data.frame(FId,FEntryId,FSegment,FNSOwn,stringsAsFactors = F)
    res[[i]] <- item_table

  }
  res<-do.call('rbind',res);
  res$FVersionTxt <- rep(version,nrow(res));
  res$FBrand <- rep(brand,nrow(res));
  nsim_save(res,table_name);
  #更新标志----
  sql <- paste("update  a set a.FNSOwn = 1  from  ",table_name,"  a
inner join dict b
on a.FSegment = b.FWord  and a.FBrand = b.FBrand where a.FVersionTxt = '",version,"'",sep="")
  sql_update(conn_nsim(),sql)
}
#' 处理问题分类
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_ques_seg();
nsim_ques_segment_version <- function(brand = 'JBLH',version='V1'){
  data_ques <- nsim_item_ques_currentVersion(brand);
  id <-as.character(data_ques$FQuestionId);
  txt <-data_ques$FQuestion;
  nsim_segment_version(id,txt,brand,version,'ques_segment_version')

}



#' 处理答案分词问题
#'
#' @param brand 品牌
#' @param version 版本
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_answ_segment_version();
nsim_answ_segment_version <- function(brand = 'JBLH',version='V1'){

  data_answ <- nsim_item_answ_currentVersion(brand)
  id <-as.character(data_answ$FAnswerId);
  txt <-data_answ$FAnswer;
  nsim_segment_version(id,txt,brand,version,'answ_segment_version')

}
# 处理网商分词情况问题及答案整合在一起-------
#' 处理网商分词情况
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_nsdict_version();
nsim_nsdict_version <- function(brand='JBLH'){
  #获取版本
  var_version <- nsim_version_getNextVersion(brand,'nsdict');
  #处理问题分词
  nsim_ques_segment_version(brand,var_version);
  #处理答案分词
  nsim_answ_segment_version(brand,var_version);
  #更新版本
  nsim_version_setCurrentVersion(brand,'nsdict',var_version);

}






