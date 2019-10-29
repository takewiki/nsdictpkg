#' 数据结果的输出
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @import dplyr
#' @export
#'
#' @examples
#' nsim_output();
#'
nsim_output <- function(brand='JBLH'){

  # 获取标题问题
  data_standard <-ques_combine_ByAnsw_query_standard(brand);
  names(data_standard) <-c('FQues_std','FAnsw_std','FId');

  #获取相应问题
  data_like <-ques_combine_ByAnsw_query_like(brand);
  names(data_like) <-c('FQues_like','FAnsw_like','FId');
  data_detail <-left_join(data_standard,data_like,by='FId');
  data_detail$FQues_like <- tsdo::na_values(data_detail$FQues_like,data_detail$FQues_std);
  #获取答案分类
  data_category <-answ_category_query_current(brand);
  names(data_category) <-c('FId','FCategory');
  res<- merge(data_detail,data_category)
  #针对标题问题进行排序，然后取答案的第一个
  res <- res[order(res$FQues_std),];
  #然后处理
  col_names <-names(res);
  res <-split(res,res$FQues_std);
  res <- lapply(res, nsim_find_standardQues);
  res <- do.call('rbind',res);
  res <- res[res$FStandard ==1L,col_names]
  return(res);
}


#' 处理结果的输出方式
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_output_download();
nsim_output_download <- function(brand ='JBLH',each_page=7000L,skip_row=2L,file_name='output.res.paging.20191023.xlsx'){
  data_template <-tsda::nsim_data_tpl()
  res <- nsim_output(brand);
  row_count <- nrow(res);
  A <- paste("捷豹路虎",tsdo::left(as.character(Sys.Date()),7),res$FCategory,sep="/");
  B <- res$FQues_std;
  C <- res$FQues_like;
  D <- res$FAnsw_std;
  E <- rep("1",row_count);
  F <-rep("",row_count);
  G <-rep("",row_count);
  H <- rep("1",row_count);
  I <-rep("",row_count);
  res_formatted <- data.frame(A,B,C,D,E,F,G,H,I,stringsAsFactors = FALSE);
  res <- rbind(data_template,res_formatted);
  res <- tsdo::df_paging(res,each_page,skip_row);
  openxlsx::write.xlsx(res,file_name)

}



#' 处理结果的输出方式
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_output_download();
nsim_output_download_test <- function(brand ='JBLH',each_page=3000L,skip_row=2L,file_name='output.res.paging.20191023T.xlsx'){
  data_template <-tsda::nsim_data_tpl()
  res <- nsim_output(brand);
  row_count <- nrow(res);
  A <- paste("捷豹路虎",tsdo::left(as.character(Sys.Date()),7),'未分类',sep="/");
  B <- res$FQues_std;
  C <- res$FQues_like;
  D <- res$FAnsw_std;
  E <- rep("1",row_count);
  F <-rep("",row_count);
  G <-rep("",row_count);
  H <- rep("1",row_count);
  I <-rep("",row_count);
  res_formatted <- data.frame(A,B,C,D,E,F,G,H,I,stringsAsFactors = FALSE);
  res <- rbind(data_template,res_formatted);
  res <- tsdo::df_paging(res,each_page,skip_row);
  openxlsx::write.xlsx(res,file_name)

}

