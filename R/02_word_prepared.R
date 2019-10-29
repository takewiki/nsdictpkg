#' 处理词汇查询
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_dict();
nsim_dict <- function(brand='JBLH'){
  str_brand <- paste(" FBrand = '",brand,"' ",sep="");
  res <-nsim_read_where('dict',field_vars = c('FWord','FCategory'),
                        where =str_brand )
  return(res)
}

#' 处理到词汇
#'
#' @param brand 品牌
#' @param unique 是否唯一
#'
#' @return 返回值
#' @export
#'
#' @examples
#'  nsim_dict_FWord()
nsim_dict_FWord <- function(brand = 'JBLH',unique=FALSE){

  res <- nsim_brand_table_query2(table_name = 'dict',
                                 field_name = 'FWord',
                                 brand,unique

  )
  return(res)

}

#' 读取分词的类别
#'
#' @param brand 品牌
#' @param unique 唯一
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_dict_category();
nsim_dict_category <- function(brand = 'JBLH',unique=TRUE){

  res <- nsim_brand_table_query2(table_name = 'dict',
                                 field_name = 'FCategory',
                                 brand,unique

  )
  return(res)

}
