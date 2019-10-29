#处理词频，不匹版本


#' 处理问题的数据
#'
#' @param brand  品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_item_ques_count();
nsim_item_ques_count <- function(brand='JBLH'){
  conn <- conn_nsim();
  sql <- paste("select FQuestionId,Ques_Count from vw_qalist_ques_count
where FBrand = '",brand,"'",sep="")
  res <- sql_select(conn,sql);
  return(res)
}


#' 处理当前版本的问题频率
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_item_ques_count_currentVersion();
nsim_item_ques_count_currentVersion <- function(brand='JBLH'){
  conn <- conn_nsim();
  sql <- paste("select FQuestionId,count(1) as ques_count from qalist_ID
where FQuestionId  in
(
select a.FQuestionId  from item_question_version_bl   a
inner join nsim_version v
on a.FVersionTxt= v.FVersionTxt
and a.FBrand = v.FBrand
inner join nsim_version v2
on a.FBL_versionTxt = v2.FVersionTxt
where v.FCurrentVersion =1 and v.FType ='nscl'
and v2.FCurrentVersion =1 and v2.FType ='nsbl'
and v.FBrand = '",brand,"'
)
group by FQuestionId",sep="")
  res <- sql_select(conn,sql);
  return(res);

}

#处理答案的在原始语料的词频率--------
#' 处理答案的在原始语料的词频率
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @import tsda
#' @import tsdo
#' @export
#'
#' @examples
#' nsim_item_answ_count();
nsim_item_answ_count <- function(brand='JBLH'){
  conn <- conn_nsim();
  sql <- paste("select   FAnswerId,Answ_Count  from vw_qalist_answ_count
where FBrand = '",brand,"'",sep="")
  res <- sql_select(conn,sql);
  return(res)
}



#' 获取数据的当前版本
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_item_answ_count_currentVersion();
nsim_item_answ_count_currentVersion <- function(brand='JBLH'){
  conn <- conn_nsim();
  sql <-paste("select b.FAnswerId,count(1) as answ_count  from qalist_ID b
where FAnswerId in
(
select  a.FAnswerId from item_answer_version_bl   a
inner join nsim_version v
on a.FVersionTxt= v.FVersionTxt
and a.FBrand = v.FBrand
inner join nsim_version v2
on a.FBL_versionTxt = v2.FVersionTxt
where v.FCurrentVersion =1 and v.FType ='nscl'
and v2.FCurrentVersion =1 and v2.FType ='nsbl'
and v.FBrand = '",brand,"')
group by b.FAnswerId
",sep="")
  res <- sql_select(conn,sql);
  return(res)
}



#查询问题经过nsbl与nscl后的当前版本-------
#' 查询问题经过nsbl与nscl后的当前版本
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_item_ques_currentVersion();
nsim_item_ques_currentVersion <- function(brand='JBLH'){
  conn <- conn_nsim();
  sql <- paste("select a.FQuestionId,a.FQuestion  from item_question_version_bl   a
inner join nsim_version v
on a.FVersionTxt= v.FVersionTxt
and a.FBrand = v.FBrand
inner join nsim_version v2
on a.FBL_versionTxt = v2.FVersionTxt
where v.FCurrentVersion =1 and v.FType ='nscl'
and v2.FCurrentVersion =1 and v2.FType ='nsbl'
and v.FBrand = '",brand,"'
",sep="");
  res <- sql_select(conn,sql)
  return(res)

}

#' 处理当前版本经过nscl与nsbl后的版本内容
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_item_answ_currentVersion();
nsim_item_answ_currentVersion <- function(brand='JBLH'){
  conn <- conn_nsim();
  sql <- paste("select  a.FAnswerId,a.FAnswer  from item_answer_version_bl   a
inner join nsim_version v
on a.FVersionTxt= v.FVersionTxt
and a.FBrand = v.FBrand
inner join nsim_version v2
on a.FBL_versionTxt = v2.FVersionTxt
where v.FCurrentVersion =1 and v.FType ='nscl'
and v2.FCurrentVersion =1 and v2.FType ='nsbl'
and v.FBrand = '",brand,"'",sep="")
  res <- sql_select(conn,sql)
  return(res)
}


#提取问答的最新版本，并进行文件字化处理-------
#' 提前当前版本
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_qalist_fullTxt_current();
nsim_qalist_fullTxt_current <- function(brand='JBLH'){
  conn <- conn_nsim();
  sql <- paste("select distinct qa.FQuestionId,qc.FQuestion,qa.FAnswerId,ac.FAnswer,qa.FBrand from qalist_ID qa
inner join
(
select  a.FAnswerId , a.FAnswer,a.FBrand from item_answer_version_bl   a
inner join nsim_version v
on a.FVersionTxt= v.FVersionTxt
and a.FBrand = v.FBrand
inner join nsim_version v2
on a.FBL_versionTxt = v2.FVersionTxt
where v.FCurrentVersion =1 and v.FType ='nscl'
and v2.FCurrentVersion =1 and v2.FType ='nsbl'
and v.FBrand = '",brand,"') ac
on qa.FAnswerId = ac.FAnswerId  and
qa.FBrand =ac.FBrand
inner join
 (
 select a.FQuestionId,a.FQuestion,a.FBrand from item_question_version_bl   a
inner join nsim_version v
on a.FVersionTxt= v.FVersionTxt
and a.FBrand = v.FBrand
inner join nsim_version v2
on a.FBL_versionTxt = v2.FVersionTxt
where v.FCurrentVersion =1 and v.FType ='nscl'
and v2.FCurrentVersion =1 and v2.FType ='nsbl'
and v.FBrand = '",brand,"'
 )  qc
 on qa.FQuestionId = qc.FQuestionId
 and qa.FBrand = qc.FBrand
",sep="")
               res <- sql_select(conn,sql)
               return(res);

}
