#----------------------------------------------------------------------------------------------------------#

## 분석데이터 호출
load(file = paste0(data_folder_route,'GREE_DATA.RData'))  # src_gree로 불러와짐
src_gree <- src_gree[order(src_gree$SEGID,src_gree$STD_YYMM),]  # STD_YYMM = MTBCO_PMT_YM = 지급년월 / SEGID와 지급년월을 기준으로 데이터 정렬
rownames(src_gree) <- 1:nrow(src_gree)

## 영업일수 호출
src_workingday = read.csv(file = paste0(data_folder_route,"영업일수(FIN).csv"), stringsAsFactors = FALSE)

## 분석 수행시점 지정 (=기준년월)
STD_YYMM <- REG_YYMM  # 기준년월 지정
STD_YYMM_CHA = as.character(STD_YYMM)  # 기준년월 문자변수 생성

# ## 분석시점을 기준으로 데이터 끊어오기
# src_gree <- src_gree[src_gree$STD_YYMM <= REG_YYMM,]

#----------------------------------------------------------------------------------------------------------#

## 항목별 실적 쌓이기 시작한 시점 생성 (=최초년월)
gree_startmm = sqldf("SELECT GREE_DCLF_CD, MIN(STD_YYMM) AS YYMM_MIN  
                         FROM  src_gree 
                         GROUP BY GREE_DCLF_CD" )

gree_2 = sqldf("SELECT A.*, B.YYMM_MIN
                     FROM src_gree AS A
                       LEFT JOIN gree_startmm AS B
                         ON A.GREE_DCLF_CD = B.GREE_DCLF_CD ")

## 항목별 근무일수 정보 추가 
gree_2 = sqldf("SELECT A.*, B.BUSINESS_DAYS
                    FROM gree_2 AS A
                      LEFT JOIN src_workingday  AS B ON  A.STD_YYMM = B.YYMM")

#----------------------------------------------------------------------------------------------------------#

## 최초시작 후 경과개월수 산출

## 기준일자 생성
gree_2$STD_DT = as.Date(paste(substr(gree_2$STD_YYMM, 1, 4),substr(gree_2$STD_YYMM, 5, 6), '01', sep='-'))
gree_2$START_DT = as.Date(paste(substr(gree_2$YYMM_MIN, 1, 4),substr(gree_2$YYMM_MIN, 5, 6), '01', sep='-'))
gree_2$BASE_DT = as.Date(paste(substr(STD_YYMM_CHA, 1, 4),substr(STD_YYMM_CHA, 5, 6), '01', sep='-'))

## 경과개월수 생성
gree_2$START_NO = as.numeric((gree_2$STD_DT - gree_2$START_DT)/30) + 1
gree_2$BASE_NO = as.numeric((gree_2$BASE_DT - gree_2$START_DT)/30) + 1

#----------------------------------------------------------------------------------------------------------#

# 음수값 처리 
gree_2$PRSP_VL = ifelse(gree_2$PRSP_VL <= 0 , 0, gree_2$PRSP_VL)

#----------------------------------------------------------------------------------------------------------#

## 세분류 항목별 작업 단위 지정
gree_list = sqldf("SELECT GREE_DCLF_CD, count(distinct STD_YYMM) as MM_CNT
                         FROM  gree_2  
                              where start_no < base_no
                          GROUP BY GREE_DCLF_CD")

gree_list$MODEL_GB = ifelse(gree_list$MM_CNT  < 3 , 'GR0',
                              ifelse(gree_list$MM_CNT < 7 , 'GR1',  
                                     ifelse(gree_list$MM_CNT <= 27 , 'GR2',  'GR3')))


#----------------------------------------------------------------------------------------------------------#

## 전망 KEY (= 전망시점(전망년월)) 생성
STD_YY = as.numeric(substr(STD_YYMM_CHA, 1, 4))
STD_YYYY = c(0:10)
STD_YYYY = STD_YYYY + STD_YY
STD_YYYY = as.data.frame(STD_YYYY)
STD_MM = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

TARGET_YM = merge(STD_YYYY, STD_MM)
TARGET_YM$STD_YM = paste(TARGET_YM$STD_YYYY, TARGET_YM$y , sep='' )
TARGET_YM = subset(TARGET_YM, STD_YM >= STD_YYMM )
TARGET_YM = sqldf("SELECT STD_YM  FROM TARGET_YM  ORDER BY STD_YM")
T_STD_YM = paste(TARGET_YM$STD_YYYY, TARGET_YM$y , sep='' )

# WORKDAY_DB = subset(src_workingday, src_workingday$YYMM >= STD_YYMM , select = 'BUSINESS_DAYS' )
## TSLM 예측을 위한 근무일수 데이터 테이블 생성 : 항목별 원천 데이터와 동일 구조 생성후 RBIND 처리함
WORKDAY_DB = subset(src_workingday, src_workingday$YYMM >= STD_YYMM)    # 전망시점(=STD_YYMM) 이후 시점의 근무일수 추출
WORKDAY_DB$SEGID = ''
WORKDAY_DB$STD_DT = as.Date(paste(substr(WORKDAY_DB$YYMM, 1, 4), substr(WORKDAY_DB$YYMM, 5, 6), '01', sep='-' ))
WORKDAY_DB$PRSP_VL = 0
WORKDAY_DB2 = sqldf("SELECT SEGID , STD_DT, PRSP_VL, BUSINESS_DAYS  FROM WORKDAY_DB")

## 시계열 분석을 위한 분석시점 및 예측종료시점의 (년/월) 생성
# 분석시점 : BASE_YEAR & BASE_MM & BASE_DATE / 예측종료시점 : END_YEAR & END_MM
BASE_YEAR = as.numeric(substr(STD_YYMM_CHA, 1, 4))
BASE_MM= as.numeric(substr(STD_YYMM_CHA, 5, 6))
BASE_DATE = as.Date(paste(substr(STD_YYMM_CHA, 1, 4), substr(STD_YYMM_CHA, 5, 6), '01', sep = '-'))

END_YEAR = BASE_YEAR + 9
END_MM = BASE_MM

WORKDAY_OUT = subset(WORKDAY_DB2  , STD_DT >= BASE_DATE, select = 'BUSINESS_DAYS')

#----------------------------------------------------------------------------------------------------------#

############################################################################################################
## STEP1 : 세분류 기준 전망값 생성
############################################################################################################

## 1. 세분류 기준 분석데이터 생성
gree_tot  = sqldf("SELECT GREE_DCLF_CD
                        , STD_YYMM
                        , SUM(PRSP_VL) AS AMT
                        , MAX(BUSINESS_DAYS) AS DAYS
                        , MAX(STD_DT) AS STD_DT
                        , MAX(START_DT) AS START_DT
                        , MAX(BASE_DT)  AS BASE_DT
                        , MAX(START_NO) AS START_NO
                        , MAX(BASE_NO)  AS BASE_NO
                    FROM gree_2 AS A
                      GROUP BY GREE_DCLF_CD , STD_YYMM")



## 2. 세분류 기준 전망값 생성

## GR0 : 2개월 이하 전망 :  평균값 적용  ==================================================================

s1_gr0_totout = sqldf("select '1' as GREE_DCLF_CD
                       , '1' as STD_DT 
                       , '1' as AMT   " )

s1_gr0_db = sqldf(" SELECT A.GREE_DCLF_CD  
                        , A.STD_YYMM AS STD_DT
                       , SUM(A.PRSP_VL) AS AMT 
                  FROM gree_2 as a, gree_list as b 
                      WHERE  A.GREE_DCLF_CD = B.GREE_DCLF_CD  AND B.MODEL_GB = 'GR0' 
                             AND A.START_NO < 3 AND A.START_NO > 0 AND A.BASE_NO - A.START_NO < 3
                      GROUP BY A.GREE_DCLF_CD, A.STD_YYMM ")

s1_gr0_out = rbind(s1_gr0_totout , s1_gr0_db)

s1_gr0_out2 = sqldf("select GREE_DCLF_CD, avg(AMT) as AMT 
                       FROM s1_gr0_out 
                         WHERE STD_DT > 10 ")

s1_gr0_out_f = sqldf("select a.GREE_DCLF_CD,  b.STD_YM as STD_YYMM, a.AMT 
                       FROM s1_gr0_out2  as a, TARGET_YM as b 
                        where a.AMT > 0")



## GR1 : 6개월 이하 전망 :  3개월 ~ 6개월 평균값 적용  ====================================================

## GR1 : 6개월 미만 전망 :  평균값으로 전망

s1_gr1_totout = sqldf("select '1' as GREE_DCLF_CD
                       , '1' as STD_DT 
                       , '1' as AMT   " )

s1_gr1_db = sqldf(" SELECT A.GREE_DCLF_CD  
                        , A.STD_YYMM
                        , A.START_NO
                       , SUM(A.PRSP_VL) AS AMT 
                  FROM gree_2 as a, gree_list as b 
                      WHERE  A.GREE_DCLF_CD = B.GREE_DCLF_CD AND A.START_NO < A.BASE_NO AND A.START_NO > 2.5
                             AND B.MODEL_GB = 'GR1' 
                      GROUP BY A.GREE_DCLF_CD, A.STD_YYMM , A.START_NO ")

s1_gr1_out = sqldf(" SELECT GREE_DCLF_CD  
                       , avg(AMT) AS AMT 
                  FROM s1_gr1_db
                      group by GREE_DCLF_CD  ")


s1_gr1_out_f = sqldf("select a.GREE_DCLF_CD,  b.STD_YM as STD_YYMM, a.AMT 
                       FROM s1_gr1_out  as a, TARGET_YM as b 
                        where a.AMT > 0")



## GR2 : 7~27개월 미만 전망 :  TSLM     ==================================================================

gr2_item = subset(gree_list, gree_list$MODEL_GB == 'GR2', select = 'GREE_DCLF_CD' )


## 근무일수 데이터 tslm 생성 : 항목별 원천 데이터와 동일 구조 생성후 RBIND 처리함
workday_db = subset(src_workingday, src_workingday$YYMM >= STD_YYMM, select = c('YYMM', 'BUSINESS_DAYS' ))
workday = sqldf("select BUSINESS_DAYS as DAYS from workday_db")


## 전망결과 누적 테이블 생성 
s1_gr2_totout = sqldf("select '1' as GREE_DCLF_CD
                       , '1' as STD_DT 
                       , '1' as AMT   " )


for (i in 1:nrow(gr2_item))  {
  
  item = gr2_item$GREE_DCLF_CD[i]
  s1_gr2_dbin = subset(gree_tot, GREE_DCLF_CD == item & START_NO > 2.5 & START_NO < BASE_NO ,
                       select = c('STD_YYMM', 'STD_DT', 'AMT', 'DAYS'))
  
  s1_gr2_dbin = subset(gree_tot, GREE_DCLF_CD == item ,
                       select = c('STD_YYMM', 'STD_DT', 'AMT', 'DAYS'))
  
  s1_gr2_dbin = s1_gr2_dbin[order(s1_gr2_dbin$STD_DT), ]
  
  # ts 변환을 위한 최초시작 년월 생성 
  start_ym = min(s1_gr2_dbin$STD_YYMM)
  st_year = as.numeric(substr(start_ym, 1, 4))
  st_mm =  as.numeric(substr(start_ym, 6, 7))
  
  # ts 변환
  s1_gr2_ts = ts(s1_gr2_dbin, start = c(st_year, st_mm), frequency = 12)
  
  # ts model 생성
  s1_gr2_md = tslm(AMT ~ trend + DAYS  , data = s1_gr2_ts)
  summary(s1_gr2_md)
  
  # 전망값 생성
  s1_gr2_out = as.data.frame(forecast(s1_gr2_md, newdata = workday))
  s1_gr2_out2 = cbind(item, workday_db$YYMM, s1_gr2_out[, 1] )
  colnames(s1_gr2_out2) = c('GREE_DCLF_CD', 'STD_DT', 'AMT')
  
  # 전망누적값 생성 
  s1_gr2_totout = rbind(s1_gr2_totout,s1_gr2_out2 )
}

s1_gr2_out_f = subset(s1_gr2_totout , STD_DT > 10)



## GR3 : 27개월 이상 전망 :  TSLM     =======================================================

gr3_item = subset(gree_list, gree_list$MODEL_GB == 'GR3', select = 'GREE_DCLF_CD' )
gr3_item_cnt = nrow(gr3_item)


if(gr3_item_cnt> 0){
  
  ## 근무일수 데이터 tslm 생성 : 항목별 원천 데이터와 동일 구조 생성후 RBIND 처리함
  workday_db = subset(src_workingday, src_workingday$YYMM >= STD_YYMM, select = c('YYMM', 'BUSINESS_DAYS' ))
  workday = sqldf("select BUSINESS_DAYS as DAYS from workday_db")
  
  ## 전망결과 누적 테이블 생성 
  s1_gr3_totout = sqldf("select '1' as GREE_DCLF_CD
                       , '1' as STD_DT 
                       , '1' as AMT   " )
  
  
  ## 데이터 분할 
  ## 최근 3개월 데이터 
  # 1) 최근 3개월 실적 검증용 데이터 : 기준 시점 3개월 이내 
  s1_gr3_db1 = sqldf("select a.GREE_DCLF_CD, a.STD_YYMM, a.STD_DT, a.AMT, a.DAYS
                      from gree_tot as a, gr3_item as b 
                      where a.GREE_DCLF_CD = b.GREE_DCLF_CD and a.START_NO >= a.BASE_NO - 3 and  a.START_NO < a.BASE_NO ")
  
  
  # 2) 검증용 모형 개발 셋 : 기준 시점 3개월 제외한 데이터 실적 
  s1_gr3_db2 = sqldf("select a.GREE_DCLF_CD, a.STD_YYMM, a.STD_DT, a.AMT, a.DAYS
                      from gree_tot as a, gr3_item as b 
                      where a.GREE_DCLF_CD = b.GREE_DCLF_CD and a.START_NO < a.BASE_NO - 3 ")
  
  
  # 3) 예측용 모형 개발 셋 : 최초 2개월 제외한 실적 
  s1_gr3_db3 = sqldf("select a.GREE_DCLF_CD, a.STD_YYMM, a.STD_DT, a.AMT, a.DAYS
                      from gree_tot as a, gr3_item as b 
                      where a.GREE_DCLF_CD = b.GREE_DCLF_CD and a.START_NO < a.BASE_NO and a.START_NO > 2.5  ")
  
  
  # 4) 모형검증을 위한 최근 3개월 working day
  workday_m3 =subset(s1_gr3_db1, select = c('GREE_DCLF_CD', 'DAYS'))
  
  
  ## 전망결과 누적 테이블 생성 
  s1_gr3_totout = sqldf("select '1' as GREE_DCLF_CD
                       , '1' as STD_DT 
                       , '1' as AMT   " )
  
  
  ## 검증용 모형 생성 
  for (i in 1:nrow(gr3_item))  {
    
    ## TSLM ============================================
    
    item = gr3_item$GREE_DCLF_CD[i]
    s1_gr3_dbin = subset(s1_gr3_db2, GREE_DCLF_CD == item , 
                         select = c('STD_YYMM', 'STD_DT', 'AMT', 'DAYS'))
    
    s1_gr3_dbin = s1_gr3_dbin[order(s1_gr3_dbin$STD_DT), ]
    
    # ts 변환을 위한 최초시작 년월 생성 
    start_ym = min(s1_gr3_dbin$STD_YYMM)
    st_year = as.numeric(substr(start_ym, 1, 4))
    st_mm =  as.numeric(substr(start_ym, 6, 7))
    
    # ts 변환
    s1_gr3_ts = ts(s1_gr3_dbin, start = c(st_year, st_mm), frequency = 12)
    
    # ts model 생성
    s1_gr3_md = tslm(AMT ~ trend + season + DAYS  , data = s1_gr3_ts)
    
    # 전망값 생성
    workday_test = subset(workday_m3, GREE_DCLF_CD == item, select = 'DAYS' )
    s1_gr3_out = as.data.frame(forecast(s1_gr3_md, newdata = workday_test))
    colnames(s1_gr3_out) = c('AMT', 'V1', 'V2', 'V3', 'V4')
    s1_gr3_out$GREE_DCLF_CD = item
    
    s1_gr3_out2 = subset(s1_gr3_out , select = c('GREE_DCLF_CD', 'AMT'))
    
    # 전망누적값 생성 
    # s1_gr2_totout = rbind(s1_gr2_totout,s1_gr2_out2 )
    
    
    ## 지수평활 ============================================
    s1_gr3_ts_ty2 = ts(s1_gr3_dbin$AMT, start = c(st_year, st_mm), frequency = 12)
    
    s1_gr3_md_ty2  = HoltWinters(s1_gr3_ts2)
    
    s1_gr3_out_ty2 = as.data.frame(forecast(s1_gr3_md_ty2, h=3))
    colnames(s1_gr3_out_ty2) = c('AMT', 'V1', 'V2', 'V3', 'V4')
    s1_gr3_out_ty2$GREE_DCLF_CD = item
    s1_gr3_out_ty2_02 = subset(s1_gr3_out_ty2 , select = c('GREE_DCLF_CD', 'AMT'))
    
    
    ## 오차비교 ===========================================
    s1_gr3_err = subset(s1_gr3_db1, GREE_DCLF_CD == item)
    s1_gr3_err$MD1 = s1_gr3_out2$AMT
    s1_gr3_err$MD2 = s1_gr3_out_ty2_02$AMT
    s1_gr3_err$TY1_ERR = abs( s1_gr3_err$MD1 -  s1_gr3_err$AMT)/ s1_gr3_err$AMT 
    s1_gr3_err$TY2_ERR = abs( s1_gr3_err$MD2 -  s1_gr3_err$AMT)/ s1_gr3_err$AMT 
    
    s1_gr3_err2 = sqldf("select GREE_DCLF_CD 
                              , avg(TY1_ERR) as TY1_ERR
                              , avg(TY2_ERR) as TY2_ERR
                              , case when avg(TY1_ERR) <= avg(TY2_ERR) then 1 else 2 end as MD_TYPE
                         from s1_gr3_err
                          group by GREE_DCLF_CD ")
    
    
    #### 전체 기간 전망값 생성    =============================================================
    
    ## TSLM ============================================
    
    item = gr3_item$GREE_DCLF_CD[i]
    s1_gr3_dbin = subset(s1_gr3_db3, GREE_DCLF_CD == item , 
                         select = c('STD_YYMM', 'STD_DT', 'AMT', 'DAYS'))
    
    s1_gr3_dbin = s1_gr3_dbin[order(s1_gr3_dbin$STD_DT), ]
    
    # ts 변환을 위한 최초시작 년월 생성 
    start_ym = min(s1_gr3_dbin$STD_YYMM)
    st_year = as.numeric(substr(start_ym, 1, 4))
    st_mm =  as.numeric(substr(start_ym, 6, 7))
    
    # ts 변환
    s1_gr3_ts = ts(s1_gr3_dbin, start = c(st_year, st_mm), frequency = 12)
    
    # ts model 생성
    s1_gr3_md = tslm(AMT ~ trend + season + DAYS  , data = s1_gr3_ts)
    
    # 전망값 생성
    s1_gr3_out = as.data.frame(forecast(s1_gr3_md, newdata = workday))
    colnames(s1_gr3_out) = c('AMT', 'V1', 'V2', 'V3', 'V4')
    s1_gr3_out$YM  = rownames(s1_gr3_out)
    s1_gr3_out$GREE_DCLF_CD = item
    
    s1_gr3_out2 = subset(s1_gr3_out , select = c('GREE_DCLF_CD', 'AMT', 'YM'))
    s1_gr3_out2$YYMM = workday_db$YYMM
    
    
    ## 지수평활 ============================================
    
    s1_gr3_ts_ty2 = ts(s1_gr3_dbin$AMT, start = c(st_year, st_mm), frequency = 12)
    
    s1_gr3_md_ty2  = HoltWinters(s1_gr3_ts_ty2)
    
    s1_gr3_out_ty2 = as.data.frame(forecast(s1_gr3_md_ty2, h=120))
    colnames(s1_gr3_out_ty2) = c('AMT', 'V1', 'V2', 'V3', 'V4')
    
    s1_gr3_out_ty2$YM = rownames(s1_gr3_out_ty2)
    s1_gr3_out_ty2$GREE_DCLF_CD = item
    
    s1_gr3_out_ty2_02 = subset(s1_gr3_out_ty2 , select = c('GREE_DCLF_CD', 'AMT', 'YM'))
    
    
    ## 예측결과 합치기
    s1_gr3_out_all = sqldf("select a.GREE_DCLF_CD, a.YYMM, a.AMT as TY1_AMT 
                                , b.AMT as TY2_AMT
                            from s1_gr3_out2 as a, s1_gr3_out_ty2_02 as b 
                              where a.GREE_DCLF_CD = b.GREE_DCLF_CD  and a.YM = b.YM ")
    
    s1_gr3_out_all_02 = sqldf("select a.* , b.MD_TYPE   
                              from s1_gr3_out_all as a, s1_gr3_err2 as b")
    
    s1_gr3_out_f = sqldf(" select GREE_DCLF_CD, YYMM as STD_DT
                            , case when MD_TYPE = 1 then  TY1_AMT else TY2_AMT end as AMT
                            from  s1_gr3_out_all_02   ")
    
    
    ## 전체항목 누적 전망값 생성 
    s1_gr3_totout = rbind(s1_gr3_totout , s1_gr3_out_f)
    
  }
  
  s1_gr3_out_f = subset(s1_gr3_totout , STD_DT > 10)
  
  
} else {
  
  s1_gr3_out_f  = sqldf("select '1' as GREE_DCLF_CD
                       , '1' as STD_DT 
                       , 1 as AMT   " )
}


###### 전체 전망값 통합 

# 2개월 이하   :  s1_gr0_out_f
# 3~6 개월     :  s1_gr1_out_f
# 7~26개월     :  s1_gr2_out_f    
# 27개월 이상  :  s1_gr3_out_f


s1_gr0_out_f$AMT =  as.numeric(s1_gr0_out_f$AMT)
s1_gr2_out_f$AMT =  as.numeric(s1_gr2_out_f$AMT)

## 이름 변환 
colnames(s1_gr2_out_f) = c('GREE_DCLF_CD', 'STD_YYMM', 'AMT')
colnames(s1_gr3_out_f) = c('GREE_DCLF_CD', 'STD_YYMM', 'AMT')

s1_pred_amt  = rbind(s1_gr0_out_f, s1_gr1_out_f, s1_gr2_out_f, s1_gr3_out_f)
s1_pred_amt = subset(s1_pred_amt , STD_YYMM > 10)


# 음수 0 으로 처리 
s1_pred_amt$AMT = as.numeric(s1_pred_amt$AMT)
s1_pred_amt$AMT = ifelse(s1_pred_amt$AMT < 0 , 0 , s1_pred_amt$AMT)



####################################################################################
## STEP2 : 상세모형 전망값 생성  
####################################################################################

## 1. 상세 SEG 기준 분석데이터 생성
gree_seg  = sqldf("SELECT GREE_DCLF_CD
                        , SEGID
                        , STD_YYMM
                        , SUM(PRSP_VL) AS AMT
                        , MAX(BUSINESS_DAYS) AS DAYS
                        , MAX(STD_DT) AS STD_DT
                        , MAX(START_DT) AS START_DT
                        , MAX(BASE_DT)  AS BASE_DT
                        , MAX(START_NO) AS START_NO
                        , MAX(BASE_NO)  AS BASE_NO
                    FROM gree_2 AS A
                      GROUP BY GREE_DCLF_CD, SEGID , STD_YYMM")


## 세분류 항목별 작업 단위 지정
gree_list_seg = sqldf("SELECT GREE_DCLF_CD, SEGID, count(distinct STD_YYMM) as MM_CNT
                         FROM  gree_2  
                              where start_no < base_no
                          GROUP BY GREE_DCLF_CD ,  SEGID")

gree_list_seg$MODEL_GB = ifelse(gree_list_seg$MM_CNT  < 3 , 'GR0',
                                  ifelse(gree_list_seg$MM_CNT < 7 , 'GR1',  
                                         ifelse(gree_list_seg$MM_CNT <= 27 , 'GR2',  'GR3')))


## item 진료비 금액 추가하여 seg별 비율 생성 
gree_seg_02  = sqldf(" select a.*, b.AMT as TOTAMT, a.AMT / b.AMT * 100 as SEG_AMTRT  
                            from gree_seg as a  
                               left join gree_tot as b
                                  on a.GREE_DCLF_CD = b.GREE_DCLF_CD and A.STD_YYMM = B.STD_YYMM")


## GR0 : 2개월 이하 전망 :  최근2개월 & 평균값 적용  ==================================================================

s2_gr0_db = sqldf(" SELECT A.SEGID
                        , A.STD_YYMM AS STD_YYMM
                       , avg(A.SEG_AMTRT) AS AMT 
                  FROM gree_seg_02 as a, gree_list_seg as b 
                      WHERE   A.SEGID = B.SEGID  AND B.MODEL_GB = 'GR0' 
                          AND A.START_NO < 3 AND A.START_NO > 0 AND A.BASE_NO - A.START_NO < 3
                      GROUP BY  A.SEGID, A.STD_YYMM ")


s2_gr0_out_f = sqldf("select SEGID, b.STD_YM as STD_YYMM, a.AMT 
                       FROM s2_gr0_db  as a, TARGET_YM as b 
                        where a.AMT > 0")



## GR1 : 6개월 미만 전망 :  평균값으로 전망   ===============================================

s2_gr1_db = sqldf(" SELECT A.SEGID
                        , A.STD_YYMM
                        , A.START_NO
                       , A.BASE_NO                        
                       , SUM(A.SEG_AMTRT) AS AMT 
                  FROM gree_seg_02 as a, gree_list_seg as b 
                      WHERE   A.SEGID = B.SEGID
                          AND A.START_NO < A.BASE_NO AND B.MODEL_GB = 'GR1' AND A.BASE_NO - A.START_NO < 7
                              AND A.START_NO > 2.5
                      GROUP BY  A.SEGID, A.STD_YYMM , A.START_NO ")


s2_gr1_out = sqldf(" SELECT SEGID
                       , SUM(AMT) AS AMT 
                       , MAX(BASE_NO) AS BASE_NO
                  FROM s2_gr1_db
                      group by SEGID  ")


## 데이터 실적 존재하는 개월수 산정한 평균값 생성
s2_gr1_out$AMTADD = ifelse(s2_gr1_out$BASE_NO > 6, s2_gr1_out$AMT/6 ,
                           ifelse(s2_gr1_out$BASE_NO > 5, s2_gr1_out$AMT/5  ,  
                                  ifelse(s2_gr1_out$BASE_NO > 4, s2_gr1_out$AMT/4  ,
                                         ifelse(s2_gr1_out$BASE_NO > 3, s2_gr1_out$AMT/3  ,  
                                                ifelse(s2_gr1_out$BASE_NO > 2, s2_gr1_out$AMT/2  ,  s2_gr1_out$AMT)))))

s2_gr1_out_f = sqldf("select A.SEGID ,  b.STD_YM as STD_YYMM, a.AMTADD AS AMT 
                       FROM s2_gr1_out  as a, TARGET_YM as b 
                        where a.AMT > 0")


## GR2 : 7~27개월 미만 전망 :  TSLM     =======================================================

gr2_seg = sqldf (" SELECT A.GREE_DCLF_CD, A.SEGID  
                         FROM gree_seg_02 as a, gree_list_seg AS B  
                         WHERE A.SEGID = B.SEGID  and b.MODEL_GB = 'GR2'
                          GROUP BY A.GREE_DCLF_CD, A.SEGID")


## 근무일수 데이터 tslm 생성 : 항목별 원천 데이터와 동일 구조 생성후 RBIND 처리함
workday_db = subset(src_workingday, src_workingday$YYMM >= STD_YYMM, select = c('YYMM', 'BUSINESS_DAYS' ))
workday = sqldf("select BUSINESS_DAYS as DAYS from workday_db")


## 전망결과 누적 테이블 생성 
s2_gr2_totout = sqldf("select '1' as SEGID
                       , '1' as STD_DT 
                       , '1' as AMT   " )


for (i in 1:nrow(gr2_seg))  {
  item = gr2_seg$SEGID[i]
  dclf = gr2_seg$GREE_DCLF_CD[i]
  
  s2_gr2_dbin = subset(gree_seg_02, SEGID == item & START_NO > 2.5 & START_NO < BASE_NO ,
                       select = c('STD_YYMM', 'STD_DT', 'AMT', 'SEG_AMTRT', 'DAYS'))
  
  s2_gr2_dbin = s2_gr2_dbin[order(s2_gr2_dbin$STD_DT), ]
  
  # ts 변환을 위한 최초시작 년월 생성 
  start_ym = min(s2_gr2_dbin$STD_YYMM)
  st_year = as.numeric(substr(start_ym, 1, 4))
  st_mm =  as.numeric(substr(start_ym, 6, 7))
  
  # ts 변환
  s2_gr2_ts = ts(s2_gr2_dbin, start = c(st_year, st_mm), frequency = 12)
  
  # ts model 생성
  s2_gr2_md = tslm(SEG_AMTRT ~ trend + DAYS  , data = s2_gr2_ts)
  
  # 전망값 생성
  s2_gr2_out = as.data.frame(forecast(s2_gr2_md, newdata = workday))
  s2_gr2_out2 = cbind(item, workday_db$YYMM, s2_gr2_out[, 1] )
  colnames(s2_gr2_out2) = c('SEGID', 'STD_DT', 'AMT')
  
  # 전망누적값 생성 
  s2_gr2_totout = rbind(s2_gr2_totout, s2_gr2_out2 )
}

s2_gr2_out_f = subset(s2_gr2_totout , STD_DT > 10)



## GR3 : 27개월 이상 전망 :  TSLM     =======================================================

gr3_seg = sqldf (" SELECT A.GREE_DCLF_CD, A.SEGID  
                         FROM gree_seg_02 as a, gree_list_seg AS B  
                         WHERE A.SEGID = B.SEGID  and b.MODEL_GB = 'GR3'
                          GROUP BY A.GREE_DCLF_CD, A.SEGID")

gr3_item = subset(gree_list_seg, gree_list_seg$MODEL_GB == 'GR3', select = 'SEGID' )

gr3_item_cnt = nrow(gr3_item)


if(gr3_item_cnt > 0){
  
  
  ## 근무일수 데이터 tslm 생성 : 항목별 원천 데이터와 동일 구조 생성후 RBIND 처리함
  workday_db = subset(src_workingday, src_workingday$YYMM >= STD_YYMM, select = c('YYMM', 'BUSINESS_DAYS' ))
  workday = sqldf("select BUSINESS_DAYS as DAYS from workday_db")
  
  
  ## 전망결과 누적 테이블 생성 
  s2_gr3_totout = sqldf("select '1' as SEGID
                       , '1' as STD_DT 
                       , '1' as AMT   " )
  
  
  ## 데이터 분할 
  ## 최근 3개월 데이터 
  # 1) 최근 3개월 실적 검증용 데이터 : 기준 시점 3개월 이내 
  s2_gr3_db1 = sqldf("select a.SEGID, a.STD_YYMM, a.STD_DT, a.SEG_AMTRT as AMT, a.DAYS
                      from gree_seg_02 as a, gr3_item as b 
                      where a.SEGID = b.SEGID and a.START_NO >= a.BASE_NO - 3 and  a.START_NO < a.BASE_NO ")
  
  
  # 2) 검증용 모형 개발 셋 : 기준 시점 3개월 제외한 데이터 실적 
  s2_gr3_db2 = sqldf("select a.SEGID, a.STD_YYMM, a.STD_DT, a.SEG_AMTRT as AMT, a.DAYS
                      from gree_seg_02 as a, gr3_item as b 
                      where a.SEGID = b.SEGID and a.START_NO < a.BASE_NO - 3 ")
  
  
  # 3) 예측용 모형 개발 셋 : 최초 2개월 제외한 실적 
  s2_gr3_db3 = sqldf("select a.SEGID, a.STD_YYMM, a.STD_DT, a.SEG_AMTRT as AMT, a.DAYS
                      from gree_seg_02 as a, gr3_item as b 
                      where a.SEGID = b.SEGID and a.START_NO < a.BASE_NO and a.START_NO > 2.5  ")
  
  
  # 4) 모형검증을 위한 최근 3개월 working day
  workday_m3 =subset(s2_gr3_db1, select = c('SEGID', 'DAYS'))
  
  
  ## 전망결과 누적 테이블 생성 
  s2_gr3_totout = sqldf("select '1' as SEGID
                       , '1' as STD_DT 
                       , '1' as AMT   " )
  
  
  
  ## 검증용 모형 생성 
  
  for (i in 1:nrow(gr3_item))  {
    
    ## TSLM ============================================
    
    item = gr3_item$SEGID[i]
    s2_gr3_dbin = subset(s2_gr3_db2, SEGID == item , 
                         select = c('STD_YYMM', 'STD_DT', 'AMT', 'DAYS'))
    
    s2_gr3_dbin = s2_gr3_dbin[order(s2_gr3_dbin$STD_DT), ]
    
    # ts 변환을 위한 최초시작 년월 생성 
    start_ym = min(s2_gr3_dbin$STD_YYMM)
    st_year = as.numeric(substr(start_ym, 1, 4))
    st_mm =  as.numeric(substr(start_ym, 6, 7))
    
    # ts 변환
    s2_gr3_ts = ts(s2_gr3_dbin, start = c(st_year, st_mm), frequency = 12)
    
    # ts model 생성
    s2_gr3_md = tslm(AMT ~ trend + season + DAYS  , data = s2_gr3_ts)
    
    # 전망값 생성
    workday_test = subset(workday_m3, SEGID == item, select = 'DAYS' )
    s2_gr3_out = as.data.frame(forecast(s2_gr3_md, newdata = workday_test))
    colnames(s2_gr3_out) = c('AMT', 'V1', 'V2', 'V3', 'V4')
    s2_gr3_out$SEGID = item
    
    s2_gr3_out2 = subset(s2_gr3_out , select = c('SEGID', 'AMT'))
    
    # 전망누적값 생성 
    # s2_gr2_totout = rbind(s2_gr2_totout,s2_gr2_out2 )
    
    
    ## 지수평활 ============================================
    
    s2_gr3_ts_ty2 = ts(s2_gr3_dbin$AMT, start = c(st_year, st_mm), frequency = 12)
    
    s2_gr3_md_ty2  = HoltWinters(s2_gr3_ts_ty2)
    
    s2_gr3_out_ty2 = as.data.frame(forecast(s2_gr3_md_ty2, h=3))
    
    colnames(s2_gr3_out_ty2) = c('AMT', 'V1', 'V2', 'V3', 'V4')
    s2_gr3_out_ty2$SEGID = item
    s2_gr3_out_ty2_02 = subset(s2_gr3_out_ty2 , select = c('SEGID', 'AMT'))
    
    
    ## 오차비교 ===========================================
    
    s2_gr3_err = subset(s2_gr3_db1, SEGID == item)
    s2_gr3_err$MD1 = s2_gr3_out2$AMT
    s2_gr3_err$MD2 = s2_gr3_out_ty2_02$AMT
    s2_gr3_err$TY1_ERR = abs( s2_gr3_err$MD1 -  s2_gr3_err$AMT)/ s2_gr3_err$AMT 
    s2_gr3_err$TY2_ERR = abs( s2_gr3_err$MD2 -  s2_gr3_err$AMT)/ s2_gr3_err$AMT 
    
    s2_gr3_err2 = sqldf("select SEGID 
                              , avg(TY1_ERR) as TY1_ERR
                              , avg(TY2_ERR) as TY2_ERR
                              , case when avg(TY1_ERR) <= avg(TY2_ERR) then 1 else 2 end as MD_TYPE
                         from s2_gr3_err
                          group by SEGID ")
    
    
    
    #### 전체 기간 전망값 생성    =============================================================
    
    ## TSLM ============================================
    
    item = gr3_item$SEGID[i]
    s2_gr3_dbin = subset(s2_gr3_db3, SEGID == item , 
                         select = c('STD_YYMM', 'STD_DT', 'AMT', 'DAYS'))
    
    s2_gr3_dbin = s2_gr3_dbin[order(s2_gr3_dbin$STD_DT), ]
    
    # ts 변환을 위한 최초시작 년월 생성 
    start_ym = min(s2_gr3_dbin$STD_YYMM)
    st_year = as.numeric(substr(start_ym, 1, 4))
    st_mm =  as.numeric(substr(start_ym, 6, 7))
    
    # ts 변환
    s2_gr3_ts = ts(s2_gr3_dbin, start = c(st_year, st_mm), frequency = 12)
    
    # ts model 생성
    s2_gr3_md = tslm(AMT ~ trend + season + DAYS  , data = s2_gr3_ts)
    
    # 전망값 생성
    s2_gr3_out = as.data.frame(forecast(s2_gr3_md, newdata = workday))
    colnames(s2_gr3_out) = c('AMT', 'V1', 'V2', 'V3', 'V4')
    s2_gr3_out$YM  = rownames(s2_gr3_out)
    s2_gr3_out$SEGID = item
    
    s2_gr3_out2 = subset(s2_gr3_out , select = c('SEGID', 'AMT', 'YM'))
    s2_gr3_out2$YYMM = workday_db$YYMM
    
    
    ## 지수평활 ============================================
    
    s2_gr3_ts_ty2 = ts(s2_gr3_dbin$AMT, start = c(st_year, st_mm), frequency = 12)
    
    s2_gr3_md_ty2  = HoltWinters(s2_gr3_ts_ty2)
    
    s2_gr3_out_ty2 = as.data.frame(forecast(s2_gr3_md_ty2, h=120))
    colnames(s2_gr3_out_ty2) = c('AMT', 'V1', 'V2', 'V3', 'V4')
    s2_gr3_out_ty2$YM = rownames(s2_gr3_out_ty2)
    
    s2_gr3_out_ty2$SEGID = item
    s2_gr3_out_ty2_02 = subset(s2_gr3_out_ty2 , select = c('SEGID', 'AMT', 'YM'))
    
    
    ## 예측결과 합치기
    s2_gr3_out_all = sqldf("select a.SEGID, a.YYMM, a.AMT as TY1_AMT 
                                , b.AMT as TY2_AMT
                            from s2_gr3_out2 as a, s2_gr3_out_ty2_02 as b 
                              where a.SEGID = b.SEGID  and a.YM = b.YM ")
    
    s2_gr3_out_all_02 = sqldf("select a.* , b.MD_TYPE   
                              from s2_gr3_out_all as a, s2_gr3_err2 as b")
    
    s2_gr3_out_f = sqldf(" select SEGID, YYMM as STD_DT
                            , case when MD_TYPE = 1 then  TY1_AMT else TY2_AMT end as AMT
                            from  s2_gr3_out_all_02   ")
    
    
    ## 전체항목 누적 전망값 생성 
    s2_gr3_totout = rbind(s2_gr3_totout , s2_gr3_out_f)
    
  }
  
  s2_gr3_out_f = subset(s2_gr3_totout , STD_DT > 10)
  
  
} else {
  
  s2_gr3_out_f  = sqldf("select '1' as SEGID
                       , '1' as STD_DT 
                       , 1 as AMT   " )
}



###### 전체 전망값 통합 

# 2개월 이하   :  s1_gr0_out_f
# 3~6 개월     :  s1_gr1_out_f
# 7~26개월     :  s1_gr2_out_f    
# 27개월 이상  :  s1_gr3_out_f


s2_gr0_out_f$AMT =  as.numeric(s2_gr0_out_f$AMT)
s2_gr1_out_f$AMT =  as.numeric(s2_gr1_out_f$AMT)
s2_gr2_out_f$AMT =  as.numeric(s2_gr2_out_f$AMT)
s2_gr3_out_f$AMT =  as.numeric(s2_gr3_out_f$AMT)


## 이름 변환 
colnames(s2_gr0_out_f) = c('SEGID', 'STD_YYMM', 'AMT')
colnames(s2_gr1_out_f) = c('SEGID', 'STD_YYMM', 'AMT')
colnames(s2_gr2_out_f) = c('SEGID', 'STD_YYMM', 'AMT')
colnames(s2_gr3_out_f) = c('SEGID', 'STD_YYMM', 'AMT')

s2_pred_amt  = rbind(s2_gr0_out_f, s2_gr1_out_f, s2_gr2_out_f, s2_gr3_out_f)

s2_pred_amt = subset(s2_pred_amt , STD_YYMM > 10)


# 음수 0 으로 처리 
s2_pred_amt$AMT = ifelse(s2_pred_amt$AMT < 0 , 0 ,  
                         ifelse(s2_pred_amt$AMT >= 100 , 100 ,  s2_pred_amt$AMT))

## GREE_DCLF_CD  변수 추가 
s2_pred_amt_02 = sqldf("select b.GREE_DCLF_CD , a.*  
                              from  s2_pred_amt as a 
                                      left join gree_list_seg  as b  on a.SEGID = b.SEGID ")

s2_pred_amt_03 = sqldf(" select GREE_DCLF_CD  , STD_YYMM
                              , sum(AMT) as TOTAMT
                              from s2_pred_amt_02 
                                group by GREE_DCLF_CD , STD_YYMM ")

s2_pred_amt_03$ADDRT = 100 / s2_pred_amt_03$TOTAMT


s2_pred_amt_04 = sqldf(" select a.*, b.ADDRT , a.AMT * b.ADDRT as AMTRT_SEG
                                 from s2_pred_amt_02 as a
                                    left join s2_pred_amt_03 as b
                                          on a.GREE_DCLF_CD = b.GREE_DCLF_CD and a.STD_YYMM = b.STD_YYMM")


s2_pred_amt_05 = sqldf(" select a.*, b.AMT as TOTAMT , a.AMTRT_SEG*b.AMT as AMT_SEG
                              from s2_pred_amt_04 as a
                                    left join s1_pred_amt  as b
                                          on a.GREE_DCLF_CD = b.GREE_DCLF_CD and a.STD_YYMM = b.STD_YYMM")


s2_pred_amt_05$REG_YYMM = STD_YYMM_CHA


#############################################################

## 최종 UPLOAD DATA 생성 

gree_info = sqldf( " select GREE_LCLS_CD, GREE_MCLS_CD, GREE_SCLS_CD, GREE_DCLF_CD
                                from gree_2  
                                 group by GREE_LCLS_CD, GREE_MCLS_CD, GREE_SCLS_CD, GREE_DCLF_CD")

gree_info_seg = sqldf( " select GREE_LCLS_CD, GREE_MCLS_CD, GREE_SCLS_CD, GREE_DCLF_CD
                                , SEGID  , MDCI_CLSC_CD, A65_AGE_CD, HSPZ_OTPT_SPCD as MCR_TP_CD
                                from gree_2  
                                 group by GREE_LCLS_CD, GREE_MCLS_CD, GREE_SCLS_CD, GREE_DCLF_CD
                                     , SEGID  , MDCI_CLSC_CD, A65_AGE_CD, HSPZ_OTPT_SPCD  ")  


s1_pred_amt$REG_YYMM = STD_YYMM_CHA

# s1_pred_amt_f = sqldf( " select a.REG_YYMM
#                                 , a.STD_YYMM
#                                 , b.GREE_LCLS_CD
#                                 , b.GREE_MCLS_CD
#                                 , b.GREE_SCLS_CD
#                                 , b.GREE_DCLF_CD
#                                 , '' as MDCI_CLSC_CD
#                                 , '' as A65_AGE_CD
#                                 , '' as MCR_TP_CD
#                                 , AMT as PRSP_VL
#                              from s1_pred_amt as a
#                                left join gree_info as b  on a.GREE_DCLF_CD = b.GREE_DCLF_CD ")


s2_pred_amt_f = sqldf( " select '3' as HIN_MODL_SPCD  
                                 , a.REG_YYMM as REG_DTTM
                                 , a.STD_YYMM
                                 , b.GREE_LCLS_CD
                                 , b.GREE_MCLS_CD
                                 , b.GREE_SCLS_CD
                                 , b.GREE_DCLF_CD
                                 , b.MDCI_CLSC_CD  
                                 , b.A65_AGE_CD
                                 , b.MCR_TP_CD   as HSPZ_OTPT_SPCD
                                 , a.AMT_SEG as PRSP_VL
                              from s2_pred_amt_05 as a
                                left join gree_info_seg as b  on a.SEGID = b.SEGID ")  


## DB적재 테이블 형식의  전망결과 저장
GREE_TABLE = s2_pred_amt_f
save(GREE_TABLE, file = paste0(result_folder_route,'TABLE/GREE_TABLE.RData'))

