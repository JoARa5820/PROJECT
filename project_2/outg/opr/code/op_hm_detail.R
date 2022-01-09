### HMUL_DETAIL 전망모형 - 지수평활(Holt-Winters)

# 환경 설정
options(scipen = 100)  # 지수표기를 숫자표기로 바꾸는 옵션
set.seed(1234)

library(data.table)
library(dplyr)
library(broom)
library(stringi)
library(zoo)
library(xts)
library(lubridate)
library(scales)
library(forecast)
library(tseries)
library(TTR)
library(ggplot2)
library(h2o)
library(MLmetrics)
library(randomForest)


fn_hm_detail <- function(REG_YYMM){
  
  # 전망시작 시점의 전월값까지 사용
  if(substr(REG_YYMM,5,6) == '01'){
    REG_YEAR <- as.numeric(substr(REG_YYMM,1,4)) - 1
    REG_MONTH <- 12
    USE_REG_YYMM <- as.numeric(paste0(REG_YEAR,REG_MONTH))
  }else{
    USE_REG_YYMM <- (as.numeric(REG_YYMM) -1)
  }
  
  # 예측 기간
  train_end <- USE_REG_YYMM
  
  
  # HMUL_DETAIL 타겟
  y_coll <- c(-)
  
  
  load(file = paste0(data_folder_route,'OPERATE_DATASET.RData'))
  inout_ds <- OPERATE_DATASET
  
  # Inf 처리
  is.na(inout_ds) <- sapply(inout_ds, is.infinite)
  inout_ds[is.na(inout_ds)] <- 0
  
  
  # 학습데이터
  train <- inout_ds[(inout_ds$YYMM <= train_end),c('YYMM',y_coll)]
  rownames(train) <- 1:nrow(train)
  train <- train[(nrow(train) - 120 + 1):nrow(train),]
  rownames(train) <- 1:nrow(train)
  

  # 예측기간 : pred_term
  start_pred_term <- as.Date(paste0(substr(REG_YYMM,1,4),'-', substr(REG_YYMM,5,6),'-01'))
  end_pred_term <- as.Date(ymd(start_pred_term) + months(119))
  pred_term <- seq(start_pred_term, end_pred_term, by = "1 month")
  pred_term <- data.frame(YYMM = paste0(substr(pred_term,1,4),substr(pred_term,6,7)))
  
  ###############################################################################################
  
  # HTW(지수평활)을 이용하기 위해 train 데이터를 time-series 타입으로 변경
  ts_train <- ts(train, start = c(as.numeric(substr(train$YYMM[1],1,4)), as.numeric(substr(train$YYMM[1],5,6))), frequency = 12)
  
  
  # HTW(지수평활)로 예측값 생성할 컬럼들
  htw_col <- y_coll
  
  ###############################################################################################
  
  # alpha_df : 지수평활에 사용될 alpha값의 시뮬레이션 결과 중 가장 최적인 것 담은 데이터프레임
  alpha_df <- data.frame(input_col = NA, alpha_num = NA, SSE = NA)
  
  # 지수평활에 사용될 alpha값에 대한 시뮬레이션
  for(htw_colnm in htw_col){
    
    min <- Inf
    
    for(alpha_num in seq(0.1,1,by=0.1)){
      
      try({htw_var <- HoltWinters(ts_train[,htw_colnm], alpha = alpha_num)}, silent = T)
      
      if(htw_var$SSE <= min){
        min <- htw_var$SSE
        tmp_alpha <- alpha_num
      }
    }
    tmp_df <- cbind(cbind(as.data.frame(htw_colnm),as.data.frame(tmp_alpha)),as.data.frame(min))
    names(tmp_df) <- c('input_col','alpha_num','SSE')
    
    alpha_df <- rbind(alpha_df, tmp_df)
  }
  alpha_df <- alpha_df[c(2:nrow(alpha_df)),]  # 첫 행(NA) 제거
  rownames(alpha_df) <- 1:nrow(alpha_df)
  
  ###############################################################################################
  
  # HTW(지수평활)로 예측값 생성
  for(k in 1:nrow(alpha_df)){
    
    htw_colnm <- alpha_df$input_col[k]
    alpha_num <- alpha_df$alpha_num[k]
    
    try({htw_mdl <- HoltWinters(ts_train[,htw_colnm], alpha = alpha_num)}, silent = T)
    
    fore_var <- forecast(htw_mdl, h = 12 * 10)  # 10년 예측
    hwt_fore_var <- as.vector(fore_var$mean)
    hwt_fore_var <- as.data.frame(hwt_fore_var)
    names(hwt_fore_var) <- htw_colnm
    
    if(k == 1){
      htw_input_fore <- hwt_fore_var
    }else{
      htw_input_fore <- cbind(htw_input_fore, hwt_fore_var)
    }
  }
  
  htw_input_fore <- cbind(pred_term,htw_input_fore)

  
  # htw_input_fore : HTW(지수평활)로 target에 대해 10년 예측한 값
  
  ###############################################################################################
  
  write.csv(htw_input_fore, file = paste0(result_folder_route,'HMUL_DETAIL/HMUL_DETAIL_전망결과_',OPERATION_POINT,'.csv'), row.names = FALSE)
  
  return(htw_input_fore)
}

