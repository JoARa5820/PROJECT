## HGUM 소분류 항목들 - 지수평활(Holt-Winters)

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


fn_hgum <- function(REG_YYMM){
  
  
  # 전망시작 시점의 전월값까지 사용
  if(substr(REG_YYMM,5,6) == '01'){
    REG_YEAR <- as.numeric(substr(REG_YYMM,1,4)) - 1
    REG_MONTH <- 12
    USE_REG_YYMM <- as.numeric(paste0(REG_YEAR,REG_MONTH))
  }else{
    USE_REG_YYMM <- (as.numeric(REG_YYMM) -1)
  }
  
  
  # 월데이터 불러오기
  load(file = paste0(data_folder_route,'OPERATE_DATASET.RData'))
  
  HGUM_DATA <- OPERATE_DATASET[,c('YYMM',-)]
  
  # Inf 처리
  is.na(HGUM_DATA) <- sapply(HGUM_DATA, is.infinite)
  HGUM_DATA[is.na(HGUM_DATA)] <- 0

  ## 기준연월 numeric으로 형변환
  HGUM_DATA$YYMM <- as.numeric(HGUM_DATA$YYMM)
  
  ## 컬럼명 변경
  names(HGUM_DATA) <- c('YYMM',-)
  
  
  ## 전망변수(타겟) 정의
  y_coll <- names(HGUM_DATA)[!names(HGUM_DATA) %in% c('YYMM',-)]
  
  
  ## 10년 학습 항목
  train_ten_year <- c(-)
  
  ## 개시 시점부터 학습 & 향후 실적 추가될 때마다 학습기간 추가되는 항목
  train_stand_n_year <- c(-)
  
  ## 예외 시점부터 학습 & 향후 실적 추가될 때마다 학습기간 추가되는 항목
  train_exclu_n_year <- c(-)
  
  
  for(y_col in y_coll){
    
    ## 학습데이터 정의
    if(y_col %in% train_ten_year){
      
      train <- HGUM_DATA[HGUM_DATA$YYMM <= USE_REG_YYMM,c('YYMM',y_col)]
      rownames(train) <- 1:nrow(train)
      train <- train[(nrow(train) - 120 + 1):nrow(train),]
      rownames(train) <- 1:nrow(train)
      
    }else if(y_col %in% train_stand_n_year){
      
      train <- HGUM_DATA[HGUM_DATA$YYMM <= USE_REG_YYMM,c('YYMM',y_col)]
      
      if(y_col == '-'){
        train <- train[train$YYMM >= yearmm,]
        
      }else if(y_col == '-'){
        train <- train[train$YYMM >= yearmm,]
        
      }else if(y_col == '-'){
        train <- train[train$YYMM >= yearmm,]
        
      }else if(y_col == '-'){
        train <- train[train$YYMM >= yearmm,]
        
      }else if(y_col == '-'){
        train <- train[train$YYMM >= yearmm,]
      }
      
      rownames(train) <- 1:nrow(train)
      
    }else if(y_col %in% train_exclu_n_year){
      
      train <- HGUM_DATA[HGUM_DATA$YYMM <= USE_REG_YYMM,c('YYMM',y_col)]
      rownames(train) <- 1:nrow(train)
      
      if(y_col == '-'){
        
        if(substr(USE_REG_YYMM,1,4) <= '2021'){
          train <- train[(nrow(train) - 24 + 1):nrow(train),]
        }else{
          train <- train[(train$YYMM >= yearmm) & (train$YYMM <= USE_REG_YYMM),]
        }
        rownames(train) <- 1:nrow(train)
        
      }else if(y_col == '-'){
        
        if(USE_REG_YYMM <= yearmm){
          train <- train[(nrow(train) - 60 + 1):nrow(train),]
        }else{
          train <- train[train$YYMM >= yearmm,]
        }
        rownames(train) <- 1:nrow(train)
        
      }
    }
    
    # 예측기간 : pred_term
    start_pred_term <- as.Date(paste0(substr(REG_YYMM,1,4),'-', substr(REG_YYMM,5,6),'-01'))
    end_pred_term <- as.Date(ymd(start_pred_term) + months(119))
    pred_term <- seq(start_pred_term, end_pred_term, by = "1 month")
    pred_term <- data.frame(YYMM = paste0(substr(pred_term,1,4),substr(pred_term,6,7)))
    
    
    # HTW(지수평활)을 이용하기 위해 train 데이터를 time-series 타입으로 변경
    ts_train <- ts(train, start = c(as.numeric(substr(train$YYMM[1],1,4)), as.numeric(substr(train$YYMM[1],5,6))), frequency = 12)
    
    # HTW(지수평활)로 예측값 생성할 컬럼들
    htw_col <- y_col
    
    
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
    
    htw_input_fore[htw_input_fore[,y_col] < 0, y_col] <- 0  # 음수값은 0으로 대체
    
    
    ### 여기서부터 추가작업 (월전망 값이 0 또는 음수일 경우, 전월부터 과거 1년치의 평균 값으로 채워서 전망)
    zero_rep_list <- c(-)
    
    if(y_col %in% zero_rep_list){
      
      ## 0값 채울 인덱스
      mody_index <- which(htw_input_fore[,y_col] == 0)
      mody_index <- (nrow(train) + mody_index)
      
      ## 0값 채울 데이터
      mody_data <- c(train[,y_col],htw_input_fore[,y_col])
      
      for(mody_num in mody_index){
        
        mody_data[mody_num] <- mean(mody_data[(mody_num-12) : (mody_num-1)])
      }
      
      htw_input_fore <- as.data.frame(mody_data[(nrow(train)+1):NROW(mody_data)])
      names(htw_input_fore) <- y_col
    }
    
    if(y_col == y_coll[1]){
      nujuk_htw_input_fore <- cbind(pred_term,htw_input_fore)
    }else{
      nujuk_htw_input_fore <- cbind(nujuk_htw_input_fore,htw_input_fore)
    }
    
  }
  
  ## 전망값 저장
  write.csv(nujuk_htw_input_fore, file = paste0(result_folder_route,'HGUM/HGUM_전망결과_',OPERATION_POINT,'.csv'), row.names = FALSE)
  
  return(nujuk_htw_input_fore)
  
}

