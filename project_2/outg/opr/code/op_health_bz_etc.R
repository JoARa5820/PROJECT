## 연값 전망 (health / etc_bz 등) - 지수평활(Holt-Winters)

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


fn_health_bz_etc <- function(REG_YYMM){
  
  
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
  
  # Inf 처리
  is.na(OPERATE_DATASET) <- sapply(OPERATE_DATASET, is.infinite)
  OPERATE_DATASET[is.na(OPERATE_DATASET)] <- 0
  
  
  # 타겟 설정
  y_coll <- c('TOT_HEALTH_GJB_AMT',
              
              'CHULYUNGUM_BDG_AMT','SIMPYUNGWON_BDG_AMT','RETIRE_AMT')
  
  
  ## 타겟별 전망값 생성
  for(y_col in y_coll){
    
    
    # 사용할 데이터셋 : data
    data <- OPERATE_DATASET[,c('YYMM',y_coll)]
    
    
    ## 연값 생성 데이터 (=학습데이터)
    if(y_col == 'TOT_HEALTH_GJB_AMT'){

      data <- data[data$YYMM <= USE_REG_YYMM, c('YYMM',y_col)]
      rownames(data) <- 1:nrow(data)
      data <- data[(nrow(data) - 120 + 1):nrow(data),]
      rownames(data) <- 1:nrow(data)
      
    }else{
     
      ## 매 1월에만 전망값 생성
      if(substr(REG_YYMM,5,6) == '01'){
        
        data <- data[data$YYMM <= USE_REG_YYMM, c('YYMM',y_col)]
        rownames(data) <- 1:nrow(data)
        data <- data[(nrow(data) - 120 + 1):nrow(data),]
        rownames(data) <- 1:nrow(data)
      }else{
        
        NOT_JAN_YYMM <- as.numeric(paste0((as.numeric(substr(REG_YYMM,1,4)) - 1),12))
        
        data <- data[data$YYMM <= NOT_JAN_YYMM, c('YYMM',y_col)]
        rownames(data) <- 1:nrow(data)
        data <- data[(nrow(data) - 120 + 1):nrow(data),]
        rownames(data) <- 1:nrow(data)
      }
    }
    
    
    ## 연값 생성
    for(i in 1:10){

      yy_sum <- as.data.frame(sum(data[((i*12) - 11):(i*12),y_col]))
      names(yy_sum) <- y_col
      
      yy_sum$S_YYMM <- data[((i*12) - 11),'YYMM']
      yy_sum$YEAR <- substr(yy_sum$S_YYMM,1,4)
      
      if(i == 1){
        fin_nujuk_df <- yy_sum
      }else{
        fin_nujuk_df <- rbind(fin_nujuk_df, yy_sum)
      }
      
    }
    
    fin_nujuk_df <- fin_nujuk_df[,c('S_YYMM','YEAR',y_col)]
    fin_nujuk_df$YEAR <- as.numeric(fin_nujuk_df$YEAR)
    fin_nujuk_df$S_YYMM <- as.numeric(fin_nujuk_df$S_YYMM)
    
    # 연데이터(학습데이터) : fin_nujuk_df
    
    ##################################################################################################################
    
    # train set (10년 학습)
    train_data <- fin_nujuk_df
    
    # Inf 처리
    is.na(train_data) <- sapply(train_data, is.infinite)
    train_data[is.na(train_data)] <- 0
    
    
    if(y_col == 'TOT_HEALTH_GJB_AMT'){
      
      # 예측기간 데이터프레임 생성
      year_df <- data.frame(YEAR = train_data$YEAR) 
      year_df$YEAR <- year_df$YEAR + 10             # 연도
      year_df$S_YYMM <- train_data$S_YYMM + 1000    # 전망시작 시점(연월)
    }
    
    
    # time-series로 데이터 형변환
    ts_train <- ts(train_data, start = train_data$YEAR[1], frequency = 1)
    
    ## HTW(지수평활)로 예측값 생성
    htw_col <- y_col
    
    # 지수평활에 사용될 alpha값 시뮬레이션
    alpha_df <- data.frame(input_col = NA, alpha_num = NA, SSE = NA)
    
    for(htw_colnm in htw_col){
      
      min <- Inf
      
      for(alpha_num in seq(0.1,1,by=0.1)){
        
        try({htw_var <- HoltWinters(ts_train[,htw_colnm], alpha = alpha_num, gamma = FALSE)}, silent = T)  # 연 모형일 경우 gamma = FALSE
        
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
      
      try({htw_var <- HoltWinters(ts_train[,htw_colnm], alpha = alpha_num, gamma = FALSE)}, silent = T)  # 연 모형일 경우 gamma = FALSE
      fore_var <- forecast(htw_var, h = 1 * 10)  # 10년 예측
      hwt_fore_var <- as.vector(fore_var$mean)
      hwt_fore_var <- as.data.frame(hwt_fore_var)
      names(hwt_fore_var) <- htw_colnm
      
      if(k == 1){
        htw_input_fore <- hwt_fore_var
      }else{
        htw_input_fore <- cbind(htw_input_fore, hwt_fore_var)
      }
    }
    
    htw_input_fore[htw_input_fore < 0] <- 0  # 음수값은 0으로 대체
    
    
    if(y_col == y_coll[1]){
      YEAR_PRED <- cbind(year_df,htw_input_fore)  # htw_input_fore : 예측값
    }else{
      YEAR_PRED <- cbind(YEAR_PRED,htw_input_fore)
    }
    
    
    if(y_col == y_coll[1]){
      YEAR_DATA <- fin_nujuk_df  # YEAR_DATA : 연값
    }else{
      fin_nujuk_df <- as.data.frame(fin_nujuk_df[,names(fin_nujuk_df)[!names(fin_nujuk_df) %in% c('YEAR','S_YYMM')]])
      names(fin_nujuk_df) <- y_col
      YEAR_DATA <- cbind(YEAR_DATA,fin_nujuk_df)
    }
    
  }
  
  ## 연 값(학습데이터) 저장
  write.csv(YEAR_DATA, file = paste0(data_folder_route,'YEAR_DATA_health_bz_etc.csv'), row.names = FALSE)
  
  ## 전망값 저장
  write.csv(YEAR_PRED, file = paste0(result_folder_route,'ETC_BZ/HEALTH_BZ_전망결과_',OPERATION_POINT,'.csv'), row.names = FALSE)

  return(YEAR_PRED)
}

