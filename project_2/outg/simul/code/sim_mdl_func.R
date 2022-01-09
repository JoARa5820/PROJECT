## 지수평활(Holt Winters), H2O

# 경로 설정
options(scipen = 100)  # 지수표기를 숫자표기로 바꾸는 옵션
set.seed(1234)


# 라이브러리 호출
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



## 학습 기간 설정과 target 지정
fn_sim_mdl <- function(MODEL1_YN,MODEL2_YN,MODEL3_YN,MODEL4_YN,REG_YYMM){
  
  
  y_coll <- ''
  
  
  if(MODEL1_YN == 'Y'){
    y_coll <- c(y_coll,'-')
  }
  if(MODEL2_YN == 'Y'){
    y_coll <- c(y_coll,'-')
  }
  if(MODEL3_YN == 'Y'){
    y_coll <- c(y_coll,'-')
  }
  if(MODEL4_YN == 'Y'){
    y_coll <- c(y_coll,'-')
  }
  
  y_coll <- y_coll[y_coll != '']
  
  
  # 전망시작 시점의 전월값까지 사용
  if(substr(REG_YYMM,5,6) == '01'){
    REG_YEAR <- as.numeric(substr(REG_YYMM,1,4)) - 1
    REG_MONTH <- 12
    USE_REG_YYMM <- as.numeric(paste0(REG_YEAR,REG_MONTH))
  }else{
    USE_REG_YYMM <- (as.numeric(REG_YYMM) -1)
  }
  
  
  # 모델 학습 종료년도
  real_train_end <- USE_REG_YYMM        # 현재 기준 학습 종료연도
  mape_train_end <- USE_REG_YYMM - 100  # 오차율 생성을 위한 학습 종료연도
  
  input_cnt <- c(NROW(MODEL1_VAR),NROW(MODEL2_VAR),NROW(MODEL3_VAR),NROW(MODEL4_VAR))  # 사용변수 축적 데이터프레임 생성을 위해 필요
  
  
  ## 모델링
  for(y_col in y_coll){
  
    ## 시뮬레이션 변수 선택
    if(y_col == '-'){
      input_col <- MODEL1_VAR
    }else if(y_col == '-'){
      input_col <- MODEL2_VAR
    }else if(y_col == '-'){
      input_col <- MODEL3_VAR
    }else if(y_col == '-'){
      input_col <- MODEL4_VAR
    }
    
    
    # 기본데이터셋 + 파생데이터
    load(file = paste0(data_folder_route,'add_deriv_tot_data.RData'))
    
    # Inf 처리
    is.na(tot_data) <- sapply(tot_data, is.infinite)
    tot_data[is.na(tot_data)] <- 0
  
    
    # 사용할 컬럼 지정 및 전월값 생성 / y_col + input_col + bf1m
    data <- tot_data[,c('YYMM',y_col,input_col)]
    data$bf1m <- shift(tot_data[,y_col], n = 1)
    
    
    # 더미변수 생성
    data <- transform(data,
                      mm1 = ifelse(substr(data$YYMM,5,6) == '01',1,0),
                      mm2 = ifelse(substr(data$YYMM,5,6) == '02',1,0),
                      mm3 = ifelse(substr(data$YYMM,5,6) == '03',1,0),
                      mm4 = ifelse(substr(data$YYMM,5,6) == '04',1,0),
                      mm5 = ifelse(substr(data$YYMM,5,6) == '05',1,0),
                      mm6 = ifelse(substr(data$YYMM,5,6) == '06',1,0),
                      mm7 = ifelse(substr(data$YYMM,5,6) == '07',1,0),
                      mm8 = ifelse(substr(data$YYMM,5,6) == '08',1,0),
                      mm9 = ifelse(substr(data$YYMM,5,6) == '09',1,0),
                      mm10 = ifelse(substr(data$YYMM,5,6) == '10',1,0),
                      mm11 = ifelse(substr(data$YYMM,5,6) == '11',1,0),
                      mm12 = ifelse(substr(data$YYMM,5,6) == '12',1,0))
    
    
    # 보장성 여부 더미변수
    data$bojang_YN <- 0
    data[data$YYMM >= 201709,'bojang_YN'] <- 1
    
    
    # 근무일수 데이터 추가
    business_days <- read.csv(file = paste0(data_folder_route,'영업일수(FIN).csv'), stringsAsFactors = FALSE, fileEncoding = 'cp949')
    data <- merge(data, business_days, by = c('YYMM'), all.x = TRUE)
    data <- data[order(data$YYMM),]
    rownames(data) <- 1:nrow(data)
    
    #----------------------------------------------------------------------------------------------#
    
    ##### 여기서부터 MAPE 생성
    
    # 기본데이터셋 : data
    # 학습데이터(mape_train)와 검증데이터(mape_test) 나누기
    mape_train <- data[(data$YYMM <= mape_train_end),]
    rownames(mape_train) <- 1:nrow(mape_train)
    mape_train <- mape_train[(nrow(mape_train) - 120 + 1):nrow(mape_train),]
    rownames(mape_train) <- 1:nrow(mape_train)
    
    mape_test <- mape_train
    mape_test$YYMM <- (mape_test$YYMM + 1000)
    mape_test <- mape_test[,names(mape_test) != y_col]  # mape_test set에서 target 변수 제거
    mape_test[,c(input_col,'bf1m','BUSINESS_DAYS')] <- NA
    mape_test[mape_test$YYMM >= 201701,'bojang_YN'] <- 1
    mape_test[mape_test$YYMM < 201701,'bojang_YN'] <- 0

    
    ## 지수평활(Holt-Winters) 이용하여 미래 input값 통예측
    # 새로 사용할 학습데이터와 검증데이터 생성
    new_mape_train <- mape_train  # mape_train : 실적값 / new_mape_train : 새로 사용할 학습데이터
    new_mape_test <- mape_test  # mape_test : 실적값 / new_mape_test : 새로 사용할 검증데이터
    
    # 학습데이터를 ts(time-series)데이터로 변환
    ts_mape_train <- ts(new_mape_train, start = c(as.numeric(substr(mape_train$YYMM[1],1,4)), as.numeric(substr(mape_train$YYMM[1],5,6))), frequency = 12)
    
    # 지수평활(Holt-Winters)로 생성할 mape_test set의 input 컬럼들
    htw_col <- c(input_col,'bf1m','BUSINESS_DAYS')
    
    # alpha_df : 지수평활에 사용될 alpha값의 시뮬레이션 결과 중 가장 최적인 것 담은 데이터프레임
    alpha_df <- data.frame(input_col = NA, alpha_num = NA, SSE = NA)
    
    # 지수평활에 사용될 alpha값에 대한 시뮬레이션
    for(htw_colnm in htw_col){
      
      min <- Inf
      
      for(alpha_num in seq(0.1,1,by=0.1)){
        try({htw_mdl <- HoltWinters(ts_mape_train[,htw_colnm], alpha = alpha_num)}, silent = T)
        
        if(htw_mdl$SSE <= min){
          min <- htw_mdl$SSE
          tmp_alpha <- alpha_num
        }
      }
      tmp_df <- cbind(cbind(as.data.frame(htw_colnm),as.data.frame(tmp_alpha)),as.data.frame(min))
      names(tmp_df) <- c('input_col','alpha_num','SSE')
      
      alpha_df <- rbind(alpha_df, tmp_df)
    }
    alpha_df <- alpha_df[c(2:nrow(alpha_df)),]  # 첫 행(NA) 제거
    rownames(alpha_df) <- 1:nrow(alpha_df)

    
    for(k in 1:nrow(alpha_df)){
      
      htw_colnm <- alpha_df$input_col[k]
      alpha_num <- alpha_df$alpha_num[k]
      
      try({htw_mdl <- HoltWinters(ts_mape_train[,htw_colnm], alpha = alpha_num)}, silent = T)
      
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
    
    
    # htw_input_fore : HTW로 input(2019년)예측한 값 mape_test set에 넣어주기
    for(htw_colnm in htw_col){
      
      new_mape_test[,htw_colnm] <- htw_input_fore[,htw_colnm]
    }
    
    
    ## 영업일수 실적값 넣어주기
    busi_YYMM <- new_mape_test$YYMM
    
    for(busi_NUM in busi_YYMM){
      new_mape_test[new_mape_test$YYMM == busi_NUM,'BUSINESS_DAYS'] <- business_days[business_days$YYMM == busi_NUM,'BUSINESS_DAYS']
    }
    
    
    ###### log 데이터 만들기 ######
    
    # 학습데이터와 검증데이터의 컬럼 중 (금액 or 건수) 데이터인 경우 log 씌워주기
    log_new_mape_train <- new_mape_train
    log_new_mape_test <- new_mape_test
    
    ## 단위 변수 리스트
    log_trans_list <- c('CNT', 'AMT', 'GDP', 'GNI', 'RAIN', 'TEMP', 'WIND','bf1m')
    log_trans_input_col <- htw_col[stri_detect_regex(htw_col, paste0(log_trans_list, '$', collapse = '|'))]
    
    log_new_mape_train[,c(y_col,log_trans_input_col)] <- log(log_new_mape_train[,c(y_col,log_trans_input_col)])
    log_new_mape_test[,c(log_trans_input_col)] <- log(log_new_mape_test[,c(log_trans_input_col)])
    
    # Inf 처리
    is.na(log_new_mape_train) <- sapply(log_new_mape_train, is.infinite)
    log_new_mape_train[is.na(log_new_mape_train)] <- 0
    
    # Inf 처리
    is.na(log_new_mape_test) <- sapply(log_new_mape_test, is.infinite)
    log_new_mape_test[is.na(log_new_mape_test)] <- 0
    
    
    ### 모델링 (AutoML(log)이용)
    
    # h2o 초기화
    h2o.init()
    
    
    # h2o 형식의 Dataframe
    h2o.train_data <- as.h2o(log_new_mape_train)
    h2o.test_data <- as.h2o(log_new_mape_test)
    
    
    # learning
    aml <- h2o.automl(y = y_col,
                      training_frame = h2o.train_data,
                      max_runtime_secs = 60,
                      max_models = 10, seed = 1, sort_metric = 'RMSE')  # validation_frame = h2o.mape_test_data

    
    ## 'MSE', 'RMSE', 'MAE', 'RMSLE' // RMSE, MAE, MAPE
    # Leader Board(모델 성능) 확인
    # print(aml@leaderboard, n = nrow(aml@leaderboard$model_id))
    # summary(aml@leader)
    
    # Forecast (타겟에 대한 예측값 생성)
    h2o_fore <- h2o.predict(aml, h2o.test_data)
    
    
    # 결과
    h2o_fore <- as.data.frame(h2o_fore)
    h2o_fore <- exp(h2o_fore)
    names(h2o_fore) <- y_col
    
    
    ## 컬럼명 매핑
    if(y_col == '-'){
      kor_y_col <- '-'
    }else if(y_col == '-'){
      kor_y_col <- '-'
    }else if(y_col == '-'){
      kor_y_col <- '-'
    }else if(y_col == '-'){
      kor_y_col <- '-'
    }
    
    
    ## 오차율 계산 후 누적
    siljuk <- tot_data[(tot_data$YYMM >= mape_test$YYMM[1]),y_col]
    siljuk <- siljuk[c(1:12)]
    pred_for_mape <- h2o_fore[c(1:12),]
    
    mape_value <- MAPE(pred_for_mape, siljuk) * 100
    mape_value <- round(mape_value,4)
    mape_value <- as.data.frame(mape_value)
    names(mape_value) <- kor_y_col
    
    
    if(y_col == y_coll[1]){
      mape_nujuk <- mape_value
    }else{
      mape_nujuk <- cbind(mape_nujuk,mape_value)
    }
    
    ##### 여기까지가 MAPE 생성
    
    #----------------------------------------------------------------------------------------------#
    
    ##### 여기서부터 실행시점으로부터의 전망값 생성
    
    # 기본데이터셋 : data
    # 학습데이터(real_train)와 검증데이터(real_test) 나누기
    real_train <- data[(data$YYMM <= real_train_end),]
    rownames(real_train) <- 1:nrow(real_train)
    real_train <- real_train[(nrow(real_train) - 120 + 1):nrow(real_train),]
    rownames(real_train) <- 1:nrow(real_train)
    
    real_test <- real_train
    real_test$YYMM <- (real_test$YYMM + 1000)
    real_test <- real_test[,names(real_test) != y_col]  # real_test set에서 target 변수 제거
    real_test[,c(input_col,'bf1m','BUSINESS_DAYS')] <- NA
    real_test[real_test$YYMM >= 201701,'bojang_YN'] <- 1
    real_test[real_test$YYMM < 201701,'bojang_YN'] <- 0
  
    
    ## 지수평활(Holt-Winters) 이용하여 미래 input값 통예측
    # 새로 사용할 학습데이터와 검증데이터 생성
    new_real_train <- real_train  # real_train : 실적값 / new_real_train : 새로 사용할 학습데이터
    new_real_test <- real_test  # real_test : 실적값 / new_real_test : 새로 사용할 검증데이터
    
    # 학습데이터를 ts(time-series)데이터로 변환
    ts_real_train <- ts(new_real_train, start = c(as.numeric(substr(real_train$YYMM[1],1,4)), as.numeric(substr(real_train$YYMM[1],5,6))), frequency = 12)
    
    # 지수평활(Holt-Winters)로 생성할 real_test set의 input 컬럼들
    htw_col <- c(input_col,'bf1m','BUSINESS_DAYS')
    
    
    # alpha_df : 지수평활에 사용될 alpha값의 시뮬레이션 결과 중 가장 최적인 것 담은 데이터프레임
    alpha_df <- data.frame(input_col = NA, alpha_num = NA, SSE = NA)
    
    # 지수평활에 사용될 alpha값에 대한 시뮬레이션
    for(htw_colnm in htw_col){
      
      min <- Inf
      
      for(alpha_num in seq(0.1,1,by=0.1)){
        try({htw_mdl <- HoltWinters(ts_real_train[,htw_colnm], alpha = alpha_num)}, silent = T)
        
        if(htw_mdl$SSE <= min){
          min <- htw_mdl$SSE
          tmp_alpha <- alpha_num
        }
      }
      tmp_df <- cbind(cbind(as.data.frame(htw_colnm),as.data.frame(tmp_alpha)),as.data.frame(min))
      names(tmp_df) <- c('input_col','alpha_num','SSE')
      
      alpha_df <- rbind(alpha_df, tmp_df)
    }
    alpha_df <- alpha_df[c(2:nrow(alpha_df)),]  # 첫 행(NA) 제거
    rownames(alpha_df) <- 1:nrow(alpha_df)
    
    
    for(k in 1:nrow(alpha_df)){
      
      htw_colnm <- alpha_df$input_col[k]
      alpha_num <- alpha_df$alpha_num[k]
      
      try({htw_mdl <- HoltWinters(ts_real_train[,htw_colnm], alpha = alpha_num)}, silent = T)
      
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
    
    
    # htw_input_fore : HTW로 input(2019년)예측한 값 real_test set에 넣어주기
    for(htw_colnm in htw_col){
      
      new_real_test[,htw_colnm] <- htw_input_fore[,htw_colnm]
    }
    
    
    ## 영업일수 실적값 넣어주기
    busi_YYMM <- new_real_test$YYMM
    
    for(busi_NUM in busi_YYMM){
      new_real_test[new_real_test$YYMM == busi_NUM,'BUSINESS_DAYS'] <- business_days[business_days$YYMM == busi_NUM,'BUSINESS_DAYS']
    }
    
    
    ###### log 데이터 만들기 ######
    
    # 학습데이터와 검증데이터의 컬럼 중 (금액 or 건수) 데이터인 경우 log 씌워주기
    log_new_real_train <- new_real_train
    log_new_real_test <- new_real_test
    
    ## 단위 변수 리스트
    log_trans_list <- c('CNT', 'AMT', 'GDP', 'GNI', 'RAIN', 'TEMP', 'WIND','bf1m')
    log_trans_input_col <- htw_col[stri_detect_regex(htw_col, paste0(log_trans_list, '$', collapse = '|'))]
    
    log_new_real_train[,c(y_col,log_trans_input_col)] <- log(log_new_real_train[,c(y_col,log_trans_input_col)])
    log_new_real_test[,c(log_trans_input_col)] <- log(log_new_real_test[,c(log_trans_input_col)])
    
    # Inf 처리
    is.na(log_new_real_train) <- sapply(log_new_real_train, is.infinite)
    log_new_real_train[is.na(log_new_real_train)] <- 0
    
    # Inf 처리
    is.na(log_new_real_test) <- sapply(log_new_real_test, is.infinite)
    log_new_real_test[is.na(log_new_real_test)] <- 0
    
    
    ### 모델링 (AutoML(log)이용)
    
    # h2o 초기화
    h2o.init()
    
    
    # h2o 형식의 Dataframe
    h2o.train_data <- as.h2o(log_new_real_train)
    h2o.test_data <- as.h2o(log_new_real_test)
    
    
    # learning
    aml <- h2o.automl(y = y_col,
                      training_frame = h2o.train_data,
                      max_runtime_secs = 60,
                      max_models = 10, seed = 1, sort_metric = 'RMSE')  # validation_frame = h2o.real_test_data
    
    
    ## 'MSE', 'RMSE', 'MAE', 'RMSLE' // RMSE, MAE, MAPE
    # Leader Board(모델 성능) 확인
    # print(aml@leaderboard, n = nrow(aml@leaderboard$model_id))
    # summary(aml@leader)
    
    # Forecast (타겟에 대한 예측값 생성)
    h2o_fore <- h2o.predict(aml, h2o.test_data)
    
    
    # 결과
    h2o_fore <- as.data.frame(h2o_fore)
    h2o_fore <- exp(h2o_fore)
    
    
    ## 컬럼명 매핑
    if(y_col == '-'){
      kor_y_col <- '-'
    }else if(y_col == '-'){
      kor_y_col <- '-'
    }else if(y_col == '-'){
      kor_y_col <- '-'
    }else if(y_col == '-'){
      kor_y_col <- '-'
    }
    names(h2o_fore) <- kor_y_col
    
    
    ## 전망값 누적
    
    # 전망년월 : real_test_yymm_df
    real_test_yymm_df <- as.data.frame(log_new_real_test[,c('YYMM')])
    names(real_test_yymm_df) <- 'YYMM'
    
    if(y_col == y_coll[1]){
      hmul_pred_nujuk <- cbind(real_test_yymm_df, h2o_fore)
    }else{
      hmul_pred_nujuk <- cbind(hmul_pred_nujuk,h2o_fore)
    }
    
    
    ## 분석에 사용한 변수의 한글명 매핑
    COLMAP <- read.csv(file = paste0(data_folder_route,'COLNM_MAPPING_SET.csv'), stringsAsFactors = FALSE)
    
    using_input_col <- input_col
    using_input_col <- data.frame(x_col = using_input_col, stringsAsFactors = FALSE)
    using_input_col <- left_join(using_input_col, COLMAP, by = 'x_col')
    using_input_col[is.na(using_input_col)] <- ''
    using_input_col$USING_INPUT_COLNM <- paste0(using_input_col$FWD_KOR,'_',using_input_col$BWD_KOR)
    using_input_col$전망모형 <- y_col
    using_input_col$전망모형_KOR <- kor_y_col
    using_input_col <- using_input_col[,c('전망모형','전망모형_KOR','x_col','USING_INPUT_COLNM')]
    names(using_input_col) <- c('전망모형','전망모형(한글명)','분석변수','분석변수(한글명)')
    
    
    if(y_col == y_coll[1]){
      using_input_col_nujuk <- using_input_col
    }else{
      using_input_col_nujuk <- rbind(using_input_col_nujuk,using_input_col)
    }
    
  }
  
  
  write.csv(using_input_col_nujuk, file = paste0(result_folder_route,'시뮬레이션_결과/HMUL_P1P_시뮬레이션_사용변수_',SIMUL_POINT,'.csv'), row.names = FALSE)
  write.csv(hmul_pred_nujuk, file = paste0(result_folder_route,'시뮬레이션_결과/HMUL_P1P_시뮬레이션_전망결과_',SIMUL_POINT,'.csv'), row.names = FALSE)
  write.csv(mape_nujuk, file = paste0(result_folder_route,'시뮬레이션_결과/HMUL_P1P_시뮬레이션_오차율_',SIMUL_POINT,'.csv'), row.names = FALSE)
  
  return(hmul_pred_nujuk)
  
  # h2o.shutdown()
}
  

