### ADPT_P 전망모형 - 시계열회귀(TSLM)

# 환경 설정
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


y_coll <- c('ADPT_P_65LT_CNT', 'ADPT_P_65GT_CNT')


fn_adpt_p <- function(REG_YYMM){
  
  # 전망시작 시점의 전월값까지 사용
  if(substr(REG_YYMM,5,6) == '01'){
    REG_YEAR <- as.numeric(substr(REG_YYMM,1,4)) - 1
    REG_MONTH <- 12
    USE_REG_YYMM <- as.numeric(paste0(REG_YEAR,REG_MONTH))
  }else{
    USE_REG_YYMM <- (as.numeric(REG_YYMM) -1)
  }
  
  # 학습 기간
  train_end <- USE_REG_YYMM
  
  
  for(y_col in y_coll){
    
    
    # 타겟 한글명
    if(y_col == 'ADPT_P_65LT_CNT'){
      kor_y_col <- 'ADPT_P_65세미만'
    }else if(y_col == 'ADPT_P_65GT_CNT'){
      kor_y_col <- 'ADPT_P_65세이상'
    }
    
    
    # 기본데이터셋 + 파생데이터
    load(file = paste0(data_folder_route,'add_deriv_tot_data.RData'))
    
    # Inf 처리
    is.na(tot_data) <- sapply(tot_data, is.infinite)
    tot_data[is.na(tot_data)] <- 0


    # 입력변수 정의
    if(y_col == 'ADPT_P_65LT_CNT'){
      default_col <- c('YYMM',y_col,'STCS_TT_CNT')
      input_col <- c('REGIST_FORGN_TT_CNT','AREA_FORGN_TT_CNT')
      mdl_col <- c(default_col,input_col)
    }else{
      default_col <- c('YYMM',y_col,'STCS_TT_CNT')
      input_col <- c('REGIST_FORGN_TT_CNT','AREA_FORGN_TT_CNT')
      mdl_col <- c(default_col,input_col)
    }
    
    
    # 기본데이터셋 : data
    data <- tot_data[,mdl_col]
    
    # 학습데이터와 미래값 생성을 위한 데이터셋(= test)
    train <- data[(data$YYMM <= train_end),]
    rownames(train) <- 1:nrow(train)
    train <- train[(nrow(train) - 120 + 1):nrow(train),]
    rownames(train) <- 1:nrow(train)
    
    
    test <- train
    test$YYMM <- test$YYMM + 1000
    test[,names(test)[!names(test) %in% c('YYMM')]] <- NA
    test <- test[,names(test) != y_col]  # target 제외
    
    
    #------------------------------------------------------------------------------------------#
    
    ## 입력변수의 예측값 생성(=test set)
    
    htw_data <- train
    htw_use_data <- ts(htw_data, start = c(as.numeric(substr(train$YYMM[1],1,4)), as.numeric(substr(train$YYMM[1],5,6))), frequency = 12)
    
    # HTW(지수평활)로 예측값 생성할 입력변수
    htw_col <- names(test)[!names(test) %in% c('YYMM')]
    
    
    # 적합 alpha 값 누적 데이터프레임
    # alpha_df <- data.frame(input_col = NA, alpha_num = NA, SSE = NA)
    
    # # HTW(지수평활)로 생성할 각 변수의 적합 alpha 값 시뮬레이션
    # for(htw_colnm in htw_col){
    #   
    #   min <- Inf
    #   
    #   for(alpha_num in seq(0.1,1,by=0.1)){
    #     htw_var <- HoltWinters(htw_use_data[,htw_colnm], alpha = alpha_num)
    #     
    #     if(htw_var$SSE <= min){
    #       min <- htw_var$SSE
    #       tmp_alpha <- alpha_num
    #     }
    #   }
    #   tmp_df <- cbind(cbind(as.data.frame(htw_colnm),as.data.frame(tmp_alpha)),as.data.frame(min))
    #   names(tmp_df) <- c('input_col','alpha_num','SSE')
    #   
    #   alpha_df <- rbind(alpha_df, tmp_df)
    # }
    # alpha_df <- alpha_df[c(2:nrow(alpha_df)),]  # 첫 행(NA) 제거
    # rownames(alpha_df) <- 1:nrow(alpha_df)
    # 
    alpha_df <- read.csv(file = paste0(data_folder_route,'ADPT_P_alpha_df.csv'), stringsAsFactors = FALSE)
    
    # HTW(지수평활)로 예측값 생성
    for(k in 1:nrow(alpha_df)){
      
      htw_colnm <- alpha_df$input_col[k]
      alpha_num <- alpha_df$alpha_num[k]
      
      try({htw_var <- HoltWinters(htw_use_data[,htw_colnm], alpha = alpha_num)}, silent = T)
      fore_var <- forecast(htw_var, h = 12 * 10)  # 10년 예측
      hwt_fore_var <- as.vector(fore_var$mean)
      hwt_fore_var <- as.data.frame(hwt_fore_var)
      names(hwt_fore_var) <- htw_colnm
      
      if(k == 1){
        htw_input_fore <- hwt_fore_var
      }else{
        htw_input_fore <- cbind(htw_input_fore, hwt_fore_var)
      }
    }
    
    
    # test에 생성한 값 넣기
    for(input_nm in htw_col){
      test[,input_nm] <- htw_input_fore[,input_nm]
    }
    

    ###############################################################################################
    ###############################################################################################
    
    # train, test : 원본 데이터
    # new_train, new_test : tslm에 사용할 train, test 데이터셋
    if(y_col == y_coll[1]){
      
      ### basic target 예측
      new_train <- train
      new_test <- test
      
      
      ## tslm으로 y값 예측
      ts_tslm_data <- ts(new_train, 
                         start = c(as.numeric(substr(train$YYMM[1],1,4)), as.numeric(substr(train$YYMM[1],5,6))), frequency = 12)
      
      
      # TSLM 모델
      tslm_formula <- c('ADPT_P_65LT_CNT ~ STCS_TT_CNT + REGIST_FORGN_TT_CNT + AREA_FORGN_TT_CNT + trend + season')
      tslm_md <- tslm(formula = tslm_formula, data = ts_tslm_data)
      
      
      # 예측값 생성
      tslm_fore <- forecast(tslm_md, newdata = new_test)
      tslm_fore <- as.numeric(tslm_fore$mean)
      
      tslm_fore <- as.data.frame(tslm_fore)
      names(tslm_fore) <- kor_y_col
      
      
      # 예측값 누적 프레임
      tmp_year <- as.data.frame(test[,c('YYMM')])
      names(tmp_year) <- 'YYMM'
      
      tot_pred_nujuk <- cbind(tmp_year,tslm_fore)
      
    }else{
      
      ### log target 예측
      log_new_train <- train
      log_new_test <- test
      
      #--------------------------------------------------------------------------------------------------------#
      
      ## log 데이터 만들기
      log_col <- names(log_new_test)
      
      ## 단위 변수 리스트
      log_trans_list <- c('CNT', 'AMT', 'GDP', 'GNI', 'RAIN', 'TEMP', 'WIND','bf1m')
      log_trans_input_col <- log_col[stri_detect_regex(log_col, paste0(log_trans_list, '$', collapse = '|'))]
      
      log_new_train[,c(y_col,log_trans_input_col)] <- log(log_new_train[,c(y_col,log_trans_input_col)])
      log_new_test[,c(log_trans_input_col)] <- log(log_new_test[,c(log_trans_input_col)])
      
      # Inf 처리
      is.na(log_new_train) <- sapply(log_new_train, is.infinite)
      log_new_train[is.na(log_new_train)] <- 0
      
      # Inf 처리
      is.na(log_new_test) <- sapply(log_new_test, is.infinite)
      log_new_test[is.na(log_new_test)] <- 0
      
      #--------------------------------------------------------------------------------------------------------#
      
      # tslm으로 log y값 예측
      log_ts_tslm_data <- ts(log_new_train, 
                             start = c(as.numeric(substr(train$YYMM[1],1,4)), as.numeric(substr(train$YYMM[1],5,6))), frequency = 12)
      
      
      # TSLM 모델
      tslm_formula <- c('ADPT_P_65GT_CNT ~ STCS_TT_CNT + REGIST_FORGN_TT_CNT + AREA_FORGN_TT_CNT + trend + season')
      tslm_md <- tslm(formula = tslm_formula, data = log_ts_tslm_data)
      
      
      # 예측값 생성
      tslm_fore <- forecast(tslm_md, newdata = log_new_test)
      tslm_fore <- as.numeric(tslm_fore$mean)
      
      tslm_fore <- as.data.frame(exp(tslm_fore))
      names(tslm_fore) <- kor_y_col
      

      # 예측값 누적 프레임
      tot_pred_nujuk <- cbind(tot_pred_nujuk,tslm_fore)
    }
  }
  
  write.csv(tot_pred_nujuk, file = paste0(result_folder_route,'ADPT_P/ADPT_P_전망결과_',OPERATION_POINT,'.csv'), row.names = FALSE)
  return(tot_pred_nujuk)
}

