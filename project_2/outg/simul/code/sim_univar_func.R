#******************************************************************************************************************************#
########################################### 전체데이터(기본+파생)에 대한 유의성분석 ###########################################
#******************************************************************************************************************************#

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


# 함수 호출
source(paste0(comm_folder_route,'ffa_outg_comm_univar_func.R'))                # 유의성분석 함수


## 유의성분석 실행 기간 설정과 target 지정
fn_sim_univar <- function(MODEL1_YN, MODEL2_YN, MODEL3_YN, MODEL4_YN, REG_YYMM, STANDARD){

  y_coll <- ''
  
  
  if(MODEL1_YN == 'Y'){
    y_coll <- c(y_coll,'HM_65LT_tt_istn_SG_NO_INCLU_P1P_COST_AMT')
  }
  if(MODEL2_YN == 'Y'){
    y_coll <- c(y_coll,'HM_65LT_tt_istn_SG_NO_INFL_P1P_COST_AMT')
  }
  if(MODEL3_YN == 'Y'){
    y_coll <- c(y_coll,'HM_65GT_tt_istn_SG_NO_INCLU_P1P_COST_AMT')
  }
  if(MODEL4_YN == 'Y'){
    y_coll <- c(y_coll,'HM_65GT_tt_istn_SG_NO_INFL_P1P_COST_AMT')
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
  
  #-------------------------------------------------------------------------------------------------------------------------------#
  
  # (1) [운영변수] - 전망모형 사용변수
  MODEL1_VAR <- c('V172.Y1_AVG',
                  'V203.Y1_AVG',
                  'V219.M3_AVG',
                  'V178.Y1_AVG',
                  'V242.Y1_AVG',
                  'V241.Y1_AVG',
                  'V238.Y1_AVG',
                  'CURR_LIFE_CSI',
                  'GNI',
                  'GDP')
  
  
  # (2) [운영변수] - 전망모형 사용변수
  MODEL2_VAR <- c('V69.M3_AVG',
                  'V161.M3_AVG',
                  'V207.M3_AVG',
                  'V207.Y1_AVG',
                  'V35.M3_AVG',
                  'V31.M3_AVG',
                  'V259.Y1_AVG',
                  'V170.M6_AVG',
                  'V178.Y1_AVG',
                  'V198.LOG',
                  'V204.Y1_AVG',
                  'V107.M1_M6_RT',
                  'V241.Y1_AVG',
                  'V243.Y1_AVG',
                  'V238.Y1_AVG',
                  'V221.M3_AVG',
                  'CURR_LIFE_CSI',
                  'GDP',
                  'GNI')
  
  
  # (3) [운영변수] - 전망모형 사용변수
  MODEL3_VAR <- c('V210.Y1_AVG',
                  'V205.Y1_AVG',
                  'V227.M6_AVG',
                  'V227.M6_M12_RT',
                  'SG_JUMYU_TT_clnc_RT',
                  'SG_JUMYU_TT_snr_t_hspt_RT',
                  'GDP',
                  'GNI')
  
  
  # (4) [운영변수] - 전망모형 사용변수
  MODEL4_VAR <- c('V150.Y1_AVG',
                  'V183.Y1_AVG',
                  'V11.M3_AVG',
                  'V31.Y1_AVG',
                  'V50.M6_AVG',
                  'V153.M3_AVG',
                  'V168.M3_AVG',
                  'V255.Y1_AVG',
                  'V7.M1_M12_RT',
                  'V7.M1_M3_RT',
                  'V7.M3_AVG',
                  'V31.M1_M3_RT',
                  'V227.M6_AVG',
                  'V227.M6_M12_RT',
                  'GDP',
                  'GNI')
  
  #-------------------------------------------------------------------------------------------------------------------------------#
  
  # 모델 학습 종료년도
  end_yymm <- USE_REG_YYMM        # 현재 기준 학습 종료연도
  
  # 기본데이터셋 + 파생데이터 불러오기
  load(file = paste0(data_folder_route,'add_deriv_tot_data.RData'))
  
  # 컬럼명 매핑 set 불러오기
  map_col <- read.csv(file = paste0(data_folder_route,'COLNM_MAPPING_SET.csv'), stringsAsFactors = FALSE)
  
  #-------------------------------------------------------------------------------------------------------------------------------#
  
  # 데이터셋 중 (금액 or 건수)데이터 log 변환
  v_list <- c('CNT', 'AMT', 'GDP', 'GNI', 'RAIN', 'TEMP', 'WIND')
  v_list <- tot_data[,stri_detect_regex(names(tot_data), paste0(v_list, '$', collapse = '|'))]
  
  except_v_list <- tot_data[!colnames(tot_data) %in% colnames(v_list)]  # (금액 or 건수)데이터가 아닌 것
  include_v_list <- tot_data[colnames(tot_data) %in% colnames(v_list)]  # (금액 or 건수)데이터인 것
  
  include_v_list <- log(include_v_list)  # (금액 or 건수)데이터인 것 log 변환
  
  tot_data <- cbind(except_v_list, include_v_list)
  is.na(tot_data) <- sapply(tot_data, is.infinite)  # log 변환 과정에서 생성된 NaN값 처리
  tot_data[is.na(tot_data)] <- 0
  
  #-------------------------------------------------------------------------------------------------------------------------------#
  
  ## 학습데이터 설정
  tot_data <- tot_data[(tot_data$YYMM <= end_yymm),]
  rownames(tot_data) <- 1:nrow(tot_data)
  tot_data <- tot_data[(nrow(tot_data) - 120 + 1):nrow(tot_data),]
  rownames(tot_data) <- 1:nrow(tot_data)
  
  #-------------------------------------------------------------------------------------------------------------------------------#
  
  ## 전체데이터셋(기본+파생) 중 유의성분석 수행할 컬럼 제한 (=기본컬럼으로만 유의성분석 수행) / 택1
  # 기본데이터 / 기본컬럼
  load(file = paste0(data_folder_route,'OPERATE_DATASET.RData'))
  basic_ds <- OPERATE_DATASET
  basic_col <- names(basic_ds)[!names(basic_ds) %in% c('YYMM')]
  basic_col <- basic_col[basic_col != 'YEAR']
  
  ## 전체 데이터셋에 대해 유의성분석 / 택1
  # basic_col <- names(tot_data)[names(tot_data) != 'YEAR']
  
  #-------------------------------------------------------------------------------------------------------------------------------#
  
  univar_nujuk_df <- data.frame(y_col = NA, KOR_y_col = NA, input_col = NA, FWD_KOR = NA,
                                correlation = NA, adj.r.squared_1 = NA, adj.r.squared_2 = NA, adj.r.squared_3 = NA,
                                max_r_squared = NA)

  
  for(y_col in y_coll){
    
    data <- tot_data
    
    ## 기본변수
    if(STANDARD == 'Y'){
      
      if(y_col == '-'){
        data <- data[,c(y_col,MODEL1_VAR)]
      }else if(y_col == '-'){
        data <- data[,c(y_col,MODEL2_VAR)]
      }else if(y_col == '-'){
        data <- data[,c(y_col,MODEL3_VAR)]
      }else if(y_col == '-'){
        data <- data[,c(y_col,MODEL4_VAR)]
      }
    }else{
      
      ## 후보변수
      # 유의성분석 수행할 컬럼 제한 (기본컬럼으로만 유의성분석 수행)
      data <- data[,basic_col]
      
      if(y_col == '-'){
        data <- data[,names(data)[!names(data) %in% MODEL1_VAR]]
      }else if(y_col == '-'){
        data <- data[,names(data)[!names(data) %in% MODEL2_VAR]]
      }else if(y_col == '-'){
        data <- data[,names(data)[!names(data) %in% MODEL3_VAR]]
      }else if(y_col == '-'){
        data <- data[,names(data)[!names(data) %in% MODEL4_VAR]]
      }
    }
    
    
    ## 유의성분석 수행
    univar_rslt <- fn_univar(data,y_col)
    
    
    ## 컬럼명 매핑
    
    # 타겟 컬럼명 매핑
    if(y_col == '-'){
      univar_rslt$KOR_y_col <- '-'
    }else if(y_col == '-'){
      univar_rslt$KOR_y_col <- '-'
    }else if(y_col == '-'){
      univar_rslt$KOR_y_col <- '-'
    }else if(y_col == '-'){
      univar_rslt$KOR_y_col <- '-'
    }
    
    # 입력변수 컬럼명 매핑
    map_fact_rslt <- merge(univar_rslt, map_col, by.x = 'input_col', by.y = 'x_col', all.x = TRUE)
    map_fact_rslt <- map_fact_rslt[,c('y_col','KOR_y_col','input_col','FWD_KOR',
                                      'correlation','adj.r.squared_1','adj.r.squared_2','adj.r.squared_3',
                                      'max_r_squared')]
    
    
    # 상관계수 소수점 4자리에서 끊기
    map_fact_rslt$correlation <- round(map_fact_rslt$correlation,4)
    
    # 최대 결정계수 순서로 정렬
    map_fact_rslt <- map_fact_rslt[order(map_fact_rslt$max_r_squared, decreasing = TRUE),]
    rownames(map_fact_rslt) <- 1:nrow(map_fact_rslt)
    
    univar_nujuk_df <- rbind(univar_nujuk_df, map_fact_rslt)
  }
  
  # 컬럼명 한글명으로 변경
  names(univar_nujuk_df) <- c('타겟','타겟(한글명)','입력변수','입력변수(한글명)','상관계수','결정계수(1차)','결정계수(2차)','결정계수(3차)','최대_결정계수')
  
  univar_nujuk_df <- univar_nujuk_df[2:nrow(univar_nujuk_df),]
  rownames(univar_nujuk_df) <- 1:nrow(univar_nujuk_df)
  
  
  # if(STANDARD == 'Y'){
  #   write.csv(univar_nujuk_df, paste0(result_folder_route,'유의성_분석/적용변수_유의성분석결과_',SIMUL_POINT,'.csv'), row.names = FALSE)
  # }else{
  #   write.csv(univar_nujuk_df, paste0(result_folder_route,'유의성_분석/후보변수_유의성분석결과_',SIMUL_POINT,'.csv'), row.names = FALSE)
  # }
  
  return(univar_nujuk_df)
}

