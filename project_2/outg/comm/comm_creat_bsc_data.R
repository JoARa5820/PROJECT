##### 기본데이터셋 수정 및 새 데이터 행(row) 추가
# 기본데이터셋 + DB값 + 전망값 => 기본데이터셋

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
library(reshape2)


#### 기본데이터셋의 미래값 생성 (미래 기준연월(YYMM) 생성)

## 전망시작 시점의 전월값까지 사용
if(substr(REG_YYMM,5,6) == '01'){
  REG_YEAR <- as.numeric(substr(REG_YYMM,1,4)) - 1
  REG_MONTH <- 12
  USE_REG_YYMM <- as.numeric(paste0(REG_YEAR,REG_MONTH))
}else{
  USE_REG_YYMM <- (as.numeric(REG_YYMM) -1)
}


## 기본데이터 호출
load(file = paste0(data_folder_route,'OPERATE_DATASET.RData'))
is.na(OPERATE_DATASET) <- sapply(OPERATE_DATASET, is.infinite)  # Inf 처리
OPERATE_DATASET[is.na(OPERATE_DATASET)] <- 0


#####################################################################################################################################

#### 외부데이터 바꿔끼기

## 외부데이터 생성코드 호출
source(paste0(prsp_hin_folder_route,'ffa_ext_crt_anly_data.R') , encoding = 'cp949')  # , encoding = 'cp949'

# 외부데이터 호출
rc <- fn_crt_ext(REG_YYMM, pwd = 'outg')

# 통계청 추계인구 외 외부데이터 바꿔끼기
extern_data <- read.csv(file = paste0(prsp_hin_folder_route,'outg_ext/외부데이터_',REG_YYMM,'.csv'), stringsAsFactors = FALSE)
extern_data <- extern_data[,c('BASE_YM','POP_TT_CNT.07','REGIST_FORGN_TT_CNT','AREA_FORGN_CNT','CURR_LIFE_CSI','GDP','GNI')]
names(extern_data) <- c('YYMM','STCS_TT_CNT','REGIST_FORGN_TT_CNT','AREA_FORGN_TT_CNT','CURR_LIFE_CSI','GDP','GNI')

extern_intersect_YYMM <- sort(intersect(extern_data$YYMM, OPERATE_DATASET$YYMM))
OPERATE_DATASET[(OPERATE_DATASET$YYMM >= extern_intersect_YYMM[1]) & (OPERATE_DATASET$YYMM <= extern_intersect_YYMM[NROW(extern_intersect_YYMM)]),c('STCS_TT_CNT','REGIST_FORGN_TT_CNT','AREA_FORGN_TT_CNT','CURR_LIFE_CSI','GDP','GNI')] <- extern_data[(extern_data$YYMM >= extern_intersect_YYMM[1]) & (extern_data$YYMM <= extern_intersect_YYMM[NROW(extern_intersect_YYMM)]),c('STCS_TT_CNT','REGIST_FORGN_TT_CNT','AREA_FORGN_TT_CNT','CURR_LIFE_CSI','GDP','GNI')]

#####################################################################################################################################

## 학습데이터 생성
train <- OPERATE_DATASET[(OPERATE_DATASET$YYMM <= USE_REG_YYMM),]
rownames(train) <- 1:nrow(train)
train <- train[(nrow(train) - 120 + 1):nrow(train),]
rownames(train) <- 1:nrow(train)

## 예측기간 df 생성
pred_term <- data.frame(YYMM = train[,'YYMM'] + 1000)

# 지수평활 이용을 위해 데이터 형 변환 (time-series 타입으로 변경)
ts_train <- ts(train, start = c(as.numeric(substr(train$YYMM[1],1,4)), as.numeric(substr(train$YYMM[1],5,6))), frequency = 12)

# 지수평활로 예측값 생성할 컬럼들
htw_col <- names(train)[names(train) != 'YYMM']

## 지수평활로 예측값 생성
for(htw_colnm in htw_col){
  
  try({htw_mdl <- HoltWinters(ts_train[,htw_colnm])}, silent = T)
  
  fore_var <- forecast(htw_mdl, h = 12 * 10)  # 10년 예측
  hwt_fore_var <- as.data.frame(as.vector(fore_var$mean))
  names(hwt_fore_var) <- htw_colnm
  
  if(htw_colnm == htw_col[1]){
    htw_input_fore <- c(pred_term,hwt_fore_var)
  }else{
    htw_input_fore <- cbind(htw_input_fore, hwt_fore_var)
  }
}


#### 기본데이터셋에 미래 예측값 채우고 붙이기

## 기본데이터셋 빈칸 예측값으로 채우기
OPR_repl_ym <- htw_input_fore$YYMM[htw_input_fore$YYMM %in% OPERATE_DATASET$YYMM]
OPR_repl_cnm <- names(htw_input_fore)[names(htw_input_fore) != 'YYMM']

for(OPR_repl_ym_num in OPR_repl_ym){
  for(OPR_repl_cnm_num in OPR_repl_cnm){
    
    # 기본데이터셋의 값이 만약 NA이면(빈값이면) 예측값 대체 수행
    if(is.na(OPERATE_DATASET[OPERATE_DATASET$YYMM == OPR_repl_ym_num,OPR_repl_cnm_num])){
      OPERATE_DATASET[OPERATE_DATASET$YYMM == OPR_repl_ym_num,OPR_repl_cnm_num] <- htw_input_fore[htw_input_fore$YYMM == OPR_repl_ym_num,OPR_repl_cnm_num]
    }
  }
}

## 빈칸 채워진 기본데이터셋에 예측값 붙이기
OPR_add_ym <- htw_input_fore$YYMM[!htw_input_fore$YYMM %in% OPERATE_DATASET$YYMM]
OPR_add_data <- htw_input_fore[htw_input_fore$YYMM %in% OPR_add_ym,]

OPERATE_DATASET <- rbind(OPERATE_DATASET,OPR_add_data)

#####################################################################################################################################

##### DB 추출 데이터로 교체 (변수명 = 파일명)

# DB_data_folder_route 경로에 들어있는 데이터 목록
DB_R_DATA_LIST <- list.files(path = paste0(DB_data_folder_route))
DB_R_DATA_LIST <- DB_R_DATA_LIST[stri_detect_regex(DB_R_DATA_LIST, "RData$")]

# 교체 대상 데이터가 아닌 것들 목록에서 제외
DB_R_DATA_LIST <- DB_R_DATA_LIST[!DB_R_DATA_LIST %in% c('BOJANGSUNG_DATA.RData',
                                                        'DB_HM_COMM_DATA.RData',
                                                        'DB_SUJIN_COMM_DATA.RData',
                                                        'DB_R_SUJIN_tt_istn_DATA.RData',
                                                        'ORG_JINRYO_CNT.RData',
                                                        'ORG_SG_JUMYU_AMT.RData')]


for(db_r_data_list in DB_R_DATA_LIST){
  load(file = paste0(DB_data_folder_route,db_r_data_list))
}

db_y_coll <- substr(DB_R_DATA_LIST,1,(nchar(DB_R_DATA_LIST)-6))


for(db_y_col in db_y_coll){

  LOT_YYMM <- get(db_y_col)$YYMM
  LOT_COLNM <- names(get(db_y_col))[names(get(db_y_col)) != 'YYMM']

  for(lot_yymm in LOT_YYMM){
    for(lot_colnm in LOT_COLNM){

      OPERATE_DATASET[OPERATE_DATASET$YYMM == lot_yymm, lot_colnm] <- get(db_y_col)[get(db_y_col)$YYMM == lot_yymm, lot_colnm]
    }
  }
}

#####################################################################################################################################

# 연도 추가 & 수정
OPERATE_DATASET$YEAR <- as.numeric(substr(OPERATE_DATASET$YYMM,1,4))

#####################################################################################################################################

## 기존에 있던 실적값은 고정 (200501~201912)
load(file = paste0(data_folder_route,'OPERATE_DATASET_2005_2019.RData'))
change_col <- names(OPERATE_DATASET)[!names(OPERATE_DATASET) %in% c('STCS_TT_CNT','REGIST_FORGN_TT_CNT','AREA_FORGN_TT_CNT','CURR_LIFE_CSI','GDP','GNI')]
OPERATE_DATASET[(OPERATE_DATASET$YYMM >= 200501) & (OPERATE_DATASET$YYMM <= 201912),change_col] <- OPERATE_DATASET_2005_2019[(OPERATE_DATASET_2005_2019$YYMM >= 200501) & (OPERATE_DATASET_2005_2019$YYMM <= 201912),change_col]


## 업데이트 된 기본데이터셋 저장
save(OPERATE_DATASET, file = paste0(data_folder_route,'OPERATE_DATASET.RData'))

#####################################################################################################################################

