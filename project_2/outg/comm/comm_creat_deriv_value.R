#******************************************************************************************************************************#
######################################################## 파생변수 생성 #########################################################
#******************************************************************************************************************************#

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


# 함수 호출
source(paste0(comm_folder_route,'ffa_outg_comm_deriv_func.R'))  # 파생변수 생성 함수


# 기본데이터 불러오기
load(file = paste0(data_folder_route,'OPERATE_DATASET.RData'))
basic_ds <- OPERATE_DATASET

# Inf 처리
is.na(basic_ds) <- sapply(basic_ds, is.infinite)
basic_ds[is.na(basic_ds)] <- 0


# 파생변수 생성
deriv_amt_data <- fn_deriv_amt(basic_ds)   # 금액 or 건수 변수
deriv_rt_data <- fn_deriv_rt(basic_ds)     # 비율 변수


# 기본데이터셋 + 파생데이터
tmp_yymm <- as.data.frame(basic_ds[,'YYMM'])
names(tmp_yymm) <- 'YYMM'

tot_data <- cbind(tmp_yymm,cbind(deriv_amt_data,deriv_rt_data))

save(tot_data, file = paste0(data_folder_route,'add_deriv_tot_data.RData'), compress = "bzip2") # gzip, bzip2, xz


#******************************************************************************************************************************#
##################################################### V_list, R_list 생성 ######################################################
#******************************************************************************************************************************#

# 기본데이터 불러오기
load(file = paste0(data_folder_route,'OPERATE_DATASET.RData'))
basic_ds <- OPERATE_DATASET

# Inf 처리
is.na(basic_ds) <- sapply(basic_ds, is.infinite)
basic_ds[is.na(basic_ds)] <- 0


## 변수명 매핑을 위한 V,R 넘버링
v_list <- c('CNT', 'AMT', 'GDP', 'GNI', 'RAIN', 'TEMP', 'WIND')
r_list <- c('RT', 'CSI')

v_list <- basic_ds[,stri_detect_regex(names(basic_ds), paste0(v_list, '$', collapse = '|'))]
r_list <- basic_ds[,stri_detect_regex(names(basic_ds), paste0(r_list, '$', collapse = '|'))]

v_list <- as.data.frame(colnames(v_list))
names(v_list) <- 'v_list'
r_list <- as.data.frame(colnames(r_list))
names(r_list) <- 'r_list'

real_v_list <- v_list
real_r_list <- r_list


# 한글명 Mapping set (파생명,컬럼명 통합)
korea_colnm <- read.csv(file = paste0(data_folder_route,'TOT_COLNM(FIN).csv'), stringsAsFactors = FALSE, fileEncoding = 'cp949')


## V_list(금액 or 건수 변수) 컬럼명 매핑
V_list <- merge(korea_colnm, v_list, by.x = 'ENG', by.y = 'v_list', all.y = TRUE)

kor_col_sunsu_nujuk <- c()
for(tmp_num1 in 1:nrow(real_v_list)){
  for(tmp_num2 in 1:nrow(V_list)){
    
    if(real_v_list[tmp_num1,'v_list'] == V_list[tmp_num2,'ENG']){
      kor_col_sunsu <- tmp_num2
      kor_col_sunsu_nujuk <- c(kor_col_sunsu_nujuk, kor_col_sunsu)
    }
  }
}

real_v_list <- cbind(real_v_list, V_list[kor_col_sunsu_nujuk,])
rownames(real_v_list) <- 1:nrow(real_v_list)
real_v_list$NUM <- paste0('V',rownames(real_v_list))
# real_v_list[,'v_list'] == real_v_list[,'ENG']
V_list <- real_v_list


## R_list(비율 변수) 컬럼명 매핑
R_list <- merge(korea_colnm, r_list, by.x = 'ENG', by.y = 'r_list', all.y = TRUE)

kor_col_sunsu_nujuk <- c()
for(tmp_num1 in 1:nrow(real_r_list)){
  for(tmp_num2 in 1:nrow(R_list)){
    
    if(real_r_list[tmp_num1,'r_list'] == R_list[tmp_num2,'ENG']){
      kor_col_sunsu <- tmp_num2
      kor_col_sunsu_nujuk <- c(kor_col_sunsu_nujuk, kor_col_sunsu)
    }
  }
}

real_r_list <- cbind(real_r_list, R_list[kor_col_sunsu_nujuk,])
rownames(real_r_list) <- 1:nrow(real_r_list)
real_r_list$NUM <- paste0('R',rownames(real_r_list))
# real_r_list[,'r_list'] == real_r_list[,'ENG']
R_list <- real_r_list


V_list <- V_list[,c('ENG','KOR','NUM')]
R_list <- R_list[,c('ENG','KOR','NUM')]


write.csv(V_list, file = paste0(data_folder_route,'V_LIST.csv'), row.names = FALSE)
write.csv(R_list, file = paste0(data_folder_route,'R_LIST.csv'), row.names = FALSE)


#******************************************************************************************************************************#
##################################################### 컬럼명 매핑 set 생성 #####################################################
#******************************************************************************************************************************#

# 기본데이터셋 + 파생데이터
load(file = paste0(data_folder_route,'add_deriv_tot_data.RData'))


# 넘버링 리스트 (금액/건수/비율 변수)
V_list <- read.csv(file = paste0(data_folder_route,'V_LIST.csv'), stringsAsFactors = FALSE)
R_list <- read.csv(file = paste0(data_folder_route,'R_LIST.csv'), stringsAsFactors = FALSE)
NUM_list <- rbind(V_list, R_list)
rownames(NUM_list) <- 1:nrow(NUM_list)


# 한글명 Mapping set (파생명,컬럼명 통합)
TOT_COLNM <- read.csv(file = paste0(data_folder_route,'TOT_COLNM(FIN).csv'), stringsAsFactors = FALSE, fileEncoding = 'cp949')


# 넘버링과 컬럼명 나누기
x_col_split <- strsplit(names(tot_data), ".", fixed = T)

split_1 <- sapply(x_col_split, "[", 1)
split_2 <- sapply(x_col_split, "[", 2)
split_3 <- sapply(x_col_split, "[", 3)


split_2[is.na(split_2)] <- ''
split_3[is.na(split_3)] <- ''


# 나눠진 파생변수 합치기 (ex. BF1M_UPDW.M1_M6_DIFF)
sum_split2and3 <- paste0(split_2,'.',split_3)
sum_split2and3_df <- as.data.frame(sum_split2and3, stringsAsFactors = FALSE)


# modi_comma_nujuk : 위의 과정에서 맨 뒤에 컴마 생성된 애들 전처리(맨 뒤 컴마 제거)
modi_comma <- sapply(sum_split2and3[stri_detect_regex(sum_split2and3, "[.]$")], function(x){substr(x,1,(nchar(x)-1))})  # 맨 뒤에 컴마 생성된 애들
modi_comma_nujuk <- vector()  # 맨 뒤에 컴마 생성된 애들 전처리 후 넣을 데이터프레임
for(k in 1:NROW(modi_comma)){
  modi_comma_nujuk <- append(modi_comma_nujuk,modi_comma[[k]])
}

# which(stri_detect_regex(sum_split2and3_df$sum_split2and3, "[.]$")) : 맨 뒤에 컴마 있는 애들
j = 1
for(i in which(stri_detect_regex(sum_split2and3_df$sum_split2and3, "[.]$"))){
  sum_split2and3_df[i,] <- modi_comma_nujuk[j]
  j = j + 1
}


# 기본변수명 : fwd_xcol
fwd_xcol <- as.data.frame(split_1)
names(fwd_xcol) <- 'fwd_xcol'

# 파생변수명 : bwd_xcol
bwd_xcol <- as.data.frame(sum_split2and3_df$sum_split2and3)
names(bwd_xcol) <- 'bwd_xcol'

# (기본변수명 / 파생변수명 / 순서) 데이터프레임
fb_xcol <- cbind(fwd_xcol, bwd_xcol)
fb_xcol$column_sunsu <- rownames(fb_xcol)


numb_y <- fb_xcol[fb_xcol$bwd_xcol != '',]   # 넘버링 되어있는 컬럼들(파생변수)
numb_n <- fb_xcol[fb_xcol$bwd_xcol == '',]   # 넘버링 안되어있는 컬럼들(기본변수)
numb_n$bwd_xcol <- NA                        # 기본변수는 파생변수 컬럼에 빈값이 들어가있으므로 ''를 NA로 바꿈


# (1) 넘버링 된 것(파생변수) 매핑 및 컬럼 정리
numb_y <- merge(numb_y, NUM_list[,c('KOR','NUM')], by.x = 'fwd_xcol', by.y = 'NUM', all.x = TRUE)
names(numb_y)[4] <- 'FWD_KOR'

numb_y <- merge(numb_y, TOT_COLNM, by.x = 'bwd_xcol', by.y = 'ENG', all.x = TRUE)
names(numb_y)[5] <- 'BWD_KOR'


# (2) 넘버링 안된 것(기본변수) 매핑 및 컬럼 정리
numb_n <- merge(numb_n, TOT_COLNM, by.x = 'fwd_xcol', by.y = 'ENG', all.x = TRUE)
names(numb_n)[4] <- 'FWD_KOR'
numb_n$BWD_KOR <- NA


# 넘버링 된 것 + 안된 것
tot_map_col <- rbind(numb_y, numb_n)
tot_map_col$x_col <- paste0(tot_map_col$fwd_xcol,'.',tot_map_col$bwd_xcol)


tot_map_col <- tot_map_col[,c('x_col','fwd_xcol','bwd_xcol','FWD_KOR','BWD_KOR','column_sunsu')]
tot_map_col$column_sunsu <- as.numeric(tot_map_col$column_sunsu)
tot_map_col <- tot_map_col[order(tot_map_col$column_sunsu),]
rownames(tot_map_col) <- 1:nrow(tot_map_col)


# modi_comma_nujuk : 맨 뒤에 컴마 있는 애들을 컴마 제거한 값
modi_comma <- sapply(tot_map_col$x_col[stri_detect_regex(tot_map_col$x_col, "[.]NA$")], function(x){substr(x,1,(nchar(x)-3))})
modi_comma_nujuk <- vector()
for(k in 1:NROW(modi_comma)){
  modi_comma_nujuk <- append(modi_comma_nujuk,modi_comma[[k]])
}

# which(stri_detect_regex(sum_split2and3_df$sum_split2and3, "[.]$")) : 맨 뒤에 컴마 있는 애들
j = 1
for(i in which(stri_detect_regex(tot_map_col$x_col, "[.]NA$"))){
  tot_map_col[i,'x_col'] <- modi_comma_nujuk[j]
  j = j + 1
}


tot_map_col <- tot_map_col[,!names(tot_map_col) %in% "column_sunsu"]


# 컬럼명 매핑 set 저장
write.csv(tot_map_col, file = paste0(data_folder_route,'COLNM_MAPPING_SET.csv'), row.names = FALSE)

