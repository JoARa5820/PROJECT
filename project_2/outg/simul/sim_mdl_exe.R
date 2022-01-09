#####################################################################################################################################
#---------------------------------------------- 분석알고리즘 전망모형 개발 시뮬레이션 ----------------------------------------------#
#####################################################################################################################################

# R 환경 초기화
rm(list=ls())


# 경로 설정
opr_folder_route <- '~/prsp_hin/outg/opr/'
data_folder_route <- paste0(opr_folder_route,'data/')
sim_folder_name <- '~/prsp_hin/outg/simul/'
result_folder_route <- paste0(sim_folder_name,'result/')
comm_folder_route <- '~/prsp_hin/outg/comm/'


# HMUL_P1P 전망 시뮬레이션 코드 가져오기
source(paste0(sim_folder_name,'code/ffa_outg_sim_mdl_func.R'))

## STEP1. 시뮬레이션 실행시점 지정 : 현재 년/월/일
SIMUL_POINT <- paste0(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10))

## 시뮬레이션 수행시점(기준시점) 지정
REG_YYMM <- 202001

#------------------------------------------------------------------------------------------------------------------------------------#

## STEP2. 전망모형별 시뮬레이션 여부 체크 및 모형개발 변수 선택
## 설명 : 기준시점이 202001인 경우, 200901~201812 값으로 학습하여 2019년의 월평균오차율(MAPE)를 구하고,
##        예측값은 201001~201912 값으로 학습한 후 202001값부터 추출함

# (1) - 전망모형 선택 여부
MODEL1_YN <- 'Y'
MODEL1_VAR <- c('V172.Y1_AVG','V203.Y1_AVG','V219.M3_AVG','V178.Y1_AVG','V242.Y1_AVG','V241.Y1_AVG','V238.Y1_AVG','CURR_LIFE_CSI',
                'GNI','GDP')

# (2) - 전망모형 선택 여부
MODEL2_YN <- 'Y'
MODEL2_VAR <- c('V69.M3_AVG','V161.M3_AVG','V207.M3_AVG','V207.Y1_AVG','V35.M3_AVG','V31.M3_AVG','V259.Y1_AVG','V170.M6_AVG',
                'V178.Y1_AVG','V198.LOG','V204.Y1_AVG','V107.M1_M6_RT','V241.Y1_AVG','V243.Y1_AVG','V238.Y1_AVG','V221.M3_AVG',
                'CURR_LIFE_CSI','GDP','GNI')

# (3) - 전망모형 선택 여부
MODEL3_YN <- 'Y'
MODEL3_VAR <- c('V210.Y1_AVG','V205.Y1_AVG','V227.M6_AVG','V227.M6_M12_RT','SG_JUMYU_TT_clnc_RT','SG_JUMYU_TT_snr_t_hspt_RT','GDP','GNI')

# (4) - 전망모형 선택 여부
MODEL4_YN <- 'Y'
MODEL4_VAR <- c('V150.Y1_AVG','V183.Y1_AVG','V11.M3_AVG','V31.Y1_AVG','V50.M6_AVG','V153.M3_AVG','V168.M3_AVG','V255.Y1_AVG',
                'V7.M1_M12_RT','V7.M1_M3_RT','V7.M3_AVG','V31.M1_M3_RT','V227.M6_AVG','V227.M6_M12_RT','GDP','GNI')

## 전망모형별 시뮬레이션 실행 (현재 날짜로 자동 저장)
fn_sim_mdl(MODEL1_YN,MODEL2_YN,MODEL3_YN,MODEL4_YN,REG_YYMM)

#------------------------------------------------------------------------------------------------------------------------------------#

