#####################################################################################################################################
#---------------------------------------------------- 전망모형 변수 유의성 분석 ----------------------------------------------------#
#####################################################################################################################################

# R 환경 초기화
rm(list=ls())


# 경로 설정
opr_folder_route <- '~/prsp_hin/outg/opr/'
data_folder_route <- paste0(opr_folder_route,'data/')
sim_folder_name <- '~/prsp_hin/outg/simul/'
result_folder_route <- paste0(sim_folder_name,'result/')
comm_folder_route <- '~/prsp_hin/outg/comm/'


# 유의성 분석 코드 가져오기
source(paste0(sim_folder_name,'code/ffa_outg_sim_univar_func.R'))

## STEP1. 시뮬레이션 실행시점 지정 : 현재 년/월/일
SIMUL_POINT <- paste0(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10))

# 시뮬레이션 수행시점(기준시점) 지정
REG_YYMM <- 202001

#------------------------------------------------------------------------------------------------------------------------------------#

## STEP2. 기본 전망모형 적용변수 유의성 분석여부 체크
MODEL1_YN <- 'Y'                   # - 전망모형 선택 여부 (Y/N)
MODEL2_YN <- 'N'                   # - 전망모형 선택 여부 (Y/N)         
MODEL3_YN <- 'Y'                   # - 전망모형 선택 여부 (Y/N)
MODEL4_YN <- 'N'                   # - 전망모형 선택 여부 (Y/N)         
STANDARD <- 'Y'                    # 기준변수 여부 : Y 고정 (운영모형 적용변수)

## 기본 전망모형 적용변수 유의성 분석 실행 및 파일로 저장
sim_univar <- fn_sim_univar(MODEL1_YN,MODEL2_YN,MODEL3_YN,MODEL4_YN,REG_YYMM,STANDARD)
write.csv(sim_univar, paste0(result_folder_route,'유의성_분석/적용변수_유의성분석결과_',SIMUL_POINT,'.csv'), row.names = FALSE)

#------------------------------------------------------------------------------------------------------------------------------------#

## STEP3. 전망모형별 후보변수 유의성 분석여부 체크  
MODEL1_YN <- 'Y'                   # - 전망모형 선택 여부 (Y/N)
MODEL2_YN <- 'N'                   # - 전망모형 선택 여부 (Y/N)         
MODEL3_YN <- 'N'                   # - 전망모형 선택 여부 (Y/N)
MODEL4_YN <- 'Y'                   # - 전망모형 선택 여부 (Y/N)         
STANDARD <- 'N'                    # 기준변수 여부 : N 고정 (후보변수)

## 전망모형별 후보변수 유의성 분석 실행 및 파일로 저장
sim_univar <- fn_sim_univar(MODEL1_YN,MODEL2_YN,MODEL3_YN,MODEL4_YN,REG_YYMM,STANDARD)
write.csv(sim_univar, paste0(result_folder_route,'유의성_분석/후보변수_유의성분석결과_',SIMUL_POINT,'.csv'), row.names = FALSE)

#------------------------------------------------------------------------------------------------------------------------------------#

