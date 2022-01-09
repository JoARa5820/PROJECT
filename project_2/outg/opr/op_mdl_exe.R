#####################################################################################################################################
#------------------------------------------------- 분석알고리즘 전망모형 개발 운영 -------------------------------------------------#
#####################################################################################################################################

# R 환경 초기화
options(scipen = 100)  # 지수표기를 숫자표기로 바꾸는 옵션
set.seed(1234)

# 경로 설정
basic_folder_name <- '~/prsp_hin/outg/opr/'
code_folder_route <- paste0(basic_folder_name,'code/')
data_folder_route <- paste0(basic_folder_name,'data/')
DB_data_folder_route <- paste0(basic_folder_name,'DB_data/')
result_folder_route <- paste0(basic_folder_name,'result/')
comm_folder_route <- '~/prsp_hin/outg/comm/'
prsp_hin_folder_route <- '~/prsp_hin/comm/'

#####################################################################################################################################

## 실행시점 (파일 저장 날짜이자 현재 날짜)
OPERATION_POINT <- paste0(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10))

## 테이블명 지정
db_qualifier_1 <- 'VM'

#####################################################################################################################################

## 기준시점(실행시점)지정 = parameter
arg <- commandArgs(trailingOnly = TRUE)
pram1 <- arg[1]

# 전망시점(= 등록년월)
if(is.na(pram1)){
  # 매월 20일 이후는 당월 기준으로 전망
  if(day(Sys.Date()) >= 20){ REG_YYMM <- paste0(substr(Sys.Date(),1,4), substr(Sys.Date(),6,7))
  }else {
    # 매월 20일 이전은 이전월 기준으로 전망
    bf_month <- ymd(Sys.Date()) - months(1)
    REG_YYMM <- paste0(substr(bf_month,1,4), substr(bf_month,6,7))}
}else {
  REG_YYMM <- pram1
}

#####################################################################################################################################

## DB 접속 코드 호출
source(paste0(prsp_hin_folder_route,'ffa_comm_db.R'))

# DB 데이터 추출
source(paste0(comm_folder_route,'ffa_outg_comm_read_db.R'))

# 기본데이터셋 생성
source(paste0(comm_folder_route,'ffa_outg_comm_creat_bsc_data.R'))

# 파생변수 생성
source(paste0(comm_folder_route,'ffa_outg_comm_creat_deriv_value.R'))

#####################################################################################################################################

# HMUL_DETAIL 전망코드 호출
source(paste0(code_folder_route,'ffa_outg_op_hm_detail.R'))

# ADPT_P 전망코드 호출
source(paste0(code_folder_route,'ffa_outg_op_adpt_p.R'))

# HMUL_P1P 전망코드 호출
source(paste0(code_folder_route,'ffa_outg_op_hm_p1p.R'))

# HEALTH / ETC_BZ 전망코드 호출
source(paste0(code_folder_route,'ffa_outg_op_health_bz_etc.R'))  # 연 전망
source(paste0(code_folder_route,'ffa_outg_op_etc_bz.R'))         # 월 전망

# HGUM 전망코드 호출
source(paste0(code_folder_route,'ffa_outg_op_hgum.R'))

#####################################################################################################################################

## HMUL_DETAIL 전망
fn_hm_detail(REG_YYMM)

#------------------------------------------------------------------------------------------------------------------------------------#

## ADPT_P 전망
fn_adpt_p(REG_YYMM)

#------------------------------------------------------------------------------------------------------------------------------------#

## HMUL_P1P 전망

# (1) - 전망모형 사용변수
MODEL1_VAR <- c('V172.Y1_AVG','V203.Y1_AVG','V219.M3_AVG','V178.Y1_AVG',
                'V242.Y1_AVG','V241.Y1_AVG','V238.Y1_AVG','CURR_LIFE_CSI','GNI','GDP')


# (2) - 전망모형 사용변수
MODEL2_VAR <- c('V69.M3_AVG','V161.M3_AVG','V207.M3_AVG','V207.Y1_AVG',
                'V35.M3_AVG','V31.M3_AVG','V259.Y1_AVG','V170.M6_AVG',
                'V178.Y1_AVG','V198.LOG','V204.Y1_AVG','V107.M1_M6_RT',
                'V241.Y1_AVG','V243.Y1_AVG','V238.Y1_AVG','V221.M3_AVG',
                'CURR_LIFE_CSI','GDP','GNI')


# (3) - 전망모형 사용변수
MODEL3_VAR <- c('V210.Y1_AVG','V205.Y1_AVG','V227.M6_AVG','V227.M6_M12_RT',
                'SG_JUMYU_TT_clnc_RT','SG_JUMYU_TT_snr_t_hspt_RT','GDP','GNI')


# (4) - 전망모형 사용변수
MODEL4_VAR <- c('V150.Y1_AVG','V183.Y1_AVG','V11.M3_AVG','V31.Y1_AVG',
                'V50.M6_AVG','V153.M3_AVG','V168.M3_AVG','V255.Y1_AVG',
                'V7.M1_M12_RT','V7.M1_M3_RT','V7.M3_AVG','V31.M1_M3_RT',
                'V227.M6_AVG','V227.M6_M12_RT','GDP','GNI')

fn_hm_p1p(REG_YYMM)

#------------------------------------------------------------------------------------------------------------------------------------#

## HEALTH / ETC_BZ 전망
fn_health_bz_etc(REG_YYMM)
fn_etc_bz(REG_YYMM)

#------------------------------------------------------------------------------------------------------------------------------------#

## HGUM 전망
fn_hgum(REG_YYMM)

#------------------------------------------------------------------------------------------------------------------------------------#

## 적재용 데이터셋 생성
source(paste0(comm_folder_route,'ffa_outg_comm_tbl_reshp.R'))

#------------------------------------------------------------------------------------------------------------------------------------#
