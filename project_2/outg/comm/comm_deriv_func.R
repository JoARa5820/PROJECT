# 환경 설정 
options(scipen = 100)  # 지수표기를 숫자표기로 바꾸는 옵션
set.seed(1234)


library(data.table)
library(dplyr)
library(broom)
library(stringi)


# 파생변수 생성 함수 호출

# 1) 단위 파생변수
fn_deriv_amt <- function(data){
  
  var_list <- c('CNT', 'AMT', 'GDP', 'GNI', 'RAIN', 'TEMP', 'WIND')
  
  ### 단위 변수 리스트
  amt_data <- data[,stri_detect_regex(names(data), paste0(var_list, '$', collapse = '|'))]

  
  if(is.data.frame(amt_data)){
    names(amt_data) <- names(data)[stri_detect_regex(names(data), paste0(var_list, '$', collapse = '|'))]
  }else{
    amt_data <- as.data.frame(amt_data)
    names(amt_data) <- names(data)[stri_detect_regex(names(data), paste0(var_list, '$', collapse = '|'))]
  }
  
  
  cnt <- ncol(amt_data)                  # 변수명 생성을 위한 cnt
  sq <- seq(cnt+1, cnt+cnt*27, by = 27)  # 27 : 생성변수 개수
  
  ### 생성
  for(i in 1:length(amt_data)){  
    
    # log(단위)
    amt_data$LOG <- log(amt_data[,i]) 
    
    for(j in 1:nrow(amt_data)){
      
      # 최근 3개월 평균(Log)
      amt_data$M3_AVG[j] <- ifelse(j <= 2, 0, mean(amt_data$LOG[(j-2):j]))
      
      # 최근 6개월 평균(Log)
      amt_data$M6_AVG[j] <- ifelse(j <= 5, 0, mean(amt_data$LOG[(j-5):j]))
      
      # 최근 1년 평균(Log)
      amt_data$Y1_AVG[j] <- ifelse(j <= 11, 0, mean(amt_data$LOG[(j-11):j]))
      
    }
    
    # 1개월 대비 3개월 비율
    amt_data$M1_M3_RT <- amt_data$LOG / amt_data$M3_AVG
    
    # 1개월 대비 6개월 비율
    amt_data$M1_M6_RT <- amt_data$LOG / amt_data$M6_AVG
    
    # 1개월 대비 12개월 비율
    amt_data$M1_M12_RT <- amt_data$LOG / amt_data$Y1_AVG
    
    # 3개월 대비 6개월 비율
    amt_data$M3_M6_RT <- amt_data$M3_AVG / amt_data$M6_AVG
    
    # 6개월 대비 12개월 비율
    amt_data$M6_M12_RT <- amt_data$M6_AVG / amt_data$Y1_AVG
    
    for(j in 1:nrow(amt_data)){
      
      # 전년동월대비증감율
      amt_data$YM1_UPDW[j] <- ifelse(j <= 12, 0, (amt_data$LOG[j] / amt_data$LOG[j-12]) - 1)
      
      # 전년동월대비증감율 최근 3개월 평균
      amt_data$YM1_UPDW.M3_AVG[j] <- ifelse(j <= 14, 0, mean(amt_data$YM1_UPDW[(j-2):j]))
      
      # 전년동월대비증감율 최근 6개월 평균
      amt_data$YM1_UPDW.M6_AVG[j] <- ifelse(j <= 17, 0, mean(amt_data$YM1_UPDW[(j-5):j]))
      
      # 전년동월대비증감율 최근 1년 평균
      amt_data$YM1_UPDW.Y1_AVG[j] <- ifelse(j <= 23, 0, mean(amt_data$YM1_UPDW[(j-11):j]))
      
    }
    
    # 전년동월대비증감율 1개월 대비 3개월 증감
    amt_data$YM1_UPDW.M1_M3_DIFF <- amt_data$YM1_UPDW - amt_data$YM1_UPDW.M3_AVG
    
    # 전년동월대비증감율 1개월 대비 6개월 증감
    amt_data$YM1_UPDW.M1_M6_DIFF <- amt_data$YM1_UPDW - amt_data$YM1_UPDW.M6_AVG
    
    # 전년동월대비증감율 1개월 대비 12개월 증감
    amt_data$YM1_UPDW.M1_M12_DIFF <- amt_data$YM1_UPDW - amt_data$YM1_UPDW.Y1_AVG
    
    # 전년동월대비증감율 3개월 대비 6개월 증감
    amt_data$YM1_UPDW.M3_M6_DIFF <- amt_data$YM1_UPDW.M3_AVG - amt_data$YM1_UPDW.M6_AVG
    
    # 전년동월대비증감율 6개월 대비 12개월 증감
    amt_data$YM1_UPDW.M6_M12_DIFF <- amt_data$YM1_UPDW.M6_AVG - amt_data$YM1_UPDW.Y1_AVG
    
    for(j in 1:nrow(amt_data)){
      
      # 전월대비 증감율
      amt_data$BF1M_UPDW[j] <- ifelse(j == 1, 0, amt_data$LOG[j] / amt_data$LOG[j-1] - 1)
      
      # 전월대비 증감율 최근 3개월 평균
      amt_data$BF1M_UPDW.M3_AVG[j] <- ifelse(j <= 3, 0, mean(amt_data$BF1M_UPDW[(j-2):j]))
      
      # 전월대비 증감율 최근 6개월 평균
      amt_data$BF1M_UPDW.M6_AVG[j] <- ifelse(j <= 6, 0, mean(amt_data$BF1M_UPDW[(j-5):j]))
      
      # 전월대비 증감율 최근 1년 평균
      amt_data$BF1M_UPDW.Y1_AVG[j] <- ifelse(j <= 12, 0, mean(amt_data$BF1M_UPDW[(j-11):j]))
      
    }
    
    # 전월대비 증감율 1개월 대비 3개월 증감
    amt_data$BF1M_UPDW.M1_M3_DIFF <- amt_data$BF1M_UPDW - amt_data$BF1M_UPDW.M3_AVG
    
    # 전월대비 증감율 1개월 대비 6개월 증감
    amt_data$BF1M_UPDW.M1_M6_DIFF <- amt_data$BF1M_UPDW - amt_data$BF1M_UPDW.M6_AVG
    
    # 전월대비 증감율 1개월 대비 12개월 증감
    amt_data$BF1M_UPDW.M1_M12_DIFF <- amt_data$BF1M_UPDW - amt_data$BF1M_UPDW.Y1_AVG
    
    # 전월대비 증감율 3개월 대비 6개월 증감
    amt_data$BF1M_UPDW.M3_M6_DIFF <- amt_data$BF1M_UPDW.M3_AVG - amt_data$BF1M_UPDW.M6_AVG
    
    # 전월대비 증감율 6개월 대비 12개월 증감
    amt_data$BF1M_UPDW.M6_M12_DIFF <- amt_data$BF1M_UPDW.M6_AVG - amt_data$BF1M_UPDW.Y1_AVG
    
    
    ### 변수명 변경(loop에서 변수명 중복 방지)
    if(i == 1){
      colnames(amt_data)[-(1:cnt)] <- paste0('V',i,'.',colnames(amt_data)[-(1:cnt)])
    }else{
      colnames(amt_data)[-(1:(sq[i]-1))] <- paste0('V',i,'.',colnames(amt_data)[-(1:(sq[i]-1))])
    }
  }
  # 결과 출력
  return(amt_data)
}

#------------------------------------------------------------------------------------------------------------------------------#

# 2) 비율 파생변수
fn_deriv_rt <- function(data){
  
  var_list <- c('RT', 'CSI')
  
  # 비율 변수 리스트
  rt_data <- data[,stri_detect_regex(names(data), paste0(var_list, '$', collapse = '|'))]
  
  
  if(is.data.frame(rt_data)){
    names(rt_data) <- names(data)[stri_detect_regex(names(data), paste0(var_list, '$', collapse = '|'))]
  }else{
    rt_data <- as.data.frame(rt_data)
    names(rt_data) <- names(data)[stri_detect_regex(names(data), paste0(var_list, '$', collapse = '|'))]
  }

  
  cnt <- ncol(rt_data)               # 변수명 생성을 위한 cnt
  sq <- seq(cnt+1, cnt+cnt*16, by = 16)  # 16 : 생성변수 개수
  
  # 생성
  for(i in 1:length(rt_data)){  
    
    for(j in 1:nrow(rt_data)){
      
      # 최근 3개월 평균
      rt_data$M3_AVG[j] <- ifelse(j <= 2, 0, mean(rt_data[,i][(j-2):j]))
      
      # 최근 6개월 평균
      rt_data$M6_AVG[j] <- ifelse(j <= 5, 0, mean(rt_data[,i][(j-5):j]))
      
      # 최근 1년 평균
      rt_data$Y1_AVG[j] <- ifelse(j <= 11, 0, mean(rt_data[,i][(j-11):j]))
      
    }
    
    # 1개월 대비 3개월 증감
    rt_data$M1_M3_DIFF <- rt_data[,i] - rt_data$M3_AVG
    
    # 1개월 대비 6개월 증감
    rt_data$M1_M6_DIFF <- rt_data[,i] - rt_data$M6_AVG
    
    # 1개월 대비 12개월 증감
    rt_data$M1_M12_DIFF <- rt_data[,i] - rt_data$Y1_AVG
    
    # 3개월 대비 6개월 증감
    rt_data$M3_M6_DIFF <- rt_data$M3_AVG - rt_data$M6_AVG
    
    # 6개월 대비 12개월 증감
    rt_data$M6_M12_DIFF <- rt_data$M6_AVG - rt_data$Y1_AVG
    
    for(j in 1:nrow(rt_data)){
      
      # 전년동월대비증감
      rt_data$YM1_UPDW[j] <- ifelse(j <= 12, 0, rt_data[,i][j] - rt_data[,i][j-12])
      
      # 전년동월대비증감 최근3개월 평균
      rt_data$YM1_UPDW.M3_AVG[j] <- ifelse(j <= 14, 0, mean(rt_data$YM1_UPDW[(j-2):j]))
      
      # 전년동월대비증감 최근6개월 평균
      rt_data$YM1_UPDW.M6_AVG[j] <- ifelse(j <= 17, 0, mean(rt_data$YM1_UPDW[(j-5):j]))
      
      # 전년동월대비증감 최근1년 평균
      rt_data$YM1_UPDW.Y1_AVG[j] <- ifelse(j <= 23, 0, mean(rt_data$YM1_UPDW[(j-11):j]))
    }
    
    
    for(j in 1:nrow(rt_data)){
      
      # 전월대비 증감
      rt_data$BF1M_UPDW[j] <-  ifelse(j == 0, 0, rt_data[j,i] - rt_data[(j-1),i])
      
      # 전월대비 증감 최근3개월 평균
      rt_data$BF1M_UPDW.M3_AVG[j] <- ifelse(j <= 3, 0, mean(rt_data$YM1_UPDW[(j-2):j]))
      
      # 전월대비 증감 최근6개월 평균
      rt_data$BF1M_UPDW.M6_AVG[j] <- ifelse(j <= 6, 0, mean(rt_data$YM1_UPDW[(j-5):j]))
      
      # 전월대비 증감 최근1년 평균
      rt_data$BF1M_UPDW.Y1_AVG[j] <- ifelse(j <= 12, 0, mean(rt_data$YM1_UPDW[(j-11):j]))
      
    }
    
    ### 변수명 변경(loop에서 변수명 중복 방지)
    if(i == 1){
      colnames(rt_data)[-(1:cnt)] <- paste0('R',i,'.',colnames(rt_data)[-(1:cnt)])
    }else{
      colnames(rt_data)[-(1:(sq[i]-1))] <- paste0('R',i,'.',colnames(rt_data)[-(1:(sq[i]-1))])
    }
  }
  # 결과 출력
  return(rt_data)
}