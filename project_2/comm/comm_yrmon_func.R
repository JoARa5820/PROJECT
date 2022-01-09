#########################################
# 연도 -> 월 1/12 : 10년 기준
#########################################
fn_yr_mon <- function(gvn_etc.data){
  # 1. 초기값 세팅
  ym <- data.frame(BASE_YM = 1:120)
  V <- tibble()
  
  # 2. 월 변환
  for(i in 1:ncol(gvn_etc.data)){
    mv <- gvn_etc.data[,i] / 12  # 12개월 1/n
    value <- rep(mv, 12)     # 1/n값 반복 
    
    V <- t(matrix(value, nrow = 10, ncol = 12))
    V <- as.data.frame(V)
    V <- melt(V)
    
    ym <- cbind(ym, V$value)
    colnames(ym)[i+1] <- colnames(gvn_etc.data)[i]
  }
  return(ym)
}

#########################################
# 연도 -> 월 1/12 : 1년 기준
#########################################
fn_bugwa_yr_mon <- function(bugwa.data){
  # 1. 초기값 세팅
  ym <- data.frame(BASE_YM = 1:12)
  V <- tibble()
  
  # 2. 월 변환
  for(i in 1:ncol(bugwa.data)){
    value <- bugwa.data[,i] / 12  # 12개월 1/n
    
    V <- t(matrix(value, nrow = 1, ncol = 12))
    V <- as.data.frame(V)
    V <- melt(V)
    
    ym <- cbind(ym, V$value)
    colnames(ym)[i+1] <- colnames(bugwa.data)[i]
  }
  return(ym)
}

#########################################
# 7월 기준 월 변환 / 12월 기준 월 변환
#########################################
fn_kosis_yrmon <- function(kosis.data, sep_year = c(1,10), cond = c(7, 12)){
  
  # 1. 기준년도 설정
  STD_YY <- kosis.data$BASE_YM[1]
  STD_YY <- paste0(substr(STD_YY,1,4),'-',substr(STD_YY,5,6),'-01')
  
  if(cond == 7){
    
    STD_YY1 <- ymd(STD_YY) + months(5)
    STD_YY2 <- ymd(STD_YY1) + years(sep_year)
    
    if(sep_year == 1){
      STD_YY2 <- ymd(STD_YY2) + years(sep_year)
    }else{
      STD_YY2 <- ymd(STD_YY2) + years(1)
    }
    
  }else {
    STD_YY1 <- ymd(STD_YY) + months(12)
    STD_YY2 <- ymd(STD_YY1) + years(sep_year)
  }
  
  STD_YY1 <- paste0(substr(STD_YY1,1,4), substr(STD_YY1,6,7))
  
  # 2. 저장 데이터 셋 초기화 세팅
  m.value <- data.frame(BASE_YM = seq(ymd(STD_YY), ymd(STD_YY2), by = "1 month"))
  m.value$BASE_YM <- paste0(lubridate::year(m.value$BASE_YM), ifelse(nchar(lubridate::month(m.value$BASE_YM)) == 1, 
                                                                     paste0(0, lubridate::month(m.value$BASE_YM)), 
                                                                     lubridate::month(m.value$BASE_YM)))
  m.value <- left_join(m.value, kosis.data, by = c("BASE_YM" = "BASE_YM"))
  
  # 3. 월 변환
  # 3.1 7월 기준 월 변환
  if(cond == 7){
    
    kosis.data <- kosis.data[substr(kosis.data$BASE_YM,5,6) == '07',]
    rownames(kosis.data) <- 1:nrow(kosis.data)
    
    kosis.data$POP_nrt <- NA
    
    ## 월 분할 로직
    for(k in 1:(nrow(kosis.data)-1)){
      kosis.data[k,'POP_nrt'] <- (kosis.data[k+1,'POP_TT_CNT'] - kosis.data[k,'POP_TT_CNT'])/12
    }
    
    POP_nrt_nujuk <- c()
    
    for(k in 1:(nrow(kosis.data)-1)){
      for(i in 1:12){
        POP_nrt_nujuk <- c(POP_nrt_nujuk,m.value[(m.value$BASE_YM == kosis.data[k,'BASE_YM']),'POP_TT_CNT'] + ((i-1)*kosis.data[k,'POP_nrt']))
      }
    }
    
    POP_nrt_nujuk <- as.data.frame(POP_nrt_nujuk, stringsAsFactors = FALSE)
    
    fin_df <- m.value[m.value$BASE_YM <= kosis.data[nrow(kosis.data),'BASE_YM'],]
    fin_df <- fin_df[-nrow(fin_df),]
    fin_df$POP_TT_CNT <- POP_nrt_nujuk$POP_nrt_nujuk
    
    return(fin_df)
    
  }else {     
    # 3.2 12월 기준 월 변환
    ## 인덱스 설정
    if(sep_year == 1){
      indx <- 13
    }else{ indx <- 133}
    
    m.value <- m.value[1:indx,]
    kosis.data <- subset(kosis.data, BASE_YM <= m.value$BASE_YM[nrow(m.value)])
    
    # 월 분할 로직
    for(j in 2:ncol(kosis.data)){
      
      kosis.data$val_nrt <- NA
      
      for(k in 1:(nrow(kosis.data)-1)){
        kosis.data[k,'val_nrt'] <- (kosis.data[k+1,j] - kosis.data[k,j])/12
      }
      
      val_nrt_nujuk <- c()
      
      for(k in 1:(nrow(kosis.data)-1)){
        for(i in 1:12){
          val_nrt_nujuk <- c(val_nrt_nujuk, m.value[(m.value$BASE_YM == kosis.data[k,'BASE_YM']),j] + ((i-1)*kosis.data[k,'val_nrt']))
        }
      }
      
      val_nrt_nujuk <- c(val_nrt_nujuk, kosis.data[nrow(kosis.data), j])
      val_nrt_nujuk <- as.data.frame(val_nrt_nujuk, stringsAsFactors = FALSE)
      
      m.value[,j] <- val_nrt_nujuk$val_nrt_nujuk
    }
    fin_df <- m.value[-1,] 
  }
  return(fin_df)
}

#########################################
# 해당 연도 값을 모든 월에 동일 적용
#########################################
fn_sal_yrmon <- function(lowSal.data){
  # 1. 초기값 세팅
  sal <- tibble()
  sal.fin <- tibble(TMP = 1:(nrow(lowSal.data)*12))
  
  # 2. 월 변환
  for(i in 1:length(lowSal.data)){
    for(j in 1:nrow(lowSal.data)){
      temp <- data.frame(X = rep(lowSal.data[j,i], 12))
      sal <- rbind(sal, temp)
    }  
    sal.fin <- bind_cols(sal.fin, sal)
    sal <- tibble()
  }
  return(sal.fin)
}

#########################################
# 분기 -> 월 1/3
#########################################
fn_quat_mon <- function(quater.data){
  # 1. 초기값 세팅
  quat <- tibble()
  quat.fin <- tibble(TMP = 1:(nrow(quater.data)*3))
  
  # 2. 월 변환
  for(i in 2:length(quater.data)){
    for(j in 1:nrow(quater.data)){
      temp <- data.frame(X = rep(quater.data[j,i]/3, 3))
      quat <- rbind(quat, temp)
    }  
    quat.fin <- bind_cols(quat.fin, quat)
    quat <- tibble()
  }
  return(quat.fin)
}




