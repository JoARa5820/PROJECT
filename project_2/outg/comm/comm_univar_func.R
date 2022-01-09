### 단변량 함수
## 변수 유의성검증 (상관계수, 다항회귀)

# 환경 설정 
options(scipen = 100)  # 지수표기를 숫자표기로 바꾸는 옵션
set.seed(1234)


library(data.table)
library(dplyr)
library(broom)


fn_univar <- function(use_dataset,y_col){
  
  # 단변량 할 데이터셋 : use_dataset
  dataset <- use_dataset
  
  # Inf 처리
  is.na(dataset) <- sapply(dataset, is.infinite)
  dataset[is.na(dataset)] <- 0
  
  
  # 시차 맞춰주기(y를 한칸 앞으로)
  dataset[,y_col] <- shift(dataset[,y_col], n = -1)
  
  
  # input_coll : YEAR, MONTH, target 제외한 컬럼
  input_coll <- colnames(dataset)
  input_coll <- input_coll[!input_coll %in% c('YYMM',y_col)]
  
  ################################################################################################################################
  
  # 내부,외부 데이터 log 변환
  # v_list <- c('CNT', 'AMT', 'GDP', 'GNI', 'RAIN', 'TEMP', 'WIND')
  # v_list <- dataset[,stri_detect_regex(names(dataset), paste0(v_list, '$', collapse = '|'))]
  # 
  # except_v_list <- dataset[!colnames(dataset) %in% colnames(v_list)]
  # include_v_list <- dataset[colnames(dataset) %in% colnames(v_list)]
  # 
  # include_v_list <- log(include_v_list)
  # 
  # dataset <- cbind(except_v_list, include_v_list)
  # is.na(dataset) <- sapply(dataset, is.infinite)  # log 변환 과정에서 생성된 NaN값 처리
  # dataset[is.na(dataset)] <- 0
  
  ################################################################################################################################
  
  ## 상관관계 확인
  nujuk_df <- data.frame(target = NA, outside_x = NA, correlation = NA)
  
  
  for(input_col in input_coll){
    
    tmp <- cor(dataset[c(1:(nrow(dataset)-1)),y_col], dataset[c(1:(nrow(dataset)-1)),input_col])
    tmp2 <- cbind(as.data.frame(y_col), as.data.frame(input_col), as.data.frame(tmp))
    names(tmp2) <- c('target','outside_x','correlation')
    nujuk_df <- rbind(nujuk_df, tmp2)
  }
  
  nujuk_df <- nujuk_df[c(2:nrow(nujuk_df)),]
  colnames(nujuk_df) <- c('y_col','input_col','correlation')
  cor_result <- nujuk_df
  
  ################################################################################################################################
  
  # 1차 회귀식
  lm_num <- 1  # 차수
  
  nujuk_df <- data.frame(y_col = NA, input_col = NA, r.squared = NA, adj.r.squared = NA, sigma = NA, statistic = NA, p.value = NA, df = NA)
  
  for(input_col in input_coll){
    
    lm_model <- lm(dataset[c(1:(nrow(dataset)-1)),y_col] ~ poly(dataset[c(1:(nrow(dataset)-1)),input_col], lm_num, raw = TRUE))
    lm_summary <- summary(lm_model)
    lm_summary <- as.data.frame(lm_summary %>% glance())
    lm_summary <- cbind(as.data.frame(y_col), as.data.frame(input_col), lm_summary)
    nujuk_df <- rbind(nujuk_df, lm_summary)
  }
  
  nujuk_df <- nujuk_df[2:nrow(nujuk_df),]
  nujuk_df$poly <- lm_num
  rownames(nujuk_df) <- 1:nrow(nujuk_df)
  assign(paste0('poly',lm_num,'_outside'), nujuk_df)
  
  #------------------------------------------------------------------------------#
  
  # 2차 회귀식
  lm_num <- 2  # 차수
  
  nujuk_df <- data.frame(y_col = NA, input_col = NA, r.squared = NA, adj.r.squared = NA, sigma = NA, statistic = NA, p.value = NA, df = NA)
  
  for(input_col in input_coll){
    
    lm_model <- lm(dataset[c(1:(nrow(dataset)-1)),y_col] ~ poly(dataset[c(1:(nrow(dataset)-1)),input_col], lm_num, raw = TRUE))
    lm_summary <- summary(lm_model)
    lm_summary <- as.data.frame(lm_summary %>% glance())
    lm_summary <- cbind(as.data.frame(y_col), as.data.frame(input_col), lm_summary)
    nujuk_df <- rbind(nujuk_df, lm_summary)
  }
  
  nujuk_df <- nujuk_df[2:nrow(nujuk_df),]
  nujuk_df$poly <- lm_num
  rownames(nujuk_df) <- 1:nrow(nujuk_df)
  assign(paste0('poly',lm_num,'_outside'), nujuk_df)
  
  #------------------------------------------------------------------------------#
  
  # 3차 회귀식
  lm_num <- 3  # 차수
  
  nujuk_df <- data.frame(y_col = NA, input_col = NA, r.squared = NA, adj.r.squared = NA, sigma = NA, statistic = NA, p.value = NA, df = NA)
  
  for(input_col in input_coll){
    
    lm_model <- lm(dataset[c(1:(nrow(dataset)-1)),y_col] ~ poly(dataset[c(1:(nrow(dataset)-1)),input_col], lm_num, raw = TRUE))
    lm_summary <- summary(lm_model)
    lm_summary <- as.data.frame(lm_summary %>% glance())
    lm_summary <- cbind(as.data.frame(y_col), as.data.frame(input_col), lm_summary)
    nujuk_df <- rbind(nujuk_df, lm_summary)
  }
  
  nujuk_df <- nujuk_df[2:nrow(nujuk_df),]
  nujuk_df$poly <- lm_num
  rownames(nujuk_df) <- 1:nrow(nujuk_df)
  assign(paste0('poly',lm_num,'_outside'), nujuk_df)
  
  #------------------------------------------------------------------------------#
  
  # 1,2,3차 회귀 결과 합치기
  poly1_outside <- poly1_outside[,c('y_col', 'input_col', 'adj.r.squared', 'p.value', 'poly')]
  names(poly1_outside) <- c('y_col', 'input_col', 'adj.r.squared_1', 'p.value_1', 'poly_1')
  poly2_outside <- poly2_outside[,c('y_col', 'input_col', 'adj.r.squared', 'p.value', 'poly')]
  names(poly2_outside) <- c('y_col', 'input_col', 'adj.r.squared_2', 'p.value_2', 'poly_2')
  poly3_outside <- poly3_outside[,c('y_col', 'input_col', 'adj.r.squared', 'p.value', 'poly')]
  names(poly3_outside) <- c('y_col', 'input_col', 'adj.r.squared_3', 'p.value_3', 'poly_3')
  
  
  m_poly_re <- merge(poly1_outside, poly2_outside, by = c('y_col','input_col'))
  m_poly_re <- merge(m_poly_re, poly3_outside, by = c('y_col','input_col'))
  
  
  # R_squared, p_value 소수점 4자리에서 끊기
  m_poly_re[,c('adj.r.squared_1','adj.r.squared_2','adj.r.squared_3')] <- round(m_poly_re[,c('adj.r.squared_1','adj.r.squared_2','adj.r.squared_3')],4)
  m_poly_re[,c('p.value_1','p.value_2','p.value_3')] <- round(m_poly_re[,c('p.value_1','p.value_2','p.value_3')],4)
  poly_result <- m_poly_re
  
  ################################################################################################################################
  
  # 상관분석 결과와 다항회귀 결과 합치기
  cor_poly_result <- merge(cor_result, poly_result, by = c('y_col', 'input_col'))
  
  cor_poly_result <- cor_poly_result[,c('y_col','input_col','correlation',
                                       'adj.r.squared_1','adj.r.squared_2','adj.r.squared_3',
                                       'p.value_1','p.value_2','p.value_3')]
  
  rownames(cor_poly_result) <- 1:nrow(cor_poly_result)
  
  # max r-squared 생성
  nujuk_df <- character()
  for(i in 1:nrow(cor_poly_result)){
    nujuk_df <- append(nujuk_df, max(cor_poly_result[i,c('adj.r.squared_1','adj.r.squared_2','adj.r.squared_3')]))
  }
  cor_poly_result$max_r_squared <- nujuk_df
  
  
  # 독립변수 중 YEAR,MONTH 제외
  cor_poly_result <- cor_poly_result[!cor_poly_result$input_col %in% c('YYMM'),]
  
  
  # max r로 정렬
  cor_poly_result <- cor_poly_result[order(cor_poly_result$max_r_squared, decreasing = TRUE),]
  rownames(cor_poly_result) <- 1:nrow(cor_poly_result)
  
  
  # NA 행 제거
  cor_poly_result <- cor_poly_result[!is.na(cor_poly_result$input_col),]
  
  
  # max차수 컬럼 생성
  cor_poly_result$poly <- NA
  
  r_numm <- nrow(cor_poly_result)
  
  for(r_num in 1:r_numm){
    if(cor_poly_result[r_num,'max_r_squared'] == cor_poly_result[r_num,'adj.r.squared_1']){
      cor_poly_result[r_num,'poly'] <- 1
    }else if(cor_poly_result[r_num,'max_r_squared'] == cor_poly_result[r_num,'adj.r.squared_2']){
      cor_poly_result[r_num,'poly'] <- 2
    }else if(cor_poly_result[r_num,'max_r_squared'] == cor_poly_result[r_num,'adj.r.squared_3']){
      cor_poly_result[r_num,'poly'] <- 3
    }
  }
  
  return(cor_poly_result)
}

