rm(list=ls())
library(readr)

# 加载Rdata文件
load("share.Rdata")
load("hrs.Rdata")
load("charls.Rdata")
load("klosa.Rdata")
ukb <- read.csv("ukb预后.csv")


#
library(haven)
charls <- zap_labels(charls)
hrs <- zap_labels(hrs)
share <- zap_labels(share)
klosa <- zap_labels(klosa)


charls_func_vars <- c('dressa', 'batha', 'eata', 'beda', 'toilta', 'urina', 
                      'housewka', 'mealsa', 'shopa', 'phonea', 'medsa', 'moneya')

charls <- charls %>%
  # 步骤1: 创建年龄分组 (age_group)
  mutate(
    age_group = case_when(
      age < 50 ~ "<50",
      age >= 50 & age <= 54 ~ "50-54",
      age >= 55 & age <= 59 ~ "55-59",
      age >= 60 & age <= 64 ~ "60-64",
      age >= 65 & age <= 69 ~ "65-69",
      age >= 70 & age <= 74 ~ "70-74",
      age >= 75 & age <= 79 ~ "75-79",
      TRUE ~ NA_character_ # 其他年龄范围不在此次定义中
    )
  ) %>%
  # 过滤掉没有明确年龄组的观测
  filter(!is.na(age_group)) %>%
  # 步骤2: 按 wave 和 age_group 分组
  group_by(wave, age_group) %>%
  # 步骤3 & 4: 计算阈值并定义认知/功能障碍
  mutate(
    # 计算认知障碍阈值 (低于1.5倍标准差)
    thresh_ser7   = mean(ser7, na.rm = TRUE) - 1.5 * sd(ser7, na.rm = TRUE),
    thresh_orient = mean(orient, na.rm = TRUE) - 1.5 * sd(orient, na.rm = TRUE),
    thresh_tr20   = mean(tr20, na.rm = TRUE) - 1.5 * sd(tr20, na.rm = TRUE),
    
    # 定义认知障碍 (cognitive_impairment)，任意一项低于阈值即为1
    cognitive_impairment = if_else(
      ser7 < thresh_ser7 | orient < thresh_orient | tr20 < thresh_tr20, 1, 0
    ),
    
    # 计算功能障碍总分 (处理变量可能不存在或为NA的情况)
    # rowSums可以优雅地处理NA，na.rm=TRUE表示计算时忽略NA值
    func_score = rowSums(across(all_of(charls_func_vars)), na.rm = TRUE),
    
    # 计算功能障碍阈值 (高于1.5倍标准差)
    thresh_func = mean(func_score, na.rm = TRUE) + 1.5 * sd(func_score, na.rm = TRUE),
    
    # 定义功能障碍 (functional_impairment)
    functional_impairment = if_else(func_score > thresh_func, 1, 0)
    
  ) %>%
  # 解除分组，以便进行后续操作
  ungroup() %>%
  # 步骤5: 定义 new_dementia
  mutate(
    memrye = if_else(
      cognitive_impairment == 1 & functional_impairment == 1, 1, 0
    )
  )


share_func_vars <- c('walkra', 'dressa', 'batha', 'eata', 'beda', 'toilta', 
                     'phonea', 'medsa', 'moneya', 'shopa', 'mealsa', 'mapa', 
                     'housewka', 'leavhsa', 'laundrya')

share <- share %>%
  # 步骤1: 创建年龄分组 (age_group)，注意share的年龄变量是 agey
  mutate(
    age_group = case_when(
      agey < 50 ~ "<50",
      agey >= 50 & agey <= 54 ~ "50-54",
      agey >= 55 & agey <= 59 ~ "55-59",
      agey >= 60 & agey <= 64 ~ "60-64",
      agey >= 65 & agey <= 69 ~ "65-69",
      agey >= 70 & agey <= 74 ~ "70-74",
      agey >= 75 & agey <= 79 ~ "75-79",
      TRUE ~ NA_character_ # 其他年龄范围不在此次定义中
    )
  ) %>%
  # 过滤掉没有明确年龄组的观测
  filter(!is.na(age_group)) %>%
  # 步骤2: 按 wave 和 age_group 分组
  group_by(wave, age_group) %>%
  # 步骤3 & 4: 计算阈值并定义认知/功能障碍
  mutate(
    # 计算认知障碍阈值 (低于1.5倍标准差)
    thresh_ser7   = mean(ser7, na.rm = TRUE) - 1.5 * sd(ser7, na.rm = TRUE),
    thresh_orient = mean(orient, na.rm = TRUE) - 1.5 * sd(orient, na.rm = TRUE),
    thresh_tr20   = mean(tr20, na.rm = TRUE) - 1.5 * sd(tr20, na.rm = TRUE),
    
    # 定义认知障碍 (cognitive_impairment)
    cognitive_impairment = if_else(
      ser7 < thresh_ser7 | orient < thresh_orient | tr20 < thresh_tr20, 1, 0
    ),
    
    # 计算功能障碍总分
    func_score = rowSums(across(all_of(share_func_vars)), na.rm = TRUE),
    
    # 计算功能障碍阈值 (高于1.5倍标准差)
    thresh_func = mean(func_score, na.rm = TRUE) + 1.5 * sd(func_score, na.rm = TRUE),
    
    # 定义功能障碍 (functional_impairment)
    functional_impairment = if_else(func_score > thresh_func, 1, 0)
    
  ) %>%
  # 解除分组
  ungroup() %>%
  # 步骤5: 定义 new_dementia
  mutate(
    alzdeme = if_else(
      cognitive_impairment == 1 & functional_impairment == 1, 1, 0
    )
  )

hrs <- hrs %>%
  mutate(
    alzdeme = if_else(cog27 <= 6, 1, 0)
  )


library(dplyr)
klosa <- klosa %>%
  mutate(work1 = case_when(
    is.na(slfemp) & is.na(work) ~ NA_real_,       # 两个都是 NA
    slfemp == 1 | work == 1 ~ 1,                  # 只要有一个是 1
    slfemp == 0 | work == 0 ~ 0,                  # 两个都是 0
    TRUE ~ NA_real_                               # 其他情况（如一个是 0，一个是 NA）
  ))

hrs <- hrs %>%
  mutate(work1 = case_when(
    is.na(slfemp) & is.na(work) ~ NA_real_,       # 两个都是 NA
    slfemp == 1 | work == 1 ~ 1,                  # 只要有一个是 1
    slfemp == 0 | work == 0 ~ 0,                  # 两个都是 0
    TRUE ~ NA_real_                               # 其他情况（如一个是 0，一个是 NA）
  ))

share <- share %>%
  mutate(work1 = case_when(
    is.na(slfemp) & is.na(work) ~ NA_real_,       # 两个都是 NA
    slfemp == 1 | work == 1 ~ 1,                  # 只要有一个是 1
    slfemp == 0 & work == 0 ~ 0,                  # 两个都是 0
    TRUE ~ NA_real_                               # 其他情况（如一个是 0，一个是 NA）
  ))


ukb$Urban_Rural_Binary <- ifelse(ukb$Urban_Rural_Binary == 1, 0, 
                                 ifelse(ukb$Urban_Rural_Binary == 0, 1, ukb$Urban_Rural_Binary))

# 疼痛
charls$pain <- apply(charls[, c("da042s1", "da042s2", "da042s3", "da042s4", "da042s5", 
                                "da042s6", "da042s7", "da042s8", "da042s9", "da042s10", 
                                "da042s11", "da042s12", "da042s13", "da042s14", "da042s15")], 
                     1, function(x) {
                       if (all(is.na(x))) {
                         return(NA)
                       } else if (any(x == 1, na.rm = TRUE)) {
                         return(1)
                       } else {
                         return(0)
                       }
                     })

hrs$pain <- apply(hrs[, c("back", "headache", "backp")], 
                  1, function(x) {
                    if (all(is.na(x))) {
                      return(NA)
                    } else if (any(x == 1, na.rm = TRUE)) {
                      return(1)
                    } else {
                      return(0)
                    }
                  })

klosa$pain <- apply(klosa[, paste0("pain_", 1:13)], 1, function(x) {
  if (all(is.na(x))) {
    return(NA)
  } else if (any(x == 1, na.rm = TRUE)) {
    return(1)
  } else if (all(x == 0, na.rm = TRUE)) {
    return(0)
  } else {
    return(NA)
  }
})


# HRS数据集
hrs$mstath <- ifelse(hrs$mstath == 1, 1, 
                     ifelse(is.na(hrs$mstath), NA, 0))

# CHARLS数据集
charls$marry <- ifelse(charls$marry == 1, 1, 
                       ifelse(is.na(charls$marry), NA, 0))

# KLOSA数据集
klosa$mstath <- ifelse(klosa$mstath == 1, 1, 
                       ifelse(is.na(klosa$mstath), NA, 0))

# SHARE数据集
share$mstath <- ifelse(share$mstath == 1, 1, 
                       ifelse(is.na(share$mstath), NA, 0))




# 加载必要的包，开始清洗数据#############################
library(dplyr)
# ==========【HRS数据处理（基线波次：10）】==========
# Step 1: 提取wave=10基线数据（保留所有列）
hrs_baseline <- hrs %>% filter(wave == 10)

# Step 2: 计算死亡状态和随访时间
hrs_survival <- hrs %>%
  group_by(hhidpn) %>%
  summarise(
    death = as.integer(any(!is.na(radyear))),
    baseline_year = first(iwendy [wave == 10]),
    death_year = min(radyear, na.rm = TRUE),
    last_year = max(iwendy, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    death_year = ifelse(is.infinite(death_year), NA, death_year),
    time = ifelse(death == 1, death_year - baseline_year, last_year - baseline_year),
    time = pmax(time, 0, na.rm = TRUE)  # 确保时间非负
  ) %>%
  select(hhidpn, death, time)

# Step 3: 合并结果
hrs_processed_data <- hrs_baseline %>%
  left_join(hrs_survival, by = "hhidpn")

# Step 4: 查看结果
print(paste("HRS样本数:", nrow(hrs_processed_data)))
print("HRS死亡状态:")
print(table(hrs_processed_data$death))
# 清理中间对象
rm(hrs_baseline, hrs_survival)

# ==========【SHARE数据处理（基线波次：2）】==========
# Step 1: 提取wave=2基线数据（保留所有列）
share_baseline <- share %>% filter(wave == 2)

# Step 2: 计算死亡状态和随访时间
share_survival <- share %>%
  group_by(mergeid) %>%
  summarise(
    death = as.integer(any(!is.na(radyear))),
    baseline_year = first(iwy[wave == 2]),
    death_year = min(radyear, na.rm = TRUE),
    last_year = max(iwy, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    death_year = ifelse(is.infinite(death_year), NA, death_year),
    time = ifelse(death == 1, death_year - baseline_year, last_year - baseline_year),
    time = pmax(time, 0, na.rm = TRUE)  # 确保时间非负
  ) %>%
  select(mergeid, death, time)

# Step 3: 合并结果
share_processed_data <- share_baseline %>%
  left_join(share_survival, by = "mergeid")

# Step 4: 查看结果
print(paste("SHARE样本数:", nrow(share_processed_data)))
print("SHARE死亡状态:")
print(table(share_processed_data$death))

# 清理中间对象
rm(share_baseline, share_survival)

# ==========【KLOSA数据处理（基线波次：3）】==========
# Step 1: 提取wave=3基线数据（保留所有列）
klosa_baseline <- klosa %>% filter(wave == 3)

# Step 2: 计算死亡状态和随访时间
klosa_survival <- klosa %>%
  group_by(pid) %>%
  summarise(
    death = as.integer(any(!is.na(radyear))),
    baseline_year = first(iwy[wave == 3]),
    death_year = min(radyear, na.rm = TRUE),
    last_year = max(iwy, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    death_year = ifelse(is.infinite(death_year), NA, death_year),
    time = ifelse(death == 1, death_year - baseline_year, last_year - baseline_year),
    time = pmax(time, 0, na.rm = TRUE)  # 确保时间非负
  ) %>%
  select(pid, death, time)

# Step 3: 合并结果
klosa_processed_data <- klosa_baseline %>%
  left_join(klosa_survival, by = "pid")

# Step 4: 查看结果
print(paste("KLOSA样本数:", nrow(klosa_processed_data)))
print("KLOSA死亡状态:")
print(table(klosa_processed_data$death))

# 清理中间对象
rm(klosa_baseline, klosa_survival)


# ==========【CHARLS数据处理（基线波次：1）】==========
# Step 1: 提取wave=1基线数据（保留所有列）
charls_baseline <- charls %>% filter(wave == 1)

# Step 2: 计算死亡状态和随访时间
charls_survival <- charls %>%
  group_by(ID) %>%
  summarise(
    death = as.integer(any(!is.na(radyear))),
    baseline_year = first(iwy[wave == 1]),
    death_year = min(radyear, na.rm = TRUE),
    last_year = max(iwy, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    death_year = ifelse(is.infinite(death_year), NA, death_year),
    time = ifelse(death == 1, death_year - baseline_year, last_year - baseline_year),
    time = pmax(time, 0, na.rm = TRUE)  # 确保时间非负
  ) %>%
  select(ID, death, time)

# Step 3: 合并结果
charls_processed_data <- charls_baseline %>%
  left_join(charls_survival, by = "ID")

# Step 4: 查看结果
print(paste("CHARLS样本数:", nrow(charls_processed_data)))
print("CHARLS死亡状态:")
print(table(charls_processed_data$death))

# 清理中间对象
rm(charls_baseline, charls_survival)



# 筛选CHARLS数据：年龄<65岁且痴呆=1
charls_wave <- charls_processed_data %>%
  filter(age < 65 & memrye == 1)

# 筛选HRS数据：年龄<65岁且痴呆=1
hrs_wave <- hrs_processed_data %>%
  filter(ragey_m < 65 & alzdeme == 1)

# 筛选SHARE数据：年龄<65岁且痴呆=1
share_wave <- share_processed_data %>%
  filter(agey < 65 & alzdeme == 1)

# 筛选KLOSA数据：年龄<65岁且痴呆=1
klosa_wave <- klosa_processed_data %>%
  filter(agey < 65 & dementia == 1)


#筛选ukb数据
# 添加death列：Age.at.death...Instance.为NA则death=0，非NA则death=1
ukb$death <- ifelse(is.na(ukb$Age.at.death...Instance.0), 0, 1)
# 添加time列
# 1. 计算调查时的年份 = Year.of.birth + Age.at.recruitment
ukb$recruitment_year <- ukb$Year.of.birth + ukb$Age.at.recruitment

# 2. 根据death状态计算time
# 如果death=0（没死），time = 2024 - 调查年份
# 如果death=1（死了），time = Age.at.death...Instance. - Age.at.recruitment
ukb$time <- ifelse(ukb$death == 0, 
                   2024 - ukb$recruitment_year, 
                   ukb$Age.at.death...Instance.0 - ukb$Age.at.recruitment)
ukb$time <- round(ukb$time)
# 选择指定列并按要求排序
ukb <- ukb[, c("sample", "death", "time", "Age.at.recruitment", 
               "Education_group", "Sex", "Hypertension", "Stroke", 
               "Hyperglycemia", "Cancer", "lung", "Heart", "Joint", 
               "Psychiatry", "Standing.height...Instance.0", 
               "Weight...Instance.0", "Past.tobacco.smoking...Instance.0", 
               "Current.tobacco.smoking...Instance.0", 
               "Alcohol.drinker.status...Instance.0", 
               "Body.mass.index..BMI....Instance.0", 
               "Number.in.household...Instance.0", 
               "employed", "Hearing.aid.user...Instance.0","Urban_Rural_Binary",
               "Hand.grip.strength..left....Instance.0", 
               "Hand.grip.strength..right....Instance.0", 
               "Number.of.days.week.of.vigorous.physical.activity.10..minutes...Instance.0", 
              "Pain_experienced_binary", 
               "Has_partner_in_household")]




###############################################




# 定义每个数据对应要保留的列名
charls_cols <- c("ID", "death","time", "age", "raeducl", "ragender", "hibpe", "stroke", "diabe", "cancre",
                 "lunge", "hearte", "arthre", "psyche", "mheight", "mweight",
                 "smokev", "smoken", "drinkev", "bmi",
                 "family_size", "work","hear_aid","hrural", "lgrip1", "rgrip1", "vgactx_c", "pain", "marry")

hrs_cols <- c("hhidpn","death","time", "ragey_m", "raeducl", "ragender", "hibpe","stroke", "diabe", "cancre",
              "lunge", "hearte", "arthre", "psyche", "height", "weight",
              "smokev", "smoken", "drink",  "bmi",
              "hhres", "work1", "hearaid", "rural", "lgrip1", "rgrip1","vgactx",  "pain", "mstath")

share_cols <- c("mergeid", "death","time", "agey", "raeducl", "ragender", "hibpe","stroke","diabe", "cancre",
                "lunge", "hearte", "arthre", "psyche", "height", "weight",
                "smokev", "smoken", "drinkev", "bmi", 
                "hhres", "work1", "hearaid", "rural", "lgrip1", "rgrip1", "vgactx", "pain_s", "mstath")

klosa_cols <- c("pid", "death","time", "agey",  "raeducl", "ragender", "hibpe", "stroke",
                "diabe", "cancre", "lunge", "hearte", 
                "arthre", "psyche", "height", "weight", "smokev", 
                "smoken",  "drinkev", "bmi", "hres", "work1", "hearaid", "rural", "lgrip1", "rgrip1", "vigactf_k", "pain", "mstath")



# 子集筛选
charls_selected <- charls_wave[, charls_cols, drop = FALSE]
hrs_selected    <- hrs_wave[, hrs_cols, drop = FALSE]
share_selected  <- share_wave[, share_cols, drop = FALSE]
klosa_selected  <- klosa_wave[, klosa_cols, drop = FALSE]


############5.07


# 移除第二列为 NA 的行，并对第一列重新编号
charls_selected_clean <- charls_selected[!is.na(charls_selected[, 2]), ]
charls_selected_clean[, 1] <- paste0("charls.", 1:nrow(charls_selected_clean))

hrs_selected_clean <- hrs_selected[!is.na(hrs_selected[, 2]), ]
hrs_selected_clean[, 1] <- paste0("hrs.", 1:nrow(hrs_selected_clean))

share_selected_clean <- share_selected[!is.na(share_selected[, 2]), ]
share_selected_clean[, 1] <- paste0("share.", 1:nrow(share_selected_clean))

klosa_selected_clean <- klosa_selected[!is.na(klosa_selected[, 2]), ]
klosa_selected_clean[, 1] <- paste0("klosa.", 1:nrow(klosa_selected_clean))

ukb_selected_clean <- ukb[!is.na(ukb[, 2]), ]
ukb_selected_clean[, 1] <- paste0("ukb.", 1:nrow(ukb_selected_clean))


# 设置新的列名
new_colnames <- c(
  "sample", "death","time", "age", "educl", "gender", "hibpe", "stroke", "diabe", "cancre",
  "lunge", "hearte", "arthre", "psyche", "height", "weight",
  "smokev", "smoken", "drinkev", "bmi",
  "family_size", "work", "hearaid", "rural", "lgrip", "rgrip", "vgactx", "pain", "marry")

# 替换列名（确保每个数据框有27列）
colnames(charls_selected_clean) <- new_colnames
colnames(hrs_selected_clean) <- new_colnames
colnames(share_selected_clean) <- new_colnames
colnames(klosa_selected_clean) <- new_colnames
colnames(ukb_selected_clean) <- new_colnames
ukb_selected_clean$height <- ukb_selected_clean$height / 100



library(mice)
library(dplyr)
library(tidyr)
library(recipes)
library(themis)
set.seed(123)  # 保证可重复

charls_selected_clean$sample <- as.character(charls_selected_clean$sample)
hrs_selected_clean$sample <- as.character(hrs_selected_clean$sample)
ukb_selected_clean$sample <- as.character(ukb_selected_clean$sample)
klosa_selected_clean$sample <- as.character(klosa_selected_clean$sample)
share_selected_clean$sample <- as.character(share_selected_clean$sample)
ukb_selected_clean$family_size <- as.numeric(ukb_selected_clean$family_size)



library(dplyr)
library(caret)
# 合并数据
combined_data <- bind_rows(charls_selected_clean, hrs_selected_clean, ukb_selected_clean, klosa_selected_clean, share_selected_clean)
combined_data <- combined_data %>%
  filter(time > 0)

# 设置随机种子以确保可重复性
set.seed(123)

# 获取数据集的行数
n <- nrow(combined_data)

# 随机抽样，70% 作为训练集
train_indices <- sample(1:n, size = 0.7 * n, replace = FALSE)

# 划分训练集和测试集
train_data <- combined_data[train_indices, ]
test_data <- combined_data[-train_indices, ]

library(mice)

# ----------------- 训练集插补 -----------------
# group=0：保留第一列和第二列，其他列进行插补
imp_train <- mice(train_data[, -c(1, 3)], m = 5, method = 'pmm', seed = 123)
train_imputed <- complete(imp_train, 1)
train_full <- cbind(train_data[, 1:3], train_imputed)  # 保留第一列 ID 和第二列 outcome

# 如果数据中包含身高和体重变量，重新计算BMI
if("weight" %in% names(train_full) & "height" %in% names(train_full)) {
  train_full$bmi <- train_full$weight / (train_full$height * train_full$height)
}

# ----------------- 测试集插补 -----------------
# group=0：保留第一列和第二列，其他列进行插补
imp_test <- mice(test_data[, -c(1, 3)], m = 5, method = 'pmm', seed = 123)
test_imputed <- complete(imp_test, 1)
test_full <- cbind(test_data[, 1:3], test_imputed)  # 保留第一列 ID 和第二列 outcome

# 如果数据中包含身高和体重变量，重新计算BMI
if("weight" %in% names(test_full) & "height" %in% names(test_full)) {
  test_full$bmi <- test_full$weight / (test_full$height * test_full$height)
}

# 定义函数来识别和移除极端异常值的行
remove_outliers <- function(data, columns) {
  # 遍历指定列
  for (col in columns) {
    # 计算四分位数和 IQR
    Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    # 定义异常值范围
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    # 过滤掉包含异常值的行
    data <- data[data[[col]] >= lower_bound & data[[col]] <= upper_bound | is.na(data[[col]]), ]
  }
  return(data)
}

# 指定要处理的列
columns_to_check <- c("height", "weight", "bmi", "lgrip", "rgrip")

# 移除训练集和测试集的异常值
train_data_clean <- remove_outliers(train_full, columns_to_check)
test_data_clean <- remove_outliers(test_full, columns_to_check)

# 将训练集和测试集保存为 CSV 文件
write.csv(train_data_clean, "train预后.csv", row.names = FALSE)
write.csv(test_data_clean, "test预后.csv", row.names = FALSE)

