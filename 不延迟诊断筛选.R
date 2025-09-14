rm(list=ls())
library(readr)

# 加载Rdata文件
load("share.Rdata")
load("hrs.Rdata")
load("charls.Rdata")
load("klosa.Rdata")
ukb <- read.csv("ukb诊断.csv")



#
library(haven)
charls <- zap_labels(charls)
hrs <- zap_labels(hrs)
share <- zap_labels(share)
klosa <- zap_labels(klosa)



hrs$alzdeme <- ifelse(
  !is.na(hrs$alzhe) & hrs$alzhe > 0 | !is.na(hrs$demen) & hrs$demen > 0, 
  1, 
  ifelse(
    is.na(hrs$alzhe) & is.na(hrs$demen), 
    NA, 
    0
  )
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

charls <- charls %>%
  mutate(memrye = case_when(
    memrye == 1 & cog_status == 1 ~ 1,                # 两个都为1
    !is.na(memrye) & !is.na(cog_status) &             # 两个都有数据
      memrye != 1 & cog_status != 1 ~ 0,              # 且都不为1
    TRUE ~ NA_real_                                   # 其他情况设为NA
  ))



# 对于 ukb 数据集的 Urban_Rural_Binary 列
ukb$Urban_Rural_Binary <- ifelse(ukb$Urban_Rural_Binary == 1, 0, 
                                 ifelse(ukb$Urban_Rural_Binary == 0, 1, ukb$Urban_Rural_Binary))


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






##############################################################################################

library(tidyr)
library(dplyr)

# ===== CHARLS 数据处理 =====
# 转宽格式
charls_wide <- charls %>%
  select(ID, wave, memrye, age) %>%
  pivot_wider(
    names_from = wave, 
    values_from = c(memrye, age),
    names_sep = "_wave",
    values_fill = list(memrye = NA, age = NA)
  )

# 筛选基线未患病且有随访的个体
charls_wide_filtered <- charls_wide %>%
  filter(memrye_wave1 == 0) %>%
  filter(!(is.na(memrye_wave2) & is.na(memrye_wave3) & is.na(memrye_wave4) & is.na(memrye_wave5)))

# 找到首次患病的波次和年龄
charls_wide_1 <- charls_wide_filtered %>%
  rowwise() %>%
  mutate(
    first_wave_with_1 = {
      waves <- c(memrye_wave2, memrye_wave3, memrye_wave4, memrye_wave5)
      first_pos <- which(waves == 1)[1]
      if (!is.na(first_pos)) first_pos + 1 else NA
    },
    dementia_age = case_when(
      first_wave_with_1 == 2 ~ age_wave2,
      first_wave_with_1 == 3 ~ age_wave3,
      first_wave_with_1 == 4 ~ age_wave4,
      first_wave_with_1 == 5 ~ age_wave5,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(first_wave_with_1)) %>%
  mutate(
    outcome = ifelse(dementia_age < 65, 1, 0),
    wave = first_wave_with_1
  ) %>%
  select(ID, wave, outcome, dementia_age)

# 未患病个体
charls_wide_0 <- charls_wide_filtered %>%
  filter(if_all(c(memrye_wave2, memrye_wave3, memrye_wave4, memrye_wave5), ~ is.na(.) | . != 1)) %>%
  mutate(
    last_age = pmax(age_wave2, age_wave3, age_wave4, age_wave5, na.rm = TRUE),
    outcome = 0,
    wave = 1
  ) %>%
  select(ID, wave, outcome, last_age)

# 合并并提取基线数据
charls_outcome <- bind_rows(
  charls_wide_1 %>% select(ID, wave, outcome),
  charls_wide_0 %>% select(ID, wave, outcome)
)

charls_wave <- charls %>%
  filter(wave == 1, ID %in% charls_outcome$ID) %>%
  left_join(charls_outcome, by = "ID") %>%
  left_join(
    bind_rows(
      charls_wide_1 %>% select(ID, dementia_age),
      charls_wide_0 %>% select(ID, last_age) %>% rename(dementia_age = last_age)
    ),
    by = "ID"
  ) %>%
  mutate(
    time = case_when(
      outcome == 1 ~ dementia_age - age,
      outcome == 0 & dementia_age >= 65 ~ 65 - age,
      outcome == 0 & dementia_age < 65 ~ dementia_age - age,
      TRUE ~ NA_real_
    )
  )

# ===== HRS 数据处理 =====
hrs_wide <- hrs %>%
  select(hhidpn, wave, alzdeme, ragey_m) %>%
  pivot_wider(
    names_from = wave, 
    values_from = c(alzdeme, ragey_m),
    names_sep = "_wave",
    values_fill = list(alzdeme = NA, ragey_m = NA)
  )

hrs_wide_filtered <- hrs_wide %>%
  filter(alzdeme_wave10 == 0) %>%
  filter(!(is.na(alzdeme_wave11) & is.na(alzdeme_wave12) & is.na(alzdeme_wave13) & is.na(alzdeme_wave14) & is.na(alzdeme_wave15)))

hrs_wide_1 <- hrs_wide_filtered %>%
  rowwise() %>%
  mutate(
    first_wave_with_1 = {
      waves <- c(alzdeme_wave11, alzdeme_wave12, alzdeme_wave13, alzdeme_wave14, alzdeme_wave15)
      first_pos <- which(waves == 1)[1]
      if (!is.na(first_pos)) first_pos + 10 else NA
    },
    dementia_age = case_when(
      first_wave_with_1 == 11 ~ ragey_m_wave11,
      first_wave_with_1 == 12 ~ ragey_m_wave12,
      first_wave_with_1 == 13 ~ ragey_m_wave13,
      first_wave_with_1 == 14 ~ ragey_m_wave14,
      first_wave_with_1 == 15 ~ ragey_m_wave15,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(first_wave_with_1)) %>%
  mutate(
    outcome = ifelse(dementia_age < 65, 1, 0),
    wave = first_wave_with_1
  ) %>%
  select(hhidpn, wave, outcome, dementia_age)

hrs_wide_0 <- hrs_wide_filtered %>%
  filter(if_all(c(alzdeme_wave11, alzdeme_wave12, alzdeme_wave13, alzdeme_wave14, alzdeme_wave15), ~ is.na(.) | . != 1)) %>%
  mutate(
    last_age = pmax(ragey_m_wave11, ragey_m_wave12, ragey_m_wave13, ragey_m_wave14, ragey_m_wave15, na.rm = TRUE),
    outcome = 0,
    wave = 10
  ) %>%
  select(hhidpn, wave, outcome, last_age)

hrs_outcome <- bind_rows(
  hrs_wide_1 %>% select(hhidpn, wave, outcome),
  hrs_wide_0 %>% select(hhidpn, wave, outcome)
)

hrs_wave <- hrs %>%
  filter(wave == 10, hhidpn %in% hrs_outcome$hhidpn) %>%
  left_join(hrs_outcome, by = "hhidpn") %>%
  left_join(
    bind_rows(
      hrs_wide_1 %>% select(hhidpn, dementia_age),
      hrs_wide_0 %>% select(hhidpn, last_age) %>% rename(dementia_age = last_age)
    ),
    by = "hhidpn"
  ) %>%
  mutate(
    time = case_when(
      outcome == 1 ~ dementia_age - ragey_m,
      outcome == 0 & dementia_age >= 65 ~ 65 - ragey_m,
      outcome == 0 & dementia_age < 65 ~ dementia_age - ragey_m,
      TRUE ~ NA_real_
    )
  )

# ===== SHARE 数据处理 =====
share_wide <- share %>%
  select(mergeid, wave, alzdeme, agey) %>%
  pivot_wider(
    names_from = wave, 
    values_from = c(alzdeme, agey),
    names_sep = "_wave",
    values_fill = list(alzdeme = NA, agey = NA)
  )

share_wide_filtered <- share_wide %>%
  filter(alzdeme_wave2 == 0) %>%
  filter(!(is.na(alzdeme_wave4) & is.na(alzdeme_wave5) & is.na(alzdeme_wave6) & is.na(alzdeme_wave7) & is.na(alzdeme_wave8)))

share_wide_1 <- share_wide_filtered %>%
  rowwise() %>%
  mutate(
    first_wave_with_1 = {
      waves <- c(alzdeme_wave4, alzdeme_wave5, alzdeme_wave6, alzdeme_wave7)
      first_pos <- which(waves == 1)[1]
      if (!is.na(first_pos)) first_pos + 3 else NA
    },
    dementia_age = case_when(
      first_wave_with_1 == 4 ~ agey_wave4,
      first_wave_with_1 == 5 ~ agey_wave5,
      first_wave_with_1 == 6 ~ agey_wave6,
      first_wave_with_1 == 7 ~ agey_wave7,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(first_wave_with_1)) %>%
  mutate(
    outcome = ifelse(dementia_age < 65, 1, 0),
    wave = first_wave_with_1
  ) %>%
  select(mergeid, wave, outcome, dementia_age)

share_wide_0 <- share_wide_filtered %>%
  filter(if_all(
    c(alzdeme_wave4, alzdeme_wave5, alzdeme_wave6, alzdeme_wave7, alzdeme_wave8),
    ~ is.na(.) | . != 1
  )) %>%
  mutate(
    last_age = pmax(agey_wave4, agey_wave5, agey_wave6, agey_wave7, agey_wave8, na.rm = TRUE),
    outcome = 0,
    wave = 2
  ) %>%
  select(mergeid, wave, outcome, last_age)

# 合并 SHARE 的 outcome 表
share_outcome <- bind_rows(
  share_wide_1 %>% select(mergeid, wave, outcome, dementia_age),
  share_wide_0 %>% select(mergeid, wave, outcome, dementia_age = last_age)
)

# 提取基线 wave=2 的记录并计算 time
share_wave <- share %>%
  filter(wave == 2, mergeid %in% share_outcome$mergeid) %>%
  left_join(share_outcome, by = "mergeid") %>%
  mutate(
    time = case_when(
      outcome == 1 ~ dementia_age - agey,
      outcome == 0 & dementia_age >= 65 ~ 65 - agey,
      outcome == 0 & dementia_age < 65  ~ dementia_age - agey,
      TRUE ~ NA_real_
    )
  )


# ===== KLOSA 数据处理 =====
klosa_wide <- klosa %>%
  select(pid, wave, dementia, agey) %>%
  pivot_wider(
    names_from  = wave,
    values_from = c(dementia, agey),
    names_sep   = "_wave",
    values_fill = list(dementia = NA, agey = NA)
  )

# 首次患病（outcome = 1）
klosa_wide_1 <- klosa_wide %>%
  filter(dementia_wave3 %in% c(2, 3)) %>%   # 基线 wave3 无痴呆（2或3都算无痴呆）
  rowwise() %>%
  mutate(
    first_wave_with_1 = {
      vals <- c(dementia_wave4, dementia_wave5, dementia_wave6, dementia_wave7)
      pos  <- which(vals == 1)[1]  # 值为1表示有痴呆
      if (!is.na(pos)) pos + 3 else NA
    },
    dementia_age = case_when(
      first_wave_with_1 == 4 ~ agey_wave4,
      first_wave_with_1 == 5 ~ agey_wave5,
      first_wave_with_1 == 6 ~ agey_wave6,
      first_wave_with_1 == 7 ~ agey_wave7,
      TRUE               ~ NA_real_
    )
  ) %>%
  filter(!is.na(first_wave_with_1)) %>%
  mutate(
    outcome = ifelse(dementia_age < 65, 1, 0),
    wave    = first_wave_with_1
  ) %>%
  select(pid, wave, outcome, dementia_age)

# 未患病（outcome = 0）
klosa_wide_0 <- klosa_wide %>%
  filter(dementia_wave3 %in% c(2, 3)) %>%   # 基线 wave3 无痴呆（2或3都算无痴呆）
  filter(if_all(
    c(dementia_wave4, dementia_wave5, dementia_wave6, dementia_wave7),
    ~ is.na(.) | . %in% c(2, 3)  # 后续wave中无痴呆（2或3）或缺失
  )) %>%
  mutate(
    last_age = pmax(agey_wave4, agey_wave5, agey_wave6, agey_wave7, na.rm = TRUE),
    outcome  = 0,
    wave     = 3
  ) %>%
  select(pid, wave, outcome, dementia_age = last_age)

# 合并 KLOSA 的 outcome 表
klosa_outcome <- bind_rows(klosa_wide_1, klosa_wide_0)

# 提取基线 wave = 3 的记录并计算 time
klosa_wave <- klosa %>%
  filter(wave == 3, pid %in% klosa_outcome$pid) %>%
  left_join(klosa_outcome, by = "pid") %>%
  mutate(
    time = case_when(
      outcome == 1 ~ dementia_age - agey,           # 病例：从基线到发病的时间
      outcome == 0 & dementia_age >= 65 ~ 65 - agey, # 对照：从基线到65岁（如果活到65+）
      outcome == 0 & dementia_age < 65  ~ dementia_age - agey, # 对照：从基线到最后观测
      TRUE ~ NA_real_
    )
  )


# ===== UKB 数据处理 =====
library(dplyr)

ukb_selected <- ukb %>%
  # 1. 在一次 mutate 调用中完成 EOD 的更新和 time 的计算
  mutate(
    # 首先，直接覆盖 EOD 列，应用新规则
    EOD = if_else(EOD == 1 & (EODtime - Year.of.birth) < 65, 1, 0),
    
    # 接着，使用上面刚刚更新过的 EOD 列来计算 time
    time = if_else(
      EOD == 1,
      # 如果是病例 (EOD == 1)，计算发病时间
      (EODtime - Year.of.birth) - Age.at.recruitment,
      # 如果是对照 (EOD == 0)，计算审查时间
      pmin(2024 - Year.of.birth, 65) - Age.at.recruitment
    )
  ) %>%
  # 2. 删掉不再需要的原始列
  select(-EODtime, -Year.of.birth) %>%
  # 3. 调整列的顺序 (现在不需要 rename 了)
  select(sample, EOD, time, everything())

# 查看结果
head(ukb_selected)


##############################################################################################




# 定义每个数据对应要保留的列名
charls_cols <- c("ID", "outcome", "time", "age", "raeducl", "ragender", "hibpe", "stroke", "diabe", "cancre",
                 "lunge", "hearte", "arthre", "psyche", "mheight", "mweight",
                 "smokev", "smoken", "drinkev", "bmi",
                 "family_size", "work", "hear_aid","hrural", "lgrip1", "rgrip1", "vgactx_c", "pain", "marry")

hrs_cols <- c("hhidpn","outcome", "time", "ragey_m", "raeducl", "ragender", "hibpe","stroke", "diabe", "cancre",
              "lunge", "hearte", "arthre", "psyche", "height", "weight",
              "smokev", "smoken", "drink",  "bmi",
              "hhres", "work1", "hearaid", "rural", "lgrip1", "rgrip1", "vgactx", "pain", "mstath")

share_cols <- c("mergeid", "outcome", "time", "agey", "raeducl", "ragender", "hibpe","stroke","diabe", "cancre",
                "lunge", "hearte", "arthre", "psyche", "height", "weight",
                "smokev", "smoken", "drinkev", "bmi", 
                "hhres", "work1", "hearaid", "rural", "lgrip1", "rgrip1", "vgactx", "pain_s", "mstath")

klosa_cols <- c("pid", "outcome", "time", "agey",  "raeducl", "ragender", "hibpe", "stroke",
                "diabe", "cancre", "lunge", "hearte", 
                "arthre", "psyche", "height", "weight", "smokev", 
                "smoken",  "drinkev", "bmi", "hres", "work1", 
                "hearaid", "rural", "lgrip1", "rgrip1", "vigactf_k", "pain", "mstath")



# 子集筛选
charls_selected <- charls_wave[, charls_cols, drop = FALSE]
hrs_selected    <- hrs_wave[, hrs_cols, drop = FALSE]
share_selected  <- share_wave[, share_cols, drop = FALSE]
klosa_selected  <- klosa_wave[, klosa_cols, drop = FALSE]

############5.07

# 1. 重新编号第一列，命名为 *_selected_1
charls_selected_1 <- charls_selected
charls_selected_1[, 1] <- paste0("charls.", seq_len(nrow(charls_selected_1)))

hrs_selected_1 <- hrs_selected
hrs_selected_1[, 1] <- paste0("hrs.", seq_len(nrow(hrs_selected_1)))

share_selected_1 <- share_selected
share_selected_1[, 1] <- paste0("share.", seq_len(nrow(share_selected_1)))

klosa_selected_1 <- klosa_selected
klosa_selected_1[, 1] <- paste0("klosa.", seq_len(nrow(klosa_selected_1)))

ukb_selected_1 <- ukb_selected
ukb_selected_1[, 1] <- paste0("ukb.", seq_len(nrow(ukb_selected_1)))

# 2. 统一设置新的列名（共 29 列）
new_colnames <- c(
  "sample", "outcome", "time", "age", "educl", "gender", "hibpe", "stroke", "diabe", "cancre",
  "lunge", "hearte", "arthre", "psyche", "height", "weight",
  "smokev", "smoken", "drinkev", "bmi",
  "family_size", "work", "hearaid", "rural", "lgrip", "rgrip", "vgactx", "pain", "marry"
)

colnames(charls_selected_1) <- new_colnames
colnames(hrs_selected_1)   <- new_colnames
colnames(share_selected_1) <- new_colnames
colnames(klosa_selected_1) <- new_colnames
colnames(ukb_selected_1)   <- new_colnames

# 3. ukb 的身高单位转换：cm → m
ukb_selected_1 <- ukb_selected_1 %>%
  mutate(height = height / 100)




####################################################
#####以下内容9月10日以后得数据没进行操作############
####################################################

charls_selected_1 <- charls_selected_1[!is.na(charls_selected[, 2]), ]

hrs_selected_1 <- hrs_selected_1[!is.na(hrs_selected[, 2]), ]

share_selected_1 <- share_selected_1[!is.na(share_selected[, 2]), ]

klosa_selected_1 <- klosa_selected_1[!is.na(klosa_selected[, 2]), ]

ukb_selected_1 <- ukb_selected_1[!is.na(ukb_selected[, 2]), ]



# 4. age < 65 且 time > 0 的筛选
charls_selected_clean <- charls_selected_1 %>%
  filter(age < 65, time > 0)

hrs_selected_clean <- hrs_selected_1 %>%
  filter(age < 65, time > 0)

share_selected_clean <- share_selected_1 %>%
  filter(age < 65, time > 0)

klosa_selected_clean <- klosa_selected_1 %>%
  filter(age < 65, time > 0)

ukb_selected_clean <- ukb_selected_1 %>%
  filter(age < 65, time > 0)


# 展示每个数据框中 outcome 列的分布
outcome_distribution <- function(data, dataset_name) {
  cat(paste("\nOutcome Distribution for", dataset_name, ":\n"))
  print(table(data$outcome))
}

# 分别展示每个数据集的 outcome 分布
outcome_distribution(charls_selected_clean, "charls")
outcome_distribution(hrs_selected_clean, "hrs")
outcome_distribution(share_selected_clean, "share")
outcome_distribution(klosa_selected_clean, "klosa")
outcome_distribution(ukb_selected_clean, "ukb")





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
library(mice)
# caret库在此版本中不再需要，但保留以防万一
# library(caret) 

# --- 1. 合并与初步处理 ---

# 假设您的数据框 charls_selected_clean, hrs_selected_clean, 
# ukb_selected_clean, klosa_selected_clean 已经加载到环境中
# 合并数据
combined_data <- bind_rows(charls_selected_clean, hrs_selected_clean, ukb_selected_clean, klosa_selected_clean)

# 重命名列
colnames(combined_data)[1:2] <- c("sample", "outcome")

# 数据清洗：对 vgactx 列的值进行上限处理
combined_data$vgactx[combined_data$vgactx > 7] <- 7

# (可选) 计算每列缺失值的比例，用于检查
missing_ratio <- sapply(combined_data, function(x) sum(is.na(x)) / length(x))
print("各变量缺失比例:")
print(missing_ratio)


# --- 2. 分层多重插补 (对整个数据集) ---
set.seed(123)  # 保证结果可复现

# 根据 outcome 变量进行分层
data_0 <- combined_data[combined_data$outcome == 0, ]
data_1 <- combined_data[combined_data$outcome == 1, ]

# 对 group = 0 的数据进行插补
# 保留第 1–3 列 (sample, outcome, time)，对其余列进行插补
imp_0 <- mice(data_0[, -c(1, 2, 3)], m = 5, method = 'pmm', seed = 123)
data_0_imputed <- complete(imp_0, 1)
data_0_final <- cbind(data_0[, 1:3], data_0_imputed)

# 对 group = 1 的数据进行插补
# 同样保留第 1–3 列，对其余列进行插补
imp_1 <- mice(data_1[, -c(1, 2, 3)], m = 5, method = 'pmm', seed = 123)
data_1_imputed <- complete(imp_1, 1)
data_1_final <- cbind(data_1[, 1:3], data_1_imputed)

# 合并插补后的两组数据
imputed_full_data <- rbind(data_0_final, data_1_final)


# --- 3. 后处理 ---

# 如果数据中包含身高和体重变量，重新计算BMI
if("weight" %in% names(imputed_full_data) & "height" %in% names(imputed_full_data)) {
  imputed_full_data$bmi <- imputed_full_data$weight / (imputed_full_data$height * imputed_full_data$height)
  print("BMI已重新计算。")
}

# 定义移除离群值的函数
remove_outliers <- function(data, columns) {
  for (col in columns) {
    if (col %in% names(data)) { # 检查列是否存在
      Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      # 移除离群值，保留NA值
      data <- data[ (data[[col]] >= lower_bound & data[[col]] <= upper_bound) | is.na(data[[col]]), ]
    }
  }
  return(data)
}

# 指定要处理离群值的列
columns_to_check <- c("height", "weight", "bmi", "lgrip", "rgrip")

# 对整个数据集移除离群值
final_data_clean <- remove_outliers(imputed_full_data, columns_to_check)


# --- 4. 输出最终文件 ---

# 检查 'time' 列是否存在，如果存在则移除
if ("time" %in% colnames(final_data_clean)) {
  final_data_to_write <- final_data_clean[, -which(colnames(final_data_clean) == "time")]
} else {
  final_data_to_write <- final_data_clean
}

# 将处理完成的数据输出为 "自报不延迟.csv"
write.csv(final_data_to_write, "自报不延迟.csv", row.names = FALSE)


# 1. 前期数据清洗 -------------------------------------------------------

# 拷贝原数据，重命名前两列
share_data <- share_selected_clean %>%
  rename(sample = 1, outcome = 2)
# 对 vgactx 做上限处理
share_data$vgactx[share_data$vgactx > 7] <- 7
# （可选）查看缺失率
missing_ratio <- sapply(share_data, function(x) sum(is.na(x)) / length(x))
print(missing_ratio)

# 2. 分层插补 -------------------------------------------------------------

set.seed(123)
# 按 outcome 分组
data_0 <- filter(share_data, outcome == 0)
data_1 <- filter(share_data, outcome == 1)

# 插补函数：对第 4 列及以后变量进行 m=5 次 pmm 插补，保留第一～三列不变
impute_group <- function(df) {
  imp <- mice(df[, -c(1:3)], m = 5, method = "pmm", seed = 123)
  imputed <- complete(imp, 1)
  bind_cols(df[, 1:3], imputed)
}

share_0_imp <- impute_group(data_0)
share_1_imp <- impute_group(data_1)

# 合并回完整数据
share_imputed <- bind_rows(share_0_imp, share_1_imp)

# 3. 重新计算 BMI（如果有身高和体重） ----------------------------------------
if (all(c("weight", "height") %in% names(share_imputed))) {
  share_imputed <- share_imputed %>%
    mutate(bmi = weight / (height^2))
}

# 4. 去除极值 --------------------------------------------------------------
share_clean <- remove_outliers(share_imputed, columns_to_check)

# 5. 导出结果 --------------------------------------------------------------
write.csv(share_clean[, -which(colnames(share_clean) == "time")], "share.csv", row.names = FALSE)
write.csv(share_clean[, 1:3], "share_ID.csv", row.names = FALSE)




################################################################################
###
################################################################################
rm(list=ls())
library(readr)

# 加载Rdata文件
load("share.Rdata")
load("hrs.Rdata")
load("charls.Rdata")
load("klosa.Rdata")
ukb <- read.csv("ukb诊断.csv")



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




# 对于 ukb 数据集的 Urban_Rural_Binary 列
ukb$Urban_Rural_Binary <- ifelse(ukb$Urban_Rural_Binary == 1, 0, 
                                 ifelse(ukb$Urban_Rural_Binary == 0, 1, ukb$Urban_Rural_Binary))


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






##############################################################################################

library(tidyr)
library(dplyr)


# ===== CHARLS 数据处理 (根据要求，此部分逻辑不变) =====
# 转宽格式
charls_wide <- charls %>%
  select(ID, wave, memrye, age) %>%
  pivot_wider(
    names_from = wave, 
    values_from = c(memrye, age),
    names_sep = "_wave",
    values_fill = list(memrye = NA, age = NA)
  )

# 筛选基线(wave 1)未患病且有随访(wave 2-5)的个体
charls_wide_filtered <- charls_wide %>%
  filter(memrye_wave1 == 0) %>%
  filter(!if_all(c(memrye_wave2, memrye_wave3, memrye_wave4, memrye_wave5), is.na))

# 找到首次患病的波次和年龄
charls_wide_1 <- charls_wide_filtered %>%
  rowwise() %>%
  mutate(
    first_wave_with_1 = {
      waves_data <- c(memrye_wave2, memrye_wave3, memrye_wave4, memrye_wave5)
      # 寻找第一个值为1的位置，对应的波次是 位置+1
      first_pos <- which(waves_data == 1)[1]
      if (!is.na(first_pos)) first_pos + 1 else NA
    },
    dementia_age = case_when(
      first_wave_with_1 == 2 ~ age_wave2,
      first_wave_with_1 == 3 ~ age_wave3,
      first_wave_with_1 == 4 ~ age_wave4,
      first_wave_with_1 == 5 ~ age_wave5,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(first_wave_with_1)) %>%
  mutate(
    outcome = ifelse(dementia_age < 65, 1, 0),
    wave = first_wave_with_1 # wave 变量记录的是首次患病波次
  ) %>%
  select(ID, wave, outcome, dementia_age)

# 未患病个体
charls_wide_0 <- charls_wide_filtered %>%
  filter(if_all(c(memrye_wave2, memrye_wave3, memrye_wave4, memrye_wave5), ~ is.na(.) | . != 1)) %>%
  mutate(
    last_age = pmax(age_wave2, age_wave3, age_wave4, age_wave5, na.rm = TRUE),
    outcome = 0,
    wave = 1 # wave 变量记录的是基线波次
  ) %>%
  select(ID, wave, outcome, last_age)

# 合并并提取基线数据
charls_outcome <- bind_rows(
  charls_wide_1 %>% select(ID, wave, outcome),
  charls_wide_0 %>% select(ID, wave, outcome)
)

charls_wave <- charls %>%
  filter(wave == 1, ID %in% charls_outcome$ID) %>% # 基线是wave 1
  left_join(charls_outcome, by = "ID", suffix = c("", ".y")) %>% # 添加后缀避免wave列冲突
  select(-wave.y) %>% # 移除来自outcome的wave列，保留原始基线wave列
  left_join(
    bind_rows(
      charls_wide_1 %>% select(ID, dementia_age),
      charls_wide_0 %>% select(ID, last_age) %>% rename(dementia_age = last_age)
    ),
    by = "ID"
  ) %>%
  mutate(
    time = case_when(
      outcome == 1 ~ dementia_age - age,
      outcome == 0 & dementia_age >= 65 ~ 65 - age,
      outcome == 0 & dementia_age < 65 ~ dementia_age - age,
      TRUE ~ NA_real_
    )
  )

# ===== HRS 数据处理 =====
hrs_wide <- hrs %>%
  select(hhidpn, wave, alzdeme, ragey_m) %>%
  pivot_wider(
    names_from = wave, 
    values_from = c(alzdeme, ragey_m),
    names_sep = "_wave",
    values_fill = list(alzdeme = NA, ragey_m = NA)
  )

hrs_wide_filtered <- hrs_wide %>%
  filter(alzdeme_wave10 == 0) %>%
  filter(!(is.na(alzdeme_wave11) & is.na(alzdeme_wave12) & is.na(alzdeme_wave13) & is.na(alzdeme_wave14) & is.na(alzdeme_wave15)))

hrs_wide_1 <- hrs_wide_filtered %>%
  rowwise() %>%
  mutate(
    first_wave_with_1 = {
      waves <- c(alzdeme_wave11, alzdeme_wave12, alzdeme_wave13, alzdeme_wave14, alzdeme_wave15)
      first_pos <- which(waves == 1)[1]
      if (!is.na(first_pos)) first_pos + 10 else NA
    },
    dementia_age = case_when(
      first_wave_with_1 == 11 ~ ragey_m_wave11,
      first_wave_with_1 == 12 ~ ragey_m_wave12,
      first_wave_with_1 == 13 ~ ragey_m_wave13,
      first_wave_with_1 == 14 ~ ragey_m_wave14,
      first_wave_with_1 == 15 ~ ragey_m_wave15,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(first_wave_with_1)) %>%
  mutate(
    outcome = ifelse(dementia_age < 65, 1, 0),
    wave = first_wave_with_1
  ) %>%
  select(hhidpn, wave, outcome, dementia_age)

hrs_wide_0 <- hrs_wide_filtered %>%
  filter(if_all(c(alzdeme_wave11, alzdeme_wave12, alzdeme_wave13, alzdeme_wave14, alzdeme_wave15), ~ is.na(.) | . != 1)) %>%
  mutate(
    last_age = pmax(ragey_m_wave11, ragey_m_wave12, ragey_m_wave13, ragey_m_wave14, ragey_m_wave15, na.rm = TRUE),
    outcome = 0,
    wave = 10
  ) %>%
  select(hhidpn, wave, outcome, last_age)

hrs_outcome <- bind_rows(
  hrs_wide_1 %>% select(hhidpn, wave, outcome),
  hrs_wide_0 %>% select(hhidpn, wave, outcome)
)

hrs_wave <- hrs %>%
  filter(wave == 10, hhidpn %in% hrs_outcome$hhidpn) %>%
  left_join(hrs_outcome, by = "hhidpn") %>%
  left_join(
    bind_rows(
      hrs_wide_1 %>% select(hhidpn, dementia_age),
      hrs_wide_0 %>% select(hhidpn, last_age) %>% rename(dementia_age = last_age)
    ),
    by = "hhidpn"
  ) %>%
  mutate(
    time = case_when(
      outcome == 1 ~ dementia_age - ragey_m,
      outcome == 0 & dementia_age >= 65 ~ 65 - ragey_m,
      outcome == 0 & dementia_age < 65 ~ dementia_age - ragey_m,
      TRUE ~ NA_real_
    )
  )

# ===== SHARE 数据处理 =====
share_wide <- share %>%
  select(mergeid, wave, alzdeme, agey) %>%
  pivot_wider(
    names_from = wave, 
    values_from = c(alzdeme, agey),
    names_sep = "_wave",
    values_fill = list(alzdeme = NA, agey = NA)
  )

share_wide_filtered <- share_wide %>%
  filter(alzdeme_wave2 == 0) %>%
  filter(!(is.na(alzdeme_wave4) & is.na(alzdeme_wave5) & is.na(alzdeme_wave6) & is.na(alzdeme_wave7) & is.na(alzdeme_wave8)))

share_wide_1 <- share_wide_filtered %>%
  rowwise() %>%
  mutate(
    first_wave_with_1 = {
      waves <- c(alzdeme_wave4, alzdeme_wave5, alzdeme_wave6, alzdeme_wave7)
      first_pos <- which(waves == 1)[1]
      if (!is.na(first_pos)) first_pos + 3 else NA
    },
    dementia_age = case_when(
      first_wave_with_1 == 4 ~ agey_wave4,
      first_wave_with_1 == 5 ~ agey_wave5,
      first_wave_with_1 == 6 ~ agey_wave6,
      first_wave_with_1 == 7 ~ agey_wave7,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(first_wave_with_1)) %>%
  mutate(
    outcome = ifelse(dementia_age < 65, 1, 0),
    wave = first_wave_with_1
  ) %>%
  select(mergeid, wave, outcome, dementia_age)

share_wide_0 <- share_wide_filtered %>%
  filter(if_all(
    c(alzdeme_wave4, alzdeme_wave5, alzdeme_wave6, alzdeme_wave7, alzdeme_wave8),
    ~ is.na(.) | . != 1
  )) %>%
  mutate(
    last_age = pmax(agey_wave4, agey_wave5, agey_wave6, agey_wave7, agey_wave8, na.rm = TRUE),
    outcome = 0,
    wave = 2
  ) %>%
  select(mergeid, wave, outcome, last_age)

# 合并 SHARE 的 outcome 表
share_outcome <- bind_rows(
  share_wide_1 %>% select(mergeid, wave, outcome, dementia_age),
  share_wide_0 %>% select(mergeid, wave, outcome, dementia_age = last_age)
)

# 提取基线 wave=2 的记录并计算 time
share_wave <- share %>%
  filter(wave == 2, mergeid %in% share_outcome$mergeid) %>%
  left_join(share_outcome, by = "mergeid") %>%
  mutate(
    time = case_when(
      outcome == 1 ~ dementia_age - agey,
      outcome == 0 & dementia_age >= 65 ~ 65 - agey,
      outcome == 0 & dementia_age < 65  ~ dementia_age - agey,
      TRUE ~ NA_real_
    )
  )




# ===== KLOSA 数据处理 =====
klosa_wide <- klosa %>%
  select(pid, wave, dementia, agey) %>%
  pivot_wider(
    names_from  = wave,
    values_from = c(dementia, agey),
    names_sep   = "_wave",
    values_fill = list(dementia = NA, agey = NA)
  )

# 首次患病（outcome = 1）
klosa_wide_1 <- klosa_wide %>%
  filter(dementia_wave3 %in% c(2, 3)) %>%   # 基线 wave3 无痴呆（2或3都算无痴呆）
  rowwise() %>%
  mutate(
    first_wave_with_1 = {
      vals <- c(dementia_wave4, dementia_wave5, dementia_wave6, dementia_wave7)
      pos  <- which(vals == 1)[1]  # 值为1表示有痴呆
      if (!is.na(pos)) pos + 3 else NA
    },
    dementia_age = case_when(
      first_wave_with_1 == 4 ~ agey_wave4,
      first_wave_with_1 == 5 ~ agey_wave5,
      first_wave_with_1 == 6 ~ agey_wave6,
      first_wave_with_1 == 7 ~ agey_wave7,
      TRUE               ~ NA_real_
    )
  ) %>%
  filter(!is.na(first_wave_with_1)) %>%
  mutate(
    outcome = ifelse(dementia_age < 65, 1, 0),
    wave    = first_wave_with_1
  ) %>%
  select(pid, wave, outcome, dementia_age)

# 未患病（outcome = 0）
klosa_wide_0 <- klosa_wide %>%
  filter(dementia_wave3 %in% c(2, 3)) %>%   # 基线 wave3 无痴呆（2或3都算无痴呆）
  filter(if_all(
    c(dementia_wave4, dementia_wave5, dementia_wave6, dementia_wave7),
    ~ is.na(.) | . %in% c(2, 3)  # 后续wave中无痴呆（2或3）或缺失
  )) %>%
  mutate(
    last_age = pmax(agey_wave4, agey_wave5, agey_wave6, agey_wave7, na.rm = TRUE),
    outcome  = 0,
    wave     = 3
  ) %>%
  select(pid, wave, outcome, dementia_age = last_age)

# 合并 KLOSA 的 outcome 表
klosa_outcome <- bind_rows(klosa_wide_1, klosa_wide_0)

# 提取基线 wave = 3 的记录并计算 time
klosa_wave <- klosa %>%
  filter(wave == 3, pid %in% klosa_outcome$pid) %>%
  left_join(klosa_outcome, by = "pid") %>%
  mutate(
    time = case_when(
      outcome == 1 ~ dementia_age - agey,           # 病例：从基线到发病的时间
      outcome == 0 & dementia_age >= 65 ~ 65 - agey, # 对照：从基线到65岁（如果活到65+）
      outcome == 0 & dementia_age < 65  ~ dementia_age - agey, # 对照：从基线到最后观测
      TRUE ~ NA_real_
    )
  )


# ===== UKB 数据处理 =====
library(dplyr)

ukb_selected <- ukb %>%
  # 1. 在一次 mutate 调用中完成 EOD 的更新和 time 的计算
  mutate(
    # 首先，直接覆盖 EOD 列，应用新规则
    EOD = if_else(EOD == 1 & (EODtime - Year.of.birth) < 65, 1, 0),
    
    # 接着，使用上面刚刚更新过的 EOD 列来计算 time
    time = if_else(
      EOD == 1,
      # 如果是病例 (EOD == 1)，计算发病时间
      (EODtime - Year.of.birth) - Age.at.recruitment,
      # 如果是对照 (EOD == 0)，计算审查时间
      pmin(2024 - Year.of.birth, 65) - Age.at.recruitment
    )
  ) %>%
  # 2. 删掉不再需要的原始列
  select(-EODtime, -Year.of.birth) %>%
  # 3. 调整列的顺序 (现在不需要 rename 了)
  select(sample, EOD, time, everything())

# 查看结果
head(ukb_selected)


##############################################################################################




# 定义每个数据对应要保留的列名
charls_cols <- c("ID", "outcome", "time", "age", "raeducl", "ragender", "hibpe", "stroke", "diabe", "cancre",
                 "lunge", "hearte", "arthre", "psyche", "mheight", "mweight",
                 "smokev", "smoken", "drinkev", "bmi",
                 "family_size", "work", "hear_aid","hrural", "lgrip1", "rgrip1", "vgactx_c", "pain", "marry")

hrs_cols <- c("hhidpn","outcome", "time", "ragey_m", "raeducl", "ragender", "hibpe","stroke", "diabe", "cancre",
              "lunge", "hearte", "arthre", "psyche", "height", "weight",
              "smokev", "smoken", "drink",  "bmi",
              "hhres", "work1", "hearaid", "rural", "lgrip1", "rgrip1", "vgactx", "pain", "mstath")

share_cols <- c("mergeid", "outcome", "time", "agey", "raeducl", "ragender", "hibpe","stroke","diabe", "cancre",
                "lunge", "hearte", "arthre", "psyche", "height", "weight",
                "smokev", "smoken", "drinkev", "bmi", 
                "hhres", "work1", "hearaid", "rural", "lgrip1", "rgrip1", "vgactx", "pain_s", "mstath")

klosa_cols <- c("pid", "outcome", "time", "agey",  "raeducl", "ragender", "hibpe", "stroke",
                "diabe", "cancre", "lunge", "hearte", 
                "arthre", "psyche", "height", "weight", "smokev", 
                "smoken",  "drinkev", "bmi", "hres", "work1", 
                "hearaid", "rural", "lgrip1", "rgrip1", "vigactf_k", "pain", "mstath")



# 子集筛选
charls_selected <- charls_wave[, charls_cols, drop = FALSE]
hrs_selected    <- hrs_wave[, hrs_cols, drop = FALSE]
share_selected  <- share_wave[, share_cols, drop = FALSE]
klosa_selected  <- klosa_wave[, klosa_cols, drop = FALSE]

############5.07

# 1. 重新编号第一列，命名为 *_selected_1
charls_selected_1 <- charls_selected
charls_selected_1[, 1] <- paste0("charls.", seq_len(nrow(charls_selected_1)))

hrs_selected_1 <- hrs_selected
hrs_selected_1[, 1] <- paste0("hrs.", seq_len(nrow(hrs_selected_1)))

share_selected_1 <- share_selected
share_selected_1[, 1] <- paste0("share.", seq_len(nrow(share_selected_1)))

klosa_selected_1 <- klosa_selected
klosa_selected_1[, 1] <- paste0("klosa.", seq_len(nrow(klosa_selected_1)))

ukb_selected_1 <- ukb_selected
ukb_selected_1[, 1] <- paste0("ukb.", seq_len(nrow(ukb_selected_1)))

# 2. 统一设置新的列名（共 29 列）
new_colnames <- c(
  "sample", "outcome", "time", "age", "educl", "gender", "hibpe", "stroke", "diabe", "cancre",
  "lunge", "hearte", "arthre", "psyche", "height", "weight",
  "smokev", "smoken", "drinkev", "bmi",
  "family_size", "work", "hearaid", "rural", "lgrip", "rgrip", "vgactx", "pain", "marry"
)

colnames(charls_selected_1) <- new_colnames
colnames(hrs_selected_1)   <- new_colnames
colnames(share_selected_1) <- new_colnames
colnames(klosa_selected_1) <- new_colnames
colnames(ukb_selected_1)   <- new_colnames

# 3. ukb 的身高单位转换：cm → m
ukb_selected_1 <- ukb_selected_1 %>%
  mutate(height = height / 100)




####################################################
#####以下内容9月10日以后得数据没进行操作############
####################################################

charls_selected_1 <- charls_selected_1[!is.na(charls_selected[, 2]), ]

hrs_selected_1 <- hrs_selected_1[!is.na(hrs_selected[, 2]), ]

share_selected_1 <- share_selected_1[!is.na(share_selected[, 2]), ]

klosa_selected_1 <- klosa_selected_1[!is.na(klosa_selected[, 2]), ]

ukb_selected_1 <- ukb_selected_1[!is.na(ukb_selected[, 2]), ]



# 4. age < 65 且 time > 0 的筛选
charls_selected_clean <- charls_selected_1 %>%
  filter(age < 65, time > 0)

hrs_selected_clean <- hrs_selected_1 %>%
  filter(age < 65, time > 0)

share_selected_clean <- share_selected_1 %>%
  filter(age < 65, time > 0)

klosa_selected_clean <- klosa_selected_1 %>%
  filter(age < 65, time > 0)

ukb_selected_clean <- ukb_selected_1 %>%
  filter(age < 65, time > 0)


# 展示每个数据框中 outcome 列的分布
outcome_distribution <- function(data, dataset_name) {
  cat(paste("\nOutcome Distribution for", dataset_name, ":\n"))
  print(table(data$outcome))
}

# 分别展示每个数据集的 outcome 分布
outcome_distribution(charls_selected_clean, "charls")
outcome_distribution(hrs_selected_clean, "hrs")
outcome_distribution(share_selected_clean, "share")
outcome_distribution(klosa_selected_clean, "klosa")
outcome_distribution(ukb_selected_clean, "ukb")





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
library(mice)
# caret库在此版本中不再需要，但保留以防万一
# library(caret) 

# --- 1. 合并与初步处理 ---

# 假设您的数据框 charls_selected_clean, hrs_selected_clean, 
# ukb_selected_clean, klosa_selected_clean 已经加载到环境中
# 合并数据
combined_data <- bind_rows(charls_selected_clean, hrs_selected_clean, ukb_selected_clean, klosa_selected_clean)

# 重命名列
colnames(combined_data)[1:2] <- c("sample", "outcome")

# 数据清洗：对 vgactx 列的值进行上限处理
combined_data$vgactx[combined_data$vgactx > 7] <- 7

# (可选) 计算每列缺失值的比例，用于检查
missing_ratio <- sapply(combined_data, function(x) sum(is.na(x)) / length(x))
print("各变量缺失比例:")
print(missing_ratio)


# --- 2. 分层多重插补 (对整个数据集) ---
set.seed(123)  # 保证结果可复现

# 根据 outcome 变量进行分层
data_0 <- combined_data[combined_data$outcome == 0, ]
data_1 <- combined_data[combined_data$outcome == 1, ]

# 对 group = 0 的数据进行插补
# 保留第 1–3 列 (sample, outcome, time)，对其余列进行插补
imp_0 <- mice(data_0[, -c(1, 2, 3)], m = 5, method = 'pmm', seed = 123)
data_0_imputed <- complete(imp_0, 1)
data_0_final <- cbind(data_0[, 1:3], data_0_imputed)

# 对 group = 1 的数据进行插补
# 同样保留第 1–3 列，对其余列进行插补
imp_1 <- mice(data_1[, -c(1, 2, 3)], m = 5, method = 'pmm', seed = 123)
data_1_imputed <- complete(imp_1, 1)
data_1_final <- cbind(data_1[, 1:3], data_1_imputed)

# 合并插补后的两组数据
imputed_full_data <- rbind(data_0_final, data_1_final)


# --- 3. 后处理 ---

# 如果数据中包含身高和体重变量，重新计算BMI
if("weight" %in% names(imputed_full_data) & "height" %in% names(imputed_full_data)) {
  imputed_full_data$bmi <- imputed_full_data$weight / (imputed_full_data$height * imputed_full_data$height)
  print("BMI已重新计算。")
}

# 定义移除离群值的函数
remove_outliers <- function(data, columns) {
  for (col in columns) {
    if (col %in% names(data)) { # 检查列是否存在
      Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      # 移除离群值，保留NA值
      data <- data[ (data[[col]] >= lower_bound & data[[col]] <= upper_bound) | is.na(data[[col]]), ]
    }
  }
  return(data)
}

# 指定要处理离群值的列
columns_to_check <- c("height", "weight", "bmi", "lgrip", "rgrip")

# 对整个数据集移除离群值
final_data_clean <- remove_outliers(imputed_full_data, columns_to_check)


# --- 4. 输出最终文件 ---

# 检查 'time' 列是否存在，如果存在则移除
if ("time" %in% colnames(final_data_clean)) {
  final_data_to_write <- final_data_clean[, -which(colnames(final_data_clean) == "time")]
} else {
  final_data_to_write <- final_data_clean
}

# 将处理完成的数据输出为 "评分不延迟.csv"
write.csv(final_data_to_write, "评分不延迟.csv", row.names = FALSE)


# 1. 前期数据清洗 -------------------------------------------------------

# 拷贝原数据，重命名前两列
share_data <- share_selected_clean %>%
  rename(sample = 1, outcome = 2)
# 对 vgactx 做上限处理
share_data$vgactx[share_data$vgactx > 7] <- 7
# （可选）查看缺失率
missing_ratio <- sapply(share_data, function(x) sum(is.na(x)) / length(x))
print(missing_ratio)

# 2. 分层插补 -------------------------------------------------------------

set.seed(123)
# 按 outcome 分组
data_0 <- filter(share_data, outcome == 0)
data_1 <- filter(share_data, outcome == 1)

# 插补函数：对第 4 列及以后变量进行 m=5 次 pmm 插补，保留第一～三列不变
impute_group <- function(df) {
  imp <- mice(df[, -c(1:3)], m = 5, method = "pmm", seed = 123)
  imputed <- complete(imp, 1)
  bind_cols(df[, 1:3], imputed)
}

share_0_imp <- impute_group(data_0)
share_1_imp <- impute_group(data_1)

# 合并回完整数据
share_imputed <- bind_rows(share_0_imp, share_1_imp)

# 3. 重新计算 BMI（如果有身高和体重） ----------------------------------------
if (all(c("weight", "height") %in% names(share_imputed))) {
  share_imputed <- share_imputed %>%
    mutate(bmi = weight / (height^2))
}

# 4. 去除极值 --------------------------------------------------------------
share_clean <- remove_outliers(share_imputed, columns_to_check)

# 5. 导出结果 --------------------------------------------------------------
write.csv(share_clean[, -which(colnames(share_clean) == "time")], "share.csv", row.names = FALSE)
write.csv(share_clean[, 1:3], "share_ID.csv", row.names = FALSE)



