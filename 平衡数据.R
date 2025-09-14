library(dplyr)
library(mice)

# ----------------- 步骤 1: 准备数据 -----------------
# 请确保以下五个数据集已经加载到您的R环境中
# 如果您的数据框名称不同，请在这里进行修改

# 假设您的原始数据框名称如下：
# ukb_selected_clean
# charls_selected_clean
# hrs_selected_clean
# klosa_selected_clean
# share_selected_clean  <-- 新增了share数据集

# 为了方便循环处理，我们将它们放入一个命名的列表中
# 键名（如 "ukb"）将用于生成输出文件名
dataset_list <- list(
  ukb = ukb_selected_clean,
  charls = charls_selected_clean,
  hrs = hrs_selected_clean,
  klosa = klosa_selected_clean,
  share = share_selected_clean
)

# ----------------- 步骤 2: 定义辅助函数 -----------------

# 定义移除离群值的函数 (与您之前的代码相同，增加了健壮性检查)
remove_outliers <- function(data, columns) {
  for (col in columns) {
    # 仅在数据框中存在该列时才进行处理
    if (col %in% names(data)) {
      Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      data <- data[data[[col]] >= lower_bound & data[[col]] <= upper_bound | is.na(data[[col]]), ]
    }
  }
  return(data)
}


# ----------------- 步骤 3: 定义核心处理函数 -----------------

#' @title process_dataset
#' @description 对单个数据集执行完整的清洗流程：预处理、分层插补、计算BMI、去离群值
#' @param df 数据框，即要处理的数据集
#' @param name 字符串，数据集的名称（用于打印日志和保存文件）
#' @return 清洗后的数据框
process_dataset <- function(df, name) {
  
  cat(paste0("\n--- 开始处理数据集: ", toupper(name), " ---\n"))
  
  # 1. 预处理
  colnames(df)[1:2] <- c("sample", "outcome")
  if ("vgactx" %in% names(df)) {
    df$vgactx[df$vgactx > 7] <- 7
  }
  
  # 2. 按 outcome 分组
  data_0 <- df %>% filter(outcome == 0)
  data_1 <- df %>% filter(outcome == 1)
  
  cat(paste0("数据集 ", name, " 包含 ", nrow(data_0), " 个 outcome=0 样本和 ", nrow(data_1), " 个 outcome=1 样本。\n"))
  
  final_imputed_list <- list() # 用于存储插补后的数据块
  
  # 3. 组内分层插补
  # 插补 group = 0
  if (nrow(data_0) > 0) {
    imp_0 <- mice(data_0[, -c(1, 2, 3)], m = 5, method = 'pmm', seed = 123, printFlag = FALSE)
    imputed_0 <- complete(imp_0, 1)
    final_0 <- cbind(data_0[, 1:3], imputed_0)
    final_imputed_list[['group0']] <- final_0
    cat("Outcome=0 组插补完成。\n")
  }
  
  # 插补 group = 1
  if (nrow(data_1) > 0) {
    imp_1 <- mice(data_1[, -c(1, 2, 3)], m = 5, method = 'pmm', seed = 123, printFlag = FALSE)
    imputed_1 <- complete(imp_1, 1)
    final_1 <- cbind(data_1[, 1:3], imputed_1)
    final_imputed_list[['group1']] <- final_1
    cat("Outcome=1 组插补完成。\n")
  }
  
  # 合并插补后的数据
  df_full <- bind_rows(final_imputed_list)
  
  # 4. 重新计算 BMI
  if ("weight" %in% names(df_full) & "height" %in% names(df_full)) {
    df_full$bmi <- df_full$weight / (df_full$height * df_full$height)
    cat("BMI 值已重新计算。\n")
  }
  
  # 5. 移除离群值
  columns_to_check <- c("height", "weight", "bmi", "lgrip", "rgrip")
  df_clean <- remove_outliers(df_full, columns_to_check)
  cat(paste0("移除离群值后，剩余 ", nrow(df_clean), " 条记录。\n"))
  
  # 6. 保存到文件
  output_filename <- paste0(name, "_cleaned.csv")
  
  # 检查 'time' 列是否存在，如果存在则移除
  if ("time" %in% colnames(df_clean)) {
    write.csv(df_clean[, -which(colnames(df_clean) == "time")], output_filename, row.names = FALSE)
  } else {
    write.csv(df_clean, output_filename, row.names = FALSE)
  }
  
  cat(paste0("--- 数据集 ", toupper(name), " 处理完毕，已保存为 '", output_filename, "' ---\n"))
  
  return(df_clean)
}


# ----------------- 步骤 4: 循环处理所有数据集 -----------------

# 创建一个列表来存储所有清洗后的数据框
cleaned_datasets <- list()

# 使用 for 循环遍历 dataset_list 中的每一个数据集
for (name in names(dataset_list)) {
  data <- dataset_list[[name]]
  cleaned_datasets[[name]] <- process_dataset(data, name)
}

cat("\n所有数据集均已处理并保存完毕！\n")