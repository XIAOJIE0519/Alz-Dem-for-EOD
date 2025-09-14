# --- 第 1 步：安装和加载必要的 R 包 ---
# 检查包是否已安装，如果未安装则自动安装
packages <- c("readxl", "dplyr", "survival", "survminer")
for(pkg in packages){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# --- 第 2 步：设置工作目录 ---
# 建议将R脚本和Excel文件放在同一目录下，并设置工作目录
# 如果您使用RStudio，可以直接通过 Session -> Set Working Directory -> To Source File Location 设置
# 或者取消下面的注释并替换为您的路径
# setwd("C:/您的/文件/路径")

# --- 第 3 步：加载数据 ---
# 加载训练集和测试集
tryCatch({
  train_data <- read_excel("预后训练.xlsx")
  test_data <- read_excel("预后测试.xlsx")
  
  # 检查数据列名是否符合预期
  expected_cols <- c("death", "time", "CoxPH", "RandomForest", "GradientBoosting", "SVM")
  if(!all(expected_cols %in% names(train_data)) | !all(expected_cols %in% names(test_data))){
    stop("数据列名与预期不符，请检查Excel文件是否包含 'death', 'time', 'CoxPH', 'RandomForest', 'GradientBoosting', 'SVM' 这些列。")
  }
  
  # 将列名 'death' 和 'time' 转换为小写，以防大小写问题
  names(train_data) <- tolower(names(train_data))
  names(test_data) <- tolower(names(test_data))
  
  # 将模型名字保持原样以便于文件命名和标题
  model_names_original <- c("CoxPH", "RandomForest", "GradientBoosting", "SVM")
  
}, error = function(e) {
  stop(paste("无法加载Excel文件，请确保 '预后训练.xlsx' 和 '预后测试.xlsx' 文件存在于工作目录中。", e))
})


# --- 第 4 步：循环处理每个模型，生成 KM 曲线图 ---
# 定义模型名称列表
model_names <- c("coxph", "randomforest", "gradientboosting", "svm") 
# 注意：这里我们使用小写，因为前面将所有列名都转为了小写

# 循环开始
for (i in 1:length(model_names)) {
  
  model_name <- model_names[i]
  model_name_original <- model_names_original[i] # 用于标题和文件名
  
  cat(paste0("\n--- 开始处理模型: ", model_name_original, " ---\n"))
  
  # 步骤 4.1: 在训练集上寻找最佳阈值
  # surv_cutpoint 函数可以找到最大化log-rank统计量的最佳切点
  res.cut <- surv_cutpoint(
    train_data,
    time = "time",
    event = "death",
    variables = model_name
  )
  
  cutoff_value <- res.cut$cutpoint$cutpoint
  cat(paste0("模型 '", model_name_original, "' 在训练集上找到的最佳阈值为: ", round(cutoff_value, 4), "\n"))
  
  # 步骤 4.2: 使用此阈值对训练集和测试集进行分组
  # 训练集分组
  train_data$risk_group <- ifelse(train_data[[model_name]] > cutoff_value, "High Risk", "Low Risk")
  train_data$risk_group <- factor(train_data$risk_group, levels = c("Low Risk", "High Risk"))
  
  # 测试集分组
  test_data$risk_group <- ifelse(test_data[[model_name]] > cutoff_value, "High Risk", "Low Risk")
  test_data$risk_group <- factor(test_data$risk_group, levels = c("Low Risk", "High Risk"))
  
  
  # --- 步骤 4.3: 绘制并保存训练集的 KM 曲线 ---
  
  # 创建生存对象
  fit_train <- survfit(Surv(time, death) ~ risk_group, data = train_data)
  
  # 定义输出文件名
  pdf_train_filename <- paste0("KM_", model_name_original, "_Train.pdf")
  
  # 开始绘制
  # 使用ggsurvplot生成高度可定制的图
  p_train <- ggsurvplot(
    fit_train,
    data = train_data,
    pval = TRUE,                       # 显示log-rank检验的p值
    conf.int = TRUE,                   # 显示置信区间
    risk.table = TRUE,                 # 在下方添加风险表
    risk.table.col = "strata",         # 风险表按分组着色
    risk.table.y.text = FALSE,         # 不显示风险表的Y轴文本
    risk.table.height = 0.25,          # 风险表的高度比例
    tables.theme = theme_cleantable(), # 简洁的表格主题
    
    # 主要曲线设置
    palette = c("#0073C2FF", "#E7B800FF"), # 设置颜色 (蓝色, 黄色)
    xlab = "Time in years",            # X轴标签
    ylab = "Overall Survival",         # Y轴标签
    legend.title = "Risk Group",       # 图例标题
    legend.labs = c("Low Risk", "High Risk"), # 图例标签
    
    # 图表整体设置
    title = paste(model_name_original, "Model - Training Set"), # 主标题
    ggtheme = theme_survminer(          # 使用预设主题
      font.main = c(16, "bold", "darkblue"),
      font.x = c(14, "plain", "black"),
      font.y = c(14, "plain", "black"),
      font.tickslab = c(12, "plain", "black"),
      font.legend = c(12, "plain", "black")
    ),
    break.time.by = 5,                 # X轴刻度间隔为5年
    
    # 删失点(censor)设置
    censor.shape = "|",                # 审查数据的标记形状
    censor.size = 4,                   # 审查数据标记的大小
    censor.col = "gray"                # *** 新增：设置删失点颜色为灰色 ***
  )
  
  # 在图的左下角添加阈值信息
  p_train$plot <- p_train$plot + 
    ggplot2::annotate("text", 
                      x = 0.5, y = 0.1, # 调整位置 (x轴靠近0, y轴靠近底部)
                      label = paste("Cutoff value =", round(cutoff_value, 3)), 
                      size = 4, hjust = 0) # hjust=0 左对齐
  
  # 保存为PDF
  pdf(pdf_train_filename, width = 8, height = 7)
  print(p_train, newpage = FALSE) # newpage=FALSE 防止生成空白页
  dev.off()
  
  cat(paste0("已生成训练集KM曲线图: ", pdf_train_filename, "\n"))
  
  
  # --- 步骤 4.4: 绘制并保存测试集的 KM 曲线 ---
  
  # 创建生存对象
  fit_test <- survfit(Surv(time, death) ~ risk_group, data = test_data)
  
  # 定义输出文件名
  pdf_test_filename <- paste0("KM_", model_name_original, "_Test.pdf")
  
  # 开始绘制
  p_test <- ggsurvplot(
    fit_test,
    data = test_data,
    pval = TRUE,
    conf.int = TRUE,
    risk.table = TRUE,
    risk.table.col = "strata",
    risk.table.y.text = FALSE,
    risk.table.height = 0.25,
    tables.theme = theme_cleantable(),
    
    palette = c("#0073C2FF", "#E7B800FF"),
    xlab = "Time in years",
    ylab = "Overall Survival",
    legend.title = "Risk Group",
    legend.labs = c("Low Risk", "High Risk"),
    
    title = paste(model_name_original, "Model - Testing Set"), # 主标题
    ggtheme = theme_survminer(
      font.main = c(16, "bold", "darkred"), # 测试集用不同颜色标题
      font.x = c(14, "plain", "black"),
      font.y = c(14, "plain", "black"),
      font.tickslab = c(12, "plain", "black"),
      font.legend = c(12, "plain", "black")
    ),
    break.time.by = 5,
    
    # 删失点(censor)设置
    censor.shape = "|",
    censor.size = 4,
    censor.col = "gray"                # *** 新增：设置删失点颜色为灰色 ***
  )
  
  # 在图的左下角添加阈值信息 (使用与训练集相同的阈值)
  p_test$plot <- p_test$plot + 
    ggplot2::annotate("text", 
                      x = 0.5, y = 0.1, 
                      label = paste("Cutoff value (from training set) =", round(cutoff_value, 3)), 
                      size = 4, hjust = 0)
  
  # 保存为PDF
  pdf(pdf_test_filename, width = 8, height = 7)
  print(p_test, newpage = FALSE)
  dev.off()
  
  cat(paste0("已生成测试集KM曲线图: ", pdf_test_filename, "\n"))
}

cat("\n所有模型的KM曲线图已生成完毕！\n")






####################################################################
# 7.4 版本：ROC
####################################################################

# 加载所需包
library(pROC)
library(PRROC)
library(caret)
library(ggplot2)
library(dplyr)

# 读取数据
train_data <- read.csv("训练1.csv")
valid_data <- read.csv("验证.csv")

# 定义模型列表
base_models <- c("NaiveBayes", "LDA", "LogisticRegression", 
                 "ElasticNet", "DecisionTree", "RandomForest", "ExtraTrees",
                 "AdaBoost", "XGBoost", "CatBoost", "MLP")

# 为这11个模型指定固定阈值
fixed_thresholds <- c(
  NaiveBayes        = 0.712,
  LDA               = 0.551,
  LogisticRegression= 0.589,
  ElasticNet        = 0.680,
  DecisionTree      = 0.547,
  RandomForest      = 0.505,
  ExtraTrees        = 0.509,
  AdaBoost          = 0.507,
  XGBoost           = 0.510,
  CatBoost          = 0.523,
  MLP               = 0.541
)

# 计算AUROC及其标准误差的函数
calc_auroc_with_se <- function(labels, preds) {
  roc_obj <- roc(labels, preds, levels = c(0,1), direction = "<")
  auc_val <- as.numeric(auc(roc_obj))
  auc_var <- var(roc_obj)
  auc_se <- sqrt(auc_var)
  
  # 95%置信区间
  ci_lower <- auc_val - 1.96 * auc_se
  ci_upper <- auc_val + 1.96 * auc_se
  
  list(estimate = auc_val, se = auc_se, ci_lower = ci_lower, ci_upper = ci_upper)
}

# 计算AUPRC及其标准误差的函数（使用近似方法）
calc_aupr_with_se <- function(labels, preds) {
  pr_obj <- pr.curve(scores.class0 = preds[labels==1],
                     scores.class1 = preds[labels==0],
                     curve = TRUE)
  aupr_val <- pr_obj$auc.integral
  
  # AUPRC标准误差的近似计算
  n_pos <- sum(labels == 1)
  n_neg <- sum(labels == 0)
  n_total <- length(labels)
  
  # 使用简化的方差估计公式
  aupr_se <- sqrt(aupr_val * (1 - aupr_val) / n_total)
  
  # 95%置信区间
  ci_lower <- aupr_val - 1.96 * aupr_se
  ci_upper <- aupr_val + 1.96 * aupr_se
  
  list(estimate = aupr_val, se = aupr_se, ci_lower = ci_lower, ci_upper = ci_upper)
}

# 计算二分类指标及其标准误差的函数
calc_binary_metrics_with_se <- function(labels, preds_binary) {
  n <- length(labels)
  
  # 混淆矩阵元素
  tp <- sum(labels == 1 & preds_binary == 1)
  tn <- sum(labels == 0 & preds_binary == 0)
  fp <- sum(labels == 0 & preds_binary == 1)
  fn <- sum(labels == 1 & preds_binary == 0)
  
  # Accuracy
  acc <- (tp + tn) / n
  acc_se <- sqrt(acc * (1 - acc) / n)
  acc_ci_lower <- acc - 1.96 * acc_se
  acc_ci_upper <- acc + 1.96 * acc_se
  
  # Precision
  if (tp + fp == 0) {
    prec <- 0
    prec_se <- 0
    prec_ci_lower <- 0
    prec_ci_upper <- 0
  } else {
    prec <- tp / (tp + fp)
    prec_se <- sqrt(prec * (1 - prec) / (tp + fp))
    prec_ci_lower <- prec - 1.96 * prec_se
    prec_ci_upper <- prec + 1.96 * prec_se
  }
  
  # Recall
  if (tp + fn == 0) {
    rec <- 0
    rec_se <- 0
    rec_ci_lower <- 0
    rec_ci_upper <- 0
  } else {
    rec <- tp / (tp + fn)
    rec_se <- sqrt(rec * (1 - rec) / (tp + fn))
    rec_ci_lower <- rec - 1.96 * rec_se
    rec_ci_upper <- rec + 1.96 * rec_se
  }
  
  # F1 Score
  if (prec + rec == 0) {
    f1 <- 0
    f1_se <- 0
    f1_ci_lower <- 0
    f1_ci_upper <- 0
  } else {
    f1 <- 2 * prec * rec / (prec + rec)
    # F1标准误差使用delta方法的近似
    f1_se <- sqrt((4 * rec^2 * prec_se^2 + 4 * prec^2 * rec_se^2) / (prec + rec)^4)
    f1_ci_lower <- f1 - 1.96 * f1_se
    f1_ci_upper <- f1 + 1.96 * f1_se
  }
  
  list(
    accuracy = list(estimate = acc, ci_lower = acc_ci_lower, ci_upper = acc_ci_upper),
    precision = list(estimate = prec, ci_lower = prec_ci_lower, ci_upper = prec_ci_upper),
    recall = list(estimate = rec, ci_lower = rec_ci_lower, ci_upper = rec_ci_upper),
    f1 = list(estimate = f1, ci_lower = f1_ci_lower, ci_upper = f1_ci_upper)
  )
}

# 格式化函数
format_with_ci <- function(estimate, ci_lower, ci_upper) {
  # 确保置信区间在合理范围内
  ci_lower <- max(0, ci_lower)
  ci_upper <- min(1, ci_upper)
  sprintf("%.3f (%.3f-%.3f)", estimate, ci_lower, ci_upper)
}

# 计算完整指标的函数
calc_all_metrics_with_se <- function(labels, preds, threshold = NULL, is_binary = FALSE) {
  if (!is_binary) {
    # 计算AUROC和AUPRC
    auroc_result <- calc_auroc_with_se(labels, preds)
    aupr_result <- calc_aupr_with_se(labels, preds)
    
    # 如果提供了阈值，使用该阈值；否则用Youden方法
    if (!is.null(threshold)) {
      preds_binary <- ifelse(preds >= threshold, 1, 0)
      used_threshold <- threshold
    } else {
      roc_obj <- roc(labels, preds, levels = c(0,1), direction = "<")
      coords_result <- coords(roc_obj, "best", ret="threshold", best.method="youden")
      used_threshold <- coords_result$threshold
      preds_binary <- ifelse(preds >= used_threshold, 1, 0)
    }
  } else {
    # 对于硬投票等二分类情况
    auroc_result <- calc_auroc_with_se(labels, preds)
    aupr_result <- calc_aupr_with_se(labels, preds)
    preds_binary <- preds
    used_threshold <- NA
  }
  
  # 计算二分类指标
  binary_metrics <- calc_binary_metrics_with_se(labels, preds_binary)
  
  list(
    AUROC = format_with_ci(auroc_result$estimate, auroc_result$ci_lower, auroc_result$ci_upper),
    AUPRC = format_with_ci(aupr_result$estimate, aupr_result$ci_lower, aupr_result$ci_upper),
    Accuracy = format_with_ci(binary_metrics$accuracy$estimate, 
                              binary_metrics$accuracy$ci_lower, 
                              binary_metrics$accuracy$ci_upper),
    Precision = format_with_ci(binary_metrics$precision$estimate, 
                               binary_metrics$precision$ci_lower, 
                               binary_metrics$precision$ci_upper),
    Recall = format_with_ci(binary_metrics$recall$estimate, 
                            binary_metrics$recall$ci_lower, 
                            binary_metrics$recall$ci_upper),
    F1 = format_with_ci(binary_metrics$f1$estimate, 
                        binary_metrics$f1$ci_lower, 
                        binary_metrics$f1$ci_upper),
    Threshold = ifelse(is.na(used_threshold), NA, round(used_threshold, 3))
  )
}

# 初始化结果存储
results_train <- data.frame(Model = character(), AUROC = character(), AUPRC = character(),
                            Accuracy = character(), Precision = character(), 
                            Recall = character(), F1 = character(), Threshold = numeric(),
                            stringsAsFactors = FALSE)

results_valid <- data.frame(Model = character(), AUROC = character(), AUPRC = character(),
                            Accuracy = character(), Precision = character(), 
                            Recall = character(), F1 = character(), Threshold = numeric(),
                            stringsAsFactors = FALSE)

# 计算基础模型的权重（用于软投票）
temp_results <- data.frame(Model = character(), AUROC = numeric(), AUPR = numeric(),
                           stringsAsFactors = FALSE)

for (model in base_models) {
  roc_obj <- roc(train_data$outcome, train_data[[model]], levels = c(0,1), direction = "<")
  pr_obj <- pr.curve(scores.class0 = train_data[[model]][train_data$outcome==1],
                     scores.class1 = train_data[[model]][train_data$outcome==0],
                     curve = TRUE)
  temp_results <- rbind(temp_results,
                        data.frame(Model = model,
                                   AUROC = as.numeric(auc(roc_obj)),
                                   AUPR = pr_obj$auc.integral,
                                   stringsAsFactors = FALSE))
}

# 计算软投票权重
weights <- sapply(temp_results$Model, function(m) {
  row <- temp_results[temp_results$Model==m,]
  (row$AUROC + row$AUPR)/2
})
weights <- weights / sum(weights)

# 计算软投票分数
train_data$SoftVoting <- Reduce(`+`,
                                lapply(seq_along(base_models),
                                       function(i) weights[i] * train_data[[ base_models[i] ]]))
valid_data$SoftVoting <- Reduce(`+`,
                                lapply(seq_along(base_models),
                                       function(i) weights[i] * valid_data[[ base_models[i] ]]))

# 计算软投票的最佳阈值（基于训练集）
roc_obj_soft_train <- roc(train_data$outcome, train_data$SoftVoting, levels = c(0,1), direction = "<")
coords_soft <- coords(roc_obj_soft_train, "best", ret="threshold", best.method="youden")
soft_voting_threshold <- coords_soft$threshold

# ================ 处理训练集 ================
print("正在计算训练集基础模型指标...")
for (model in base_models) {
  cat("处理模型:", model, "\n")
  metrics <- calc_all_metrics_with_se(train_data$outcome, train_data[[model]], 
                                      threshold = fixed_thresholds[[model]])
  
  results_train <- rbind(results_train,
                         data.frame(Model = model,
                                    AUROC = metrics$AUROC,
                                    AUPRC = metrics$AUPRC,
                                    Accuracy = metrics$Accuracy,
                                    Precision = metrics$Precision,
                                    Recall = metrics$Recall,
                                    F1 = metrics$F1,
                                    Threshold = fixed_thresholds[[model]],
                                    stringsAsFactors = FALSE))
  
  # 保存二分类预测用于硬投票
  train_data[[paste0(model, "_Pred_Binary")]] <- 
    ifelse(train_data[[model]] >= fixed_thresholds[[model]], 1, 0)
}

# 训练集软投票
print("正在计算训练集软投票指标...")
metrics_soft_train <- calc_all_metrics_with_se(train_data$outcome, train_data$SoftVoting,
                                               threshold = soft_voting_threshold)
results_train <- rbind(results_train,
                       data.frame(Model = "SoftVoting",
                                  AUROC = metrics_soft_train$AUROC,
                                  AUPRC = metrics_soft_train$AUPRC,
                                  Accuracy = metrics_soft_train$Accuracy,
                                  Precision = metrics_soft_train$Precision,
                                  Recall = metrics_soft_train$Recall,
                                  F1 = metrics_soft_train$F1,
                                  Threshold = soft_voting_threshold,
                                  stringsAsFactors = FALSE))

# 训练集硬投票
print("正在计算训练集硬投票指标...")
hard_votes_train <- rowSums(train_data[, paste0(base_models, "_Pred_Binary")]) >= 6
hard_bin_train <- as.integer(hard_votes_train)
train_data$HardVoting <- hard_bin_train

metrics_hard_train <- calc_all_metrics_with_se(train_data$outcome, hard_bin_train, 
                                               is_binary = TRUE)
results_train <- rbind(results_train,
                       data.frame(Model = "HardVoting",
                                  AUROC = metrics_hard_train$AUROC,
                                  AUPRC = metrics_hard_train$AUPRC,
                                  Accuracy = metrics_hard_train$Accuracy,
                                  Precision = metrics_hard_train$Precision,
                                  Recall = metrics_hard_train$Recall,
                                  F1 = metrics_hard_train$F1,
                                  Threshold = NA,
                                  stringsAsFactors = FALSE))

# ================ 处理验证集 ================
print("正在计算验证集基础模型指标...")
for (model in base_models) {
  cat("处理模型:", model, "\n")
  metrics <- calc_all_metrics_with_se(valid_data$outcome, valid_data[[model]], 
                                      threshold = fixed_thresholds[[model]])
  
  results_valid <- rbind(results_valid,
                         data.frame(Model = model,
                                    AUROC = metrics$AUROC,
                                    AUPRC = metrics$AUPRC,
                                    Accuracy = metrics$Accuracy,
                                    Precision = metrics$Precision,
                                    Recall = metrics$Recall,
                                    F1 = metrics$F1,
                                    Threshold = fixed_thresholds[[model]],
                                    stringsAsFactors = FALSE))
  
  # 保存二分类预测用于硬投票
  valid_data[[paste0(model, "_Pred_Binary")]] <- 
    ifelse(valid_data[[model]] >= fixed_thresholds[[model]], 1, 0)
}

# 验证集软投票
print("正在计算验证集软投票指标...")
metrics_soft_valid <- calc_all_metrics_with_se(valid_data$outcome, valid_data$SoftVoting,
                                               threshold = soft_voting_threshold)
results_valid <- rbind(results_valid,
                       data.frame(Model = "SoftVoting",
                                  AUROC = metrics_soft_valid$AUROC,
                                  AUPRC = metrics_soft_valid$AUPRC,
                                  Accuracy = metrics_soft_valid$Accuracy,
                                  Precision = metrics_soft_valid$Precision,
                                  Recall = metrics_soft_valid$Recall,
                                  F1 = metrics_soft_valid$F1,
                                  Threshold = soft_voting_threshold,
                                  stringsAsFactors = FALSE))

# 验证集硬投票
print("正在计算验证集硬投票指标...")
hard_votes_valid <- rowSums(valid_data[, paste0(base_models, "_Pred_Binary")]) >= 6
hard_bin_valid <- as.integer(hard_votes_valid)
valid_data$HardVoting <- hard_bin_valid

metrics_hard_valid <- calc_all_metrics_with_se(valid_data$outcome, hard_bin_valid, 
                                               is_binary = TRUE)
results_valid <- rbind(results_valid,
                       data.frame(Model = "HardVoting",
                                  AUROC = metrics_hard_valid$AUROC,
                                  AUPRC = metrics_hard_valid$AUPRC,
                                  Accuracy = metrics_hard_valid$Accuracy,
                                  Precision = metrics_hard_valid$Precision,
                                  Recall = metrics_hard_valid$Recall,
                                  F1 = metrics_hard_valid$F1,
                                  Threshold = NA,
                                  stringsAsFactors = FALSE))

# ================ 保存评估结果 ================
write.csv(results_train, "训练集结果_含置信区间.csv", row.names = FALSE)
write.csv(results_valid, "验证集结果_含置信区间.csv", row.names = FALSE)

# ================ 保存每个样本的模型评分 ================
print("正在保存样本评分...")

# 准备所有模型的列名
all_models <- c(base_models, "SoftVoting", "HardVoting")

# 训练集样本评分
train_scores <- train_data[, c("outcome", all_models)]
write.csv(train_scores, "训练集样本评分.csv", row.names = FALSE)

# 验证集样本评分
valid_scores <- valid_data[, c("outcome", all_models)]
write.csv(valid_scores, "验证集样本评分.csv", row.names = FALSE)

print("计算完成！结果已保存到:")
print("评估结果文件:")
print("- 训练集结果_含置信区间.csv")
print("- 验证集结果_含置信区间.csv")
print("样本评分文件:")
print("- 训练集样本评分.csv")
print("- 验证集样本评分.csv")

# 显示模型权重信息
print("\n软投票权重:")
for (i in seq_along(base_models)) {
  cat(sprintf("%s: %.4f\n", base_models[i], weights[i]))
}
cat(sprintf("软投票阈值: %.3f\n", soft_voting_threshold))




####################################################################
# 完整诊断绘图代码 - 包含必要的辅助函数
####################################################################

# 加载绘图包
library(ggplot2)
library(RColorBrewer)
library(pROC)
library(PRROC)
library(Cairo)

# 计算AUROC及其标准误差的函数
calc_auroc_with_se <- function(labels, preds) {
  roc_obj <- roc(labels, preds, levels = c(0,1), direction = "<")
  auc_val <- as.numeric(auc(roc_obj))
  auc_var <- var(roc_obj)
  auc_se <- sqrt(auc_var)
  
  # 95%置信区间
  ci_lower <- max(0, auc_val - 1.96 * auc_se)
  ci_upper <- min(1, auc_val + 1.96 * auc_se)
  
  list(estimate = auc_val, se = auc_se, ci_lower = ci_lower, ci_upper = ci_upper, roc_obj = roc_obj)
}

# 计算AUPRC及其标准误差的函数
calc_aupr_with_se <- function(labels, preds) {
  pr_obj <- pr.curve(scores.class0 = preds[labels==1],
                     scores.class1 = preds[labels==0],
                     curve = TRUE)
  aupr_val <- pr_obj$auc.integral
  
  # AUPRC标准误差的近似计算
  n_total <- length(labels)
  aupr_se <- sqrt(aupr_val * (1 - aupr_val) / n_total)
  
  # 95%置信区间
  ci_lower <- max(0, aupr_val - 1.96 * aupr_se)
  ci_upper <- min(1, aupr_val + 1.96 * aupr_se)
  
  list(estimate = aupr_val, se = aupr_se, ci_lower = ci_lower, ci_upper = ci_upper, pr_obj = pr_obj)
}

# 所有模型列表
all_models <- c(base_models, "SoftVoting", "HardVoting")

# 美观的颜色方案
set.seed(123)
colors <- c(
  brewer.pal(11, "Set3"),
  brewer.pal(3, "Dark2")[1:2]
)

# 线型设置
line_types <- c(rep("solid", 11), "dashed", "dotted")

# 模型名称美化
model_labels <- c(
  "Naive Bayes", "LDA", "Logistic Regression", 
  "Elastic Net", "Decision Tree", "Random Forest", "Extra Trees",
  "AdaBoost", "XGBoost", "CatBoost", "MLP",
  "Soft Voting", "Hard Voting"
)

# 绘制ROC曲线的函数
plot_roc_curves <- function(data, dataset_name) {
  roc_data <- data.frame()
  legend_labels <- character()
  
  for (i in seq_along(all_models)) {
    model <- all_models[i]
    
    # 确保模型列存在
    if (!model %in% colnames(data)) {
      cat("警告: 模型", model, "在数据中不存在，跳过\n")
      next
    }
    
    # 计算ROC
    auroc_result <- calc_auroc_with_se(data$outcome, data[[model]])
    roc_coords <- coords(auroc_result$roc_obj, "all", ret = c("threshold", "sensitivity", "specificity"))
    
    # 准备数据
    model_data <- data.frame(
      FPR = 1 - roc_coords$specificity,
      TPR = roc_coords$sensitivity,
      Model = model_labels[i]
    )
    
    roc_data <- rbind(roc_data, model_data)
    
    # 准备图例标签
    legend_labels[i] <- sprintf("%s: %.3f (%.3f-%.3f)", 
                                model_labels[i], 
                                auroc_result$estimate,
                                auroc_result$ci_lower,
                                auroc_result$ci_upper)
  }
  roc_data$Model <- factor(roc_data$Model, levels = model_labels)
  
  # 绘图
  p <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
    geom_line(aes(linetype = Model), size = 1.0, alpha = 0.8) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40", size = 0.6) +
    scale_color_manual(values = colors, labels = legend_labels) +
    scale_linetype_manual(values = line_types, labels = legend_labels) +
    labs(
      title = paste("ROC Curves -", dataset_name),
      x = "False Positive Rate (1 - Specificity)",
      y = "True Positive Rate (Sensitivity)",
      color = "Model (AUROC with 95% CI)",
      linetype = "Model (AUROC with 95% CI)"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 20)),
      axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
      axis.text = element_text(size = 12, color = "black"),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "bottom",
      legend.margin = margin(t = 15),
      panel.grid.major = element_line(color = "gray85", size = 0.4),
      panel.grid.minor = element_line(color = "gray92", size = 0.2),
      panel.border = element_rect(color = "black", fill = NA, size = 0.8),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    guides(
      color = guide_legend(ncol = 2, byrow = TRUE, keywidth = 2, keyheight = 1),
      linetype = guide_legend(ncol = 2, byrow = TRUE, keywidth = 2, keyheight = 1)
    ) +
    coord_fixed(ratio = 1) +
    scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1))
  
  return(p)
}

# 绘制PR曲线的函数
plot_pr_curves <- function(data, dataset_name) {
  pr_data <- data.frame()
  legend_labels <- character()
  
  for (i in seq_along(all_models)) {
    model <- all_models[i]
    
    # 确保模型列存在
    if (!model %in% colnames(data)) {
      cat("警告: 模型", model, "在数据中不存在，跳过\n")
      next
    }
    
    # 计算AUPR
    aupr_result <- calc_aupr_with_se(data$outcome, data[[model]])
    
    # 准备数据
    model_data <- data.frame(
      Recall = aupr_result$pr_obj$curve[, 1],
      Precision = aupr_result$pr_obj$curve[, 2],
      Model = model_labels[i]
    )
    
    pr_data <- rbind(pr_data, model_data)
    
    # 准备图例标签
    legend_labels[i] <- sprintf("%s: %.3f (%.3f-%.3f)", 
                                model_labels[i], 
                                aupr_result$estimate,
                                aupr_result$ci_lower,
                                aupr_result$ci_upper)
  }
  pr_data$Model <- factor(pr_data$Model, levels = model_labels)
  
  # 计算基线（正样本比例）
  baseline <- mean(data$outcome)
  
  # 绘图
  p <- ggplot(pr_data, aes(x = Recall, y = Precision, color = Model)) +
    geom_line(aes(linetype = Model), size = 1.0, alpha = 0.8) +
    geom_hline(yintercept = baseline, linetype = "dashed", color = "gray40", size = 0.6,
               alpha = 0.7) +
    annotate("text", x = 0.02, y = baseline + 0.02, 
             label = sprintf("Baseline: %.3f", baseline), 
             hjust = 0, size = 3.5, color = "gray30") +
    scale_color_manual(values = colors, labels = legend_labels) +
    scale_linetype_manual(values = line_types, labels = legend_labels) +
    labs(
      title = paste("Precision-Recall Curves -", dataset_name),
      x = "Recall (Sensitivity)",
      y = "Precision (Positive Predictive Value)",
      color = "Model (AUPRC with 95% CI)",
      linetype = "Model (AUPRC with 95% CI)"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 20)),
      axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
      axis.text = element_text(size = 12, color = "black"),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "bottom",
      legend.margin = margin(t = 15),
      panel.grid.major = element_line(color = "gray85", size = 0.4),
      panel.grid.minor = element_line(color = "gray92", size = 0.2),
      panel.border = element_rect(color = "black", fill = NA, size = 0.8),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    guides(
      color = guide_legend(ncol = 2, byrow = TRUE, keywidth = 2, keyheight = 1),
      linetype = guide_legend(ncol = 2, byrow = TRUE, keywidth = 2, keyheight = 1)
    ) +
    scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1))
  
  return(p)
}

# 绘制并保存图形
print("正在绘制ROC和PR曲线...")

# 训练集ROC
print("绘制训练集ROC曲线...")
roc_train_plot <- plot_roc_curves(train_data, "Training Set")
ggsave("训练集ROC曲线.pdf", roc_train_plot, 
       width = 14, height = 11, device = cairo_pdf, dpi = 300)

# 验证集ROC
print("绘制验证集ROC曲线...")
roc_valid_plot <- plot_roc_curves(valid_data, "Validation Set")
ggsave("验证集ROC曲线.pdf", roc_valid_plot, 
       width = 14, height = 11, device = cairo_pdf, dpi = 300)

# 训练集PR
print("绘制训练集PR曲线...")
pr_train_plot <- plot_pr_curves(train_data, "Training Set")
ggsave("训练集PR曲线.pdf", pr_train_plot, 
       width = 14, height = 11, device = cairo_pdf, dpi = 300)

# 验证集PR
print("绘制验证集PR曲线...")
pr_valid_plot <- plot_pr_curves(valid_data, "Validation Set")
ggsave("验证集PR曲线.pdf", pr_valid_plot, 
       width = 14, height = 11, device = cairo_pdf, dpi = 300)

print("绘图完成！生成的文件:")
print("- 训练集ROC曲线.pdf")
print("- 验证集ROC曲线.pdf") 
print("- 训练集PR曲线.pdf")
print("- 验证集PR曲线.pdf")



####################################################################
# CatBoost模型性能评估 - ROC, PRC, 混淆矩阵（使用标准误差法）
####################################################################

# 加载所需包
library(ggplot2)
library(pROC)
library(PRROC)
library(RColorBrewer)
library(dplyr)
library(caret)
library(reshape2)
library(gridExtra)

# 读取数据
catboost_data <- read.csv("Catboost.csv")

# 查看数据结构
print("数据结构:")
print(head(catboost_data))
print(str(catboost_data))

# 假设列名为：outcome, group, score
colnames(catboost_data) <- c("outcome", "group", "score")

# 分割数据
train_data <- catboost_data[catboost_data$group == "train", ]
test_data <- catboost_data[catboost_data$group == "test", ]
valid_data <- catboost_data[catboost_data$group == "valid", ]



# 数据集列表
datasets <- list(
  "Train" = train_data,
  "Test" = test_data,
  "Valid" = valid_data
)
# 设置阈值
threshold <- 0.523
# 美观的颜色设置
primary_color <- "#2E86AB"
secondary_color <- "#A23B72"
accent_color <- "#F18F01"

# 1. ROC曲线（使用标准误差法计算置信区间）
plot_roc_with_ci <- function(data, dataset_name) {
  # 计算ROC
  roc_obj <- roc(data$outcome, data$score, levels = c(0, 1), direction = "<")
  
  # 计算AUC的置信区间
  ci_obj <- ci.auc(roc_obj, conf.level = 0.95, method = "delong")
  
  # 获取ROC坐标
  roc_coords <- coords(roc_obj, "all", ret = c("threshold", "sensitivity", "specificity"))
  
  # 计算敏感性和特异性的标准误差
  n_pos <- sum(data$outcome == 1)
  n_neg <- sum(data$outcome == 0)
  
  # 使用标准误差公式计算置信区间
  sensitivity_se <- sqrt(roc_coords$sensitivity * (1 - roc_coords$sensitivity) / n_pos)
  specificity_se <- sqrt(roc_coords$specificity * (1 - roc_coords$specificity) / n_neg)
  
  # 计算置信区间
  sens_ci_lower <- pmax(0, roc_coords$sensitivity - 1.96 * sensitivity_se)
  sens_ci_upper <- pmin(1, roc_coords$sensitivity + 1.96 * sensitivity_se)
  
  # 准备绘图数据
  plot_data <- data.frame(
    FPR = 1 - roc_coords$specificity,
    TPR = roc_coords$sensitivity,
    CI_Lower = sens_ci_lower,
    CI_Upper = sens_ci_upper
  )
  
  # 移除NA值
  valid_idx <- complete.cases(plot_data)
  plot_data <- plot_data[valid_idx, ]
  
  # 绘图
  p <- ggplot(plot_data, aes(x = FPR, y = TPR)) +
    geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), alpha = 0.3, fill = primary_color) +
    geom_line(color = primary_color, size = 1.2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40", size = 0.6) +
    labs(
      title = paste("ROC Curve -", dataset_name),
      subtitle = sprintf("AUC: %.3f (%.3f-%.3f)", 
                         as.numeric(auc(roc_obj)), 
                         as.numeric(ci_obj[1]), 
                         as.numeric(ci_obj[3])),
      x = "False Positive Rate (1 - Specificity)",
      y = "True Positive Rate (Sensitivity)"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10, color = "black"),
      panel.grid.major = element_line(color = "gray85", size = 0.4),
      panel.grid.minor = element_line(color = "gray92", size = 0.2),
      panel.border = element_rect(color = "black", fill = NA, size = 0.8),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    coord_fixed(ratio = 1) +
    scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1))
  
  return(p)
}

# 2. PRC曲线（使用标准误差法计算置信区间）
plot_prc_with_ci <- function(data, dataset_name) {
  # 计算PR曲线
  pr_obj <- pr.curve(scores.class0 = data$score[data$outcome == 1],
                     scores.class1 = data$score[data$outcome == 0],
                     curve = TRUE)
  
  # 计算AUPRC的标准误差
  n_total <- nrow(data)
  n_pos <- sum(data$outcome == 1)
  aupr_val <- pr_obj$auc.integral
  
  # AUPRC标准误差的近似计算
  aupr_se <- sqrt(aupr_val * (1 - aupr_val) / n_total)
  aupr_ci_lower <- max(0, aupr_val - 1.96 * aupr_se)
  aupr_ci_upper <- min(1, aupr_val + 1.96 * aupr_se)
  
  baseline <- mean(data$outcome)
  
  # 准备绘图数据
  plot_data <- data.frame(
    Recall = pr_obj$curve[, 1],
    Precision = pr_obj$curve[, 2]
  )
  
  # 计算Precision的标准误差
  precision_se <- sqrt(plot_data$Precision * (1 - plot_data$Precision) / n_pos)
  plot_data$CI_Lower <- pmax(0, plot_data$Precision - 1.96 * precision_se)
  plot_data$CI_Upper <- pmin(1, plot_data$Precision + 1.96 * precision_se)
  
  # 移除NA值
  valid_idx <- complete.cases(plot_data)
  plot_data <- plot_data[valid_idx, ]
  
  # 绘图
  p <- ggplot(plot_data, aes(x = Recall, y = Precision)) +
    geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), alpha = 0.3, fill = secondary_color) +
    geom_line(color = secondary_color, size = 1.2) +
    geom_hline(yintercept = baseline, linetype = "dashed", color = "gray40", size = 0.6) +
    annotate("text", x = 0.02, y = baseline + 0.02, 
             label = sprintf("Baseline: %.3f", baseline), 
             hjust = 0, size = 3.5, color = "gray30") +
    labs(
      title = paste("Precision-Recall Curve -", dataset_name),
      subtitle = sprintf("AUPRC: %.3f (%.3f-%.3f)", 
                         aupr_val, 
                         aupr_ci_lower, 
                         aupr_ci_upper),
      x = "Recall (Sensitivity)",
      y = "Precision (Positive Predictive Value)"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10, color = "black"),
      panel.grid.major = element_line(color = "gray85", size = 0.4),
      panel.grid.minor = element_line(color = "gray92", size = 0.2),
      panel.border = element_rect(color = "black", fill = NA, size = 0.8),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1))
  
  return(p)
}

# 3. 混淆矩阵
plot_confusion_matrix <- function(data, dataset_name, threshold) {
  # 计算预测结果
  predictions <- ifelse(data$score >= threshold, 1, 0)
  
  # 创建混淆矩阵
  cm <- confusionMatrix(factor(predictions), factor(data$outcome), positive = "1")
  
  # 准备数据
  cm_data <- as.data.frame(cm$table)
  colnames(cm_data) <- c("Prediction", "Reference", "Freq")
  
  # 计算百分比
  cm_data$Percentage <- cm_data$Freq / sum(cm_data$Freq) * 100
  
  # 添加性能指标文本
  metrics_text <- sprintf(
    "Sensitivity: %.3f\nSpecificity: %.3f\nPPV: %.3f\nNPV: %.3f", 
    cm$byClass['Sensitivity'],
    cm$byClass['Specificity'],
    cm$byClass['Pos Pred Value'],
    cm$byClass['Neg Pred Value']
  )
  
  # 绘图
  p <- ggplot(cm_data, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white", size = 1) +
    geom_text(aes(label = sprintf("%d\n(%.1f%%)", Freq, Percentage)), 
              color = "white", size = 5, fontface = "bold") +
    scale_fill_gradient(low = "lightblue", high = accent_color, name = "Count") +
    labs(
      title = paste("Confusion Matrix -", dataset_name),
      subtitle = sprintf("Threshold: %.2f | Accuracy: %.3f", threshold, cm$overall['Accuracy']),
      x = "Actual",
      y = "Predicted"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10, color = "black"),
      legend.title = element_text(size = 10, face = "bold"),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.8)
    ) +
    coord_fixed() +
    # 添加性能指标注释
    annotate("text", x = 2.3, y = 1.5, label = metrics_text, 
             hjust = 0, vjust = 0.5, size = 3, color = "black")
  
  return(p)
}

# 生成所有图表
print("正在生成CatBoost模型评估图表（ROC, PRC, 混淆矩阵）...")

for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]
  
  if (nrow(data) == 0) {
    warning(paste("数据集", dataset_name, "为空，跳过"))
    next
  }
  
  print(paste("处理数据集:", dataset_name))
  
  # 1. ROC曲线
  print("  - 绘制ROC曲线...")
  roc_plot <- plot_roc_with_ci(data, dataset_name)
  ggsave(paste0(dataset_name, "_ROC曲线.pdf"), roc_plot, 
         width = 10, height = 8, device = "pdf", dpi = 300)
  
  # 2. PRC曲线
  print("  - 绘制PRC曲线...")
  prc_plot <- plot_prc_with_ci(data, dataset_name)
  ggsave(paste0(dataset_name, "_PRC曲线.pdf"), prc_plot, 
         width = 10, height = 8, device = "pdf", dpi = 300)
  
  # 3. 混淆矩阵
  print("  - 绘制混淆矩阵...")
  cm_plot <- plot_confusion_matrix(data, dataset_name, threshold)
  ggsave(paste0(dataset_name, "_混淆矩阵.pdf"), cm_plot, 
         width = 10, height = 8, device = "pdf", dpi = 300)
}

print("\n图表生成完成！生成的文件:")
for (dataset_name in names(datasets)) {
  print(paste("- ", dataset_name, "_ROC曲线.pdf"))
  print(paste("- ", dataset_name, "_PRC曲线.pdf"))
  print(paste("- ", dataset_name, "_混淆矩阵.pdf"))
}

# 显示基本统计信息
print("\n=== 数据集基本信息 ===")
for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]
  if (nrow(data) > 0) {
    cat(sprintf("\n%s:\n", dataset_name))
    cat(sprintf("  样本数: %d\n", nrow(data)))
    cat(sprintf("  正样本数: %d (%.1f%%)\n", sum(data$outcome), 100 * mean(data$outcome)))
    cat(sprintf("  评分范围: %.3f - %.3f\n", min(data$score), max(data$score)))
    cat(sprintf("  评分均值: %.3f ± %.3f\n", mean(data$score), sd(data$score)))
    
    # 计算主要性能指标
    predictions <- ifelse(data$score >= threshold, 1, 0)
    cm <- confusionMatrix(factor(predictions), factor(data$outcome), positive = "1")
    roc_obj <- roc(data$outcome, data$score, levels = c(0, 1), direction = "<")
    pr_obj <- pr.curve(scores.class0 = data$score[data$outcome == 1],
                       scores.class1 = data$score[data$outcome == 0],
                       curve = FALSE)
    
    cat(sprintf("  AUC: %.3f\n", as.numeric(auc(roc_obj))))
    cat(sprintf("  AUPRC: %.3f\n", pr_obj$auc.integral))
    cat(sprintf("  准确率: %.3f\n", cm$overall['Accuracy']))
    cat(sprintf("  敏感性: %.3f\n", cm$byClass['Sensitivity']))
    cat(sprintf("  特异性: %.3f\n", cm$byClass['Specificity']))
  }
}




