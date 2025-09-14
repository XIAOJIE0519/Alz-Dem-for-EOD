import pandas as pd
import numpy as np
from lifelines import CoxPHFitter, KaplanMeierFitter
from lifelines.statistics import logrank_test
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor
from sklearn.svm import SVR
from sklearn.metrics import roc_auc_score, roc_curve
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
import seaborn as sns
import joblib
import os
import warnings

warnings.filterwarnings('ignore')

# Set matplotlib Chinese font
plt.rcParams['font.sans-serif'] = ['SimHei', 'DejaVu Sans']
plt.rcParams['axes.unicode_minus'] = False

# Create model save directory
os.makedirs("预后模型", exist_ok=True)

# Read data
train_data = pd.read_csv("./train预后.csv")
test_data = pd.read_csv("./test预后.csv")

# Data preprocessing
X_train = train_data.iloc[:, 3:]
y_train_event = train_data.iloc[:, 1]
y_train_time = train_data.iloc[:, 2]
X_test = test_data.iloc[:, 3:]
y_test_event = test_data.iloc[:, 1]
y_test_time = test_data.iloc[:, 2]

# Standardize features for SVM
scaler = StandardScaler()
X_train_scaled = scaler.fit_transform(X_train)
X_test_scaled = scaler.transform(X_test)

# Create Cox regression data
train_cox_data = pd.concat([X_train, y_train_event, y_train_time], axis=1)


# Function to find optimal threshold
def find_optimal_threshold_maxstat(scores, time, event, min_group_size=10):
    scores_sorted = np.sort(np.unique(scores))
    best_threshold, max_stat, best_p_value = None, 0, 1

    for threshold in scores_sorted:
        high_risk_mask = scores > threshold
        low_risk_mask = scores <= threshold

        if np.sum(high_risk_mask) < min_group_size or np.sum(low_risk_mask) < min_group_size:
            continue

        try:
            results = logrank_test(
                time[high_risk_mask], time[low_risk_mask],
                event_observed_A=event[high_risk_mask],
                event_observed_B=event[low_risk_mask]
            )
            if results.test_statistic > max_stat:
                max_stat = results.test_statistic
                best_threshold = threshold
                best_p_value = results.p_value
        except:
            continue

    return best_threshold, max_stat, best_p_value


# Function to calculate time-dependent ROC
def calculate_time_dependent_roc(risk_scores, time, event, target_times):
    roc_results = {}

    for t in target_times:
        # Create binary outcome: event occurred before time t
        y_binary = ((time <= t) & (event == 1)).astype(int)

        # Only include patients with follow-up >= t or event before t
        valid_mask = (time >= t) | (event == 1)

        if valid_mask.sum() > 0 and y_binary[valid_mask].sum() > 0:
            try:
                auc = roc_auc_score(y_binary[valid_mask], risk_scores[valid_mask])
                fpr, tpr, _ = roc_curve(y_binary[valid_mask], risk_scores[valid_mask])
                roc_results[t] = {'auc': auc, 'fpr': fpr, 'tpr': tpr}
            except:
                roc_results[t] = {'auc': 0.5, 'fpr': [0, 1], 'tpr': [0, 1]}
        else:
            roc_results[t] = {'auc': 0.5, 'fpr': [0, 1], 'tpr': [0, 1]}

    return roc_results


# Function to plot ROC curves
def plot_roc_curves(roc_results_train, roc_results_test, model_name, target_times):
    fig, axes = plt.subplots(2, 3, figsize=(15, 10))
    fig.suptitle(f'{model_name} - ROC Curves', fontsize=16)

    for i, t in enumerate(target_times):
        row = i // 3
        col = i % 3
        ax = axes[row, col]

        # Plot training ROC
        train_roc = roc_results_train[t]
        ax.plot(train_roc['fpr'], train_roc['tpr'],
                label=f'Train (AUC={train_roc["auc"]:.3f})', linewidth=2)

        # Plot test ROC
        test_roc = roc_results_test[t]
        ax.plot(test_roc['fpr'], test_roc['tpr'],
                label=f'Test (AUC={test_roc["auc"]:.3f})', linewidth=2)

        # Plot diagonal line
        ax.plot([0, 1], [0, 1], 'k--', alpha=0.5)

        ax.set_xlabel('False Positive Rate')
        ax.set_ylabel('True Positive Rate')
        ax.set_title(f'{t}-Year Survival')
        ax.legend()
        ax.grid(True, alpha=0.3)

    plt.tight_layout()
    plt.savefig(f'预后模型/{model_name}_ROC_curves.png', dpi=300, bbox_inches='tight')
    plt.show()


# Function to plot KM curves
def plot_km_curves(risk_scores, time, event, threshold, model_name, dataset_name):
    high_risk_mask = risk_scores > threshold
    low_risk_mask = risk_scores <= threshold

    kmf = KaplanMeierFitter()

    plt.figure(figsize=(10, 6))

    # High risk group
    if high_risk_mask.sum() > 0:
        kmf.fit(time[high_risk_mask], event[high_risk_mask], label=f'High Risk (n={high_risk_mask.sum()})')
        kmf.plot_survival_function(ax=plt.gca(), color='red')

    # Low risk group
    if low_risk_mask.sum() > 0:
        kmf.fit(time[low_risk_mask], event[low_risk_mask], label=f'Low Risk (n={low_risk_mask.sum()})')
        kmf.plot_survival_function(ax=plt.gca(), color='blue')

    # Perform log-rank test
    try:
        results = logrank_test(
            time[high_risk_mask], time[low_risk_mask],
            event_observed_A=event[high_risk_mask],
            event_observed_B=event[low_risk_mask]
        )
        p_value = results.p_value
    except:
        p_value = 1.0

    plt.title(f'{model_name} - Kaplan-Meier Curves ({dataset_name})\nLog-rank p-value: {p_value:.4f}')
    plt.xlabel('Time')
    plt.ylabel('Survival Probability')
    plt.legend()
    plt.grid(True, alpha=0.3)
    plt.tight_layout()
    plt.savefig(f'预后模型/{model_name}_{dataset_name}_KM_curve.png', dpi=300, bbox_inches='tight')
    plt.show()


# Train models
models = {}
risk_scores = {"train": {}, "test": {}}
optimal_thresholds = {}

print("Training models...")

# CoxPH
print("Training Cox regression...")
cox_model = CoxPHFitter()
try:
    cox_model.fit(train_cox_data, duration_col=train_data.columns[2], event_col=train_data.columns[1])
    models['CoxPH'] = cox_model
    risk_scores['train']['CoxPH'] = cox_model.predict_partial_hazard(X_train).values
    risk_scores['test']['CoxPH'] = cox_model.predict_partial_hazard(X_test).values
    joblib.dump(cox_model, "预后模型/cox_model.pkl")
    print("Cox regression completed")
except Exception as e:
    print(f"Cox regression failed: {e}")

# Random Forest
print("Training Random Forest...")
y_train_composite = y_train_event * y_train_time + (1 - y_train_event) * (y_train_time + 10)
rf_model = RandomForestRegressor(n_estimators=100, random_state=123)
rf_model.fit(X_train, y_train_composite)
models['RandomForest'] = rf_model
risk_scores['train']['RandomForest'] = -rf_model.predict(X_train)
risk_scores['test']['RandomForest'] = -rf_model.predict(X_test)
joblib.dump(rf_model, "预后模型/rf_model.pkl")
print("Random Forest completed")

# Gradient Boosting
print("Training Gradient Boosting...")
gb_model = GradientBoostingRegressor(n_estimators=100, random_state=123)
gb_model.fit(X_train, y_train_composite)
models['GradientBoosting'] = gb_model
risk_scores['train']['GradientBoosting'] = -gb_model.predict(X_train)
risk_scores['test']['GradientBoosting'] = -gb_model.predict(X_test)
joblib.dump(gb_model, "预后模型/gb_model.pkl")
print("Gradient Boosting completed")

# SVM Survival Model
print("Training SVM...")
svm_model = SVR(kernel='rbf', C=1.0, gamma='scale')
svm_model.fit(X_train_scaled, y_train_composite)
models['SVM'] = svm_model
risk_scores['train']['SVM'] = -svm_model.predict(X_train_scaled)
risk_scores['test']['SVM'] = -svm_model.predict(X_test_scaled)
joblib.dump({'model': svm_model, 'scaler': scaler}, "预后模型/svm_model.pkl")
print("SVM completed")

# Find optimal thresholds
print("\nFinding optimal thresholds...")
for model_name in risk_scores['train'].keys():
    scores = risk_scores['train'][model_name]
    threshold, max_stat, p_value = find_optimal_threshold_maxstat(
        scores, y_train_time.values, y_train_event.values
    )
    optimal_thresholds[model_name] = {
        'threshold': threshold,
        'max_statistic': max_stat,
        'p_value': p_value
    }
    print(f"{model_name} - Threshold: {threshold:.4f}, Max Statistic: {max_stat:.4f}, p-value: {p_value:.4f}")

# Calculate ROC curves for 3, 5, 7 years
target_times = [3, 5, 7]
print(f"\nCalculating ROC curves for {target_times} years...")

for model_name in risk_scores['train'].keys():
    print(f"Processing {model_name}...")

    # Calculate ROC for train and test sets
    train_roc = calculate_time_dependent_roc(
        risk_scores['train'][model_name], y_train_time.values, y_train_event.values, target_times
    )
    test_roc = calculate_time_dependent_roc(
        risk_scores['test'][model_name], y_test_time.values, y_test_event.values, target_times
    )

    # Plot ROC curves
    plot_roc_curves(train_roc, test_roc, model_name, target_times)

    # Plot KM curves
    threshold = optimal_thresholds[model_name]['threshold']
    plot_km_curves(risk_scores['train'][model_name], y_train_time.values, y_train_event.values,
                   threshold, model_name, 'Train')
    plot_km_curves(risk_scores['test'][model_name], y_test_time.values, y_test_event.values,
                   threshold, model_name, 'Test')

# Output risk scores
print("\n=== Risk Scores ===")
for dataset_name in ["train", "test"]:
    print(f"\n{dataset_name.capitalize()} Set:")
    scores_df = pd.DataFrame({
        model_name: risk_scores[dataset_name][model_name]
        for model_name in risk_scores[dataset_name]
    })
    scores_df.index.name = 'Sample_ID'
    print(scores_df)
    scores_df.to_csv(f"预后模型/{dataset_name}_risk_scores.csv")

# Save AUC results
print("\n=== AUC Results ===")
auc_results = []
for model_name in risk_scores['train'].keys():
    for t in target_times:
        train_roc = calculate_time_dependent_roc(
            risk_scores['train'][model_name], y_train_time.values, y_train_event.values, [t]
        )
        test_roc = calculate_time_dependent_roc(
            risk_scores['test'][model_name], y_test_time.values, y_test_event.values, [t]
        )

        auc_results.append({
            'Model': model_name,
            'Time': f'{t}-Year',
            'Train_AUC': train_roc[t]['auc'],
            'Test_AUC': test_roc[t]['auc']
        })

auc_df = pd.DataFrame(auc_results)
print(auc_df)
auc_df.to_csv("预后模型/AUC_results.csv", index=False)

print("\nModels saved to '预后模型' folder")
print("Risk scores saved as CSV files")
print("ROC curves and KM curves saved as PNG files")
print("AUC results saved as CSV file")






import pandas as pd
from lifelines import CoxPHFitter
import warnings
import numpy as np

# 忽略警告信息
warnings.filterwarnings('ignore')

try:
    # --- 1. 读取训练数据并训练模型 ---
    print("正在读取 'train预后.csv' 文件...")
    train_data = pd.read_csv("./train预后.csv")
    print("文件读取成功。")

    # 准备用于Cox模型的数据 (移除ID列)
    df_for_cox = train_data.drop(train_data.columns[0], axis=1)

    # 获取事件时间和事件状态的列名
    event_col_name = train_data.columns[1]
    duration_col_name = train_data.columns[2]

    # 初始化并训练Cox比例风险模型
    cph = CoxPHFitter()
    print("\n正在训练 Cox 比例风险模型...")
    cph.fit(df_for_cox, duration_col=duration_col_name, event_col=event_col_name)
    print("模型训练完成。")

    # --- 2. 导出模型系数为CSV文件 ---
    coefficients_df = cph.summary
    output_filename = "cox_model_coefficients.csv"
    coefficients_df.to_csv(output_filename)
    print(f"\n模型系数已成功导出到 '{output_filename}' 文件。")
    print("\n--- Cox 模型摘要 ---")
    print(coefficients_df)

    # --- 3. 【再次修正版】计算测试集中第一个样本的风险得分 ---
    print("\n--- 计算测试集第一个样本的风险得分 ---")

    # 读取测试数据
    print("正在读取 'test预后.csv' 文件...")
    test_data = pd.read_csv("./test预后.csv")
    print("文件读取成功。")

    # 提取第一个样本的特征数据
    first_sample_features = test_data.iloc[0, 3:]

    # 提取模型系数
    coefficients_series = cph.params_

    # 【关键修正】: 使用正确的内部属性 _norm_mean 来获取训练集的均值
    training_means = cph._norm_mean

    # 将测试样本的特征值减去训练集的均值，进行中心化处理
    centered_first_sample = first_sample_features - training_means

    # 使用中心化后的特征值计算对数偏风险 (log partial hazard)
    log_partial_hazard_manual = np.dot(centered_first_sample, coefficients_series)

    # 将手动计算的对数偏风险转换为偏风险，以便与内置函数结果比较
    partial_hazard_manual = np.exp(log_partial_hazard_manual)

    print("\n第一个样本的原始特征值:")
    print(first_sample_features.to_string())

    print(f"\n手动计算的【对数】偏风险 (log partial hazard) 为: {log_partial_hazard_manual:.6f}")
    print(f"手动计算的【偏风险】 (partial hazard = exp(score)) 为: {partial_hazard_manual:.6f}")

    # --- 验证 ---
    # 使用lifelines内置的预测函数来验证我们的手动计算结果
    X_test_first = test_data.iloc[[0], 3:]
    internal_prediction_score = cph.predict_partial_hazard(X_test_first).iloc[0]
    print(
        f"模型内置函数 predict_partial_hazard 的结果 (partial hazard) 为: {internal_prediction_score:.6f} (现在应与手动计算的偏风险一致)")

except FileNotFoundError as e:
    print(f"\n错误：找不到文件 {e.filename}。")
    print("请确认文件名是否正确，以及文件是否和Python脚本在同一个文件夹里。")
except Exception as e:
    print(f"\n处理过程中发生错误: {e}")

