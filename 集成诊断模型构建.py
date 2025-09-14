import os
import pandas as pd
import numpy as np
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier, AdaBoostClassifier,  ExtraTreesClassifier
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.naive_bayes import GaussianNB
from sklearn.linear_model import LogisticRegression
from xgboost import XGBClassifier
from sklearn.metrics import (
    accuracy_score, precision_score, recall_score, f1_score,
    confusion_matrix, roc_curve, auc, precision_recall_curve
)
from sklearn.model_selection import GridSearchCV
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.backends.backend_pdf import PdfPages
import time
from self_paced_ensemble import SelfPacedEnsembleClassifier
from sklearn.neural_network import MLPClassifier
from sklearn.svm import SVC
import warnings
from imblearn.ensemble import EasyEnsembleClassifier
from catboost import CatBoostClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.linear_model import RidgeClassifier, SGDClassifier
warnings.filterwarnings("ignore", category=UserWarning, module="sklearn.utils.parallel")
import joblib

# 定义模型保存目录
MODEL_SAVE_DIR = './saved_models'
os.makedirs(MODEL_SAVE_DIR, exist_ok=True)


# 设置中文字体（如果需要）
plt.rcParams['font.sans-serif'] = ['SimHei', 'DejaVu Sans']
plt.rcParams['axes.unicode_minus'] = False

# Define output directories
PDF_OUTPUT_DIR = './model_analysis_reports'
SAMPLE_RESULTS_DIR = './sample_level_results'

# Create directories if they don't exist\

os.makedirs(PDF_OUTPUT_DIR, exist_ok=True)
os.makedirs(SAMPLE_RESULTS_DIR, exist_ok=True)


# ---------------------------------------------
# 1. 读取数据并做最基本的预处理
# ---------------------------------------------
print("Reading the training and testing data...")
df_train = pd.read_csv("./train.csv", index_col=0)
df_test = pd.read_csv("./test.csv", index_col=0)

X_train = df_train.drop(['outcome'], axis=1).fillna(0)
y_train = df_train['outcome']
X_test = df_test.drop(['outcome'], axis=1).fillna(0)
y_test = df_test['outcome']

# 保存样本名称和原标签
train_sample_names = X_train.index.tolist()
test_sample_names = X_test.index.tolist()
train_labels = y_train.values
test_labels = y_test.values

print("Data loading and preprocessing completed.")
print(f"Training set shape: {X_train.shape}, Testing set shape: {X_test.shape}")

# ---------------------------------------------
# 2. 定义各个基分类器和优化的超参数搜索范围
#    根据上一次运行结果调整 param_grids
# ---------------------------------------------
print("Defining base classifiers and hyperparameter optimization ranges...")

base_classifiers = {
    'RandomForest': RandomForestClassifier(random_state=42, n_jobs=-1),
    'ExtraTrees': ExtraTreesClassifier(random_state=42, n_jobs=-1),
    'XGBoost': XGBClassifier(random_state=42, eval_metric='logloss', n_jobs=-1),
    'AdaBoost': AdaBoostClassifier(random_state=42),
    'DecisionTree': DecisionTreeClassifier(random_state=42),
    'LogisticRegression': LogisticRegression(max_iter=2000, random_state=42, n_jobs=-1), # 增加max_iter以处理收敛警告
    'LDA': LinearDiscriminantAnalysis(),
    'NaiveBayes': GaussianNB(),
    'MLP': MLPClassifier(random_state=42, max_iter=1000),
    'CatBoost': CatBoostClassifier(random_seed=42, verbose=0),
    'Ridge': RidgeClassifier(random_state=42),
    'ElasticNet': SGDClassifier(random_state=42, penalty='elasticnet', max_iter=1000)
}

# 超参数网格
param_grids = {
    'RandomForest': {'n_estimators': [100, 200, 300], 'max_depth': [10, 15, 20]}, # 扩展n_estimators和max_depth
    'ExtraTrees': {'n_estimators': [100], 'max_depth': [10, 15, 20]}, # 扩展n_estimators和max_depth
    'XGBoost': {'n_estimators': [50, 100, 150], 'learning_rate': [0.05, 0.1, 0.15], 'max_depth': [3, 4, 5]}, # 细化和扩展
    'AdaBoost': {'n_estimators': [100, 150, 200], 'learning_rate': [0.5, 1, 1.5]}, # 扩展n_estimators和学习率
    'DecisionTree': {'max_depth': [3, 5, 7], 'min_samples_split': [2, 5]}, # 围绕最佳值细化max_depth，并微调min_samples_split
    'LogisticRegression': {'C': [1, 10, 100, 1000]}, # 扩展C值
    'LDA': {'solver': ['svd']},
    'NaiveBayes': {'var_smoothing': [1e-10, 1e-9, 1e-8, 1e-7, 1e-6]},
    'MLP': {'hidden_layer_sizes': [(50,), (100,), (150,)], 'activation': ['relu', 'tanh'], 'solver': ['adam', 'sgd']},
    'CatBoost': {'iterations': [100, 200], 'depth': [4, 6, 8], 'learning_rate': [0.01, 0.1]},
    'Ridge': {'alpha': [0.1, 1.0, 10.0]},
    'ElasticNet': {'alpha': [0.1, 0.5, 1], 'l1_ratio': [0.2, 0.5, 0.8]}
}

print("Base classifiers and hyperparameter grids defined.")

# 用于存储各模型指标
results = []
detailed_results = []
roc_data_train = {}
prc_data_train = {}
roc_data_test = {}
prc_data_test = {}
train_scores = {} # Stores probabilities for each model on training set
test_scores = {}  # Stores probabilities for each model on testing set
model_confusion_matrices = {}  # 存储混淆矩阵
threshold_analysis = {}  # 存储阈值分析数据


# ---------------------------------------------
# 3. 训练并评估每个模型
# ---------------------------------------------
def train_and_evaluate_model(name, base_clf, param_grid):
    """训练并评估单个模型的函数"""
    print(f"\n{'=' * 50}")
    print(f"Training model: {name} with EasyEnsemble...")
    start_time = time.time()

    try:
        # 超参数调优
        print(f"Performing hyperparameter tuning with 5-fold CV...")
        grid_search = GridSearchCV(
            base_clf, param_grid,
            cv=5,
            n_jobs=-1,
            scoring='average_precision',
            verbose=0
        )
        grid_search.fit(X_train, y_train)
        best_base_clf = grid_search.best_estimator_
        print(f"Best parameters for {name}: {grid_search.best_params_}")

        # 使用EasyEnsembleClassifier包装最佳基分类器
        print(f"Training EasyEnsemble with 30 estimators...")
        easy_ensemble = EasyEnsembleClassifier(
            estimator=best_base_clf,
            n_estimators=50,
            random_state=42,
            n_jobs=-1
        )

        # 训练EasyEnsemble模型
        easy_ensemble.fit(X_train, y_train)

        # 保存训练好的模型
        model_save_path = os.path.join(MODEL_SAVE_DIR, f'{name}_easy_ensemble_model.pkl')
        joblib.dump(easy_ensemble, model_save_path)
        print(f"Model {name} saved to {model_save_path}")

        # 预测
        print("Making predictions...")
        y_train_prob = easy_ensemble.predict_proba(X_train)[:, 1]
        y_test_prob = easy_ensemble.predict_proba(X_test)[:, 1]

        # 存储样本评分（带样本名和原标签）
        train_scores[name] = y_train_prob
        test_scores[name] = y_test_prob

        # 计算约登系数最优阈值
        fpr_train, tpr_train, thresholds_train = roc_curve(y_train, y_train_prob)
        youden_train = tpr_train - fpr_train
        optimal_threshold_train = thresholds_train[np.argmax(youden_train)]

        fpr_test, tpr_test, thresholds_test = roc_curve(y_test, y_test_prob)
        youden_test = tpr_test - fpr_test
        optimal_threshold_test = thresholds_test[np.argmax(youden_test)]

        # 分析不同阈值下的性能（训练集） - 这部分保留，因为它提供了模型行为的详细洞察
        thresholds_to_test = np.arange(0.1, 1.0, 0.05)
        threshold_metrics = []

        for thresh in thresholds_to_test:
            y_pred_thresh = (y_train_prob >= thresh).astype(int)
            f1 = f1_score(y_train, y_pred_thresh, zero_division=0) # Added zero_division to avoid warning
            prec = precision_score(y_train, y_pred_thresh, zero_division=0)
            rec = recall_score(y_train, y_pred_thresh)
            threshold_metrics.append({
                'threshold': thresh,
                'f1': f1,
                'precision': prec,
                'recall': rec
            })

        threshold_analysis[name] = threshold_metrics

        # 计算特定阈值的混淆矩阵 - 保留 0.5 和 约登系数最优阈值
        confusion_matrices = {}

        # 0.5阈值
        y_train_pred_05 = (y_train_prob >= 0.5).astype(int)
        y_test_pred_05 = (y_test_prob >= 0.5).astype(int)
        confusion_matrices['train_0.5'] = confusion_matrix(y_train, y_train_pred_05)
        confusion_matrices['test_0.5'] = confusion_matrix(y_test, y_test_pred_05)

        # 约登系数阈值（训练集优化）
        y_train_pred_youden = (y_train_prob >= optimal_threshold_train).astype(int)
        y_test_pred_youden = (y_test_prob >= optimal_threshold_train).astype(int)
        confusion_matrices['train_youden'] = confusion_matrix(y_train, y_train_pred_youden)
        confusion_matrices['test_youden'] = confusion_matrix(y_test, y_test_pred_youden)

        model_confusion_matrices[name] = {
            'matrices': confusion_matrices,
            'optimal_threshold': optimal_threshold_train
        }

        # 计算各种指标的函数
        def calculate_metrics(y_true, y_pred, y_prob):
            cm = confusion_matrix(y_true, y_pred)
            acc = accuracy_score(y_true, y_pred)
            prec = precision_score(y_true, y_pred, zero_division=0)
            rec = recall_score(y_true, y_pred)
            # 防止除数为零
            spec = cm[0, 0] / (cm[0, 0] + cm[0, 1]) if (cm[0, 0] + cm[0, 1]) > 0 else 0
            f1 = f1_score(y_true, y_pred, zero_division=0)

            fpr, tpr, _ = roc_curve(y_true, y_prob)
            auroc = auc(fpr, tpr)
            prec_curve, rec_curve, _ = precision_recall_curve(y_true, y_prob)
            auprc = auc(rec_curve, prec_curve)

            return {
                'cm': cm, 'acc': acc, 'prec': prec, 'rec': rec,
                'spec': spec, 'f1': f1, 'auroc': auroc, 'auprc': auprc,
                'fpr': fpr, 'tpr': tpr, 'rec_curve': rec_curve, 'prec_curve': prec_curve
            }

        # 训练集指标
        train_metrics_05 = calculate_metrics(y_train, y_train_pred_05, y_train_prob)
        train_metrics_youden = calculate_metrics(y_train, y_train_pred_youden, y_train_prob)

        # 测试集指标
        test_metrics_05 = calculate_metrics(y_test, y_test_pred_05, y_test_prob)
        test_metrics_youden = calculate_metrics(y_test, y_test_pred_youden, y_test_prob)

        # 保存曲线数据
        roc_data_train[name] = (train_metrics_05['fpr'], train_metrics_05['tpr'], train_metrics_05['auroc'])
        prc_data_train[name] = (
        train_metrics_05['rec_curve'], train_metrics_05['prec_curve'], train_metrics_05['auprc'])
        roc_data_test[name] = (test_metrics_05['fpr'], test_metrics_05['tpr'], test_metrics_05['auroc'])
        prc_data_test[name] = (test_metrics_05['rec_curve'], test_metrics_05['prec_curve'], test_metrics_05['auprc'])

        # 记录详细结果
        detailed_results.extend([
            {
                'Model': name, 'Dataset': 'Train', 'Threshold': '0.5',
                'Accuracy': train_metrics_05['acc'], 'Precision': train_metrics_05['prec'],
                'Recall': train_metrics_05['rec'], 'Specificity': train_metrics_05['spec'],
                'F1': train_metrics_05['f1'], 'AUROC': train_metrics_05['auroc'],
                'AUPRC': train_metrics_05['auprc']
            },
            {
                'Model': name, 'Dataset': 'Train', 'Threshold': f'Youden({optimal_threshold_train:.3f})',
                'Accuracy': train_metrics_youden['acc'], 'Precision': train_metrics_youden['prec'],
                'Recall': train_metrics_youden['rec'], 'Specificity': train_metrics_youden['spec'],
                'F1': train_metrics_youden['f1'], 'AUROC': train_metrics_youden['auroc'],
                'AUPRC': train_metrics_youden['auprc']
            },
            {
                'Model': name, 'Dataset': 'Test', 'Threshold': '0.5',
                'Accuracy': test_metrics_05['acc'], 'Precision': test_metrics_05['prec'],
                'Recall': test_metrics_05['rec'], 'Specificity': test_metrics_05['spec'],
                'F1': test_metrics_05['f1'], 'AUROC': test_metrics_05['auroc'],
                'AUPRC': test_metrics_05['auprc']
            },
            {
                'Model': name, 'Dataset': 'Test', 'Threshold': f'Youden({optimal_threshold_test:.3f})',
                'Accuracy': test_metrics_youden['acc'], 'Precision': test_metrics_youden['prec'],
                'Recall': test_metrics_youden['rec'], 'Specificity': test_metrics_youden['spec'],
                'F1': test_metrics_youden['f1'], 'AUROC': test_metrics_youden['auroc'],
                'AUPRC': test_metrics_youden['auprc']
            }
        ])

        # 简化版结果 (仍然基于0.5阈值)
        results.extend([
            {'Model': name, 'Dataset': 'Train', 'Recall': train_metrics_05['rec'],
             'F1': train_metrics_05['f1'], 'AUPRC': train_metrics_05['auprc'],
             'AUROC': train_metrics_05['auroc']},
            {'Model': name, 'Dataset': 'Test', 'Recall': test_metrics_05['rec'],
             'F1': test_metrics_05['f1'], 'AUPRC': test_metrics_05['auprc'],
             'AUROC': test_metrics_05['auroc']}
        ])

        elapsed_time = time.time() - start_time
        print(f"{name} training completed in {elapsed_time:.2f} seconds.")
        print(f"Training AUROC: {train_metrics_05['auroc']:.4f}, Testing AUROC: {test_metrics_05['auroc']:.4f}")
        print(f"Training AUPRC: {train_metrics_05['auprc']:.4f}, Testing AUPRC: {test_metrics_05['auprc']:.4f}")
        print(
            f"Optimal threshold (train): {optimal_threshold_train:.3f}, Optimal threshold (test): {optimal_threshold_test:.3f}")

        return True

    except Exception as e:
        print(f"Error training {name}: {str(e)}")
        print(f"Skipping {name} due to error.")
        return False


# 按计算复杂度从低到高的顺序训练模型
model_order = ['NaiveBayes', 'LDA', 'LogisticRegression', 'Ridge', 'ElasticNet',
               'DecisionTree', 'RandomForest', 'ExtraTrees', 'AdaBoost',
               'XGBoost', 'CatBoost', 'MLP']
successful_models = []

for name in model_order:
    if name in base_classifiers:
        success = train_and_evaluate_model(name, base_classifiers[name], param_grids[name])
        if success:
            successful_models.append(name)

print(f"\nSuccessfully trained models: {successful_models}")


# ---------------------------------------------
# 4. 生成PDF报告
# ---------------------------------------------
def create_pdf_report():
    """创建包含所有图表的PDF报告"""
    pdf_path = os.path.join(PDF_OUTPUT_DIR, 'model_evaluation_report.pdf')
    with PdfPages(pdf_path) as pdf:

        # 1. 混淆矩阵 - 0.5阈值
        if successful_models and model_confusion_matrices:
            n_models = len(successful_models)
            cols = min(3, n_models)
            rows = (n_models + cols - 1) // cols

            # Helper to manage axes for single/multiple plots
            def get_axes_for_subplots(fig, rows, cols, n_models):
                axes = fig.subplots(rows, cols)
                if n_models == 1:
                    return np.array([axes]) # Ensure it's always an array for consistent indexing
                elif rows == 1:
                    return np.array(axes)
                else:
                    return axes.ravel()

            # Training set confusion matrices (0.5 threshold)
            fig_train_05 = plt.figure(figsize=(18, 5 * rows))
            axes_train_05 = get_axes_for_subplots(fig_train_05, rows, cols, n_models)
            fig_train_05.suptitle('Confusion Matrices - Training Set (Threshold = 0.5)', fontsize=16)

            for i, name in enumerate(successful_models):
                if i < len(axes_train_05) and name in model_confusion_matrices:
                    cm = model_confusion_matrices[name]['matrices']['train_0.5']
                    sns.heatmap(cm, annot=True, fmt='d', cmap='Blues', ax=axes_train_05[i])
                    axes_train_05[i].set_title(f'{name}')
                    axes_train_05[i].set_xlabel('Predicted')
                    axes_train_05[i].set_ylabel('Actual')

            for j in range(n_models, len(axes_train_05)):
                axes_train_05[j].set_visible(False)

            plt.tight_layout(rect=[0, 0.03, 1, 0.95]) # Adjust layout to prevent suptitle overlap
            pdf.savefig(fig_train_05, bbox_inches='tight')
            plt.close(fig_train_05)

            # Test set confusion matrices (0.5 threshold)
            fig_test_05 = plt.figure(figsize=(18, 5 * rows))
            axes_test_05 = get_axes_for_subplots(fig_test_05, rows, cols, n_models)
            fig_test_05.suptitle('Confusion Matrices - Test Set (Threshold = 0.5)', fontsize=16)

            for i, name in enumerate(successful_models):
                if i < len(axes_test_05) and name in model_confusion_matrices:
                    cm = model_confusion_matrices[name]['matrices']['test_0.5']
                    sns.heatmap(cm, annot=True, fmt='d', cmap='Blues', ax=axes_test_05[i])
                    axes_test_05[i].set_title(f'{name}')
                    axes_test_05[i].set_xlabel('Predicted')
                    axes_test_05[i].set_ylabel('Actual')

            for j in range(n_models, len(axes_test_05)):
                axes_test_05[j].set_visible(False)

            plt.tight_layout(rect=[0, 0.03, 1, 0.95])
            pdf.savefig(fig_test_05, bbox_inches='tight')
            plt.close(fig_test_05)

            # Training set confusion matrices (Youden Index threshold)
            fig_train_youden = plt.figure(figsize=(18, 5 * rows))
            axes_train_youden = get_axes_for_subplots(fig_train_youden, rows, cols, n_models)
            fig_train_youden.suptitle('Confusion Matrices - Training Set (Youden Index Threshold)', fontsize=16)

            for i, name in enumerate(successful_models):
                if i < len(axes_train_youden) and name in model_confusion_matrices:
                    cm = model_confusion_matrices[name]['matrices']['train_youden']
                    threshold = model_confusion_matrices[name]['optimal_threshold']
                    sns.heatmap(cm, annot=True, fmt='d', cmap='Blues', ax=axes_train_youden[i])
                    axes_train_youden[i].set_title(f'{name} (Threshold={threshold:.3f})')
                    axes_train_youden[i].set_xlabel('Predicted')
                    axes_train_youden[i].set_ylabel('Actual')

            for j in range(n_models, len(axes_train_youden)):
                axes_train_youden[j].set_visible(False)

            plt.tight_layout(rect=[0, 0.03, 1, 0.95])
            pdf.savefig(fig_train_youden, bbox_inches='tight')
            plt.close(fig_train_youden)

            # Test set confusion matrices (Youden Index threshold)
            fig_test_youden = plt.figure(figsize=(18, 5 * rows))
            axes_test_youden = get_axes_for_subplots(fig_test_youden, rows, cols, n_models)
            fig_test_youden.suptitle('Confusion Matrices - Test Set (Youden Index Threshold)', fontsize=16)

            for i, name in enumerate(successful_models):
                if i < len(axes_test_youden) and name in model_confusion_matrices:
                    cm = model_confusion_matrices[name]['matrices']['test_youden']
                    threshold = model_confusion_matrices[name]['optimal_threshold']
                    sns.heatmap(cm, annot=True, fmt='d', cmap='Blues', ax=axes_test_youden[i])
                    axes_test_youden[i].set_title(f'{name} (Threshold={threshold:.3f})')
                    axes_test_youden[i].set_xlabel('Predicted')
                    axes_test_youden[i].set_ylabel('Actual')

            for j in range(n_models, len(axes_test_youden)):
                axes_test_youden[j].set_visible(False)

            plt.tight_layout(rect=[0, 0.03, 1, 0.95])
            pdf.savefig(fig_test_youden, bbox_inches='tight')
            plt.close(fig_test_youden)

        # 2. 阈值变化分析图 (保留，提供关键洞察)
        if threshold_analysis:
            # F1 Score变化
            fig, ax = plt.subplots(figsize=(12, 8))
            for name in successful_models:
                if name in threshold_analysis:
                    data = threshold_analysis[name]
                    thresholds = [d['threshold'] for d in data]
                    f1_scores = [d['f1'] for d in data]
                    ax.plot(thresholds, f1_scores, marker='o', label=f'{name}', linewidth=2)

            ax.set_xlabel('Threshold', fontsize=12)
            ax.set_ylabel('F1 Score', fontsize=12)
            ax.set_title('F1 Score vs Threshold (Training Set)', fontsize=14)
            ax.legend()
            ax.grid(True, alpha=0.3)
            plt.tight_layout()
            pdf.savefig(fig, bbox_inches='tight')
            plt.close()

            # Precision变化
            fig, ax = plt.subplots(figsize=(12, 8))
            for name in successful_models:
                if name in threshold_analysis:
                    data = threshold_analysis[name]
                    thresholds = [d['threshold'] for d in data]
                    precision_scores = [d['precision'] for d in data]
                    ax.plot(thresholds, precision_scores, marker='s', label=f'{name}', linewidth=2)

            ax.set_xlabel('Threshold', fontsize=12)
            ax.set_ylabel('Precision', fontsize=12)
            ax.set_title('Precision vs Threshold (Training Set)', fontsize=14)
            ax.legend()
            ax.grid(True, alpha=0.3)
            plt.tight_layout()
            pdf.savefig(fig, bbox_inches='tight')
            plt.close()

            # Recall变化
            fig, ax = plt.subplots(figsize=(12, 8))
            for name in successful_models:
                if name in threshold_analysis:
                    data = threshold_analysis[name]
                    thresholds = [d['threshold'] for d in data]
                    recall_scores = [d['recall'] for d in data]
                    ax.plot(thresholds, recall_scores, marker='^', label=f'{name}', linewidth=2)

            ax.set_xlabel('Threshold', fontsize=12)
            ax.set_ylabel('Recall', fontsize=12)
            ax.set_title('Recall vs Threshold (Training Set)', fontsize=14)
            ax.legend()
            ax.grid(True, alpha=0.3)
            plt.tight_layout()
            pdf.savefig(fig, bbox_inches='tight')
            plt.close()

        # 3. ROC曲线
        if roc_data_train and roc_data_test:
            # 训练集ROC
            fig, ax = plt.subplots(figsize=(10, 8))
            for name in successful_models:
                if name in roc_data_train:
                    fpr_t, tpr_t, auc_t = roc_data_train[name]
                    ax.plot(fpr_t, tpr_t, lw=2, label=f'{name} (AUC={auc_t:.3f})')
            ax.plot([0, 1], [0, 1], color='gray', linestyle='--', alpha=0.8)
            ax.set_xlim([0, 1])
            ax.set_ylim([0, 1.05])
            ax.set_xlabel('False Positive Rate', fontsize=12)
            ax.set_ylabel('True Positive Rate', fontsize=12)
            ax.set_title('ROC Curves - Training Dataset', fontsize=14)
            ax.legend(loc='lower right')
            ax.grid(True, alpha=0.3)
            plt.tight_layout()
            pdf.savefig(fig, bbox_inches='tight')
            plt.close()

            # 测试集ROC
            fig, ax = plt.subplots(figsize=(10, 8))
            for name in successful_models:
                if name in roc_data_test:
                    fpr_t, tpr_t, auc_t = roc_data_test[name]
                    ax.plot(fpr_t, tpr_t, lw=2, label=f'{name} (AUC={auc_t:.3f})')
            ax.plot([0, 1], [0, 1], color='gray', linestyle='--', alpha=0.8)
            ax.set_xlim([0, 1])
            ax.set_ylim([0, 1.05])
            ax.set_xlabel('False Positive Rate', fontsize=12)
            ax.set_ylabel('True Positive Rate', fontsize=12)
            ax.set_title('ROC Curves - Testing Dataset', fontsize=14)
            ax.legend(loc='lower right')
            ax.grid(True, alpha=0.3)
            plt.tight_layout()
            pdf.savefig(fig, bbox_inches='tight')
            plt.close()

        # 4. PRC曲线
        if prc_data_train and prc_data_test:
            # 训练集PRC
            fig, ax = plt.subplots(figsize=(10, 8))
            for name in successful_models:
                if name in prc_data_train:
                    rec_t, prec_t, auprc_t = prc_data_train[name]
                    ax.plot(rec_t, prec_t, lw=2, label=f'{name} (AUPRC={auprc_t:.3f})')
            ax.set_xlim([0, 1])
            ax.set_ylim([0, 1.05])
            ax.set_xlabel('Recall', fontsize=12)
            ax.set_ylabel('Precision', fontsize=12)
            ax.set_title('Precision-Recall Curves - Training Dataset', fontsize=14)
            ax.legend(loc='lower left')
            ax.grid(True, alpha=0.3)
            plt.tight_layout()
            pdf.savefig(fig, bbox_inches='tight')
            plt.close()

            # 测试集PRC
            fig, ax = plt.subplots(figsize=(10, 8))
            for name in successful_models:
                if name in prc_data_test:
                    rec_t, prec_t, auprc_t = prc_data_test[name]
                    ax.plot(rec_t, prec_t, lw=2, label=f'{name} (AUPRC={auprc_t:.3f})')
            ax.set_xlim([0, 1])
            ax.set_ylim([0, 1.05])
            ax.set_xlabel('Recall', fontsize=12)
            ax.set_ylabel('Precision', fontsize=12)
            ax.set_title('Precision-Recall Curves - Testing Dataset', fontsize=14)
            ax.legend(loc='lower left')
            ax.grid(True, alpha=0.3)
            plt.tight_layout()
            pdf.savefig(fig, bbox_inches='tight')
            plt.close()


if successful_models:
    print("Creating PDF report...")
    pdf_output_path = os.path.join(PDF_OUTPUT_DIR, 'model_evaluation_report.pdf')
    create_pdf_report()
    print(f"PDF report saved as '{pdf_output_path}'")

# ---------------------------------------------
# 5. 输出结果和保存数据
# ---------------------------------------------
if detailed_results:
    print(f"\n{'=' * 80}")
    print("DETAILED EVALUATION RESULTS")
    print(f"{'=' * 80}")

    df_detailed = pd.DataFrame(detailed_results)
    print("\n详细评估指标（包含阈值0.5和约登系数最优阈值）：")
    print(df_detailed.round(4))

    # 分别输出不同阈值的结果
    print(f"\n{'=' * 60}")
    print("阈值 = 0.5 的结果：")
    print(f"{'=' * 60}")
    df_05 = df_detailed[df_detailed['Threshold'] == '0.5']
    print(df_05.round(4))

    print(f"\n{'=' * 60}")
    print("约登系数最优阈值的结果：")
    print(f"{'=' * 60}")
    df_youden = df_detailed[df_detailed['Threshold'].str.contains('Youden')]
    print(df_youden.round(4))

    if results:
        df_results = pd.DataFrame(results)
        df_pivot = df_results.pivot(index='Model', columns='Dataset')
        df_pivot.columns = ['_'.join(col).strip() for col in df_pivot.columns.values]
        print(f"\n{'=' * 50}")
        print("简化版评估指标汇总（阈值=0.5）：")
        print(f"{'=' * 50}")
        print(df_pivot.round(4))

# ---------------------------------------------
# 6. 保存样本级别结果
# ---------------------------------------------
def save_sample_level_results(
    train_sample_names, train_labels, train_scores_dict,
    test_sample_names, test_labels, test_scores_dict,
    successful_models, output_dir
):
    print(f"\n{'=' * 50}")
    print("Generating sample-level results files...")
    os.makedirs(output_dir, exist_ok=True) # Ensure directory exists
    print(f"Results will be saved in: '{output_dir}'")

    # Train set results
    train_data = {'Sample_Name': train_sample_names, 'True_Label': train_labels}
    for model_name in successful_models:
        if model_name in train_scores_dict:
            train_data[f'{model_name}_Prob'] = train_scores_dict[model_name]
            # Calculate predictions at 0.5 threshold for the sample-level output
            train_data[f'{model_name}_Pred_0.5'] = (train_scores_dict[model_name] >= 0.5).astype(int)

    train_results_df = pd.DataFrame(train_data)
    train_excel_path = os.path.join(output_dir, '训练集样本详细结果.xlsx')
    train_results_df.to_excel(train_excel_path, index=False)
    print(f"训练集详细结果已保存至: {train_excel_path}")

    # Test set results
    test_data = {'Sample_Name': test_sample_names, 'True_Label': test_labels}
    for model_name in successful_models:
        if model_name in test_scores_dict:
            test_data[f'{model_name}_Prob'] = test_scores_dict[model_name]
            # Calculate predictions at 0.5 threshold for the sample-level output
            test_data[f'{model_name}_Pred_0.5'] = (test_scores_dict[model_name] >= 0.5).astype(int)

    test_results_df = pd.DataFrame(test_data)
    test_excel_path = os.path.join(output_dir, '测试集样本详细结果.xlsx')
    test_results_df.to_excel(test_excel_path, index=False)
    print(f"测试集详细结果已保存至: {test_excel_path}")

# Call the function to save sample-level results
if successful_models:
    save_sample_level_results(
        train_sample_names, train_labels, train_scores,
        test_sample_names, test_labels, test_scores,
        successful_models, SAMPLE_RESULTS_DIR
    )
