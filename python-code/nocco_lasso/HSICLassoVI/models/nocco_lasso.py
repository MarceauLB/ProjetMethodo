# Implementation of NOCCO Lasso

#------------------------------------------
# Packages and functions
#------------------------------------------

import os
import sys
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import time
from sklearn.model_selection import KFold # Cross-validation
from api import Proposed_NOCCO_Lasso # File with the NOCCO Lasso method

from storage_csv_plots import save_matrix_to_csv, csv_to_plot # functions defined in the file 'storage_csv_plots.py'
from red import red_score # function defined in the file 'red.py'

# Code reproductibility
np.random.seed(123) 

#--------------------------------------------------------
# Model: NOCCO Lasso
#--------------------------------------------------------

#--------------------------------------------------------
# Optimization of the regularization parameter (lambda)
#--------------------------------------------------------

"""
def optimization_reg_param_regression(X, Y, d_star, k_fold=5):
    n = X.shape[0]
    lambda_seq = np.zeros(k_fold)

    nocco_lasso_model = Proposed_NOCCO_Lasso(lam=[np.inf, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1], eps=1e-3)
    nocco_lasso_model.input(X, Y)
    res_samQL = nocco_lasso_model.regression_multi(kernels=['Gaussian'])
    grid_lamb = nocco_lasso_model.get_lambda()  # Replace with actual method to retrieve lambdas

    kf = KFold(n_splits=k_fold, shuffle=True, random_state=2012)
    
    for k, (train_index, val_index) in enumerate(kf.split(X)):
        X_train, X_val = X[train_index], X[val_index]
        Y_train, Y_val = Y[train_index], Y[val_index]

        nocco_lasso_model.input(X_train, Y_train)
        nocco_lasso_model.regression_multi(kernels=['Gaussian'])

        selected_features = nocco_lasso_model.get_features(num_feat = d_star)
        # How to recover y_pred_val from the selected features???
        
        mse = np.mean((y_pred_val - Y_val) ** 2, axis=0)
        best_lambda_index = np.argmin(mse)
        lambda_seq[k] = grid_lamb[best_lambda_index]

    lambda_opt = np.mean(lambda_seq)
    return lambda_opt
"""
#--------------------------------------------------------
# Data 1: Additive model
#--------------------------------------------------------

def data1_nocco_lasso(data1_number_samples: int, data1_step_size: int):
    #------------------------------------
    ## Initialization
    #------------------------------------

    nocco_lasso_model = Proposed_NOCCO_Lasso(lam = [np.inf, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1], eps= 1e-3)

    start_time = time.time()

    data1_true_variables = [0, 1, 2, 3] # True variables (indices of X_1, X_2, X_3, X_4)
    data1_d_star = len(data1_true_variables)  # Number of true variables (4)
    # We want to compare it with the 4 most important variables found

    data1_number_tested_features = 256

    # Range of sample sizes to test (from 25 to the max number of samples, with a chosen step size)
    data1_sample_sizes = range(25, data1_number_samples + 1, data1_step_size)
    number_data1_sample_sizes = len(data1_sample_sizes)

    data1_avg_proportion_correct_feat = []

    data1_number_runs = 30 

    correctly_selected_feat_matrix = np.zeros((data1_number_runs, number_data1_sample_sizes))
    row_index_matrix = 0

    # Iterations
    for data1_n_train in data1_sample_sizes:
        print(f"\n--- Running {data1_number_runs} experiments with {data1_n_train} samples ---")
        
        proportions_for_current_size = []
        
        for run in range(data1_number_runs):
            #------------------------------------
            # Model definition for each run
            #------------------------------------

            np.random.seed(run)

            # Generation of samples for each feature (X1, X2, X3, ..., X_256)
            data1_X = np.random.randn(data1_number_samples, data1_number_tested_features)

            data1_X1 = data1_X[:, 0]  # X_1
            data1_X2 = data1_X[:, 1]  # X_2
            data1_X3 = data1_X[:, 2]  # X_3
            data1_X4 = data1_X[:, 3]  # X_4

            data1_E = np.random.normal(loc=0, scale=1, size = data1_number_samples)

            data1_Y = -2 * np.sin(2 * data1_X1) + data1_X2**2 + data1_X3 + np.exp(-data1_X4) + data1_E

            data1_X_train = data1_X[:data1_n_train, :]
            data1_Y_train = data1_Y[:data1_n_train]

            nocco_lasso_model.input(data1_X_train, data1_Y_train,featname = pd.DataFrame(data1_X_train).columns)
            nocco_lasso_model.regression_multi(kernels=['Gaussian'], B=25, M=1)

            selected_variables = nocco_lasso_model.get_features()[:data1_d_star]
            coefficients_selected_variables = nocco_lasso_model.get_index_score()[:data1_d_star]

            # Test
            print(selected_variables)

            # Correctly selected features
            correct_selections = set(selected_variables).intersection(data1_true_variables)
            proportion_correct = len(correct_selections) / data1_d_star
            
            proportions_for_current_size.append(proportion_correct)

            correctly_selected_feat_matrix[run, row_index_matrix] = proportion_correct

        # Average proportion for this sample size
        avg_proportion = np.mean(proportions_for_current_size)
        data1_avg_proportion_correct_feat.append(avg_proportion)
        row_index_matrix += 1

    end_time = time.time() 
    
    # Total computation time (in s)
    computation_time = end_time - start_time
    print(f"\nTotal computation time: {computation_time:.2f} seconds")

    return data1_sample_sizes, correctly_selected_feat_matrix, data1_avg_proportion_correct_feat

#------------------------------------------
# Plot the results
#------------------------------------------

def data1_nocco_lasso_plot(data1_number_samples: int, data1_step_size: int, file_name: str):
    plt.figure(figsize=(10, 6))
    sample_sizes, _, avg_proportion_correct_feat = data1_nocco_lasso(data1_number_samples, data1_step_size)
    plt.plot(sample_sizes, avg_proportion_correct_feat, marker='o', linestyle='-', color='b', 
             label="Proportion of Correctly Selected Features")    
    plt.xlabel("Number of training samples")
    plt.ylabel("Fraction of correctly selected features")
    plt.title("Data 1 (Additive model): Proportion of Correctly Selected Features depending on the size of training samples")
    plt.grid(True)
    plt.legend()

    # Save the plot
    plots_directory = os.path.join("Code", "plots")
    os.makedirs(plots_directory, exist_ok=True)
    file_path = os.path.join(plots_directory, file_name)
    plt.savefig(file_path)
    plt.show()


# Examples
# Example for data1
data1_number_samples = 250
data1_step_size = 25

# Sample sizes, matrix and average proportions
#sample_sizes, feat_matrix, avg_proportions = data1_nocco_lasso(data1_number_samples, data1_step_size)

# CSV file
#save_matrix_to_csv(feat_matrix, "data1_nocco_lasso_results_matrix.csv", sample_sizes)

#csv_to_plot("data1_nocco_lasso_results_matrix.csv", "data1_nocco_lasso_correctly_selected_features.pdf")
# data1_nocco_lasso_plot(data1_number_samples, data1_step_size, "data1_nocco_lasso_correctly_selected_features.pdf")


#--------------------------------------------------------
# Data 2: Non-additive model
#--------------------------------------------------------

#--------------------------------------------------------
# Model 1: NOCCO Lasso
#--------------------------------------------------------

def data2_nocco_lasso(data2_number_samples: int, data2_step_size: int):
    #------------------------------------
    ## Initialization
    #------------------------------------

    nocco_lasso_model = Proposed_NOCCO_Lasso(lam = [np.inf, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1], eps= 1e-3)

    start_time = time.time()

    data2_true_variables = [0, 1, 2] # True variables (indices of X_1, X_2, X_3)
    data2_d_star = len(data2_true_variables)  # Number of true variables (3)
    # We want to compare it with the 3 most important variables found

    data2_number_tested_features = 1000

    # Range of sample sizes to test (from 25 to the max number of samples, with a chosen step size)
    data2_sample_sizes = range(25, data2_number_samples + 1, data2_step_size)
    number_data2_sample_sizes = len(data2_sample_sizes)

    data2_avg_proportion_correct_feat = []

    data2_number_runs = 30

    correctly_selected_feat_matrix = np.zeros((data2_number_runs, number_data2_sample_sizes))
    row_index_matrix = 0

    # Iterations
    for data2_n_train in data2_sample_sizes:
        print(f"\n--- Running {data2_number_runs} experiments with {data2_n_train} samples ---")
        
        proportions_for_current_size = []
        
        for run in range(data2_number_runs):
            #------------------------------------
            # Model definition for each run
            #------------------------------------

            np.random.seed(run)

            # Generation of samples for each feature (X1, X2, X3, ..., X_256)
            data2_X = np.random.randn(data2_number_samples, data2_number_tested_features)

            data2_X1 = data2_X[:, 0]  # X_1
            data2_X2 = data2_X[:, 1]  # X_2
            data2_X3 = data2_X[:, 2]  # X_3

            data2_E = np.random.normal(loc=0, scale=1, size = data2_number_samples)

            data2_Y = data2_X1*np.exp(2*data2_X2) + data2_X3**2 + data2_E

            data2_X_train = data2_X[:data2_n_train, :]
            data2_Y_train = data2_Y[:data2_n_train]

            nocco_lasso_model.input(data2_X_train, data2_Y_train,featname = pd.DataFrame(data2_X_train).columns)
            nocco_lasso_model.regression_multi(kernels=['Gaussian'])

            selected_variables = nocco_lasso_model.get_features()[:data2_d_star]
            coefficients_selected_variables = nocco_lasso_model.get_index_score()[:data2_d_star]

            # Test
            print(selected_variables, coefficients_selected_variables)

            # Correctly selected features
            correct_selections = set(selected_variables).intersection(data2_true_variables)
            proportion_correct = len(correct_selections) / data2_d_star
            
            proportions_for_current_size.append(proportion_correct)

            correctly_selected_feat_matrix[run, row_index_matrix] = proportion_correct

        # Average proportion for this sample size
        avg_proportion = np.mean(proportions_for_current_size)
        data2_avg_proportion_correct_feat.append(avg_proportion)
        row_index_matrix += 1

    end_time = time.time() 
    
    # Total computation time (in s)
    computation_time = end_time - start_time
    print(f"\nTotal computation time: {computation_time:.2f} seconds")

    return data2_sample_sizes, correctly_selected_feat_matrix, data2_avg_proportion_correct_feat

#------------------------------------------
# Plot the results
#------------------------------------------

def data2_nocco_lasso_plot(data2_number_samples: int, data2_step_size: int, file_name: str):
    plt.figure(figsize=(10, 6))
    sample_sizes, _, avg_proportion_correct_feat = data2_nocco_lasso(data2_number_samples, data2_step_size)
    plt.plot(sample_sizes, avg_proportion_correct_feat, marker='o', linestyle='-', color='b', 
             label="Proportion of Correctly Selected Features")    
    plt.xlabel("Number of training samples")
    plt.ylabel("Fraction of correctly selected features")
    plt.title("Data 2 (Non-additive model): Proportion of Correctly Selected Features depending on the size of training samples")
    plt.grid(True)
    plt.legend()

    # Save the plot
    plots_directory = os.path.join("Code", "plots")
    os.makedirs(plots_directory, exist_ok=True)
    file_path = os.path.join(plots_directory, file_name)
    plt.savefig(file_path)
    plt.show()


# Examples
# Example for data2
data2_number_samples = 250
data2_step_size = 25

# Sample sizes, matrix and average proportions
sample_sizes, feat_matrix, avg_proportions = data2_nocco_lasso(data2_number_samples, data2_step_size)

# CSV file
save_matrix_to_csv(feat_matrix, "data2_nocco_lasso_results_matrix.csv", sample_sizes)

csv_to_plot("data2_nocco_lasso_results_matrix.csv", "data2_nocco_lasso_correctly_selected_features.pdf")
# data2_nocco_lasso_plot(data2_number_samples, data2_step_size, "data2_nocco_lasso_correctly_selected_features.pdf")

#--------------------------------------------------------
# Other settings
#--------------------------------------------------------

if __name__ == "__main__":
    print("Running file nocco_lasso.py")