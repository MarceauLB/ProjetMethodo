# Storage in CSV files and plots based on CSV files

#------------------------------------------
# Packages
#------------------------------------------

import os
import numpy as np
import matplotlib.pyplot as plt
from pyHSICLasso import HSICLasso
from ITMO_FS.wrappers.deterministic import qpfs_wrapper # Quadratic Programming Feature Selection
import pandas as pd
import time
from qpsolvers import available_solvers
from sklearn.model_selection import KFold # Cross-validation

# Code reproductibility
np.random.seed(123)

#--------------------------------------------------------
# Storage of results
#--------------------------------------------------------

def save_matrix_to_csv(matrix: np.ndarray, file_name: str, sample_sizes: range):
    """Function to save the matrix as a CSV file in the csv_results folder."""
    
    # Directory for the CSV file
    directory = os.path.join("Code", "csv_results")  # Saved in 'Code/csv_results'

    # Directory created if it doesn't exist
    os.makedirs(directory, exist_ok=True)

    full_file_path = os.path.join(directory, file_name)

    df = pd.DataFrame(matrix, columns=[f'Sample_Size_{size}' for size in sample_sizes])

    # Save to CSV
    df.to_csv(full_file_path, index=False)
    
    print(f"Matrix saved to {full_file_path}")

#--------------------------------------------------------
# Plots based on CSV files
#--------------------------------------------------------

def csv_to_plot(csv_file_name: str, plot_file_name: str):
    csv_directory = os.path.join("Code", "csv_results")
    csv_full_file_path = os.path.join(csv_directory, csv_file_name)

    if os.path.exists(csv_full_file_path):
        rate_selected_variables_df = pd.read_csv(csv_full_file_path) # CSV into dataframe
    else:
        return None  # CSV file not found
    
    nb_columns = len(rate_selected_variables_df.columns)
    
    # Récupération des nombres d'échantillons testés à partir des noms de colonne du CSV (Sample_Size_[nb])
    sample_sizes = [col.split('_')[2] for col in rate_selected_variables_df.columns]
    avg_proportion_correct_feat = []

    for i in range(nb_columns):
        avg_proportion_correct_feat.append(np.mean(rate_selected_variables_df.iloc[:, i]))
    
    plt.figure(figsize=(10, 6))
    plt.plot(sample_sizes, avg_proportion_correct_feat, marker='o', linestyle='-', color='b', 
             label="Proportion of Correctly Selected Features")    
    plt.xlabel("Number of training samples")
    plt.ylabel("Fraction of correctly selected features")
    plt.title("Data 1 (Additive model): Proportion of Correctly Selected Features depending on the size of training samples")
    plt.grid(True)
    plt.legend()
    plot_directory = os.path.join("Code", "plots")
    os.makedirs(plot_directory, exist_ok=True)
    plot_file_path = os.path.join(plot_directory, plot_file_name)
    plt.savefig(plot_file_path) # Figure saved
    plt.show()

#--------------------------------------------------------
# Other settings
#--------------------------------------------------------

if __name__ == "__main__":
    print("Running file storage_csv_plots_lasso.py")