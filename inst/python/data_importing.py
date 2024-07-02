
# '/home/josimms/Documents/CASSIA_Calibration/Raw_Data/hyytiala_weather/'

import os
import pandas as pd
from concurrent.futures import ThreadPoolExecutor, as_completed

def read_file(file_path):
    filename = os.path.splitext(os.path.basename(file_path))[0]
    try:
        df = pd.read_csv(file_path, sep=',')
        df['source_file'] = filename  # Add a column to identify the source file
        return df
    except Exception as e:
        print(f"Error reading file {filename}: {str(e)}")
        return None

def import_and_combine_txt_files(folder_path, file_pattern='.txt'):
    file_paths = [os.path.join(folder_path, f) for f in os.listdir(folder_path) 
                  if f.endswith(file_pattern)]
    
    dataframes = []
    with ThreadPoolExecutor() as executor:
        future_to_file = {executor.submit(read_file, file_path): file_path for file_path in file_paths}
        for future in as_completed(future_to_file):
            df = future.result()
            if df is not None:
                dataframes.append(df)
                print(f"Successfully imported {os.path.basename(future_to_file[future])}")
    
    # Combine all DataFrames into a single DataFrame
    combined_df = pd.concat(dataframes, ignore_index=True)
    return combined_df

# Example usage
folder_path = '/home/josimms/Documents/CASSIA_Calibration/Raw_Data/hyytiala_weather/'
file_pattern = '.txt'  # Modify if your files don't end with .txt

print(f"Current working directory: {os.getcwd()}")
print(f"Searching for files in: {folder_path}")

combined_data = import_and_combine_txt_files(folder_path, file_pattern)

print(f"\nTotal files imported: {combined_data['source_file'].nunique()}")
print(f"Combined DataFrame shape: {combined_data.shape}")

# Print summary of the combined DataFrame
print("\nSummary of combined DataFrame:")
print(combined_data.dtypes)
print("\nFirst few rows of combined DataFrame:")
print(combined_data.head())

# Optionally, save the combined DataFrame to a CSV file
combined_data.to_csv('combined_data.csv', index=False)
