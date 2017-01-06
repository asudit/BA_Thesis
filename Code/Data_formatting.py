# for ease of use in interpreter: exec(open("/Users/Adam/Research/BA_Thesis/Code/Data_formatting.py").read())

import pandas as pd

fdic_data_path = '/Users/Adam/Research/BA_Thesis/Data/FDIC_first_conversion.dta'
fdic_df = pd.read_stata(fdic_data_path)

#first, we need to rename the column headers
reader = pd.io.stata.StataReader(fdic_data_path)
label_dict = reader.variable_labels()
new_column_header = []
for column_name in list(fdic_df):
	new_column_header.append(label_dict[column_name])
fdic_df.columns = new_column_header


