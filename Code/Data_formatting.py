# for ease of use in interpreter: exec(open("/Users/Adam/Research/BA_Thesis/Code/Data_formatting.py").read())

import pandas as pd

fdic_data_path = '/Users/Adam/Research/BA_Thesis/Data/FDIC_first_conversion.dta'
cotton_data_path = '/Users/Adam/Research/BA_Thesis/Data/Cotton_ICSPR.dta'
fdic_df = pd.read_stata(fdic_data_path)
agri_df = pd.read_stata(cotton_data_path)

#first, we need to rename the column headers for fdic data
reader = pd.io.stata.StataReader(fdic_data_path)
reader1 = pd.io.stata.StataReader(cotton_data_path)
label_dict = reader.variable_labels()
label_dict1 = reader1.variable_labels()

new_column_header = []
#need to make sure that the column headers end in a year
for column_name in list(fdic_df):
	new_name = label_dict[column_name]
	for i in range(len(new_name)):
		if new_name[-1] == "S":
			new_name = new_name[:-1]
		elif new_name[-1] == " ":
			new_name = new_name[:-1]
		else:
			break
	new_column_header.append(new_name)
fdic_df.columns = new_column_header

new_column_header = []
for column_name in list(agri_df):
	new_column_header.append(label_dict1[column_name])
agri_df.columns = new_column_header

#the agricultural census is in long format -- one observation corresponds to one year. We need to convert the fdic to long format

fdic_df["id"] = fdic_df.index
new_header_convert = set([])
for column_header in list(fdic_df.columns):
	if "FDIC" in column_header:
		new = column_header.rsplit(" ", 1)
		new_header_convert.add(new[0] + " ")
print(new_header_convert)
fdic_df_long = pd.wide_to_long(fdic_df, list(new_header_convert), i = 'id', j = 'Year')


#fdic_df.unstack()


