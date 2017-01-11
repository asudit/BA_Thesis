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

#time to take care of this SUS issue once and for all 
new_column_header_fixed = []
for header in new_column_header:
	if 'SUS' in header:
		
		header_parts  = header.split(" ")
		new_header = "_".join(header_parts)
		new_column_header_fixed.append(new_header)
	else:
		new_column_header_fixed.append(header)


fdic_df.columns = new_column_header_fixed
#print(new_column_header)

new_column_header = []
for column_name in list(agri_df):
	new_column_header.append(label_dict1[column_name])
agri_df.columns = new_column_header

#the agricultural census is in long format -- one observation corresponds to one year. We need to convert the fdic to long format

fdic_df["id"] = fdic_df.index
new_header_convert = set([])
for column_header in list(fdic_df.columns):
	if "FDIC" in column_header:
		#if " " not in column_header[-2:]:
		if 'SUS' in column_header:
			new = column_header.rsplit("_", 1)
			new_header_convert.add(new[0] + "_")
		else:
			new = column_header.rsplit(" ", 1)
			new_header_convert.add(new[0] + " ")
		
		
		#print(new)
print(list(new_header_convert))
print(list(fdic_df.columns))

fdic_df_long = pd.wide_to_long(fdic_df, list(new_header_convert), i = 'id', j = 'Year')

#now I want to turn the indices, which are id and year, into their own columns
fdic_df_long['Year'] = fdic_df_long.index.get_level_values('Year') 
fdic_df_long['id'] = fdic_df_long.index.get_level_values('id') 

#get rid of old year column -- check robustness of new year columns later, chance you did this wrong
del fdic_df_long['YEAR']


df = fdic_df_long
#fdic_df.unstack()


