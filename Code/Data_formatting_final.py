# for ease of use in interpreter: exec(open("/Users/Adam/Research/BA_Thesis/Code/Data_formatting_final.py").read())


import xlrd, numpy as np

ICSPR_state_codes = {41 : 'ALABAMA',
81 : 'ALASKA',
61 : 'ARIZONA',
42 : 'ARKANSAS',
71 : 'CALIFORNIA',
62 : 'COLORADO',
1 : 'CONNECTICUT',
11 : 'DELAWARE',
43 : 'FLORIDA',
44 : 'GEORGIA',
82 : 'HAWAII',
63 : 'IDAHO' ,
21 : 'ILLINOIS',
22 : 'INDIANA' ,
31 : 'IOWA'  ,
32 : 'KANSAS'  ,
51 : 'KENTUCKY' ,
45 : 'LOUISIANA' ,
2 : 'MAINE' ,
52 : 'MARYLAND' ,
3 : 'MASSACHUSETTS' ,
23 : 'MICHIGAN',
33 : 'MINNESOTA',
46 : 'MISSISSIPPI', 
34 : 'MISSOURI' ,
64 : 'MONTANA' ,
35 : 'NEBRASKA', 
65 : 'NEVADA' ,
4 : 'NEW HAMSHIRE' ,
12 : 'NEW JERSEY' ,
66 : 'NEW MEXICO' ,
13 : 'NEW YORK',
47 : 'NORTH CAROLINA', 
36 : 'NORTH DAKOTA', 
24 : 'OHIO' ,  
53 : 'OKLAHOMA', 
72 : 'OREGON',  
14 : 'PENNSYLVANIA', 
5 : "RHODE ISLAND", 
48 : "SOUTH CAROLINA",
37 : "SOUTH DAKOTA",
54 : "TENNESSEE",
49 : "TEXAS",   
67 : "UTAH",    
6 : "VERMONT",
40 : "VIRGINIA", 
73 : "WASHINGTON",
56 : "WEST VIRGINA",
25 : "WISCONSIN",
68 : "WYOMING", 
55 : 'DISTRICT OF COLUMBIA'}

import pandas as pd
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit

fdic_data_path = '/Users/Adam/Research/BA_Thesis/Data final/FDIC_first_conversion.dta'
debt_path = "/Users/Adam/Research/BA_Thesis/Data final/land_prices.dta"
fips_path = "/Users/Adam/Research/BA_Thesis/Data final/national_county.txt"
census_path = "/Users/Adam/Research/BA_Thesis/Data final/CoM_extract_for_Adam.dta"

fdic_df = pd.read_stata(fdic_data_path)
debt_df = pd.read_stata(debt_path)
census_df = pd.read_stata(census_path)

#no duplicates
fdic_df.drop_duplicates()

#######################################################################first, we need to rename the column headers for fdic data ###########################
reader = pd.io.stata.StataReader(fdic_data_path)
reader1 = pd.io.stata.StataReader(debt_path)
reader2 = pd.io.stata.StataReader(census_path)
label_dict = reader.variable_labels()
label_dict1 = reader1.variable_labels()
label_dict2=  reader2.variable_labels()

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
for column_name in list(debt_df):
	new_column_header.append(label_dict1[column_name])
debt_df.columns = new_column_header

new_column_header = []
for column_name in list(census_df):
	new_column_header.append(label_dict2[column_name])
census_df.columns = new_column_header

# We need to convert the fdic to long format

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


fdic_df_long = pd.wide_to_long(fdic_df, list(new_header_convert), i = 'id', j = 'Year')

#now I want to turn the indices, which are id and year, into their own columns
fdic_df_long['Year'] = fdic_df_long.index.get_level_values('Year') 
fdic_df_long['id'] = fdic_df_long.index.get_level_values('id') 


#get rid of old year column -- check robustness of new year columns later, chance you did this wrong
del fdic_df_long['YEAR']

#rename states
fdic_df_long['ICPR STATE CODE'] = fdic_df_long['ICPR STATE CODE'].apply(lambda x: ICSPR_state_codes[int(x)].lower())

#################################################### Now, we have to convert the fips code in debt_df to county and state columns#########################################################

fips_df = pd.read_csv(fips_path, sep=',', header = None)
fips_df.columns = ['state', 'state_code', 'county_code', 'county', 'class']




def fix_fips(row):
	state_code = str(row['state_code'])
	county_code = str(row['county_code'])
	length_code = len(county_code)
	left = 3- length_code
	county_code = '0'*left + str(county_code)
	
	fips_code = str(state_code) + county_code
	return fips_code

fips_df['FIPS code'] = fips_df.apply(fix_fips, axis = 1)
fips_df['FIPS code'] = fips_df['FIPS code'].astype(int)

def no_county_str(row):
	county = str(row['county'])
	county = county.split(' County')[0]
	return county

fips_df['county'] = fips_df.apply(no_county_str, axis = 1)

def fix_debt_fips_code(df):
	'''
	the debt data frame read in fips codes as floats, but cannot convert
	to integer due to null values
	'''
	fips_list = []
	for index, row in df.iterrows():
		fips_code = row['FIPS code']
		if index < 3335:
			fips_code = int(fips_code)
		else:
			fips_code = 0
		fips_list.append(fips_code)
	return fips_list

fips_code_fixed = fix_debt_fips_code(debt_df)
new_fips  = pd.DataFrame(fips_code_fixed)
debt_df['FIPS code'] = new_fips


merged_debt_df = pd.merge(fips_df, debt_df[['FIPS code', 'change in mortgage debt per acre, 1920-1910']], how='inner', on='FIPS code')
merged_debt_df=merged_debt_df.rename(columns = {'change in mortgage debt per acre, 1920-1910':'debt'})


excel_test_path = '/Users/Adam/Research/BA_Thesis/Data final/rajan_data.xlsx'
writer = pd.ExcelWriter(excel_test_path, engine='xlsxwriter')
merged_debt_df.to_excel(writer, 'Sheet1')
writer.save()







