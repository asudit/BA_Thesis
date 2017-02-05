# for ease of use in interpreter: exec(open("/Users/Adam/Research/BA_Thesis/Code/Data_formatting.py").read())
# general data notes: 
import xlrd, numpy as np
desired_regression_var = ['ID code','Year', 'County','Open in 1929', 'Open in 1931', 'Open in 1933','Open in 1935','Total value of products','bank_sus_norm', 'Post_1929' , 
'Total cost of materials, fuel, and electric cost(sum of f001, f002, f003)', 'Wage earners by months, total', 
'Branch or subsidiary of other firm', 'alt_iv', 'Balance year']

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

fdic_data_path = '/Users/Adam/Research/BA_Thesis/Data/FDIC_first_conversion.dta'
cotton_data_path = '/Users/Adam/Research/BA_Thesis/Data/Cotton_ICSPR.dta'
fdic_df = pd.read_stata(fdic_data_path)
agri_df = pd.read_stata(cotton_data_path)

#no duplicates
agri_df.drop_duplicates()
fdic_df.drop_duplicates()

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

fdic_df_long = pd.wide_to_long(fdic_df, list(new_header_convert), i = 'id', j = 'Year')

#now I want to turn the indices, which are id and year, into their own columns
fdic_df_long['Year'] = fdic_df_long.index.get_level_values('Year') 
fdic_df_long['id'] = fdic_df_long.index.get_level_values('id') 


#get rid of old year column -- check robustness of new year columns later, chance you did this wrong
del fdic_df_long['YEAR']

#rename states
fdic_df_long['ICPR STATE CODE'] = fdic_df_long['ICPR STATE CODE'].apply(lambda x: ICSPR_state_codes[int(x)].lower())



#df = fdic_df_long

########################################################################################### And so the merging begins#######################################################
#make sure agri and fdic data set have same type of county
fdic_df_long['COUNTY NAME'] = fdic_df_long['COUNTY NAME'].apply(lambda x: str(x).lower())
agri_df['County'] = agri_df['County'].apply(lambda x: str(x).lower())
agri_df['State'] = agri_df['State'].apply(lambda x: str(x).lower())

#rename FDIC county column header name; year headr names should already be uniform
fdic_df_long=fdic_df_long.rename(columns = {'COUNTY NAME':'County', 'ICPR STATE CODE': 'State'})

fdic_df_long.reset_index(drop=True)
fdic_df_long.set_index(['Year', 'State' ,'County'])
agri_df.set_index(['Year', 'State' ,'County'])
#I think inner worked better in terms of dropped observations
merged_df = pd.merge(agri_df, fdic_df_long, how='inner', on=['Year', 'State', 'County'])
#merged_df = pd.merge(agri_df, fdic_df_long, how='left', on=['Year', 'State', 'County'])

#this follows the method of Nanda et al.
aggregations = {'FDIC_BANKS_SUS_': 'sum' }
sum_df = fdic_df_long[(fdic_df_long['Year'] == 1930) | (fdic_df_long['Year'] == 1931) | (fdic_df_long['Year'] == 1932) | (fdic_df_long['Year'] == 1933)].groupby('County').agg(aggregations)
#sum_df = fdic_df_long[(fdic_df_long['Year'] == 1930) | (fdic_df_long['Year'] == 1931) | (fdic_df_long['Year'] == 1932) | (fdic_df_long['Year'] == 1933)].groupby(['State','County']).agg(aggregations)
sum_df['County'] = sum_df.index.get_level_values('County') 
#sum_df['County'] = sum_df.index.get_level_values('County') 
sum_df.reset_index(drop=True)

#take out inf values
#merged_df.replace([np.inf, -np.inf], np.nan).dropna(subset=list(merged_df.columns), how="all")

def normalize_suspensions(row):
	county = row['County']
	state = row['State']
	
	banks_sus = list(sum_df.loc[(sum_df['County'] == county), 'FDIC_BANKS_SUS_'])
	banks_sus = banks_sus[0]
	
	banks_1929_list = list(merged_df.loc[(merged_df['Year'] == 1929) & (merged_df['County'] == county) & (merged_df['State'] == state), 'FDIC BANKS '])

	#assert not isinstance(banks_1929_list, str)
	if isinstance(banks_1929_list, str):
		banks_1929 = banks_1929_list[0]
	elif banks_1929_list == []:
		banks_1929 = 1
	else:
		
		banks_1929 = banks_1929_list[0]
	
	normalized = float(banks_sus/banks_1929)

	return normalized

merged_df['bank_sus_norm'] = merged_df.apply(normalize_suspensions, axis = 1)

def alt_iv(row):
	county = row['County']
	state = row['State']
	year = row['Year']
	banks_year = list(merged_df.loc[(merged_df['County'] == county) & (merged_df['Year'] == year), 'FDIC_BANKS_SUS_'])
	banks_year = banks_year[0]

	banks_1929_list = list(merged_df.loc[(merged_df['Year'] == 1929) & (merged_df['County'] == county) & (merged_df['State'] == state), 'FDIC BANKS '])

	#assert not isinstance(banks_1929_list, str)
	if isinstance(banks_1929_list, str):
		banks_1929 = banks_1929_list[0]
	elif banks_1929_list == []:
		banks_1929 = 1
	else:
		banks_1929 = banks_1929_list[0]

	alt_iv = float(banks_year/banks_1929)
	#print(alt_iv)
	return alt_iv
merged_df['alt_iv'] = merged_df.apply(alt_iv, axis = 1)




def create_post_29_var(row):
	year = row['Year']
	post = 0
	if year != 1929:
		post =1
	elif year == 1929:
		post = 0
	return post
merged_df['Post_1929'] = merged_df.apply(create_post_29_var, axis = 1)

#merged_df.groupby('ID code').first()

# example check for merge: long[(long['County'] == 'kane') & (long['Year'] == 1929)]

#eliminate duplcate rows


# what's the damage

################################################preliminary regression stuff ######################################################
# adding plant size interaction -- maybe incorporated is a better way to go; try both
def plant_size_interaction(row):
	subsidiary = row['Branch or subsidiary of other firm']
	#print(subsidiary)
	if str(subsidiary) == 'yes':
		big_plant = 1
	else:
		big_plant = 0
	return big_plant
merged_df['Branch or subsidiary of other firm'] = merged_df.apply(plant_size_interaction, axis =1)

#adjust value added dependent variable via CPI, per year
CPI_excel_path = '/Users/Adam/Research/BA_Thesis/Data/CPI Unadjusted,annual,index units.xls'
xl = pd.ExcelFile(CPI_excel_path)
cpi_df = xl.parse("Sheet1")
#cpi_df['Test'] = cpi_df['CPI'].apply(lambda x: x * 2)


def normalize_valued_added(row):
	year = row['Year']
	cpi = cpi_df.loc[(cpi_df['Year'] == year), 'CPI']
	new_val_added = float(row['Total value of products'])/float(cpi)
	#print(new_val_added)
	#print(cpi, new_val_added)
	#new_cost_materials = row['Cost of all materials and raw stock actually used']/cpi
	return new_val_added
	
def normalize_materials_cost(row):
	year = row['Year']
	cpi = cpi_df.loc[(cpi_df['Year'] == year), 'CPI']
	new_cost_materials = float(row['Total cost of materials, fuel, and electric cost(sum of f001, f002, f003)'])/float(cpi)
	return new_cost_materials
merged_df['Total value of products'] = merged_df.apply(normalize_valued_added, axis =1)
merged_df['Total cost of materials, fuel, and electric cost(sum of f001, f002, f003)'] = merged_df.apply(normalize_materials_cost, axis =1)

########################## fill in blank rows with 0 ###############

#merged_df.groupby('ID code')
def balance_data(row):
	open_29, open_31, open_33, open_35 = row['Open in 1929'], row['Open in 1931'], row['Open in 1933'], row['Open in 1935']
	ind_list = [open_29, open_31, open_33, open_35]
	year_list = ['1929', '1931', '1933', '1935']
	first_year = '0'
	for i in range(len(ind_list)):
		if ind_list[i] == 1:
			first_year = year_list[i]
			break
	return first_year
			
merged_df['Balance year'] = merged_df.apply(balance_data, axis =1)






	
#now just create a new df with only variables you want for regression
regression_df = merged_df[desired_regression_var]

def balance_data2(df):
	new_rows = []
	for index, row in df.iterrows():
		years_needed = []
		year = row['Year']
		balance_year = row['Balance year']
		year_list = ['1929', '1931', '1933', '1935']
		open_29, open_31, open_33, open_35 = row['Open in 1929'], row['Open in 1931'], row['Open in 1933'], row['Open in 1935']
		ind_list = [open_29, open_31, open_33, open_35]

		if year == balance_year:
			print('Yay')
			for k in range(len(ind_list)):
				if ind_list[k] == 0:
					years_needed.append(year_list[k])
			for j in years_needed:
				new_rows.append([row['ID code'] , j, row['County'], open_29, open_31, open_33,open_35,0,0, 0, 0, 0, 0, 0, 0])
	return new_rows

#new_rows = balance_data2(regression_df)
#df1 = pd.DataFrame(new_rows, columns = desired_regression_var)
#regression_df.append(df1)

'''
excel_test_path = '/Users/Adam/Research/BA_Thesis/Data/preliminary_merge.xlsx'
writer = pd.ExcelWriter(excel_test_path, engine='xlsxwriter')
merged_df.to_excel(writer, 'Sheet1')
writer.save()
'''

'''
excel_test_path = '/Users/Adam/Research/BA_Thesis/Data/regression_var.xlsx'
writer = pd.ExcelWriter(excel_test_path, engine='xlsxwriter')
regression_df.to_excel(writer, 'Sheet1')
writer.save()
'''
#first-differencing
'''
merged_df_diff = merged_df
merged_df_diff.reset_index(drop=True)
#merged_df_diff.astype('float64')
merged_df_diff.set_index(['Year', 'ID code']).diff(-1)

excel_test_path = '/Users/Adam/Research/BA_Thesis/Data/preliminary_merge_differences.xlsx'
writer = pd.ExcelWriter(excel_test_path, engine='xlsxwriter')
merged_df_diff.to_excel(writer, 'Sheet1')
writer.save()
'''
######################### write to excel #####





####################### Some data visualization
#fdic_df_long.plot(x='Year', y = 'County')
'''
#fdic_df_long[fdic_df_long['Year'] == 1931 ].plot(x='County', y = 'FDIC_BANKS_SUS_')
merged_df['Total Suspensions'] = merged_df['FDIC_ST_BANKS_SUS_'] + merged_df['FDIC_BANKS_SUS_'] + merged_df['FDIC_NTL_BANKS_SUS_']
#merged_df[merged_df['Year'] == 1933].plot.scatter(x = 'FDIC_BANKS_SUS_', y='Total value of products') # this graph is interesting
merged_df[merged_df['Year'] == 1929].plot.scatter(x = 'FDIC_BANKS_SUS_', y='Total value of products')
plt.title('1929', color='black')
merged_df[merged_df['Year'] == 1931].plot.scatter(x = 'FDIC_BANKS_SUS_', y='Total value of products') # this graph is interesting
plt.title('1931', color='black')
merged_df[merged_df['Year'] == 1933].plot.scatter(x = 'FDIC_BANKS_SUS_', y='Total value of products')
plt.title('1933', color='black')
#merged_df.plot.scatter(x = 'Year', y='Total value of products')
plt.show()
'''


'''
	df_dict = {}
	for index, row in df.iterrows():
		year = row['Year']
		#open_29, open_31, open_33, open_35 = row['Open in 1929'], row['Open in 1931'], row['Open in 1933'], row['Open in 1935']
		id_code = row['ID code'] 
		if id_code not in df_dict:
			df_dict[id_code] = {}
		df_dict[id_code][year] = row
	keys = df_dict.keys()
	for id_code in keys:
		print(id_code)
		year_list = ['1929', '1931', '1933', '1935']
		for year in df_dict[id_code].keys():
			old_row = df_dict[id_code][year]
			if year in year_list:
				year_list.remove(year)
		for year in year_list:
			
			new_row = []
			new_row.extend((id_code, year, old_row['County']))
			for var in desired_regression_var[3:]:
				new_row.append(0)
			regression_df.loc[-1] = new_row
			


			#row1 = df_dict[id_code][year]
			#row1
	#return df_dict
	'''

'''
	for k in range(len(year_list)):
		if year_list[k] == year:
			index = k
	open_29, open_31, open_33, open_35 = row['Open in 1929'], row['Open in 1931'], row['Open in 1933'], row['Open in 1935']
	ind_list = [open_29, open_31, open_33, open_35]
	proceed = False
	
	if year_list[index] == year and ind_list[index] == 1:
		proceed = True

	#if year == '1929' and open_29 == 1:
		#proceed = True
	#elif year == '1931' and open_31 == 1:
		#proceed = True
	#elif year == '1933' and open_33 == 1:
		#proceed = True
	#elif year == '1935' and open_35 == 1:
		#proceed = True
	if proceed:
		proceed2 = False
		for i in range(len(ind_list)):
	'''











