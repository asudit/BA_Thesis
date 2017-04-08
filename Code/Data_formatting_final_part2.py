# for ease of use in interpreter: exec(open("/Users/Adam/Research/BA_Thesis/Code/Data_formatting_final_part2.py").read())

import xlrd, numpy as np, csv
from Levenshtein import distance
from difflib import get_close_matches

import pandas as pd
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit

merged_df = pd.read_excel('/Users/Adam/Research/BA_Thesis/Data final/merged_data.xlsx')
fdic_df_long = pd.read_excel('/Users/Adam/Research/BA_Thesis/Data/FDIC_check.xlsx')

CPI_excel_path = '/Users/Adam/Research/BA_Thesis/Data/CPI Unadjusted,annual,index units.xls'
xl = pd.ExcelFile(CPI_excel_path)
cpi_df = xl.parse("Sheet1")

#################################################### create indicator of open in 1929 year ##################################

def indicator_1929(row):
	firm = row['firm code']
	if list(merged_df.loc[(merged_df['firm code'] == firm) & (merged_df['Year'] == 1929), 'firm code']) != []:
		open_29 = 1
	else:
		open_29 = 0
	return open_29

merged_df['open_29'] = merged_df.apply(indicator_1929, axis = 1)

def indicator_1931(row):
	firm = row['firm code']
	if list(merged_df.loc[(merged_df['firm code'] == firm) & (merged_df['Year'] == 1931), 'firm code']) != []:
		open_31 = 1
	else:
		open_31 = 0
	return open_31

merged_df['open_31'] = merged_df.apply(indicator_1931, axis = 1)

def indicator_1933(row):
	firm = row['firm code']
	if list(merged_df.loc[(merged_df['firm code'] == firm) & (merged_df['Year'] == 1933), 'firm code']) != []:
		open_33 = 1
	else:
		open_33 = 0
	return open_33

merged_df['open_33'] = merged_df.apply(indicator_1933, axis = 1)

def indicator_1935(row):
	firm = row['firm code']
	if list(merged_df.loc[(merged_df['firm code'] == firm) & (merged_df['Year'] == 1935), 'firm code']) != []:
		open_35 = 1
	else:
		open_35 = 0
	return open_35

merged_df['open_35'] = merged_df.apply(indicator_1935, axis = 1)
print('done with indicator variables')

######################################### create county fixed characteristic and varying IV of bank distress

aggregations = {'FDIC_BANKS_SUS_': 'sum' }
sum_df = fdic_df_long[(fdic_df_long['Year'] == 1930) | (fdic_df_long['Year'] == 1931) | (fdic_df_long['Year'] == 1932) | (fdic_df_long['Year'] == 1933) | (fdic_df_long['Year'] == 1934) | (fdic_df_long['Year'] == 1935)].groupby(['County', 'State']).agg(aggregations)
sum_df['County'] = sum_df.index.get_level_values('County') 
sum_df['State'] = sum_df.index.get_level_values('State') 
sum_df.reset_index(drop=True)


def fixed_characteristic(row):
	county = row['County']
	state = row['State']
	
	banks_sus = list(sum_df.loc[(sum_df['County'] == county) & (sum_df['State'] == state), 'FDIC_BANKS_SUS_'])
	if banks_sus == []:
		banks_sus = [0]
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

merged_df['fixed_char'] = merged_df.apply(fixed_characteristic, axis = 1)
print('done with first iv')

def varying_iv(row):
	county = row['County']
	state = row['State']
	year = row['Year']
	banks_year = list(merged_df.loc[(merged_df['County'] == county) & (merged_df['Year'] == year) & (merged_df['State'] == state), 'FDIC_BANKS_SUS_'])
	if banks_year == []:
		banks_year = [0]
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
merged_df['varying_iv'] = merged_df.apply(varying_iv, axis = 1)
print('done with second iv')

###################################################################    make post_29 variable ####################

def create_post_29_var(row):
	year = row['Year']
	post = 0
	if year != 1929:
		post =1
	elif year == 1929:
		post = 0
	return post
merged_df['Post_1929'] = merged_df.apply(create_post_29_var, axis = 1)
print('done with post 29 var')

#############################################################

excel_test_path = '/Users/Adam/Research/BA_Thesis/Data final/regression_data.xlsx'
writer = pd.ExcelWriter(excel_test_path, engine='xlsxwriter')
merged_df.to_excel(writer, 'Sheet1')
writer.save()
















