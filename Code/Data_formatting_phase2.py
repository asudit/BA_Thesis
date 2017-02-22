# for ease of use in interpreter: exec(open("/Users/Adam/Research/BA_Thesis/Code/Data_formatting_phase2.py").read())

import xlrd, numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit

desired_regression_var = ['ID code','Year', 'County','Open in 1929', 'Open in 1931', 'Open in 1933','Open in 1935','Total value of products','bank_sus_norm', 'Post_1929' , 
'Total cost of materials, fuel, and electric cost(sum of f001, f002, f003)', 'Wage earners by months, total', 
'Branch or subsidiary of other firm', 'alt_iv', 'Balance year', 'Branch or subsidiary of other firm', 'Industry']

merged_df = pd.read_excel('/Users/Adam/Research/BA_Thesis/Data/preliminary_merge.xlsx')
merged_df_auto = pd.read_excel('/Users/Adam/Research/BA_Thesis/Data/preliminary_merge_auto.xlsx')


merged_df['Industry'] = 'Cotton'
merged_df_auto['Industry'] = 'Auto'


regression_df = merged_df[desired_regression_var]
regression_df_auto = merged_df_auto[desired_regression_var]

regression_df2 = regression_df

regression_df2 = regression_df2.append(regression_df_auto)

def balance_data2(df):
	new_rows = []
	for index, row in df.iterrows():
		years_needed = []
		year = row['Year']
		balance_year = row['Balance year']
		year_list = ['1929', '1931', '1933', '1935']
		open_29, open_31, open_33, open_35 = row['Open in 1929'], row['Open in 1931'], row['Open in 1933'], row['Open in 1935']
		ind_list = [open_29, open_31, open_33, open_35]
		#I totally forgot this line
		bank_sus_norm = row['bank_sus_norm']
		alt_iv = row['alt_iv']

		if year == balance_year:
			#print('Yay')
			for k in range(len(ind_list)):
				if ind_list[k] == 0:
					#years_needed.append(year_list[k])
					years_needed.append(k)
			for j in years_needed:
				#print('yay2?')
				#print(int(year_list[j]), int(balance_year))
				if int(year_list[j]) < int(balance_year):
					#print('cool')
					new_rows.append([row['ID code'] , year_list[j], row['County'], open_29, open_31, open_33,open_35,'',bank_sus_norm, '', '', '', '', alt_iv, '', '', row['Industry']])
					#print('cool')
				if int(year_list[j]) > int(balance_year):
					#print('sauce')
					new_rows.append([row['ID code'] , year_list[j], row['County'], open_29, open_31, open_33,open_35,0,bank_sus_norm, 0, 0, 0, 0, alt_iv, 0, 0, row['Industry']])
					#print('sauce')
	return new_rows	

new_rows = balance_data2(regression_df)
df1 = pd.DataFrame(new_rows, columns = desired_regression_var)
regression_df = regression_df.append(df1)

new_rows2 = balance_data2(regression_df2)
df2 = pd.DataFrame(new_rows2, columns = desired_regression_var)
regression_df2 = regression_df2.append(df2)

''''
excel_test_path = '/Users/Adam/Research/BA_Thesis/Data/regression_var.xlsx'
writer = pd.ExcelWriter(excel_test_path, engine='xlsxwriter')
regression_df.to_excel(writer, 'Sheet1')
writer.save()
'''
excel_test_path = '/Users/Adam/Research/BA_Thesis/Data/regression_var_auto.xlsx'
writer = pd.ExcelWriter(excel_test_path, engine='xlsxwriter')
regression_df2.to_excel(writer, 'Sheet1')
writer.save()