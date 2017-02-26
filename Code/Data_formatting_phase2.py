# for ease of use in interpreter: exec(open("/Users/Adam/Research/BA_Thesis/Code/Data_formatting_phase2.py").read())

import xlrd, numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit

desired_regression_var = ['ID code','Year','State' ,'County','Open in 1929', 'Open in 1931', 'Open in 1933','Open in 1935','Total value of products','bank_sus_norm', 'Post_1929' , 
'Total cost of materials, fuel, and electric cost(sum of f001, f002, f003)', 'Wage earners by months, total', 
'Branch or subsidiary of other firm', 'alt_iv', 'Balance year', 'Branch or subsidiary of other firm', 'Industry', 'FDIC_BANKS_SUS_', 'alt_iv_lag']

merged_df = pd.read_excel('/Users/Adam/Research/BA_Thesis/Data/preliminary_merge.xlsx')
merged_df_auto = pd.read_excel('/Users/Adam/Research/BA_Thesis/Data/preliminary_merge_auto.xlsx')
fdic_df_long = pd.read_excel('/Users/Adam/Research/BA_Thesis/Data/FDIC_check.xlsx')


merged_df['Industry'] = 'Cotton'
merged_df_auto['Industry'] = 'Auto'


regression_df = merged_df[desired_regression_var]
#regression_df_auto = merged_df_auto[desired_regression_var]

#regression_df2 = regression_df

#regression_df2 = regression_df2.append(regression_df_auto)

def balance_data2(df):
	new_rows = []
	for index, row in df.iterrows():
		years_needed = []
		year = row['Year']
		county = row['County']
		state = row['State']
		balance_year = row['Balance year']
		year_list = ['1929', '1931', '1933', '1935']
		open_29, open_31, open_33, open_35 = row['Open in 1929'], row['Open in 1931'], row['Open in 1933'], row['Open in 1935']
		ind_list = [open_29, open_31, open_33, open_35]

		
		#I totally forgot this line
		bank_sus_norm = row['bank_sus_norm']
		#alt_iv = row['alt_iv']

		if year == balance_year:
			#print('Yay')
			for k in range(len(ind_list)):
				if ind_list[k] == 0:
					#years_needed.append(year_list[k])
					years_needed.append(k)
			for j in years_needed:
				banks_sus = list(merged_df.loc[(merged_df['Year'] == int(year_list[j])) & (merged_df['County'] == row['County']) & (merged_df['State'] == row['State']), 'FDIC_BANKS_SUS_'])
				banks_1929 = list(merged_df.loc[(merged_df['Year'] == 1929) & (merged_df['County'] == county) & (merged_df['State'] == state), 'FDIC BANKS '])
				lag_year = int(year_list[j])-1
				banks_sus_lag = list(fdic_df_long.loc[(fdic_df_long['Year'] == lag_year) & (fdic_df_long['County'] == row['County']) & (fdic_df_long['State'] == row['State']), 'FDIC_BANKS_SUS_'])

				#print(banks_sus)
				
				if banks_sus == []:
					banks_sus = 0
				else:
					banks_sus = banks_sus[0]

				if banks_sus_lag == []:
					banks_sus_lag = 0
				else:
					banks_sus_lag = banks_sus_lag[0]

				if banks_1929 == []:
					banks_1929 = 1
				else:
					banks_1929 = banks_1929[0]
				alt_iv = float(banks_sus/banks_1929)
				banks_sus_avg  = float((banks_sus + banks_sus_lag)/2)
				alt_iv_lag = float(banks_sus_avg/banks_1929)

				#print('yay2?')
				#print(int(year_list[j]), int(balance_year))
				if int(year_list[j]) < int(balance_year):
					#print('cool')
					new_rows.append([row['ID code'] , year_list[j],row['State'] ,row['County'], open_29, open_31, open_33,open_35,'',bank_sus_norm, '', '', '', '', alt_iv, '', '', row['Industry'],
						banks_sus, alt_iv_lag])
					#print('cool')
				if int(year_list[j]) > int(balance_year):
					#print('sauce')
					new_rows.append([row['ID code'] , year_list[j], row['State'],row['County'], open_29, open_31, open_33,open_35,0,bank_sus_norm, 0, 0, 0, 0, alt_iv, 0, 0, row['Industry'],
						banks_sus, alt_iv_lag])
					#print('sauce')
	return new_rows	

new_rows = balance_data2(regression_df)
df1 = pd.DataFrame(new_rows, columns = desired_regression_var)
regression_df = regression_df.append(df1)

#new_rows2 = balance_data2(regression_df2)
#df2 = pd.DataFrame(new_rows2, columns = desired_regression_var)
#regression_df2 = regression_df2.append(df2)


excel_test_path = '/Users/Adam/Research/BA_Thesis/Data/regression_var.xlsx'
writer = pd.ExcelWriter(excel_test_path, engine='xlsxwriter')
regression_df.to_excel(writer, 'Sheet1')
writer.save()


'''
excel_test_path = '/Users/Adam/Research/BA_Thesis/Data/regression_var_auto.xlsx'
writer = pd.ExcelWriter(excel_test_path, engine='xlsxwriter')
regression_df2.to_excel(writer, 'Sheet1')
writer.save()
'''