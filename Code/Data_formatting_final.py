# for ease of use in interpreter: exec(open("/Users/Adam/Research/BA_Thesis/Code/Data_formatting_final.py").read())


import xlrd, numpy as np, csv
from Levenshtein import distance
from difflib import get_close_matches

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
4 : 'NEW HAMPSHIRE' ,
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
56 : "WEST VIRGINIA",
25 : "WISCONSIN",
68 : "WYOMING", 
55 : 'DISTRICT OF COLUMBIA'}

states = {
        'AK': 'Alaska',
        'AL': 'Alabama',
        'AR': 'Arkansas',
        'AS': 'American Samoa',
        'AZ': 'Arizona',
        'CA': 'California',
        'CO': 'Colorado',
        'CT': 'Connecticut',
        'DC': 'District of Columbia',
        'DE': 'Delaware',
        'FL': 'Florida',
        'GA': 'Georgia',
        'GU': 'Guam',
        'HI': 'Hawaii',
        'IA': 'Iowa',
        'ID': 'Idaho',
        'IL': 'Illinois',
        'IN': 'Indiana',
        'KS': 'Kansas',
        'KY': 'Kentucky',
        'LA': 'Louisiana',
        'MA': 'Massachusetts',
        'MD': 'Maryland',
        'ME': 'Maine',
        'MI': 'Michigan',
        'MN': 'Minnesota',
        'MO': 'Missouri',
        'MP': 'Northern Mariana Islands',
        'MS': 'Mississippi',
        'MT': 'Montana',
        'NA': 'National',
        'NC': 'North Carolina',
        'ND': 'North Dakota',
        'NE': 'Nebraska',
        'NH': 'New Hampshire',
        'NJ': 'New Jersey',
        'NM': 'New Mexico',
        'NV': 'Nevada',
        'NY': 'New York',
        'OH': 'Ohio',
        'OK': 'Oklahoma',
        'OR': 'Oregon',
        'PA': 'Pennsylvania',
        'PR': 'Puerto Rico',
        'RI': 'Rhode Island',
        'SC': 'South Carolina',
        'SD': 'South Dakota',
        'TN': 'Tennessee',
        'TX': 'Texas',
        'UT': 'Utah',
        'VA': 'Virginia',
        'VI': 'Virgin Islands',
        'VT': 'Vermont',
        'WA': 'Washington',
        'WI': 'Wisconsin',
        'WV': 'West Virginia',
        'WY': 'Wyoming'
}

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
fdic_df_long=fdic_df_long.rename(columns = {'COUNTY NAME':'County', 'ICPR STATE CODE': 'State'})

#######west virginia spelled wrong in fdic_df_long#########
#for index, row in fdic_df_long.iterrows():
	#if row['State'] == 'west virgina':
		#print('gotcha!')
		#fdic_df_long.ix[index, 'State'] = 'west virginia'

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

'''
excel_test_path = '/Users/Adam/Research/BA_Thesis/Data final/rajan_data.xlsx'
writer = pd.ExcelWriter(excel_test_path, engine='xlsxwriter')
merged_debt_df.to_excel(writer, 'Sheet1')
writer.save()
'''
##################################################### fix county names in ziebarth data #################################################

fips_df['county'] = fips_df['county'].apply(lambda x: str(x).lower())
merged_debt_df['county'] = merged_debt_df['county'].apply(lambda x: str(x).lower())

#us_counties = list(fips_df['county'].astype(str))
def make_county_state_dict(df):
	state_county_dict = {}
	for index, row in df.iterrows():
		if row['State'] not in state_county_dict:
			state_county_dict[row['State'].lower()] = []
		state_county_dict[row['State']].append(row['County'].lower())
	return state_county_dict
state_county_dict = make_county_state_dict(fdic_df_long)

#us_counties = list(fdic_df_long['County'].apply(lambda x: str(x).lower()))


county_cleaning_path = '/Users/Adam/Research/BA_Thesis/Data/Rajan County Cleaning.xlsx'
#county_cleaning_df = pandas.read_excel(county_cleaning_path, sheet = 'Sheet1')

#county_cleaning_df['county'] = county_cleaning_df['county'].apply(lambda x: str(x).lower())
'''
workbook = xlrd.open_workbook(county_cleaning_path)
sheet = workbook.sheet_by_name('Sheet1')
row_list = []
for row_num in list(range(sheet.nrows)):
	if row_num == 0:
		pass
	else:
		row = sheet.row_values(row_num)
		state  = states[row[0]].lower()
		if state == 'south carolina ':
			state = 'south carolina'
		#if state not in ['district of columbia','', 'wyoming'] and str(row[2]).lower() not in state_county_dict[state]:
		if state not in ['alaska', 'district of columbia', 'hawaii', 'wyoming'] and str(row[2]).lower() not in state_county_dict[state]:
			print('Ouch!')
			missing = 1
			bad_county = str(row[2]).lower()
			#print(bad_county, us_counties)
			#print(get_close_matches(bad_county, us_counties, n=1))
			#correction = get_close_matches(bad_county, us_counties, len(us_counties), 0)
			correction = []
			for correct_county in state_county_dict[state]:
				if distance(bad_county, correct_county) <= 2:
					#if row[0] in states_dict:
						#state_abbrev = states_dict[row[0]]
					#if correct_county in state_county_dict[state_abbrev]:
					correction.append(correct_county)
			correction = [missing] + correction
		else:
			missing = 0
			correction = [missing]
		new_row = row + correction
		row_list.append(new_row)


with open('/Users/Adam/Research/BA_Thesis/Data/Rajan county cleaning.csv', "w") as f:
	writer = csv.writer(f)
	for row in row_list:
		writer.writerow(row)
	
#difflib.get_close_matches(word, possibilities, n=3
'''


##############################################use county cleaning csv's to update the dataframe #################
Rajan_clean_csv_path = '/Users/Adam/Research/BA_Thesis/Data final/Rajan county cleaning -- Don\'t change.csv'
ziebarth_clean_csv_path = '/Users/Adam/Research/BA_Thesis/Data final/County cleaning-Don\'t change.csv'

with open(Rajan_clean_csv_path, "r") as f:
	rows = csv.reader(f)
	for row in list(rows)[1:]:
		new_county = row[5].lower().rstrip()
		if new_county != '':
			FIPS_Code = row[3]
			#print(new_county, FIPS_Code)
			merged_debt_df.loc[merged_debt_df['FIPS code'] == int(FIPS_Code), 'county'] = new_county

with open(ziebarth_clean_csv_path, "r", encoding='mac_roman') as f:
	rows = csv.reader(f)
	for row in list(rows)[1:]:
		new_county = row[7].lower().rstrip()
		if new_county != '':
			firm_Code = row[3]
			Year = row[5]
			census_df.loc[(census_df['Establishment ID composed of IndNum and random sequence of 10 letters'] == firm_Code) & (census_df['Year'] == Year), 'County'] = new_county


####################################### standardize headers and prepare for merge ######################################################
merged_debt_df=merged_debt_df.rename(columns = {'state':'State', 'county':'County', 'year': 'Year'})
census_df=census_df.rename(columns = {'Establishment ID composed of IndNum and random sequence of 10 letters': 'firm code'})

#convert debt df states to full states
merged_debt_df['State'] = merged_debt_df['State'].apply(lambda x: states[str(x)].lower())
merged_debt_df['County'] = merged_debt_df['County'].apply(lambda x: str(x).lower())

census_df['County']  = census_df['County'].apply(lambda x: str(x).lower())
census_df['State']  = census_df['State'].apply(lambda x: str(x).lower())
census_df['Year']  = census_df['Year'].apply(lambda x: int(x))

fdic_df_long['County'] = fdic_df_long['County'].apply(lambda x: str(x).lower())
fdic_df_long['State'] = fdic_df_long['State'].apply(lambda x: str(x).lower())
fdic_df_long['Year'] = fdic_df_long['Year'].apply(lambda x: int(x))

census_fdic_merged_df = pd.merge(census_df, fdic_df_long, how='inner', on=['Year', 'State', 'County'])
merge_final_df = pd.merge(census_fdic_merged_df, merged_debt_df, how='inner', on=['State', 'County'])

'''
excel_test_path = '/Users/Adam/Research/BA_Thesis/Data final/merged_data.xlsx'
writer = pd.ExcelWriter(excel_test_path, engine='xlsxwriter')
merge_final_df.to_excel(writer, 'Sheet1')
writer.save()
'''

####################################### now the fun begins ####################################################################





















################################## other cleaning in ziebarth data ############################

#####some new york is just n ################
#for index, row in census_df.iterrows():
	#if row['state'] == 'n':
		#print('gotcha!')
		#fdic_df_long.ix[index, 'state'] = 'new york'

#other ziebarth exceptions: n for new york, new hempshire instead of new hampshire!, blanks for states, south carolina has a space at the end "south carolina "
# "st." in city names --  take the period out of the st
# parishes in county name -- issue? -- st john the baptist especially
# arlington county VA not there, hernando FL, grays harbor, WA, ; alexandria, VA; drexel city in cass county and bates county, made it in bates county
#some places have two counties ->/ ;; de witt vs deWitt;; made haleyville AL in marion county (in 2 counties
# RADIOZLDXQDWJVL 1929 wront state -- should be kentucky; same with 118TVCMLLYDTJ 1935 should be NC not NY??
# 119PAJRDVUYNW put in pettis county since in two counties
# independent cities like st. louis dont have a county -> still making it st luois county; 118OLDKHJWCSR 1933 should be MO;
# 122APOXATJSZD 1935 should be NJ not NY; BTTUQCNAJQ technically in greenville and anderson counties 
# 118USMNJVOHNW 1929 should be North Dakota, not NC; same for 118WQJCLHLIPY 1929 and 118NIWRWPVBKX 1929 and 118QUHDJYIIMM 1929 and 118CWIKDINYXE 1929
# and 118OVTQBVZLXI 1929 and 118PIGIDFHTRQ 1929 and 118MTSTKZOEQR 1929
# 118JESCTMJCQN 1931 should be Colorado I think
# i took out parish from all the county names in LA, and city from other county names in MO (st louis city) and VA (alexandria etc) -> maybe worth investigating later

























