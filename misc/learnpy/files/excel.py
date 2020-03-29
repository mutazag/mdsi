# working with excel files 
# requires pip install xlrd
import pandas as pd

xls = pd.ExcelFile("files/battledeath.xlsx")
print(xls.sheet_names)

xlsdf = xls.parse("2002")
print(xlsdf.head())

print(xlsdf.keys())

xlsdf2  = xls.parse("2004")
print(xlsdf2.keys())
print(xlsdf2.head(10))