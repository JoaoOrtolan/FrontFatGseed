# -*- coding: utf-8 -*-
"""
Created on Mon Nov  2 20:41:21 2020

@author: Joao Ortolan
"""
    
    
import urllib.request
import pandas as pd
import zipfile

urllib.request.urlretrieve ("https://www.bcb.gov.br/fis/info/cad/postos/202008POSTOS.zip", "C:\\Users\\Joao Ortolan\\Desktop\\Teste_BC\\202008POSTOS.zip")

with zipfile.ZipFile("C:\\Users\\Joao Ortolan\\Desktop\\Teste_BC\\202008POSTOS.zip","r") as zip_ref:
    zip_ref.extractall("C:\\Users\\Joao Ortolan\\Desktop\\Teste_BC")
    
df = pd.read_excel("C:\\Users\\Joao Ortolan\\Desktop\\Teste_BC\\202008POSTOS.xlsx", header=9)
df2 = df[df[df.columns[13]].str.contains("PAA|PAB|PAC", na = False)]
df3 = df2.filter(items=df2.columns[0:17])
df3.columns = list(map(str.strip, df3.columns))

df3.to_excel("C:\\Users\\Joao Ortolan\\Desktop\\Teste_BC\\202008POSTOS.xls", index=False)





