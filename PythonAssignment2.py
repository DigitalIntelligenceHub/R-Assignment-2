import datetime
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import style
style.use('ggplot')

hospitaldata = pd.read_csv(r'D:\DIH\Assignments\FarooqUrRehman_KCI_Python_Assignment2\hospitaldata.csv')

#Answer 1
hospitaldata = hospitaldata.rename(columns=lambda x: x.replace('.', ''))
print(hospitaldata.columns)

#Answer 2
hospitaldata['WeekDay'] = pd.to_datetime(hospitaldata['Date']).dt.weekday_name
WeekDay_Counts=hospitaldata[['WeekDay','id']].groupby('WeekDay').agg('count')
WeekDay_Max = WeekDay_Counts['id'].max()
print(WeekDay_Counts[(WeekDay_Counts['id'] == WeekDay_Max)])

#Answer 3

Average_Age = hospitaldata.dropna(subset =['Age'])
Average_Age = Average_Age[(Average_Age['Age']!='-')]
Average_Age['Age_Years']=Average_Age['Age'].str.extract('(\d+)').astype(int)
Average_Age['Age_Years']=np.where(Average_Age['Age'].str.contains('M') == True, Average_Age['Age_Years']/12.0, Average_Age['Age_Years'])
print(Average_Age['Age_Years'].mean())

#Answer 4
Children_Count = Average_Age[(Average_Age['Age_Years'] >=1) & (Average_Age['Age_Years'] <=12)]
print(Children_Count['Age_Years'].count())

#Answer 5
Gender_Procedure = hospitaldata.dropna(subset =['Sex'])
Gender_Procedure = Gender_Procedure.dropna(subset =['Procedure'])
Gender_Procedure = Gender_Procedure[(Gender_Procedure['Sex']!='-')]
Gender_Procedure['Sex']=Gender_Procedure['Sex'].str.replace("m","M")
Gender_Procedure['Sex']=Gender_Procedure['Sex'].str.replace("f","F")
print(Gender_Procedure.groupby('Sex')['Procedure'].value_counts().nlargest(2))

#Answer 6
Doctor_Earnings = hospitaldata[(hospitaldata['ConsultingDoctor'].str.contains("Dr")==True)]
print(Doctor_Earnings.groupby("ConsultingDoctor")['AmountReceived'].max().nlargest(1))

#Answer 7
Procedure_Earnings=hospitaldata.dropna(subset =['Procedure'])
Procedure_Earnings=hospitaldata.dropna(subset =['AmountReceived'])
print(Procedure_Earnings.groupby("Procedure")['AmountReceived'].sum().nlargest(1))

#Answer 8
VF = hospitaldata.dropna(subset =['Time'])
VF = VF[(VF['Time']!='-')]
#Visit_Frequency['AM_PM']=np.where(Average_Age['Time'].str.contains('PM') == True, "PM","AM")
VF['VisitHour']=pd.to_datetime(VF['Time'])
VF['VisitHour']=VF['VisitHour'].dt.hour
VF1=VF[['VisitHour','id']]
VF1.groupby('VisitHour')['id'].count().nlargest(1)

#Answer 9
VF['TimeBracket']= np.where((VF['VisitHour'] >=6) & (VF['VisitHour'] <12), "Morning",np.where((VF['VisitHour'] >=12) & (VF['VisitHour'] <16), "Afternoon",np.where((VF['VisitHour'] >=16) & (VF['VisitHour'] <19), "Evening","Night")))
VF[['TimeBracket','VisitHour']]

#Answer 10 & 11
VF2=VF[['id','Date']].groupby('id').count()
VF2[VF2['Date'] > 1]

# Answer 12
Gender_Procedure2=Gender_Procedure[['id','Procedure','Date']].groupby(['id','Procedure']).count()
Gender_Procedure2[Gender_Procedure2['Date']>1]

# Answer 13
Average_Age['Sex']=Average_Age['Sex'].str.replace("m","M")
Average_Age['Sex']=Average_Age['Sex'].str.replace("f","F")
Average_Age.groupby("Sex")['Age_Years'].mean()

# Answer 14
Gender_Procedure = Gender_Procedure.dropna(subset =['AmountBalance'])
Gender_Procedure = Gender_Procedure[(Gender_Procedure['AmountBalance']!=' -   ')]
Gender_Procedure['AmountBalance1'] = Gender_Procedure['AmountBalance'].str.replace(',','')
Gender_Procedure['AmountBalance1'] = Gender_Procedure['AmountBalance1'].str.extract('(\d+)').astype(float)
sum(Gender_Procedure['AmountBalance1'])

# Answer 15
Consultation_Earnings=Procedure_Earnings[(Procedure_Earnings['Procedure'] == 'Consultation')]
print(sum(Consultation_Earnings['AmountReceived']))

# Answer 16
Age_Charges_Cor = Average_Age.dropna(subset =['TotalCharges'])
Age_Charges_Cor = Age_Charges_Cor[(Age_Charges_Cor['TotalCharges']!='Cancelled')]
np.correlate(Age_Charges_Cor['Age_Years'],Age_Charges_Cor['TotalCharges'].str.extract('(\d+)').astype(float))

# Answer 17
Average_Age['Age_Group']= np.where((Average_Age['Age_Years'] <=15), "1-15 Yrs",np.where((Average_Age['Age_Years'] >15) & (Average_Age['Age_Years'] <=30), "16-30 Yrs",np.where((Average_Age['Age_Years'] >30) & (Average_Age['Age_Years'] <=45), "31-45 Yrs","Above 45 Yrs")))
print(Average_Age.groupby('Age_Group')['id'].count())

# Answer 18
XRay_Scalling_Earnings=Procedure_Earnings[(Procedure_Earnings['Procedure'] == 'X Ray') | (Procedure_Earnings['Procedure'] == 'Scalling')]
print(sum(XRay_Scalling_Earnings['AmountReceived']))