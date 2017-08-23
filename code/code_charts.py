# PLOT SOME RESULTS - DONUTS
import pandas as pd
import matplotlib.pyplot as plt
import plotly.plotly as py

#Get data
ops = pd.read_csv("~/BGSE_Classes/thesis/results/byoperator.csv")
ops.drop(ops.columns[0],axis=1, inplace=True)

sel = ops[ops['Year']==2016]
sel.drop('Year',axis=1, inplace=True)
sel['Count'] = 100 * sel['Count']/sel['Count'].sum()

pts = sel.copy()
pts['Count'] = [37.7,19.3,18.4,4.7,4.7,15.1]

col = ['#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c']
# #Format labels to make them shorter
# short_labels=np.zeros(6,dtype='object')
# for i,lab in enumerate(labels):
#     if i != 5:
#         pos=lab.find(' ')
#         short_labels[i]= lab[:pos]+"\n"+lab[pos+1:]
#     else:
#         short_labels[i]=lab

#Create and adjsut plot grid
fig, (ax1,ax2) = plt.subplots(1,2,figsize=(12,5))
plt.subplots_adjust(wspace=0.2)

#Draw 1st donut
ax1.axis('equal')
width = 0.5
kwargs = dict(colors=col, startangle=180,pctdistance=1-width/2,autopct='%1.1f%%')
outside, _, autotexts = ax1.pie(sel['Count'].values, radius=1, labels=sel['Operator'].values,**kwargs)
# for i,autotext in enumerate(autotexts):
#     autotext.set_color('white')
plt.setp( outside, width=width, edgecolor='white')
kwargs = dict(size=20, fontweight='bold', va='center')
ax1.text(0, 0, "IIS", ha='center', **kwargs)
ax1.text(0, 0, "\n\ndata", ha='center', size=15,va='center')

#Draw 2nd donut
ax2.axis('equal')
width = 0.5
kwargs = dict(colors=col, startangle=180,pctdistance=1-width/2,autopct='%1.1f%%')
outside, _, autotexts = ax2.pie(pts['Count'].values, radius=1, labels=pts['Operator'].values,**kwargs)
# for i,autotext in enumerate(autotexts):
#     if i !=5:
#         autotext.set_color('white')
plt.setp( outside, width=width, edgecolor='white')
kwargs = dict(size=20, fontweight='bold', va='center')
ax2.text(0, 0, "PTS", ha='center', **kwargs)
ax2.text(0, 0, "\n\ndata", ha='center', size=15,va='center')

plt.show()  # TO SAVE IT REPLACE WITH
#plt.savefig('donuts_operators.pdf',bbox_inches='tight')
