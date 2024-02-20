import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt

csv_path = 'data/sound_data.csv'
data = pd.read_csv(csv_path)

data = data.replace({'boat': 'vessel', 'healthy reef': 'reef', 'healthy reef + boat': 'reef + vessel'})
model_path = 'data/model_output.csv'
model = pd.read_csv(model_path)

data['settlement_rate'] = data['settled'] / (data['settled'] + data['unsettled']) * 100

model[['predicted', 'conf.low', 'conf.high']] = model[['predicted', 'conf.low', 'conf.high']] * 100

significance = {'reef': 'A',  'reef + vessel': 'AB', 'vessel': 'B', 'off reef': 'B', 'no sound': 'B'}
fig, ax = plt.subplots()
ax = sns.pointplot(model, x='x', y='predicted', ax=ax, linestyles="none", color='k')
ax = sns.stripplot(data, x='treatment', y='settlement_rate', hue='date', alpha=0.5, ax=ax, palette='colorblind')
ax.errorbar(data=model, x='x', y='predicted',
             yerr=[model.predicted - model['conf.low'].values, model['conf.high'].values - model.predicted ],
             color='black', linestyle='none')
for val, k in significance.items():
    ax.text(val, model.loc[model['x'] == val, 'conf.high'] * 1.05, k, horizontalalignment='center')
plt.ylabel('Larvae settled [%]')
plt.xlabel('Treatment')
plt.savefig('figures/model_stats_results.png')
plt.show()