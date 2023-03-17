import pandas as pd
import pickle as pkl
import numpy as np
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.compose import ColumnTransformer
import pyreadr
import matplotlib.pyplot as plt
from sklearn.inspection import permutation_importance
from sksurv.metrics import integrated_brier_score

#load model for analysis, from nonparametric non-time varying models
with open("5_predictions/model_objects/cphnet_model.pkl", "rb") as input_file:
 model = pkl.load(input_file)

#print(model)
#print('end model')
save_path = 'path_to_save_to'

#load train and test datasets
train = pyreadr.read_r("4_modelling data/train.rds")[None]
test = pyreadr.read_r("4_modelling data/test.rds")[None]

train = train.reset_index()
test = test.reset_index()
print(test.columns)

#subset data to specific population if desired
#train = train[train['r_educl_cat'] == '3'] #1-3
#train = train[train['r_gender_cat'] == '1'] #1,0
#train = train[train['r_race_cat'] == '1'] #1,2,3
#train = train

# Get outcomes, predictors, and ids for specific dataset
# Same as model processing code
train = train.sort_values('wave_outcome').drop_duplicates('hhidpn')
test = test.sort_values('wave_outcome').drop_duplicates('hhidpn')

outcomes = [c for c in train.columns if 'outcome' in c]

train['event_type_outcome'] = train['event_type_outcome'].astype(bool)
test['event_type_outcome'] = test['event_type_outcome'].astype(bool)

y_train = train.loc[:,['event_type_outcome', 'exit_time_outcome']].to_records(index=False)
y_test = test.loc[:,['event_type_outcome', 'exit_time_outcome']].to_records(index=False)

id_train = train['hhidpn']
id_test = test['hhidpn']

preds = [c for c in train.columns if not 'outcome' in c and c not in ['hhidpn']]

X_train = train.loc[:,preds]
X_test = test.loc[:,preds]

categoricals = [c for c in X_train.columns if '_cat' in c]
numerics = [c for c in X_train.columns if not c in categoricals]

scale_transformer = StandardScaler()
categorical_transformer = OneHotEncoder(handle_unknown="ignore", drop = 'first')

preprocessor = ColumnTransformer(
    transformers=[
        ("cat", categorical_transformer, categoricals),
        ('scale', scale_transformer, ['age']),
    ], remainder = 'passthrough'
)

t = preprocessor.fit_transform(X_train)
pd.DataFrame(t).head()

times = np.arange(1.0, 28.0)
np.max(train['exit_time_outcome'])

#custom model scoring function
def model_score(mod, Xt, ys):
    survs = mod.predict_survival_function(Xt)
    times = np.arange(1.0, 28.0)
    preds = np.asarray([[fn(t) for t in times] for fn in survs])
    score = integrated_brier_score(ys[0], ys[1], preds, times)
    return -score

print('var impt running')
perm_importance = permutation_importance(model, X_test, (y_train, y_test), scoring = model_score)

print('var impt done')

#sort variable importance and make df
sorted_idx = perm_importance.importances_mean.argsort()
outs = pd.DataFrame({'importance': perm_importance.importances_mean[sorted_idx], 'variable': X_test.columns[sorted_idx]})
print(outs)

#make figure to display
plt.rcParams["figure.figsize"] = (10,15)
plt.barh(X_train.columns[sorted_idx][-10:], perm_importance.importances_mean[sorted_idx][-10:])
plt.xlabel("Permutation Importance")
plt.show()
plt.savefig(f'{save_path}/cphnet_outs.png')

#save outputs
with open(f'{save_path}/cphnet_outs_negibs_df.pickle', 'wb') as file:
    pkl.dump(outs, file)
