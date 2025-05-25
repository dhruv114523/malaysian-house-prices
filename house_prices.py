import pandas as pd
pd.set_option('display.max_columns', None)

df = pd.read_csv("malaysia_house_price_data_2025.csv")

states_df = df.groupby(["State"])

print(df)

for state, state_df in states_df:
    print(state)
    print(state_df["Median_PSF"].mean().round(2))

print("="*20, "Type", "="*20)

types_df = df.groupby("Type")

for type_name, type_df in types_df:
    print(type_name)
    print(type_df["Median_PSF"].mean().round(2))

