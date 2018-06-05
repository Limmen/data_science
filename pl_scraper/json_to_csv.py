import json

json_file = "result.json"
json_data=open(json_file)
data = json.load(json_data)

title_row = "year,title,Team,A,D,F,GD,L,P,Pos,Pts,W\n"

data = filter(lambda x: "tables" in x and len(x["tables"]) > 0, data)
entries = [[year["year"], year["tables"][0]["title"],row["Team"], row["A"], row["D"], row["F"], row["GD"], row["L"], row["P"], row["Pos"], row["Pts"], row["W"]] for year in data for row in year["tables"][0]["tableRows"]]

entries_cleaned = map(lambda x: ",".join(map(lambda z: z.replace("\n", "").replace("\r", ""), list(map(lambda y: str(y), x)))), entries)
with open('result.csv', 'a') as file:
    file.write(title_row)
    map(lambda x: file.write(x + "\n"), entries_cleaned)
