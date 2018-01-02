import pandas as pd

edgelist = pd.read_csv("example2.csv", header=None)
edgelist.columns = ["a", "b", "c"]
edgelistClean = edgelist[["a", "b"]]
edgelistClean.to_csv("example2_clean.csv", index=False, header=False)