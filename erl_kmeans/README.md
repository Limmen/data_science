# erl_kmeans

## About 
Implementation of k-means clustering (unsupervised clustering algorithm).

The implementation uses a simple example dataset sample_kmeans_data.txt

## How to run

```erlang
c(erl_kmeans).
Data = erl_kmeans:read_data("sample_kmeans_data.txt").
Clusters = erl_kmeans:run_k_means(2, 50, Data).
```

## License

MIT

## Author 

Kim Hammar <kimham@kth.se>

2017