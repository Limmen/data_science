# erl_content_based_recommendation

## About 
Implementation of a Content-Based recommendation system in Erlang. 

The system recommends movies to user based on its ratings and similar movies.

The dataset for training is in `data/`.
 
To compute similarity between movies the tags/textual description of movies is taken into account.
A movie is represented numerically as a one-hot-vector of tags weighted with TFIDF values to promote more rare tags than more common tags.
I.e rare tag: "indians", common tag: "based on a book".

To recommend movie to users given the TFIDF weighted matrix the set of movies seen by the user is merged into a long TFIDF vector that is weighted by the rating, this vector is then used to compute similarities with movies f or recommendation based on cosine-similarity between the vectors.

The top K most similar movies are recommended to the user to watch next.

## How to run

```erlang
c(erl_content_based_recommendation).
erl_content_based_recommendation:recommend(4045, 5).
erl_content_based_recommendation:recommend(144, 5).
```

## License

MIT

## Author 

Kim Hammar <kimham@kth.se>

2017