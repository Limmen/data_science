# rm_fairness

## About 
Implementation of Dominant Resource Fairness, Basic Fairness and Max-Min fairness as CSPs implemented in prolog.

The example user demands and available resources is taken from the paper "Dominant Resource Fairness: Fair Allocation of Multiple Resource Types" by Ghodsi et al.
## How to run

```prolog
user1(X1), user2(X2), total(Tot), drf(X1, X2, Tot, (User1Tasks, User2Tasks)).

user1(X1), user2(X2), total(Tot), basic_fairness(X1, X2, Tot, (User1Tasks, User2Tasks)).

user1(X1), user2(X2), total(Tot), max_min_fairness(X1, X2, Tot, (User1Tasks, User2Tasks)).
```

## License

MIT

## Author 

Kim Hammar <kimham@kth.se>

2017