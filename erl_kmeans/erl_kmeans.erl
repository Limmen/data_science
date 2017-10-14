%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc erl_kmeans.erl
%% K-Means partitions the dataset into K clusters where each datapoint
%% belong to the cluster which has a mean that most resembles its own value
%% The algorithm iteratively works as follows.
%% 1. Start with K initial cluster-centers
%% 2. For each cluster center (K), compute all items which are closer to
%% that center than any othe center.
%% 3. For each cluster center (K) take the mean of the points that belong to it
%% 4. Repeat step 2 and 3 with the new centers being the computed mean-values.
%% Example use-case:
%% > c(erl_kmeans).
%% > Data = erl_kmeans:read_data("sample_kmeans_data.txt").
%% > Clusters = erl_kmeans:run_k_means(2, 50, Data).
%% @end
%%%-------------------------------------------------------------------
-module(erl_kmeans).
-author('Kim Hammar <kimham@kth.se>').

%% API
-export([read_data/1, run_k_means/3, print_clusters/1]).

%% types

-type cluster():: list(feature_vector()).

-type data_points() :: list(feature_vector()).

-type feature_vector() :: list().

-type clusters():: list(cluster()).

-type centroids() :: list(feature_vector()).



%%====================================================================
%% API functions
%%====================================================================

-spec read_data(FileName::string())->list(list()).
read_data(FileName)->
    Lines = read_input_lines(FileName),
    parse_input(Lines).

%% k-means algorithm given K, Number of Iterations and DataPoints
-spec run_k_means(K :: integer(), Iterations :: integer(), FeatureVectors :: data_points()) -> Clusters :: clusters().
run_k_means(K, Iterations, FeatureVectors)->
    io:format("--- K-MEANS Algorithm starting ---~n K : ~p, Iterations : ~p~n", [K,Iterations]),
    Centroids = initial_centroids(K, FeatureVectors),
    io:format("Initial Centroids: ~p~n", [Centroids]),
    Clusters = clusters(Centroids, FeatureVectors),
    io:format("Initial clusters ~p~n", [Clusters]),
    Clusters2 = iterations(Iterations, Iterations, Clusters, FeatureVectors),
    print_clusters(Clusters2),
    Clusters2.

%% pretty print clusters
print_clusters(Clusters)->
    lists:map(fun(I) ->
		      io:format("Cluster ~p: ~p ~n", [I, lists:nth(I, Clusters)])
	      end, lists:seq(1, length(Clusters))),
    io:format("~n").


%%====================================================================
%% Internal functions
%%====================================================================

%% Get clusters given Centroids and DataPoints
-spec clusters(Centroids :: centroids(), Data :: data_points()) -> Clusters :: clusters().
clusters(Centroids, Data)->
    InitMapping = lists:map(fun(C) -> {C, []} end, Centroids),
    MappedToCentroids = lists:foldl(fun(DataPoint, ClustersAcc) ->
					    Cluster = least_squares_error(DataPoint, Centroids),
					    {Cluster, Members} = lists:keyfind(Cluster, 1, ClustersAcc),
					    lists:keyreplace(Cluster, 1, ClustersAcc, {Cluster, [DataPoint|Members]})
				    end, InitMapping, Data),
    Clusters = lists:map(fun({_, Cluster}) -> Cluster end, MappedToCentroids),
    Clusters.

%% Computes the best centroid for a given datapoint
-spec least_squares_error(DataPoint :: feature_vector(), Centroids :: centroids()) -> Centroid :: feature_vector().
least_squares_error(DataPoint, Centroids)->
    SquaredErrors = lists:map(fun(C) -> {euclidean_distance(DataPoint, C), C} end, Centroids),
    {_, C} = lists:min(SquaredErrors),
    C.

%% Computes euclidean distance between two datapoints
-spec euclidean_distance(Q :: feature_vector(), P :: feature_vector()) -> float().
euclidean_distance(Q, P)->
    SumSquaredErrors = lists:sum(lists:map(fun(I) -> math:pow(lists:nth(I, Q) - lists:nth(I, P), 2) end, lists:seq(1, length(Q)))),
    math:sqrt(SumSquaredErrors).

%% Performs N iterations of hte kmeans algorithm given initial clusters
-spec iterations(N :: integer(), MaxIterations :: integer(), Clusters :: clusters(),  Data :: data_points()) -> Clusters :: clusters().
iterations(0, _, Clusters, _)->
    io:format("All iterations finished ~n~n"),
    Clusters;

iterations(N, Max, Clusters, Data) when N > 0 ->
    Centroids = average_centroids(Clusters),
    io:format("Iteration ~p, new centroids: ~p ~n", [Max - N+1, Centroids]),
    iterations(N-1, Max, clusters(Centroids, Data), Data).

%% Compute new average centroids based on given clusters
-spec average_centroids(Clusters :: clusters()) -> Centroids :: centroids().
average_centroids(Clusters) ->
    lists:map(fun(C) ->
		      average(C)
	      end, Clusters).

%% Computes average of a cluster
-spec average(Cluster :: cluster()) -> AveragedCluster :: cluster().
average(Cluster)->
    SummedCluster = lists:foldl(fun(Member, Acc) ->
					lists:foldl(fun(I, Acc2) ->
							    case Acc of
								[] ->
								    Sum = lists:nth(I, Member),
								    [Sum|Acc2];
								_ ->
								    Sum = lists:nth(I, Member) + lists:nth(I, Acc),
								    [Sum|Acc2]
							    end
						    end, [], lists:seq(1, length(Member))) end, [], Cluster),
    AveragedCluster = lists:map(fun(C) -> C/length(Cluster) end, SummedCluster),
    AveragedCluster.

%% Reads input file into list of lines
-spec read_input_lines(FileName::string())-> ListOfLined :: list().
read_input_lines(FileName)->
    {ok, Binary} = file:read_file(FileName),
    string:tokens(erlang:binary_to_list(Binary), "\n").

%% Parses list of lines into feature-vectors
-spec parse_input(FileName::list)-> ParsedInput :: data_points().
parse_input(Lines)->
    lists:map(fun(Line) ->
		      lists:map(fun(X) -> list_to_float(X) end,
				string:tokens(Line, " ")) end, Lines).

%% Computes initial centroids by picking random datapoints
-spec initial_centroids(K::integer(), Data::list(list())) -> Centroids :: list(list()).
initial_centroids(K, Data)->
    {Centroids, _} = lists:foldl(fun(_, {Centroids, DataAcc}) ->
			Centroid = lists:nth(rand:uniform(length(DataAcc)), DataAcc),
			{[Centroid|Centroids], lists:delete(Centroid, DataAcc)}
		       end, {[], Data}, lists:seq(1, K)),
    Centroids.
