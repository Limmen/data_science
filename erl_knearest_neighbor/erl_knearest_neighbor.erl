%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc erl_knearest_neighbor.erl
%% Example use-case:
%% > c(erl_knearest_neighbor).
%% > Examples = erl_knearest_neighbor:examples_play_tennis().
%% > erl_knearest_neighbor:classify(4, [{outlook, overcast}, {temperature, hot}, {humidity, high}, {windy, true}], Examples).
%% > erl_knearest_neighbor:classify(4, [{outlook, rain}, {windy, true}], Examples).
%% > erl_knearest_neighbor:classify(4, [{outlook, rain}], Examples).
%% @end
%%%-------------------------------------------------------------------
-module(erl_knearest_neighbor).
-author('Kim Hammar <kimham@kth.se>').

%% API
-export([examples_play_tennis/0, classify/3]).

%% types

-type attribute_value_pairs()::list(attribute_value_pair()).

-type attribute_value_pair()::{attribute(), attribute_value()}.

-type attribute():: atom().

-type attributes():: list(attribute()).

-type attribute_value():: atom().

-type values():: values().

-type classification():: atom().

-type classes():: list(classification()).

-type example_with_distance()::{float(), example()}.

-type example():: {attribute_value_pairs(), classification()}.

-type examples() :: list(example()).

%%====================================================================
%% API functions
%%====================================================================

%% Classify set of attribute-value pairs with k-nearest neighbor given K and set of training examples
-spec classify(integer(), attribute_value_pairs(), examples())-> classification().
classify(K, X, Examples)->
    ExamplesWithDist = lists:map(fun(Ex = {AVs, _}) -> {overlap_distance(X, AVs), Ex} end, Examples),
    NeighborHood = neighborhood(K, ExamplesWithDist),
    select_class(NeighborHood).

%%====================================================================
%% Internal functions
%%====================================================================

%% Calculates the distance between two symbolic objects, uses the overlap-distance (0,1).
-spec overlap_distance(attribute_value_pairs(), attribute_value_pairs())-> float().
overlap_distance(X, Y)->
    Attributes = attributes(X) ++ attributes(Y),
    Distance = lists:foldl(fun(A, Acc) ->
		        Bool1 = lists:keymember(A, 1, X),
			Bool2 = lists:keymember(A, 1, Y),
			case (Bool1 and Bool2) of
			    true ->
				{A, V1} = lists:keyfind(A,1,X),
				{A, V2} = lists:keyfind(A,1,Y),
				case V1 =:= V2 of
				    true ->
					Acc + 0;
				    false ->
					Acc + 1
				end;
			    false ->
				Acc + 1
			end
		end, 0, Attributes),
    Distance.

%% Returns the K-Neighborhood given Examples with distances and a input X to be classfied.
-spec neighborhood(integer(), list(example_with_distance())) -> list(example_with_distance()).
neighborhood(K, Examples)->
    NeighborHood = lists:foldl(fun(Ex, Acc) ->
			case length(Acc) < K of
			    true ->
				[Ex|Acc];
			    false ->
				case lists:max([Ex|Acc]) of
				    Ex ->
					Acc;
				    Ex2 ->
					[Ex|lists:delete(Ex2, Acc)]
				end
			end
		end, [], Examples),
    NeighborHood.

%% Selects the most common class based on neighborhood and distance-weights
-spec select_class(list(example_with_distance())) -> list(example_with_distance()).
select_class(NeighborHood)->
    Classes = sets:to_list(sets:from_list(classes(NeighborHood))),
    WeightedClasses = lists:map(fun(C) ->
	      WeightedCount = lists:map(fun(Neighbor) ->
						weighted_class_equals(C,Neighbor)
						    end, NeighborHood),
		      {WeightedCount, C}
	      end, Classes),
    {_, Class} = lists:max(WeightedClasses),
    Class.

%% Calculate weight-equals value for given class and example
-spec weighted_class_equals(classification(), example_with_distance()) -> float().
weighted_class_equals(Class, {Dist, {_, C}})->
    I = class_equals(Class, C),
    (1/math:pow(Dist, 2))*I.

%% 1 if classes are equals otherwise 0.
-spec class_equals(classification(), classification())->integer().
class_equals(C, C)->
    1;
class_equals(_,_) ->
    0.


%% Extract list of classes from WeightedExamples
-spec classes(list(example_with_distance())) -> Classes::classes().
classes(Examples)->
    lists:map(fun({_, {_, C}}) -> C end, Examples).

%% Extract attributes from attribute-value pairs
-spec attributes(AV :: attribute_value_pairs()) -> Attributes::attributes().
attributes(AV)->
    lists:map(fun({A,_}) -> A end, AV).

%%====================================================================
%% Example Data
%%====================================================================

%% Sample set of examples
-spec examples_play_tennis() -> examples().
examples_play_tennis()->
    [
     {[
       {outlook,sunny},{temperature,hot},{humidity,high},{windy,false}
      ], not_play
     },
     {[
       {outlook,sunny},{temperature,hot},{humidity,high},{windy,true}
      ], not_play
     },
     {[
       {outlook,overcast},{temperature,hot},{humidity,high},{windy,false}
      ], play
     },
     {[
       {outlook,rain},{temperature,mild},{humidity,high},{windy,false}
      ], play
     },
     {[
       {outlook,rain},{temperature,cool},{humidity,normal},{windy,false}
      ], play
     },
     {[
       {outlook,rain},{temperature,cool},{humidity,normal},{windy,true}
      ], not_play
     },
     {[
       {outlook,overcast},{temperature,cool},{humidity,normal},{windy,true}
      ], play
     },
     {[
       {outlook,sunny},{temperature,mild},{humidity,high},{windy,false}
      ], not_play
     },
     {[
       {outlook,sunny},{temperature,cool},{humidity,normal},{windy,false}
      ], play
     },
     {[
       {outlook,rain},{temperature,mild},{humidity,normal},{windy,false}
      ], play
     },
     {[
       {outlook,sunny},{temperature,mild},{humidity,normal},{windy,true}
      ], play
     },
     {[
       {outlook,overcast},{temperature,mild},{humidity,high},{windy,true}
      ], play
     },
     {[
       {outlook,overcast},{temperature,hot},{humidity,normal},{windy,false}
      ], play
     },
     {[
       {outlook,rain},{temperature,mild},{humidity,high},{windy,true}
      ], not_play
     }
    ].
