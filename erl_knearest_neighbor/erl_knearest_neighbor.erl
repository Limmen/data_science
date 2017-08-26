%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc erl_knearest_neighbor.erl
%% @end
%%%-------------------------------------------------------------------
-module(erl_knearest_neighbor).
-author('Kim Hammar <kimham@kth.se>').

%% API
-export([examples_play_tennis/0]).

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

%%====================================================================
%% Internal functions
%%====================================================================

%% Calculates the distance between two symbolic objects, uses the overlap-distance (0,1).
-spec overlap_distance(attribute_value_pairs(), attribute_value_pairs())-> float().
overlap_distance(X, Y)->
    ok.

%% Returns the K-Neighborhood given Examples with distances and a input X to be classfied.
-spec neighborhood(integer(), attribute_value_pairs(), list(example_with_distance())) -> list(example_with_distance()).
neighborhood(K, X, Examples)->
    ok.


%% Use weights here
-spec select_class(list(example_with_distance())) -> list(example_with_distance()).
select_class(Neighborhood)->
    ok.

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
