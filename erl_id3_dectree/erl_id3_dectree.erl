%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc erl_id3_dectree.erl
%% ID3 Algorithm creates a decision tree for classification based on labeled
%% training data. It creates the tree top-down and selects the attribute to split on
%% which gives most information. The measure for information is entropy. The tree
%% is created recursively and at each step we compute the attribute to split on based on
%% entropy measure of the attributes that have not yet split. In each node the attribute with
%% the highest information gain is selected and branches for each value of the attribute is created
%% and nodes for the branches are computed in the same manner recursively.
%% The tree is on the format {node, attribute, Children} where children is a list of child nodes
%% or branches. A branch is {branch, decision, childNode}.
%% The computed decision tree can be printed as a set of IF-THEN rules.
%% Example use-case:
%% erl
%% > c(erl_id3_dectree).
%% > Examples = erl_id3_dectree:examples_play_tennis().
%% > Tree = erl_id3_dectree:learn_tree(Examples).
%% > erl_id3_dectree:pretty_print(Tree).
%% > erl_id3_dectree:generalize(Tree, [{outlook, overcast}, {temperature, hot}, {humidity, high}, {windy, true}]).
%% @end
%%%-------------------------------------------------------------------
-module(erl_id3_dectree).
-author('Kim Hammar <kimham@kth.se>').

%% API
-export([learn_tree/1, generalize/2, examples_play_tennis/0, pretty_print/1]).

%% types

-type dec_tree():: id3_node().

-type id3_node()::{node, Attribute:: attribute(), Children:: list(id3_node() | branch() | nil)}.

-type branch()::{branch, Decision:: attribute_value(), Child:: id3_node()}.

-type attribute_value_pairs()::list(attribute_value_pair()).

-type attribute_value_pair()::{attribute(), attribute_value()}.

-type attribute():: atom().

-type attributes():: list(attribute()).

-type attribute_value():: atom().

-type values():: values().

-type classification():: atom().

-type classes():: list(classification()).

-type example():: {attribute_value_pairs(), classification()}.

-type examples() :: list(example()).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Learns the decision tree given a list of examples
-spec learn_tree(Examples:: examples()) -> dec_tree().
learn_tree(Examples)->
    Classes = classes(Examples),
    MostCommonClass = most_common_class(Classes),
    Attributes = attributes(Examples),
    case length(sets:to_list(sets:from_list(Classes))) =:= 1 of
	true ->
	    {node, lists:nth(1, Classes), []};
	false ->
	    case length(Attributes) of
		0 ->
		    {node, MostCommonClass, []};
		_ ->
		    BestAttribute = best_classifying_attribute(Examples),
		    Children = lists:foldl(fun(Value, Acc) ->
						   ExamplesSplit = decide(Examples, BestAttribute, Value),
						   case length(ExamplesSplit) of
						       0 ->
							   [{branch, Value, {MostCommonClass, []}}|Acc];
						       _ ->
							   [{branch, Value, learn_tree(remove_attribute(ExamplesSplit, BestAttribute))}|Acc]
						   end
					   end, [], sets:to_list(sets:from_list(values(Examples, BestAttribute)))),
		    {node, BestAttribute, Children}
	    end
    end.

%% @doc
%% Generalize the classification based on decision tree and given attribute-value pairs.
-spec generalize(Tree :: dec_tree(), AttributeValues :: attribute_value_pairs()) -> classification().
generalize({node, Classification, []}, _) ->
    Classification;

generalize({node, Attribute, Children}, AttributeValues) ->
    case lists:keyfind(Attribute, 1, AttributeValues) of
	{Attribute, Value} ->
	    {branch, Value, Node} = lists:keyfind(Value, 2, Children),
	    generalize(Node, AttributeValues);
	false ->
	    io:format("Attribute in attribute-value list is missing, cannot generalize with the decision tree ~n"),
	    nil
    end.


%% @doc
%% Print tree with if-then rules
-spec pretty_print(dec_tree()) -> ok.
pretty_print(Tree)->
    pretty_print(Tree, ""),
    ok.

-spec pretty_print(dec_tree(), string()) -> ok.
pretty_print({node, Attr, Children}, SoFar) ->
    lists:map(fun(Child) ->
		      pretty_print(Child, io_lib:format("~sif ~p ", [SoFar,Attr]))
	      end, Children),
    ok;

pretty_print({branch, Decision, {node, Attr, []}}, SoFar)->
    io:format("~s= ~p then ~p~n", [SoFar, Decision, Attr]);

pretty_print({branch, Decision, Child}, SoFar)->
    pretty_print(Child, io_lib:format("~s= ~p and ", [SoFar,Decision])).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc
%% Remove a attribute from examples
remove_attribute(Examples, Attribute)->
    lists:map(fun({Attributes, C}) ->
		      FilteredAttributes = lists:filter(fun({A,_}) -> A =/= Attribute end, Attributes),
		      {FilteredAttributes, C}
	      end, Examples).

%% @doc
%% Return examples consistent with Attribute=Value
-spec decide(Examples::examples(), Attribute::attribute(), Value::attribute_value()) -> examples().
decide(Examples, Attribute, Value)->
    lists:filter(fun({Attributes, _}) ->
			 lists:foldl(fun({A,V}, Acc) ->
					     case A of
						 Attribute ->
						     Value =:= V;
						 _ ->
						     Acc
					     end
				     end, true, Attributes) end, Examples).

%% @doc
%% Returns the attribute from a set of examples that is the best classifier
-spec best_classifying_attribute(Examples::examples())->attribute().
best_classifying_attribute(Examples)->
    {_,A} = lists:max(lists:map(fun(Attribute) -> {information_gain(Examples, Attribute), Attribute} end, sets:to_list(sets:from_list(attributes(Examples))))),
    A.

%% @doc
%% Calculate the entropy of list of example attribute-value pairs
-spec entropy(Examples :: examples()) -> float().
entropy(Examples) when length(Examples) =:= 0->
    0;

entropy(Examples) when length(Examples) > 0->
    Classes = classes(Examples),
    Proportions = lists:map(fun(C) ->
				    Part = length(lists:filter(fun(C2) -> C =:=C2  end,
							       Classes)),
				    Whole = length(Classes),
				    Part/Whole
			    end, sets:to_list(sets:from_list(Classes))),
    lists:sum(lists:map(fun(P) -> P * math:log2(1/P) end, Proportions)).

%% @doc
%% Calculate information gain of given attribute based on the examples
-spec information_gain(Examples :: examples(), Attribute :: attribute()) -> float().
information_gain(Examples, Attribute)->
    EntropyOfAllExamples = entropy(Examples),
    Subsets = subsets(Examples, Attribute),
    EntropyOfAllExamples - lists:sum(lists:map(fun(S) ->
						       Part = length(S),
						       Whole = length(Examples),
						       Proportion = Part/Whole,
						       EntropyOfSubset = entropy(S),
						       EntropyOfSubset * Proportion
					       end, Subsets)).

%% @doc
%% Calculate subsets of classifications when splitting on Attribute
-spec subsets(Examples :: examples(), Attribute :: attribute()) -> list(examples()).
subsets(Examples, Attribute)->
    Values = values(Examples, Attribute),
    lists:foldl(fun(Value, Acc) ->
			Subset = lists:filter(fun({Attributes, _}) ->
						      lists:foldl(fun({A,V}, Bool) ->
									  case A of
									      Attribute ->
										  V =:= Value;
									      _ ->
										  Bool
									  end
								  end, true, Attributes) end, Examples),
			[Subset|Acc]
		end, [], sets:to_list(sets:from_list(Values))).

%% @doc
%% Extract list of classes from Examples
-spec classes(Examples :: examples()) -> Classes::classes().
classes(Examples)->
    lists:map(fun({_, C}) -> C end, Examples).

%% @doc
%% Extract values for a given attribute from Examples of attribute-value pairs + classification
-spec values(Examples :: examples(), Attribute :: attribute()) -> Values::values().
values(Examples, Attribute)->
    lists:map(fun({_,V}) -> V end,
	      lists:filter(fun({A,_}) -> A =:= Attribute end,
			   attribute_value_pairs(Examples))).

%% @doc
%% Extract attribute-value-pairs from Examples
-spec attribute_value_pairs(Examples :: examples()) -> AttributeValues::attribute_value_pairs().
attribute_value_pairs(Examples)->
    lists:flatten(lists:map(fun({A, _}) -> A end, Examples)).

%% @doc
%% Extract attributes from Examples
-spec attributes(Examples :: examples()) -> Attributes::attributes().
attributes(Examples)->
    lists:map(fun({A,_}) -> A end, lists:flatten(lists:map(fun({A, _}) -> A end, Examples))).

%% @doc
%% Gets the most common class of a list of classes
-spec most_common_class(Classes :: classes()) -> classification().
most_common_class(Classes)->
    Counted = lists:map(fun(Class) ->
				{Class, length(lists:filter(fun(C) -> Class =:= C end, Classes))}
			end, Classes),
    {Class, _} = lists:foldl(fun({C, N}, {Class, A}) ->
				     case N > A of
					 true ->
					     {C,N};
					 false ->
					     {Class, A}
				     end
			     end, {nil, 0}, Counted),
    Class.

%%====================================================================
%% Example Data
%%====================================================================

%% @doc
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
