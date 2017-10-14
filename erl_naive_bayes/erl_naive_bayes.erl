%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc erl_naive_bayes.erl
%% Naive Bayes means that the simplifying assumption that the value of
%% a particular feature is independent of the values of other features.
%% Learning in NaiveBayes essentially means to construct the Frequency-table,
%% the likelihood-table and the conditional-probability table (based on the observations in likelihood table)
%% the conditional probability contains probabilities P(attribute|class)
%% To generalize/classify: calculate posterior probability based on the "evidence" of the data to be classified.
%% Pick the classification with the highest posterior probability.
%% To compute the posterior probability we use the frequency table, the likelihood table and the conditional
%% probability table which all were constructed during training based on training data.
%% The naive thing with the classifier is that we assume that all atrtributes are independent and thus to
%% calculate the posterior probability given a set of evidence-attributes we can take the product. This is
%% naive because it is likely that the attributes are not independent in reality, but it makes it much simpler
%% to compute.
%% FrequencyTable is a table of {attribute, AttributeTable} where AttirbuteTable is a list of all observed values of
%% the attribute and its corresponding classification in the training set.
%% LikelihoodTable is a table of {{attribute, Value}, Probability} or {{classification}, Probability} based on the training data.
%% ConditionalProbabilityTable is a table of {{attribute, value}, Class, Probability}, i.e the probabiliy of attribute value given
%% classification. Based on training data.
%% Example use-case:
%% > c(erl_naive_bayes).
%% > Examples = erl_naive_bayes:examples_play_tennis().
%% > Model = erl_naive_bayes:learn_model(Examples).
%% > erl_naive_bayes:classify(Model, [{outlook, overcast}, {temperature, hot}, {humidity, high}, {windy, true}]).
%% > erl_naive_bayes:classify(Model, [{outlook, overcast}]).
%% > erl_naive_bayes:classify(Model, [{outlook, sunny}, {humidity, high}]).
%% @end
%%%-------------------------------------------------------------------
-module(erl_naive_bayes).
-author('Kim Hammar <kimham@kth.se>').

%% API
-export([learn_model/1, examples_play_tennis/0, classify/2]).

%% types

-type model()::{frequency_table(), likelihood_table(), conditional_probability_table(), classes()}.

-type conditional_probability_table()::list({attribute_value_pair(), classification(), float()}).

-type likelihood_table()::list({attribute_value_pair() | classification(), float()}).

-type frequency_table()::list(frequency_table_row()).

-type frequency_table_row()::{attribute(), attribute_table()}.

-type attribute_table()::list(attribute_table_row()).

-type attribute_table_row()::{attribute_value(), list({classification(), integer()})}.

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

%% Learn the naive-bayes model given a set of examples
-spec learn_model(Examples :: examples()) -> model().
learn_model(Examples) ->
    FreqTable = create_frequency_table(Examples),
    LikelihoodTable = create_likelihood_table(Examples),
    ConditionalProbabilityTable = create_conditional_probability_table(Examples, FreqTable),
    Classes = sets:to_list(sets:from_list(classes(Examples))),
    {FreqTable, LikelihoodTable, ConditionalProbabilityTable, Classes}.

%% Classify a set of attributes to a given class given a Model
-spec classify(Model::model(), Evidence::attribute_value_pairs())-> {ClassProbability :: float(), Class :: classification()}.
classify({_,LikelihoodTable,ConditionalProbabilityTable,Classes}, Evidence)->
    PosteriorProbabilities = lists:map(fun(C) -> {posterior_probability(C, Evidence, LikelihoodTable, ConditionalProbabilityTable), C} end, Classes),
    {ClassProb, Class} = lists:max(PosteriorProbabilities),
    {ClassProb, Class}.

%%====================================================================
%% Internal functions
%%====================================================================

%% Calculate Posterior probability of Class given evidence and Conditional probabilities
-spec posterior_probability(Class::classification(), Evidence::attribute_value_pairs(), LikelihoodTable::likelihood_table(), ConditionalProbabilityTable::conditional_probability_table()) -> PosteriorProbability::float().
posterior_probability(Class, Evidence, LikelihoodTable, ConditionalProbabilityTable)->
    {Class, ClassProb} = lists:keyfind(Class, 1, LikelihoodTable),
    ConditionalProbList = lists:map(fun(AV) ->
					    {AV, Class, AttributeValueProb} = lists:keyfind(Class, 2, lists:filter(fun({AV2, _, _})-> AV =:= AV2 end, ConditionalProbabilityTable)),
					    AttributeValueProb
				    end, Evidence),
    ConditionalProb = lists:foldl(fun(P, Acc) ->
					  P*Acc
				  end, ClassProb, ConditionalProbList),
    EvidenceProbList = lists:map(fun(AV) ->
					 {AV, AVProb} = lists:keyfind(AV, 1, LikelihoodTable),
					 AVProb
				 end, Evidence),
    EvidenceProb = lists:foldl(fun(P, Acc) ->
				       P*Acc
			       end, 1, EvidenceProbList),
    ConditionalProb/EvidenceProb.
%% Create conditional probability table for given exampels and frequency table
-spec create_conditional_probability_table(Examples:: examples(), FreqTable::frequency_table()) -> conditional_probability_table().
create_conditional_probability_table(Examples, FreqTable)->
    AttributeValues = sets:to_list(sets:from_list(attribute_value_pairs(Examples))),
    Classes = sets:to_list(sets:from_list(classes(Examples))),
    lists:flatten(lists:foldl(fun(AV, Acc) ->
				      ConditionalProbabilityList = lists:foldl(fun(C, Acc2) ->
										       [{AV, C, conditional_probability(AV, C, FreqTable)}|Acc2]
									       end, [], Classes),
				      [ConditionalProbabilityList|Acc]
			      end, [], AttributeValues)).

%% Calculate conditional probability for Attribute-Value pair given Class
-spec conditional_probability(attribute_value_pair(), classification(), frequency_table()) -> ConditionalProbability::float().
conditional_probability({Attribute, Value}, Class, FreqTable)->
    {all, L} = lists:keyfind(all, 1, FreqTable),
    {Class, [{Class, ClassFreq}]} = lists:keyfind(Class, 1, L),
    {Attribute, AttributeTable} = lists:keyfind(Attribute, 1, FreqTable),
    {Value, ValueClassCountList} = lists:keyfind(Value, 1, AttributeTable),
    {Class, AttributeValueGivenClassFreq} = lists:keyfind(Class, 1, ValueClassCountList),
    AttributeValueGivenClassFreq/ClassFreq.

%% Create likelihoodTable given a set of examples
-spec create_likelihood_table(Examples :: examples) -> likelihood_table().
create_likelihood_table(Examples) ->
    AttributeValues = attribute_value_pairs(Examples),
    Classes = classes(Examples),
    ClassesAttributes = Classes ++ AttributeValues,
    ClassesAttributesSet = sets:to_list(sets:from_list(ClassesAttributes)),
    LikelihoodTable = lists:map(fun(CA) ->
					Count = lists:foldl(fun(CA2, Acc) ->
								    case CA2 of
									CA ->
									    Acc + 1;
									_ ->
									    Acc
								    end
							    end, 0, ClassesAttributes),
					Probability = Count / length(Examples),
					{CA, Probability}
				end, ClassesAttributesSet),
    LikelihoodTable.

%% Create frequency table given a set of examples
-spec create_frequency_table(Examples :: examples()) -> frequency_table().
create_frequency_table(Examples)->
    Attributes = attributes(Examples),
    FreqAttributes = lists:foldl(fun(A, Acc) ->
					 [{A, create_attribute_table(A,Examples)}|Acc]
				 end, [], sets:to_list(sets:from_list(Attributes))),
    Classes = classes(Examples),
    FreqTotal = {all, lists:map(fun(C) ->
					Count = length(lists:filter(fun(C2) -> C2 =:= C end, Classes)),
					{C, [{C, Count}]}
				end, sets:to_list(sets:from_list(Classes)))},
    FreqAttributes ++ [FreqTotal].

%% Create attribute table for a given attribute and set of examples
-spec create_attribute_table(Attribute :: attribute(), Examples :: examples()) -> attribute_table().
create_attribute_table(Attribute, Examples)->
    Values = values(Examples, Attribute),
    lists:foldl(fun(V, Acc) ->
			[{V, count_classifications_for_attribute_value(Attribute, V, Examples)}|Acc]
		end, [], sets:to_list(sets:from_list(Values))).

%% Count classifications per value for a given attribute
-spec count_classifications_for_attribute_value(Attribute::attribute(), Value :: attribute_value(), Examples :: examples) -> attribute_table_row().
count_classifications_for_attribute_value(Attribute, Value, Examples)->
    Classes = classes(Examples),
    CountedClasses = lists:map(fun(C) ->
				       Count = lists:foldl(fun
							       ({AVs, C2}, Acc) ->
								  case C2 of
								      C ->
									  case lists:member({Attribute, Value}, AVs) of
									      true ->
										  Acc + 1;
									      false ->
										  Acc
									  end;
								      _ ->
									  Acc
								  end
							  end, 0, Examples),
				       {C, Count}
			       end, sets:to_list(sets:from_list(Classes))),
    CountedClasses.


%% Extract list of classes from Examples
-spec classes(Examples :: examples()) -> Classes::classes().
classes(Examples)->
    lists:map(fun({_, C}) -> C end, Examples).

%% Extract values for a given attribute from Examples of attribute-value pairs + classification
-spec values(Examples :: examples(), Attribute :: attribute()) -> Values::values().
values(Examples, Attribute)->
    lists:map(fun({_,V}) -> V end,
	      lists:filter(fun({A,_}) -> A =:= Attribute end,
			   attribute_value_pairs(Examples))).

%% Extract attribute-value-pairs from Examples
-spec attribute_value_pairs(Examples :: examples()) -> AttributeValues::attribute_value_pairs().
attribute_value_pairs(Examples)->
    lists:flatten(lists:map(fun({A, _}) -> A end, Examples)).

%% Extract attributes from Examples
-spec attributes(Examples :: examples()) -> Attributes::attributes().
attributes(Examples)->
    lists:map(fun({A,_}) -> A end, lists:flatten(lists:map(fun({A, _}) -> A end, Examples))).

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
