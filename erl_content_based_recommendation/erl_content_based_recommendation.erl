%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc erl_knearest_neighbor.erl
%% We have a set of movies with textual desciption (assoicated "tags"),
%% We have a set of users and their preference of movies in the form of ratings
%% We want to recommend movies for a user based on its ratings
%% We need to find movies that are similar to what the user have rated
%%
%% To be able to calculate similarities of movies we need to represent each movie
%% mathematically in a way such that metrics like euclidean distance or cosine similarity
%% gives indiciation of actual similary of movies.
%%
%% The approach I've taken here is to only focus on the textual description (the tags),
%% not the titles of the movies. If two movies have similar tags they are considered similar.
%% Feature extraction to represent text can be done simply with Onehot-vector where each entry
%% represent the frequency of a tag for the movie. This would however loose the information that
%% some tags are more important than others for example "indians" is a much less frequent tag than
%% "based on a book" to represent this importance-factor of each tag we multiply each tag-frequency(TF)
%% with a weight called "Inverse-document frequency (IDF)" which leaves us with our final represenation of each tag for each movie which is the TF-IDF value.
%%
%% Now given the numerical representation of each movie as a row in a large matrix where the row
%% is a vector with the TF-IDF for each of its tags, we can calculate the similarities of movies.
%%
%% To recommend the user movies we need to represent the user's ratings as a movie so that we can
%% find similar movies and recommend those. To represent user-profile as a movie we merge all
%% tags of the movies rated by the user and their associated TFIDFs and then we multiply
%% each TFIDF with a weight that is based on how high the user-rating was.
%%
%% Finally we calculate the top K recommended movies by calculating the cosine-similarity between
%% each unseen movie and the user-profile (both represented as vectors) and return the top K
%% movies similar to the user-profile.
%%
%% Example:
%% > c(erl_content_based_recommendation).
%% > erl_content_based_recommendation:recommend(4045, 5).
%% > erl_content_based_recommendation:recommend(144, 5).
%% @end
%%%-------------------------------------------------------------------
-module(erl_content_based_recommendation).
-author('Kim Hammar <kimham@kth.se>').

%% API
-export([recommend/2]).

%% records
-record(movie_score, {id::movie_id(), score::score()}).
-record(user_profile, {id::user_id(), preference::tf_vector()}).
-record(tf_vector_entry, {tag_id :: tag_id(), tf :: tf()}).
-record(model, {tag_ids :: tag_ids(), tf_idfs :: movie_tfidfs()}).
-record(movie_tfidf, {id :: movie_id(), tfidf :: tf_vector()}).
-record(tag_id, {tag :: movie_tag(), id :: tag_id()}).
-record(dataset, {movie_tags :: movie_id_tags(), movie_titles :: movie_id_titles(),
		  user_ratings :: user_id_movie_ratings(), user_names :: user_id_names()}).
-record(movie_id_tag, {id :: movie_id(), tag :: movie_tag()}).
-record(movie_id_title, {id :: movie_id(), title :: movie_title()}).
-record(user_id_movie_rating, {id :: user_id(), movie :: movie_id(), rating :: movie_rating()}).
-record(user_id_name, {id :: user_id(), name :: user_name()}).

%% types
-type movie_id() :: integer().

-type movie_tag() :: string().

-type movie_id_tags() :: list(#movie_id_tag{}).

-type movie_title() :: string().

-type movie_id_titles() :: list(#movie_id_title{}).

-type user_id() :: integer().

-type movie_rating() :: float().

-type user_name() :: string().

-type user_id_names() :: list(#user_id_name{}).

-type user_id_movie_ratings() :: list(#user_id_movie_rating{}).

-type tag_id() :: integer().

-type tag_ids() :: list(#tag_id{}).

-type tf() :: float().

-type tf_vector() :: list(#tf_vector_entry{}).

-type movie_tfidfs() :: list(#movie_tfidf{}).

-type score() :: float().

-type movie_scores() :: list(#movie_score{}).

-type parsed_csv() :: list(csv_line()).

-type csv_line() :: list(string()).


%%====================================================================
%% API functions
%%====================================================================

%% Return top K recommended unseen movies for user.
-spec recommend(user_id(), integer()) -> movie_scores().
recommend(User, K)->
    Dataset = read_data(),
    io:format("Parse Dataset complete ~n", []),
    Model = tfidf_model(Dataset),
    io:format("Create Model complete ~n", []),
    UserProfile = user_profile(User, Model, Dataset),
    io:format("Create UserProfile complete ~n", []),
    io:format("Top ~p recommended movies for user: ~p~n", [K, User]),
    top_K_movies(Model, UserProfile, Dataset#dataset.user_ratings, K).

%%====================================================================
%% Internal functions
%%====================================================================

%% Return top K movies with highest cosine similarity
-spec top_K_movies(#model{}, #user_profile{}, user_id_movie_ratings(), integer()) -> movie_scores().
top_K_movies(Model, UserProfile, Ratings, K)->
    UserRatings = lists:filter(fun(R) -> R#user_id_movie_rating.id =:= UserProfile#user_profile.id end, Ratings),
    ScoredMovies = score_movies(Model, UserProfile),
    UnSeen = lists:filter(fun(M) ->
				  case lists:keyfind(M#movie_score.id, #user_id_movie_rating.movie, UserRatings) of
				      false ->
					  true;
				      _ ->
					  false
				  end
			  end, ScoredMovies),
    {_, TopK} = lists:foldl(fun(_, {MoviesLeft, TopKAcc}) ->
				    TopScore = top_score(MoviesLeft, nil),
				    MoviesLeft1 = lists:keydelete(TopScore#movie_score.id, #movie_score.id, MoviesLeft),
				    {MoviesLeft1, [TopScore|TopKAcc]}
			    end, {UnSeen, []}, lists:seq(1, K)),
    TopK.

%% Return highest movie_score
-spec top_score(movie_scores(), nil | float()) -> #movie_score{}.
top_score([], Max)->
    Max;
top_score([H|T], nil) ->
    top_score(T, H);
top_score([H|T], Max) ->
    case H#movie_score.score > Max#movie_score.score of
	true ->
	    top_score(T, H);
	false ->
	    top_score(T, Max)
    end.


%% Calculate score (cosine similarity) for each movie based on the user
-spec score_movies(#model{}, #user_profile{}) -> movie_scores().
score_movies(Model, UserProfile)->
    lists:foldl(fun(Movie, Acc) ->
			Numerator = dot_product(UserProfile#user_profile.preference, Movie#movie_tfidf.tfidf),
			Denominator = norm(UserProfile#user_profile.preference) * norm(Movie#movie_tfidf.tfidf),
			CosineSim = Numerator/Denominator,
			[#movie_score{id=Movie#movie_tfidf.id, score=CosineSim}|Acc]
		end, [], Model#model.tf_idfs).

%% Return dot-product (scalar) of vector A and B
-spec dot_product(tf_vector(), tf_vector()) -> float().
dot_product(A,B)->
    ATags = lists:map(fun(Entry) -> Entry#tf_vector_entry.tag_id end, A),
    BTags = lists:map(fun(Entry) -> Entry#tf_vector_entry.tag_id end, B),
    AllTags = ATags ++ BTags,
    lists:sum(lists:map(fun(Tag) ->
				case lists:keyfind(Tag, #tf_vector_entry.tag_id, A) of
				    false ->
					0;
				    TFEntryA ->
					case lists:keyfind(Tag, #tf_vector_entry.tag_id, B) of
					    false ->
						0;
					    TFEntryB ->
						TFEntryA#tf_vector_entry.tf * TFEntryB#tf_vector_entry.tf
					end
				end
			end, AllTags)).

%% Return norm of vector
-spec norm(tf_vector()) -> float().
norm(A)->
    math:sqrt(lists:sum(lists:map(fun(TFE) ->
					  math:pow(TFE#tf_vector_entry.tf, 2)
				  end, A))).

%% Return user profile given user-id and model and dataset
-spec user_profile(user_id(), #model{}, #dataset{}) -> #user_profile{}.
user_profile(User, Model, Dataset)->
    Ratings = lists:filter(fun(Rating) ->
				   Rating#user_id_movie_rating.id =:= User
			   end, Dataset#dataset.user_ratings),
    AvgRating = average_rating(Ratings),
    lists:foldl(fun(Rating, Acc)->
			RatingVal = Rating#user_id_movie_rating.rating,
			Multiplier = RatingVal - AvgRating,
			MovieId = Rating#user_id_movie_rating.movie,
			MovieModel = lists:keyfind(MovieId, #movie_tfidf.id, Model#model.tf_idfs),
			update_user_profile_vector(MovieModel#movie_tfidf.tfidf, Multiplier, Acc)
		end, #user_profile{id=User, preference=[]},Ratings).

%% Update user-profile with a moviemodel based on his/hers rating
-spec update_user_profile_vector(tf_vector(), float(), #user_profile{}) -> #user_profile{}.
update_user_profile_vector(MovieModel, Multiplier, Profile)->
    lists:foldl(fun(TFIDFVector, Acc) ->
			TagId = TFIDFVector#tf_vector_entry.tag_id,
			TF = TFIDFVector#tf_vector_entry.tf,
			Increment = TF*Multiplier,
			case lists:keyfind(TagId, #tf_vector_entry.tag_id, Acc#user_profile.preference) of
			    false ->
				#user_profile{
				   id=Acc#user_profile.id,
				   preference=[#tf_vector_entry{tag_id=TagId, tf=Increment}|Acc#user_profile.preference]};
			    Old ->
				Pref = lists:keyreplace(TagId, #tf_vector_entry.tag_id, Acc#user_profile.preference, #tf_vector_entry{
															tag_id=TagId,
															tf=Increment+Old#tf_vector_entry.tf}),
				#user_profile{
				   id=Acc#user_profile.id,
				   preference=Pref
				  }
			end
		end, Profile, MovieModel).

%% Calculate average rating
-spec average_rating(user_id_movie_ratings()) -> float().
average_rating(Ratings)->
    Sum = lists:sum(lists:map(fun(Rating) ->
				      Rating#user_id_movie_rating.rating
			      end, Ratings)),
    Len = length(Ratings),
    case Len > 0 of
	true ->
	    Sum/Len;
	false ->
	    Sum
    end.

%% Calculate the TFIDF model given a dataset.
%% Essentially it will calculate the TF-vector for each movie weighted with the IDF for each tag.
-spec tfidf_model(#dataset{}) -> #model{}.
tfidf_model(Dataset)->
    TagIds = tag_ids(Dataset#dataset.movie_tags),
    Movies = sets:to_list(sets:from_list(lists:map(fun(M) -> M#movie_id_title.id end, Dataset#dataset.movie_titles))),
    MoviesTF = lists:map(fun(M) ->
				 #movie_tfidf{id = M, tfidf = tf(TagIds, Dataset#dataset.movie_tags, M)}
			 end, Movies),
    DocTF = doc_tf(TagIds, MoviesTF),
    N = length(Dataset#dataset.movie_titles),
    GlobalIDF = idf(DocTF, N),
    WeightedMoviesTags = tfidf(GlobalIDF, MoviesTF),
    NormalizedWeightedMoviesTags = normalize(WeightedMoviesTags),
    #model{tag_ids = TagIds, tf_idfs = NormalizedWeightedMoviesTags}.

%% Normalizes list of movie vectors
-spec normalize(movie_tfidfs()) -> tf_vector().
normalize(MoviesIDF)->
    lists:map(fun(M) ->
		      Magnitude = norm(M#movie_tfidf.tfidf),
		      NormalizedTFIDFVector = lists:map(fun(TFE) ->
								#tf_vector_entry{
								   tag_id=TFE#tf_vector_entry.tag_id,
								   tf=TFE#tf_vector_entry.tf/Magnitude}
							end, M#movie_tfidf.tfidf),
		      #movie_tfidf{id=M#movie_tfidf.id, tfidf=NormalizedTFIDFVector}
	      end, MoviesIDF).

%% Calculates idf given a Global TF-vector and the total number of movies
-spec idf(tf_vector(), integer())-> tf_vector().
idf(TFVector, Size)->
    lists:map(fun(TFEntry) ->
		      N = Size,
		      DocTF = TFEntry#tf_vector_entry.tf,
		      IDF = math:log10(N/(1+DocTF)),
		      #tf_vector_entry{tag_id=TFEntry#tf_vector_entry.tag_id, tf=IDF}
	      end, TFVector).

%% Weights the TF for each movie and tag wwith the corresponding tag-idf
-spec tfidf(tf_vector(), movie_tfidfs())-> tf_vector().
tfidf(GlobalIDF, MoviesTF)->
    lists:map(fun(M) ->
		      TFVector = M#movie_tfidf.tfidf,
		      TFIDFVector = lists:map(fun(TFE) ->
						      TF = TFE#tf_vector_entry.tf,
						      TagID = TFE#tf_vector_entry.tag_id,
						      IDF = lists:keyfind(TFE#tf_vector_entry.tag_id, #tf_vector_entry.tag_id, GlobalIDF),
						      TFIDF = TF*IDF#tf_vector_entry.tf,
						      #tf_vector_entry{tag_id=TagID, tf=TFIDF}
					      end, TFVector),
		      #movie_tfidf{id=M#movie_tfidf.id, tfidf=TFIDFVector}
	      end, MoviesTF).

%% Calculates the vector with number of documents with tf(t,d) != 0 for each tag
-spec doc_tf(tag_ids(), movie_tfidfs()) -> tf_vector().
doc_tf(TagIds, MoviesTF)->
    lists:map(fun(TagId) ->
		      DocsMatch = lists:foldl(fun(MTF, Acc) ->
					  case lists:keyfind(TagId#tag_id.id, #tf_vector_entry.tag_id, MTF#movie_tfidf.tfidf) of
					      false ->
						  Acc;
					      _ ->
						  Acc + 1
					  end
				  end, 0, MoviesTF),
		      #tf_vector_entry{tag_id=TagId#tag_id.id, tf=DocsMatch}
	      end, TagIds).

%% Calculates the TF for each tag
-spec tf(tag_ids(), movie_id_tags()) -> tf_vector().
tf(TagIds, MovieTags)->
    lists:foldl(fun(MT, Acc) ->
			TagId = lists:keyfind(MT#movie_id_tag.tag, #tag_id.tag, TagIds),
			case lists:keyfind(TagId#tag_id.id, #tf_vector_entry.tag_id, Acc) of
			    false ->
				[#tf_vector_entry{tag_id=TagId#tag_id.id, tf=1}|Acc];
			    MT0 ->
				lists:keyreplace(TagId#tag_id.id, #tf_vector_entry.tag_id, Acc,
						 #tf_vector_entry{
						    tag_id=TagId#tag_id.id,
						    tf=MT0#tf_vector_entry.tf+1
						   })
			end
		end, [], MovieTags).

%% Calculates the TF-vector for a given movie
-spec tf(tag_ids(), movie_id_tags(), movie_id()) -> tf_vector().
tf(TagIds, MovieTags, Movie)->
    FilteredTags = lists:filter(fun(MT) -> MT#movie_id_tag.id =:= Movie
				end, MovieTags),
    tf(TagIds, FilteredTags).

%% Maps string-tags to numeric identifiers
-spec tag_ids(movie_id_tags()) -> tag_ids().
tag_ids(MovieTags)->
    Tags = sets:to_list(sets:from_list(lists:map(fun(MovieTag) -> MovieTag#movie_id_tag.tag end, MovieTags))),
    {TagIds, _} = lists:foldl(fun(Tag, {Acc, IdAcc}) -> {[#tag_id{tag=Tag, id=IdAcc+1}|Acc], IdAcc+1} end, {[], length(Tags)}, Tags),
    TagIds.

%% Reads and parses the data
-spec read_data() -> #dataset{}.
read_data()->
    io:format("Reading data...~n"),
    MovieTags = movie_tags(),
    io:format("MovieTags read and parsed ~n"),
    MovieTitles = movie_titles(),
    io:format("MovieTitles read and parsed ~n"),
    UserRatings = user_ratings(),
    io:format("UserRatings read and parsed ~n"),
    UserNames = user_names(),
    io:format("UserNames read and parsed ~n"),
    #dataset{
       movie_tags=MovieTags,
       movie_titles=MovieTitles,
       user_ratings=UserRatings,
       user_names=UserNames
      }.

%% Reads and parses data/users.csv
-spec user_names() -> user_id_names().
user_names()->
    ParsedCsv = parse_csv(read_lines("data/users.csv")),
    lists:map(fun([Id,Name]) ->
		      {IdInt, _} = string:to_integer(Id),
		      #user_id_name{
			 id=IdInt,
			 name = Name
			}
	      end, ParsedCsv).

%% Reads and parses data/ratings.csv
-spec user_ratings() -> user_id_movie_ratings().
user_ratings()->
    ParsedCsv = parse_csv(read_lines("data/ratings.csv")),
    lists:map(fun([Id,MovieId,Rating]) ->
		      {IdInt, _} = string:to_integer(Id),
		      {MovieIdInt, _} = string:to_integer(MovieId),
		      {RatingFloat, _} = string:to_float(Rating),
		      #user_id_movie_rating{
			 id=IdInt,
			 movie=MovieIdInt,
			 rating=RatingFloat
			}
	      end, ParsedCsv).

%% Reads and parses data/movie-titles.csv
-spec movie_titles() -> movie_id_titles().
movie_titles()->
    ParsedCsv = parse_csv(read_lines("data/movie-titles.csv")),
    lists:map(fun([Id,Title]) ->
		      {IdInt, _} = string:to_integer(Id),
		      #movie_id_title{
			 id=IdInt,
			 title=Title
			}
	      end, ParsedCsv).

%% Reads and parses data/movie-tags.csv
-spec movie_tags() -> movie_id_tags().
movie_tags()->
    ParsedCsv = parse_csv(read_lines("data/movie-tags.csv")),
    lists:map(fun([Id,Tag]) ->
		      {IdInt, _} = string:to_integer(Id),
		      #movie_id_tag{
			 id=IdInt,
			 tag=Tag
			}
	      end, ParsedCsv).

%% Reads input file into list of lines
-spec read_lines(FileName::string())-> ListOfLines :: list().
read_lines(FileName)->
    {ok, Binary} = file:read_file(FileName),
    string:tokens(erlang:binary_to_list(Binary), "\n").

%% Parses list of lines into feature-vectors
-spec parse_csv(FileName::list())-> ParsedCsv :: parsed_csv().
parse_csv(Lines)->
    lists:map(fun(Line) ->
		      string:tokens(Line, ",")
	      end, Lines).
