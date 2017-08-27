%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc erl_knearest_neighbor.erl
%% @end
%%%-------------------------------------------------------------------
-module(erl_content_based_recommendation).
-author('Kim Hammar <kimham@kth.se>').

%% API
-export([]).

%% records
-record(tf_vector_entry, {tag_id :: tag_id(), tf :: tf()}).
-record(model, {tag_ids :: tag_ids(), tf_idfs :: movie_tfidfs()}).
-record(movie_tfidf, {id :: movie_id(), tfidf :: tfidf()}).
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

-type tfidf() :: float().

-type tf() :: float().

-type tf_vector() :: list(#tf_vector_entry{}).

-type movie_tfidfs() :: list(#movie_tfidf{}).

-type parsed_csv() :: list(csv_line()).

-type csv_line() :: list(string()).


%%====================================================================
%% API functions
%%====================================================================


%%====================================================================
%% Internal functions
%%====================================================================

-spec tfidf_model(#dataset{}) -> #model{}.
tfidf_model(Dataset)->
    TagIds = tag_ids(Dataset#dataset.movie_tags),
    GlobalTF = tf(TagIds, Dataset#dataset.movie_tags),
    ok.

tf(TagIds, MovieTags)->
    ok.

-spec tag_ids(movie_id_tags()) -> tag_ids().
tag_ids(MovieTags)->
    Tags = sets:to_list(sets:from_list(lists:map(fun(MovieTag) -> MovieTag#movie_id_tag.tag end, MovieTags))),
    {TagIds, _} = lists:foldl(fun(Tag, {Acc, IdAcc}) -> {[#tag_id{tag=Tag, id=IdAcc+1}|Acc], IdAcc+1} end, length(Tags), Tags),
    TagIds.

-spec read_data() -> #dataset{}.
read_data()->
    MovieTags = movie_tags(),
    MovieTitles = movie_titles(),
    UserRatings = user_ratings(),
    UserNames = user_names(),
    #dataset{
       movie_tags=MovieTags,
       movie_titles=MovieTitles,
       user_ratings=UserRatings,
       user_names=UserNames
      }.

-spec user_names() -> user_id_names().
user_names()->
    ParsedCsv = parse_csv(read_lines("data/ratings.csv")),
    lists:map(fun([Id,Name]) ->
		      #user_id_name{
			 id=string:to_integer(Id),
			name = Name
			}
	      end, ParsedCsv).

-spec user_ratings() -> user_id_movie_ratings().
user_ratings()->
    ParsedCsv = parse_csv(read_lines("data/ratings.csv")),
    lists:map(fun([Id,MovieId,Rating]) ->
		      #user_id_movie_rating{
			 id=string:to_integer(Id),
			 movie=string:to_integer(MovieId),
			rating=string:to_float(Rating)
			}
	      end, ParsedCsv).

-spec movie_titles() -> movie_id_titles().
movie_titles()->
    ParsedCsv = parse_csv(read_lines("data/movie-titles.csv")),
    lists:map(fun([Id,Title]) ->
		      #movie_id_title{
			 id=string:to_integer(Id),
			 title=Title
			}
	      end, ParsedCsv).

-spec movie_tags() -> movie_id_tags().
movie_tags()->
    ParsedCsv = parse_csv(read_lines("data/movie-tags.csv")),
    lists:map(fun([Id,Tag]) ->
		      #movie_id_tag{
			 id=string:to_integer(Id),
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
		      lists:map(fun(X) -> list_to_float(X) end,
				string:tokens(Line, " ")) end, Lines).
