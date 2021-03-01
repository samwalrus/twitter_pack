:- module(twitter,
         [token/1,
          get_bearer_token/5,
          make_a_search/4,
          get_friends_list/4,
          get_user/4,
          get_user_following/4,
          get_tweet/4]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_session)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_files)).
:- use_module(library(http/json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).

:- dynamic
	token/1.

bearer_token_credentials(Key,Secret,B_Token):-
        format(atom(B_Token),"~w:~w",[Key,Secret]).

get_bearer_token(Key,Secret,JSON,Token,ErrorCode):-
	bearer_token_credentials(Key,Secret,B_Token),
	base64(B_Token,B_Token64),
	format(atom(My_Auth),"Basic ~w",[B_Token64]),
	ListofData =[grant_type=client_credentials],
        http_open('https://api.twitter.com/oauth2/token', In,
                  [ request_header(authorization=My_Auth),status_code(ErrorCode),
		    method(post),post(form(ListofData))
                  ]),
	call_cleanup(json_read_dict(In, JSON),
	close(In)),
	Token = JSON.access_token,
	assertz(token(Token)).


make_a_search(My_Search,B_Token64,JSON,ErrorCode):-
	Path='/1.1/search/tweets.json',
	Search=[q(My_Search)],
	get_json(Path, Search, B_Token64, JSON, ErrorCode).

get_friends_list(Username, B_Token64, JSON, ErrorCode) :-
    % DEPRECATED: Twitter API v1.1 https://developer.twitter.com/en/docs/twitter-api/v1/accounts-and-users/follow-search-get-users/api-reference/get-friends-list
    get_friends_list_at_cursor(Username, B_Token64, -1, JSON, ErrorCode).

get_friends_list_at_cursor(Username, B_Token64, Cursor, JSON, ErrorCode) :-
    Cursor=\=0,
    Path='/1.1/friends/list.json',
    Search=[screen_name=Username, count=200, cursor=Cursor],
    get_json(Path, Search, B_Token64, Json0, ErrorCode0),
    (   % succeed for current
        JSON = Json0,
        ErrorCode = ErrorCode0
    ;   % succeed for next
        _{next_cursor:NextCursor}:<Json0,
        get_friends_list_at_cursor(Username, B_Token64, NextCursor, JSON, ErrorCode)
    ).

get_user_following(UserId, B_Token64, JSON, ErrorCode) :-
    % Twitter API v2 https://developer.twitter.com/en/docs/twitter-api/users/follows/introduction
    get_user_following(UserId, [], B_Token64, JSON, ErrorCode).

get_user_following(UserId, Options, B_Token64, JSON, ErrorCode) :-
    number(UserId),
    !,
    format(atom(Path), '/2/users/~w/following', [UserId]),
    Search=[max_results=1000 | Options],
    get_json(Path, Search, B_Token64, Json0, ErrorCode0),
    (   % succeed for current
        JSON=Json0,
        ErrorCode=ErrorCode0
    ;   % if there is a pagination token, succeed for next
        _{meta:Metadata}:<Json0,
        _{next_token:PaginationToken}:<Metadata,
        NextOptions=[pagination_token=PaginationToken],
        get_user_following(UserId, NextOptions, B_Token64, JSON, ErrorCode)
    ).

get_tweet(TweetId, B_Token64, JSON, ErrorCode) :-
    number(TweetId),
    format(atom(Path), '/2/tweets/~w', [TweetId]),
    Search=[expansions=author_id],
    get_json(Path, Search, B_Token64, JSON, ErrorCode).

get_user(UserId, B_Token64, JSON, ErrorCode) :-
    number(UserId), !, 
    format(atom(Path), '/2/users/~w', [UserId]),
	Search=['user.fields'=description],
	get_json(Path, Search, B_Token64, JSON, ErrorCode).

get_user(Username, B_Token64, JSON, ErrorCode) :-
    atom(Username),
    uri_encoded(path, Username, EncodedUsername),
    format(atom(Path), '/2/users/by/username/~w', [EncodedUsername]),
	Search=['user.fields'=description],
	get_json(Path, Search, B_Token64, JSON, ErrorCode).


get_json(Path, Search, B_Token64, JSON, ErrorCode) :-
	URL=[scheme(https), host('api.twitter.com'), path(Path), search(Search)],
	Options=[ authorization(bearer(B_Token64)),
			status_code(ErrorCode)
			],
	setup_call_cleanup(http_open(URL, In, Options),
					   json_read_dict(In, JSON),
					   close(In)).
