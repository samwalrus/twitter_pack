:- module(twitter,
         [token/1,
          get_bearer_token/3,
          make_a_search/4]).

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
:- use_module(key_and_secret).

:- dynamic
	token/1.

bearer_token_credentials(B_Token):-
	consumer_key(Key),
	consumer_secret(Secret),
        format(atom(B_Token),"~w:~w",[Key,Secret]).

get_bearer_token(JSON,Token,ErrorCode):-
	bearer_token_credentials(B_Token),
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
	format(atom(My_Auth),"Bearer ~w",[B_Token64]),
	URL0='https://api.twitter.com/1.1/search/tweets.json',
	url_extend(search([q(My_Search)]),URL0,URL),
	http_open(URL, In,
                  [ request_header(authorization=My_Auth),
		    status_code(ErrorCode)

                  ]),
	call_cleanup(json_read_dict(In, JSON),
	close(In)).


url_extend(search(Params), URL0, URL) :-
	uri_components(URL0, Components0),
	uri_data(search, Components0, Search0),
	extend_search(Search0, Params, Search),
	uri_data(search, Components0, Search, Components),
	uri_components(URL, Components).

extend_search(Var, Params, String) :-
	var(Var), !,
	uri_query_components(String, Params).
extend_search(String0, Params, String) :-
	uri_query_components(String0, Params0),
	append(Params0, Params, AllParams),
	uri_query_components(String, AllParams).
