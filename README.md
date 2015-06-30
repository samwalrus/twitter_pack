This is a pack to make application searches of twitter.

You need to go to 
https://apps.twitter.com/ and make an app with a consumer key and consumer secret.

You then need to edit key_and_secret.pl.example to have the correct values
and rename that file to key_and_secret.pl

At the moment you then need to create a new zip file 'twitter-0.10.zip' 
then you can use pack_install/1

To use :

use_module(library(twitter)).
get_bearer_token(Json,Token,Error).

%This this asserts token/1

Then you can make a search. e.g. searching for tweets with the
word 'walrus' is done as so: 

token(T),make_a_search('walrus',T,Json,ErrorCode).

Json is a dict with the response from twitter.
