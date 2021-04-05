:- use_module(library(apply)).
:- use_module(library(dicts)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).

% Query for a schedule from interface
ischedule(Schedule) :-
  write("Enter list of participants, separated by spaces: "),
  flush_output(current_output),
  readln(Participants),
  write("Enter list of activities, separated by spaces: "),
  flush_output(current_output),
  readln(Activities),
  write("Enter city where activities will take place: "),
  flush_output(current_output),
  readln(LocationInput),
  atomic_list_concat(LocationInput, '+', Location),
%  write("Enter how many days into the future you would like to schedule events (max 16): "),
%  flush_output(current_output),
%  readln(Days),
  schedule(Participants, Activities, Location, Schedule).

% schedule(Participants, Activities, Location, S) gives an optimal schedule S for Participants to take part in Activities at Location
% TODO: schedule(P, A, L, S) :- ...
% Calling ischedule(S) currently fails here, as expected, with 'Unknown procedure: schedule/4'


% available(P, [0,1,...]) is true if person P is busy tomorrow, available the day after tomorrow, etc.
available(aa, [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]).
available(bb, [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]).
available(cc, [0,1,1,0,1,1,1,0,0,1,1,1,0,1,1,0]).
available(dd, [0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1]).

% activity(A,C) is true if A is an activity with weather constraints C
activity(bbq,    [lt(pop, 5), gt(temp, 15), lt(wind_spd, 5)]).
activity(beach,  [lt(pop, 5), gt(temp, 15), lt(wind_spd, 10)]).
activity(camp,   [lt(precip, 10), gt(temp, 10), lt(wind_spd, 10)]).
activity(dinner, []).
activity(ski,    [lt(temp, 5), lt(wind_spd, 15)]).


/* TODO: Please read
Everything *above* here is incomplete.
The stuff you see is just the result of my brainstorming.
I do not know if available() or activity() will be useful at all; I think that's going to depend
 on how we implement the schedule predicate.
Some points I want to discuss:
- I called the things to schedule 'activities' because after thinking through it, I felt an 'activity'
 is more influenced by the weather than an 'event' is. Your thoughts?
- I think I like the idea of defining the constraints that each activity has (like I have defined here),
 then asking the user via an interface (in ischedule()) for the list of activities they would like to
 schedule (so that not all activities have to always be scheduled). I don't think this would make
 the app any more complex to implement, but it feels like a nice-to-have from a user's perspective.

Everything *below* here works fine IMO.
This part takes care of the fetching and cleaning up of the data we get from the Weatherbit API.
By calling forecast(L,F), the app is able to fetch forecast F for location L from Weatherbit's 16-day forecast API.
I was playing around in the command line to get this to work, so I wrote some predicates to minify
 the response object (so that, you know, my terminal doesn't get too cluttered).
 I don't know yet if we'll need them when implementing the actual scheduling, but it's there for now.
 You can try forecast("New+York", F) to see what I mean. F should be a list of objects, where
 each object only contains the properties we are interested in.
*/

% forecast(Location, Forecast) is true if Forecast is the minified weather forecast for Location
forecast(L, MinF) :-
  api_key(K),
  atomic_list_concat(['https://api.weatherbit.io/v2.0/forecast/daily?', 'city=', L, '&key=', K], Url),
  http_get(Url, RawF, [json_object(dict)]),
  minify_l(RawF.data, prop, MinF).

% api_key(K) retrieves Weatherbit API key from config.json
api_key(K) :-
  open('config.json', read, Stream),
  json_read_dict(Stream, Config),
  close(Stream),
  K = Config.get('key').

/*
Relevant weather parameters
- pop:      probability of precipitation, in %
- precip:   amount of precipitation, in mm
- temp:     average temperature, in degrees Celsius
- wind_spd: wind speed, in m/s
*/
% prop(P) is true if P is a valid weather property used in the app's weather constraints
% Property names should correspond to those returned by Weatherbit's weather forecast API
prop(pop).
prop(precip).
prop(temp).
prop(wind_spd).

% minify_l(SrcList, PickBy, OutList) is true if list of dicts OutList is InList
%  but with each dict containing only the properties satisfying the PickBy predicate
minify_l([], _, []).
minify_l([IH|IT], P, [OH|OT]) :-
  minify_s(IH, P, OH),
  minify_l(IT, P, OT).

% minify_s(InDict, PickBy, OutDict) is true if dict OutDict is InDict
%   but with only the properties satisfying the PickBy predicate
minify_s(ID, P, OD) :-
  dict_keys(ID, IKeys),
  include(P, IKeys, OKeys),
  foldl(copyprop(ID), OKeys, _{}, OD).

% copyprop(SrcDict, Key, InDict, OutDict) is true if dict OutDict is dict InDict
%  but with the property identified by Key copied from SrcDict
copyprop(S, K, ID, OD) :-
  OD = ID.put(K, S.get(K)).
  
  
  
  
  
Some ideas I've added.  
  
%activity(Type, Name). 
activity("Sport", "Hockey").
activity("Sport", "Football").
activity("Sport", "Volleyball").

%section(Name, SecCode, Date, Start_Time, End_Time).
section("Hockey", "Hockey 100", 5, 4, 6).
section("Hockey", "Hockey 200", 6, 4, 6).
section("Football", "Football 200", 5, 5, 6).
section("Football", "Football 300", 6, 7, 8).
section("Volleyball", "Volleyball 300", 5, 5, 6).
section("Soccer", "Soccer 300", 5, 5, 6).

% Checks that the activities exist
valid_activities([]).
valid_activities([Name|T]) :- activity(_, Name), valid_activities(T).

% Is true if the list does not contain redundant elements
set([]).
set([E|T]) :-
   maplist(dif(E), T),
   set(T).


% This is true of a list of activities matches to a list of sections
match([],[]).
match([Activity|T1], [Section|T2]) :- activity(_, Activity), section(Activity, Section, _, _, _), match(T1,T2).


% Checks for conflicts in a list of sections
conflicts([S1,S2|_]) :- conflicts(S1, S2).
conflicts([S1,_|T]) :- conflicts([S1|T]).
conflicts([_,S2|T]) :- conflicts([S2|T]).

% Checks if 2 sections conflict
% This is a very rudimentary check
conflicts(S1, S2) :- section(_, S1, Date1, Start1, _), section(_, S2, Date2, Start2, End2),  Date1 == Date2, End2 > Start1, Start1 >= Start2. 
conflicts(S1, S2) :- section(_, S1, Date1, Start1, End1), section(_, S2, Date2, Start2, _),  Date1 == Date2, End1 > Start2, Start2 >= Start1.  


% This outputs a schedule that avoids conflicts if such a schedule exists. Unfortunately, this leads to an infinite loop? if no such schedule exists.
% This is a rough draft so please feel free to modify this! If this seems ok, we can try to modify/merge it with the activity function you wrote above that has the weather constraints. 
schedule([], _) :- writeln("No activity selected"), !, fail.
schedule(Activities, Sections) :- valid_activities(Activities), set(Activities), set(Sections), match(Activities, Sections), \+ conflicts(Sections).
  
