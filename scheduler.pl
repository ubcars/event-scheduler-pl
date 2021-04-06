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
  
  
  
  
  
% Some ideas I've added. 
  

% Checks for conflicts within a list of sections
conflicts([S1,S2|_]) :- conflicts(S1, S2).
conflicts([S1,_|T]) :- conflicts([S1|T]).
conflicts([_,S2|T]) :- conflicts([S2|T]).

% Checks if two sections conflict
% This is a very rudimentary check
conflicts(S1, S2) :- section(_, S1, Date1, Start1, _), section(_, S2, Date2, Start2, End2),  Date1 == Date2, End2 > Start1, Start1 >= Start2. 
conflicts(S1, S2) :- section(_, S1, Date1, Start1, End1), section(_, S2, Date2, Start2, _),  Date1 == Date2, End1 > Start2, Start2 >= Start1.  


% filters a list of sections by a list of days
filterDays(_,[]).
filterDays(S, [D|T]) :- filterDay(S, D), filterDays(S, T).

% inputs a list of sections and a day to be filtered
filterDay([], _).
filterDay([S|T], Day) :- section(_, S, D, _, _), dif(D, Day), filterDay(T, Day).


% Filters out sections that are not suitable based on the weather each day
filterWeathers([], _, _).
filterWeathers([W|T], A, S) :- filterWeather(W, A, S), filterWeathers(T, A, S).

% Determines available sections based on a day's weather 
filterWeather(_, [], []).
filterWeather(weather(Day1, Pop, Temp, Wind_spd, Precip),[Activity|R1],[Section|R2]) :- section(Activity, Section, Day2, _, _), dif(Day1, Day2), filterWeather(weather(Day1, Pop, Temp, Wind_spd, Precip), R1, R2).
filterWeather(weather(Day, Pop, Temp, Wind_spd, Precip), [Activity|R1], [Section|R2]) :- activity(_, Activity, [Po1,Po2], [T1,T2], [W1,W2], [Pr1,Pr2]), section(Activity, Section, Day, _, _), Pop >= Po1, Po2 >= Pop, Temp >= T1, T2 >= Temp, Wind_spd >= W1, W2 >= Wind_spd, Precip >= Pr1, Pr2 >= Precip, filterWeather(weather(Day, Pop, Temp, Wind_spd, Precip), R1, R2).   


% Outputs an optimal schedule based on constraints.
% So far the constrains include weather and user selected days.
schedule([], _) :- writeln("No activity selected"), !, fail.
schedule(Weather, Activities, Sections, Days) :- filterWeathers(Weather, Activities, Sections), filterDays(Sections, Days), \+ conflicts(Sections).



% Facts

% activity(Type, Name, Pop, Temp, Wind_spd, Precip). 
activity("Sport", "Hockey", [5,10], [5,10], [5,10], [5,10]).
activity("Sport", "Football", [1,10], [1,10], [1,10], [1,10]).
activity("Sport", "Volleyball", [5,10], [5,10], [5,10], [5,10]).

% section(Name, SecCode, Date, Start_Time, End_Time).
section("Hockey", "Hockey 100", 5, 4, 6).
section("Hockey", "Hockey 200", 6, 4, 6).
section("Football", "Football 100", 5, 6, 7).
section("Football", "Football 200", 6, 7, 8).
section("Volleyball", "Volleyball 100", 5, 5, 6).
section("Soccer", "Soccer 100", 5, 5, 6).

% weather(Day, Pop, Temp, Wind_spd, Precip).
weather(1, [5,10], [6,11], [7,13], [1,4]).
weather(1, [3,8], [4,15], [9,13], [1,4]).
weather(2, [5,10], [6,11], [7,13], [1,4]).



% Tests

% conflicts(["Hockey 100", "Football 200"]). 	            Output: false
% conflicts(["Hockey 100", "Volleyball 100"]). 	          Output: true

% filterDays(["Hockey 100", "Football 200"], [5, 6]).     Output: false
% filterDays(["Hockey 200", "Football 100"], [4, 6]).     Output: false
% filterDays(["Hockey 100", "Football 100"], [4, 6]).     Output: true  
 
% filterWeathers([weather(4, 5, 5, 5, 5), weather(4, 5, 3, 5, 5)], ["Hockey", "Football"], S).           Output: S = ["Hockey 100", "Football 100"]; S = ["Hockey 100", "Football 200"] ; S = ["Hockey 200", "Football 100"]; S = ["Hockey 200", "Football 200"] 
% filterWeathers([weather(5, 5, 5, 5, 5), weather(6, 5, 3, 5, 5)], ["Hockey", "Football"], S).           Output: S = ["Hockey 100", "Football 200"]; S = ["Hockey 100", "Football 100"] 
% filterWeathers([weather(5, 5, 3, 5, 5), weather(6, 5, 3, 5, 5)], ["Hockey", "Football"], S).           Output: false

% schedule([weather(5, 5, 5, 5, 5), weather(6, 5, 5, 5, 5)], ["Hockey", "Football"], Sections, [4,7]).   Output: S = ["Hockey 200", "Football 200"]; S = ["Hockey 200", "Football 100"] ; S = ["Hockey 100", "Football 200"]; S = ["Hockey 100", "Football 100"]
% schedule([weather(5, 5, 5, 5, 5), weather(6, 5, 5, 5, 5)], ["Hockey", "Football"], Sections, [5]).     Output: Sections = ["Hockey 200", "Football 200"]
% schedule([weather(5, 5, 5, 5, 5), weather(6, 5, 3, 5, 5)], ["Hockey", "Football"], Sections, [3]).     Output: Sections = ["Hockey 100", "Football 200"]; Sections = ["Hockey 100", "Football 100"]
% schedule([weather(5, 5, 5, 5, 5), weather(6, 5, 3, 5, 5)], ["Hockey", "Football"], Sections, [6]).     Output: Sections = ["Hockey 100", "Football 100"]
% schedule([weather(5, 5, 5, 5, 5), weather(6, 5, 5, 5, 5)], ["Hockey", "Football"], Sections, [5,6]).   Output: false
  
