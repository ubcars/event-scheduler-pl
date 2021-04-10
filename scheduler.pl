:- use_module(library(apply)).
:- use_module(library(dicts)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).

% Query for a schedule from interface
ischedule(Days, Activities, Forecast, Schedule) :- % TODO: Use this line for debugging; remove line when we finish project
% ischedule(Schedule) :-
  write("Enter list of available days, in the form of days into the future (e.g. today is 0, tomorrow is 1), separated by spaces: "),
  flush_output(current_output),
  readln(Days),
  write("Enter list of activities, separated by spaces: "),
  flush_output(current_output),
  readln(Activities),
  write("Enter city where activities will take place: "),
  flush_output(current_output),
  readln(LocationInput),
  atomic_list_concat(LocationInput, '+', Location),
  forecast(Location, Forecast),
  schedule(Days, Activities, Forecast, Schedule).

% forecast(Location, Forecast) is true if Forecast is the weather forecast for Location
forecast(Location, Forecast) :-
  api_key(Key),
  atomic_list_concat(['https://api.weatherbit.io/v2.0/forecast/daily?', 'city=', Location, '&key=', Key], Url),
  http_get(Url, RawForecast, [json_object(dict)]),
  weather_objs_to_terms(RawForecast.data, Forecast).

% api_key(Key) retrieves Weatherbit API Key from config.json
api_key(K) :-
  open('config.json', read, Stream),
  json_read_dict(Stream, Config),
  close(Stream),
  K = Config.get('key').

/*
weather(Day, Pop, Precip, Temp, WindSpd) represents the forecast for the Day-th day into the future,
i.e. weather(0, ...) represents today's forecasted weather, weather(1, ...) represents tomorrow's, etc.

The remaining parameters directly correspond to parameters of the same name returned by Weatherbit's API
- Pop:     probability of precipitation, in %
- Precip:  amount of precipitation, in mm
           (0: no precipitation; 40: day with heaviest Vancouver rainfall in a typical year)
- Temp:    average temperature, in degrees Celsius
- WindSpd: wind speed, in m/s
           (0: no wind; 10: small trees sway; 20: very difficult to walk against wind)
*/

% weather_objs_to_terms(WeatherObjects, WeatherTerms) is true if WeatherTerms is a list of terms of the form
%  weather(Day, Pop, Precip, Temp, WindSpd), where the data for each term comes from each object in WeatherObjects
weather_objs_to_terms(WeatherObjects, WeatherTerms) :- weather_objs_to_terms(WeatherObjects, 0, WeatherTerms).

% weather_objs_to_terms(WeatherObjects, Index, WeatherTerms) is true if WeatherTerms is a list of terms of the form
%  weather(Day, Pop, Precip, Temp, WindSpd), where the data for each term comes from each object in WeatherObjects
%  and Index is the Day number for the head of the two lists
weather_objs_to_terms([], _, []).
weather_objs_to_terms([OH|OT], Index, [TH|TT]) :-
  weather_obj_to_term(OH, Index, TH),
  NextIndex is Index+1,
  weather_objs_to_terms(OT, NextIndex, TT).

% weather_obj_to_term(WeatherObject, Day, weather(Day, Pop, Precip, Temp, WindSpd)) is true if weather(Day, Pop, Precip, Temp, WindSpd)
%  is populated with the Day number and the remaining parameters are populated with the corresponding data from WeatherObject
weather_obj_to_term(WeatherObject, Day, weather(Day, Pop, Precip, Temp, WindSpd)) :-
  Pop     = WeatherObject.get(pop),
  Precip  = WeatherObject.get(precip),
  Temp    = WeatherObject.get(temp),
  WindSpd = WeatherObject.get(wind_spd).

% conflicts_list(SectionsList) is true if there exists a conflict in list of sections SectionsList
conflicts_list([S1, S2|_]) :- conflicts_pair(S1, S2).
conflicts_list([S1, _|T]) :- conflicts_list([S1|T]).
conflicts_list([_, S2|T]) :- conflicts_list([S2|T]).

% conflicts_pair(Section1, Section2) is true if there exists a conflict between sections Section1 and Section2
% This is a very rudimentary check
conflicts_pair(S1, S2) :-
  section(_, S1, Date1, Start1, _),
  section(_, S2, Date2, Start2, End2),
  Date1 == Date2,
  End2 > Start1,
  Start1 >= Start2.
conflicts_pair(S1, S2) :-
  section(_, S1, Date1, Start1, End1),
  section(_, S2, Date2, Start2, _),
  Date1 == Date2,
  End1 > Start2,
  Start2 >= Start1.

% sections_in_days(SectionCodes, Days) is true if every section identified by SectionCodes takes place on one of the Days
sections_in_days([], _).
sections_in_days([S|T], Days) :- section_in_days(S, Days), sections_in_days(T, Days).

% section_in_days(SectionCode, Days) is true if the section identified by SectionCode takes place on one of the Days
section_in_days(SectionCode, Days) :-
  section(_, SectionCode, Day, _, _),
  member(Day, Days).

% Filters out sections that are not suitable based on the weather each day
% True if S contains only suitable sections
filter_weathers([], _, _).
filter_weathers([W|T], A, S) :- filter_weather(W, A, S), filter_weathers(T, A, S).

% Determines available sections based on a day's weather
% True if only suitable sections remain
filter_weather(_, [], []).
filter_weather(weather(Day1, Pop, Precip, Temp, WindSpd), [Activity|R1], [Section|R2]) :-
  section(Activity, Section, Day2, _, _),
  dif(Day1, Day2),
  filter_weather(weather(Day1, Pop, Precip, Temp, WindSpd), R1, R2).
filter_weather(weather(Day, Pop, Precip, Temp, WindSpd), [Activity|R1], [Section|R2]) :-
  activity(_, Activity, (PopMin,PopMax), (PrecipMin,PrecipMax), (TempMin,TempMax), (WindSpdMin,WindSpdMax)),
  section(Activity, Section, Day, _, _),
  Pop    >= PopMin,
  PopMax >= Pop,
  Precip    >= PrecipMin,
  PrecipMax >= Precip,
  Temp    >= TempMin,
  TempMax >= Temp,
  WindSpd    >= WindSpdMin,
  WindSpdMax >= WindSpd,
  filter_weather(weather(Day, Pop, Precip, Temp, WindSpd), R1, R2).

% schedule(Days, Activities, Location, Schedule) is true if Schedule is a valid schedule for doing all Activities
%  some time during the selected Days under the forecasted weather at Location
% Note: There must exist a weather term in Weathers for each day in Days
schedule(_, [], _, _) :- writeln("No activity selected"), !, fail.
schedule(Days, Activities, Weathers, Sections) :-
  filter_weathers(Weathers, Activities, Sections),
  sections_in_days(Sections, Days),
  \+ conflicts_list(Sections).

% list_schedules(Days, Activities, Weathers, Schedules) is true if Schedules is the list of all valid schedules (schedules that satisfy all constraints)
%  which can be produced under the constraints given by Days, Activities, and Weather
list_schedules(Days, Activities, Weathers, Schedules) :- findall(Sections, schedule(Days, Activities, Weathers, Sections), Schedules).

% find_min(Schedules, MinSchedule) is true if MinSchedule is the schedule with the minimum total time commitment
find_min([Min], Min).
find_min([H,K|T], Min) :-
  sum(H,S1),
  sum(K,S2),
  S1 =< S2,
  find_min([H|T], Min).
find_min([H,K|T], Min) :-
  sum(H,S1),
  sum(K,S2),
  S1 > S2,
  find_min([K|T], Min).

% sum(SectionCodes, Duration) is true if the activities identified by SectionCodes spans a combined total of Duration hours
sum([], 0).
sum([H|T], S) :-
  sum(T, ST),
  section_duration(H, Duration),
  S is Duration+ST.

% section_duration(SectionCode, Duration) is true if the activity section identified by SectionCode spans Duration hours
section_duration(SectionCode, Duration) :-
  section(_, SectionCode, _, StartTime, EndTime),
  Duration is EndTime-StartTime.
  
  
  
% April 9 update: added schedule2 which outputs a list of recommended schedules based on the user's preferences. 
% Each schedule contains the maximum possible number of activities while satisfying the (maximum) time constraint and other constraints specified by the user. 
% schedule2(weather, time constraint, list of (N,T) where N is the minimum number of T type activity required, list of minimum activities required, list of schedules outputted)  
% For example, schedule2([weather(5, 5, 5, 5, 5), weather(6, 6, 6, 6, 6)], 7, [(4, Sport), (1, Leisure)], [hockey, football, volleyball], Schedules).   


% True if the schedule contains all desired activities 
must_contain([], _).
must_contain([Activity|T], Schedule) :- 
  must_contain_helper(Activity, Schedule), 
  must_contain(T, Schedule).

must_contain_helper(_, []) :- 
  false.
must_contain_helper(Activity, [Section|_]) :- 
  section(Activity, Section, _, _, _).
must_contain_helper(Activity, [_|T]) :- 
  must_contain_helper(Activity, T).

% True if the schedule contains at least N activities of type C
numeq([], _).
numeq([(N, C)|T], S) :- 
  numeq(N1, C, S), 
  N1 >= N, 
  numeq(T, S).

numeq(0,_, []).
numeq(N1, C, [H|T]) :- 
  activity(C, A, _, _, _, _), 
  section(A, H, _, _, _), 
  numeq(N, C, T), 
  N1 is N+1.

% List is true if contains all sections
all_sections(List) :- 
  findall(Sections, section(_, Sections, _, _, _), List).

% List is true if it produces all combinations of the sections (before being filtered by numeq and must_contain)
comb(S1, List, Types, Activities) :- 
  findall(S2, (comb_helper(S1, S2), numeq(Types, S2), must_contain(Activities, S2)), List).  

comb_helper([],[]).
comb_helper([H|T1],[H|T2]) :- 
  comb_helper(T1,T2).
comb_helper([_|T1],T2) :- 
  comb_helper(T1,T2).

% under_limit(S1, T, S2) is true if S2 contains every schedule that has a total time under the given time limit T
under_limit([], _ ,[]).
under_limit([Sections|R1], T, R2) :- 
  sum(Sections, Sum), 
  Sum > T, 
  under_limit(R1, T, R2).
under_limit([Sections|R1], T, [Sections|R2]) :- 
  sum(Sections, Sum), 
  Sum =< T, 
  under_limit(R1, T, R2).

% match(S, A) is true if A is a list of activities that correspond to S
match([], []).
match([Section|T1], [Activity|T2]) :- 
  activity(_, Activity, _, _, _, _), 
  section(Activity, Section, _, _, _), 
  match(T1, T2).

% valid(W, S1, S2) is true if S2 is a list of valid schedules.
valid(_, [], []).
valid(W, [S|T1], [S|T2]) :- 
  match(S, A), 
  \+ conflicts_list(S), 
  filter_weathers(W, A, S), 
  valid(W, T1, T2).
valid(W, [S|T1], T2) :- 
  match(S, A), 
  \+ filter_weathers(W, A, S), 
  valid(W, T1, T2).
valid(W, [S|T1], T2) :- 
  conflicts_list(S), 
  valid(W, T1, T2).

% True if Max is the schedule with the maximum number of sections
find_max([Max], Max).
find_max([H,K|T], Max) :- 
  length(H,Length1), 
  length(K,Length2), 
  Length1 >= Length2, 
  find_max([H|T], Max).
find_max([H,K|T], Max) :- 
  length(H,Length1), 
  length(K,Length2), 
  Length1 < Length2, 
  find_max([K|T], Max).

% same_num_sec(S1, Max, S2) is true if S2 contains every schedule that has the same number of sections as Max
same_num_sec([], _, []).
same_num_sec([S|T1], Max, [S|T2]) :- 
  length(Max, Length1), 
  length(S, Length2), 
  Length1 == Length2, 
  same_num_sec(T1, Max, T2). 
same_num_sec([S|T1], Max, T2) :- 
  length(Max, Length1), 
  length(S, Length2), 
  dif(Length1, Length2), 
  same_num_sec(T1, Max, T2). 

% Schedules is true if it is a list of valid schedules each with the maximum possible number of sections and a total time under or equal to T
% Each schedule includes the activities specified by Types and Activities
schedule2(W, T, Types, Activities, Schedules) :- 
  all_sections(S1), 
  comb(S1, S2, Types, Activities), 
  under_limit(S2, T, S3), 
  valid(W, S3, S4), 
  find_max(S4, Max), 
  same_num_sec(S4, Max, Schedules).  
  
  
  
/* Facts */

% activity(Type, Name, Pop, Precip, Temp, WindSpd) is an activity with a Type and Name, where
%  Pop, Temp, WindSpd, Precip are pairs of [min, max] values for each weather parameter
activity(Sport, hockey,     (5,10), (5,10), (5,10), (5,10)).
activity(Sport, football,   (1,10), (1,10), (1,10), (1,10)).
activity(Sport, volleyball, (5,10), (5,10), (5,10), (5,10)).
activity(Sport, soccer,     (5,10), (5,10), (5,10), (5,10)).
% Examples with somewhat realistic weather conditions
activity(Sport, indoor_ice_hockey, (0,100), (0,30), (-100,15), (0,20)).
activity(Sport, outdoor_badminton, (0,5), (0,1), (5,25), (0,1)).
activity(Sport, beach_volleyball,  (0,5), (0,1), (10,25), (0,5)).
activity(Leisure, sleep, (0,100), (0,1000), (-100,100), (0,100)).
activity(Leisure, picnic, (0,5), (0,1), (5,25), (0,1)).

% section(ActivityName, SectionCode, Day, StartTime, EndTime) is a schedulable section for activity ActivityName,
%  where the section has a unique SectionCode, is Day days into the future, and starts at StartTime and ends at EndTime on that day
% Note: the app produces garbage values when the Day is not in the scope of the forecast returned by the API
section(hockey,     "Hockey 100",     5, 3, 6).
section(hockey,     "Hockey 200",     6, 4, 6).
section(football,   "Football 100",   5, 6, 7).
section(football,   "Football 200",   6, 7, 9).
section(volleyball, "Volleyball 100", 5, 5, 6).
section(soccer,     "Soccer 100",     5, 5, 6).
% Examples with somewhat realistic weather conditions
section(indoor_ice_hockey, "Indoor Ice Hockey 0-13-14", 0, 13, 14).
section(indoor_ice_hockey, "Indoor Ice Hockey 0-13-15", 0, 13, 15).
section(beach_volleyball, "Beach Volleyball 0-14-15", 0, 14, 15).
section(beach_volleyball, "Beach Volleyball 0-14-16", 0, 14, 16).
section(sleep, "Sleep 0-0-23", 0, 0, 23).
section(sleep, "Sleep 0-12-15", 0, 12, 15).
section(sleep, "Sleep 1-0-1", 1, 0, 1).
section(sleep, "Sleep 1-12-15", 1, 12, 15).


/* Tests */

% conflicts_list(["Hockey 100", "Football 200"]).     Output: false
% conflicts_list(["Hockey 100", "Volleyball 100"]).   Output: true

% sections_in_days(["Hockey 100"], []).                      Output: false
% sections_in_days(["Hockey 100", "Football 100"], [4, 6]).  Output: false
% sections_in_days(["Hockey 100", "Football 100"], [5, 6]).  Output: true
% sections_in_days(["Hockey 100", "Football 200"], [5, 6]).  Output: true

% filter_weathers([weather(5, 5, 5, 5, 4), weather(6, 5, 5, 5, 4)], [hockey, football], S).   Output: false
% filter_weathers([weather(5, 5, 5, 5, 5), weather(6, 5, 5, 5, 4)], [hockey, football], S).   Output: S = ["Hockey 100", "Football 200"]; S = ["Hockey 100", "Football 100"]
% filter_weathers([weather(5, 5, 5, 5, 5), weather(6, 5, 5, 5, 5)], [hockey, football], S).   Output: S = ["Hockey 100", "Football 100"]; S = ["Hockey 100", "Football 200"]; S = ["Hockey 200", "Football 100"]; S = ["Hockey 200", "Football 200"]

% schedule([5,6], [hockey, football], [weather(5, 5, 5, 5, 4), weather(6, 5, 5, 5, 4)], S).   Output: false
% schedule([5,6], [hockey, football], [weather(5, 5, 5, 5, 4), weather(6, 5, 5, 5, 5)], S).   Output: S = ["Hockey 200", "Football 200"]; S = ["Hockey 200", "Football 100"]
% schedule([5,6], [hockey, football], [weather(5, 5, 5, 5, 5), weather(6, 5, 5, 5, 5)], S).   Output: S = ["Hockey 200", "Football 200"]; S = ["Hockey 200", "Football 100"]; S = ["Hockey 100", "Football 200"]; S = ["Hockey 100", "Football 100"]
% schedule([5],   [hockey, football], [weather(5, 5, 5, 5, 4), weather(6, 5, 5, 5, 4)], S).   Output: false
% schedule([5],   [hockey, football], [weather(5, 5, 5, 5, 5), weather(6, 5, 5, 5, 5)], S).   Output: S = ["Hockey 100", "Football 100"]
% schedule([6],   [hockey, football], [weather(5, 5, 5, 5, 5), weather(6, 5, 5, 5, 5)], S).   Output: S = ["Hockey 200", "Football 200"]
% schedule([4],   [hockey, football], [weather(4, 5, 5, 5, 5)], S).                           Output: false
% schedule([4,7], [hockey, football], [weather(4, 5, 5, 5, 5), weather(5, 5, 5, 5, 5), weather(6, 5, 5, 5, 5), weather(7, 5, 5, 5, 5)], S).   Output: false

% list_schedules([4],   [hockey, football], [weather(4, 5, 5, 5, 5)], Schedules).                           Output: false
% list_schedules([5],   [hockey, football], [weather(5, 5, 5, 5, 5), weather(6, 5, 5, 5, 5)], Schedules).   Output: Schedules = [["Hockey 100", "Football 100"]]
% list_schedules([5,6], [hockey, football], [weather(5, 5, 5, 5, 5), weather(6, 5, 5, 5, 5)], Schedules).   Output: Schedules = [["Hockey 200", "Football 200"], ["Hockey 200", "Football 100"], ["Hockey 100", "Football 200"], ["Hockey 100", "Football 100"]]

% sum(["Hockey 200", "Football 200"], Sum).   Output: Sum = 4

% find_min([["Hockey 100", "Football 100"], ["Hockey 100", "Football 200"], ["Hockey 200", "Football 100"], ["Hockey 200", "Football 200"]], Min).   Output: Min = ["Hockey 200", "Football 100"]
