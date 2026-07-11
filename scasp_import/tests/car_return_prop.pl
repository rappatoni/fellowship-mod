%% Example from: https://personal.utdallas.edu/~gupta/nfm-ex/
%% Author: Gopal Gupta (2025)    %%

% Scenario: Taken from Stanford Philosophy Encylopaedia, Deontic Logic entry
% John is obligated to return his friend Xavier's car,
% and the least he can do is return it by noon with
% the same level of battery charge it had when he borrowed
% it. It is permissible to not maintain the same battery
% level if the car needs a battery change.


% Following facts are involved in creating a narrative
  % borrowed_car. 
  % car_returned.
  % returned_car_before_noon.
  % same_battery_level.
  % financially_broke.
  % sick.
  % needBatteryChange.
  % wrecked_car.
% To create a narrative, set some of the facts above as true,
% load the program, and issue a query (the default query ?- true
% can show the possible answer sets entailed.
% Seven example narratives are shown below. To try other narratives
%      comment-out Narrative 1 and uncomment other narratives. 

% Narrative 1: wrecked the car: could not keep any obligation.
wrecked_car. borrowed_car.

% Narrative 2: car returned before noon with same battery level; met all obligations
% returned_car_before_noon. 
% borrowed_car. same_battery_level. 

% Narrative 3: car returned late after 12 due to being sick, but battery level ok.
% car_returned. sick.
% borrowed_car. same_battery_level. 

% Narrative 4: car returned before noon, but with low battery due to being broke to get it charged.
% returned_car_before_noon.
% borrowed_car. financially_broke.

% Narrative 5: car returned but late and with low battery levels due to being too broke to get it charged.
% car_returned. sick.
% borrowed_car. financially_broke.

% Narrative 6: car returned before noon, low battery levels, but battery needs replacement (so financially broke has no impact)
% returned_car_before_noon. needBatteryChange.
% borrowed_car. financially_broke. 

% Narrative 7: car returned before noon, low battery levels, but battery needs replacement (so financially broke has no impact)
% returned_car_before_noon. needBatteryChange.
% borrowed_car. -financially_broke. 

% if car returned before noon, then car returned
car_returned :- returned_car_before_noon.

% car can be returned, only if borrowed.
:- car_returned, not borrowed_car.


%  modeling permissibility
battery_ok_to_return :- same_battery_level, 
        not abnormal_battery_status.
battery_ok_to_return :- 
        needBatteryChange.
abnormal_battery_status :- needBatteryChange.

% Main obligation of returning the car as an OLON rule
fail_to_return_car :- 
    not car_returned,
    borrowed_car,
    not fail_to_return_car.

% Secondary obligation to return before noon
fail_to_return_by_noon :- 
    not returned_car_before_noon,
    car_returned,
    borrowed_car,
    not fail_to_return_by_noon.

% Secondary obligation to return with battery not depleted
fail_to_return_ok_battery :- 
    not battery_ok_to_return, 
    car_returned, 
    borrowed_car,
    not fail_to_return_ok_battery.

% if the car is wrecked, we can't return the car
fail_to_return_car  :- borrowed_car, wrecked_car.

% if become sick, then cannot return car before noon
fail_to_return_by_noon :- borrowed_car, sick.
% if fail to return car, then fail to return before noon
fail_to_return_by_noon :- fail_to_return_car. 

% if don't have money, cannot charge depleted battery
fail_to_return_ok_battery :- 
    borrowed_car, financially_broke, not needBatteryChange.
% if fail to return car, then failed to return with ok battery
fail_to_return_ok_battery :- fail_to_return_car.




% Queries
?- true.                   % Models depends on the narratives...