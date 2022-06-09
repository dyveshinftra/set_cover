% Set Cover Problem
%
% Example:
%   U = {1, 2, 3, 4, 5}
%   S = {{1, 2, 3}, {2, 4}, {3, 5}, {4, 5}}
%   C = {{1, 2, 3}, {4, 5}}
%
% Given a set of elements (the universe U) and a family of subsets of U (S), a
% cover (C) is a subset of S whose union matches the universe.
%
% Finding the cover with the fewest sets, the optimization problem, is NP-hard.

-module(set_cover).
-export([brute_force/1]).
-include_lib("eunit/include/eunit.hrl").

% brute-force search algorithm
brute_force(M) ->
    U = lists:usort(lists:flatten(maps:values(M))),

    % start brute-force search algorithm
    brute_force(U, M, [], [], maps:keys(M)).

% found a cover
brute_force(U, _, C, CKeys, _) when length(U) == length(C) -> CKeys;

% no cover found
brute_force(_, S, _, _, []) -> maps:keys(S);

% try with and without next subset
brute_force(U, S, C, CKeys, [H|T]) ->
    % with subset from S
    CKeys1 = brute_force(U, S, lists:usort(C ++ maps:get(H,S)), [H|CKeys], T),

    % without subset from S
    CKeys2 = brute_force(U, S, C, CKeys, T),

    % prefer smallest cover
    case length(CKeys1) < length(CKeys2) of
        true -> CKeys1;
        false -> CKeys2
    end.

brute_force_test() ->
    % empty universe
    [] = brute_force(#{}),

    % example universe
    [one,four] = brute_force(#{one=>[1,2,3], two=>[2,4], three=>[3,5], four=>[4,5]}),

    % Pokémon Go
    [steel,ice,ground,grass,ghost,flying,fighting] = brute_force(#{
        bug         =>  [dark,grass,psychic],
        dark        =>  [psychic,ghost],
        dragon      =>  [dragon],
        electric    =>  [water,flying],
        fairy       =>  [fighting,dragon,dark],
        fighting    =>  [normal,ice,rock,dark,steel],
        fire        =>  [grass,ice,bug,steel],
        flying      =>  [grass,fighting,bug],
        ghost       =>  [psychic,ghost],
        grass       =>  [water,ground,rock],
        ground      =>  [fire,electric,poison,rock,steel],
        ice         =>  [grass,ground,flying,dragon],
        normal      =>  [rock,steel],
        poison      =>  [grass,fairy],
        psychic     =>  [fighting,poison],
        rock        =>  [fire,ice,flying,bug],
        steel       =>  [ice,rock,fairy],
        water       =>  [fire,ground,rock]}),

    % all done
    ok.