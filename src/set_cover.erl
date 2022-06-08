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
brute_force(_S) -> [].

brute_force_test() ->
    % empty universe
    [] = brute_force(#{}),

    % all done
    ok.