-module(markov).
-export([run/3]).

run(State, Params, Prog) ->
	case choose_next_state(State, Prog) of
		false ->
			[{State, Params}];
		{State, Func} ->
			{NewState, NewParams} = Func(Params),
			[{State, Params}|run(NewState, NewParams, Prog)]
	end.

choose_next_state(State, Prog) ->
	case lists:sum([ Likelihood || {NewState, Likelihood, _} <- Prog, State == NewState ]) of
		0 -> false;
		TotalLikelihoods ->
			Choices =
				[ {NewState, Likelihood / TotalLikelihoods, Func} ||
				  {NewState, Likelihood, Func} <- Prog, State == NewState ],
		pick_random(rand:uniform_real(), Choices)
	end.

pick_random(X, [{NewState, Likelihood, Func}|_]) when X < Likelihood -> {NewState, Func};
pick_random(X, [{_, Likelihood, _}|Rest]) -> pick_random(X - Likelihood, Rest).
