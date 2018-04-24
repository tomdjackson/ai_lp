candidate_number(21611).

% solve_task(Task,Cost):-
%   my_agent(Agent),
%   query_world( agent_current_position, [Agent,P] ),
%   solve_task_bt(Task,[c(0,P),P],0,R,Cost,_NewPos),!,  % prune choice point for efficiency
%   reverse(R,[_Init|Path]),
%   query_world( agent_do_moves, [Agent,Path] ).

solve_task(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  solve_task_a_star(Task,[[c(, 0, P),P]], R,Cost,_NewPos),!,  % prune choice point for efficiency
  reverse(R,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ).

%%%%%%%%%% A* Search %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve_task_a_star(Task, [Current|_], R, Costs, NewPos):-
  Current = [Current1|RPath],
  Costs = [cost(Cost), depth(Depth)],
  achieved(Task, Current, RPath, Cost, NewPos),
solve_task_a_star(Task, Agenda, RPath, Cost, NewPos):-
  Agenda = [Current|Tail],
  Cost = [c(_, G, P1)|RPath],
  Task = go(p(X,Y)),
  write("hello"),
  setof(Child, create_child(Child, p(X,Y), Task, RPath, G), Children),
  append(Agenda, Children, NewAgenda),
  solve_task_a_star(Task, NewAgenda, RPath, Cost, NewPos).

create_child(Child, P, Task, RPath, G):-
  write(P),
  search(P,P1,P1,_),
  \+memberchk(P1, RPath),
  map_distance(P, P1, H),
  write(H),
  G1 is G+1,
  F is G+H,
  Child = [c(F, G1, P1)|RPath].

%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtracking depth-first search, needs to be changed to agenda-based A*
solve_task_bt(Task,Current,Depth,RPath,[cost(Cost),depth(Depth)],NewPos) :-
  achieved(Task,Current,RPath,Cost,NewPos).
solve_task_bt(Task,Current,D,RR,Cost,NewPos) :-
  Current = [c(F,P)|RPath],
  search(P,P1,R,C),
  \+ memberchk(R,RPath),  % check we have not been here already
  D1 is D+1,
  F1 is F+C,
  solve_task_bt(Task,[c(F1,P1),R|RPath],D1,RR,Cost,NewPos).  % backtrack search

achieved(go(Exit),Current,RPath,Cost,NewPos) :-
  Current = [c(Cost,NewPos)|RPath],
  ( Exit=none -> true
  ; otherwise -> RPath = [Exit|_]
  ).
achieved(find(O),Current,RPath,Cost,NewPos) :-
  Current = [c(Cost,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

search(F,N,N,1) :-
  map_adjacent(F,N,empty).
