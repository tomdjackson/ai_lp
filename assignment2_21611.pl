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
  solve_task_a_star(Task,[[c(0,0,P),P]],R,Cost,_NewPos,[]),!,
  reverse(R,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ).

solve_task_a_star(Task,[Current|_],R,Costs,NewPos,_) :-
  Current = [c(_,G,P)|RPath],
  Costs = [cost(Cost), depth(G)],
  achieved(Task, [c(G,P)|RPath], R, Cost, NewPos).
solve_task_a_star(go(Goal),[Current|Agenda],RR,Cost,NewPos,Visited) :-
  Current = [c(_,G,P)|RPath],
  (
    setof(Child, a_star_child(Goal, P, RPath, G, Child, Visited), Children) -> append(Agenda, Children, NewAgenda);
    NewAgenda = Agenda
  ),
  remove_visited(NewAgenda, Visited, FinalAgenda),
  solve_task_a_star(go(Goal),FinalAgenda,RR,Cost,NewPos,[P|Visited]).
solve_task_a_star(Task,[Current|Agenda],RR,Cost,NewPos, Visited) :-
    Current = [c(_,G,P)|RPath],
    (
      findall(Child, breadth_child(P, RPath, G, Child, Visited), Children) -> append(Agenda, Children, NewAgenda);
      NewAgenda = Agenda
    ),
    remove_visited(NewAgenda, Visited, FinalAgenda),
    solve_task_a_star(Task,FinalAgenda,RR,Cost,NewPos,[P|Visited]).

remove_visited(Agenda, [], Agenda).
remove_visited(Agenda, [P|Tail], NewAgenda) :-
    delete(Agenda, [c(_,_,P)|_], NewAgenda),
    remove_visited(NewAgenda, Tail, _).

a_star_child(Goal, P, RPath, G, Child, Visited) :-
  search(P, P1, R, _),
  \+ memberchk(R, RPath),
  \+ memberchk(P1, Visited),
  map_distance(P1, Goal, H),
  G1 is G+1,
  F is G1+H,
  Child = [c(F, G1, P1), P1|RPath].

breadth_child(P, RPath, G, Child, Visited) :-
  search(P, P1, R, _),
  \+ memberchk(R, RPath),
  \+ memberchk(P1, Visited),
  G1 is G+1,
  Child = [c(G1, G1, P1), P1|RPath].

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
