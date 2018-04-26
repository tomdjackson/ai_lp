candidate_number(21611).

% solve_task(Task,Cost):-
%   my_agent(Agent),
%   query_world( agent_current_position, [Agent,P] ),
%   solve_task_bt(Task,[c(0,P),P],0,R,Cost,_NewPos),!,  % prune choice point for efficiency
%   reverse(R,[_Init|Path]),
%   query_world( agent_do_moves, [Agent,Path] ).

solve_task(go(X),Cost):-
  my_agent(Agent),
  check_energy(Agent),
  write("GOING TO "),
  X = p(I,J),
  write("("), write(I), write(","), write(J), write(")"),nl,
  query_world( agent_current_position, [Agent,P] ),
  solve_task_a_star(go(X),[[c(0,0,P),P]],R,Cost,_NewPos,[]),!,
  reverse(R,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ).

solve_task(find(o(X)),Cost):-
  my_agent(Agent),
  check_energy(Agent),
  write("GOING TO ORACLE"),nl,
  query_world( agent_current_position, [Agent,P] ),
  solve_task_breadth(find(o(X)),[[c(0,0,P),P]],R,Cost,_NewPos,[]),!,
  reverse(R,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ).

solve_task(find_next_oracle(o(X)),Cost):-
  my_agent(Agent),
  check_energy(Agent),
  write("GOING TO NEXT ORACLE"),nl,
  query_world( agent_current_position, [Agent,P] ),
  solve_task_breadth(find_next_oracle(o(X)),[[c(0,0,P),P]],R,Cost,_NewPos,[]),!,
  reverse(R,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ).

solve_task(find(c(X)),Cost):-
  my_agent(Agent),
  write("GOING TO CHARGING STATION"),nl,
  query_world( agent_current_position, [Agent,P] ),
  solve_task_breadth(find(c(X)),[[c(0,0,P),P]],R,Cost,_NewPos,[]),!,
  reverse(R,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ),
  query_world( agent_topup_energy, [Agent, c(X)]).

check_energy(Agent):-
  write("CHECKING ENERGY"),nl,
  query_world(agent_current_energy, [Agent, Energy]),
  write("ENERGY = "),
  write(Energy),nl,
  ( Energy > 50 -> true
  ; otherwise -> solve_task(find(c(_)), A)
  ).

solve_task_a_star(Task,[Current|_],R,Costs,NewPos,_) :-
  Current = [c(_,G,P)|RPath],
  Costs = [cost(Cost), depth(G)],
  achieved(Task, [c(G,P)|RPath], R, Cost, NewPos).
solve_task_a_star(go(Goal),[Current|Agenda],RR,Cost,NewPos,Visited) :-
  Current = [c(_,G,P)|RPath],
  ( setof(Child, a_star_child(Goal, P, RPath, G, Child, Visited), Children) -> append(Agenda, Children, NewAgenda)
  ; NewAgenda = Agenda
  ),
  remove_visited_from_agenda(NewAgenda, Visited, FinalAgenda),
  solve_task_a_star(go(Goal),FinalAgenda,RR,Cost,NewPos,[P|Visited]).

remove_visited_from_agenda(Agenda, [], Agenda).
remove_visited_from_agenda(Agenda, [P|Tail], NewAgenda) :-
    delete(Agenda, [c(_,_,P)|_], NewAgenda),
    remove_visited_from_agenda(NewAgenda, Tail, _).

a_star_child(Goal, P, RPath, G, Child, Visited) :-
  search(P, P1, R, _),
  \+ memberchk(R, RPath),
  \+ memberchk(P1, Visited),
  map_distance(P1, Goal, H),
  G1 is G+1,
  F is G1+H,
  Child = [c(F, G1, P1), P1|RPath].

solve_task_breadth(Task, [Current|_], R, Costs, NewPos, _):-
  Current = [c(_,G,P)|RPath],
  Costs = [cost(Cost), depth(G)],
  achieved(Task, [c(G,P)|RPath], R, Cost, NewPos).
solve_task_breadth(Task, [Current|Agenda], RR, Cost, NewPos, Visited):-
  Current = [c(_,G,P)|RPath],
  (setof(Child, breadth_child(P, RPath, G, Child, Visited), Children) -> append(Agenda, Children, NewAgenda)
  ; NewAgenda = Agenda
  ),
  remove_visited_from_agenda(NewAgenda, Visited, FinalAgenda),
  solve_task_breadth(Task,FinalAgenda,RR,Cost,NewPos,[P|Visited]).

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
achieved(find_next_oracle(O), Current, RPath, Cost, NewPos) :-
  Current = [c(Cost, NewPos)|RPath],
  my_agent(Agent),
  ( O=none -> true
  ; otherwise -> RPath = [Last|_], map_adjacent(Last,_,O), write("ORACLE FOUND, I = "), write(O), nl,
  \+ query_world(agent_check_oracle, [Agent, O ])
  ).
achieved(find(O),Current,RPath,Cost,NewPos) :-
  Current = [c(Cost,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

search(F,N,N,1) :-
  map_adjacent(F,N,empty).
