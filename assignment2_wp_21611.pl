% candidate_number(21611).

% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A):-
  (part_module(2)   -> find_identity_2(A)
  ; otherwise -> find_identity_o(A)
  ).

find_identity_2(A):-
  findall(Actor, actor(Actor), Actors),
  find_actor_identity(A, Actors).

find_actor_identity(A, Actors) :-
  agent_ask_oracle(oscar, o(1), link, L),
  include(link_on_page(L), Actors, NewActors),
  length(NewActors, Length),
  (Length = 1 -> nth0(0, NewActors, A);
   find_actor_identity(A, NewActors)
  ).

link_on_page(Link,A):-
  links_on_page(A, Links),
  member(Link, Links).

links_on_page(A, Links):-
  actor(A),
  setof(Link, (link(Link), wp(A,WT),wt_link(WT,Link)), Links).

%%%%%%%%%%%%%%%%%%%%%%% PART 3 %%%%%%%%%%%%%%%%%%%%%%%

find_identity_o(A):-
  findall(Actor, actor(Actor), Actors),
  find_actor_identity_o(A, Actors).

find_actor_identity_o(A, Actors) :-
  solve_task(find_next_oracle(o(X)), _),
  my_agent(Agent),
  query_world( agent_ask_oracle, [Agent, o(X), link, L]),
  include(link_on_page(L), Actors, NewActors),
  length(NewActors, Length),
  ( Length = 1 -> nth0(0, NewActors, A)
  ; find_actor_identity_o(A, NewActors)
  ).
