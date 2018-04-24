candidate_number(21611).

/*
* Part A
*/
q1(ailp_start_position(p(X,Y))).
q2a(new_pos(p(1,1), e, p(X,Y))).
q2b(109).
q3([s,e,w,n]).

/*
* Part B
*/
q4a( [p(4,4), p(4,3), p(4,2)] ).
q4b( [p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(4,2)] ).
q4c( [p(4, 2), p(4, 3), p(4, 4), p(3, 4), p(2, 4), p(1, 4), p(1, 3), p(2, 3), p(3, 3), p(3, 2), p(2, 2), p(1, 2), p(1, 1), p(2, 1), p(3, 1), p(4, 1)] ).
q4d( [p(4, 2), p(4, 3), p(4, 4), p(3, 4), p(2, 4), p(1, 4), p(1, 3), p(1, 2), p(1, 1), p(2, 1), p(2, 2), p(2, 3), p(3, 3), p(3, 2), p(3, 1), p(4, 1)] ).

/*
* Part C
* NOTE: the mower does move to the first position but doesn't animate it going there
* NOTE: should the first position aXways be the start position?
*/
q5_corner_move:-
  ailp_start_position(p(X,Y)),
  ailp_show_move(p(X,Y), p(1,1)),
  ailp_show_move(p(1,1), p(4,1)),
  ailp_show_move(p(4,1), p(4,4)),
  ailp_show_move(p(4,4), p(1,4)).

q5_corner_move2:-
  ailp_grid_size(N),
  ailp_start_position(p(X,Y)),
  ailp_show_move(p(X,Y), p(1,1)),
  ailp_show_move(p(1,1), p(N,1)),
  ailp_show_move(p(N,1), p(N,N)),
  ailp_show_move(p(N,N), p(1,N)).
