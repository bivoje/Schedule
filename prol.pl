
credit
lang_basic()

% [taken] is list of crsids
graduate(Taken) :-
  aggregate(sum(CR), C, (credit(C, CR), member(C, Taken)), CR_total),
  CR_total >= 130,

cresum(Criteria, X) :-
  aggregate(sum(CR), C, (credit(C, CR), Criteria), X)
