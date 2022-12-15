solution :-
    Tokens = ["X", "+", "(", "Y", "++", ")", "-", "Z"],
    infix_tree(Tokens, Tree),
    writeln(Tokens),
    writeln(Tree).

term("X").
term("Y").
term("Z").
op(uniry, "++").
op(binary, "+").
op(binary, "-").
open("(").
closing(")").

t("++", X, expr(X, "++")).
t("+", X, Y, expr("+", X, Y)).
t("-", X, Y, expr("-", X, Y)).
t("*", X, Y, expr("*", X, Y)).
t("/", X, Y, expr("/", X, Y)).

infix_tree(L, R) :-
    infix_tree(L, [], [], R).

infix_tree([], [R], [], R).
infix_tree([], [T0,T2|Terms], [Op|Ops], R) :-
    t(Op, T2, T0, Term),
    infix_tree([], [Term|Terms], Ops, R).
infix_tree([O|L], Terms, Ops, R) :-
    open(O), !,
    infix_tree(L, [O|Terms], Ops, R).
infix_tree([C|L], [Term|Terms], [Op|Ops], R) :-
    closing(C), op(uniry, Op), !,
    t(Op, Term, Term2),
    infix_tree(L, [Term2|Terms], Ops, R).
infix_tree([C|L], [Term,Term2|Terms], [Op|Ops], R) :-
    closing(C), op(binary, Op), !,
    t(Op, Term2, Term, Term0),
    infix_tree(L, [Term0|Terms], Ops, R).
infix_tree([Term|L], [], Ops, R) :-
    once(term(Term)), !,
    infix_tree(L, [Term], Ops, R).
infix_tree([Term|L], [O|Terms], Ops, R) :-
    once(( term(Term), open(O) )), !,
    infix_tree(L, [Term|Terms], Ops, R).
infix_tree([Term|L], [A|Terms], [Op|Ops], R) :-
    once(( term(Term), op(binary, Op) )), !,
    t(Op, A, Term, Term2),
    infix_tree(L, [Term2|Terms], Ops, R).
infix_tree([Term|L], Terms, [Op|Ops], R) :-
    once(( term(Term), op(uniry, Op) )), !,
    t(Op, Term, Term2),
    infix_tree(L, [Term2|Terms], Ops, R).
infix_tree([Op|L], Terms, Ops, R) :-
    once( op(_, Op) ), !,
    infix_tree(L, Terms, [Op|Ops], R).
