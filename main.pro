% Copyright

implement main
    open core

domains
    integerTree =
        tree(integer, integerTree, integerTree);
        nil.

class predicates
    startTask : (integer, integerTree [out]).
    iterationTask : (integer, integer, integerTree, integerTree [out]).
    add : (integer, integerTree, integerTree [out]).
    writeTree : (integerTree).
    heightTree : (integerTree, integer [out]).
    maxHeight : (integer, integer, integer [out]).
    maxElement : (integerTree, integer [out]) determ.
    minElement : (integerTree, integer [out]) determ.

clauses
    add(X, nil, tree(X, nil, nil)).

    add(X, tree(E, L, R), tree(E, NewL, R)) :-
        X < E,
        !,
        add(X, L, NewL).

    add(X, tree(E, L, R), tree(E, L, NewR)) :-
        add(X, R, NewR).

    startTask(N, FindBinTree) :-
        console::write('Start input elements of tree:\n'),
        iterationTask(1, N, nil, FindBinTree).

    iterationTask(I, N, TmpTree, FindBinTree) :-
        I <= N,
        !,
        X = stdio::read(),
        console::clearInput(),
        add(X, TmpTree, TmpFindBinTree),
        II = I + 1,
        iterationTask(II, N, TmpFindBinTree, FindBinTree).

    iterationTask(_, _, FindBinTree, FindBinTree).

    writeTree(nil).

    writeTree(tree(E, L, R)) :-
        stdio::write(E, ' '),
        writeTree(L),
        writeTree(R).

    heightTree(nil, 0) :-
        !.

    heightTree(tree(_E, L, R), Height) :-
        heightTree(L, LeftHeight),
        heightTree(R, RightHeight),
        maxHeight(LeftHeight, RightHeight, MaxHeight),
        Height = MaxHeight + 1.

    maxHeight(Left, Right, Left) :-
        Left > Right,
        !.

    maxHeight(_Left, Right, Right).

    maxElement(tree(E, _L, nil), MaxEl) :-
        MaxEl = E,
        !.

    maxElement(tree(_E, _L, R), MaxEl) :-
        maxElement(R, MaxEl).

    minElement(tree(E, nil, _R), MinEl) :-
        MinEl = E,
        !.

    minElement(tree(_E, L, _R), MinEl) :-
        minElement(L, MinEl).

    run() :-
        console::init(),
        startTask(10, FindBinTree),
        console::write('Built tree:\n'),
        writeTree(FindBinTree),
        console::write('\nHeight of tree:\n'),
        heightTree(FindBinTree, Height),
        console::write(Height),
        console::write('\nMax element in tree:\n'),
        maxElement(FindBinTree, MaxEl),
        console::write(MaxEl),
        console::write('\nMin element in tree:\n'),
        minElement(FindBinTree, MinEl),
        console::write(MinEl),
        _ = console::readLine(),
        fail
        or
        succeed.
        % place your own code here

end implement main

goal
    console::runUtf8(main::run).
