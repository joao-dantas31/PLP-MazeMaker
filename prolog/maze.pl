:- dynamic visited/2, pass/4, win/2.
:- use_module(library(pce)).
:- ['./personagem.pl'].

% Prepara os dialogos e depois chama ao metodo de iniciar
main :-
    print('Quais as dimencoes?'),
    read(X),

    new(D, window('Maze')),

    new(Per, personagem), % Novo personagem, que carrega a posição atual e o circulo que representa o mesmo
    new(P, circle(20)),
    send(P, fill_pattern, colour(green)),
    send(Per,slot,circle,P),

    new(H, dialog('Controles')),

    send(H, append(button(esquerda, message(@prolog, moviment, D, Per, e, X)))),
    send(H, append(button(cima, message(@prolog, moviment,  D, Per, c, X)))),
    send(H, append(button(direita, message(@prolog, moviment,  D, Per, d, X)))),
    send(H, append, button(baixo, message(@prolog, moviment,  D, Per, b, X)), below),

    send(H, open),

    start(X, D, Per).

% Inicia/Reinicia o jogo
start(X, D, Per) :-
    retractall(win(_,_)), 

    maze(X, X, D),
    
    WinCell is X-1,
    assert(win(WinCell, WinCell)),

    get(Per,slot,circle,P),
    send(D, display, P, point(15,15)),

    send(Per,slot,line,0),
    send(Per,slot,column,0).

% Chamado toda vez que o usuario tenta se movimentar
moviment(D, Per, Dir, X) :-
    get(Per,slot,line,L),
    get(Per,slot,column,C),
    get(Per,slot,circle,Cir),

    next_move(Dir, L, C, NL, NC),
    (pass(L, C, NL, NC) ->
        XL is NL * 30 + 15,
        YL is NC * 30 + 15,
        send(D, display, Cir, point(YL, XL)),

        (win(NL, NC) ->
            start(X, D, Per)
        ;
            send(Per,slot,line,NL),
            send(Per,slot,column,NC)
        )
    ).

% Com base no movimento escolhido e a posição atual, a nova posição é localizada
next_move(e, L, C, L, NC) :-
    NC is C - 1. 
next_move(d, L, C, L, NC) :-
    NC is C + 1. 
next_move(b, L, C, NL, C) :-
    NL is L + 1. 
next_move(c, L, C, NL, C) :-
    NL is L - 1. 
next_move(_, L, C, L, C).

% Geração do labirinto

maze(Lig, Col, D) :-
    retractall(visited(_,_)),     % Apaga todas as celulas visitadas
    retractall(pass(_, _, _, _)), % Apaga todas as 'passagens' abertas
    
    % Refaz todas as paredes
    forall(between(0,Lig, I),
            (XL is  10, YL is I * 30 + 10,
        XR is Col * 30 + 10,
        new(L, line(XL, YL, XR, YL)),
        send(D, display, L))),
     
    forall(between(0,Col, I),
            (XT is  10 + I * 30, YT is 10,
        YB is Lig * 30 + 10,
        new(L, line(XT, YT, XT, YB)),
        send(D, display, L))),
     
    SX is Col * 30 + 20,
    SY is Lig * 30 + 20,
    
    new(G, circle(20)),
    send(D, display, G, point(SX - 35, SY - 35)),
    send(G, fill_pattern, colour(orange)),

    send(D, size, new(_, size(SX, SY))),
     
        
    % Escolhe a primeira celula aleatoriamente e comeca a DFS a partir dela
    L0 is random(Lig),
    C0 is random(Col),
    assert(visited(L0, C0)),
    \+search(D, Lig, Col, L0, C0),
        
    send(D, open).

% Busca e destroi as paredes recursivamente, usando o algoritmo de Busca em profundidade(DFS)
search(D, Lig, Col, L, C) :-
    Dir is random(4),
    nextcell(Dir, Lig, Col, L, C, L1, C1),
    assert(visited(L1,C1)),
    erase_line(D, L, C, L1, C1),
    search(D, Lig, Col, L1, C1).

% Apaga uma linha entre duas celulas da mesma linha
erase_line(D, L, C, L, C1) :-
    (C < C1 -> C2 = C1; C2 = C),
    XT is C2  * 30 + 10,
    YT is L * 30 + 11,
    YR is (L+1) * 30 + 10,
    new(Line, line(XT, YT, XT, YR)),
    send(Line, colour, white),
    send(D, display, Line),
    make_way(L, C, L, C1).

% Apaga uma linha entre duas celulas da mesma coluna
erase_line(D, L, C, L1, C) :-
    (L < L1 -> L2 is L1; L2 is L),
    XT is  11 + C * 30,
    YT is L2 * 30 + 10,
    XR is 10 + (C + 1) * 30,
    new(Line, line(XT, YT, XR, YT)),
    send(Line, colour, white),
    send(D, display, Line),
    make_way(L, C, L1, C).

% Garante que o personagem pode passar entre duas celulas
make_way(L1, C1, L2, C2) :-
    assert(pass(L1, C1, L2, C2)),
    assert(pass(L2, C2, L1, C1)).
     
% Metodo responsavel pelo 'backtracking' na formacao 
nextcell(Dir, Lig, Col, L, C, L1, C1) :-
    next(Dir, Lig, Col, L, C, L1, C1);
    (   
        Dir1 is (Dir+1) mod 4,
        next(Dir1, Lig, Col, L, C, L1, C1)
    );
    (   
        Dir2 is (Dir+2) mod 4,
        next(Dir2, Lig, Col, L, C, L1, C1)
    );
    (   
        Dir3 is (Dir+3) mod 4,
        next(Dir3, Lig, Col, L, C, L1, C1)
    ).
     
% Retorna a celula acima de outra, caso esta ainda não tenha sido visitada.
next(0, _Lig, _Col, L, C, L1, C) :-
    L > 0,
    L1 is L - 1,
    \+visited(L1, C).
     
% Retorna a celula a direita de outra, caso esta ainda não tenha sido visitada.
next(1, _Lig, Col, L, C, L, C1) :-
    C < Col - 1,
    C1 is C + 1,
    \+visited(L, C1).
     
% Retorna a celula abaixo de outra, caso esta ainda não tenha sido visitada.
next(2, Lig, _Col, L, C, L1, C) :-
    L < Lig - 1,
    L1 is L + 1,
    \+visited(L1, C).
     
% Retorna a celula esquerda de outra, caso esta ainda não tenha sido visitada.
next(2, _Lig, _Col, L, C, L, C1) :-
    C > 0,
    C1 is C - 1,
    \+visited(L, C1).