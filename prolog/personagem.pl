:- module(personagem, []).
:- use_module(library(pce)).

:- pce_begin_class(personagem, object).

variable(line, int, get,    "Linha do personagem").
variable(column, int, get,    "Coluna do personagem").
variable(circle, object, get,    "Circulo que representa o personagem").

initialise(D) :->
    send_super(D, initialise),
    send(D, slot, line, 0),
    send(D, slot, column, 0).
    

:- pce_end_class.