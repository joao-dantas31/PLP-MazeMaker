# MazeMaker

Link da especificação inicial do projeto: https://docs.google.com/document/d/1L7Gbb_QrNGnycIYpjV0Ga0rJYiwYXdilgonrp0vZhMw

## Contexto:

Infelizmente dos 5 integrantes apenas 2 contribuiram para o projeto, por isso ele estão mais simples que o especificado.

## Instalação:

### Bibliotecas utilizadas:

Nenhuma biblioteca adicional foi utilizada, apenas as já presente no SWI-Prolog.

## Executando:

### Windons

Basta rodar:

```
stack build --exec mazemaker-exe
```

## Explicação:

### Gerador:

Não tem muito o que falar, foi tudo pego de outro [projeto](https://github.com/CPSC-312-Haskell-Project/mazekell "Mazekell"), e a pessoa que pegou não faz a minima ideia de como funciona.

### Interface:

Foi utilizado o Gloss biblioteca e o seguimos este [tutorial](https://blog.jayway.com/2020/11/01/making-a-small-game-with-gloss/) como base.

A logica basica do projeto é transformar tudo em [Pictures](https://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Data-Picture.html), um objeto da biblioteca Gloss que carrega uma posição cartesiana e uma imagem, e desenhar tudo na janela do app.
