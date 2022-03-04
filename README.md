# MazeMaker

Link da especificação inicial do projeto: https://docs.google.com/document/d/1L7Gbb_QrNGnycIYpjV0Ga0rJYiwYXdilgonrp0vZhMw

## Contexto:

Infelizmente dos 5 integrantes apenas 2 contribuiram para o projeto, por isso ele estão mais simples que o especificado.
E um dos integrantes acabou apenas plagiando um código de gerador de labirintos. (https://github.com/CPSC-312-Haskell-Project/mazekell)
Durante a apresentação eu (João Paulo Alves Dantas) esclareci todos esses pontos com o professor, e ele pediu pra que fosse colocado isso aqui.

## Instalação:

### Bibliotecas utilizadas:
```
- gloss
- random
- unordered-containers
```

### Pacotes instalados no sistema:

Talvez seja necessario instalar alguns pacotes para que o Gloss consigo rodar.

#### No Windons (Usando o mingw64):

```
pacman -S mingw64/mingw-w64-x86_64-freeglut
```

#### No linux:

```
sudo apt-get install freeglut3-dev
```

## Executando:

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
