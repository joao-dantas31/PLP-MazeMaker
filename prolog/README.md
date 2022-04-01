# MazeMaker

Link da especificação inicial do projeto: https://docs.google.com/document/d/1L7Gbb_QrNGnycIYpjV0Ga0rJYiwYXdilgonrp0vZhMw

## Contexto:

Infelizmente dos 5 integrantes apenas 1 contribuiram para o projeto, por isso ele estão mais simples que o especificado.

## Instalação:

### Bibliotecas utilizadas:

Nenhuma biblioteca adicional foi utilizada, apenas as já presente no SWI-Prolog.

## Executando:

### Windons

Basta rodar no SWI-Prolog:

```
working_directory(CWD, 'diretorio dos arquivos .pl').
[maze].
main.
```

### Linux

Não tive tempo de testar no linux, mas creio que seja só rodar o `maze.pl`

## Explicação:

### Gerador:

Basicamente é usada uma DFS para gerar um grafo aleatório a partir de uma célula aleatória, e com isso garantimos que o labirinto gerado sempre tem uma solução.

### Interface:

Foi utilizado a biblioteca [XPCE](https://www.swi-prolog.org/packages/xpce/) pra gerar a interface visual. No começo um grid é gerado com todas as linhas do labirinto, e a medida que o algoritmo de DFS vai caminhando pelo labirinto ele vai apagando as linhas entre as célula.
