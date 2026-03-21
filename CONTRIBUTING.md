


### Estructura del Proyecto - parte de funcional
haskell/
├── src/
│   ├── Sudoku.hs      # Módulo principal (re-exports)
│   ├── Types.hs       # Tipos de datos (Board, Cell, Position)
│   ├── Solver.hs      # Algoritmo de backtracking
│   └── Utils.hs       # Parsing, I/O, ejemplos
├── app/
│   └── Main.hs        # Aplicación CLI
├── test/
│   └── Spec.hs        # Tests con HSpec
├── package.yaml       # Configuración de paquete
├── stack.yaml         # Configuración de Stack
└── README.md
## Instalación
Prerequisitos
Instalar Stack (gestor de proyectos Haskell):
bash# Linux/macOS
curl -sSL https://get.haskellstack.org/ | sh

# macOS (Homebrew)
brew install haskell-stack

# Windows
# Descargar de: https://docs.haskellstack.org/en/stable/install_and_upgrade/
Build del Proyecto
bashcd haskell

# Primera vez (descarga GHC y dependencias)
stack build

# Ejecutar tests
stack test

# Ejecutar aplicación
stack run
## Uso
Modo Interactivo
bashstack run
Muestra un menú para seleccionar ejemplos (fácil, medio, difícil).
Ejemplos desde CLI
bash# Resolver ejemplo fácil
stack run -- --example easy

# Resolver ejemplo medio
stack run -- --example medium

# Resolver ejemplo difícil
stack run -- --example hard

# Ejecutar benchmarks
stack run -- --benchmark
Desde Archivo
bash# Formato: 81 dígitos (0 o . para vacío)
echo "530070000600195000..." > puzzle.txt
stack run -- --file puzzle.txt
Desde GHCi (REPL)
bashstack ghci

> import Sudoku
> let board = exampleEasy
> prettyBoard board
5 3 . | . 7 . | . . .
6 . . | 1 9 5 | . . .
. 9 8 | . . . | . 6 .
---------------------
...

> case solve board of Just s -> prettyBoard s
5 3 4 | 6 7 8 | 9 1 2
6 7 2 | 1 9 5 | 3 4 8
...





### Requisitos - parte de logica

SWI-Prolog versión 8.0 o superior
Sistema operativo: Linux, macOS, o Windows

## Instalación de SWI-Prolog
Ubuntu/Debian:
bashsudo apt-add-repository ppa:swi-prolog/stable
sudo apt-get update
sudo apt-get install swi-prolog
macOS (con Homebrew):
bashbrew install swi-prolog
Windows:
Descarga el instalador desde: https://www.swi-prolog.org/Download.html
Verificar instalación:
bashswipl --version
## Uso
Versión con CLP(FD) (Recomendada)
bash# Iniciar SWI-Prolog
swipl

# Cargar el archivo
?- [sudoku_clpfd].

# Resolver un sudoku
?- sudoku([
    [_,_,_,_,_,_,_,_,_],
    [_,_,_,_,_,3,_,8,5],
    [_,_,1,_,2,_,_,_,_],
    [_,_,_,5,_,7,_,_,_],
    [_,_,4,_,_,_,1,_,_],
    [_,9,_,_,_,_,_,_,_],
    [5,_,_,_,_,_,_,7,3],
    [_,_,2,_,1,_,_,_,_],
    [_,_,_,_,4,_,_,_,9]
], Solution).
Versión Manual (Sin CLP)
bashswipl

?- [sudoku_manual].

# Usar lista plana de 81 elementos
?- sudoku([4,x,x,x,6,x,9,1,x,
           2,x,x,x,x,7,x,5,x,
           x,9,x,8,x,x,x,2,x,
           x,x,1,6,x,9,x,x,2,
           x,8,x,x,x,x,x,6,3,
           x,7,x,x,4,x,x,x,x,
           7,x,3,x,x,8,x,9,x,
           x,x,x,x,3,x,4,x,5,
           x,4,x,9,x,x,6,x,x], Solucion).
Ejecutar Tests
bashswipl

?- [tests].
?- run_all_tests.

### DEPENDENCIES
Tambien es necesario instalar matplotlib (con pip) y alguna libreria mas de estadisticas/graficos.
