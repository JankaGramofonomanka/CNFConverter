# Projekt 3.
Projekt polega na napisaniu konwertera formuł logicznych (bez kwantyfikatorów) 
do postaci CNF. Format wyjściowy może być w postaci dostosowanej dla 
sat-solvera ```cadical``` lub w formie czytelnej dla człowieka.

## Wymagania:
```
ghc 8.0.2
```

## Kompilacja
```
$ make
```

## Przyładowe użycie
Konwersja formuły i wypisanie w konsoli:
```
$ ./tocnf "(p ==> q) <=> (not p or q)"
```

Konwersja formuły "przekazanie" sat-solverowi:
```
$ ./tocnf "(p ==> q) <=> (not p or q)" | path/to/cadical
```

Wypisanie w czytelnym dla człowieka formacie:
```
$ ./tocnf "(p ==> q) <=> (not p or q)" --human-friendly
```

Konwersja zaprzeczenia formuły
```
$ ./tocnf "(p ==> q) <=> (not p or q)" --contradict
```

Wczytanie z pliku i zapisanie w pliku
```
$ ./tocnf -o putput.bf -f input.bf
```

## Więcej opcji
```
$ ./tocnf --help
```
