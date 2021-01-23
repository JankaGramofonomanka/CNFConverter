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
konwersja formuły i wypisanie w konsoli:
```
$ ./tocnf "(p ==> q) <=> (not p or q)"
```

konwersja formuły "przekazanie" sat-solverowi:
```
$ ./tocnf "(p ==> q) <=> (not p or q)" | path/to/cadical
```

wypisanie w czytelnym dla człowieka formacie:
```
$ ./tocnf "(p ==> q) <=> (not p or q)" --human-friendly
```


## Więcej opcji
```
$ ./tocnf --help
```
