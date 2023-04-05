 qfLes news sur autobill:
- Il n'y a plus qu'un seul exécutable, nommé 'autobill'. Il prend en entrée un
  code CBPV par défaut, et un code machine avec l'option '-M'
- Le code CBPV est transformé proprement, chaque définition au toplevel devent
  une définition au toplevel dans la machine.
- Les messages d'erreurs sont uniformisés, et les erreurs sont disponibles au
  format JSON sur la sortie d'erreur standard avec l'option '-j'
- Nouvelles possibilités d'exports:
  - La contrainte logique '-C' sur un programme (existait déjà)
  - La contrainte logique au format Coq avec '-q'
  - Une contrainte d'optimisation avec '-a' au format minizinc, qui peut être
    résolue par le solveur classique. Cette dernière possibilité n'existe que
    pour les programmes machine qui utilise la déclaration "goal" dans le
    toplevel. C'est encore expérimental
