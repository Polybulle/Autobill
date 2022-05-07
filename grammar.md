# Generalities 

Variables induced by user-defined types are:

 - type constructors $\text{tcons}$ and `tcons`. 

There are three kinds of boxes:

- Linear, written $\mathbf{lin}$ or `boxed`
- Affine, written $\mathbf{aff}$ or `affine`
- Exponential, written $\mathbf{exp}$ or `exp`

We use variable $K$ and `K` for kinds of boxes in this document. Users can't use them.

# Types

Types are sorted as follows:

- $a,b,c$ are types
- $p,q$ are datatypes (positive types)
- $n,m$ are computation types (negative types)

Grammar for $t$:

- $a$ or `{a}` are type variables 
- $p^+$ or `{pos p}` or just `{p}` are datatypes 
- $n^-$ or `{neg n}` or just `{n}` are computation types 

Grammar for $p$:

- $a^+$ or `{+a}` are datatype variables 
- $\square_K t$ or `{box K t}` are boxed types
- $\text{tcons}(a_1,\dots,a_n)$ or `{tcons a1...}` are datatypes 

Grammar for $n$:

- $a^-$ or `{~a}` are computation type variables 
- computation types go as datatypes. Users explicitly resolve which `tcons` are positive or negative at definition. 

# ILL Type constructors 
- Units:
  - The unit type (neutral for products) is $\mathbb{1}$ and `{unit}`
  - The zero type (neutral for sums) is $\mathbb{0}$ and `{zero}`
  - The top type (neutral for choices) is $\top$ and `{top}`
  - The bottom type is $\bot$ and `{bottom}`
- Combinators:
  - Pairs are written $a \otimes b$ and `{prod a b}`
  - Sums are written $a \oplus b$ and `{sum a b}`
  - Functions are $a \multimap b$ and `{fun a b}`
  - Choices are $a \& b$ and `{choice a b}`
  
# L-machine 
  
- Grammar of values $V$ and `V`:
  - Variables $x$ and `x`
  - binders $\mu \alpha : a^-.c$ and `(let !alpha {n} c)`
  - boxes $\mu \square_K \alpha : a.c$ and `(box K !alpha a c)`
  - constructors $\text{cons}^+(V_1,\dots,V_n)$ and `(cons V1...)`
  - co-destruction $\mu( \text{cons}^- (\alpha;x_1,\dots,x_n).c \dots )$ and `(match (cons !alpha x1...) c...)`
- Grammar for stacks $S$ and `S`:
  - Co-variables $\alpha$ and `!alpha`
  - Co-binders $\mu x:a^+.c$ and `(let x {p} c)`
  - Un-boxing $\square S$ and `(unbox K S)`
  - Destructors $\text{cons}^- (\alpha;V_1\dots;V_n)$ and `(cons S V1...)`
  - Co-constructors $\mu( \text{cons}^+(x_1,\dots,x_n).c \dots )$ and `(match (case (cons x1...) c)...)`
- Terms $t$, `t` are values and forces $\phi \alpha : a^+.c$ and `(force !\alpha {p} c)`
- Environments $e$, `e` are stacks and co-forces $\phi x: a^-.c$ `(force x {n} c)` 
- Commands are either $\left< t || S \right>^+$, `(jump t S)` or $\left< V || e \right>^-$, `(call V e)`.

# ILL constructors
- for pairs, $x \otimes y$ and `(pair x y)`
- for sums, $\text{left}(x), \text{right}(y)$ and `(left x)`, `(right y)`
- for functions, $x :: \alpha$ and `(call !alpha x)`
- for choices, $\text{yes}(\alpha), \text{no}(\beta)$ and `(yes !\alpha)`, `(no !\beta)`
- for the units, same as the types

# Prelude

```
decl sort my_sort;
decl rel my_rel : sort1 * ... * sortN;
decl type my_type : sort;

type {my_type {a1 sort1} ... {aN sortN}} : sort = ...;

data {my_type {a1 sort1} ... {aN sortN}}  =
  | cons1 of t1 * ... * tn 
  | ...
end

codata {my_type {a1 sort1} ... {aN sortN}}  =
  | cons1 of t1 * ... * tn cont tcont 
  | ...
end

term my_term : type = ...;

env my_env : type = ...;

cmd my_command = ...;
``` 

# TODO 

```
pack $T_{pack}$ $(\alpha_{1} : k_{1})\dots(\alpha_{n} : k_{n})$ = $K$ $(\beta_{1} : l_{1})$...$(\beta_{m} : l_{m})$ of $A$ with $E$;
spec $T_{spec}$ $(\alpha_{1} : k_{1})\dots(\alpha_{n} : k_{n})$ = $K$ $(\beta_{1} : l_{1})$...$(\beta_{m} : l_{m})$ cont $B$ with $E$;

indexed data $T_{idata}$ $(\alpha_{1} : k_{1})\dots(\alpha_{n} : k_{n})$ =
  | $K$ $(\beta_{1} : l_{1})$...$(\beta_{m} : l_{m})$ of $A_{K,1}$*...*$A_{K,q}$ with $E$
  | ...
end
indexed codata $T_{icod}$ $(\alpha_{1} : k_{1})$...$(\alpha_{n} : k_{n})$ =
  | $K$ $(\beta_{1} : l_{1})$...$(\beta_{m} : l_{m})$ of $A_{H,1}$*...*$A_{H,q}$ cont $B_{H}$ with $E$
  | ...
end
```
