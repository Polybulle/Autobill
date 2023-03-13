(*Set Printing Existential Instances.*)
Require Import Lia.

Definition fuel__23 (x:nat) := 2 * x.
Definition add x y := x+y.
Definition one := 1.
Definition z := 0.

Ltac eparam v := evar (v : nat); eexists v.
Ltac setparam v := simpl; subst v; reflexivity.

Goal

(forall (t__68:nat) (t__322:nat),
 exists (t__381:nat) (t__404:nat) (t__471:nat) (t__518:nat) (t__851:nat),
     ((t__68 = t__322)
   /\ (t__381 = t__322)
   /\ (t__404 = (fuel__23 t__322))
   /\ (t__851 = (fuel__23 t__322)))
   -> ((forall (t__837:nat),
         (t__381 = z)
          -> (exists (t__102:nat) (t__103:nat) (t__405:nat) (t__406:nat) (t__433:nat) (t__833:nat) (t__836:nat) (t__834:nat) (t__835:nat),
                  ((t__404 = (add t__406 t__405))
                /\ (t__405 = t__834)
                /\ (t__406 = t__433)
                /\ (t__406 = t__835)
                /\ (t__433 = z)
                /\ (t__833 = z)
                /\ (t__834 = t__103)
                /\ (t__835 = t__102)
                /\ (t__836 = (add t__406 t__405)))
                /\ True))
      /\ (forall (t__114:nat) (t__472:nat) (t__849:nat) (t__850:nat),
          exists (t__577:nat) (t__655:nat) (t__848:nat),
              ((t__114 = t__472)
            /\ (t__381 = t__471)
            /\ (t__471 = (add t__472 one))
            /\ (t__848 = (fuel__23 t__655)))
            -> ((exists (t__138:nat) (t__139:nat) (t__144:nat) (t__150:nat) (t__151:nat) (t__519:nat) (t__520:nat) (t__547:nat) (t__548:nat) (t__575:nat) (t__576:nat) (t__840:nat)
                 (t__842:nat) (t__843:nat) (t__846:nat) (t__838:nat) (t__839:nat) (t__841:nat) (t__844:nat) (t__845:nat),
                     ((t__404 = t__518)
                   /\ (t__518 = (add t__520 t__519))
                   /\ (t__519 = t__844)
                   /\ (t__520 = t__547)
                   /\ (t__520 = t__845)
                   /\ (t__547 = (add t__548 one))
                   /\ (t__548 = t__575)
                   /\ (t__548 = t__841)
                   /\ (t__575 = (add t__577 t__576))
                   /\ (t__576 = t__838)
                   /\ (t__577 = t__839)
                   /\ (t__838 = t__139)
                   /\ (t__839 = t__138)
                   /\ (t__840 = (add t__577 t__576))
                   /\ (t__841 = t__144)
                   /\ (t__842 = (add t__548 one))
                   /\ (t__843 = one)
                   /\ (t__844 = t__151)
                   /\ (t__845 = t__150)
                   /\ (t__846 = (add t__520 t__519)))
                   /\ True)
               /\ (exists (t__175:nat) (t__847:nat),
                       ((t__655 = t__847)
                     /\ (t__847 = t__175))
                     /\     (((fuel__23 t__655) = t__577)
                          /\ (t__655 = t__472))))))).


intros.

eparam t__381.
eparam t__404.
eparam t__471.
eparam t__518.
eparam t__851.

intro H. decompose [and] H. clear H.

instantiate (t__381 := t__322); clear H2.
instantiate (t__404 := fuel__23 t__322); clear H1.
instantiate (t__851 := fuel__23 t__322); clear H4.

subst t__68.

split.

intros.


eparam t__102.
eparam t__103.
eparam t__405.
eparam t__406.
eparam t__433.
eparam t__833.
eparam t__836.
eparam t__834.
eparam t__835.

repeat split.

all: cycle 1.

setparam t__834.
setparam t__406.
setparam t__835.
setparam t__433.
setparam t__833.
setparam t__103.
setparam t__102.
setparam t__836.

intros.

eparam t__577.
eparam t__655.
eparam t__848.

intro H. decompose [and] H. clear H.


split.

eparam t__138.
eparam t__139.
eparam t__144.
eparam t__150.
eparam t__151.
eparam t__519.
eparam t__520.
eparam t__547.
eparam t__548.
eparam t__575.
eparam t__576.
eparam t__840.
eparam t__842.
eparam t__843.
eparam t__846.
eparam t__838.
eparam t__839.
eparam t__841.
eparam t__844.
eparam t__845.

repeat split.

setparam t__518.

all: cycle 1.
setparam t__519.
setparam t__520.
setparam t__845.
setparam t__547.
setparam t__548.
setparam t__841.
setparam t__575.
setparam t__576.
setparam t__839.
setparam t__838.
setparam t__138.
setparam t__840.
setparam t__144.

all:cycle 1.
setparam t__843.
setparam t__844.
setparam t__150.
setparam t__846.

eparam t__175.
eparam t__847.

repeat split.

setparam t__847.
setparam t__175.
setparam t__577.
setparam t__655.
setparam t__405.

all: cycle 1.

setparam t__842.

subst t__472.
subst t__381.
subst t__518.
subst t__404.
subst t__519.
subst t__520.
subst t__577.

(* important *)
instantiate (t__471 := t__322).
subst t__471.
subst t__322.



unfold add. unfold one. unfold fuel__23.


instantiate (t__139 := 1).
instantiate (t__151 := 0).
subst t__139.
subst t__151.
 lia.

Unshelve.
auto.

Qed.
