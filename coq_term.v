(* Set Printing Existential Instances. *)
Require Import Lia.

Definition fuel__23 (x:nat) := x.
Definition add x y := x+y.
Definition one := 1.
Definition z := 0.

Goal (forall (t__65:nat) (t__268:nat),
 exists (t__323:nat) (t__346:nat) (t__365:nat) (t__412:nat) (t__637:nat),
     ((t__65 = t__268)
   /\ (t__323 = t__268)
   /\ (t__346 = (fuel__23 t__268))
   /\ (t__637 = (fuel__23 t__268)))
   -> ((forall (t__102:nat) (t__366:nat) (t__635:nat) (t__636:nat),
        exists (t__413:nat) (t__455:nat) (t__634:nat),
            ((t__102 = t__366)
          /\ (t__323 = t__365)
          /\ (t__365 = (add t__366 one))
          /\ (t__634 = (fuel__23 t__455)))
          -> ((exists (t__121:nat) (t__631:nat) (t__632:nat) (t__630:nat),
                   ((t__346 = t__412)
                 /\ (t__412 = (add t__413 one))
                 /\ (t__413 = t__630)
                 /\ (t__630 = t__121)
                 /\ (t__631 = (add t__413 one))
                 /\ (t__632 = one))
                 /\ True)
             /\ (exists (t__145:nat) (t__633:nat),
                     ((t__455 = t__633)
                   /\ (t__633 = t__145))
                   /\     (((fuel__23 t__455) = t__413)
                        /\ (t__455 = t__366)))))
      /\ (forall (t__629:nat),
           (t__323 = z)
            -> True))) .

firstorder.

evar (t__323 : nat). eexists t__323.
evar (t__346 : nat). eexists t__346.
evar (t__365 : nat). eexists t__365.
evar (t__412 : nat). eexists t__412.
evar (t__637 : nat). eexists t__637.
firstorder.

instantiate (t__323 := t__268).
instantiate (t__346 := fuel__23 t__268).
instantiate (t__637 := fuel__23 t__268).

evar (t__413 : nat). exists t__413.
evar (t__455 : nat). exists t__455.
evar (t__634 : nat). exists t__634.

firstorder.

instantiate (t__634 := fuel__23 t__455).

evar (t__121 : nat). exists t__121.
evar (t__631 : nat). exists t__631.
evar (t__632 : nat). exists t__632.
evar (t__630 : nat). exists t__630.

firstorder.
subst t__412; reflexivity.

2: subst t__630; reflexivity.
2: subst t__121; reflexivity.
2: subst t__631; reflexivity.
2: subst t__632; reflexivity.

2: {
  evar (t__145 : nat). exists t__145.
  evar (t__633 : nat). exists t__633.
  repeat split.

  subst t__633; reflexivity.
  subst t__145; reflexivity.
  2: subst t__455; reflexivity.

  subst t__455 t__413; reflexivity.
}

unfold t__412.
unfold t__346.
unfold add.
unfold one.
unfold fuel__23.
unfold t__413.


subst t__323.
subst t__365.
subst t__346.
subst t__412 t__637.
subst t__455 t__634.
rewrite H5.

unfold fuel__23.
unfold add.
unfold one.

reflexivity.

Qed.
