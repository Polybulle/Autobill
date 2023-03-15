(*Set Printing Existential Instances.*)
Require Import Lia.

Definition fuel__23 (x:nat) := 2 * x.
Definition add x y := x+y.
Definition one := 1.
Definition z := 0.

Ltac eparam v := evar (v : nat); eexists v.
Ltac setparam v := simpl; subst v; reflexivity; subst v.

Goal

(forall (t__72:nat) (t__293:nat),
 exists (t__334:nat) (t__355:nat) (t__451:nat) (t__762:nat),
     ((t__72 = t__293)
   /\ (t__293 = t__334)
   /\ (t__762 = (fuel__23 t__293)))
   -> ((forall (t__106:nat) (t__335:nat) (t__755:nat) (t__756:nat),
        exists (t__510:nat) (t__587:nat) (t__754:nat),
            ((t__106 = t__335)
          /\ (t__334 = (add t__335 one))
          /\ (t__754 = (fuel__23 t__587)))
          -> ((exists (t__129:nat) (t__130:nat) (t__135:nat) (t__141:nat) (t__142:nat) (t__452:nat) (t__453:nat) (t__480:nat) (t__481:nat) (t__508:nat) (t__509:nat) (t__746:nat)
               (t__748:nat) (t__749:nat) (t__752:nat) (t__744:nat) (t__745:nat) (t__747:nat) (t__750:nat) (t__751:nat),
                   (((fuel__23 t__293) = t__451)
                 /\ (t__451 = (add t__453 t__452))
                 /\ (t__452 = t__750)
                 /\ (t__453 = t__480)
                 /\ (t__453 = t__751)
                 /\ (t__480 = (add t__481 one))
                 /\ (t__481 = t__508)
                 /\ (t__481 = t__747)
                 /\ (t__508 = (add t__510 t__509))
                 /\ (t__509 = t__744)
                 /\ (t__510 = t__745)
                 /\ (t__744 = t__130)
                 /\ (t__745 = t__129)
                 /\ (t__746 = (add t__510 t__509))
                 /\ (t__747 = t__135)
                 /\ (t__748 = (add t__481 one))
                 /\ (t__749 = one)
                 /\ (t__750 = t__142)
                 /\ (t__751 = t__141)
                 /\ (t__752 = (add t__453 t__452)))
                 /\ True)
             /\ (exists (t__161:nat) (t__753:nat),
                     ((t__587 = t__753)
                   /\ (t__753 = t__161))
                   /\     (((fuel__23 t__587) = t__510)
                        /\ (t__587 = t__335)))))
      /\ (forall (t__761:nat),
           (t__334 = z)
            -> (exists (t__95:nat) (t__96:nat) (t__356:nat) (t__357:nat) (t__384:nat) (t__757:nat) (t__760:nat) (t__758:nat) (t__759:nat),
                    (((fuel__23 t__293) = t__355)
                  /\ (t__355 = (add t__357 t__356))
                  /\ (t__356 = t__758)
                  /\ (t__357 = t__384)
                  /\ (t__357 = t__759)
                  /\ (t__384 = z)
                  /\ (t__757 = z)
                  /\ (t__758 = t__96)
                  /\ (t__759 = t__95)
                  /\ (t__760 = (add t__357 t__356)))
                  /\ True)))).

  intros.

  eparam t__334; eparam t__355; eparam t__452; eparam t__762.
  intro H. decompose [and] H. clear H.


  instantiate (t__334 := t__293); subst t__334.
  instantiate (t__762 := fuel__23 t__293); subst t__762.

  split.

  intros.
  eparam t__510; eparam t__587; eparam t__754.
  intro H. decompose [and] H. clear H.

  instantiate (t__754 := t__335); subst t__754.
  subst t__587.

  split.

  eparam t__129.
  eparam t__130.
  eparam t__135.
  eparam t__141.
  eparam t__142.
  eparam t__0.
  eparam t__453.
  eparam t__480.
  eparam t__481.
  eparam t__508.
  eparam t__509.
  eparam t__746.
  eparam t__748.
  eparam t__749.
  eparam t__752.
  eparam t__744.
  eparam t__745.
  eparam t__747.
  eparam t__750.
  eparam t__751.

  repeat split.

  all:cycle 2.
  setparam t__0.
  setparam t__453.
  setparam t__453.
  setparam t__480.
  setparam t__481.
  setparam t__481.
  setparam t__508.
  setparam t__509.
  setparam t__510.
  setparam t__744.
  setparam t__745.
  setparam t__746.
  setparam t__135.
  setparam t__748.
  setparam t__749.
  setparam t__750.
  setparam t__141.
  setparam t__752.

  eparam t__161; eparam t__753.

  repeat split.

  setparam t__161.

  intros.

  subst t__293.

  eparam t__95.
  eparam t__96.
  eparam t__356.
  eparam t__357.
  eparam t__384.
  eparam t__757.
  eparam t__760.
  eparam t__758.
  eparam t__759.

  repeat split.

  all: cycle 2.
  setparam t__356.
  setparam t__357.
  setparam t__357.
  setparam t__384.
  setparam t__757.
  setparam t__758.
  setparam t__95.
  setparam t__760.

  setparam t__452.
  unfold t__452. unfold t__453. unfold t__510. unfold t__0.
  rewrite H5.
  unfold fuel__23. unfold add. unfold one.

  all: cycle 1.
  setparam t__355.

  unfold t__355. unfold t__357. unfold t__356.
  rewrite H.
  unfold fuel__23. unfold add. unfold z.


  instantiate (t__96 := 0). subst t__96. lia.
  instantiate (t__130 := 0). subst t__130.
  instantiate (t__142 := 1). subst t__142. lia.

  Qed.
