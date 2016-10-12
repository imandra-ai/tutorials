(**
 * Copyright 2016, Aesthetic Integration Limited. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *    * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above
 * copyright notice, this list of conditions and the following disclaimer
 * in the documentation and/or other materials provided with the
 * distribution.
 *
 *    * Neither the name of Aesthetic Integration Limited nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

type nat = Z | S of nat;;

let rec add (x, y) =
  match x with
    Z -> y
  | S n -> S (add(n, y));;

let rec mult (x, y) =
  match x with
    Z -> Z
  | S n -> add(mult(n, y), y);;

let one = S Z;;

:light off

verify add_assoc (x, y, z) =
  add(x, add(y, z)) = add(add(x, y), z);;

verify add_by_zero_l (x) =
  add(x, Z) = x;;

verify add_one_neq (x,y) =
  add(x, S y) <> x;;

verify add_by_zero_r (x) =
  add(Z, x) = x;;

verify add_commutes (x,y) =
  add(x,y) = add(y,x);;

verify not_add_big (x,y) =
  (y <> Z) ==> (add(x,y) <> x);;

verify zero_cancel (x,y) =
  (add(x,y) = x)
   =
  (y = Z);;

verify add_cancel (x,y,z) =
  (add(x,y) = add(x,z))
   =
  (y = z);;

verify add_cancel_succ (x, y) =
  S (add (x, y)) = add(x, S y);;

verify add_cancel_1 (x,y) =
  (x = add(x, y))
    =
  (y = Z);;

verify add_nonzero_neq (x, y) =
  add(x, y) <> x
    <==>
  (y <> Z);;

verify mult_by_zero (x) =
  mult(x, Z) = Z;;

verify mult_commutes (x, y) =
  mult(x, y) = mult(y, x);;
