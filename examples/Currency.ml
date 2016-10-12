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
 
type currency = GBP of float | USD of float | EUR of float;;

let f x =
  match x with
    Some n -> n + 1
  | None -> 0;;

let foo (x,y) =
  match x, y with
    Some n, 100 ->
    n + 25
  | Some n, m ->
     76
  | _ -> 25;;

let bar (x,y,z) =
  match x,y,z with
    Some n, Some m, Some k ->
    n + m + k - 2
  | None, Some n, Some n' ->
     if n = 2*n' + 28
     then 956
     else foo (Some (43*n), n')
  | _ -> 99;;

verify _ (x,y) = foo(x,y) <> 76;;

verify _ (x,y) = foo(x,y) <> 97;;

verify _ (x,y,z) = bar(x,y,z) <> 101883;;

let g x =
  if x > 0 then Some x else None;;

verify _ x = g x <> None;;

let c_add (x,y) =
  match x,y with
    GBP n, GBP m -> Some (GBP (n +. m))
  | USD n, USD m -> Some (USD (n +. m))
  | EUR n, EUR m -> Some (EUR (n +. m))
  | _ -> None;;

let same_currency (x,y) =
  match x,y with
    USD _, USD _ -> true
  | GBP _, GBP _ -> true
  | _ -> false;;

verify c_add_safe (x,y) =
  (same_currency(x,y))
    ==>
  (c_add(x,y) <> None);;

verify _ (x,y) = same_currency(x,y);;

