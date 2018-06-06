module type FIXED = sig
	type t
	val of_float : float -> t
	val of_int : int -> t
	val to_float : t -> float
	val to_int : t -> int
	val to_string : t -> string
	val zero : t
	val one : t
	val succ : t -> t
	val pred : t -> t
	val min : t -> t -> t
	val max : t -> t -> t
	val gth : t -> t -> bool
	val lth : t -> t -> bool
	val gte : t -> t -> bool
	val lte : t -> t -> bool
	val eqp : t -> t -> bool (** physical equality *)
	val eqs : t -> t -> bool (** structural equality *)
	val add : t -> t -> t
	val sub : t -> t -> t
	val mul : t -> t -> t
	val div : t -> t -> t
	val foreach : t -> t -> (t -> unit) -> unit
end

module type FRACTIONNAL_BITS =
	sig
		val bits : int
	end


module Fractionnal_bits : FRACTIONNAL_BITS =
	functor (Fractionnal_bits : FRACTIONNAL_BITS) ->
	struct
		let bits = Fractionnal_bits.bits
	end

module type MAKE =
	functor (Fractionnal_bits : FRACTIONNAL_BITS) -> FIXED

module Make : MAKE =
	functor (Fractionnal_bits : FRACTIONNAL_BITS) ->
	struct
		type t = int
		of_float fl = (float_of_int fl) *. (2. ** (float_of_int Fractionnal_bits.bits))
		of_int i = i lsl Fractionnal_bits.bits
		to_float fl = (float_of_int fl) /. (2. ** (float_of_int Fractionnal_bits.bits))
		to_int x = x lsr Fractionnal_bits.bits
		to_string t = string_of_float (to_float t)
		zero = of_int 0
		one = of_int 1
		succ x = x + 1
		pred x = x - 1
		min x y = if x <= y then x else y
		max x y = if x >= y then x else y
		gth x y = (x > y)
		lth x y = (x < y)
		gte x y = (x >= y)
		lte x y = (x <= y)
		eqp x y = (x = y)
		eqs x y = (x == y)
		add x y = x + y
		sub x y = x - y
		mul x y = of_float ((to_float x) *. (to_float y))
		div x y = of_float ((to_float x) /. (to_float y))
		foreach x y fn =
		(* t -> t -> (t -> unit) -> unit *)
	end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
	let x8 = Fixed8.of_float 21.10 in
	let y8 = Fixed8.of_float 21.32 in
	let r8 = Fixed8.add x8 y8 in
	print_endline (Fixed8.to_string r8);
	Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f))
