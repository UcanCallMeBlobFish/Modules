module type Field = sig
  type t
  val zero : t                  (* zero element of the field *)
  val one : t                   (* unit element of the field *)
  val compare : t -> t -> int   (* comparison *)
  val to_string : t -> string   (* field element to string *)
  val add : t -> t -> t         (* addition *)
  val mul : t -> t -> t         (* multiplication *)
  val sub : t -> t -> t         (* subtraction *)
  val div : t -> t -> t         (* division *)
  val add_inv : t -> t          (* additive inverse *) 
  val mul_inv : t -> t          (* multiplicative inverse *)
end ;;



module type RationalField =
sig
  include Field with type t = int * int
  type t = int * int          (* rationals are represented as pairs of int *)
  exception Bad_rational of string
  val standard_form : t -> t  (* standard from of a rational number *)
  val to_float : t -> float   (* decimal expansion *)
  val from_int : int -> t     (* integer to rational conversion *)          
end;;

module type GaussianRationalField =
sig
  include Field with type t = (int * int) * (int * int)
    (* Gaussian rationals are represented as pairs of rationals *)
  exception Division_by_zero of string
  val from_rational : (int * int ) -> t   (* rational to complex *)     
  val conj : t -> t                       (* conjugate *)
  val re : t -> (int * int)               (* real part *)
  val im : t -> (int * int)               (* inaginary part *)
end




module Rationals : RationalField =
  
struct
  type t = int * int
           
  exception Bad_rational of string
      
  let zero = (0,1) 
             
  let one = (1,1)
             
  let standard_form (n,d) = 
    let rec gcd a b =
      if b = 0 then a else gcd b (a mod b)
    in
    let k = gcd n d 
    in 
    (n/k,d/k) 
            
  let compare (r1,i1) (r2,i2) = 
    let (a,b) = standard_form (r1,i1)
    in
    let (c,d) = standard_form (r2,i2)
    in
    
    let l = a * d
    in
    let r = b * c 
    in compare l r 
  
  let add (a,b) (c,d) = 
    if (b==0 || d==0) then raise (Bad_rational ("cant divade by zero")) else 
      standard_form ((a*d + b*c), (b*d))
      
  let mul (a,b) (c,d) =
    if (b==0 || d==0) then raise (Bad_rational ("cant divade by zero")) else 
    
      standard_form ((a*c),(b*d))
      
  let sub (a,b) (c,d) = 
    if (b==0 || d==0) then raise (Bad_rational ("cant divade by zero")) else standard_form ((a*d - b*c), (b*d))
      
  let div (a,b) (c,d) =
    if b==0 || c == 0 then raise (Bad_rational ("cant divade by zero")) else 
      standard_form ((a*d),(b*c))

  let add_inv (a,b) =
    if (b==0 || a ==0 ) then raise (Bad_rational ("cant divade by zero")) else 
      standard_form(-a,b)
                      
  let mul_inv (a,b) = 
    if a==0 || b == 0 then raise (Bad_rational ("cant divade by zero"))
    else 
      standard_form(b,a) 

  let from_int a = (a,1)     (*wrong*)
                   
  let to_float (a,b) =
    if b = 0 then raise (Bad_rational ("cant divade by zero")) else
      float_of_int a /. float_of_int b 
            
        
               
  let to_string (a, b) =
    
    let temp =
      standard_form (a, b)
    in 
    match temp with | (c, d) ->
      if c = 0 then string_of_int(0)
      else if d = 1 then string_of_int(c)
      else string_of_int c ^ "/" ^ string_of_int (d)

             
  
end


module GaussianRationals : GaussianRationalField =
struct
  type t = (int * int) * (int * int)
                         
  exception Division_by_zero of string
      
  let zero = ((0,1),(0,1))
             
  let one = ((1,1),(1,1)) 
            
            
  let add x1 x2 =
    
    
    let  ((a,b),(a1,b1)) = x1
    in 
    let  ((c,d),(c1,d1)) = x2
    in
    if (d ==0 || d1 ==0 || b1 ==0 || b ==0) then raise (Division_by_zero("cant devide by zero"))
    else
      let k1 = Rationals.standard_form ((a*d + b*c), (b*d))
      in
      let k2 = Rationals.standard_form ((a1*d1 + b1*c1), (b1*d1))
      in
      (k1,k2)
    
  let sub x1 x2 = 
     
    let  ((a,b),(a1,b1)) = x1
    in 
    let  ((c,d),(c1,d1)) = x2
    in
    if (d ==0 || d1 ==0 || b1 ==0 || b ==0) then raise (Division_by_zero("cant devide by zero"))
    else
      
      let k1 = Rationals.standard_form ((a*d - b*c), (b*d))
      in
      let k2 = Rationals.standard_form ((a1*d1 - b1*c1), (b1*d1))
      in
      (k1,k2)
    
  let mul ((a,b),(a1,b1)) ((c,d),(c1,d1)) = 
    if (d ==0 || d1 ==0 || b1 ==0 || b ==0) then raise (Division_by_zero("cant devide by zero"))
    else
      let s = Rationals.mul (a,b) (c,d)
      in
      let k = Rationals.mul (a1,b1) (c1,d1)
      in
      let re = Rationals.sub s k 
      in
      let p = Rationals.mul (a,b) (a1,b1)
      in
      let t = Rationals.mul (a1,b1)(c,d)
      in
      let im = Rationals.sub p t
      in
      (re,im)
      
      
      
    
  let div ((a,b),(a1,b1)) ((c,d),(c1,d1)) = 
    
    if (d ==0 || d1 ==0 || b1 ==0 || b ==0 || c1 ==0 || c ==0 ) then raise (Division_by_zero("cant devide by zero"))
    else
  
        
      let s = Rationals.standard_form ((a*d),(b*c)) 
          
      in
      
      let y = Rationals.standard_form ((a1*d1),(b1*c1))
          
      in
      (s,y)
      
    
  let add_inv ((a,b),(c,d)) = 
    if (d ==0 || b ==0) then raise (Division_by_zero("cant devide by zero"))
    else
      ( Rationals.standard_form(-b,a),Rationals.standard_form(-c,d) )
                             
  let mul_inv ((a,b),(c,d)) =
    if (d ==0 || a ==0 || c ==0 || b ==0) then raise (Division_by_zero("cant devide by zero"))
    else
      ((Rationals.standard_form (b, a)), (Rationals.standard_form (d, c)))
      
                             
  let from_rational (a,b) =
    if ( b ==0) then raise (Division_by_zero("cant devide by zero"))
    else
      (Rationals.standard_form(a,b),(0,1))
                            
  let conj ((a, b), (c,d)) = 
    if (d ==0 || b ==0) then raise (Division_by_zero("cant devide by zero"))
    else
      (Rationals.standard_form(a, b), Rationals.standard_form((-c), d))

      
  let re ((a,b),(c,d)) = 
    if (d ==0  || b ==0) then raise (Division_by_zero("cant devide by zero"))
    else
      Rationals.standard_form(a,b)
                         
  let im ((a,b),(c,d)) =
    if (d ==0 || b ==0) then raise (Division_by_zero("cant devide by zero"))
    else
      Rationals.standard_form(c,d)
  
  
  let compare (r1,i1) (r2,i2) = 
    
    if (Rationals.compare r1 r2) != 0 then Rationals.compare r1 r2 
    else Rationals.compare i1 i2
    
  let to_string (r,i) = 
    let (k1,k2) = Rationals.standard_form r
    in 
    let (c1,c2) = Rationals.standard_form i
    in
    if c1 == 0 || k1 ==0 then string_of_int(0) 
    else if c1<0 then string_of_int(k1) ^ "/" ^ string_of_int(k2) ^ "-" ^ string_of_int c1 ^ "/" ^ string_of_int c2 ^ "i"
      
    else  string_of_int(k1) ^ "/" ^ string_of_int(k2) ^ " + " ^ string_of_int c1 ^ "/" ^ string_of_int c2 ^ " i"
      
        (* make sure that both the real part r and the imaginary part i
           are in standard form as a rational number. 
           Skip a summand if it is 0. 
  *)

 
end
;;
GaussianRationals.mul ((5,10),(10,7)) ((5,10),(10,7));;
