let rec (<->) a b =
   if a > b then [] else a::((a+1) <-> b)

let cart a b = List.(flatten (map (fun i -> map (fun j -> i,j) b) a))

let rec not_doub = function
   | '0'::l    -> not_doub l
   | a::b::q -> (a <> b) && not_doub (b::q)
   | _       -> true

let zone (i,j) =
   (fun x y -> cart (x <-> (x+2)) (y <-> (y+2))) (3 * (i/3)) (3 * (j/3))

let lin (i,_) = cart [i] (0 <-> 8)

let col (_,j) = cart (0 <-> 8) [j]

let test_lst mat l =
   not_doub List.(sort compare (map (fun (i,j) -> mat.(i).[j]) l))

let is_ok mat pos =
   List.([zone;lin;col] |> map ((|>) pos) |> map (test_lst mat) |> for_all (fun i -> i))

let shuffle tab =
   for i = Array.length tab - 1 downto 1 do	
      let j = Random.int (i+1) and z = tab.(i) in
         tab.(i) <- tab.(j); tab.(j) <- z
   done;
   tab

let rec f mat k tab =
   if k = -1
      then mat
      else let (i,j) = tab.(k) in
           match mat.(i).[j] with
           | '9' -> mat.(i).[j] <- '0'; f mat (k+1) tab 
           | x   -> (mat.(i).[j] <- char_of_int (1 + int_of_char x);
                    f mat (k - if is_ok mat (i,j) then 1 else 0) tab)

let solve mat =
   0 <-> 8
   |> cart (0 <-> 8)
   |> List.filter (fun (i,j) -> mat.(i).[j] = '0')
   |> Array.of_list
   |> shuffle
   |> (fun x -> f mat (Array.length x - 1) x) 

let _ =
   (fun _ -> read_line ())
   |> Array.init 9
   |> solve
   |> Array.iter print_endline
