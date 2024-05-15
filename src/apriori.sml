(* Your name:
   Your netlink id:
 *)
structure Apriori :> APRIORI =
struct


type ItemsDict = int Dicts.ItemMap.map
type PairsDict = int Dicts.PairMap.map
type FirstPassResult = {nrecords:int, minSupport: int, popItems: ItemsDict}

(* minimum width for name of item in output *)
val MINIMUM_WIDTH = 10

(* use these strings for format your output *)
val DATA_FORMAT_STRING = "%9.6f %s %6d %8.6f %s %6d %9.3f %9.3f\n"
val TITLE_FORMAT_STRING = " SuppPair-%s - Freq- Support-%s - FreqPair-  Conf.  - Lift\n"

fun do_first_pass (threshold: real, lines: TextIO.instream, delim: char): FirstPassResult =
    
    let
      val empty_dict = Dicts.ItemMap.empty


      fun remove_new_line str =
        if String.isSuffix "\n" str then
           String.substring (str, 0, size str - 1)
        else
           str

      (*for reading the file*)
      fun read_file (rtn_dict, count) =
        case (TextIO.inputLine lines) of
             NONE => (rtn_dict, count)
           | SOME temp_line =>
               
               let
                val line = remove_new_line temp_line
                val strings_of_line = String.tokens (fn str => (str = delim)) line
                fun read_line (strings, curr_dictionary) =
                  case strings of
                       [] => curr_dictionary
                     | str::bc =>
                         case Dicts.ItemMap.find(curr_dictionary, str) of
                          NONE   => read_line(bc,Dicts.ItemMap.insert(curr_dictionary, str, 1))
                        | SOME value =>
                            let
                              val (temp_dictionary,_) = Dicts.ItemMap.remove(curr_dictionary, str)
                            in
                              read_line(bc, Dicts.ItemMap.insert(temp_dictionary, str, (value+1)))
                            end
               in
                read_file(read_line(strings_of_line, rtn_dict), (count + 1))
               end

    fun get_popular_items(input_dict, minimum_support) =
       Dicts.ItemMap.filter (fn dict_val => (dict_val >= minimum_support)) input_dict

    fun print_key_list key_list =
      case key_list of
           [] => 1
         |a::bc => 
           let 
             val nothing = 1;
           in
            print(a ^ "\n");
            print_key_list bc 
           end
    in
      let
        val (dicts, count) = read_file(empty_dict, 0)
        val minimum_support = Real.toInt IEEEReal.TO_ZERO (threshold * Real.fromInt(count))
        val pop_dicts = get_popular_items(dicts, minimum_support)
        val rtn_record = {nrecords = count, minSupport = minimum_support, popItems = pop_dicts}
      in
        print("point 1");
        rtn_record
      end
    end

fun do_second_pass(support: int, popItems: ItemsDict, lines: TextIO.instream, delim:char): PairsDict  =
    
    let
      val empty_dict = Dicts.PairMap.empty

      
      fun remove_new_line str =
        if String.isSuffix "\n" str then
           String.substring (str, 0, size str - 1)
        else
           str
      (*for reading the file*)
      fun read_file (rtn_dict, count) =

        case (TextIO.inputLine lines) of
             NONE => (rtn_dict, count)
           | SOME temp_line =>
               
               let
                val line = remove_new_line temp_line
                val strings_of_line = String.tokens (fn str => (str = delim)) line
                 
                 fun find_string(curr_str) =
                   case Dicts.ItemMap.find(popItems, curr_str) of
                        NONE => false
                      | SOME _ => true

                 fun read_line (strings, curr_dictionary) =
                  case strings of
                       []      => curr_dictionary
                     | str::bc =>
                         if find_string(str) = false
                         then read_line(bc, curr_dictionary)
                         else
                           let
                            fun make_pairs(rest_of_list, dict) =
                              case rest_of_list of
                                  [] =>  dict
                                 | item2::rest =>  
                                    if find_string(item2) = false
                                    then make_pairs(rest, dict)
                                    else
                                     let
                                       val key = 
                                        if (String.compare(str, item2) = LESS)
                                        then (str, item2)
                                        else (item2, str)
                                       
                                       fun insert_item() =
                                        case Dicts.PairMap.find(dict, key) of
                                                NONE   =>
                                                make_pairs(rest,Dicts.PairMap.insert(dict,key,
                                                1))
                                              | SOME value =>
                                                 let
                                                   val (temp_dictionary,_) =
                                                     Dicts.PairMap.remove(dict,key)
                                                 in
                                                   make_pairs(rest,Dicts.PairMap.insert(temp_dictionary,key,value+1))

                                                 end
                                     in
                                        insert_item()
                                     end
                          in
                            read_line(bc, make_pairs(bc, curr_dictionary))
                          end
               in
                 read_file(read_line(strings_of_line, rtn_dict), (count + 1))
               end
    
      fun get_popular_items(input_dict, minimum_support) =
       Dicts.PairMap.filter (fn dict_val => (dict_val >= minimum_support)) input_dict
    
    in
        let
          val (dicts, count) = read_file(empty_dict, 0)
          val pop_dicts = get_popular_items(dicts, support)
        in
          print("point 2");
          pop_dicts
        end
    end
        
        
fun print_table(nTransactions: int,
                popItems: ItemsDict, popPairs: PairsDict,
                toPrint:int):int = 
                
                let
                  fun get_confidence dict_pair =
                      let
                         val ((item1, item2), value) = dict_pair
                         val pair_support = (Real.fromInt(value)) / (Real.fromInt(nTransactions))
                         val item2_val = valOf (Dicts.ItemMap.find(popItems,item2))
                         val item2_support = (Real.fromInt(item2_val)) / (Real.fromInt(nTransactions)) 
                      in
                         pair_support / item2_support
                      end
                  
                  fun get_record dict_item =
                    let
                       val ((item1, item2), v) = dict_item
                       val confidence = get_confidence dict_item
                       val item1_val = valOf (Dicts.ItemMap.find(popItems,item1))
                       val item2_val = valOf (Dicts.ItemMap.find(popItems,item2))
                       
                       val item1_support =(Real.fromInt(item1_val)) / (Real.fromInt(nTransactions)) 
                       val item2_support = (Real.fromInt(item2_val)) / (Real.fromInt(nTransactions)) 
                       val pair_support = (Real.fromInt(v)) / (Real.fromInt(nTransactions))
                       val lift = pair_support / (item1_support * item2_support)
                       val rtn_record = {
                       item1 = item1, item2 = item2, pair_freq= v,
                       item_freq = item1_val, other_item_freq = item2_val, 
                       confidence = confidence, lift=lift, pair_support =
                       pair_support, item1_support = item1_support,
                       item2_support=item2_support}
                    in
                       rtn_record
                    end
                  
                  fun add_switched_items(pair_list, rtn_dict) =
                    case pair_list of
                         [] => rtn_dict
                       | a::bc => let
                                   val ((item1, item2), v) = a
                                   val new_key = (item2, item1) 
                                  in
                                   add_switched_items(bc, Dicts.PairMap.insert(rtn_dict,new_key, v))
                                  end   
                  fun halve nil = (nil, nil)
                    |   halve [a] = ([a], nil)
                    |   halve (a :: b :: cs) =
                          let
                            val (x, y) = halve cs
                          in
                            (a :: x, b :: y)
                          end

                  fun real_close(a, b) =
                    if abs(a - b) < 0.00001 then true
                    else false

                  fun merge_sort xs =
                    case xs of
                         [] => []
                       | [a] => [a]
                       | _  =>
                        let

                          val (left, right) = halve xs     
                          val sorted_left = merge_sort left
                          val sorted_right = merge_sort right
                          
                          (*returns true if x is larger than y*)
                          fun determine_order(x, y) =
                             let
                               val x_conf = get_confidence x
                               val y_conf = get_confidence y
                             in
                               if real_close(x_conf, y_conf) = true
                               then
                                 let
                                  val ((x_item1, x_item2), x_v) = x
                                  val ((y_item1, y_item2), y_v) = y
                                   val item1_val_x = valOf(Dicts.ItemMap.find(popItems,x_item1))
                                   val item2_val_x =valOf(Dicts.ItemMap.find(popItems,x_item2))
                                  val item1_val_y = valOf(Dicts.ItemMap.find(popItems,y_item1))
                                  val item2_val_y = valOf(Dicts.ItemMap.find(popItems,y_item2))

                                  val item1_support_x =(Real.fromInt(item1_val_x)) / (Real.fromInt(nTransactions)) 
                                  val item2_support_x =(Real.fromInt(item2_val_x)) / (Real.fromInt(nTransactions)) 
                                  val pair_support_x = (Real.fromInt(x_v)) / (Real.fromInt(nTransactions))
                                  val lift_x = pair_support_x / (item1_support_x * item2_support_x)
                                  
                                  val item1_support_y =(Real.fromInt(item1_val_y)) / (Real.fromInt(nTransactions)) 
                                  val item2_support_y =(Real.fromInt(item2_val_y)) / (Real.fromInt(nTransactions)) 
                                  val pair_support_y = (Real.fromInt(y_v)) / (Real.fromInt(nTransactions))
                                  val lift_y = pair_support_y / (item1_support_y * item2_support_y)
                                 in
                                   (*print (Real.toString lift_x ^ " " ^
                                   Real.toString lift_y ^ "\n");*)
                                   if real_close(lift_x, lift_y) = true
                                   then
                                       if item1_val_x = item1_val_y
                                       then
                                          if String.compare(x_item1, y_item1) =EQUAL
                                          then
                                              if String.compare(x_item2, y_item2) =EQUAL
                                              then true
                                              else if String.compare(x_item2,y_item2)= LESS
                                              then true
                                              else false
                                          else if String.compare(x_item1,y_item1)=LESS
                                          then true
                                          else false
                                      else if Int.compare(item1_val_x,item1_val_y) = GREATER
                                      then true
                                      else false
                                   else if lift_x > lift_y
                                   then true
                                   else false
                                 end
                              else if x_conf > y_conf
                              then true
                              else false 
                             end 

                          fun merge (xs, ys) =
                            case (xs, ys) of
                                ([], _) => ys
                               |(_, []) => xs
                               |(x_head::x_rest, y_head::y_rest) =>
                                     if determine_order(x_head, y_head) = true
                                     then x_head :: merge(x_rest, y_head::y_rest) 
                                     else y_head :: merge(y_rest, x_head::x_rest)

                        in
                          merge(merge_sort sorted_left, merge_sort sorted_right)
                        end
                
                  fun pad_string (str, pad_num) =
                      if pad_num <= 0
                      then str
                      else pad_string(str ^ " ", pad_num-1)

                        

                  fun print_pairs (xs, len1, len2, n) =
                    if n <= 0
                    then ~1
                    else
                      case xs of
                          [] => ~1
                         | a::bc =>
                                let

                                   fun get_longest xs longest = 
                                     case xs of
                                          [] => longest
                                        | a::bc =>
                                            if size a > size longest
                                            then get_longest bc a
                                            else get_longest bc longest
                                   
                                   fun print_line dict_item =
                                          let

                                            val ((item1, item2), v) = dict_item
                                            val confidence = get_confidence dict_item
                                            val item1_val = valOf (Dicts.ItemMap.find(popItems,item1))
                                            val item2_val = valOf (Dicts.ItemMap.find(popItems,item2))
                                            
                                            val item1_support =(Real.fromInt(item1_val)) / (Real.fromInt(nTransactions)) 
                                            val item2_support = (Real.fromInt(item2_val)) / (Real.fromInt(nTransactions)) 
                                            val pair_support = (Real.fromInt(v)) / (Real.fromInt(nTransactions))
                                            val lift = pair_support / (item1_support * item2_support)
                                            val pad_len1 = len1 - (String.size item1) 
                                            val pad_len2 = len2 - (String.size item2)
                                            val output_str1 = pad_string (item1, pad_len1) 
                                            val output_str2 = pad_string (item2, pad_len2) 
                                            val format_list = [
                                                        Format.REAL
                                                        pair_support,
                                                        Format.STR
                                                        output_str1,
                                                        Format.INT
                                                        item1_val,
                                                        Format.REAL
                                                        item1_support,
                                                        Format.STR
                                                        output_str2,
                                                        Format.INT
                                                        v,
                                                        Format.REAL
                                                        confidence,
                                                        Format.REAL
                                                        lift
                                                        ]
                                        (* val longest = get_longest format_list
                                        * ""*)                      
                                          in
                                            print(Format.format
                                             DATA_FORMAT_STRING
                                            format_list);
                                            1
                                          end
                                   val print_return = print_line a
                                in
                                  print_pairs(bc, len1, len2, n-1)
                                end     


                  fun print_heading (len1, len2)=
                        let
                          val item_str = "Item"
                          val with_str = "With"
                          val pad_len1 = len1 - (String.size item_str) 
                          val pad_len2 = len2 - (String.size with_str)
                          val str1 = pad_string (item_str, pad_len1)
                          val str2 = pad_string (with_str, pad_len2)
                          val format_list = [
                                        Format.STR
                                        str1,
                                        Format.STR
                                        str2]
                        in
                          print(Format.format
                                TITLE_FORMAT_STRING
                                format_list);
                          1
                        end 

                  fun get_length_list (dicts, length_list, n) =
                    if n <= 0 then length_list
                    else    
                      case dicts of
                          [] => length_list
                         | a::bc =>
                                let
                                
                                  val {item1, item2, pair_freq,
                                       item_freq, other_item_freq, 
                                       confidence, lift, pair_support,
                                       item1_support, item2_support} = get_record a

                                  val str_dict_list = [Real.toString pair_support, item1,
                                  Int.toString item_freq, Real.toString
                                  item1_support, item2, Int.toString pair_freq,
                                  Real.toString confidence, Real.toString lift]
                                  
                                  fun zip lists =
                                    case lists of
                                      ([], []) => []
                                    | (hd1::tl1, hd2::tl2) =>
                                        (hd1,hd2)::zip(tl1,tl2)
                                    | _ => []

                                  val size_list = map (fn str => size str) str_dict_list
                                  val combined_list = zip (size_list,length_list);
                                  val result_list = foldr (fn ((x,y), acc) => if Int.compare(x, y) = GREATER
                                                                              then x::acc
                                                                              else
                                                                                y::acc)
                                                                                [] combined_list 

                                 (*val nothing = map (fn str => let in
                                   print(Int.toString str ^ " ");
                                   str end) result_list*)
                                in
                                  (*print("\n");*)
                                 get_length_list (bc, result_list,n-1)                                 
                                end


                  fun get_min(a,b) =
                    if Int.compare(a,b) = LESS then a
                    else b
                
                  val list_of_pairs = Dicts.PairMap.listItemsi popPairs
                  val final_dict = add_switched_items(list_of_pairs,popPairs) 
                  
                  val list_all_pairs = Dicts.PairMap.listItemsi final_dict
                  val sorted_list = merge_sort list_all_pairs
                  val num_pairs = Int.toString(Dicts.PairMap.numItems(final_dict))
                  val num_items = Int.toString(Dicts.ItemMap.numItems(popItems));
                  val max_column_len = [MINIMUM_WIDTH, MINIMUM_WIDTH, 
                    MINIMUM_WIDTH,MINIMUM_WIDTH,MINIMUM_WIDTH,
                    MINIMUM_WIDTH,MINIMUM_WIDTH]
                  val max_list = get_length_list (sorted_list, max_column_len,toPrint) 
                  val item1_len = List.nth (max_list,1) 
                  val item2_len = List.nth (max_list,4) 
                  val title_output = print_heading(item1_len, item2_len)
                  val pairs_output = print_pairs(sorted_list, item1_len, item2_len, toPrint)
                  val printed_pairs = get_min(toPrint, Dicts.PairMap.numItems(final_dict)) 
                in
                  print("\n");
                  print("Number items printed: " ^ Int.toString(printed_pairs) ^ "\n");
                  print("\n");
                  print("Number transactions: " ^ Int.toString(nTransactions) ^ "\n");
                  print("Number popular items: " ^ num_items ^ "\n");
                  print("Number popular pairs: " ^ num_pairs ^ "\n");
                 1
                end

end


    
