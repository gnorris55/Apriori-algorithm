structure Main =
struct

exception InvalidCommandLineOptions

fun main (prog_name, args) =
    let
      
      val filename :string = case args of
                                 [_,f,_,_,_] => f
                               | _ =>
                                 let
                                   val _ = print("Invalid command line options\n")
                                 in
                                   raise InvalidCommandLineOptions
                                 end
                                     
      val separator : char = case args of
                                 [_,_,s,_,_] => (case Char.fromString(s) of
                                                     SOME c => c
                                                  |  NONE =>
                                                     let
                                                       val _ = print("Invalid character as separator\n")
                                                     in
                                                       raise InvalidCommandLineOptions
                                                     end)
                               | _ =>
                                 let
                                   val _ = print("Invalid command line options\n")
                                 in
                                   raise InvalidCommandLineOptions
                                 end
                                     
      val threshold :real = case args of
                                [_,_,_,s,_] =>
                                (case Real.fromString(s) of
                                     SOME v => v
                                  |  NONE =>
                                     let
                                       val _ = print("Invalid number given for threshold\n")
                                     in
                                       raise InvalidCommandLineOptions
                                     end)
                              | _ =>
                                let
                                  val _ = print("Invalid command line options\n")
                                in
                                  raise InvalidCommandLineOptions
                                end
                                    
      val toPrint:int = case args of
                            [_,_,_,_,s] =>
                            (case Int.fromString(s) of
                                 SOME v => v
                              |  NONE =>
                                 let
                                   val _ = print("Invalid number given for records to print\n")
                                 in
                                   raise InvalidCommandLineOptions
                                 end)
                          | _ =>
                            let
                              val _ = print("Invalid command line options\n")
                            in
                              raise InvalidCommandLineOptions
                            end
                                       
      val stream = TextIO.openIn filename
                                 
      val (nTransactions, minSupport, popItems) =
          case Apriori.do_first_pass(threshold, stream, separator) of
              {nrecords, minSupport, popItems} => (nrecords, minSupport, popItems)
                                                      
      val stream2 = TextIO.openIn filename
                                  
      val popPairs = Apriori.do_second_pass(minSupport, popItems, stream2, separator)
                                           
      val _ = Apriori.print_table(nTransactions, popItems, popPairs, toPrint)

    in
      0
    end                                      
end

