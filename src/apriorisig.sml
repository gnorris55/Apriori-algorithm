signature APRIORI =
sig

  type ItemsDict = int Dicts.ItemMap.map
  type PairsDict = int Dicts.PairMap.map

  type FirstPassResult = {nrecords:int, minSupport: int, popItems: ItemsDict}

  val do_first_pass: (real * TextIO.instream * char) -> FirstPassResult
                                                            
  val do_second_pass: (int * ItemsDict * TextIO.instream  *char) -> PairsDict 

  val print_table : (int * ItemsDict * PairsDict * int) -> int 

end
  

