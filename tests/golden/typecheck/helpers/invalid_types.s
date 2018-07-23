Type error at Line 9, Column 7:
   Could not match expected type 'map<account, fixed2>' with actual type 'map<account, fixed3>':
      Type map<account, fixed2> was inferred from the assignment to variable 'm' on Line 9, Column 7
      Type map<account, fixed3> was inferred from the return type of the function 'calcAndInsert' on Line 9, Column 7

Type error at Line 9, Column 27:
   Could not match expected type 'fixed2' with actual type 'fixed3':
      Type fixed2 was inferred from the type signature of argument 3 of function 'calcAndInsert' on Line 9, Column 7
      Type fixed3 was inferred from the type signature of argument 2 of function 'f' on Line 8, Column 22

Type error at Line 16, Column 21:
   Could not match expected type 'map<account, fixed3>' with actual type 'map<account, fixed2>':
      Type map<account, fixed3> was inferred from the type signature of argument 3 of function 'mapInsert' on Line 16, Column 21
      Type map<account, fixed2> was inferred from the type signature of argument 1 of function 'calcAndInsert' on Line 13, Column 36