open util/ordering[Input] as T

abstract sig Input {prev:lone Input} 
	{prev = T/prev[this]}

one sig Sentinel extends Input {}
sig Pos extends Input {val:Int} {val>=0}
sig Neq extends Input {val:Int} {val<0}

one abstract sig System 
	{read: set Input}
	{read = (T/prevs[Sentinel])}

sig Output extends System {count:Int,s:Int}
 {some read
  count = #(read & Pos)
  s=sum[(read<:Pos).val]
  count > 0
}

sig Error extends System {}
 {no (read & Pos)}

pred Show { }

run Show for 5 but exactly 1 Pos
