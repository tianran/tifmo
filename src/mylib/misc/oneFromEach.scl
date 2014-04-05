package mylib


package misc {
	/**
	 * For a collection of collections, `oneFromEach` selects one element from each collection, 
	 * and enumerates all the selection possibilities. 
	 */
	object oneFromEach {
		/**
		 * Returns a list of lists, where each list is composed of elements selected from 
		 * collections in `ll`, one from each. 
		 * @param ll A collection of collections.
		 */
		def apply[T](ll: Iterable[_ <: Iterable[T]]) = {
			
			def recurse(i: Iterable[_ <: Iterable[T]], a: List[List[T]], s: List[T]): List[List[T]] = {
				if (i.isEmpty) {
					s.reverse :: a
				} else {
					(a /: i.head)((x, y) => recurse(i.tail, x, y :: s))
				}
			}
			recurse(ll, Nil, Nil)
		}
		/**
		 * Map `oneFromEach.apply(ll)` with pruning. 
		 * 
		 * This is the same as 
		 * {{{
			 * oneFromEach.apply(ll).map(l => (init /: l)(proc))
		 * }}}
		 * but will skip enumeration of selections whenever the condition `skip` is 
		 * satisfied.
		 */
		def map[T, S](ll: Iterable[_ <: Iterable[T]], init: S, proc: (S, T) => S, skip: S => Boolean) = {
			
			def recurse(i: Iterable[_ <: Iterable[T]], a: List[S], s: S): List[S] = {
				
				if (i.isEmpty) {
					s :: a
				} else {
					(a /: i.head)((x, y) => {
						val tmp = proc(s, y)
						if (skip(tmp)) {
							x
						} else {
							recurse(i.tail, x, tmp)
						}
					})
				}
			}
			recurse(ll, Nil, init)
		}
		
	}
	
}
