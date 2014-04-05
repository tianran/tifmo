package mylib


package misc {
	/**
	 * Calculates partitions of a set.
	 */
	object listPartitions {
		/**
		 * @param set The set to be partitioned.
		 * @param subsets A collection of subsets of `set`.
		 * @return A list of partitions of `set`, where each partition is a list 
		 * of subsets, whose union covers `set`, and each of which are disjoint 
		 * to each other. 
		 */
		def apply[T](set: Set[T], subsets: Iterable[Set[T]]) = {
			
			def recurse(a: Set[T], ss: Iterable[Set[T]]): List[List[Set[T]]] = {
				var ret = Nil:List[List[Set[T]]]
				var cc = ss
				while(!cc.isEmpty) {
					if (cc.head == a) {
						ret = List(cc.head) :: ret
					} else if (cc.head.subsetOf(a)) {
						recurse(a -- cc.head, cc.tail).foreach((x: List[Set[T]]) => ret = (cc.head :: x) :: ret)
					}
					cc = cc.tail
				}
				ret
			}
			
			recurse(set, subsets)
		}
	}
	
}
