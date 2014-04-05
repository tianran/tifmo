package mylib


package misc {
	/**
	 * Calculates minimal coverings of a set.
	 */
	object listCoverings {
		/**
		 * @param tocover The set to be covered.
		 * @param subsets Sets which are used to cover `tocover`.
		 * @param toavoid Sets which should not be covered.
		 * @return A list of minimal coverings, where each minimal covering is a 
		 * list of subsets, whose union covers `tocover` but does not cover any 
		 * set in `toavoid`. 
		 */
		def apply[T](tocover: Set[T], subsets: Iterable[Set[T]], toavoid: Iterable[Set[T]] = Set.empty[Set[T]]) = {
			
			def recurse(a: Set[T], ss: Set[Set[T]], b: Set[Set[T]]): List[List[Set[T]]] = {
				var ret = Nil:List[List[Set[T]]]
				
				var cc = a.map(x => ss.filter(_.contains(x))).minBy[Int](_.size)
				val rest = ss -- cc
				
				while(!cc.isEmpty) {
					if (!b.exists(_.subsetOf(cc.head))) {
						if (a.subsetOf(cc.head)) {
							ret = List(cc.head) :: ret
						} else {
							val (in, out) = a.partition(cc.head.contains(_))
							for (x <- recurse(out, rest ++ cc.tail, b.map(_ -- cc.head) + in)) {
								ret = (cc.head :: x) :: ret
							}
						}
					}
					cc = cc.tail
				}
				
				ret
			}
			
			if (tocover.isEmpty) {
				List(Nil)
			} else {
				recurse(tocover, subsets.toSet, toavoid.toSet)
			}
		}
	}
	
}

