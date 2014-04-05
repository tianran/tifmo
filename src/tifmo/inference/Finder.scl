package tifmo

import dcstree.SemRole

import scala.collection.mutable

package inference {
	
	object Finder {
		
		/**
		 * Returns if `h` is known to be a Cartesian product of `c`.
		 */
		def isCP(h: Term, c: Set[(Term, SemRole)]) = h.index.iscps.contains(IEPredCP(h.index, c.map(x => (x._1.index, x._2))))
		
		/**
		 * Find Cartesian product.
		 */
		def findCP(comp: Set[(Term, SemRole)]) = {
			if (comp.size <= 1) {
				Some(comp.head._1)
			} else {
				val icomp = comp.map(x => (x._1.index, x._2))
				val tmp = icomp.minBy[Int](_._1.mkcps.size)._1.mkcps
				tmp.find(_.comp == icomp) match {
					case Some(x) => Some(x.head.holder)
					case None => None
				}
			}
		}
		
		/**
		 * Returns if `h` is known to be a projection of `t` into dimension `r`.
		 */
		def isPI(h: Term, t: Term, r: SemRole) = h.index.ispis.contains(IEPredPI(h.index, t.index, r))
		
		/**
		 * Find projection.
		 */
		def findPI(compt: Term, headrs: Set[SemRole]) = {
			compt.index.mkpis.find(_.headrs == headrs) match {
				case Some(x) => Some(x.head.holder)
				case None => None
			}
		}
		
		/**
		 * Returns if `h` is known to be an intersection of `c`.
		 */
		def isIN(h: Term, c: Set[Term]) = c.forall(h.hasSuper(_)) && {
			val htm = h.index
			val comp = c.map(_.index)
			
			val whole = mutable.Set.empty[TermIndex]
			for (x <- comp) whole ++= x.superSets
			val regi = mutable.Map(htm -> List(() => (whole += htm):Unit))
			
			var toexplore = Set(htm)
			while (!toexplore.isEmpty) {
				toexplore = toexplore.flatMap(x => {
					var ret = Nil:List[TermIndex]
					if (!whole.contains(x) && !x.isins.exists(in => {
						val tms = (in.comp -- whole).toArray
						if (tms.isEmpty) {
							regi(x).reverse.foreach(_())
							true
						} else {
							for (i <- 0 until tms.length) {
								val tmp = tms(i)
								regi(tmp) = (() => {
									tms(i) = null
									if (tms.forall(_ == null) && !whole.contains(x)) regi(x).reverse.foreach(_())
								}) :: regi.getOrElse(tmp, {
									ret = tmp :: ret
									List(() => (whole += tmp):Unit)
								})
							}
							false
						}
					})) ret else Nil
				})
			}
			
			whole.contains(htm)
		}
		
		/**
		 * Find intersection.
		 */
		def findIN(comp: Set[Term]) = {
			if (comp.size <= 1) {
				Some(comp.head)
			} else {
				val tmp = comp.minBy[Int](_.index.assuper.size).subSets
				val fil = tmp.filter(x => comp.forall(_.hasSub(x)))
				val mfil = fil.filter(x => fil.forall(x.hasSub(_)))
				if (mfil.isEmpty) {
					None
				} else if (isIN(mfil.head, comp)) {
					Some(mfil.head)
				} else {
					None
				}
			}
		}
		
		/**
		 * Find `func(tms.tail, param)` for the user-defined function `func`.
		 */
		def findFunc(func: IEFunction, tms: Seq[Term], param: Any) = {
			assert(tms.head == null)
			val itmstl = tms.tail.map(_.index)
			val tmp = itmstl.minBy[Int](_.funcs.size).funcs
			tmp.find(fc => fc.func == func && fc.tms.tail == itmstl && fc.param == param) match {
				case Some(fc) => Some(fc.tms.head.holder)
				case None => None
			}
		}
		
		
	}
	
}
