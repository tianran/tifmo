package tifmo

import dcstree.Declarative
import inference.IEngine

import mylib.misc.FibonacciHeap

import scala.collection.mutable

package onthefly {
	/**
	 * An interface to try generated on-the-fly knowledge. 
	 */
	class OnTheFly(ie: IEngine, ae: AEngine) {
		
		private[this] val assumptions = ae.allAssumptions
		
		// contruct terms in advance, necessary for on-the-fly knowledge generation.
		alignPaths.init(assumptions, ie)
		contextCandidates.init(assumptions, ie)
		
		// save the status of current ie
		private[this] val dump = ie.dump()
		
		// lock ie to make sure its status not changed during on-the-fly knowledge generation.
		ie.locked = true
		
		// generate aligned paths
		private[this] val algnps = alignPaths(ae.allAssumptions, ie)
		
		private[this] val kdgcache = mutable.Map.empty[PathAlignment, Declarative]
		
		/**
		 * Add on-the-fly knowledge.
		 * 
		 * Given a score function, a threshold of score, and a status checker, 
		 * this function will generate a copy of `ie`, add on-the-fly knowledge to it, 
		 * starting from the one with highest score, until the status checker returns 
		 * `true` or there is no more on-the-fly knowledge. Then the function reports 
		 * all generated knowledge and the final value of the status checker.
		 * 
		 * Note that this function keeps the original `ie` uninfected so you can try 
		 * many kinds of score functions in many times.
		 * 
		 * @param score The score function.
		 * @param threshold Path alignments with scores lower than the threshold will be discarded.
		 * @param until Keep adding on-the-fly knowledge (to a copy of `ie`) until this get `true` or no more knowledge available.
		 * @return The final status of `until`, and a list of generated on-the-fly knowledge. 
		 */
		def tryKnowledge(score: (PathAlignment) => Double, threshold: Double, until: (IEngine) => Boolean) = {
			
			val xie = new IEngine
			xie.load(dump.copy)
			
			val heap = new FibonacciHeap[PathAlignment]
			for (algn <- algnps) {
				val scr = score(algn)
				if (scr > threshold) heap.insert(algn, -scr)
			}
			
			var rec = Nil:List[(Set[(PathAlignment, Declarative)], Double)]
			def loop(): Boolean = {
				if (until(xie)) {
					true
				} else if (heap.isEmpty) {
					false
				} else {
					val minv = heap.minNode.value
					var algs = Set.empty[(PathAlignment, Declarative)]
					while (!heap.isEmpty && heap.minNode.value == minv) {
						val x = heap.removeMin()
						val kdg = kdgcache.getOrElseUpdate(x, {
							val ctx = contextCandidates(x.subPath)
							x.toOnTheFly(ie, ctx)
						})
						algs += ((x, kdg))
					}
					rec = (algs, -minv) :: rec
					algs.foreach(_._2.toStatements.foreach(xie.claimStatement(_)))
					loop()
				}
			}
			
			(loop(), rec.reverse)
		}
		
	}
	
}
