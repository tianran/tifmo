package tifmo

import dcstree.SemRole
import dcstree.WordBase
import dcstree.Denotation
import dcstree.DenotationWordSign

package inference {
	/**
	 * Serializable status of [[tifmo.inference.IEngine]].
	 */
	class IEDump(
		private[inference] val ws: Set[TermIndex], 
		private[inference] val preds: List[IEPred], 
		private[inference] val guards: List[Guard[_ <: IEPred]], 
		private[inference] val wdtm: Map[WordBase, TermIndex], 
		private[inference] val wdrs: Map[WordBase, Set[SemRole]], 
		private[inference] val wspool: Set[DenotationWordSign], 
		private[inference] val dcache: Map[Denotation, TermIndex]
	) extends Serializable {
		
		def copy = {
			
			val tmmap = (for (IEPredSubsume(sub, sup) <- preds; if ws.contains(sup)) yield {
				(sub, new TermIndex(sup.dim))
			}).toMap
			val nws = ws.map(tmmap(_))
			val npreds = preds.map(_.dumpMe(tmmap))
			val nguards = guards.map(_.dumpMe(tmmap))
			val nwdtm = wdtm.mapValues(tmmap(_)).toMap
			val nwdrs = wdrs
			val nwspool = wspool
			val ndcache = dcache.mapValues(tmmap(_)).toMap
			
			new IEDump(nws, npreds, nguards, nwdtm, nwdrs, nwspool, ndcache)
		}
		
	}
	
}

