package tifmo

import dcstree.Relation

import scala.collection.mutable

package inference {
	/**
	 * An inference engine term. 
	 */
	class Term extends Serializable {
		
		private[inference] var idx = null:TermIndex
		
		/**
		 * Returns the index of this term. 
		 * 
		 * Term index is crucial for fast matcing in the inference engine. 
		 * When two terms are merged, their indices are merged, and the 
		 * return value of this function is updated automatically. 
		 */
		def index: TermIndex = {
			if (idx != idx.holder.idx) idx = idx.holder.index
			idx
		}
		
		def dim = index.dim
		
		/**
		 * Whether this is a W-term (all terms of the same dimension are subsumed by a W-term).
		 */
		def isW = index.isW
		
		/**
		 * Returns if the term is known to be non-empty.
		 */
		def knownNE = index.knownNE
		
		/**
		 * Returns all currently known supersets of this term (including itself).
		 */
		def superSets = index.superSets.map(_.holder)
		
		/**
		 * Returns if `that` is known to be a superset of this term.
		 */
		def hasSuper(that: Term) = index.hasSuper(that.index)
		
		/**
		 * Returns all currently known subsets of this term (including itself).
		 */
		def subSets = index.subSets.map(_.holder)
		
		/**
		 * Returns if `that` is known to be a subset of this term.
		 */
		def hasSub(that: Term) = index.hasSub(that.index)
		
		/**
		 * Returns all sets which are currently known to be disjoint to this term.
		 */
		def disjointSets = index.disjointSets.map(_.holder)
		
		/**
		 * Returns if `that` is known to be disjoint to this term.
		 */
		def disjointTo(that: Term) = index.disjointTo(that.index)
		
		/**
		 * Returns if this term is known to be disjoint to itself.
		 */
		def selfDisjoint = index.selfDisjoint
		
		def allARLX = index.allARLX.map(x => (x._1, x._2.holder))
		
		def hasARLX(rl: Relation, xb: Term) = index.hasARLX(rl, xb.index)
		
		def allXRLB = index.allXRLB.map(x => (x._1.holder, x._2))
		
		def hasXRLB(xa: Term, rl: Relation) = index.hasXRLB(xa.index, rl)
		
		override def toString = index.toString
	}
	
	class TermIndex(val dim: Dimension) extends Serializable {
		
		val holder = new Term
		holder.idx = this
		
		private[this] var wflag = false
		private[inference] def setwflag() { wflag = true }
		def isW = wflag
		
		def valid = (holder.index == this)
		
		///////////////////////////////////////////////
		
		private[inference] val kne = mutable.Set.empty[IEPredNonEmpty]
		def index_kne = kne.toSet
		private[inference] val assub = mutable.Set.empty[IEPredSubsume]
		def index_assub = assub.toSet
		private[inference] val assuper = mutable.Set.empty[IEPredSubsume]
		def index_assuper = assuper.toSet
		private[inference] val djts = mutable.Set.empty[IEPredDisjoint]
		def index_djts = djts.toSet
		private[inference] val iscps = mutable.Set.empty[IEPredCP]
		def index_iscps = iscps.toSet
		private[inference] val mkcps = mutable.Set.empty[IEPredCP]
		def index_mkcps = mkcps.toSet
		private[inference] val ispis = mutable.Set.empty[IEPredPI]
		def index_ispis = ispis.toSet
		private[inference] val mkpis = mutable.Set.empty[IEPredPI]
		def index_mkpis = mkpis.toSet
		private[inference] val isins = mutable.Set.empty[IEPredIN]
		def index_isins = isins.toSet
		private[inference] val mkins = mutable.Set.empty[IEPredIN]
		def index_mkins = mkins.toSet
		
		private[inference] val funcs = mutable.Set.empty[IEPredFunc]
		def index_funcs = funcs.toSet
		
		private[inference] val asarl = mutable.Set.empty[IEPredRL]
		def index_asarl = asarl.toSet
		private[inference] val asrlb = mutable.Set.empty[IEPredRL]
		def index_asrlb = asrlb.toSet
		
		private[this] val ppmap = Map(
			"ne" -> kne,
			"sub" -> assub, 
			"super" -> assuper, 
			"djt" -> djts, 
			"iscp" -> iscps, 
			"mkcp" -> mkcps, 
			"ispi" -> ispis, 
			"mkpi" -> mkpis, 
			"isin" -> isins, 
			"mkin" -> mkins, 
			"func" -> funcs, 
			"arl" -> asarl, 
			"rlb" -> asrlb
		)
		private[inference] def predPool[T <: IEPred](name: String) = 
			ppmap(name).asInstanceOf[mutable.Set[T]]
		
		///////////////////////////////////////////////
		
		private[inference] val neTriggers = mutable.Set.empty[Trigger[IEPredNonEmpty]]
		private[inference] val subTriggers = mutable.Set.empty[Trigger[IEPredSubsume]]
		private[inference] val superTriggers = mutable.Set.empty[Trigger[IEPredSubsume]]
		private[inference] val djtTriggers = mutable.Set.empty[Trigger[IEPredDisjoint]]
		private[inference] val iscpTriggers = mutable.Set.empty[Trigger[IEPredCP]]
		private[inference] val mkcpTriggers = mutable.Set.empty[Trigger[IEPredCP]]
		private[inference] val ispiTriggers = mutable.Set.empty[Trigger[IEPredPI]]
		private[inference] val mkpiTriggers = mutable.Set.empty[Trigger[IEPredPI]]
		private[inference] val isinTriggers = mutable.Set.empty[Trigger[IEPredIN]]
		private[inference] val mkinTriggers = mutable.Set.empty[Trigger[IEPredIN]]
		
		private[inference] val arlTriggers = mutable.Set.empty[Trigger[IEPredRL]]
		private[inference] val rlbTriggers = mutable.Set.empty[Trigger[IEPredRL]]
		
		private[this] val tpmap = Map(
			"ne" -> neTriggers,
			"sub" -> subTriggers, 
			"super" -> superTriggers, 
			"djt" -> djtTriggers, 
			"iscp" -> iscpTriggers, 
			"mkcp" -> mkcpTriggers, 
			"ispi" -> ispiTriggers, 
			"mkpi" -> mkpiTriggers, 
			"isin" -> isinTriggers, 
			"mkin" -> mkinTriggers, 
			"arl" -> arlTriggers, 
			"rlb" -> rlbTriggers
		)
		private[inference] def triggerPool[T <: IEPred](name: String) = 
			tpmap(name).asInstanceOf[mutable.Set[Trigger[T]]]
		
		private[inference] val disposers = mutable.Set.empty[() => Unit]
		
		///////////////////////////////////////////////
		
		def knownNE = (!kne.isEmpty)
		
		def superSets = assub.map(_.superset).toSet
		
		def hasSuper(that: TermIndex) = assub.contains(IEPredSubsume(this, that))
		
		def subSets = assuper.map(_.subset).toSet
		
		def hasSub(that: TermIndex) = assuper.contains(IEPredSubsume(that, this))
		
		def disjointSets = djts.map(_.b).toSet
		
		def disjointTo(that: TermIndex) = djts.contains(IEPredDisjoint(this, that))
		
		def selfDisjoint = disjointTo(this)
		
		def allARLX = asarl.map(x => (x.rl, x.b)).toSet
		
		def hasARLX(rl: Relation, xb: TermIndex) = asarl.contains(IEPredRL(this, rl, xb))
		
		def allXRLB = asrlb.map(x => (x.a, x.rl)).toSet
		
		def hasXRLB(xa: TermIndex, rl: Relation) = asrlb.contains(IEPredRL(xa, rl, this))
		
	}
	
}
