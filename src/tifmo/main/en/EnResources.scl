package tifmo

import mylib.misc.longestCommSeq
import mylib.res.en.EnWordNet
import mylib.res.en.EnWordNet.SemEntry
import mylib.res.en.EnWordNet.WNRelation._

import scala.collection.mutable

package main.en {
	/**
	 * Manager of various language/knowledge resources.
	 * 
	 * If you'd like to experiment on new resources, this should be the first 
	 * class you want to extend.
	 * 
	 * This class provides four methods (namely, `synonym`, `semrel`, `hyponym`, `antonym`). 
	 * You can simply override these methods to utilize your own resources.
	 * 
	 * All lookup functions in this class are cached to save execution time, and appropriately 
	 * synchronized to provide safe re-entry in concurrent programming.
	 * 
	 * Currently, the implementation of this class has integrated the following 
	 * resources/heuristics:
	 *  - surface string matching for named entities
	 *  - subsumption of time tags recognized by Stanford CoreNLP
	 *  - synonym, hyponym and sematically related words by WordNet
	 * See protected methods for more details. 
	 */
	class EnResources {
		
		private[this] val wordnetCacheSyn = mutable.Map.empty[EnWord, Set[SemEntry]]
		protected def lookupWordNetSyn(x: EnWord) = {
			if (x.mypos == "D" || x.isStopWord) {
				Set.empty[SemEntry]
			} else {
				val pre = wordnetCacheSyn.synchronized {
					wordnetCacheSyn.getOrElse(x, null)
				}
				if (pre != null) {
					pre
				} else {
					val tmp = x.mypos match {
						case "R" => {
							val prepre = EnWordNet.synsets(x.lemma, "R")
							val pre = if (prepre.isEmpty) EnWordNet.synsets(x.lemma, "J") else prepre
							if (pre.isEmpty) EnWordNet.synsets(x.lemma, "O") else pre
						}
						case "J" | "N" | "V" => {
							val pre = EnWordNet.synsets(x.lemma, x.mypos)
							if (pre.isEmpty) EnWordNet.synsets(x.lemma, "O") else pre
						}
						case _ => EnWordNet.synsets(x.lemma, x.mypos)
					}
					val ret = if (x.mypos == "R") tmp ++ EnWordNet.lexical(tmp, DERIVED_FROM_ADJ) else tmp
					wordnetCacheSyn.synchronized {
						wordnetCacheSyn(x) = ret
					}
					ret
				}
			}
		}
		private[this] val wordnetCacheHypo = mutable.Map.empty[EnWord, Set[SemEntry]]
		protected def lookupWordNetHypo(x: EnWord) = {
			val pre = wordnetCacheHypo.synchronized {
				wordnetCacheHypo.getOrElse(x, null)
			}
			if (pre != null) {
				pre
			} else {
				val syn = lookupWordNetSyn(x)
				val hypos = EnWordNet.semantic(syn, HYPONYM)
				val hypo_insts = EnWordNet.semantic(syn, HYPONYM_INSTANCE)
				val mero_mems = EnWordNet.semantic(syn, MERONYM_MEMBER)
				val mero_parts = EnWordNet.semantic(syn, MERONYM_PART)
				val mero_substs = EnWordNet.semantic(syn, MERONYM_SUBSTANCE)
				val ret = syn ++ hypos ++ hypo_insts ++ mero_mems ++ mero_parts ++ mero_substs
				wordnetCacheHypo.synchronized {
					wordnetCacheHypo(x) = ret
				}
				ret
			}
		}
		private[this] val wordnetCacheHyper = mutable.Map.empty[EnWord, Set[SemEntry]]
		protected def lookupWordNetHyper(x: EnWord) = {
			val pre = wordnetCacheHyper.synchronized {
				wordnetCacheHyper.getOrElse(x, null)
			}
			if (pre != null) {
				pre
			} else {
				val syn = lookupWordNetSyn(x)
				val hypers = EnWordNet.semantic(syn, HYPERNYM)
				val hyper_insts = EnWordNet.semantic(syn, HYPERNYM_INSTANCE)
				val holo_mems = EnWordNet.semantic(syn, HOLONYM_MEMBER)
				val holo_parts = EnWordNet.semantic(syn, HOLONYM_PART)
				val holo_substs = EnWordNet.semantic(syn, HOLONYM_SUBSTANCE)
				val ret = syn ++ hypers ++ hyper_insts ++ holo_mems ++ holo_parts ++ holo_substs
				wordnetCacheHyper.synchronized {
					wordnetCacheHyper(x) = ret
				}
				ret
			}
		}
		private[this] val wordnetCacheAnt = mutable.Map.empty[EnWord, Set[SemEntry]]
		protected def lookupWordNetAnt(x: EnWord) = {
			val pre = wordnetCacheAnt.synchronized {
				wordnetCacheAnt.getOrElse(x, null)
			}
			if (pre != null) {
				pre
			} else {
				val syn = lookupWordNetSyn(x)
				val ret = EnWordNet.lexical(syn, ANTONYM)
				wordnetCacheAnt.synchronized {
					wordnetCacheAnt(x) = ret
				}
				ret
			}
		}
		private[this] val wordnetCacheEnt = mutable.Map.empty[EnWord, Set[SemEntry]]
		protected def lookupWordNetEnt(x: EnWord) = {
			val pre = wordnetCacheEnt.synchronized {
				wordnetCacheEnt.getOrElse(x, null)
			}
			if (pre != null) {
				pre
			} else {
				val syn = lookupWordNetSyn(x)
				val ret = EnWordNet.semantic(syn, ENTAILMENT)
				wordnetCacheEnt.synchronized {
					wordnetCacheEnt(x) = ret
				}
				ret
			}
		}
		private[this] val wordnetCacheDeriv = mutable.Map.empty[EnWord, Set[SemEntry]]
		protected def lookupWordNetDeriv(x: EnWord) = {
			val pre = wordnetCacheDeriv.synchronized {
				wordnetCacheDeriv.getOrElse(x, null)
			}
			if (pre != null) {
				pre
			} else {
				val syn = lookupWordNetSyn(x)
				val ret = EnWordNet.lexical(syn, DERIVATIONALLY_RELATED)
				wordnetCacheDeriv.synchronized {
					wordnetCacheDeriv(x) = ret
				}
				ret
			}
		}
		
		def synonym(a: EnWord, b: EnWord) = {
			(a.isNamedEntity && b.isNamedEntity && {
				longestCommSeq.rateMin[String](a.lemma.split(" "), b.lemma.split(" ")) > 0.5
			}) || (!a.isNamedEntity && !b.isNamedEntity && {
				!(lookupWordNetSyn(a) intersect lookupWordNetSyn(b)).isEmpty
			})
		}
		
		def semrel(a: EnWord, b: EnWord) = {
			(a.isNamedEntity && b.isNamedEntity && {
				longestCommSeq.rateMin[String](a.lemma.split(" "), b.lemma.split(" ")) > 0.5
			}) || (!a.isNamedEntity && !b.isNamedEntity && {
				!(lookupWordNetSyn(a) intersect lookupWordNetSyn(b)).isEmpty || 
					!(lookupWordNetSyn(a) intersect lookupWordNetAnt(b)).isEmpty || 
					!(lookupWordNetSyn(b) intersect lookupWordNetAnt(a)).isEmpty || 
					!(lookupWordNetSyn(a) intersect lookupWordNetDeriv(b)).isEmpty || 
					!(lookupWordNetSyn(b) intersect lookupWordNetDeriv(a)).isEmpty
			})
		}
		
		def hyponym(a: EnWord, b: EnWord) = {
			(a.mypos == "D" && b.mypos == "D" && {
				val asp = a.lemma.split(" ")
				val bsp = b.lemma.split(" ")
				def comp(a0: String, b0: String) = {
					b0.length <= a0.length && (0 until b0.length).forall(i => b0.charAt(i) == a0.charAt(i) || (b0.charAt(i) == 'X' && a0.charAt(i).isDigit))
				}
				bsp.forall(b0 => asp.exists(a0 => comp(a0, b0)))
			}) || (a.mypos == "N" && b.mypos == "N" && {
				!(lookupWordNetHyper(a) intersect lookupWordNetHypo(b)).isEmpty || 
					!(lookupWordNetEnt(a) intersect lookupWordNetSyn(b)).isEmpty
			})
		}
		
		def antonym(a: EnWord, b: EnWord) = {
			!(lookupWordNetSyn(a) intersect lookupWordNetAnt(b)).isEmpty || 
				!(lookupWordNetSyn(b) intersect lookupWordNetAnt(a)).isEmpty
		}

	}
	
}
