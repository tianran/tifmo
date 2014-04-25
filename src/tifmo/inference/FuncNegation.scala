package tifmo

import dcstree.Executor
import dcstree.Relation

package inference {
	
	import RAConversion._
	
	/**
	 * Negation. 
	 * 
	 * Negation is not implemented as a complement set.
	 * Instead, we implement (i) disjointness: `neg(a) || a`, 
	 * and (ii) downward monotonicity: 
	 * `a \subset b, neg(a), neg(b) => neg(b) \subset neg(a)`. 
	 */
	object FuncNegation extends IEFunction {
		
		def headDim(tms: Seq[Term], param: Any) = tms(1).dim
		
		def applyFunc(ie: IEngineCore, tms: Seq[TermIndex], param: Any) {
			assert(param == null)
			tms match {
				case Seq(h, a) => {
					ie.claimDisjoint(h, a, Debug_SimpleRuleTrace("FuncNegation", ie.getNewPredID()))
					ie.claimRL(h, RelNegation, a, Debug_SimpleRuleTrace("FuncNegation", ie.getNewPredID()))
				}
				case _ => throw new Exception("FuncNegation error!")
			}
		}
	}
	
	private[inference] object RelNegation extends Relation {
		
		def execute[T](ex: Executor, a: T, b: T) {
			(ex, a, b) match {
				case (ie:IEngineCore, xa:TermIndex, xb:TermIndex) => {
					ie.foreachSuperset(xb, Seq(xa), rRelNeg1)
				}
				case _ => {}
			}
		}
	}
	
	private[inference] object rRelNeg1 extends RuleDo[IEPredSubsume] {
		def apply(ie: IEngineCore, pred: IEPredSubsume, args: Seq[RuleArg]) {
			ie.foreachXRLB(pred.superset, args, rRelNeg0)
		}
	}
	
	private[inference] object rRelNeg0 extends RuleDo[IEPredRL] {
		def apply(ie: IEngineCore, pred: IEPredRL, args: Seq[RuleArg]) {
			args match {
				case Seq(RuleArg(xa:TermIndex)) => {
					if (pred.rl == RelNegation) {
						ie.claimSubsume(pred.a, xa, Debug_SimpleRuleTrace("FuncNegation", ie.getNewPredID()))
					}
				}
				case _ => throw new Exception("rRelNeg0 error!")
			}
		}
	}
	
}
