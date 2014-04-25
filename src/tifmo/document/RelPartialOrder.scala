package tifmo

import dcstree.Executor
import dcstree.Relation
import inference.Dimension
import inference.IEngineCore
import inference.TermIndex
import inference.Term
import inference.IEFunction
import inference.RuleDo
import inference.RuleArg
import inference.IEPredRL
import inference.IEPredSubsume
import inference.Debug_SimpleRuleTrace
import inference.RAConversion._

package document {
	
	/**
	 * Partial order relation. 
	 */
	case class RelPartialOrder(name: String) extends Relation {
		
		def execute[T](ex: Executor, a: T, b: T) {
			(ex, a, b) match {
				case (ie:IEngineCore, xa:TermIndex, xb:TermIndex) => {
					assert(xa.dim == xb.dim)
					ie.claimFunc(FuncPartialOrder, Seq(xa), (name, xa.dim), Debug_SimpleRuleTrace("RelPartialOrder", ie.getNewPredID()))
					ie.claimFunc(FuncPartialOrder, Seq(xb), (name, xb.dim), Debug_SimpleRuleTrace("RelPartialOrder", ie.getNewPredID()))
				}
				case _ => {}
			}
		}
	}
	
	private[document] object FuncPartialOrder extends IEFunction {
		
		def headDim(tms: Seq[Term], param: Any) = param.asInstanceOf[(String, Dimension)]._2
		
		def applyFunc(ie: IEngineCore, tms: Seq[TermIndex], param: Any) {
			val name = param.asInstanceOf[(String, Dimension)]._1
			ie.foreachARLX(tms.head, Seq(name), rFuncPO3)
			ie.foreachXRLB(tms.head, Seq(name), rFuncPO2)
			ie.foreachSubset(tms.head, Seq(name), rFuncPO1)
		}
	}
	
	private[document] object rFuncPO3 extends RuleDo[IEPredRL] {
		def apply(ie: IEngineCore, pred: IEPredRL, args: Seq[RuleArg]) {
			args match {
				case Seq(RuleArg(name:String)) => pred.rl match {
					case RelPartialOrder(nm) => if (nm == name) {
						var task = Nil:List[() => Unit]
						for ((x, rl @ RelPartialOrder(xnm)) <- pred.a.allXRLB; if xnm == name) {
							task = (() => ie.claimRL(x, rl, pred.b, Debug_SimpleRuleTrace("Partial Order transitivity", ie.getNewPredID()))) :: task
						}
						task.foreach(_())
					}
					case _ => {}
				}
				case _ => throw new Exception("rFuncPO3 error!")
			}
		}
	}
	
	private[document] object rFuncPO2 extends RuleDo[IEPredRL] {
		def apply(ie: IEngineCore, pred: IEPredRL, args: Seq[RuleArg]) {
			args match {
				case Seq(RuleArg(name:String)) => pred.rl match {
					case RelPartialOrder(nm) => if (nm == name) {
						var task = Nil:List[() => Unit]
						for ((rl @ RelPartialOrder(xnm), x) <- pred.b.allARLX; if xnm == name) {
							task = (() => ie.claimRL(pred.a, rl, x, Debug_SimpleRuleTrace("Partial Order transitivity", ie.getNewPredID()))) :: task
						}
						task.foreach(_())
					}
					case _ => {}
				}
				case _ => throw new Exception("rFuncPO2 error!")
			}
		}
	}
	
	private[document] object rFuncPO1 extends RuleDo[IEPredSubsume] {
		def apply(ie: IEngineCore, pred: IEPredSubsume, args: Seq[RuleArg]) {
			args match {
				case Seq(RuleArg(name:String)) => {
					var task = Nil:List[() => Unit]
					for ((rl @ RelPartialOrder(xnm), x) <- pred.superset.allARLX; if xnm == name) {
						task = (() => ie.claimRL(pred.subset, rl, x, Debug_SimpleRuleTrace("Partial Order sub", ie.getNewPredID()))) :: task
					}
					for ((x, rl @ RelPartialOrder(xnm)) <- pred.superset.allXRLB; if xnm == name) {
						task = (() => ie.claimRL(x, rl, pred.subset, Debug_SimpleRuleTrace("Partial Order sub", ie.getNewPredID()))) :: task
					}
					task.foreach(_())
				}
				case _ => throw new Exception("rFuncPO1 error!")
			}
		}
	}
	
	
}
