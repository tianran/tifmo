package tifmo

import dcstree.SemRole

package inference {
	
	import RAConversion._
	
	/**
	 * Division (QuantifierALL). 
	 * 
	 * `tms = Seq(h, a, b)`, `param = r` means that `h = q^r(a, b)`.
	 */
	object FuncDIall extends IEFunction {
		
		def headDim(tms: Seq[Term], param: Any) = {
			val r = param.asInstanceOf[SemRole]
			Dimension(tms(1).dim.decrease(r))._1
		}
		
		def applyFunc(ie: IEngineCore, tms: Seq[TermIndex], param: Any) {
			val r = param.asInstanceOf[SemRole]
			tms match {
				case Seq(h, a, b) => {
					val hrs = a.dim.decrease(r)
					ie.claimSubsume(h, ie.getPI(a.holder, hrs).index, Debug_SimpleRuleTrace("FuncDIall", ie.getNewPredID()))
					val (hdim, hr) = Dimension(hrs)
					ie.claimSubsume(ie.getCP(Set((h.holder, hr), (b.holder, r))).index, a, Debug_SimpleRuleTrace("FuncDIall", ie.getNewPredID()))
					ie.foreachSubset(a, Seq(b, r, h), rDI2)
				}
				case _ => throw new Exception("FuncDIall error!")
			}
		}
	}
	
	private[inference] object rDI2 extends RuleDo[IEPredSubsume] {
		def apply(ie: IEngineCore, pred: IEPredSubsume, args: Seq[RuleArg]) {
			ie.foreachIsCP(pred.subset, args, rDI1)
		}
	}
	
	private[inference] object rDI1 extends RuleDo[IEPredCP] {
		def apply(ie: IEngineCore, pred: IEPredCP, args: Seq[RuleArg]) {
			args match {
				case Seq(RuleArg(b:TermIndex), RuleArg(r:SemRole), RuleArg(h:TermIndex)) => {
					pred.comp.find(_._2 == r) match {
						case Some(x) => ie.ifSubsume(b, x._1, Seq(pred.comp - x, h), rDI0)
						case None => {}
					}
				}
				case _ => throw new Exception("rDI1 error!")
			}
		}
	}
	
	private[inference] object rDI0 extends RuleDo[IEPredSubsume] {
		def apply(ie: IEngineCore, pred: IEPredSubsume, args: Seq[RuleArg]) {
			args match {
				case Seq(RuleArg(tc:Set[(TermIndex, SemRole)]), RuleArg(h:TermIndex)) => {
					if (tc.size <= 1) {
						ie.claimSubsume(tc.head._1, h, Debug_SimpleRuleTrace("FuncDIall", ie.getNewPredID()))
					} else {
						ie.constructCP(tc.map(y => (y._1.holder, y._2)), Seq(h), (x:Term, args: Seq[RuleArg]) => {
							ie.claimSubsume(x.index, h, Debug_SimpleRuleTrace("FuncDIall", ie.getNewPredID()))
						})
					}
				}
				case _ => throw new Exception("rDI0 error!")
			}
		}
	}
	
}
