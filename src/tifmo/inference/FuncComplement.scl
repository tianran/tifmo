package tifmo


package inference {
	
	import RAConversion._
	
	/**
	 * Complement. 
	 * 
	 * `tms = Seq(h, a, tot)` means `h = tot \ a`.
	 */
	object FuncComplement extends IEFunction {
		
		def headDim(tms: Seq[Term], param: Any) = tms(1).dim
		
		def applyFunc(ie: IEngineCore, tms: Seq[TermIndex], param: Any) {
			assert(param == null)
			tms match {
				case Seq(h, a, tot) => {
					ie.claimSubsume(h, tot, Debug_SimpleRuleTrace("FuncComplement", ie.getNewPredID()))
					ie.claimDisjoint(h, a, Debug_SimpleRuleTrace("FuncComplement", ie.getNewPredID()))
					ie.foreachDisjoint(a, Seq(tot, h), rComp1)
				}
				case _ => throw new Exception("FuncComplement error!")
			}
		}
	}
	
	private[inference] object rComp1 extends RuleDo[IEPredDisjoint] {
		def apply(ie: IEngineCore, pred: IEPredDisjoint, args: Seq[RuleArg]) {
			args match {
				case Seq(RuleArg(tot:TermIndex), RuleArg(h:TermIndex)) => {
					ie.ifSubsume(pred.b, tot, Seq(h), rComp0)
				}
				case _ => throw new Exception("rComp1 error!")
			}
		}
	}
	
	private[inference] object rComp0 extends RuleDo[IEPredSubsume] {
		def apply(ie: IEngineCore, pred: IEPredSubsume, args: Seq[RuleArg]) {
			args match {
				case Seq(RuleArg(h:TermIndex)) => {
					ie.claimSubsume(pred.subset, h, Debug_SimpleRuleTrace("FuncComplement", ie.getNewPredID()))
				}
				case _ => throw new Exception("rComp0 error!")
			}
		}
	}
	
}
