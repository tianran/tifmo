package tifmo


package inference {
	
	import RAConversion._
	
	/**
	 * Singleton. 
	 */
	object FuncSingle extends IEFunction {
		
		def headDim(tms: Seq[Term], param: Any) = param.asInstanceOf[Dimension]
		
		def applyFunc(ie: IEngineCore, tms: Seq[TermIndex], param: Any) {
			ie.foreachMkPI(tms.head, Seq.empty[RuleArg], rSingle2)
			ie.foreachSubset(tms.head, Seq.empty[RuleArg], rSingle1)
		}
	}
	
	private[inference] object rSingle2 extends RuleDo[IEPredPI] {
		def apply(ie: IEngineCore, pred: IEPredPI, args: Seq[RuleArg]) {
			assert(args.isEmpty)
			ie.claimFunc(FuncSingle, Seq(pred.head), pred.head.dim, Debug_SimpleRuleTrace("FuncSingle", ie.getNewPredID()))
		}
	}
	
	private[inference] object rSingle1 extends RuleDo[IEPredSubsume] {
		def apply(ie: IEngineCore, pred: IEPredSubsume, args: Seq[RuleArg]) {
			assert(args.isEmpty)
			ie.ifNotEmpty(pred.subset, Seq(pred.superset), rSingle0)
		}
	}
	
	private[inference] object rSingle0 extends RuleDo[IEPredNonEmpty] {
		def apply(ie: IEngineCore, pred: IEPredNonEmpty, args: Seq[RuleArg]) {
			args match {
				case Seq(RuleArg(x:TermIndex)) => {
					ie.claimSubsume(x, pred.term, Debug_SimpleRuleTrace("FuncSingle", ie.getNewPredID()))
				}
				case _ => throw new Exception("rSingle0 error!")
			}
		}
	}
	
}
