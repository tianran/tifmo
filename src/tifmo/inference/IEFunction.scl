package tifmo


package inference {
	/**
	 * Interface for implementing a user-defined function. 
	 */
	abstract class IEFunction extends Serializable {
		/**
		 * The dimension of the output term.
		 * 
		 * @param tms Input terms.
		 * @param param Any other parameter.
		 */
		def headDim(tms: Seq[Term], param: Any): Dimension
		/**
		 * Definition of the function. Write your own axioms here.
		 * 
		 * @param ie The inference engine.
		 * @param tms Input terms.
		 * @param param Any other parameter.
		 */
		def applyFunc(ie: IEngineCore, tms: Seq[TermIndex], param: Any): Unit
	}
	
}
