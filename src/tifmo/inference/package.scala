package tifmo

/**
 * This package deals with logical inference on abstract denotations and statements.
 * 
 * The main interface is [[tifmo.inference.IEngine]], which can receive statements as 
 * premises and can check if a statement is proven.
 * 
 * An abstract denotation corresponds to a [[tifmo.inference.Term]] in the inference 
 * engine, from which we can get its subsets, supersets, etc.
 * 
 * A [[tifmo.inference.Term]] keeps a [[tifmo.inference.TermIndex]] as indices for all 
 * atomic sentences related to this term. 
 * 
 * An atomic sentence is represented by a [[tifmo.inference.IEPred]] instance.
 * 
 * [[tifmo.inference.IEngine]] is derived from the [[tifmo.inference.IEngineCore]] class, 
 * which does the heavy lifting of logical inference. It also provides an interface for 
 * writing new rules and axioms. 
 */
package object inference {}
