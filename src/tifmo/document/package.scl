package tifmo

/**
 * This package provides an interface to generate DCS trees and declaratives.
 * 
 * The process is like this: 
 * 
 * (i) Make tokens. A [[tifmo.document.Token]] is constructed from its surface form, 
 * and we should add two pieces of information: `word` and `corefID`. The `word` field 
 * can usually be obtained from a lemmatizer, and `corefID` is obtained from coreference 
 * resolution.
 * 
 * (ii) Make document. A [[tifmo.document.Document]] is constructed from an [[scala.collection.immutable.IndexedSeq]] 
 * of [[tifmo.document.Token]]s. 
 * 
 * (iii) Add dependencies. From a [[tifmo.document.Document]], you can call `getTokenNode` 
 * method to get the corresponding [[tifmo.document.TokenNode]], which is mostly parallel to 
 * [[tifmo.dcstree.DCSTreeNode]], but you can modify most fields as you will. Information 
 * on dependencies are added using the [[tifmo.document.TokenNode]] method `addChild`. In 
 * case of zero anaphora, you can also make new [[tifmo.document.TokenNode]] by your own, 
 * instead of calling `Document.getTokenNode` method.
 * 
 * (iv) Make a dictionary of possible semantic roles of each word. This can be obtained from 
 * the heuristic function `tentRoles`.
 * 
 * (v) Set a calculation order of semantic roles for each node (the `rseq` field). This can be 
 * obtained from the heuristic function `tentRoleOrder` (which uses positions of tokens to 
 * disambiguate the calculation order or scopes of quantifiers, mostly suited for English).
 * 
 * (vi) Call `Document.makeDeclaratives` method to get a set of [[tifmo.dcstree.Declarative]].
 * 
 * Extensions of DCS trees, including the implementation of coreferences [[tifmo.document.SelCoref]], 
 * numerics [[tifmo.document.SelNum]], and superlatives [[tifmo.document.SelSup]], as well as 
 * a simple treatment of conjunctions (the `conj` field and `addConjunction` method of 
 * [[tifmo.document.TokenNode]]), are also found in this package. 
 */
package object document {}
