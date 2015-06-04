package arx.macros

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/9/15
 * Time: 12:53 PM
 */

import scala.annotation.{Annotation, compileTimeOnly, StaticAnnotation}
import scala.language.experimental.macros
import reflect.macros.whitebox
import reflect.macros.blackbox

/**
 * This macro modifies any field of the annotated class to have an explicit getter and setter. The getter returns
 * the value as normal, the setter updates the value, but also triggers a call to <code>fieldModified(...)</code> if
 * the new value does not match the old. We use this for networking purposes, to detect which objects need to be
 * re-synced to the client.
 * <br />
 * Fields that already have an explicit setter/getter will be ignored, it is assumed that the implementor will call
 * <code>fieldModified(...)</code> directly if that is desired behavior.
 */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class NetworkedAuxData extends StaticAnnotation {
	def macroTransform(annottees: Any*) : Any  = macro NetworkedAuxDataImpl.transformAnnottees
}

object NetworkedAuxDataImpl {

	def transformAnnottees(c: whitebox.Context)(annottees: c.Tree*) = {
		val tmpTree = annottees.head

		import c.universe._

		tmpTree match {
			case cd : ClassDef => {
				val q"class $className extends ..$bases { ..$body }" = cd

				val fields = (body : List[_]).collect { case vd : ValDef => vd }
				val methods = (body : List[_]).collect { case dd : DefDef => dd }

				val augmentFields = fields
					// This is hideous, but I didn't see any better way to extract out this information, unfortunately
					.filterNot ( field => field.mods.annotations.exists( annot => annot.toString().contains(classOf[NonTriggering].getSimpleName)))
					.filter ( field => {
					val simpleName = field.name.toString
					val cutName = simpleName.dropWhile(_ == '_')
					val checked = c.typecheck(field.duplicate).asInstanceOf[ValDef]

					checked.mods.hasFlag(Flag.MUTABLE) &&
					! methods.exists (method => {
						val mn = method.name.toString
						mn.endsWith(cutName + "_$eq")
					})
				})

				val replacedFields = augmentFields.map(f => {
					val name = f.name
					val newName = TermName("_" + name.toString)
					val assignment = f.rhs
					q"var $newName : ${f.tpt} = $assignment"
				})

				val newSetters = augmentFields.map (f => {
					val variableName = TermName("_" + f.name.toString)
					val setterName = TermName(f.name.toString + "_$eq")
					val checked = c.typecheck(f.duplicate)
					checked match {
						case vd : ValDef => {
							val typev = vd.tpt
							q"""def $setterName (x : $typev) : Unit = {
			  						if ($variableName != x) {
										$variableName = x
										fieldModified()
									}
								}
							 """
						}
						case _ => throw new IllegalStateException("WAT")
					}
				})

				val newGetters = augmentFields.map (f => {
					val getterName = TermName(f.name.toString)
					val varName = TermName(s"_${f.name.toString}")
					val checked = c.typecheck(f.duplicate)
					checked match {
						case vd : ValDef => {
							val typev = vd.tpt
							q"def $getterName : $typev = { $varName }"
						}
						case _ => throw new IllegalStateException("WAT")
					}
				})

				val nonModifiedBody = (body : List[Tree]).filterNot(x => augmentFields.contains(x))

				val resultant = q"""
					class $className extends ..$bases {
						..$nonModifiedBody

						..$newSetters

  						..$newGetters

		  				..$replacedFields
					}
				"""

				println("NetworkedAuxData modifications complete for class " + className)
				println("Augmented fields {")
				for (field <- augmentFields) {
					println(s"\t- ${field.name.toString()}")
				}
				println("}")

				showCode(resultant)

				resultant
			}
		}
	}
}




//@compileTimeOnly("enable macro paradise to expand macro annotations")
//class DelegateToThing(fieldName : String) extends StaticAnnotation {
//	def macroTransform(annottees: Any*) : Any  = macro DelegateToThingImpl.transformAnnottees
//}
//
//object DelegateToThingImpl {
//	def transformAnnottees(c: whitebox.Context)(annottees: c.Tree*) = {
//		val tmpTree = annottees.head
//
//		import c.universe._
//
//		tmpTree match {
//			case cd : ClassDef => {
//				val q"class $className extends ..$bases { ..$body }" = cd
//
//				val fields = (body : List[_]).collect { case vd : ValDef => vd }
//				val methods = (body : List[_]).collect { case dd : DefDef => dd }
//
//				println("fields : " + fields)
//				println("methods : " + methods)
//
//				val resultant = q"""
//					class $className extends ..$bases {
//						..$nonModifiedBody
//
//						..$newSetters
//
//  						..$newGetters
//
//		  				..$replacedFields
//					}
//				"""
//
//				showCode(resultant)
//
//				resultant
//			}
//		}
//	}
//}
//
//
