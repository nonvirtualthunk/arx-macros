package arx.macros

import scala.annotation.Annotation

/**
 * Any field annotated with this will be ignored by the @NetworkedAuxData annotation when it modifies a class. We use
 * this on fields that do not warrant a full sync when they change, or fields that we manage manually.
 */
class NonTriggering extends Annotation
