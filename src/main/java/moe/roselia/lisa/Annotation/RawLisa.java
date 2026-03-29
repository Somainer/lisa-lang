package moe.roselia.lisa.Annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The annotation to indicate a module or object is a lisa module,
 * the interpreter will not convert Expression to Any
 * when calling functions of this module.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD, ElementType.TYPE})
public @interface RawLisa {
}
