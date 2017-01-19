
package compiler.exceptions;

public class DeclarationException extends RuntimeException {

    public DeclarationException() {
        super();
    }

    public DeclarationException(String message) {
        super(message);
    }

    public DeclarationException(String message, Throwable cause) {
        super(message, cause);
    }

    public DeclarationException(Throwable cause) {
        super(cause);
    }

}
