package compiler.exceptions;

public class ContainerException extends RuntimeException{

    public ContainerException(){ super(); }
    public ContainerException(String message) { super(message); }
    public ContainerException(String message, Throwable cause) { super(message, cause); }
    public ContainerException(Throwable cause) { super(cause); }

}
