package compiler.exceptions;

public class IndentationException extends RuntimeException{

    public IndentationException(){ super(); }
    public IndentationException(String message) { super(message); }
    public IndentationException(String message, Throwable cause) { super(message, cause); }
    public IndentationException(Throwable cause) { super(cause); }

}
