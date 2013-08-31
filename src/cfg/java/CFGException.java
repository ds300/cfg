package cfg.java;

public class CFGException extends Exception {

  public Object badValue;
  public Object keyPath;
  public Exception originalException;

  public CFGException (String msg, Object keyPath, Object badValue, Exception originalException) {
    super(msg);
    this.keyPath = keyPath;
    this.badValue = badValue;
    this.originalException = originalException;
  }

  @Override
  public String toString() {
    String s = "CFGException: " + getMessage();
    if (keyPath != null) {
      s += "\nBad value for field " + keyPath.toString() + ": " + badValue;
    }
    if (originalException != null) {
      s += "\nOriginal Exception:\n" + originalException.toString();
    }
    return s;
  }
}