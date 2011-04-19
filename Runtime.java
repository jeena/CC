import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.IOException;

class Runtime {
  
  public static void main(String [] argv) {
    Runtime.printString("Runtime test\nWrite a int: ");
    
    int i = Runtime.readInt();
    Runtime.printString("Int: ");
    Runtime.printInt(i);

    Runtime.printString("\nWrite a double: ");
    double d = Runtime.readDouble();
    Runtime.printString("Double: ");
    Runtime.printDouble(d);
    Runtime.printString("\n");
  }
  
  public static void printInt(int i) {
    System.out.print(i);
  }
  
  public static void printDouble(double d) {
    System.out.print(d);
  }
  
  public static void printString(String s) {
    System.out.print(s);
  }
  
  public static int readInt() {
    String line = null;
    int val = 0;
    try {
      BufferedReader is = new BufferedReader(
        new InputStreamReader(System.in));
      line = is.readLine();
      val = Integer.parseInt(line);
    } catch (NumberFormatException ex) {
      System.err.println("Not a valid number: " + line);
    } catch (IOException e) {
      System.err.println("Unexpected IO ERROR: " + e);
    }
    
    return val;
  }
  
  public static double readDouble() {
    InputStreamReader convert = new InputStreamReader(System.in);
    BufferedReader stdin = new BufferedReader(convert);
    String instr = null;
    double val = 0;

    try {
      instr = stdin.readLine();
      val = new Double(instr).doubleValue();;
    } catch(NumberFormatException ex) {
      System.err.println("Not a valid number: " + instr);      
    } catch(IOException e) {
      System.err.println("Unexpected IO ERROR: " + e);
    }
    
    return val;
  }
  
}