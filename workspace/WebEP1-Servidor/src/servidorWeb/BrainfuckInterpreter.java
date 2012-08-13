package servidorWeb;

// A Java Brainfuck Interpreter


import java.io.IOException;
import java.io.PrintStream;
import java.io.InputStream;



/** This class is a brainfuck interpreter written by Sven Stucki.<br>
 *  It lets you execute a bf program step-by-step for easy debugging or visualization or it can interpret the whole program at once.<br>
 *  Release date: 2.7.2009
 *  @author Sven Stucki
 */

public class BrainfuckInterpreter {


    /** Output of the brainfuck programm goes here, default = System.out **/
    public PrintStream ps;  // Output stream
    /** Input of the brainfuck programm is read from here, default = System.in **/
    public InputStream is;  // Input stream

    /** This points to the currently selected memory cell **/
    public int mp;          // Memory pointer
    /** Maximum value of mp **/
    public int mp_max;
    /** This array represents the memory of the brainfuck program **/
    public int[] cell;      // Memory
    
    /** This is the index of the char in the code, which will be executed next **/
    private int cp;         // Code pointer
    /** The code is stored in this String **/
    private String cmd;     // Code
    
    
    /** This String contains all in the code allowed chars **/
    public final String CHARS = "<>+-[].,";


    /** Standard constructor, sets everything to default **/
    public BrainfuckInterpreter() {
        init( 1000 );
    }
    /** With this constructor you can specify the number of memory cells to provide, I/O uses default **/
    public BrainfuckInterpreter( int cnt ) {
        init( cnt );
    }
    /** This constructor lets you specify the number of memory cells, and the I/O Streams **/
    public BrainfuckInterpreter( int cnt, PrintStream p, InputStream i ) {
        ps = p;
        is = i;
        init( cnt );
    }
    
    
    /** Initialises / Resets the interpreter **/
    
    public void init( int cnt ) {
        cell = new int[cnt];
        mp = 0;
        mp_max = 0;
        if( cmd==null )
            cmd = "";
        if( ps==null )
            ps = System.out;
        if( is==null )
            is = System.in;
    }
    
    
    /** Resets the memory field and the pointers **/
    
    public void reset() {
        cell = new int[cell.length];
        mp = 0;
        mp_max = 0;
        cp = 0;
    }
    
    
    /** Set In- / PrintStream **/
    
    public void setInputStream( InputStream ips, PrintStream ops ) {
        is = ips;
        ps = ops;
    }
    
    
    /** Usable for running multiple programs in the same environment **/
    
    public void setPointer( int pos ) {
        mp = pos;
    }
    
    /** Loads brainfuck program, filters every char not in the CHARS constant or after a \ out of the String **/
    
    public void setProgram( String prg ) {
        
        boolean take = false;
        cmd = "";
        for( int i=0; i<prg.length(); i++ ) {
            if( take ) {
                cmd += prg.charAt(i);
                take = false;
            } else {
                if( CHARS.indexOf( prg.charAt(i) ) != -1 ) {
                    cmd += prg.charAt(i);
                } else if( prg.charAt(i) == '\\' ) {
                    cmd += "\\";
                    take = true;
                }
            }
        }
        
//        System.err.println( "Parsed program: " + cmd );
        
    }
    
    
    /** Returns next char **/
    
    public char getChar() {
        return getChar( this.cp );
    }
    
    /** Returns char from command string at specified position **/
    
    public char getChar( int nr ) {
        return cmd.charAt( nr );
    }
    
    
    /** Get a slice of the programm **/
    
    public String getChars( int start, int end ) {
        
        String tmp = "";
        for( int i=start-1; i<end; i++ ) {
            tmp += cmd.charAt(i);
        }
        return tmp;
        
    }
    
    
    /** Interpret program, set start address **/ 

    public boolean interpret( int spos ) {
        this.cp = spos;
        return interpret();
    }
    
    /** Start interpretation of programm **/
    
    public boolean interpret() {

        char inst;
        
        // Fetch next instruction
        try {
            inst = getChar();
        } catch( Exception e ) {
            return false;
        }
        
        // Parse instruction
        switch( inst ) {
            case '<':
                mp = (mp>0) ? mp-1 : 0;
                break;
            case '>':
                mp = (mp<cell.length) ? mp+1 : mp;
                mp_max = (mp>mp_max)? mp : mp_max;
                break;
            case '+':
                cell[mp]++;
                break;
            case '-':
                cell[mp]--;
                break;
            case '[':
                if( cell[mp] <= 0 )
                    return jumpWhileEnd();
                break;
            case ']':
                return jumpWhileStart();
                // break;
            case '\\':
                try {
                    cell[mp] = getChar( cp+1 );
                    cp++;
                } catch( Exception e ) {
                    return false;
                }
                break;
            case '.':
                if( cell[mp]==13 || cell[mp]==10 ) {
                    // Printing out char 13 or 10 causes an exception (don't know why)
                    ps.println( "" );
                } else {
                    ps.print( (char)cell[mp] );
                }
                break;
            case ',':
                try {
                    cell[mp] = is.read();
                    if( cell[mp] == '\n' )
                        cell[mp] = 0;
                } catch( Exception e ) {
                    return false;
                }
                break;
        }
        cp++;
        
        return true;

    }
    
    
    /** This method searches the corresponding [ sign **/
    
    public boolean jumpWhileStart() {
        
        int level = 0;
        
        for( int i=cp-1; i>=0; i-- ) {
            switch( cmd.charAt(i) ) {
                case '[':
                    if( level > 0 ) {
                        level--;
                    } else {
                        cp = i;
                        return true;
                    }
                    break;
                case ']':
                    level++;
                    break;
            }
        }
        return false;
        
    }
    
    /** This method searches trailing ] sign **/
    
    public boolean jumpWhileEnd() {
        
        int level = 0;
        
        for( int i=cp+1; i<cmd.length(); i++ ) {
            switch( cmd.charAt(i) ) {
                case '[':
                    level++;
                    break;
                case ']':
                    if( level <= 0 ) {
                        cp = i+1;
                        return true;
                    } else {
                        level--;
                    }
                    break;
            }
        }
        return false;
        
    }
    
    
    /** Print current state to specified PrintStream **/
    
    public void printState() {
        ps.println();
        
        ps.println("Code:");
        if( cp == 0 ) {
            ps.println("==> " + cmd);
        } else if( cp >= cmd.length()-1 ) {
            ps.println(cmd + " <==");
        } else {
            ps.println(getChars(1, cp) + "  ## " + getChar() + " ##  " + getChars(cp+2, cmd.length()));
        }
        ps.println();
        
        ps.println("Memory:");
        for(int i=0; i<mp_max; i++) {
            ps.print(i + ":\t" + cell[i]);
            if( mp == i )
                ps.print(" <--");
            ps.println();
        }
        ps.println();
    }
    
    
    public void start() {
        start(0, false);
    }
    
    /** This method goes through the whole code starting at a certain point in the code **/
    
    public void start( int off, boolean step ) {
        
        if( interpret(off) ) {
            while( interpret() ) {
                if( step ) {
                    printState();
                }
            }
        }
        
//        try {
//            System.err.println( "Programm terminated at instructions: " + getChars( cp-1, cp+2 ) );
//            System.err.println( "Code before exception: " + getChars( 1, cp+2 ) );
//        } catch( Exception e ) {
//            System.err.print( "Programm terminated. " );
//            if( cp >= cmd.length() ) {
//                System.err.println( "(End of Program)" );
//            } else {
//                System.err.println( "(Exception, no debuggin info available)" );
//            }
//        }
        
        try{
        	getChars( cp-1, cp+2 );
        	getChars( 1, cp+2 );
        } catch (Exception e) {
		}
        
    }
    
    
    /** Start a simple Hello World program, using the special ascii extension of this interpreter **/
    
    public void helloworldExt() {
        
        init( 100 );
        setProgram( "\\H.\\a.\\l.\\l.\\o.\\ .\\W.\\e.\\l.\\t.\\!." );
        start();
        
    }
    
    /** Starts a simple Hello World program, in "standard" brainfuck **/
    
    public void helloworld() {
        
        init( 100 );
        setProgram( ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-] <.>+++++++++++[<++++++++>-]<-.--------.+++.------.--------.[-]>++++++++[<++++>- ]<+.[-]++++++++++." );
        start();
        
    }
    
    
    /** If class is run directly, execute the argument as brainfuck program **/
    
    public static void main( String[] args ) {
        
        if( args.length < 1 ) {
            System.out.println( "Usage: " );
            System.out.println( "java bfi <brainfuck program> [-d]" );
            System.out.println();
            System.exit( -1 );
        }
        
        boolean debug = false;
        if( args.length > 1 && args[1].compareTo("-d") == 0 )
            debug = true;
        
        BrainfuckInterpreter bfi = new BrainfuckInterpreter();
        bfi.setProgram( args[0] );
        bfi.start(0, debug);
        
        System.out.println();
        
    }
    

}