import java.io.*;

public class Tile {
    public int tile[];

    /* distance into the file. */
    public int offset;

    public Tile() {
	offset = -1;
	tile = new int[64];
    }

    public class TileException extends Exception {
        TileException() { super(); }
	TileException(String s) { super(s); }
    }
    
    public Tile(RandomAccessFile f, int off) {
	offset = off;
	tile = new int[64];
	byte[] b = new byte[16];

	try {
	    f.seek(off);
	    
	    f.readFully(b);
	} catch (Exception e) {
	    throw new Error();
	}
	int x = 0;
	for(int u = 0; u < 8; u++) {
	    for(int i = 7; i >= 0; i--) {
		
		tile[u*8 + (7-i)] = 
		    (((b[x+1] & (1 << i))>0)?2:0) |
		    (((b[x  ] & (1 << i))>0)?1:0);

	    }
	    x += 2;
	}
    }
    
    /** returns remainder of string */
    public String init(String s) throws TileException{
	int index = 0;
	int arrayindex = 0;
	
	try {
	    while (arrayindex < 64) {
		if (s.charAt(index) == '.') tile[arrayindex] = 0;
		else if (s.charAt(index) == '+') tile[arrayindex] = 1;
		else if (s.charAt(index) == '*') tile[arrayindex] = 2;
		else if (s.charAt(index) == '#') tile[arrayindex] = 3;
		else arrayindex --;
		
		arrayindex ++;
		index ++;
		
	    }
	} catch (StringIndexOutOfBoundsException siob) {
	    throw new TileException();
	}
	
	return s.substring(index);
    }

    public byte[] tobytes() {
	int subcnt = 0;
	byte byte1 = 0;
	byte byte2 = 0;
	int cnt = 0;
	byte[] buf = new byte[16];
	for (int i=0; i < 64; i++) {
	    subcnt ++;

	    int n = tile[i];
		
	    byte1 <<= 1;
	    byte2 <<= 1;
	    byte1 |= ((n&1) > 0) ? 1 : 0;
	    byte2 |= ((n&2) > 0) ? 1 : 0;


	    if (subcnt >= 8) {
		buf[cnt] = byte1;
		buf[cnt+1] = byte2;
		cnt += 2;

		if (cnt >= 16) {
		    return buf;
		}

		subcnt = 0;
		byte1 = 0;
		byte2 = 0;
	    }


	}
	throw new Error();
    }

    public void patch(RandomAccessFile f) {
	if (offset > -1) {
	    try {
		f.seek(offset);
		f.write(tobytes());
	    } catch (Exception e) {
		System.out.println("Can't seek or write..?: " + e);
		/*		throw new Error();*/
	    }
	} else {
	    System.out.println("can't patch this tile (unknown offset).");
	}
    }

    public void clear() {
	for (int y = 0; y < 8; y ++) 
	    for (int x = 0; x < 8; x ++) 
		tile[y*8 + x] = 0;

    }
    
    public String toString() {
	String result = new String();

	for (int y = 0; y < 8; y ++) {
	    for (int x = 0; x < 8; x ++) {
		int index = y*8 + x;

		result += ".+*#".charAt(tile[index]);
	    }
	    result += '\n';
	}
	return result;


    }

    
}
