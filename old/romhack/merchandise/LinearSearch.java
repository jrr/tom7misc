import java.util.*;
import java.io.*;

public class LinearSearch implements FindInFile {

    private class Thing {
	public int idx;
	public byte [] what;
	public LinkedList found;
	Thing() { 
	    idx = 0; 
	    found = new LinkedList(); /* of integers */
	}
    }

    public LinkedList find(String file, LinkedList tofind) {

	Thing [] look = new Thing [tofind.size()];

	int j = 0;

	int length = 999;

	for (ListIterator i = tofind.listIterator(); i.hasNext();) {
	    look[j] = new Thing();
	    byte[] b = (byte[]) i.next();
	    look[j].idx = -1;
	    length = b.length;
	    look[j].what = b;
	    j++;
	}

	byte[] buf = new byte[length];

	LinkedList result = new LinkedList(); /* of FindHit */

	/* nothing to search for? */
	if (j == 0) return result;

	try {
	    RandomAccessFile f = new RandomAccessFile(file, "r");
	    f.seek(0);
	    
	    f.readFully(buf);
	    
	    int idx = length;
	    
	    /* steady state.
	       buf is full.
	       Check if, starting at idx % length (and wrapping),
	       we match any string in our list. If so, add a hit
	       at offset (idx - length). Then, read a byte,
	       increment idx, and continue. 
	       When we hit eof, we're done.
	    */
	    
	    while(true) {
		for(int u=0; u < j; u++) {
		fail: 
		    do {
			for (int z=0; z < length; z++) {
			    if (look[u].what[z] != buf[(z + idx) % length]) 
				break fail;
			}
			/* match! */
			System.out.println("found tile " + u + " at " + 
					   (idx - length) + "!\n");

			look[u].found.addLast(new Integer(idx - length));
		    } while (false);
		}
		int a = f.read();
		if (a == -1) break; /* EOF */
		buf[idx % length] = (byte)a;
		idx ++;
	    }
	    f.close();
  
	    for(int h=0; h < j; h++) {
		result.addLast(new FindHit(look[h].what, look[h].found));
	    }

	    return result;
	} catch (Exception e) {
	    System.out.println("IO failure: " + e + "\n");
	    e.printStackTrace();
	    throw new Error();
	}
    }


};
