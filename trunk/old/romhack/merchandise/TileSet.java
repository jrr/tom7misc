import java.util.*;
import java.io.*;

public class TileSet {

    public Vector tiles; /* Vector of Tile */
  
    public TileSet(){
	tiles = new Vector();
    }

    public void patch(String file) {
	try {
	    RandomAccessFile f = new RandomAccessFile(file, "rw");
	    int i = 0;
	    for (; i < tiles.size(); i++) {
		((Tile)tiles.elementAt(i)).patch(f);
	    }
	    f.close();
	    System.out.println("Patched " + i + " tiles.");
	} catch (IOException e) {
	    System.out.println("When patching: " + e);
	    e.printStackTrace();
	}
    }

    public void loadfromfile(String file, LinkedList offsets) {
	System.out.println("TS: Reading from file '" + file + "'.");
	tiles = new Vector();
	
	try {
	    RandomAccessFile f = new RandomAccessFile(file, "r");
	    for(ListIterator i = offsets.listIterator();
		i.hasNext();) {
		Integer ii = (Integer)i.next();
		System.out.println("TS: Add tile at " + ii.intValue() + "...");
		tiles.add(new Tile(f, ii.intValue()));
	    }
	    f.close();
	} catch (IOException e) {
	    System.out.println("TS: IO Error: " + e + "\n");
	    throw new Error();
	}
    }

    public void load (String s) {
	System.out.println("TS: Destroying old tiles");
	tiles = new Vector();
	System.out.println("TS: Adding tiles");
	while (s.length() > 64) {
	    try {
		Tile newtile = new Tile();
		s = newtile.init(s);
		tiles.add(newtile);
		System.out.println("TS: Added tile");
	    } catch (Tile.TileException t) {
		return;
	    }
	}
    }

    public String save(boolean showaddr){

	String result = new String();

	for (int index = 0; index < tiles.size(); index++) {
	    if (showaddr && ((Tile)tiles.elementAt(index)).offset > -1) {
		result += "(" + ((Tile)tiles.elementAt(index)).offset + ")\n\n";
	    } else {
		result += (Tile)tiles.elementAt(index);
		result += '\n';
	    }
	}

	return result;
	
    }

}
