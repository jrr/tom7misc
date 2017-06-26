import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

public class TileSetView extends JPanel implements ActionListener {
    
    TileArtist tileartist;

    JButton text, find, patch;

    JFrame jframe;

    TileSet tileset;

    MainWindow mainwindow;

    public String lastrom;

    public TileSetView(TileSet ts, TileEditor te, JFrame jf, MainWindow mw) {

	mainwindow = mw;
	jframe = jf;

	lastrom = "";

	setLayout(new BorderLayout());

	tileset = ts;
	tileartist = new TileArtist(tileset, te);
	JPanel bottom = new JPanel();
	JPanel subbot = new JPanel();

	add(tileartist, BorderLayout.CENTER);
	add(bottom, BorderLayout.SOUTH);
	add(subbot, BorderLayout.NORTH);

	text  = new JButton("text");
	find  = new JButton("find");
	patch = new JButton("patch");

	text .addActionListener(this);

	find .addActionListener(this);
	patch.addActionListener(this);

	bottom.add(text);
	subbot.add(find);
	subbot.add(patch);
    }

    public void actionPerformed(ActionEvent e) {
	Object o = e.getSource();

	if (o == text) new LoadDialog(this, tileset.save(false), jframe);
	else if (o == find) new FindDialog(this, tileset.save(true));
	else if (o == patch) new Patch(lastrom, tileset);

    }


    public void update(String s){
	tileset.load(s);
	mainwindow.te.removetiles();
	mainwindow.redraw();
    }

    public void find(String file, String s){
	/* FIXME: not implemented */

	LinkedList tofind = new LinkedList(), 
	    known = new LinkedList();

	makebytes(s, tofind, known);

	for(ListIterator i = known.listIterator();
	    i.hasNext();) {
	    Integer ii = (Integer)i.next();
	    System.out.println("Known: " + ii.intValue());
	}

	/*
	  for(ListIterator i = tofind.listIterator();
	  i.hasNext();) {
	  byte[] b = (byte[])i.next();
	  for (int z=0; z < 16; z++) 
	  System.out.print((char)b[z]);
	  System.out.println("\n");
	  }
	*/


	LinkedList found = 
	    (new LinearSearch()).find(file, tofind);

	for(ListIterator i = found.listIterator();
	    i.hasNext();) {
	    FindHit fh = (FindHit)i.next();
	    for(ListIterator j = fh.where.listIterator();
		j.hasNext();) {
		Integer ii = (Integer)j.next();
		known.addLast(ii);
	    }
	}

	tileset.loadfromfile(file, known);
	mainwindow.te.removetiles();
	mainwindow.redraw();
    }

    static void makebytes(String s, LinkedList tofind, LinkedList found) {

	int accoff = 0;
	int acctimes = 0;
	int state = 0;

	byte byte1 = 0, byte2 = 0;

	int cnt = 0;
	int subcnt = 0;
	byte[] buf = new byte[16];

	for(int i=0;
	    i < s.length();
	    i++) {
	    
	    char c = s.charAt(i);

	    switch(state) {
		
	    case 0: /* start */
		if (c == '(') {
		    state = 1;
		    accoff = 0;
		    break;
		} else if (c == '[') {
		    /* comment. */
		    state = 3;
		    break;
		} else if (c == '.' ||
			   c == '+' ||
			   c == '*' ||
			   c == '#') {

		    subcnt ++;

		    int n = 0;
		    if (c == '+') n = 1;
		    else if (c == '*') n = 2;
		    else if (c == '#') n = 3;

		    byte1 <<= 1;
		    byte2 <<= 1;
		    byte1 |= ((n&1) > 0) ? 1 : 0;
		    byte2 |= ((n&2) > 0) ? 1 : 0;


		    if (subcnt >= 8) {
			buf[cnt] = byte1;
			buf[cnt+1] = byte2;
			cnt += 2;

			if (cnt >= 16) {
			    cnt=0;
			    tofind.addLast(buf);
			    buf = new byte [16];
			}

			subcnt = 0;
			byte1 = 0;
			byte2 = 0;
		    }

		} else {
		    /* ignore it */
		}
		break;
	    case 1:
		if (c == ')') {
		    found.addLast(new Integer(accoff));
		    state = 0;
		} else if (c == ':') {
		    state = 2;
		    acctimes = 0;
		} else if (c >= '0' && c <= '9') {
		    accoff *= 10;
		    accoff += ((int)(c - '0'));
		}
		break;
	    case 2:
		if (c == ')') {
		    for(int x = 0; x < acctimes; x ++) {
			found.addLast(new Integer(accoff + x * 16));
		    }
		    state = 0;
		} else if (c >= '0' && c <= '9') {
		    acctimes *= 10;
		    acctimes += ((int)(c - '0'));
		}
		break;
	    case 3:
		/* comment */
		if (c == ']') state = 0;
		break;
	    default:
	    }
	}
    }
}
