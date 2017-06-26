package org.tom7.seenery;

import java.io.Serializable;

public class FileData implements Serializable {

	static final long serialVersionUID = 200508010;
	
	static final int BORING = -1;
	static final int UNRATED = 0;
	static final int EXCLAIM = 1;
	static final int EXCLAIM2 = 2;
	
	boolean seen;
	
	int rating;
	
	/* seen to, etc. */
	String note;
	
	FileData(boolean s, int r, String n) {
		seen = s;
		rating = r;
		note = n;
	}
	
	FileData() {
		seen = false;
		rating = UNRATED;
		note = "";
	}
}
