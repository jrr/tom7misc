package org.tom7.seenery;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.FileInputStream;
import java.io.ObjectInputStream;

/** This singleton is responsible for maintaining
 *  a database of data associated with file (contents)
 *  by their hashes. This database is stored in a persistant
 *  way.  
 * @author Tom
 * @see FileData
 */
public class FileDB {
	
	/**
	 * Initialize the file database. Must be called once
	 * before calling any other function.
	 */
	static void init() {
		/* nothing now, since each operation is eager */
	}
	
	/** 
	 * Get the data associated with this file, if any.
	 * 
	 * @param hash the hash of the file to find
	 * @return the FileData structure, or null if not found
	 */
	static FileData lookup(byte[] hash) {
		File fi = getfilename(hash);
		
		try {
			FileInputStream fis = new FileInputStream(fi.getAbsolutePath());
			ObjectInputStream ois = new ObjectInputStream(fis);
			FileData fd = (FileData)ois.readObject();
			/* XXX close */
			return fd;
		} catch (Exception e) {
			/* normal, when the file does not exist yet */
			return null;
		}
	}
	
	/**
	 * Update the database to reflect the changes made
	 * to this hash.  
	 * @param hash the file's hash
	 * @param f the file's data
	 */
	static void update(byte [] hash, FileData f) {
		File fi = getfilename(hash);	
		/* make sure the directories exist */
		fi.getParentFile().mkdirs();
		try {
			FileOutputStream fos = new FileOutputStream(fi.getAbsolutePath());
			ObjectOutputStream oos = new ObjectOutputStream(fos);
			oos.writeObject(f);
			/* XXX close ... */
		} catch (Exception e) {
			e.printStackTrace();
			/* fails?? ut oh... */
			throw new Error(e);
		}
	}
	
	/**
	 * Flush any pending changes to disk in preparation for
	 * shutdown.
	 */
	static void flush() {
		/* nothing right now, since we are eager. */
	}

	/* XXX bogus */
	static final String prefix = "c:\\seenery\\data\\";
	
	private static File getfilename(byte[] b) {
		char[] ch = new char[b.length * 2];
		for (int i = 0; i < b.length; i++) {
			ch[i * 2] = "0123456789ABCDEF".charAt(15 & (b[i] >> 4));
			ch[i * 2 + 1] = "0123456789ABCDEF".charAt(15 & b[i]);
		}
		String h = new String(ch);
		return new File(prefix +
						h.substring(0, 2) + File.separator + 
						h.substring(2, h.length() - 2));
	}
	
}
