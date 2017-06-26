package org.tom7.seenery;

import java.lang.Thread;
import java.io.File;
import java.io.IOException;
import java.io.FileInputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.text.DateFormat;
import java.util.Date;

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.program.Program;

public class FileInfo {

	private static final DateFormat dateFormat = DateFormat
			.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.MEDIUM);

	/* canonical file name */
	File file;

	/* name to display */
	String display;

	IconCache ic;

	public FileInfo(File f, IconCache ic_) throws IOException {
		file = f.getCanonicalFile();
		display = f.getName();

		ic = ic_;

		if (f.isDirectory()) {
			icon = ic.stockImages[ic.iconClosedFolder];
		} else {
			icon = ic.stockImages[ic.iconFile];
		}
	}

	/*
	 * these items are filled in through filesystem calls, and those take time,
	 * so they happen in another thread.
	 */

	/* bogus */
	boolean loaded = false;

	private Image icon;

	private byte[] hash;
	private String hashstring;
	
	private String type;

	private String size;

	private String date;

	private FileData data;

	synchronized boolean isLoaded() {
		return loaded;
	}
	
	synchronized FileData getData() {
		return data;
	}

	/** get the stored data, or create a new
	 * data structure for the file.
	 * @return the FileData associated with this hash
	 */
	synchronized FileData getDataOrDefault() {
		if (file.isDirectory() ||
			!loaded ||
			hash == null) return null;
		
		if (data != null) return data;
		
		/* defaults.. */
		data = new FileData();
		return data;
	}

	synchronized void saveData() {
		FileDB.update(hash, data);		
	}
	
	/* display version of the hash */
	synchronized String getHash() {
		if (file.isDirectory())
			return "";
		if (hashstring == null)
			return "ERROR";
		else
			return hashstring;
	}

	/* useless? */
	synchronized String getType() {
		if (type == null)
			return "";
		else
			return type;
	}

	/* XXX error icons */
	synchronized Image getIcon() {
		return icon;
	}

	synchronized String getSize() {
		if (size == null)
			return "";
		else
			return size;
	}

	synchronized String getDate() {
		if (date == null)
			return "";
		else
			return date;
	}

	void seticonandtype() {

		if (file.isDirectory()) {
			icon = ic.stockImages[ic.iconClosedFolder];
		} else {
			String name = file.getName();
			int dot = name.lastIndexOf('.');
			if (dot != -1) {
				String extension = name.substring(dot);
				Program program = Program.findProgram(extension);
				if (program != null) {
					type = program.getName();
					icon = ic.getIconFromProgram(program);
				} else {
					type = ""; // "Unknown: " + extension.toUpperCase();
					icon = ic.stockImages[ic.iconFile];
				}
			} else {
				type = "";
				icon = ic.stockImages[ic.iconFile];
			}
		}

	}

	/* fill in the details! */
	synchronized void fullDetails() {
		/* only once */
		if (loaded) return;
		
		seticonandtype();

		if (file.isDirectory()) {
			/* XXX something for directories... */
			date = "";
			size = "";
			type = "";
			hash = null;
			hashstring = null;
		} else {
			try {
				MessageDigest md = MessageDigest.getInstance("MD5");

				long len = file.length();

				/* start hash with length */
				{
					long ilen = len;
					while (ilen > 0) {
						md.update((byte) (ilen & 255));
						ilen >>= 8;
					}
				}

				/*
				 * now add some data from the file. use the first 1024 bytes,
				 * and the 'middle' 1024 bytes (if len>2048) and the last 1024
				 * bytes (if len>1024)
				 */
				byte[] buffer = new byte[1024];
				{
					FileInputStream fis = new FileInputStream(file);

					/* first 1024 */
					readbytes(fis, buffer, 1024);
					md.update(buffer);

					/* middle */
					if (len > 2048) {
						long skip = ((file.length() - 1024) / 2) -
						/* current */1024;
						fis.skip(skip);
						readbytes(fis, buffer, 1024);
						md.update(buffer);

					}
				}

				/* now the last 1k */
				/* PERF would be better if we had seek/rewind */
				if (file.length() > 1024) {
					int tail = (int) (file.length() - 1024);
					if (tail > 1024)
						tail = 1024;

					FileInputStream fis = new FileInputStream(file);
					long skip = file.length() - tail;
					fis.skip(skip);
					readbytes(fis, buffer, tail);
					md.update(buffer);
				}

				hash = md.digest();
				hashstring = digest_tostring(hash);

				data = FileDB.lookup(hash);
			} catch (Exception e) {
				/* if there's no hashing, there's basically no point. */
				/* XXX ... this also happens if the file can't be found... */
				e.printStackTrace();
				throw new Error();
			}
			date = dateFormat.format(new Date(file.lastModified()));
			/* XXX bytes, k, megs */
			size = "" + ((file.length() + 512) / 1024) + "k";
		}
	
		loaded = true;
	}

	/* PERF */
	private static String digest_tostring(byte[] b) {
		char[] ch = new char[b.length * 2];
		for (int i = 0; i < b.length; i++) {
			ch[i * 2] = "0123456789ABCDEF".charAt(15 & (b[i] >> 4));
			ch[i * 2 + 1] = "0123456789ABCDEF".charAt(15 & b[i]);
		}
		return new String(ch);
	}

	/**
	 * Reads min(n, remaining) bytes from fis into the beginning of bs (which
	 * must have enough room); return the number of bytes read. Blocks until all
	 * bytes have been read.
	 * 
	 * @throws IOException
	 */
	private static int readbytes(FileInputStream fis, byte[] bs, int n)
			throws IOException {
		int target = n;
		if (fis.available() < n)
			target = fis.available();

		int remaining = target;
		while (remaining > 0) {
			remaining -= fis.read(bs, target - remaining, remaining);
		}
		return target;
	}
}
